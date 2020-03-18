use std::collections::HashMap;
use std::collections::HashSet;

/// `InputCellID` is a unique identifier for an input cell.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct InputCellID(u32);
/// `ComputeCellID` is a unique identifier for a compute cell.
/// Values of type `InputCellID` and `ComputeCellID` should not be mutually assignable,
/// demonstrated by the following tests:
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input: react::ComputeCellID = r.create_input(111);
/// ```
///
/// ```compile_fail
/// let mut r = react::Reactor::new();
/// let input = r.create_input(111);
/// let compute: react::InputCellID = r.create_compute(&[react::CellID::Input(input)], |_| 222).unwrap();
/// ```
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct ComputeCellID(u32);
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct CallbackID(u32);

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum CellID {
    Input(InputCellID),
    Compute(ComputeCellID),
}

#[derive(Debug, PartialEq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

pub struct Reactor<'a, T> {
    input_cells: HashMap<InputCellID, InputCell<T>>,
    next_input_cell_id: InputCellID,
    compute_cells: HashMap<ComputeCellID, ComputeCell<'a, T>>,
    next_compute_cell_id: ComputeCellID,
    callbacks: HashMap<CallbackID, ComputeCellID>,
    next_callback_id: CallbackID,
}

// You are guaranteed that Reactor will only be tested against types that are Copy + PartialEq.
impl<'a, T> Reactor<'a, T>
where
    T: Copy + Eq,
{
    pub fn new() -> Self {
        Reactor {
            input_cells: HashMap::new(),
            next_input_cell_id: InputCellID(0),
            compute_cells: HashMap::new(),
            next_compute_cell_id: ComputeCellID(0),
            callbacks: HashMap::new(),
            next_callback_id: CallbackID(0),
        }
    }

    // Creates an input cell with the specified initial value, returning its ID.
    pub fn create_input(&mut self, initial: T) -> InputCellID {
        let value = initial;
        let id = self.next_input_cell_id;
        let dependants = HashSet::new();
        let input_cell = InputCell { value, dependants };
        self.input_cells.insert(id, input_cell);
        self.next_input_cell_id.increment();
        id
    }

    // Creates a compute cell with the specified dependencies and compute function.
    // The compute function is expected to take in its arguments in the same order as specified in
    // `dependencies`.
    // You do not need to reject compute functions that expect more arguments than there are
    // dependencies (how would you check for this, anyway?).
    //
    // If any dependency doesn't exist, returns an Err with that nonexistent dependency.
    // (If multiple dependencies do not exist, exactly which one is returned is not defined and
    // will not be tested)
    //
    // Notice that there is no way to *remove* a cell.
    // This means that you may assume, without checking, that if the dependencies exist at creation
    // time they will continue to exist as long as the Reactor exists.
    pub fn create_compute<F>(
        &mut self,
        dependencies: &[CellID],
        compute_func: F,
    ) -> Result<ComputeCellID, CellID>
    where
        F: 'a + Fn(&[T]) -> T,
    {
        for &dep_id in dependencies {
            if !self.cell_exists(dep_id) {
                return Err(dep_id);
            }
        }
        let function = Box::new(compute_func);
        let dependencies = dependencies.to_vec();
        let id = self.next_compute_cell_id;
        let dependants = HashSet::new();
        self.add_dependants(&dependencies, id);
        let compute_cell = ComputeCell {
            function,
            dependencies,
            dependants,
            value: None,
            callbacks: Vec::new(),
        };
        self.compute_cells.insert(id, compute_cell);
        self.next_compute_cell_id.increment();
        Ok(id)
    }

    // Retrieves the current value of the cell, or None if the cell does not exist.
    //
    // You may wonder whether it is possible to implement `get(&self, id: CellID) -> Option<&Cell>`
    // and have a `value(&self)` method on `Cell`.
    //
    // It turns out this introduces a significant amount of extra complexity to this exercise.
    // We chose not to cover this here, since this exercise is probably enough work as-is.
    pub fn value(&mut self, id: CellID) -> Option<T> {
        match id {
            CellID::Input(input_id) => {
                self.input_cells.get(&input_id).map(InputCell::value)
            }
            // CellID::Compute(compute_id) => {
            //     self.compute_cells
            //         .get_mut(&comute_id)
            //         .map(|cell| cell.compute(&mut self));
            // }
            CellID::Compute(compute_id) => {
                if let Some(cell) = self.compute_cells.get_mut(&compute_id) {
                    cell.compute(&mut self);
                }
                // let cell = self.compute_cells
                //     .get_mut(&compute_id);
                // cell.map(|cell| cell.compute(&mut self));
                println!("Compute");
                None
            }
        }
    }

    // Sets the value of the specified input cell.
    //
    // Returns false if the cell does not exist.
    //
    // Similarly, you may wonder about `get_mut(&mut self, id: CellID) -> Option<&mut Cell>`, with
    // a `set_value(&mut self, new_value: T)` method on `Cell`.
    //
    // As before, that turned out to add too much extra complexity.
    pub fn set_value(&mut self, id: InputCellID, new_value: T) -> bool {
        match self.input_cells.get_mut(&id) {
            Some(cell) => {
                cell.set_value(new_value);
                self.update_dependencies(id);
                true
            }
            None => false,
        }
    }

    // Adds a callback to the specified compute cell.
    //
    // Returns the ID of the just-added callback, or None if the cell doesn't exist.
    //
    // Callbacks on input cells will not be tested.
    //
    // The semantics of callbacks (as will be tested):
    // For a single set_value call, each compute cell's callbacks should each be called:
    // * Zero times if the compute cell's value did not change as a result of the set_value call.
    // * Exactly once if the compute cell's value changed as a result of the set_value call.
    //   The value passed to the callback should be the final value of the compute cell after the
    //   set_value call.
    pub fn add_callback<F>(
        &mut self,
        compute_cell_id: ComputeCellID,
        callback: F,
    ) -> Option<CallbackID>
    where
        F: 'a + FnMut(T) -> (),
    {
        let compute_cell = match self.compute_cells.get_mut(&compute_cell_id) {
            Some(cell) => cell,
            None => return None,
        };
        let callback_id = self.next_callback_id;
        compute_cell.add_callback(callback);
        self.callbacks.insert(callback_id, compute_cell_id);
        self.next_callback_id.increment();
        Some(callback_id)
    }

    // Removes the specified callback, using an ID returned from add_callback.
    //
    // Returns an Err if either the cell or callback does not exist.
    //
    // A removed callback should no longer be called.
    pub fn remove_callback(
        &mut self,
        cell: ComputeCellID,
        callback: CallbackID,
    ) -> Result<(), RemoveCallbackError> {
        unimplemented!(
            "Remove the callback identified by the CallbackID {:?} from the cell {:?}",
            callback,
            cell,
        )
    }

    fn cell_exists(&self, id: CellID) -> bool {
        match id {
            CellID::Input(input_id) => self.input_cells.contains_key(&input_id),
            CellID::Compute(compute_id) => {
                self.compute_cells.contains_key(&compute_id)
            }
        }
    }

    fn add_dependants(&mut self, ids: &[CellID], dependant_id: ComputeCellID) {
        for id in ids {
            self.add_dependant(*id, dependant_id);
        }
    }

    fn add_dependant(&mut self, id: CellID, dependant_id: ComputeCellID) {
        match id {
            CellID::Input(input_id) => self
                .input_cells
                .get_mut(&input_id)
                .unwrap()
                .add_dependant(dependant_id),
            CellID::Compute(compute_id) => self
                .compute_cells
                .get_mut(&compute_id)
                .unwrap()
                .add_dependant(dependant_id),
        }
    }

    // fn get_all_dependancies(&self, id: CellID) -> HashSet<CellID> {
    //     let mut deps = HashSet::new();
    //     deps.insert(id);
    //     if let CellID::Compute(compute_id) = id {
    //         let mut dep_ids = HashSet::new();
    //         dep_ids.insert(id);
    //         if let Some(cell) = self.compute_cells.get(&compute_id) {
    //             for dep_id in cell.dependencies.iter() {
    //                 deps.extend(self.get_all_dependancies(*dep_id));
    //             }
    //         }
    //     };
    //     deps
    // }

    fn update_dependencies(&mut self, id: InputCellID) {
        let cell = self.input_cells[&id];
        for dependant_id in cell.dependants {
            let dependant_cell = self.compute_cells[&dependant_id];
            dependant_cell.update(&mut self);
        }
    }

    pub fn calculate<F>(&mut self, dependencies: &[CellID], func: F) -> T
    where F: 'a + Fn(&[T]) -> T,
    {
        let args: Vec<_> = dependencies
            .iter()
            .map(|dep| self.value(*dep).unwrap())
            .collect();
        func(&args)
    }
}

struct InputCell<T> {
    value: T,
    dependants: HashSet<ComputeCellID>,
}

impl<T> InputCell<T>
where
    T: Copy,
{
    pub fn value(&self) -> T {
        self.value
    }

    pub fn set_value(&mut self, new_value: T) {
        self.value = new_value;
    }

    pub fn add_dependant(&mut self, compute_id: ComputeCellID) {
        self.dependants.insert(compute_id);
    }
}

struct ComputeCell<'a, T> {
    function: Box<dyn 'a + Fn(&[T]) -> T>,
    dependencies: Vec<CellID>,
    dependants: HashSet<ComputeCellID>,
    value: T,
    callbacks: Vec<Box<dyn 'a + FnMut(T) -> ()>>,
}

impl<'a, T> ComputeCell<'a, T>
where
    T: Copy + Eq,
{
    pub fn update(&mut self, reactor: &mut Reactor<'a, T>) {
        // let args: Vec<_> = self
        //     .dependencies
        //     .iter()
        //     .map(|dep| reactor.value(*dep).unwrap())
        //     .collect();
        // self.value = (self.function)(&args);
        self.value = reactor.calculate(&self.dependencies, self.function);
        for dependant_id in self.dependants {
            let dependant_cell = reactor.compute_cells[&dependant_id];
            dependant_cell.update(reactor);
        }
    }

    pub fn value(&mut self, reactor: &mut Reactor<'a, T>) -> T {
        self.value
    }

    pub fn add_dependant(&mut self, compute_id: ComputeCellID) {
        self.dependants.insert(compute_id);
    }

    pub fn add_callback<F>(&mut self, callback: F)
    where
        F: 'a + FnMut(T) -> (),
    {
        self.callbacks.push(Box::new(callback));
    }
}

impl InputCellID {
    fn increment(&mut self) {
        (*self).0 += 1;
    }
}

impl ComputeCellID {
    fn increment(&mut self) {
        (*self).0 += 1;
    }
}

impl CallbackID {
    fn increment(&mut self) {
        (*self).0 += 1;
    }
}
