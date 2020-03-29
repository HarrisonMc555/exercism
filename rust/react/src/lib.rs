use std::collections::HashMap;

/// `InputCellID` is a unique identifier for an input cell.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct InputCellID(usize);
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
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ComputeCellID(usize);
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CallbackID(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum CellID {
    Input(InputCellID),
    Compute(ComputeCellID),
}

#[derive(Debug, PartialEq)]
pub enum RemoveCallbackError {
    NonexistentCell,
    NonexistentCallback,
}

struct ComputeCell<'a, T> {
    dependencies: Vec<&'a Cell<'a, T>>,
    compute_func: Box<dyn Fn(&[T]) -> T>,
}

enum Cell<'a, T> {
    Input(T),
    Compute(ComputeCell<'a, T>),
}

pub struct Reactor<'a, T>
where
    T: Clone + PartialEq,
{
    cur_input_cell_id: InputCellID,
    cur_compute_cell_id: ComputeCellID,
    cells: HashMap<CellID, Cell<'a, T>>,
}

// You are guaranteed that Reactor will only be tested against types that are Copy + PartialEq.
impl<'a, T> Reactor<'a, T>
where
    T: Clone + PartialEq,
{
    pub fn new() -> Self {
        Reactor {
            cur_input_cell_id: InputCellID(0),
            cur_compute_cell_id: ComputeCellID(0),
            cells: HashMap::new(),
        }
    }

    // Creates an input cell with the specified initial value, returning its ID.
    pub fn create_input(&mut self, initial: T) -> InputCellID {
        let input_cell_id = self.get_next_input_cell_id();
        self.cells
            .insert(CellID::Input(input_cell_id), Cell::Input(initial));
        input_cell_id
    }

    fn get_next_input_cell_id(&mut self) -> InputCellID {
        self.cur_input_cell_id.0 += 1;
        self.cur_input_cell_id
    }

    fn get_next_compute_cell_id(&mut self) -> ComputeCellID {
        self.cur_compute_cell_id.0 += 1;
        self.cur_compute_cell_id
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
    pub fn create_compute<F: 'static + Fn(&[T]) -> T>(
        &'a mut self,
        dependencies: &[CellID],
        compute_func: F,
    ) -> Result<ComputeCellID, CellID> {
        let dependencies = dependencies
            .iter()
            .map(|&id| self.get_cell(id).ok_or(id))
            .collect::<Result<Vec<_>, _>>()?;
        let compute_cell_id = self.get_next_compute_cell_id();
        let compute_cell = ComputeCell {
            dependencies: dependencies,
            compute_func: Box::new(compute_func),
        };
        self.cells.insert(
            CellID::Compute(compute_cell_id),
            Cell::Compute(compute_cell),
        );
        Ok(compute_cell_id)
    }

    fn get_cell(&self, id: CellID) -> Option<&Cell<T>> {
        self.cells.get(&id)
    }

    // Retrieves the current value of the cell, or None if the cell does not exist.
    //
    // You may wonder whether it is possible to implement `get(&self, id: CellID) -> Option<&Cell>`
    // and have a `value(&self)` method on `Cell`.
    //
    // It turns out this introduces a significant amount of extra complexity to this exercise.
    // We chose not to cover this here, since this exercise is probably enough work as-is.
    pub fn value(&self, id: CellID) -> Option<T> {
        self.cells.get(&id).map(|&cell| self.value_of(cell))
    }

    fn compute(&self, compute_cell: &ComputeCell<T>) -> T {
        let dependencies = compute_cell
            .dependencies
            .iter()
            .map(|&&cell| self.value_of(cell))
            .collect::<Vec<_>>();
        (*compute_cell.compute_func)(&dependencies)
    }

    fn value_of(&self, cell: Cell<'a, T>) -> T {
        match cell {
            Cell::Input(value) => value.clone(),
            Cell::Compute(compute_cell) => self.compute(&compute_cell),
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
        if let Some(Cell::Input(value)) = self.cells.get_mut(&CellID::Input(id)) {
            *value = new_value;
            true
        } else {
            false
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
    pub fn add_callback<F: FnMut(T) -> ()>(
        &mut self,
        _id: ComputeCellID,
        _callback: F,
    ) -> Option<CallbackID> {
        unimplemented!()
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
}
