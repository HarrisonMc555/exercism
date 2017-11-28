/* eslint no-plusplus: "off" */

function LinkedList() {
  this.head = null;
  this.tail = null;
  this.length = 0;

  // _createNode: createa new node
  this._createNode = function _createNode(value, prev, next) {
    return { value, prev, next };
  };

  // _deleteNode: remove node
  this._deleteNode = function _deleteNode(node) {
    if (node === null) {
      return;
    }
    const next = node.next;
    const prev = node.prev;
    if (this.head === node) {
      this.head = next;
    }
    if (this.tail === node) {
      this.tail = prev;
    }
    if (prev !== null) {
      prev.next = next;
    }
    if (next !== null) {
      next.prev = prev;
    }
    this.length--;
  };

  // unshift: add at front
  this.unshift = function unshift(x) {
    if (this.head === null) {
      this.head = this._createNode(x, null, null);
      this.tail = this.head;
    } else {
      const newNode = this._createNode(x, null, this.head);
      if (this.head !== null) {
        this.head.prev = newNode;
      }
      this.head = newNode;
    }
    this.length++;
  };

  // shift: remove at front
  this.shift = function shift() {
    let value;
    if (this.head === null) {
      value = undefined;
    } else {
      value = this.head.value;
      this._deleteNode(this.head);
    }
    return value;
  };

  // push: add at back
  this.push = function push(x) {
    if (this.tail === null) {
      this.tail = this._createNode(x, null, null);
      this.head = this.tail;
    } else {
      const newNode = this._createNode(x, this.tail, null);
      if (this.tail !== null) {
        this.tail.next = newNode;
      }
      this.tail = newNode;
    }
    this.length++;
  };

  // pop: remove at back
  this.pop = function pop() {
    let value;
    if (this.tail === null) {
      value = undefined;
    } else {
      value = this.tail.value;
      this._deleteNode(this.tail);
    }
    return value;
  };

  // count: length
  this.count = function count() {
    return this.length;
  };

  // delete: remove matching element
  this.delete = function delete_(x) {
    for (let node = this.head; node !== null; node = node.next) {
      if (node.value === x) {
        this._deleteNode(node);
      }
    }
  };

  // toString: string representation
  this.toString = function toString() {
    let s = '[';
    for (let node = this.head; node !== null; node = node.next) {
      s += node.value;
      if (node !== this.tail) {
        s += ', ';
      }
    }
    s += ']';
    return s;
  };
}

module.exports = LinkedList;
