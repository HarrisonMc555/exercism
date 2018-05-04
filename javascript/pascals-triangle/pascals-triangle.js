function nextRow(row) {
  let next_row = [1];
  for (let i = 0; i < row.length - 1; i += 1) {
    next_row.push(row[i] + row[i + 1]);
  }
  next_row.push(1);
  return next_row;
}

function Triangle(num_row) {
  let row = [1];
  let rows = [];
  for (let i = 0; i < num_row; i += 1) {
    rows.push(row);
    row = nextRow(row);
  }
  this.rows = rows;
  this.lastRow = rows[num_row - 1];
}

module.exports = Triangle;
