function extractRows(arr) {
  return arr;
}

function extractColumns(arr) {
  return arr[0].map((e, i) => arr.map(r => r[i]));
}

function extractArray(str) {
  const rows = str.split('\n');
  const strArr = rows.map(r => r.split(' '));
  const intArr = strArr.map(r => r.map(e => parseInt(e, 10)));
  return intArr;
}

function Matrix(str) {
  this.str = str;
  const arr = extractArray(str);
  this.rows = extractRows(arr);
  this.columns = extractColumns(arr);
}

module.exports = Matrix;
