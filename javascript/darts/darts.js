export const solve = (x, y) => {
  const distance = getDistance([x, y], center);
  if (distance <= 1.0) {
    return 10;
  } else if (distance <= 5.0) {
    return 5;
  } else if (distance <= 10.0) {
    return 1;
  } else {
    return 0;
  }
};

const getDistance = ([x1, y1], [x2, y2]) => {
  const dx = x2 - x1;
  const dy = y2 - y1;
  return Math.sqrt(dx*dx + dy*dy);
}

const center = [0, 0];
