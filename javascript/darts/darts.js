export const solve = (x, y) => {
  const distance = getDistance([x, y], center);
  return distance <= 1.0 ? 10 :
    distance <= 5.0 ? 5 :
    distance <= 10.0 ? 1 :
    0;
};

const getDistance = ([x1, y1], [x2, y2]) => {
  const dx = x2 - x1;
  const dy = y2 - y1;
  return Math.sqrt(dx*dx + dy*dy);
}

const center = [0, 0];
