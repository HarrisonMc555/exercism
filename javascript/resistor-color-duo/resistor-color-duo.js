const COLOR_TO_NUM = {
  black: 0,
  brown: 1,
  red: 2,
  orange: 3,
  yellow: 4,
  green: 5,
  blue: 6,
  violet: 7,
  grey: 8,
  white: 9,
};

export const decodedValue = ([color1, color2]) => COLOR_TO_NUM[color1] * 10 + COLOR_TO_NUM[color2];

export default 'decodedValue';
