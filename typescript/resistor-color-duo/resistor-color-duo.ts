const COLOR_TO_DIGIT: Record<string, number> = {
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

type Color = keyof typeof COLOR_TO_DIGIT;
const BASE = 10;

export function decodedValue(colors: [Color, Color, ...Color[]]): number {
  let [color1, color2] = colors;
  return BASE * COLOR_TO_DIGIT[color1] + COLOR_TO_DIGIT[color2];
}
