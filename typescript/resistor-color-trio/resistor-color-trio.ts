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

const PREFIX_NUMS = [1, 2, 3] as const;
type PREFIX_NUM = typeof PREFIX_NUMS[number];
const PREFIXES: Readonly<Record<PREFIX_NUM, string>> = {
  3: "giga",
  2: "mega",
  1: "kilo",
}
const NUM_PREFIX_ZEROS = 3;

type Color = keyof typeof COLOR_TO_DIGIT;
const BASE = 10;

function numericalResistorValue(colors: [Color, Color, Color]): number {
  let [value1, value2, numZeros] = colors.map(c => COLOR_TO_DIGIT[c]);
  let originalValue = BASE * value1 + value2;
  let factor = Math.pow(BASE, numZeros)
  return originalValue * factor;
}

function toCanonical(value: number): [number, string] {
  for (const num of PREFIX_NUMS) {
    let prefix = PREFIXES[num];
    let divisor = Math.pow(BASE, num * NUM_PREFIX_ZEROS);
    if (value >= divisor) {
      return [value / divisor, prefix];
    }
  }
  return [value, ""];
}

export function decodedResistorValue(colors: [Color, Color, Color]): string {
  let resistorValue = numericalResistorValue(colors);
  let [canonicalValue, prefix] = toCanonical(resistorValue);
  return `${canonicalValue} ${prefix}ohms`;
}
