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

export class ResistorColor {
  private colors: Color[];

  constructor(colors: Color[]) {
    if (colors.length < 2) {
      throw "At least two colors need to be present";
    }
    this.colors = colors.slice(0, 2);
  }

  value = (): number => {
    let num = 0;
    for (const color of this.colors) {
      num *= 10;
      num += ResistorColor.colorToDigit(color);
    }
    return num;
  };

  private static colorToDigit(color: Color): number {
    return COLOR_TO_DIGIT[color];
  }
}
