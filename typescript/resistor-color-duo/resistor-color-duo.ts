enum Color {
  Black,
  Brown,
  Red,
  Orange,
  Yellow,
  Green,
  Blue,
  Violet,
  Grey,
  White,
}

const STRING_TO_COLOR: { [key: string]: Color } = {
  black: Color.Black,
  brown: Color.Brown,
  red: Color.Red,
  orange: Color.Orange,
  yellow: Color.Yellow,
  green: Color.Green,
  blue: Color.Blue,
  violet: Color.Violet,
  grey: Color.Grey,
  white: Color.White,
};

export class ResistorColor {
  private colors: Color[];

  constructor(colors: string[]) {
    if (colors.length < 2) {
      throw "At least two colors need to be present";
    }
    this.colors = colors.slice(0, 2).map(ResistorColor.stringToColor);
  }

  value = (): number => {
    let num = 0;
    for (const color of this.colors) {
      num *= 10;
      const digit = ResistorColor.colorToDigit(color);
      if (digit !== null) {
        num += digit;
      }
    }
    return num;
  };

  private static stringToColor(s: string): Color {
    const color: Color | null = STRING_TO_COLOR[s];
    if (color === null) {
      throw `Invalid color string ${s}`;
    }
    return color;
  }

  private static colorToDigit(color: Color): number {
    switch (color) {
      case Color.Black:
        return 0;
      case Color.Brown:
        return 1;
      case Color.Red:
        return 2;
      case Color.Orange:
        return 3;
      case Color.Yellow:
        return 4;
      case Color.Green:
        return 5;
      case Color.Blue:
        return 6;
      case Color.Violet:
        return 7;
      case Color.Grey:
        return 8;
      case Color.White:
        return 9;
    }
  }
}
