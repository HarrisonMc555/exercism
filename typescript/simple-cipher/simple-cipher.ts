/* eslint-disable no-unused-vars */
/* eslint no-throw-literal: "off" */

enum EncodeDirection {
  Encode,
  Decode,
}

const LOWER_A_CHAR_CODE = "a".charCodeAt(0);
const LOWER_Z_CHAR_CODE = "z".charCodeAt(0);

type CombineNumber = (x1: number, x2: number) => number;

class SimpleCipher {
  public key: string;

  static LETTERS = "abcdefghijklmnopqrstuvwxyz";

  static KEY_LEN = 100;

  constructor(key?: string) {
    if (key !== undefined) {
      this.key = SimpleCipher.validateKey(key);
    } else {
      this.key = SimpleCipher.generateRandomKey();
    }
  }

  encode(message: string): string {
    return this.transform(message, EncodeDirection.Encode);
  }

  decode(encoded: string): string {
    return this.transform(encoded, EncodeDirection.Decode);
  }

  transform(input: string, direction: EncodeDirection): string {
    const inputLetters = input.split("");
    const keyLetters = this.key.split("");
    const transformHelper = (c1: string, c2: string): string =>
      this.transformLetter(direction, c1, c2);
    const outputLetters = cycleZip(inputLetters, keyLetters, transformHelper);
    return outputLetters.join("");
  }

  transformLetter(
    direction: EncodeDirection,
    inputLetter: string,
    keyLetter: string
  ): string {
    let combineOffsets: CombineNumber;
    switch (direction) {
      case EncodeDirection.Encode:
        combineOffsets = (x, y): number => x + y;
        break;
      case EncodeDirection.Decode:
        combineOffsets = (x, y): number => x - y;
        break;
      default:
        combineOffsets = assertUnreachable(direction);
        break;
    }
    const inputOffset = SimpleCipher.letterToOffset(inputLetter);
    const keyOffset = SimpleCipher.letterToOffset(keyLetter);
    const offset = euclidMod(
      combineOffsets(inputOffset, keyOffset),
      SimpleCipher.LETTERS.length
    );
    return SimpleCipher.offsetToLetter(offset);
  }

  static validateKey(key: string): string {
    if (!SimpleCipher.isValidKey(key)) {
      throw "Bad key";
    }
    return key;
  }

  static isValidKey(key: string): boolean {
    return key.length > 2 && key.split("").every(isLowerCaseLetter);
  }

  static generateRandomKey(): string {
    const letters: string[] = [];
    for (let i = 0; i < SimpleCipher.KEY_LEN; i += 1) {
      const letterIndex = getRandomInt(SimpleCipher.LETTERS.length);
      letters.push(SimpleCipher.LETTERS[letterIndex]);
    }
    return letters.join("");
  }

  static letterToOffset(letter: string): number {
    return letter.charCodeAt(0) - LOWER_A_CHAR_CODE;
  }

  static offsetToLetter(offset: number): string {
    return String.fromCharCode(LOWER_A_CHAR_CODE + offset);
  }
}

function getRandomInt(max: number): number {
  return Math.floor(Math.random() * Math.floor(max));
}

function euclidMod(num: number, divisor: number): number {
  return ((num % divisor) + divisor) % divisor;
}

function isLowerCaseLetter(char: string): boolean {
  const charCode = char.charCodeAt(0);
  return LOWER_A_CHAR_CODE <= charCode && charCode <= LOWER_Z_CHAR_CODE;
}

/**
 * Zip two arrays, cycling through the second array if it is shorter than the
 * first.
 */
function cycleZip<T, U>(
  array1: T[],
  array2: T[],
  combine: (t1: T, t2: T) => U
): U[] {
  return array1.map((t1, index) => {
    const t2 = array2[index % array2.length];
    return combine(t1, t2);
  });
}

function assertUnreachable(_x: never): never {
  throw "Expected not to get here";
}

export default SimpleCipher;
