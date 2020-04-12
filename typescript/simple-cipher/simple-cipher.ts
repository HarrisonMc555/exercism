enum EncodeDecode {
  Encode,
  Decode,
}

const LOWER_A_CHAR_CODE: number = "a".charCodeAt(0);
const LOWER_Z_CHAR_CODE: number = "z".charCodeAt(0);

class SimpleCipher {
  public key: string;
  static LETTERS: string = "abcdefghijklmnopqrstuvwxyz";
  static KEY_LEN: number = 100;

  constructor(key?: string) {
    if (key !== undefined) {
      this.key = SimpleCipher.validateKey(key);
    } else {
      this.key = SimpleCipher.generateRandomKey();
    }
  }

  encode(message: string): string {
    return this.encodeDecode(message, EncodeDecode.Encode);
  }

  decode(encoded: string): string {
    return this.encodeDecode(encoded, EncodeDecode.Decode);
  }

  encodeDecode(input: string, direction: EncodeDecode): string {
    const combineOffsets: (x: number, y: number) => number =
      direction == EncodeDecode.Encode ? (x, y) => x + y : (x, y) => x - y;
    let altered = [];
    for (let i = 0; i < input.length; i++) {
      const inputLetter = input[i];
      const inputOffset = SimpleCipher.letterToOffset(inputLetter);
      const keyLetter = this.key[i % this.key.length];
      const keyOffset = SimpleCipher.letterToOffset(keyLetter);
      const offset = euclidMod(
        combineOffsets(inputOffset, keyOffset),
        SimpleCipher.LETTERS.length
      );
      const decodedLetter = SimpleCipher.offsetToLetter(offset);
      altered.push(decodedLetter);
    }
    return altered.join("");
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
    let letters: string[] = [];
    for (let i = 0; i < SimpleCipher.KEY_LEN; i++) {
      const letterIndex: number = getRandomInt(SimpleCipher.LETTERS.length);
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

export default SimpleCipher;
