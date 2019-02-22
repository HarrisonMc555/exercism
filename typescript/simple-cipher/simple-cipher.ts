const NUM_LETTERS: number = 26;
const KEY_LENGTH: number = 100;
const FIRST_LETTER: string = 'a';
const FIRST_LETTER_CHAR_CODE: number = FIRST_LETTER.charCodeAt(0);

class SimpleCipher {
    key: string;

    constructor(key?: string) {
        if (!key)
            key = this.generateKey();
        this.key = key;
    }

    encode(message: string) {
        let encoded: string[] = [];
        for (let i: number = 0; i < message.length; i++) {
            const messageCharCode = message.charCodeAt(i);
            const keyIndex = i % this.key.length;
            const keyCharCode = this.key.charCodeAt(keyIndex);
            const keyOffset = keyCharCode - FIRST_LETTER_CHAR_CODE;
            const encodedCharCode = (messageCharCode + keyOffset) % NUM_LETTERS;
            const encodedLetter = String.fromCharCode(encodedCharCode);
            encoded.push(encodedLetter);
        }
        let encodedString = encoded.join('');
        return encodedString;
    }

    decode(encoded: string) {
        return encoded;
    }

    generateKey() {
        let key: string[] = [];
        for (let i: number = 0; i < KEY_LENGTH; i++) {
            const offset: number = Math.floor(Math.random() * NUM_LETTERS);
            const letterCode: number = FIRST_LETTER_CHAR_CODE + offset;
            const letter: string = String.fromCharCode(letterCode);
            key.push(letter);
        }
        let keyString: string = key.join('');
        return keyString;
    }
}

export default SimpleCipher
