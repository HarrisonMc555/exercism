/* ========== Enums ========== */
const CodeAction = Object.freeze({
  ENCODE: 'Encode',
  DECODE: 'Decode',
});

const IncDec = Object.freeze({
  INC: 'Increment',
  DEC: 'Decrement',
});

/* ========== Constants ========== */
const NUM_LETTERS = 26;

const ORD_A = 'a'.charCodeAt();

/* ========== Helper functions ========== */
function strMap(str, func) {
  return str.split('').map(func).join('');
}

function mod(num, divisor) {
  let rem = num % divisor;
  if (rem < 0) {
    rem += divisor;
  }
  return rem;
}

function isLowerCase(char) {
  return /^[a-z]+$/.test(char);
}

/* ========== Caesar cipher functions ========== */
function caesarChr(num) {
  return String.fromCharCode(ORD_A + num);
}

function caesarOrd(char) {
  if (!isLowerCase(char)) {
    throw new Error(`Invalid character ${char}`);
  }
  return char.toLowerCase().charCodeAt() - ORD_A;
}

function caesarRotateChar(incDec, rotateAmount, char) {
  let rotateFunction;
  switch (incDec) {
    case IncDec.INC: rotateFunction = (x, y) => x + y; break;
    case IncDec.DEC: rotateFunction = (x, y) => x - y; break;
    default: throw new Error('Non-exhaustive case (IncDec)');
  }
  const charOrd = caesarOrd(char);
  const uncheckedRotatedOrd = rotateFunction(charOrd, rotateAmount);
  const rotatedOrd = mod(uncheckedRotatedOrd, NUM_LETTERS);
  return caesarChr(rotatedOrd);
}

function caesarIncrementChar(rotateAmount, char) {
  return caesarRotateChar(IncDec.INC, rotateAmount, char);
}

function caesarDecrementChar(rotateAmount, char) {
  return caesarRotateChar(IncDec.DEC, rotateAmount, char);
}

function caesarChangeChar(incDec, keyChar, msgChar) {
  let incDecChar;
  switch (incDec) {
    case IncDec.INC: incDecChar = caesarIncrementChar; break;
    case IncDec.DEC: incDecChar = caesarDecrementChar; break;
    default: throw new Error('Non-exhaustive case (IncDec)');
  }
  return incDecChar(caesarOrd(keyChar), msgChar);
}

function caesarDecodeChar(keyChar, msgChar) {
  return caesarChangeChar(IncDec.DEC, keyChar, msgChar);
}

function caesarEncodeChar(keyChar, msgChar) {
  return caesarChangeChar(IncDec.INC, keyChar, msgChar);
}

function caesarChangeMessage(codeAction, key, msg) {
  let codeActionFunction;
  switch (codeAction) {
    case CodeAction.ENCODE: codeActionFunction = caesarEncodeChar; break;
    case CodeAction.DECODE: codeActionFunction = caesarDecodeChar; break;
    default: throw new Error('Non-exhaustive case (CodeAction)');
  }
  return strMap(msg, (msgChar, index) => {
    const keyChar = key[index % key.length];
    return codeActionFunction(keyChar, msgChar);
  });
}

/* ========== Exported functions ========== */
function Cipher(key) {
  if (key === undefined) {
    this.key = 'aaaaaaaaaa';
  } else if (key === '' || !isLowerCase(key)) {
    throw new Error('Bad key');
  } else {
    this.key = key;
  }

  this.decode = function decode(msg) {
    return caesarChangeMessage(CodeAction.DECODE, this.key, msg);
  };

  this.encode = function encode(msg) {
    return caesarChangeMessage(CodeAction.ENCODE, this.key, msg);
  };
}

module.exports = Cipher;
