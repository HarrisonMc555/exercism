'use strict';

const Bob = function Bob() {};

/* Mimic default implementation of Python's strip */
function strip(s) {
  /* Strip leading and trailing spaces */
  return s.replace(/^ */, '').replace(/ *$/, '');
}

Bob.prototype.hey = function hey(msg) {
  if (!strip(msg)) {
    /* Blank and empty strings */
    return 'Fine. Be that way!';
  } else if (msg === msg.toUpperCase() && msg.match(/[a-z]/i)) {
    /* At least one letter, already all upper case */
    return 'Whoa, chill out!';
  } else if (msg[msg.length - 1] === '?') {
    /* Last character is question mark */
    return 'Sure.';
  } else {
    /* Default */
    return 'Whatever.';
  }
};

module.exports = Bob;
