'use strict';

const DnaTranscriber = function DnaTranscriber() {};

DnaTranscriber.prototype.convertBase = function convertBase(b) {
  switch (b) {
    case 'G' : return 'C';
    case 'C' : return 'G';
    case 'T' : return 'A';
    case 'A' : return 'U';
    default : throw Error('Invalid input');
  }
};

DnaTranscriber.prototype.toRna = function toRna(seq) {
  return Array.prototype.map.call(seq, e => this.convertBase(e)).join('');
};

module.exports = DnaTranscriber;
