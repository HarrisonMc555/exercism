/* Phone number for NANP number */
const PhoneNumber = function PhoneNumber(s) {
  /* Extract the numbers */
  const dirtyNums = s.replace(/[^0-9]/g, '');
  var cleanNums;
  if (this.isValidPhoneNumber(dirtyNums)) {
    cleanNums = dirtyNums.substr(dirtyNums.length - 10);
  } else {
    cleanNums = '0000000000';
  }
  /* Extract different parts */
  this.area = cleanNums.substr(0, 3);
  this.exchange = cleanNums.substr(3, 3);
  this.subscriber = cleanNums.substr(6, 4);
  this.num = cleanNums;
};

/* Return true if the string is a valid NANP phone number */
PhoneNumber.prototype.isValidPhoneNumber = function isValidPhoneNumber(nums) {
  /* Either 10 digits or 11 digits with leading 1 */
  return (nums.length === 11 && nums.charAt(0) === '1') ||
         nums.length === 10;
};

/* Return the full number with no formatting (without leading 1) */
PhoneNumber.prototype.number = function number() {
  return this.num;
};

/* Return area code */
PhoneNumber.prototype.areaCode = function areaCode() {
  return this.area;
};

/* Return formatted string, ex: (xxx) xxx-xxx */
PhoneNumber.prototype.toString = function toString() {
  return `(${this.area}) ${this.exchange}-${this.subscriber}`;
};

module.exports = PhoneNumber;
