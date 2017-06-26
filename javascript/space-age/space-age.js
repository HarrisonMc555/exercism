'use strict';

const SpaceAge = function SpaceAge(seconds) {
  /* secondsPerEarthYear is number of seconds in an Earth year */
  const secondsPerEarthYear = 31557600;

  /* earthYears is the number of Earth years for this object */
  const earthYears = seconds / secondsPerEarthYear;

  /* precision is the number of decimal places to round to */
  const precision = 2;

  /* roundDecimal(x, n) rounds x to n decimal places */
  const roundDecimal = function roundDecimal(x, n) {
    const d = Math.pow(10, n);
    return Math.round(x * d) / d;
  };

  /* onPlanet(r) returns a function for a planet with ratio r */
  const onPlanet = function onPlanet(r) {
    const years = roundDecimal(earthYears / r, precision);
    return function planet() { return years; };
  };

  /* seconds stores the number of seconds given */
  this.seconds = seconds;

  /* onEarth() returns `seconds` in Earth years */
  this.onEarth = onPlanet(1.0);

  /* onMercury() returns `seconds` in Mercury years */
  this.onMercury = onPlanet(0.2408467);

  /* onVenus() returns `seconds` in Venus years */
  this.onVenus = onPlanet(0.61519726);

  /* onMars() returns `seconds` in Mars years */
  this.onMars = onPlanet(1.8808158);

  /* onJupiter() returns `seconds` in Jupiter years */
  this.onJupiter = onPlanet(11.862615);

  /* onSaturn() returns `seconds` in Saturn years */
  this.onSaturn = onPlanet(29.447498);

  /* onUranus() returns `seconds` in Uranus years */
  this.onUranus = onPlanet(84.016846);

  /* onNeptune() returns `seconds` in Neptune years */
  this.onNeptune = onPlanet(164.79132);
};

module.exports = SpaceAge;
