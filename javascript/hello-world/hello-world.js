const HelloWorld = function HelloWorld() {};

HelloWorld.prototype.hello = function hello(input) {
  const name = input || 'World';
  return `Hello, ${name}!`;
};

module.exports = HelloWorld;
