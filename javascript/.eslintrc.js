module.exports = {
  "env": {
    "es6": true
  },
  "plugins": [
    "node",
  ],
  "extends": [
    "eslint:recommended",
    "eslint-config-airbnb",
    "plugin:node/recommended",
  ],
  "rules": {
    "linebreak-style": "off"
  },
  "parserOptions": {
    "ecmaVersion": 2015
  }
};
