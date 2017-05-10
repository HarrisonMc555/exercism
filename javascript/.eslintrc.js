module.exports = {
  "env": {
    "es6": true
  },
  "extends": "eslint-config-airbnb-es5",
  "plugins": [
    "import"
  ],
  "rules": {
    "indent": ["warn", 2, {"SwitchCase": 1}],
    "no-else-return": "off",
    "quotes" : "off"
  },
  "globals": {
    "Set": true
  }
};
