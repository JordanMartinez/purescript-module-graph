"use strict";

const { execSync } = require('child_process');

exports.execSync = function(command) {
  return function () {
    return execSync(command);
  }
}
