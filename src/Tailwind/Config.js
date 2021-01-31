"use strict";

exports._resolveConfig = function (file) {
  const resolveConfig = require("tailwindcss/resolveConfig");
  // TODO: probably change the require(file) to be more safe
  const tailwindConfig = require(file);
  return resolveConfig(tailwindConfig);
};
