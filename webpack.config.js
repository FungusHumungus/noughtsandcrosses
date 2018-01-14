var path = require('path');

module.exports = {
	entry: './kick.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist')
  }
};

