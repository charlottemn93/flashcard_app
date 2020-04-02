const path = require('path');

module.exports = {
  entry: './js/index.js',
  module: {
    rules: [
      {
        test: /\.(js)$/,
        include: /js/,
        exclude: /node_modules/,
        use: ['babel-loader', 'eslint-loader'],
      },
    ],
  },
  output: {
    path: path.resolve(__dirname, 'public-elm'),
    filename: 'flashcard.js',
  },
};
