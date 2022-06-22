// @ts-check

'use strict';

const path = require('path');

/**@type {import('webpack').Configuration}*/
const config = {
  mode: 'development',
  target: 'node',
  entry: path.resolve(__dirname, 'src', 'server.ts'),
  output: {
    // the bundle is stored in the 'dist' folder (check package.json), 📖 -> https://webpack.js.org/configuration/output/
    path: path.resolve(__dirname, 'dist'),
    filename: 'server.js',
    libraryTarget: 'commonjs2',
    devtoolModuleFilenameTemplate: '../[resource-path]'
  },
  devtool: 'source-map',
  externals: {
    vscode: 'commonjs vscode' // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
  },
  resolve: {
    // support reading TypeScript and JavaScript files, 📖 -> https://github.com/TypeStrong/ts-loader
    extensions: ['.ts', '.js']
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: /node_modules/,
        enforce: 'pre',
        use: [
          {
            loader: 'ts-loader'
          },
          {
            loader: 'tslint-loader',
            options: {
              typeCheck: true
            }
          }
        ]
      }
    ]
  }
};

module.exports = config;