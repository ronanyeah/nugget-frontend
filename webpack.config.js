const { resolve } = require("path");
const webpack = require("webpack");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const InlineChunkHtmlPlugin = require("react-dev-utils/InlineChunkHtmlPlugin");
//const analyzer = require("webpack-bundle-analyzer");

const publicFolder = resolve("./public");

const { RPC_URL } = process.env;

module.exports = (env) => {
  const xnft = Boolean(process.env.XNFT);
  const devMode = Boolean(env.WEBPACK_SERVE);

  const loaderConfig = {
    loader: "elm-webpack-loader",
    options: {
      debug: false,
      optimize: !devMode,
      cwd: __dirname,
    },
  };

  const elmLoader = devMode
    ? [{ loader: "elm-reloader" }, loaderConfig]
    : [loaderConfig];

  return {
    mode: devMode ? "development" : "production",
    entry: "./src/index.ts",
    output: {
      publicPath: "/",
      path: publicFolder,
      filename: "bundle.js",
    },
    stats: devMode ? "errors-warnings" : "normal",
    infrastructureLogging: {
      level: "warn",
    },
    devServer: {
      port: 8000,
      hot: "only",
    },
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: elmLoader,
        },
        {
          test: /\.ts$/,
          use: "ts-loader",
          exclude: /node_modules/,
        },
      ],
    },
    resolve: {
      extensions: [".ts", ".js"],
    },
    plugins: [
      new NodePolyfillPlugin(),
      new webpack.NoEmitOnErrorsPlugin(),
      new webpack.DefinePlugin({
        RPC_URL_: JSON.stringify(RPC_URL),
      }),
      ...(xnft
        ? [
            new HtmlWebpackPlugin({
              minify: false,
              cache: false,
              inject: "body",
              template: "./public/index.html",
              filename: "./xnft.html",
            }),
            new InlineChunkHtmlPlugin(HtmlWebpackPlugin, [/.(js)$/]),
          ]
        : []),
      //new analyzer.BundleAnalyzerPlugin(),
    ],
  };
};
