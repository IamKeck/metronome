module.exports = {
    entry: "./src/index.js",
    output: {
        filename: "index.js",
    },
    module: {
        rules: [
            {
                test:    /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                loader:  'elm-webpack-loader?verbose=true&warn=true',
              },
        ]
    }

}