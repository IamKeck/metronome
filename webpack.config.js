const SWPrecacheWebpackPlugin = require('sw-precache-webpack-plugin');

const PUBLIC_PATH = 'https://metronome.keckserver.xyz/';

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
    },
    plugins: [
        new SWPrecacheWebpackPlugin(
            {
                cacheId: 'metronome',
                filepath: "./service-worker.js",
                dontCacheBustUrlsMatching: /\.\w{8}\./,
                filename: 'service-worker.js',
                minify: true,
                navigateFallback: PUBLIC_PATH + 'index.html',
                staticFileGlobsIgnorePatterns: [/\.map$/, /asset-manifest\.json$/],
                staticFileGlobs:[
                    "dist/index.js",
                    "style.css",
                    "index.html",
                    "*.svg"
                ],
                runtimeCaching: [
                    {
                        urlPattern: /^https:\/\/cdnjs\.cloudflare\.com/,
                        handler: "cacheFirst"

                    }
                ]
            },
        ),
    ]

}
