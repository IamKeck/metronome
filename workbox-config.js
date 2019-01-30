module.exports = {
  "globDirectory": "./",
  "globPatterns": [
    "dist/elm.js",
    "dist/ps.js",
    "style.css",
    "index.html",
    "*.svg"
   ],
  runtimeCaching: [
      {
          urlPattern: /^https:\/\/cdnjs\.cloudflare\.com/,
          handler: "cacheFirst"

      }
  ],
  "swDest": "service-worker.js"
};