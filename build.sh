#/bin/sh
mkdir build
cp index.html build

elm-make iconic-memory-game.elm --output build/iconic-memory-game.js
uglifyjs build/iconic-memory-game.js -o build/iconic-memory-game.min.js
