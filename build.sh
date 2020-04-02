#!/bin/bash
./node_modules/.bin/elm make src/Main.elm --output=public-elm/flashcard-elm.js --optimize
./node_modules/.bin/webpack