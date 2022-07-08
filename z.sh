#!/bin/bash

elm-live src/Main.elm -- --output main.js
python -m http.server
