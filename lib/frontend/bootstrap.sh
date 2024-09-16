#!/usr/bin/env sh

BASEDIR=$(dirname "$0")
happy -agc "$BASEDIR/boot-src/Parser.ly" -o "$BASEDIR/src/Happy/Frontend/Parser.hs"
happy -agc "$BASEDIR/boot-src/AttrGrammarParser.ly" -o "$BASEDIR/src/Happy/Frontend/AttrGrammar/Parser.hs"
