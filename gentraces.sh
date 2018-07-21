#!/bin/bash

set -e

if [ "$#" -ne 3 ]; then
	echo "Usage: $0 COMMAND infile.zip outfile.zip"
	echo "COMMAND is one of"
	echo "  gentrace-dummy"
	exit 1
fi

EXEC="icfpc2018-exe"
COMMAND="$1"
INFILE=`realpath "$2"`
OUTFILE=`realpath "$3"`

TMP_IN_DIR=`realpath out/gentraces/models`
TMP_OUT_DIR=`realpath out/gentraces/traces`

do_unzip(){
	rm -rf "$TMP_IN_DIR"
	mkdir -p "$TMP_IN_DIR"
	pushd "$TMP_IN_DIR"
	echo "unzip: $INFILE -> $TMP_IN_DIR"
	unzip "$INFILE"
	popd
}

do_zip(){
	pushd "$TMP_OUT_DIR"
	rm -rf "$OUTFILE"
	echo "zip: $TMP_OUT_DIR/* $OUTFILE"
	#TODO: encrypt
	zip "$OUTFILE" *
	popd 
}

do_run(){
	MDL="$1"
	NBT="${MDL%mdl}nbt"
	echo "exec: $TMP_IN_DIR/$MDL -> $TMP_OUT_DIR/$NBT"
	stack exec "$EXEC" -- "$COMMAND" "$TMP_IN_DIR/$MDL" "$TMP_OUT_DIR/$NBT"
}

run(){
	rm -rf "$TMP_OUT_DIR"
	mkdir -p "$TMP_OUT_DIR"
	#TODO: use xargs ?
	ls -1 "$TMP_IN_DIR" | grep 'mdl$' | while read f ; do
#		echo "file:$f"
		do_run "$f"
	done
}

if [ "$COMMAND" == "gentrace-dummy" ]; then
	stack build
	do_unzip
	run
	do_zip
	echo "done"
else
	echo "Unknown COMMAND: $COMMAND"
	exit 2
fi

