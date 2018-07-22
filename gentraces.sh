#!/usr/bin/env bash

set -e

if [ "$#" -ne 3 ]; then
	echo "Usage: $0 COMMAND infile.zip outfile.zip"
	echo "COMMAND is one of"
	echo "  prepare-submission"
	exit 1
fi

EXEC="icfpc2018-exe"
COMMAND="$1"
KEYFILE=`realpath privatekey`
INFILE=`realpath "$2"`
OUTFILE=`realpath "$3"`

TMP_IN_DIR=`realpath out/gentraces/models`
TMP_OUT_DIR=`realpath out/gentraces/traces`

if [ -f "$KEYFILE" ]; then
	echo "ENCRYPT: Using private key from $KEYFILE"
	KEY=`cat "$KEYFILE"`
else
	echo "ENCRYPT: don't encrypt (no keyfile)"
	KEY=''
fi

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
	if [ -z "$KEY" ]; then
		zip "$OUTFILE" *
		echo "$OUTFILE UNENCRYPTED"
		echo "put private key to $KEYFILE to encrypt"
	else
		zip --encrypt -P "$KEY" "$OUTFILE" *
		echo "$OUTFILE ENCRYPTED with $KEY"
	fi
	popd 
}

solve_fa(){
	ls -1 "$TMP_IN_DIR" | grep 'mdl$' | grep '^FA' | while read f ; do
        MDL="$f"
        NBT="${MDL%_tgt.mdl}.nbt"
        echo "exec: $TMP_IN_DIR/$MDL -> $TMP_OUT_DIR/$NBT"
        stack exec "$EXEC" -- "construct" "$TMP_IN_DIR/$MDL" "$TMP_OUT_DIR/$NBT"
	done
}

solve_fd(){
	ls -1 "$TMP_IN_DIR" | grep 'mdl$' | grep '^FD' | while read f ; do
        MDL="$f"
        NBT="${MDL%_src.mdl}.nbt"
        echo "exec: $TMP_IN_DIR/$MDL -> $TMP_OUT_DIR/$NBT"
        stack exec "$EXEC" -- "deconstruct" "$TMP_IN_DIR/$MDL" "$TMP_OUT_DIR/$NBT"
	done
}

solve_fr(){
	ls -1 "$TMP_IN_DIR" | grep '_src\.mdl$' | grep '^FR' | while read f ; do
        SRC_MDL="$f"
        TGT_MDL="${SRC_MDL%_src.mdl}_tgt.mdl"
        NBT="${SRC_MDL%_src.mdl}.nbt"
        echo "exec: $TMP_IN_DIR/$SRC_MDL -> $TMP_OUT_DIR/$NBT"
        stack exec "$EXEC" -- "reconstruct" "$TMP_IN_DIR/$SRC_MDL" "$TMP_IN_DIR/$TGT_MDL"  "$TMP_OUT_DIR/$NBT"
	done
}

if [ "$COMMAND" == "prepare-submission" ]; then
	stack build
	do_unzip

	rm -rf "$TMP_OUT_DIR"
	mkdir -p "$TMP_OUT_DIR"

	solve_fa
    solve_fd
    solve_fr
	do_zip
	echo "done"
else
	echo "Unknown COMMAND: $COMMAND"
	exit 2
fi

