#!/bin/sh

#
# Building Schiffer for the first time
#

current_dir_=$(dirname "${0}")

echo ⇒ Compiling Schiffer
sbcl --noinform --non-interactive \
	 --eval '(ql:quickload :alexandria)' \
	 --eval "(compile-file \"${current_dir_}/../generated-src/schiffer.lisp\" :output-file \"../build/schiffer.fasl\")" \
	 --eval "(quit)"

echo ⇒ Building Schiffer executable
buildapp --manifest-file quicklisp-manifest.txt \
		 --load-system alexandria \
		 --load "${current_dir_}/../build/schiffer.fasl" \
		 --entry schiffer:main \
		 --output "${current_dir_}/../schiffer"
