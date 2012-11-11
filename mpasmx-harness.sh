#!/bin/sh

## Do not call this by hand: it's a harness for the code in
## picasm-external.el (and should stay in the same directory so that
## the elisp code can find it)
##
## Parameters:
##
##   $1 = The path to mpasmx
##   $2 = The path of the asm file to compile
##   $3 = The output file name
##   $4 = The chip name (lower case, stripped of PIC prefix)
##   $5 = Default radix
##   $6 = Output format
##   $7 = Absolute mode?
##
## Behaviour:
##
##   MPASMX is run, if possible. The harness outputs any warnings or
##   errors to stderr
##
## Known problems:
## 
##   Currently, one can't specify a list of include directories. This
##   is because MPASMX doesn't implement it. A hacky solution would be
##   to copy the file to be assembled, together with any asm files in
##   an include directory, into a temp directory and then compile
##   there, but I can't really face that now.

mpasmx=${1}
infile=${2}
outfile=${3}
chipname=${4}
radix=${5}
outfmt=${6}
absolute=${7}

no_temp ()
{
    echo "ERROR: Couldn't create temp file."
    exit 1
}

errfile="$(mktemp)" || no_temp
lstfile="$(mktemp)" || no_temp

if [ x"${absolute}" = x"true" ]; then
    set -x
    "${mpasmx}" "-a${outfmt}" "-p${chipname}" "-r${radix}" "-e${errfile}" "-l${lstfile}" "${infile}"
    set +x
else
    set -x
    "${mpasmx}" "-o${outfile}" "-p${chipname}" "-r${radix}" "-e${errfile}" "-l${lstfile}" "${infile}"
    set +x
fi

cat "${errfile}"

grep '^Error\[' "${errfile}"
exitcode=$(($? == 0))

rm ${errfile} ${lstfile}

exit $exitcode
