#!/usr/bin/env bash

# set -e

die () {
        echo "$1" >&2
        exit 1
}

if $(which gtime) --version >/dev/null 2>&1; then
        time=$(which gtime)
elif $(which gnutime) --version >/dev/null 2>&1; then
        time=$(which gnutime)
elif $(which time) --version 2>&1 | grep -q GNU; then
        time=$(which time)
else
        die 'Can'\''t find GNU time'
fi


minTime="30.0"

#bench="barnes-hut boyer checksum count-graphs DLXSimulator even-odd fft fib flat-array hamlet imp-for knuth-bendix lexgen life logic mandelbrot matrix-multiply md5 merge mlyacc model-elimination mpuz nucleic output1 peek pidigits psdes-random ratio-regions ray raytrace simple smith-normal-form tailfib tak tensor tsp tyan vector-concat vector-rev vliw wc-input1 wc-scanStream zebra zern"

bench="barnes-hut"

cd tests
for prog in $bench; do
    case "$prog" in
        "fxp")
            continue ;;
    esac

    ( cat $prog.sml ; cat run.sml; printf "val _ = print(\"%s time:  \" ^Time.toString(Time.-(fin, start)) ^ \" seconds %s\")" $prog '\n') > $prog.main.sml
    ../../build/bin/mlton -target i386-rtems4.11 -codegen i386-rtems4.11 -output $prog $prog.main.sml
    mv $prog ../benchmarks/$prog
    #rm $prog.main.sml
done
