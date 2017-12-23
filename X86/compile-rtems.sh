RTEMS="/home/oem/pickle/rtems/rtems-4.11-host/sparc-rtems4.11/leon3/lib"
PATH="/home/oem/pickle/rtems/4.11/bin:$PATH"
RTEMS411="/home/oem/pickle/rtems/rtems-4.11-host/sparc-rtems4.11/include"
GMP="/home/oem/pickle/gmp_sparc"

echo "fp=$2" > runtime/Makefile
cat runtime/Makefile.rtems >> runtime/Makefile

cd runtime && cd ..

make TARGET=sparc-rtems4.11 TARGET_OS=rtems TARGET_ARCH=$1\
    CFLAGS="-pipe -B$RTEMS  \
    -I$RTEMS411 \
    -specs bsp_specs -qrtems -Wall $2 \
    -B$GMP -ffunction-sections -fdata-sections -Wl,--gc-sections -mcpu=cypress -DNO_FENV_H" \
    COMPILE_FAST=yes dirs runtime

    
echo "fp=$2" > build/bin/mlton
cat $1-rtems-mlton >> build/bin/mlton
cp $1-rtems-constants build/lib/targets/sparc-rtems4.11/constants
