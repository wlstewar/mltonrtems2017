RTEMS="/home/oem/pickle/rtems/rtems-4.11-host/i386-rtems4.11/pc386/lib"
RTEMS411="/home/oem/pickle/rtems/rtems-4.11-host/i386-rtems4.11/include"
GMP="/home/oem/pickle/gmp_i386"

cp runtime/Makefile.rtems runtime/Makefile
cd runtime && make clean && cd ..


make TARGET=i386-rtems4.11 TARGET_OS=rtems TARGET_ARCH=X86 \
    CFLAGS="-pipe -B$RTEMS  \
    -I$RTEMS411 \
    -specs bsp_specs -qrtems -g -Wall -msoft-float \
    -B$GMP -Wl,-Ttext,0x00100000 -mtune=i386 -DNO_FENV_H" \
    COMPILE_FAST=yes dirs runtime

cp rtems-constants build/lib/targets/i386-rtems4.11/constants
cp rtems-mlton build/bin/mlton
