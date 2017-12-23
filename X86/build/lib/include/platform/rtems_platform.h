#ifndef RTEMS_H_
#define RTEMS_H_

#include <rtems.h>
#include <sys/mman.h>
#include <bsp.h>
#include <sys/dirent.h>
#include <sys/termios.h>
#include <sys/fcntl.h>
#include <fcntl.h>
#include <sys/utsname.h>
#include <pwd.h>
#include <grp.h>
#include <sys/wait.h>
#include <sys/utime.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <rtems/score/resource.h>
#include <sys/syslog.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
//#include <sys/sysconf.h>
//#include <sys/sysctl.h>
#include <unistd.h>
//#include <process.h>
#include <inttypes.h>
#include <sys/poll.h>
//#define PRIuMAX "llu"
//#define PRIxMAX "llx"
//#define PRIx32 "d"
//#define PRIu32 "u"
//#define UINTPTR_MAX  UINT32_MAX
//#define PRIxPTR "x"
/*#define PRIdMAX "d"
#define PRIx16  "d"
#define PRIu16  "d"
#define PRIu8   "d"
#define PRId32  "d"*/

//typedef unsigned long MLton_Rlim_t;
typedef unsigned int nfds_t;
//typedef MLton_Rlim_t rlim_t;


#ifdef __sparc__
//#include "float-math.h"
// //#include "setenv.h" 
#define RLIMIT_AS 6
#define RLIMIT_CORE 4
#define RLIMIT_CPU 0
#define RLIMIT_DATA 2
#define RLIMIT_FSIZE 1
#define RLIMIT_INFINITY 4294967295
#define RLIM_INFINITY 4294967295
#define RLIMIT_MEMLOCK 4294967295
#define RLIMIT_NOFILE 5
#define RLIMIT_NPROC 4294967295
#define RLIMIT_RSS 4294967295
#define RLIMIT_STACK 3

#endif

#ifdef __sparc__

#define FE_TONEAREST  0
#define FE_DOWNWARD   1
#define FE_UPWARD     2
#define FE_TOWARDZERO 3

#undef fegetround
#undef fesetround
#define fegetround MLton_fegetround
#define fesetround MLton_fesetround
int fegetround(void);
int fesetround(int rounding_mode);
#endif


//#define HAS_FEROUND FALSE
#define HAS_FEROUND TRUE
#define HAS_REMAP FALSE
#define HAS_SIGALTSTACK FALSE
#define HAS_SPAWN FALSE
#define HAS_TIME_PROFILING FALSE
#define HAS_MSG_DONTWAIT FALSE

#define MLton_Platform_OS_host "rtems"


typedef unsigned long MLton_rlim_t;
#undef rlim_t
#define rlim_t MLton_rlim_t

struct MLton_rlimit {
        rlim_t  rlim_cur;
        rlim_t  rlim_max;
};
#undef rlimit
#define rlimit MLton_rlimit

#endif
