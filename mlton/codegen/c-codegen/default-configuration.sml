structure RtemsMain = 
   struct
      val defaultMain = "/*\n *  Default configuration file\n *\n *  COPYRIGHT (c) 1989-2008.\n *  On-Line Applications Research Corporation (OAR).\n *\n *  The license and distribution terms for this file may be\n *  found in the file LICENSE in this distribution or at\n *  http://www.rtems.org/license/LICENSE.\n */\n\n#ifdef HAVE_CONFIG_H\n#include \"config.h\"\n#endif\n\n#include <stdlib.h>\n#include <stdio.h>\n#include <rtems.h>\n\n//int main( int argc, char **argv );\n\nstatic void Init( rtems_task_argument arg )\n{\n  const char *boot_cmdline = *((const char **) arg);\n  char       *cmdline = NULL;\n  int         argc = 0;\n  char      **argv = NULL;\n  int         result;\n\n  if ( boot_cmdline != NULL ) {\n    size_t n = strlen( boot_cmdline ) + 1;\n\n    cmdline = malloc( n );\n    if ( cmdline != NULL ) {\n      char* command;\n\n      memcpy( cmdline, boot_cmdline, n);\n\n      command = cmdline;\n\n      /*\n       * Break the line up into arguments with \"\" being ignored.\n       */\n      while ( true ) {\n        command = strtok( command, \" \\t\\r\\n\" );\n        if ( command == NULL )\n          break;\n\n        ++argc;\n        command = '\\0';\n      }\n\n      /*\n       * If there are arguments, allocate enough memory for the argv\n       * array to be passed into main().\n       *\n       * NOTE: If argc is 0, then argv will be NULL.\n       */\n      argv = calloc( argc, sizeof( *argv ) );\n      if ( argv != NULL ) {\n        int a;\n\n        command = cmdline;\n        argv[ 0 ] = command;\n\n        for ( a = 1; a < argc; ++a ) {\n          command += strlen( command ) + 1;\n          argv[ a ] = command;\n        }\n      } else {\n        argc = 0;\n      }\n    }\n  }\n\n  result = MLton_main( argc, argv );\n\n  free( argv );\n  free( cmdline );\n\n  exit( result );\n}\n\n/* configuration information */\n\n/* This is enough to get a basic main() up. */\n#define CONFIGURE_RTEMS_INIT_TASKS_TABLE\n#define CONFIGURE_UNIFIED_WORK_AREAS\n#define CONFIGURE_STACK_CHECKER_ENABLED\n\n/* on smaller architectures lower the number or resources */\n#if defined(__m32c__)\n  #define CONFIGURE_MAXIMUM_TASKS 3\n#else\n  #define CONFIGURE_UNLIMITED_OBJECTS\n  #define CONFIGURE_MAXIMUM_USER_EXTENSIONS 8\n  #define CONFIGURE_MAXIMUM_DRIVERS 16\n  #define CONFIGURE_LIBIO_MAXIMUM_FILE_DESCRIPTORS 32\n#endif\n\n/* Include basic device drivers needed to call delays */\n#define CONFIGURE_APPLICATION_NEEDS_CLOCK_DRIVER\n#define CONFIGURE_APPLICATION_NEEDS_CONSOLE_DRIVER\n\n#define CONFIGURE_SMP_APPLICATION\n\n#define CONFIGURE_SMP_MAXIMUM_PROCESSORS 32\n\n#define CONFIGURE_DISABLE_BSP_SETTINGS\n\n#define CONFIGURE_INIT\n\n"
   end (* structure RtemsMain *)