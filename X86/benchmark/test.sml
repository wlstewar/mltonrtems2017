val n = [1,
         2,
         4,
         6,
         8,
         12,
         16,
         24,
         32,
         48,
         64,
         96,
         128,
         192,
         256,
         384,
         512,
         768,
         1024,
         1536,
         2048,
         3072,
         4096,
         6144,
         8192,
         12288,
         16384,
         24576]

fun goforit () = List.app (fn x => Main.doit(x)) n;
val start = Time.now()
val _ = goforit()
val fin = Time.now()
val _ = print("Total time:  " ^Time.toString(fin - start) ^ " seconds\n")
