val ins = TextIO.openIn("input/input1.txt");
fun prints ios = case TextIO.inputLine(ios) of
                    SOME(s) => (print(s); prints(ios))
                    | NONE => (print("done!"); TextIO.closeIn(ios));
prints(ins);
