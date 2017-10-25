fun printGenList1 f l = case l of
			  x::xs => let val _ = f x in printGenList1 f xs end
			| []    => print("bigEND\n\n")
fun printL (x) = printGenList1 (fn(y) => print(y^"\n")) x

(* Datatypes *)

datatype stackVal = INT of int | STR of string 
                  | BOOL of bool | NAME of string 
				  | ERROR | UNIT
				  | CLOSURE of (stackVal * Command list * (stackVal * stackVal) list list)
				  | IOCLOSURE of (stackVal * Command list * (stackVal * stackVal) list list)
and Command = ADD | SUB | MUL | DIV | REM | SWAP | POP | NEG | PUSH of stackVal | QUIT (* Part 1 (arithmetic) *)
		         | AND | OR | NOT | IF | EQUAL | LESSTHAN | BIND | LET | END (* Part 2 (scopes, variables, and logic)*)
		         | FUN of stackVal * stackVal | INOUTFUN of stackVal * stackVal | FUNEND (* Part 3 (~functions~) *)
				 | RETURN | IO of {formalParam : stackVal, argument: stackVal} |RETURN_IO of {formalParam : stackVal, argument : stackVal} | CALL (* function specific commands *)

fun toString(x : stackVal) : string = case x of
	  INT(i) => if i < 0 then "-"^Int.toString(~i) else Int.toString(i)
	| STR(s)  => s
	| BOOL(b) => ":"^Bool.toString(b)^":"
	| NAME(n) => n
	| ERROR   => ":error:"
	| UNIT    => ":unit:"
	| CLOSURE _ => "CLOSURE" (* a closure may be returned from a function, but it won't be on your stack when the program terminates *)
	| IOCLOSURE _ => "IOCLOSURE"

(*************************************************************************************************)
(* Bind Lookup *)
(*************************************************************************************************)

fun eval (NAME(x), environment : (stackVal * stackVal) list list) : stackVal = (case environment of
		((NAME(y), value)::bs)::bss => if y = x then value else eval(NAME(x), bs::bss)
		| []::bss => eval(NAME(x),bss)
		| [] => ERROR
		| _  => (print("eval failed:  "^x^"\n"); ERROR))
    | eval (_, _) = ERROR

(*************************************************************************************************)
(* Parsing input *)
(*************************************************************************************************)
fun stripnewline (s : string) : string = let
	fun strip (chs : char list) : char list =
		case chs of
		#"\n"::cs => strip(cs)
		| c::cs     => c::strip(cs)
		| []        => []
	in
	implode(strip(explode s))
	end	
fun stripQuotes(s : string) : string = let
	fun stripHead (chs : char list) : char list =
		case chs of
			  #"\""::xs => xs
			| z         => z
	fun stripTail (chs : char list) : char list =
		case chs of
			  #"\""::[] => []
			| c::cs     => c::stripTail(cs)
			| []        => []
	in
	implode(stripTail(stripHead(explode s)))
	end
val digits = explode "0123456789"
val alphanum = explode("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")@digits
fun checkCh (set : char list, ch : char) : bool = 
	case set of
		  s::rest => if s = ch then true else checkCh (rest, ch)
		| []    => false

fun checkChs (set : char list, chs : char list) : bool = 
	case chs of
		  c::cs => if checkCh(set, c) then checkChs(set, cs) else false
		| []    => true
	
fun parsePositiveInt (s : string) : stackVal = if checkChs(digits, explode(s)) then INT(valOf(Int.fromString(s))) else ERROR
fun parseNegativeInt (s : string) : stackVal = if checkChs(digits, explode(s)) then INT(~(valOf(Int.fromString(s)))) else ERROR
fun parseName (s : string) : stackVal = if checkChs(alphanum, explode(s)) then NAME(s) else ERROR
fun parseBoolError (s : string) : stackVal = case explode(s) of
	  (#":")::(#"t")::cs => BOOL(true)
	| (#":")::(#"f")::cs => BOOL(false)
	| (#":")::(#"e")::cs => ERROR
	| _              => ERROR
	
fun parseLiteral(s : string) : stackVal = 
	case explode(s) of
		#"-"::cs => parseNegativeInt(implode(cs))
		| #"\""::cs => STR(stripQuotes(s))
		| c::cs => if checkCh(digits, c) then parsePositiveInt(s) else parseName(s)
		| _ => ERROR

(*************************************************************************************************)
(* FUNCTION PARSING *)
(*************************************************************************************************)
fun removeFunStrings(strs : string list, n) = case (strs,n) of
	  ("funEnd"::xs,0) => xs
	| ("funEnd"::xs,z) => removeFunStrings(xs, z-1)
	| (x::xs,_) => removeFunStrings (xs,n)
	| _ => [] (* we should never ever get here *)

    (* We need different behavior depending on the last command in an inOut function.
     * If we have a return, we need to perform a normal return routine, along with rebinding the name of the argument to the value of the formal parameter.
     * But if we have no return at the end of the function, then we don't perform a return routine, but we still have to rebind the argument at the call site.
     * The 2 commands introduced for inOut function takes care of both cases appropriately 
	 * (and can be thought of as different commands due to the nature of their respective executions).
	 *)	
fun mutateReturn(commands : Command list, FormalParameter : stackVal, NONE : stackVal option) = (case commands of
		  RETURN::[] => RETURN_IO({formalParam = FormalParameter, argument = ERROR})::[]
		| x::xs => x::(mutateReturn(xs, FormalParameter, NONE))
		| []         => IO({formalParam = FormalParameter, argument = ERROR})::[])
  | mutateReturn(commands : Command list, FormalParameter : stackVal, SOME s : stackVal option) = (case commands of
		  RETURN::[] => RETURN_IO({formalParam = FormalParameter, argument = s})::[]
		| x::xs => x::(mutateReturn(xs, FormalParameter, SOME s))
		| []         => IO({formalParam = FormalParameter, argument = s})::[])
(*************************************************************************************************)
(* PARSE STRINGS INTO COMMAND LIST *)
(*************************************************************************************************)
fun parseCommands ([] : string list) : Command list = []
  | parseCommands (s::rest : string list) : Command list = let
		val tokens = String.tokens (fn x => x = #" ") s (* creates a string list from the line of input, where each element was separated by a space *)
	in
		case tokens of
		  "push"::xs     => PUSH(parseLiteral(String.substring(s,5,size s - 5)))::parseCommands(rest)
		| "add"::xs      => ADD::parseCommands(rest)
		| "sub"::xs      => SUB::parseCommands(rest)
		| "mul"::xs      => MUL::parseCommands(rest)
		| "div"::xs      => DIV::parseCommands(rest)
		| "rem"::xs      => REM::parseCommands(rest)
		| "swap"::xs     => SWAP::parseCommands(rest)
		| "pop"::xs      => POP::parseCommands(rest)
		| ":false:"::xs  => PUSH(BOOL(false))::parseCommands(rest)
		| ":true:"::xs   => PUSH(BOOL(true))::parseCommands(rest)
		| ":error:"::xs  => PUSH(ERROR)::parseCommands(rest)
		| "neg"::xs      => NEG::parseCommands(rest)
		| "and"::xs      => AND::parseCommands(rest)
		| "or"::xs       => OR::parseCommands(rest)
		| "not"::xs      => NOT::parseCommands(rest)
		| "if"::xs       => IF::parseCommands(rest)
		| "equal"::xs    => EQUAL::parseCommands(rest)
		| "lessThan"::xs => LESSTHAN::parseCommands(rest)
		| "bind"::xs     => BIND::parseCommands(rest)
		| "let"::xs      => LET::parseCommands(rest)
		| "end"::xs      => END::parseCommands(rest)
		
		| "call"::xs     => CALL::parseCommands(rest)
		| "return"::xs   => RETURN::parseCommands(rest)
		| "quit"::xs     => QUIT::parseCommands(rest)
		
		| "fun"::funName::Arg::xs => FUN(parseLiteral(funName), parseLiteral(Arg))::parseCommands(rest)
	    | "inOutFun"::funName::Arg::xs => INOUTFUN(parseLiteral(funName), parseLiteral(Arg))::parseCommands(rest)
		| "funEnd"::xs   => FUNEND::parseCommands(rest)
		
		| _ => []
	end
	
(*************************************************************************************************)	
(* STACK MANIPULATION FUNCTION *)
(*************************************************************************************************)	
fun callPop (s::stk) = stk
  | callPop (stk)    = ERROR::stk

fun callSwap (s1::s2::stk) = s2::s1::stk
  | callSwap (stk)         = ERROR::stk
  
(*************************************************************************************************)	
(* ARITHMETIC FUNCTIONS *)
(*************************************************************************************************)

                                       (* ADD *)
fun callAdd (INT(x)::INT(y)::stk, bindings) = INT(x+y)::stk
  | callAdd (INT(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		INT(z) => INT(x+z)::stk
		| _    => ERROR::INT(x)::NAME(y)::stk)
  | callAdd (NAME(x)::INT(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		INT(z) => INT(y+z)::stk
		| _    => ERROR::NAME(x)::INT(y)::stk)
  | callAdd (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		(INT(z), INT(z')) => INT(z+z')::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callAdd (stk, bindings) = ERROR::stk
(**********************************************) 

                                       (* SUB *) 
fun callSub (INT(x)::INT(y)::stk, bindings) = INT(y-x)::stk
  | callSub (INT(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		INT(z) => INT(z-x)::stk
		| _    => ERROR::INT(x)::NAME(y)::stk)
  | callSub (NAME(x)::INT(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		INT(z) => INT(y-z)::stk
		| _    => ERROR::NAME(x)::INT(y)::stk)
  | callSub (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		(INT(z), INT(z')) => INT(z'-z)::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callSub (stk, bindings) = ERROR::stk
(**********************************************) 

                                       (* MUL *)
fun callMul (INT(x)::INT(y)::stk, bindings) = INT(x*y)::stk
  | callMul (INT(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		INT(z) => INT(x*z)::stk
		| _    => ERROR::INT(x)::NAME(y)::stk)
  | callMul (NAME(x)::INT(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		INT(z) => INT(y*z)::stk
		| _    => ERROR::NAME(x)::INT(y)::stk)
  | callMul (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		(INT(z), INT(z')) => INT(z*z')::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callMul (stk, bindings) = ERROR::stk
(**********************************************) 

                                       (* DIV *)		
fun callDiv (INT(0)::stk, bindings) = ERROR::INT(0)::stk
  | callDiv (INT(x)::INT(y)::stk, bindings) = INT(y div x)::stk
  | callDiv (INT(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		INT(z) => INT(z div x)::stk
		| _    => ERROR::INT(x)::NAME(y)::stk)
  | callDiv (NAME(x)::INT(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		INT(z) => INT(y div z)::stk
		| _    => ERROR::NAME(x)::INT(y)::stk)
  | callDiv (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		  (INT(0), INT(z')) => ERROR::NAME(x)::NAME(y)::stk
		| (INT(z), INT(z')) => INT(z' div z)::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callDiv (stk, bindings) = ERROR::stk 	
(**********************************************) 

                                       (* REM *)		
fun callRem (INT(0)::stk, bindings) = ERROR::INT(0)::stk
  | callRem (INT(x)::INT(y)::stk, bindings) = INT(y mod x)::stk
  | callRem (INT(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		INT(z) => INT(z mod x)::stk
		| _    => ERROR::INT(x)::NAME(y)::stk)
  | callRem (NAME(x)::INT(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		  INT(0) => ERROR::NAME(x)::INT(y)::stk
		| INT(z) => INT(y mod z)::stk
		| _      => ERROR::NAME(x)::INT(y)::stk)
  | callRem (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		  (INT(0), INT(z')) => ERROR::NAME(x)::NAME(y)::stk
		| (INT(z), INT(z')) => INT(z+z')::stk
		| _                 => ERROR::NAME(x)::NAME(y)::stk)
  | callRem (stk, bindings) = ERROR::stk
(**********************************************) 

                                       (* NEG *)		
fun callNeg (INT(x)::stk, bindings) = INT(~x)::stk
  | callNeg (NAME(x)::stk, bindings) = (case eval(NAME(x), bindings) of
		INT(z) => INT(~z)::stk
		| _    => ERROR::NAME(x)::stk)
  | callNeg (stk, bindings) = ERROR::stk
(**********************************************) 

(*************************************************************************************************)
(* BOOLEAN FUNCTIONS *)
(*************************************************************************************************)

fun callAnd (BOOL(x)::BOOL(y)::stk, bindings) = BOOL(x andalso y)::stk
  | callAnd (BOOL(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		BOOL(z) => BOOL(x andalso z)::stk
		| _    => ERROR::BOOL(x)::NAME(y)::stk)
  | callAnd (NAME(x)::BOOL(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		BOOL(z) => BOOL(y andalso z)::stk
		| _    => ERROR::NAME(x)::BOOL(y)::stk)
  | callAnd (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		(BOOL(z), BOOL(z')) => BOOL(z andalso z')::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callAnd (stk, bindings) = ERROR::stk
 
fun callOr (BOOL(x)::BOOL(y)::stk, bindings) = BOOL(x orelse y)::stk
  | callOr (BOOL(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		BOOL(z) => BOOL(x orelse z)::stk
		| _    => ERROR::BOOL(x)::NAME(y)::stk)
  | callOr (NAME(x)::BOOL(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		BOOL(z) => BOOL(y orelse z)::stk
		| _    => ERROR::NAME(x)::BOOL(y)::stk)
  | callOr (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		(BOOL(z), BOOL(z')) => BOOL(z orelse z')::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callOr (stk, bindings) = ERROR::stk
 
fun callEqual (INT(x)::INT(y)::stk, bindings) = BOOL(x=y)::stk
  | callEqual (INT(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		INT(z) => BOOL(x=z)::stk
		| _    => ERROR::INT(x)::NAME(y)::stk)
  | callEqual (NAME(x)::INT(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		INT(z) => BOOL(y=z)::stk
		| _    => ERROR::NAME(x)::INT(y)::stk)
  | callEqual (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		(INT(z), INT(z')) => BOOL(z=z')::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callEqual (stk, bindings) = ERROR::stk
  
fun callLessThan (INT(x)::INT(y)::stk, bindings) = BOOL(y < x)::stk
  | callLessThan (INT(x)::NAME(y)::stk, bindings) = (case eval(NAME(y), bindings) of
		INT(z) => BOOL(z < x)::stk
		| _    => ERROR::INT(x)::NAME(y)::stk)
  | callLessThan (NAME(x)::INT(y)::stk, bindings) = (case eval(NAME(x), bindings) of
		INT(z) => BOOL(y < z)::stk
		| _    => ERROR::NAME(x)::INT(y)::stk)
  | callLessThan (NAME(x)::NAME(y)::stk, bindings) = (case (eval(NAME(x), bindings), eval(NAME(y), bindings)) of
		(INT(z), INT(z')) => BOOL(z' < z)::stk
		| _               => ERROR::NAME(x)::NAME(y)::stk)
  | callLessThan (stk, bindings) = ERROR::stk

fun callIf (x::y::BOOL(z)::stk, bindings) = (case z of
	  true  => x::stk
	| false => y::stk)
  | callIf (x::y::NAME(z)::stk, bindings) = (case eval(NAME(z),bindings) of
	  BOOL(true)  => x::stk
	| BOOL(false) => y::stk
	| _           => ERROR::x::y::NAME(z)::stk)
  | callIf (stk, bindings) = ERROR::stk

fun callNot (BOOL(x)::stk, bindings) = BOOL(not x)::stk
  | callNot (NAME(x)::stk, bindings) = (case eval(NAME(x), bindings) of
		BOOL(z) => BOOL(not z)::stk
		| _    => ERROR::NAME(x)::stk)
  | callNot (stk, bindings) = ERROR::stk
  
fun callBind (ERROR::stk, currentScope::bindings) = (ERROR::ERROR::stk, currentScope::bindings)
  | callBind (NAME(x)::NAME(y)::stk, currentScope::bindings) = (case eval(NAME(x), currentScope::bindings) of
          ERROR => (ERROR::NAME(x)::NAME(y)::stk, currentScope::bindings)
		| z     => (UNIT::stk, ((NAME(y), z)::currentScope)::bindings))
  | callBind (x::NAME(y)::stk, currentScope::bindings) = (UNIT::stk, ((NAME(y), x)::currentScope)::bindings)
  | callBind (stk, bindings) = (ERROR::stk, bindings)
  
fun parseFunction((c::commands)::rest, stk::stks, (topBinds::bindings)::B) = let
	fun parser(comms, n) = case (comms, n) of
		  (FUNEND::cs,0)           => []
		| (FUNEND::cs,_)           => FUNEND::parser(cs, n-1)
		| (FUN(name,f)::cs,_)      => FUN(name,f)::parser(cs, n+1)
		| (INOUTFUN(name,f)::cs,_) => INOUTFUN(name,f)::parser(cs, n+1)
		| (x::cs,_)                => x::parser(cs, n)
	 
	fun getRest (comms, n) =  case (comms, n) of
		  (FUNEND::cs,0)      => cs
		| (FUNEND::cs,_)      => getRest(cs, n-1)
		| (FUN(_)::cs,_)      => getRest(cs, n+1)
		| (INOUTFUN(_)::cs,_) => getRest(cs, n+1)
		| (x::cs,_)           => getRest(cs, n)
	in
	case c of
	  FUN(funName, fp)   => (getRest(commands, 0)::rest, (UNIT::stk)::stks, (((funName, (CLOSURE(fp, parser(commands, 0), topBinds::bindings)))::topBinds)::bindings)::B)
	| INOUTFUN(funName, fp) => (getRest(commands, 0)::rest, (UNIT::stk)::stks, (((funName, (IOCLOSURE(fp, parser(commands, 0), topBinds::bindings)))::topBinds)::bindings)::B)
	end

fun functionCall(commands, (x::ERROR::stk)::stks, B) = (commands, (ERROR::x::ERROR::stk)::stks, B)
  | functionCall(commands, (NAME(funName)::NAME(Arg)::stk)::stks, bindings::B) = (case (eval(NAME(Arg), bindings), eval(NAME(funName), bindings)) of
		  (ERROR, _) => (commands, (ERROR::NAME(funName)::NAME(Arg)::stk)::stks, bindings::B)
		| (x, CLOSURE(FP, funCommands, topBinds::rest))   => (funCommands::commands, []::stk::stks, (((FP,x)::(NAME(funName), CLOSURE(FP, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B)
	    | (x, IOCLOSURE(FP, funCommands, topBinds::rest)) => (mutateReturn(funCommands, FP, SOME (NAME (Arg)))::commands, []::stk::stks, (((FP,x)::(NAME(funName),IOCLOSURE(FP, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B)
		| (_,_) => (commands, (ERROR::NAME(funName)::NAME(Arg)::stk)::stks, bindings::B))
  | functionCall (commands, (NAME(funName)::(x)::stk)::stks, bindings::B) = (case eval(NAME(funName), bindings) of
		  ERROR => (commands, (ERROR::NAME(funName)::(x)::stk)::stks, bindings::B)
		| CLOSURE(Arg, funCommands, topBinds::rest)   => (funCommands::commands, []::stk::stks, (((Arg,x)::(NAME(funName),CLOSURE(Arg, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B)
	    | IOCLOSURE(Arg, funCommands, topBinds::rest) => (mutateReturn(funCommands, Arg, SOME x)::commands, []::stk::stks, (((Arg,x)::(NAME(funName),IOCLOSURE(Arg, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B)
		| _ => (commands, (ERROR::NAME(funName)::(x)::stk)::stks, bindings::B))
  
  (* REGULAR FUNCTION ON STACK - THESE WEREN'T TESTED FOR.
   * The design document does indeed allow for functions to be returned from functions. *)
  | functionCall(commands, (CLOSURE(FP, funCommands, topBinds::rest)::NAME(Arg)::stk)::stks, bindings::B) = (case eval(NAME(Arg), bindings) of
		  ERROR => (commands, (ERROR::IOCLOSURE(FP, funCommands, topBinds::rest)::NAME(Arg)::stk)::stks, bindings::B)
		| x     => (funCommands::commands, []::stk::stks, (((FP,x)::(ERROR, CLOSURE(FP, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B))
  | functionCall (commands, (CLOSURE(FP, funCommands, topBinds::rest)::(x)::stk)::stks, bindings::B) =
		  (funCommands::commands, []::stk::stks, (((FP,x)::(ERROR, CLOSURE(FP, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B)
  
  (* INOUT FUNCTION ON STACK - THESE WEREN'T TESTED FOR.
   * The design document does indeed allow for functions to be returned from functions. *)
  | functionCall(commands, (IOCLOSURE(FP, funCommands, topBinds::rest)::NAME(Arg)::stk)::stks, bindings::B) = (case (eval(NAME(Arg), bindings)) of
		  ERROR => (commands, (ERROR::IOCLOSURE(FP, funCommands, topBinds::rest)::NAME(Arg)::stk)::stks, bindings::B)
		| x     => (mutateReturn(funCommands, FP, SOME (NAME (Arg)))::commands, []::stk::stks, (((FP,x)::(ERROR,IOCLOSURE(FP, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B))
  | functionCall (commands, (IOCLOSURE(FP, funCommands, topBinds::rest)::(x)::stk)::stks, bindings::B) = 
	    (mutateReturn(funCommands, FP, SOME x)::commands, []::stk::stks, (((FP,x)::(ERROR,IOCLOSURE(FP, funCommands, topBinds::rest))::topBinds)::rest)::bindings::B)
  
  | functionCall (commands, stk::stks, B) = (commands, (ERROR::stk)::stks, B)
  | functionCall (commands, [], B) = (commands, [[ERROR]], B) (* This should never happen *)
  
fun functionReturn(comms::rest, (s::stk)::stk2::stks, funBinds::B) = (case eval(s, funBinds) of 
	  ERROR => (rest, (s::stk2)::stks, B)
	| x     => (rest, (x::stk2)::stks, B))
  | functionReturn(comms::rest, stk::stk2::stks, funBinds::B) = (rest, stk2::stks, B)
  | functionReturn([], stk::stks, funBinds::B) = ([], []::stks, B)
  | functionReturn([], [], B) = ([], [[]], B)
 
fun functionIOReturn(comms::rest, (s::stk)::stk2::stks, (funBinds)::(topBinds::nextBinds)::B, formalparameter, argument) =
	case (eval(formalparameter, funBinds), argument, eval(s,funBinds)) of
		(ERROR, _, ERROR)   => (rest, (s::stk2)::stks, (topBinds::nextBinds)::B)
	  | (ERROR, _, x)       => (rest, (x::stk2)::stks, (topBinds::nextBinds)::B)
	  | (z, NAME(y), ERROR) => (rest, (s::stk2)::stks, (((NAME y,z)::topBinds)::nextBinds)::B)
	  | (z, NAME(y), x)     => (rest, (x::stk2)::stks, (((NAME y,z)::topBinds)::nextBinds)::B)
      | (_, _, ERROR)       => (rest, (s::stk2)::stks, (topBinds::nextBinds)::B)
	  | (_, _, x)           => (rest, (x::stk2)::stks, (topBinds::nextBinds)::B)
  
fun functionIO(comms::rest, stk::stks, (funBinds)::(topBinds::nextBinds)::B, formalparameter, argument) =
	case (eval(formalparameter, funBinds), argument) of
		(ERROR, _)   => (rest, stks, (topBinds::nextBinds)::B)
	  | (x, NAME(y)) => (rest, stks, (((NAME y,x)::topBinds)::nextBinds)::B)
      | (_, _)       => (rest, stks, (topBinds::nextBinds)::B)
  
fun interpret((c::comms)::rest : Command list list, stack::stacks : stackVal list list, mainBinds::B : (stackVal * stackVal) list list list) : stackVal list = case c of
	  PUSH(x)  => interpret(comms::rest, (x::stack)::stacks, mainBinds::B)
	| ADD      => interpret(comms::rest, callAdd(stack, mainBinds)::stacks, mainBinds::B)
	| SUB      => interpret(comms::rest, callSub(stack, mainBinds)::stacks, mainBinds::B)
	| MUL      => interpret(comms::rest, callMul(stack, mainBinds)::stacks, mainBinds::B)
	| DIV      => interpret(comms::rest, callDiv(stack, mainBinds)::stacks, mainBinds::B)
	| REM      => interpret(comms::rest, callRem(stack, mainBinds)::stacks, mainBinds::B)
	| SWAP     => interpret(comms::rest, callSwap(stack)::stacks, mainBinds::B)
	| POP      => interpret(comms::rest, callPop(stack)::stacks, mainBinds::B)
	
	| NEG      => interpret(comms::rest, callNeg(stack, mainBinds)::stacks, mainBinds::B)
	| AND      => interpret(comms::rest, callAnd(stack, mainBinds)::stacks, mainBinds::B)
	| OR       => interpret(comms::rest, callOr(stack, mainBinds)::stacks, mainBinds::B)
	| NOT      => interpret(comms::rest, callNot(stack, mainBinds)::stacks, mainBinds::B)
	| IF       => interpret(comms::rest, callIf(stack, mainBinds)::stacks, mainBinds::B)
	| EQUAL    => interpret(comms::rest, callEqual(stack, mainBinds)::stacks, mainBinds::B)
	| LESSTHAN => interpret(comms::rest, callLessThan(stack, mainBinds)::stacks, mainBinds::B)
	| BIND     => (case callBind(stack, mainBinds) of 
						(newStack, newBinds) => interpret(comms::rest, newStack::stacks, newBinds::B))
	| LET      => interpret(comms::rest, []::stack::stacks, ([]::mainBinds)::B)
	| END      => (case (stack::stacks, mainBinds::B) of 
					((s::stk)::stk2::stks, (topbinds::restbinds)::B) => interpret(comms::rest, (s::stk2)::stks, restbinds::B)
					| (stk::stks, (topbinds::restbinds)::B) => interpret(comms::rest, stks, restbinds::B)
					| _ => interpret(comms::rest, stacks, B)) (* this should definitely never happen *)
	
	| FUN(_)      => interpret(parseFunction((c::comms)::rest, stack::stacks, mainBinds::B)) 
	| INOUTFUN(_) => interpret(parseFunction((c::comms)::rest, stack::stacks, mainBinds::B)) 
	| RETURN      => interpret(functionReturn(comms::rest, stack::stacks, mainBinds::B))
	| CALL               => interpret(functionCall(comms::rest, stack::stacks, mainBinds::B))
	| RETURN_IO({formalParam = p, argument = a}) => interpret(functionIOReturn(comms::rest, stack::stacks, mainBinds::B, p, a))
    | IO({formalParam = p, argument = a})        => interpret(functionIO(comms::rest, stack::stacks, mainBinds::B, p, a))
	
	| QUIT               => (stack)
	| _                  => []
fun interpreter (inputFile : string, outputFile : string) = let
		fun printGenList f l = case l of
			  x::xs => let val _ = f x in printGenList f xs end
			| []    => ()
		fun print2L (x) = printGenList (fn(y) => print(y^"\n")) x
		fun printList (file, l) = let
				val outs = TextIO.openOut file 
			in
				(printGenList (fn x => TextIO.output(outs, x^"\n")) l; TextIO.closeOut(outs))
			end
		val ins = TextIO.openIn inputFile
		(*val outs = TextIO.openOut outputFile*)
		fun inputList instream = case (TextIO.inputLine instream) of
		  	  SOME(s) => stripnewline(s)::inputList(instream)
			| NONE => (TextIO.closeIn instream; [])
		val inList = inputList ins
		val commands = parseCommands(inList)
		val finalStack = interpret([commands], [[]], [[[]]]) handle _ => (print("uh oh spaghetti-o's\n");[])
		val stringList = map toString finalStack
	in
		print2L(stringList)
	end
	
val _ = interpreter("input/input15.txt", "output15.txt");
