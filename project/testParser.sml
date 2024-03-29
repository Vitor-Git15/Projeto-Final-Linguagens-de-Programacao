(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fromString "15";
fromString "true";
fromString "()";
fromString "(6,false)[1]";
fromString "([Bool] [])"; (* check *)
fromString "print x; true"; 
fromString "3::7::t";
fromString "fn (Int x) => -x end";
fromString "var x = 9; x + 3";
fromString "fun f(Int x) = x; f(1)";
fromString "match x with | 0 -> 1| _ -> -1 end"; (* check *)
fromString "fun rec f1(Int x):Int = x + 1; f1(12)"

fromFile ("example.plc");

use "testParserCases.sml";

(* Try to add a systematic way of using the test cases in
   testParserCases to stress test your parser *)

fun test((s, _)::[]) = [fromString s] 
   | test((s, _)::t) = (fromString s)::(test(t));

fun getAnswers((_, e)::[]) = [e]
   | getAnswers((_, e)::t) = e::(getAnswers(t));

val results = test cases;
val answers = getAnswers cases;