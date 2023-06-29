(* Plc interpreter main file *)


fun run (e: expr) = 
  let
    val eType = teval e []
    val eValue = eval e []
  in
    (val2string eValue) ^ " : " ^ (type2string eType)
  end
  handle SymbolNotFound        => "Symbol not found."
       | EmptySeq              => "PLC Checker: Empty sequence should have a sequence type."
       | UnknownType           => "PLC Checker: Unknown operator, or type error."
       | NotEqTypes            => "PLC Checker: Types in comparison are different."
       | WrongRetType          => "PLC Checker: Wrong return type in function."
       | DiffBrTypes           => "PLC Checker: 'if' branch types differ."
       | IfCondNotBool         => "PLC Checker: 'if' condition not Boolean."
       | NoMatchResults        => "PLC Checker: No match results."
       | MatchResTypeDiff      => "PLC Checker: 'match' result types differ."
       | MatchCondTypesDiff    => "PLC Checker: 'match' condition types differ matching expression's type."
       | CallTypeMisM          => "PLC Checker: Type mismatch in function call."
       | ListOutOfRange        => "PLC Checker: List index out of range."
       | OpNonList             => "PLC Checker: Selection with operator # applied to non-list."
       | NotAFunc              => "teval CALL: Not a function."
       | NotFunc               => "eval CALL: Not a function."
       | Impossible            => "PLC Interp: Impossible evaluate expression."
       | HDEmptySeq            => "PLC Interp: 'hd' empty sequence argument."
       | TLEmptySeq            => "PLC Interp: 'tl' empty sequence argument."
       | ValueNotFoundInMatch  => "PLC Interp: Value not found in match."
       | _                     => "Unexpected exception.";
       