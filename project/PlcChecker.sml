(* PlcChecker *)

exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList

fun teval (e: expr) (env: plcTrype env): plcType = 
  case e of

      ConI i => IntT

    | ConB b => BoolT

    | ConB b => BoolT

    (* | ESeq es => SeqV *) 

    | Var x => lookup env x

    | Let(x, e1, e2) =>
        let
          val t = teval e1 env
          val env' = (x, t)::env
        in
          teval e2 env'
        end

    (* | Letrec(s1, p1, s2, p2, e1, e2) *)

    | Prim(opr, e1) =>
        let
          val t1 = teval e1 env
        in
          case (opr, t1) of
              ("print", _) => ListT []
            | _ => raise UnknownType
        end
    
    | Prim2(opr, e1, e2) =>
        let
          val t1 = teval e1 env
          val t2 = teval e2 env
        in
          case (opr, t1, t2) of
              ("*", IntT, IntT) => IntT
            | ("/", IntT, IntT) => IntT
            | ("+", IntT, IntT) => IntT
            | ("-", IntT, IntT) => IntT
            | (";", _, _) => t2
            | _ => raise UnknownType
        end
    
    | If(c, e1, e2) =>
        let 
          val tc = teval c env
          val t1 = teval e1 env
          val t2 = teval e2 env
        in 
          case (tc, t1, t2) of
              (BoolT, t2, t1) => t1
            | (_, t2, t1)     => raise IfCondNotBool
            | (_, _, _)       => raise DiffBrTypes
            | _               => raise UnknownType
        end
    
    | Match(v, l) =>
        let
          val tl = teval l env
        in 


          

    | _ raise => UnknownType