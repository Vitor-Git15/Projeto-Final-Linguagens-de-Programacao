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

fun checkEqualityOperatorDefined (t: plcType) = 
  case t of
      IntT         => true
    | BoolT        => true
    | ListT []     => true
    | ListT (h::t) => 
        if (checkEqualityOperatorDefined h)
        then (checkEqualityOperatorDefined t)
        else false
    | SeqT s       => checkEqualityOperatorDefined s
    | _            => false

fun teval (e: expr) (env: plcType env): plcType = 
  case e of

      ConI i => IntT

    | ConB b => BoolT

    | ESeq(SeqT(s)) => SeqT s 

    | Var x => lookup env x

    | Let(x, e1, e2) =>
        let
          val t = teval e1 env
          val env' = (x, t)::env
        in
          teval e2 env'
        end

    | Letrec(f, targ, narg, tr, e1, e2) =>
        let
          val t1 = teval e1 ((f, FunT(targ, tr))::(narg, targ)::env)
          val t2 = teval e2 ((f, FunT(targ, tr))::env)
        in
          case t1 of
              tr => t2
            | _  => WrongRetType

    | Prim1(opr, e1) =>
        let
          val t1 = teval e1 env
        in
          case (opr, t1) of
              ("print", _)             => ListT []
            | ("!", BoolT)             => BoolT
            | (("hd" | "tl"), SeqT ts) => ts
            | (("hd" | "tl"), _)       => OpNonList
            | ("-", IntT)              => IntT
            | ("ise", SeqT _)          => BoolT
            | _                        => raise UnknownType
        end
    
    | Prim2(opr, e1, e2) =>
        let
          val t1 = teval e1 env
          val t2 = teval e2 env
        in
          case (opr, t1, t2) of
              (("*" | "/" | "+" | "-"), IntT, IntT) => IntT
            | (";", _, _) => t2
            | (("<" | "<="), IntT, IntT) => BoolT
            | ("::", _, SeqT t1) => SeqT t1
            | ("::", _, _) => NotEqTypes
            | ("&&", BoolT, BoolT) => BoolT
            | ("&&", _, _) => NotEqTypes
            | (("=" | "!="), _, t1) => 
                if (checkEqualityOperatorDefined t1)
                then BoolT
                else NotEqTypes
            | _ => raise UnknownType
        end
    
    | If(c, e1, e2) =>
        let 
          val tc = teval c env
          val t1 = teval e1 env
          val t2 = teval e2 env
        in 
          case (tc, t1, t2) of
              (BoolT, _, t1) => t1
            | (BoolT, _, _)  => raise DiffBrTypes
            | (_, _, _)      => raise IfCondNotBool
            | _              => raise UnknownType
        end
    (*Implement*)
    | Match(v, l) =>
        let
          val t1 = teval e1 env
          val t2 = teval e2 env
        in 
        
    (*----------*)

    | Call(e1, e2) =>
        let
          val t1 = teval e1 env
          val t2 = teval e2 env
        in
          case t1 of
              FunT(t2, tr) => tr
            | FunT(_, _)   => CallTypeMisM
            | _            => NotAFunc
        end
    (*Implement*)
    | List(e1) =>
        case e1 of
            [] => ListT []
          | _  =>
              let
                val t1 = teval e1 env
              in
                case t1 of
                  (h::t) => ListT((teval h env)::(teval h env))
              end
    (*----------*)

    | Item(i, e1) =>
        let 
          val t1 = teval e1 env
        in
          case t1 of
              List l => 
                if ((i > 0) andalso (i <= List.length(l)))
                then List.nth(l, i-1)
                else ListOutOfRange
            | _      => OpNonList
        end

    | Anon(t, v, e1) => FunT(t, teval e1 ((v, t)::env))

    | _ raise => UnknownType