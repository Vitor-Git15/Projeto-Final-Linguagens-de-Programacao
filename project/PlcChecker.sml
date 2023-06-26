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

fun allEquals (l: plcType list): bool =
  let
    val target = hd l
  in
    List.all (fn x => x = target) l
  end

fun checkEqualityOperatorDefined (t: plcType) = 
  case t of
      IntT         => true
    | BoolT        => true
    | ListT []     => true
    | ListT (h::t) => 
        if (checkEqualityOperatorDefined h)
        then (checkEqualityOperatorDefined (ListT t))
        else false
    | SeqT s       => checkEqualityOperatorDefined s
    | _            => false

fun teval (e: expr) (env: plcType env): plcType = 
  case e of
      ConI i => IntT

    | ConB b => BoolT

    | ESeq t => ListT [] (* verificar *)

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
          if t1 = tr then t2 else raise WrongRetType
        end

    | Prim1(opr, e1) =>
        let
          val t1 = teval e1 env
        in
          case (opr, t1) of
              ("print", _)             => ListT []
            | ("!", BoolT)             => BoolT
            | (("hd" | "tl"), SeqT ts) => ts
            | (("hd" | "tl"), _)       => raise OpNonList
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
            | ("::", _, _) => raise NotEqTypes
            | ("&&", BoolT, BoolT) => BoolT
            | ("&&", _, _) => raise NotEqTypes
            | (("=" | "!="), _, t1) => 
                if (checkEqualityOperatorDefined t1)
                then BoolT
                else raise NotEqTypes
            | _ => raise UnknownType
        end
    
    | If(c, e1, e2) =>
        let 
          val tc = teval c env
          val t1 = teval e1 env
          val t2 = teval e2 env
        in 
          if not (tc = BoolT) then raise IfCondNotBool
          else if not (t1 = t2) then raise DiffBrTypes
          else t1
        end

    | Match(v, l) =>
        (case l of
            [] => raise NoMatchResults
          | _  => 
            let
              val conditions_types = map(fn (SOME cond,_) => teval cond env | (_,_) => raise UnknownType) l
              val return_types = map(fn (_,res) => teval res env) l
              val expression_type = teval v env
            in
              if not (allEquals conditions_types) then raise MatchCondTypesDiff
              else if expression_type = hd conditions_types then raise DiffBrTypes
              else if not (allEquals return_types) then raise MatchResTypeDiff
              else hd return_types
            end)

    | Call (e1, e2) =>
        let
          val t1 = teval e1 env
          val t2 = teval e2 env
        in
          case t1 of
              FunT (t2, tr) => tr
            | FunT (_, _)   => raise CallTypeMisM
            | _            => raise NotFunc
        end (* verificar *)

    | List(e1) =>
        (case e1 of
            [] => ListT []
          | _  => ListT (map(fn x => teval x env) e1))

    | Item(i, e1) =>
        let 
          val t1 = teval e1 env
        in
          case t1 of
              ListT l => 
                if ((i > 0) andalso (i <= List.length(l)))
                then List.nth(l, i-1)
                else raise ListOutOfRange
            | _      => raise OpNonList
        end

    | Anon(t, v, e1) => 
      let
        val t1 = teval e1 ((v, t)::env)
      in
        FunT (t, t1)
      end