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

fun allEqualOrNone xs =
  let
    fun checkList [] = true
      | checkList (x::xs) =
          case xs of
            [] => true
          | y::ys => (x = y orelse y = NONE) andalso checkList xs
  in
    case xs of
      [] => true
    | x::xs => checkList xs
  end

fun list_type l =
  case l of
      [] => NONE
    | NONE::xt => list_type xt
    | x::_ => x

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

    | ESeq t => 
        (case t of
            SeqT s => SeqT s
          | _ => raise EmptySeq)

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
            | ("hd", SeqT ts) => ts
            | ("tl", SeqT ts) => SeqT ts
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
            | ("::", t1, t2) =>
                (case (t1, t2) of
                    (IntT, ListT []) => SeqT IntT
                  | (BoolT, ListT []) => SeqT BoolT
                  | (IntT, SeqT s) => if s = IntT then SeqT IntT else raise NotEqTypes
                  | (BoolT, SeqT s) => if s = BoolT then SeqT BoolT else raise NotEqTypes
                  | (ListT l, ListT []) => SeqT (ListT l)
                  | (ListT l, SeqT s) => if s = ListT l then SeqT s else raise NotEqTypes
                  | _ => raise UnknownType
                  ) 
            | ("&&", BoolT, BoolT) => BoolT
            | ("&&", _, _) => raise NotEqTypes
            | (("=" | "!="), t1, t2) => 
                if t1 = t2
                then
                  if (checkEqualityOperatorDefined t1)
                  then BoolT
                  else raise UnknownType
                else
                  raise NotEqTypes
            | _ => raise UnknownType
        end
    
    | If(c, e1, e2) =>
        let 
          val tc = teval c env
          val t1 = teval e1 env
          val t2 = teval e2 env
        in 
          if (tc <> BoolT) then raise IfCondNotBool
          else if (t1 <> t2) then raise DiffBrTypes
          else t1
        end

    | Match (e1, e2) =>
        (case e2 of
            [] => raise NoMatchResults
          | _ =>
            let 
              val expr_type = teval e1 env
              val conditions_types = map(fn (SOME cond,_) => SOME (teval cond env) | (NONE,_) => NONE) e2
              val cond_types = list_type conditions_types
              val return_types = map(fn (_,res) => teval res env) e2
            in
              if not (allEqualOrNone conditions_types) then raise MatchCondTypesDiff
              else if 
                case cond_types of
                    NONE => false
                  | SOME value => not (expr_type = value) 
              then raise MatchCondTypesDiff 
              else if not(allEquals return_types) then raise MatchResTypeDiff
              else hd return_types
            end)
    
    | Call (Var(e2), e1) =>
        let
            val mayBeFunType = lookup env e2;
            val t1 = teval e1 env;
        in 
            case mayBeFunType of FunT(argT, retT) => if t1 = argT then retT else raise CallTypeMisM 
                | _ => raise NotFunc
        end 
    | Call (Call(e1), e2) =>    
        let
          val t1 = teval (Call(e1)) env
        in
          case t1 of
              FunT(_, tr) => tr
            | _           => raise NotFunc
        end 
    | Call (_, _) => raise NotFunc
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
      end;
