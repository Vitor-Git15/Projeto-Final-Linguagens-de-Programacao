(* PlcInterp *)

exception ThisImpossible
exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e: expr) (env: plcVal env): plcVal =
  case e of

      ConI i => IntV i

    | ConB b => BoolV b

    | ESeq _ => SeqV []
      
    | Var x => lookup env x
    
    | Prim1(opr, e1) =>
        let
          val v1 = eval e1 env
        in
          case (opr, v1) of
              ("print", _) => 
                let
                  val s = val2string v1
                in
                  print(s^"\n"); ListV []
                end
            | ("!", BoolV b1)          => BoolV(not(b1)) 
            | ("hd", SeqV vs) =>
                (case vs of
                    []      => raise HDEmptySeq
                  | (h::t) => h)
            | ("tl", SeqV vs) => 
                (case vs of
                    []      => raise TLEmptySeq
                  | (h::t) => SeqV t)
            | ("-", IntV i)     => IntV(~i)
            | ("ise", SeqV vs)  => BoolV(vs = [])
            | _ => raise Impossible
        end

    | Prim2(opr, e1, e2) =>
        let 
          val v1 = eval e1 env
          val v2 = eval e2 env
        in 
          case (opr, v1, v2) of
              ("*", IntV i1, IntV i2)    => IntV(i1 * i2)
            | ("/", IntV i1, IntV i2)    => IntV(i1 div i2)
            | ("+", IntV i1, IntV i2)    => IntV(i1 + i2)
            | ("-", IntV i1, IntV i2)    => IntV(i1 - i2)
            | (";", _, _) => v2
            | ("::", _, _) => 
                (case (v1, v2) of 
                    (IntV _, SeqV s)  => SeqV(v1::s)
                  | (BoolV _, SeqV s) => SeqV(v1::s)
                  | (ListV _, SeqV s)  => SeqV(v1::s)
                  | _ => raise Impossible)
            | ("=", IntV i1, IntV i2)    => BoolV(i1 = i2)
						| ("=", BoolV b1, BoolV b2)    => BoolV(b1 = b2)
            | ("=", ListV [], ListV [])    => BoolV(true)
            | ("=", SeqV [], SeqV [])    => BoolV(true)
            | ("<", IntV i1, IntV i2)    => BoolV(i1 < i2)
						| ("<=", IntV i1, IntV i2)   => BoolV(i1 <= i2)
						| ("!=", IntV i1, IntV i2)   => BoolV(i1 <> i2)
            | ("!=", BoolV b1, BoolV b2)    => BoolV(b1 <> b2)
            | ("!=", SeqV [], SeqV [])    => BoolV(false)
            | ("!=", ListV [], ListV [])    => BoolV(false)
            | ("&&", BoolV b1, BoolV b2) => BoolV(b1 andalso b2)
            | _ => raise Impossible
        end

    | Let(x, e1, e2) =>
        let
          val v = eval e1 env
          val env' = (x,v)::env
        in
          eval e2 env'
        end

    | Letrec(f, targ, nargs, tr, e1, e2) =>
        let
          val c = Clos(f, nargs, e1, env)
          val env' = (f, c)::env
        in
          eval e2 env'
        end

    | If(c,e1,e2) =>
      let
        val res = eval c env
      in
        case res of
            BoolV true => eval e1 env
          | BoolV false => eval e2 env
          | _ => raise Impossible
      end

    | Match(e1, e2) => 
        let
          fun matcher [] = raise ValueNotFoundInMatch
            | matcher ((SOME m, r)::t) =
                if eval e1 env = eval m env
                then eval r env
                else matcher t
            | matcher ((NONE, r)::t) = eval r env
        in
          matcher e2
        end

    | Call(e1,e2) => 
      let 
      
        fun evalArgs (List []) = []
          | evalArgs (List [x]) = [eval x env]
          | evalArgs (List (h::t)) = [eval h env] @ evalArgs (List t)
          | evalArgs x = [eval x env]
        
        val env' = [("$list", ListV (evalArgs e2))] @ env
        val v1 = eval e1 env
      in
        case v1 of
            Clos(f, var, e, cenv) => 
              let
                val v2 = eval e2 env'
                val env'' = (var, v2)::(f, v1)::cenv
              in
                eval e env''
              end
          | _ => raise NotAFunc
      end

    | List [] => ListV []
    | List el => 
      let
        fun evalList ([x]) = eval x env :: []
			    | evalList (h::t) = eval h env :: evalList t
			    | evalList (_) = raise Impossible;
      in
        ListV (evalList(el))
      end

    | Item (i, el) => 
      let
        val l = eval el env

        fun getElement (_, []) = raise Impossible 
          | getElement (1, h::t) = h
          | getElement (n, _::t) = getElement(n-1, t)
      in
        case (i, l) of
            (_, ListV list) => getElement(i, list)
          | (_, SeqV seq) => getElement(i, seq)
          | _ => raise Impossible 
      end
    | Anon(t, x, e) => 
        Clos("", x, e, env);
