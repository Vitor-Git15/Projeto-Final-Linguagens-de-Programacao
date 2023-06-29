(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e: expr) (env: plcVal env): plcVal =
  case e of

      ConI i => IntV i

    | ConB b => BoolV b

    | ESeq s => (* TODO *)
      
    | Var x => lookup env x

    | Prim1(opr, e1) =>
        let
          val v1 = eval e1 env
        in
          case (opr, v1) of

              ("-", IntV i) => IntV (~i)

            | ("print", _) => 
                let
                  val s = val2string v1
                in
                  print(s^"\n"); ListV []
                end
            
            | _ => raise Impossible
        end

    | Prim2(opr, e1, e2) =>
        let 
          val v1 = eval e1 env
          val v2 = eval e2 env
        in 
          case (opr, v1, v2) of
              ("*", IntV i1, IntV i2) => IntV (i1 * i2)
            | ("/", IntV i1, IntV i2) => IntV (i1 div i2)
            | ("+", IntV i1, IntV i2) => IntV (i1 + i2)
            | ("-", IntV i1, IntV i2) => IntV (i1 - i2)
            | (";", _, _) => v2
            | _ => raise Impossible
        end

    | Let(x, e1, e2) =>
        let
          val v = eval e1 env
          val env' = (x,v)::env
        in
          eval e2 env'
        end

    | Letrec(l1,l2,l3,l4,l5,l6) => (* TODO *)

    | If(c,e1,e2) =>
      let
        val res = eval c env
      in
        case res of
            BoolV true => eval e1 env
          | BoolV false => eval e2 env
          | _ => raise Impossible
      end

    | Match(e1,e2,e3) => (* TODO *)

    | Call(e1,e2) => (* TODO *)

    | List el => (* TODO *)

    | Item(i,e1) => (*TODO *)

    | Anon(t,s,e1) => (* TODO *)
    
    | _ => raise Impossible