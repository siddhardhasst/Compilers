signature SEMANT =
sig
  val transProg : Absyn.exp -> unit
end 

structure Semant :> SEMANT = struct
  structure A = Absyn
  structure E = Env
  val err = ErrorMsg.error
  exception ErrMsg
  
  
  fun compare_ty (ty1, ty2, pos)=
    (case ty1 = ty2 of
      true => true
    | false => (err pos "type mismatch"; false))


  fun checkInt ({exp, ty}, pos) =
    ((case ty of
        Types.INT => ()
      | _ => err pos "integer required");
      exp)
      
      
  fun typelookup tenv n pos= 
  let 
    val result=Symbol.look (tenv, n)
  in  
    (case result of
      SOME ty2 => ty2
    | NONE => (err pos ("type is not defined: " ^ Symbol.name n) ; Types.UNIT))
  end
  

  fun transExp(venv, tenv)  =    

    let fun trexp (A.NilExp) = {exp=A.nilExp(), ty=Types.NIL}
      | trexp (A.VarExp var) = trvar var
      | trexp (A.IntExp i) = {exp=A.nilExp(), ty=Types.INT}
      | trexp (A.OpExp {left, oper, right, pos}) = 
        if oper = A.PlusOp orelse oper = A.MinusOp orelse 
           oper = A.TimesOp orelse oper = A.DivideOp then
          (checkInt(trexp left, pos);
           checkInt(trexp right, pos);
           {exp=A.nilExp(), ty=Types.INT})
       else
        (err pos "error";{exp=A.nilExp(), ty=Types.INT})


    | trexp   (A.IfExp {test, then', else', pos}) =
         let
           val test' = trexp (test)
           val then'' = trexp (then')
           val else'' = trexp (else')            
         in
           checkInt(test', pos);
           checkUnit(then'', pos);
           checkUnit(else'', pos);
           {exp=A.nilExp(), ty=Types.UNIT}
         end

      | trexp (A.WhileExp {test, body, pos}) =
        let
          val body' = transExp (venv,tenv) body
          val test' = transExp (venv,tenv) body
        
          val test'' = checkInt (test', pos);
          val body'' = checkUnit (body', pos);
        in
          {exp=A.nilExp(), ty=Types.UNIT}
        end 
        

      | trexp (A.SeqExp exps) =
        {exp=A.nilExp(), ty=Types.UNIT}

      | trexp (A.AssignExp {var, exp, pos}) =
        let
          val  {exp=left,  ty=expect} = trvar (var)
          val  {exp=right, ty=actual} = trexp (exp)
        in
          if
          expect <> actual
          then
          (err pos "assignment mismatch";{exp=A.nilExp(), ty=Types.UNIT})
          else
          {exp=A.nilExp(), ty=Types.UNIT}
        end

      | trexp (A.LetExp {decs, body, pos}) =
        let val {venv=venv', tenv=tenv'} = transDecs(venv, tenv, decs)
          in transExp(venv', tenv') body
        end

      | trexp (A.ArrayExp {typ, size, init, pos}) =
        {exp=A.nilExp(), ty=Types.UNIT}

     and trvar (A.SimpleVar(id,pos)) = 
          (case Symbol.look(venv, id) of
            SOME (E.VarEntry{ty}) => {exp=A.nilExp(), ty= ty}
          | _ => (err pos ("undefined variable: " ^ Symbol.name id); {exp=A.nilExp(), ty=Types.INT}))

    in
      trexp
    end
    
    and transDec (venv, tenv, A.VarDec{name, typ=NONE, init,... }) = 
          let 
            val {exp,ty} = transExp (venv, tenv) init
          in 
            {tenv = tenv, venv=Symbol.enter(venv, name, E.VarEntry{ty=ty})}
          end

    | transDec (venv, tenv, A.TypeDec[{name,ty}]) =  {venv = venv,
                                tenv = S.enter(tenv, name, trensTy(tenv, ty))}

    | transDec(venv, tenv, _) = {venv=venv, tenv=tenv}

    
  fun transProg(absyn) = (transExp (E.base_tenv, E.base_venv) absyn; ())
end
