signature ENV =
sig
  datatype enventry = VarEntry of { ty: Types.ty}
  val base_tenv : Types.ty Symbol.table
   val base_venv : enventry Symbol.table
end

structure Env :> ENV = struct
  datatype enventry = VarEntry of {ty: Types.ty}
  val base_tenv = Symbol.enter(Symbol.empty, Symbol.symbol("int"), Types.INT)


end