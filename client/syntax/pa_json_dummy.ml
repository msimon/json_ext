open Camlp4
open Pa_deriving_common
open Utils

module Id : Sig.Id =
struct
  let name = "pa_json"
  let version = "0.1"
end

module Description : Defs.ClassDescription = struct
  let classname = "Json_ext"
  let runtimename = "Json_ext_dummy"
  let default_module = None
  let alpha = None
  let allow_private = false
  let predefs = [
    ["int"], ["Json_ext_dummy";"int"];
    ["int32"], ["Json_ext_dummy";"int32"];
    ["Int32";"t"], ["Json_ext_dummy";"int32"];
    ["int64"], ["Json_ext_dummy";"int64"];
    ["Int64";"t"], ["Json_ext_dummy";"int64"];
    ["bool"], ["Json_ext_dummy";"bool"];
    ["float"], ["Json_ext_dummy";"float"];
    ["string"], ["Json_ext_dummy";"string"];
    ["list"], ["Json_ext_dummy";"list"];
    ["array"],["Json_ext_dummy";"array"];
    ["option"], ["Json_ext_dummy";"option"];
  ]


  let depends = []
end

module Builder(Loc : Defs.Loc) = struct

  module Helpers = Base.AstHelpers(Loc)
  module Generator = Base.Generator(Loc)(Description)

  open Loc
  open Camlp4.PreCast
  open Description

  let generator = (object(self)

  inherit Generator.generator

    method proxy () =
      None, [
      ]

    method record ?eq ctxt tname params constraints (fields : Pa_deriving_common.Type.field list) =
      [
        <:str_item< value f _ =  assert False >>
      ]

    method tuple ctxt tys =
      [
        <:str_item< value f _ =  assert False >>
      ]

    method sum ?eq ctxt tname params constraints summands =
      [
        <:str_item< value f _ =  assert False >>
      ]


    method variant ctxt tname params constraints (_, tags) =
      [
        <:str_item< value f _ =  assert False >>
      ]


end :> Generator.generator)

let generate = Generator.generate generator
let generate_sigs = Generator.generate_sigs generator

end

module Json_ext = Base.Register(Description)(Builder)
