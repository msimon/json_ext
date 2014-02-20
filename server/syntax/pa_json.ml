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
  let runtimename = "Json_ext"
  let default_module = None
  let alpha = None
  let allow_private = false
  let predefs = [
    ["int"], ["Json_ext";"int"];
    ["int32"], ["Json_ext";"int32"];
    ["Int32";"t"], ["Json_ext";"int32"];
    ["int64"], ["Json_ext";"int64"];
    ["Int64";"t"], ["Json_ext";"int64"];
    ["bool"], ["Json_ext";"bool"];
    ["float"], ["Json_ext";"float"];
    ["string"], ["Json_ext";"string"];
    ["list"], ["Json_ext";"list"];
    ["array"],["Json_ext";"array"];
    ["option"], ["Json_ext";"option"];
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
      None, [ <:ident< to_json >>;
	            <:ident< from_json >>;
            ]

    method record ?eq ctxt tname params constraints (fields : Pa_deriving_common.Type.field list) =
      let to_json =
        let l =
          List.map (
            fun (name,ty,_) ->
              <:expr<
                ($`str:name$,$self#call_poly_expr ctxt ty "to_json"$ t.$lid:name$)
              >>
          ) fields
        in

        <:str_item<
          value to_json (t : $lid:tname$) =
            let json_list = $Helpers.expr_list l$ in
            Json_ext.to_json json_list
        >>
      in

      let from_json =
        let l =
          List.map (
            fun (name,ty,_) ->
              let expr =
                <:expr<
                  try
                    let v = Json_ext.assoc $`str:name$ json_list in
                    $self#call_poly_expr ctxt ty "from_json"$ v
                  with [ Json_ext.Error_json s ->
                    raise (Json_ext.Incorect_type (Printf.sprintf "%s is %s" $`str:name$ s))
                  ]
                >>
              in
              name,expr
          ) fields
        in

        <:str_item<
          value from_json json : $lid:tname$ =
            let json_list = Json_ext.fetch_assoc_value json in
            $Helpers.record_expr l$
        >>
      in

      [
        to_json;
        from_json
      ]


    method tuple ctxt tys =
      let ntys = List.length tys in
      let ids,tpatt,texpr = Helpers.tuple ntys in

      let to_json =
        let l = List.map (
            fun (ty,id) ->
              <:expr<
                (* $lid:id$ variable are define by the $tpatt$ below, They are normal type *)
                $self#call_expr ctxt ty "to_json"$ $lid:id$
              >>
          ) (List.zip tys ids)
        in

        <:str_item<
          value to_json t =
            (* $tpatt$ will be something like (id1,id2,id3...), so here id are value name of a subset of t*)
            let $tpatt$ = t in
            `List ($Helpers.expr_list l$)
        >>
      in

      let from_json =
        let l =
          List.map (
            fun (ty,id) ->
              (* $lid:id$ variable are define by the match case below, they are Yojson type *)
              <:expr< $self#call_expr ctxt ty "from_json"$ $lid:id$ >>
          ) (List.zip tys ids)
        in

        <:str_item<
          value from_json json =
            match (Json_ext.fetch_list json) with
             [ $Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$ -> $Helpers.tuple_expr l$
                  | _ -> raise (Json_ext.Error_json "tuple")
             ]
        >>
      in

      [
        to_json;
        from_json
      ]

    method sum_to_json: 'c. Generator.context -> ('a -> 'b -> 'c -> 'd) -> 'c list -> Camlp4.PreCast.Ast.str_item = fun ctxt mc tags ->
      let to_json =
        let no_expr ~is_sum name =
          if is_sum then
            <:match_case<
              $uid:name$ ->
                Json_ext.to_json [ ($str:name$,`Null) ]
            >>
          else
            <:match_case<
              `$uid:name$ ->
                Json_ext.to_json [ ($str:name$,`Null) ]
            >>
        in

        let with_expr ~is_sum name tys =
          let ntys = List.length tys in
          let ids,tpatt,texpr = Helpers.tuple ntys in

          let l = List.map (
              fun (ty,id) ->
                <:expr<
                  (* $lid:id$ variable are define by the $tpatt$ below, They are normal type *)
                  $self#call_expr ctxt ty "to_json"$ $lid:id$
                >>
            ) (List.zip tys ids)
          in

          if is_sum then
            <:match_case<
              $uid:name$ $tpatt$ ->
              Json_ext.to_json [ ($str:name$, `List ($Helpers.expr_list l$))]
            >>
          else
            <:match_case<
              `$uid:name$ $tpatt$ ->
                Json_ext.to_json [ ($str:name$, `List ($Helpers.expr_list l$))]
            >>
        in

        let mcs = List.map (mc no_expr with_expr) tags in

        <:str_item<
          value to_json t =
            match t with
              [
                $list:mcs$
              ]
        >>
      in

      to_json

    method sum_from_json: 'c. Generator.context -> ('e -> 'f -> 'c -> 'g) -> 'c list -> Camlp4.PreCast.Ast.str_item = fun ctxt mc tags ->
      let from_json =
        let no_expr acc ~is_sum name =
          if is_sum then
            <:match_case<
              `Assoc [ ($str:name$,`Null) ] ->
                $uid:name$
            >>::
              <:match_case<
                  `Assoc [ ($str:(String.lowercase name)$,`Null) ] ->
                  $uid:name$
              >>::acc
          else
            <:match_case<
              `Assoc [ ($str:name$,`Null) ] ->
                `$uid:name$
            >>::
              <:match_case<
                `Assoc [ ($str:(String.lowercase name)$,`Null) ] ->
                `$uid:name$
              >>::acc
        in
        let with_expr acc ~is_sum name tys =
          let ntys = List.length tys in
          let ids,tpatt,texpr = Helpers.tuple ntys in

          let l =
            List.map (
              fun (ty,id) ->
                (* $lid:id$ variable are define by the match case below, they are Yojson type *)
                <:expr< $self#call_expr ctxt ty "from_json"$ $lid:id$ >>
            ) (List.zip tys ids)
          in

          if is_sum then
            <:match_case<
              `Assoc [ ($`str:name$, `List ($Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$)) ] ->
                $uid:name$ $Helpers.tuple_expr l$
            >>::acc
          else
            <:match_case<
              `Assoc [ ($`str:name$, `List ($Helpers.patt_list (List.map (fun x -> <:patt<$lid:x$>>) ids)$)) ] ->
                `$uid:name$ $Helpers.tuple_expr l$
            >>::acc
        in

        let mcs =
          List.fold_left (
            fun acc t ->
              mc (no_expr acc) (with_expr acc) t
          ) [ <:match_case< _ -> raise (Json_ext.Error_json "variant") >> ] tags
        in

        <:str_item<
          value from_json json =
          match json with
            [
              $list:mcs$
            ]
        >>
     in

     from_json

    method sum ?eq ctxt tname params constraints summands =
      let mc no_expr with_expr =
        function
          | (name, []) -> no_expr ~is_sum:true name
          | (name,tys) -> with_expr ~is_sum:true name tys
      in

      let to_json = self#sum_to_json ctxt mc summands in
      let from_json = self#sum_from_json ctxt mc summands in

      [
        to_json ;
        from_json ;
      ]


    method variant ctxt tname params constraints (_, tags) =
      let mc no_expr with_expr =
        function
          | Type.Tag (name, []) -> no_expr ~is_sum:false name
          | Type.Tag (name,tys) -> with_expr ~is_sum:false name tys
          | Type.Extends _ -> assert false
      in

      let to_json = self#sum_to_json ctxt mc tags in
      let from_json = self#sum_from_json ctxt mc tags in

      [
        to_json ;
        from_json ;
      ]


  end :> Generator.generator)

  let generate decls =
    let i = Generator.generate generator decls in

    let modules =
      List.map (
        fun t ->
          let (name,params,_,_,_) = t in

          let functor_params =
            List.fold_left (
              fun m (name, _) ->
                <:module_expr< $m$ ($uid:"M" ^ name$) >>
            ) (<:module_expr< $uid:("Json_ext_" ^ name) $>>) params
          in

          let body =
            <:module_expr<
              struct
                value of_file file_name =
                  let module $uid:"Json_ext_" ^ name$ = $functor_params$ in
                  let json = Json_ext.from_file file_name in
                  $uid:("Json_ext_" ^ name)$.from_json json;

                value to_file obj file_name =
                  let module $uid:"Json_ext_" ^ name$ = $functor_params$ in
                  let json = $uid:("Json_ext_" ^ name)$.to_json obj in
                  Json_ext.to_file json file_name;


                value list_of_file file_name =
                  let module $uid:"Json_ext_" ^ name$ = $functor_params$ in
                  let json = Json_ext.from_file file_name in
                  List.map (
                    fun json ->
                      $uid:("Json_ext_" ^ name)$.from_json json
                  ) (Json_ext.fetch_list json);

                value list_to_file obj_list file_name =
                  let module $uid:"Json_ext_" ^ name$ = $functor_params$ in
                  let json_list =
                    List.map (
                      fun obj ->
                        $uid:("Json_ext_" ^ name)$.to_json obj
                     ) obj_list
                  in
                  Json_ext.to_file (Json_ext.to_list json_list) file_name;
              end
            >>
          in

          let body =
            List.fold_right (
              fun (name,_) body ->
                <:module_expr<
                  functor ($uid:("M" ^ name)$ : $uid:"Json_ext"$.$uid:"Json_ext"$)
                  -> $body$
                >>
            ) params body
          in

          <:str_item<
            module $uid:("Json_utils_" ^ name)$ = $body$
          >>
      ) decls
    in

    <:str_item<
      $i$;
      $list:modules$
    >>

  let generate_sigs = Generator.generate_sigs generator

end

module Json_ext = Base.Register(Description)(Builder)
