(** {1 Parameters} *)

module ParamSerDe' = struct

  open StripeTypes
  open StripeEncdrs
  
  let _loosen_spec_enable_deepobject_array = true
  let _jfield_to_s kind ~n = match kind with
    | `ObjectN okinds -> begin
      match List.assoc_opt n okinds with
      | Some [okind] -> EncBase'.json_to_string okind
      | _ -> EncBase'.json_to_string (match List.assoc_opt "" okinds with
        | Some [okind] -> okind
        | _ -> `Null)
      end
    | _ -> EncBase'.json_to_string `Null
  
  let _jitem_to_s kind idx v = match kind with
    | `Array [`List ikind] -> EncBase'.json_to_string ikind v
    | `Array akind ->
      let vs = Ezjsonm.get_list Fun.id v in
      let kinds = EncBase'.singletons_of_array ~len:(min idx (List.length vs)) akind |> Array.of_list in
      EncBase'.json_to_string (kinds.(idx)) v
    | _ -> EncBase'.json_to_string `Null v
  
  let _string_of ~kind ~ctr =
    let j_to_s = EncBase'.json_to_string kind in
    let jfield_to_s = _jfield_to_s kind in
    let jitem_to_s = _jitem_to_s kind in
    fun ~p ~op ->
      let _m =
        "The parameter '" ^ p ^ "' in the REST path " ^ op
        ^ " could not be serialized as a string."
      in
      fun ~(loc:[`Path]) ~(style:[`Label | `Matrix | `Simple]) ~explode x ->
        let j = ctr x in
        match (loc, style, explode, j) with
        | `Path, `Matrix, false, `A a -> ";" ^ p ^ "=" ^ String.concat "," (List.mapi jitem_to_s a)
        | `Path, `Matrix, true, `A a -> String.concat "" (List.mapi (fun idx ai -> ";" ^ p ^ "=" ^ (jitem_to_s idx) ai) a)
        | `Path, `Matrix, false, `O o -> ";" ^ p ^ "=" ^ String.concat "," (List.map (fun (n, v) -> n ^ "," ^ jfield_to_s ~n v) o)
        | `Path, `Matrix, true, `O o -> String.concat "" (List.map (fun (n, v) -> ";" ^ n ^ "=" ^ jfield_to_s ~n v) o)
        | `Path, `Matrix, _, _ -> ";" ^ p ^ "=" ^ j_to_s j
        | `Path, `Label, _, `A a -> String.concat "" (List.mapi (fun idx ai -> "." ^ jitem_to_s idx ai) a)
        | `Path, `Label, false, `O o -> String.concat "." (List.map (fun (n, v) -> n ^ "." ^ jfield_to_s ~n v) o)
        | `Path, `Label, true, `O o -> String.concat "" (List.map (fun (n, v) -> "." ^ n ^ "=" ^ jfield_to_s ~n v) o)
        | `Path, `Label, _, _ -> j_to_s j
        | `Path, `Simple, _, `A a -> String.concat "," (List.mapi jitem_to_s a)
        | `Path, `Simple, false, `O o -> String.concat "," (List.map (fun (n, v) -> n ^ "," ^ jfield_to_s ~n v) o)
        | `Path, `Simple, true, `O o -> String.concat "," (List.map (fun (n, v) -> n ^ "=" ^ jfield_to_s ~n v) o)
        | `Path, `Simple, _, _ -> j_to_s j
        (* | _ -> raise (Invalid_argument m) *)
  
  let _string_to ~kind ~dtr =
    let s_to_j = EncBase'.string_to_json kind in
    fun ~(loc:[`Path]) ~(style:[`Label | `Matrix | `Simple]) ~explode s ->
      let jopt =
        match (loc, style, explode) with
        | `Path, `Matrix, false -> s_to_j ~sep:';' ~inner_sep:',' ~leading:true s
        | `Path, `Matrix, true -> s_to_j ~sep:';' ~inner_sep:'=' ~leading:true s
        | `Path, `Label, false -> s_to_j ~sep:'.' ~inner_sep:'.' ~leading:true s
        | `Path, `Simple, false -> s_to_j ~sep:',' ~inner_sep:',' ~leading:false s
        | `Path, `Label, true -> s_to_j ~sep:'.' ~inner_sep:'=' ~leading:true s
        | `Path, `Simple, true -> s_to_j ~sep:',' ~inner_sep:'=' ~leading:false s
      in
      match jopt with
      | None, _ -> None
      | Some j, _ -> (
          try Some (dtr j) with Json_encoding.Cannot_destruct _ -> None)
  
  let _namevalues_of ~kind ~ctr =
    let j_to_s = EncBase'.json_to_string kind in
    let jfield_to_s = _jfield_to_s kind in
    let jitem_to_s = _jitem_to_s kind in
    fun ~p ~op ->
      let m = ("The parameter '" ^ p ^ "' used by the REST operation " ^ op ^ " could not be serialized.") in
      fun
        ~(loc:[`Cookie | `Header | `Query ])
        ~(style:[`DeepObject
          | `Form
          | `PipeDelimited
          | `Simple
          | `SpaceDelimited ])
        ~explode x ->
        let j = ctr x in
        match loc,style,explode,j with
        | (`Query|`Cookie),`Form,false,`A a -> [(p, String.concat "," (List.mapi jitem_to_s a))]
        | (`Query|`Cookie),`Form,true,`A a -> List.mapi (fun idx ai -> (p, jitem_to_s idx ai)) a
        | (`Query|`Cookie),`Form,false,`O o -> [(p, String.concat "," (List.map (fun (n,v) -> n ^ "," ^ jfield_to_s ~n v) o))]
        | (`Query|`Cookie),`Form,true,`O o -> List.map (fun (n,v) -> (n, jfield_to_s ~n v)) o
        | (`Query|`Cookie),`Form,_,_ -> [(p, j_to_s j)]
        | `Header,`Simple,_,`A a -> [(p, String.concat "," (List.mapi jitem_to_s a))]
        | `Header,`Simple,false,`O o -> [(p, String.concat "," (List.map (fun (n,v) -> n ^ "," ^ jfield_to_s ~n v) o))]
        | `Header,`Simple,true,`O o -> [(p, String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ jfield_to_s ~n v) o))]
        | `Header,`Simple,_,_ -> [(p, j_to_s j)]
        | `Query,`SpaceDelimited,false,`A a -> [(p, String.concat " " (List.mapi jitem_to_s a))]
        | `Query,`SpaceDelimited,false,`O o -> [(p, String.concat " " (List.map (fun (n,v) -> n ^ " " ^ jfield_to_s ~n v) o))]
        | `Query,`PipeDelimited,false,`A a -> [(p, String.concat "|" (List.mapi jitem_to_s a))]
        | `Query,`PipeDelimited,false,`O o -> [(p, String.concat "|" (List.map (fun (n,v) -> n ^ "|" ^ jfield_to_s ~n v) o))]
        | `Query,`DeepObject,true,`O o -> List.map (fun (n,v) -> (p ^ "[" ^ n ^ "]", jfield_to_s ~n v)) o
        | `Query,`DeepObject,true,`A a ->
          if _loosen_spec_enable_deepobject_array then List.mapi (fun idx ai -> (p ^ "[]", jitem_to_s idx ai)) a
          else raise (Invalid_argument m)
        | _ -> raise (Invalid_argument m)
  
  
  
  let string_of_p_StringList ~p ~op ~loc ~style ~explode (_x : string list) =
    _string_of ~kind:(
      `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x)))
      ~ctr:(Json_encoding.construct (Json_encoding.list Json_encoding.string))
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_StringList ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct
                 (Json_encoding.list Json_encoding.string)) in _string_to
      ~kind:(`Array [(`List (`String))]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_StringList ~p ~op ~loc ~style ~explode
    (_x : string list) =
    _namevalues_of ~kind:(
      `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x)))
      ~ctr:(Json_encoding.construct (Json_encoding.list Json_encoding.string))
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_fd1c57d9a7 ~p ~op ~loc ~style ~explode
    (_x : t_fd1c57d9a7) =
    _string_of ~kind:(
      match _x with
      | T_d1f4aaecc2 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_fd1c57d9a7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fd1c57d9a7 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_d1f4aaecc2) in
        Option.map (fun _y : t_fd1c57d9a7 -> T_d1f4aaecc2 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_fd1c57d9a7 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_fd1c57d9a7 ~p ~op ~loc ~style ~explode
    (_x : t_fd1c57d9a7) =
    _namevalues_of ~kind:(
      match _x with
      | T_d1f4aaecc2 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_fd1c57d9a7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_p_String_ ~p ~op ~loc ~style ~explode (_x : string) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Json_encoding.string)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_String_ ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Json_encoding.string) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_String_ ~p ~op ~loc ~style ~explode (_x : string) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Json_encoding.string)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_p_Int ~p ~op ~loc ~style ~explode (_x : int) =
    _string_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct Json_encoding.int)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_Int ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Json_encoding.int) in _string_to
      ~kind:(`Integer) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_Int ~p ~op ~loc ~style ~explode (_x : int) =
    _namevalues_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct Json_encoding.int)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_830aa13cdc ~p ~op ~loc ~style ~explode
    (_x : t_830aa13cdc) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_830aa13cdc)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_830aa13cdc ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_830aa13cdc) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_830aa13cdc ~p ~op ~loc ~style ~explode
    (_x : t_830aa13cdc) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_830aa13cdc)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_b886c9886d ~p ~op ~loc ~style ~explode
    (_x : t_b886c9886d) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("director", match _x.director with | None -> []
          | Some _x -> [`Boolean]);
         ("executive", match _x.executive with | None -> []
          | Some _x -> [`Boolean]);
         ("legal_guardian", match _x.legal_guardian with | None -> []
          | Some _x -> [`Boolean]);
         ("owner", match _x.owner with | None -> [] | Some _x -> [`Boolean]);
         ("representative", match _x.representative with | None -> []
          | Some _x -> [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_b886c9886d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b886c9886d ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b886c9886d) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("director", [`Boolean]);
                ("executive", [`Boolean]); ("legal_guardian", [`Boolean]);
                ("owner", [`Boolean]); ("representative", [`Boolean])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b886c9886d ~p ~op ~loc ~style ~explode
    (_x : t_b886c9886d) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("director", match _x.director with | None -> []
          | Some _x -> [`Boolean]);
         ("executive", match _x.executive with | None -> []
          | Some _x -> [`Boolean]);
         ("legal_guardian", match _x.legal_guardian with | None -> []
          | Some _x -> [`Boolean]);
         ("owner", match _x.owner with | None -> [] | Some _x -> [`Boolean]);
         ("representative", match _x.representative with | None -> []
          | Some _x -> [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_b886c9886d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_df97d4c864 ~p ~op ~loc ~style ~explode
    (_x : t_df97d4c864) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("director", match _x.director with | None -> []
          | Some _x -> [`Boolean]);
         ("executive", match _x.executive with | None -> []
          | Some _x -> [`Boolean]);
         ("legal_guardian", match _x.legal_guardian with | None -> []
          | Some _x -> [`Boolean]);
         ("owner", match _x.owner with | None -> [] | Some _x -> [`Boolean]);
         ("representative", match _x.representative with | None -> []
          | Some _x -> [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_df97d4c864)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_df97d4c864 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_df97d4c864) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("director", [`Boolean]);
                ("executive", [`Boolean]); ("legal_guardian", [`Boolean]);
                ("owner", [`Boolean]); ("representative", [`Boolean])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_df97d4c864 ~p ~op ~loc ~style ~explode
    (_x : t_df97d4c864) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("director", match _x.director with | None -> []
          | Some _x -> [`Boolean]);
         ("executive", match _x.executive with | None -> []
          | Some _x -> [`Boolean]);
         ("legal_guardian", match _x.legal_guardian with | None -> []
          | Some _x -> [`Boolean]);
         ("owner", match _x.owner with | None -> [] | Some _x -> [`Boolean]);
         ("representative", match _x.representative with | None -> []
          | Some _x -> [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_df97d4c864)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e12c761965 ~p ~op ~loc ~style ~explode
    (_x : t_e12c761965) =
    _string_of ~kind:(
      match _x with
      | T_ee07665ba6 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_e12c761965)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e12c761965 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_ee07665ba6) in
        Option.map (fun _y : t_e12c761965 -> T_ee07665ba6 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e12c761965 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_e12c761965 ~p ~op ~loc ~style ~explode
    (_x : t_e12c761965) =
    _namevalues_of ~kind:(
      match _x with
      | T_ee07665ba6 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_e12c761965)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e9c8494c48 ~p ~op ~loc ~style ~explode
    (_x : t_e9c8494c48) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]); ("type", let _x = _x.type_ in [`String]);
         ("user", match _x.user with | None -> [] | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_e9c8494c48)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e9c8494c48 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e9c8494c48) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("type", [`String]); ("user", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e9c8494c48 ~p ~op ~loc ~style ~explode
    (_x : t_e9c8494c48) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]); ("type", let _x = _x.type_ in [`String]);
         ("user", match _x.user with | None -> [] | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_e9c8494c48)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_890cf6dfd5 ~p ~op ~loc ~style ~explode
    (_x : t_890cf6dfd5) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]); ("type", let _x = _x.type_ in [`String]);
         ("user", match _x.user with | None -> [] | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_890cf6dfd5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_890cf6dfd5 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_890cf6dfd5) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("type", [`String]); ("user", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_890cf6dfd5 ~p ~op ~loc ~style ~explode
    (_x : t_890cf6dfd5) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]); ("type", let _x = _x.type_ in [`String]);
         ("user", match _x.user with | None -> [] | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_890cf6dfd5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_d867b2e5ee ~p ~op ~loc ~style ~explode
    (_x : t_d867b2e5ee) =
    _string_of ~kind:(
      match _x with
      | T_9976a2991b _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d867b2e5ee)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d867b2e5ee ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_9976a2991b) in
        Option.map (fun _y : t_d867b2e5ee -> T_9976a2991b _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d867b2e5ee -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_d867b2e5ee ~p ~op ~loc ~style ~explode
    (_x : t_d867b2e5ee) =
    _namevalues_of ~kind:(
      match _x with
      | T_9976a2991b _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d867b2e5ee)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_04deb19963 ~p ~op ~loc ~style ~explode
    (_x : t_04deb19963) =
    _string_of ~kind:(
      match _x with
      | T_24f63781c8 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_04deb19963)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_04deb19963 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_24f63781c8) in
        Option.map (fun _y : t_04deb19963 -> T_24f63781c8 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_04deb19963 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_04deb19963 ~p ~op ~loc ~style ~explode
    (_x : t_04deb19963) =
    _namevalues_of ~kind:(
      match _x with
      | T_24f63781c8 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_04deb19963)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_c9c8b6e8e4 ~p ~op ~loc ~style ~explode
    (_x : t_c9c8b6e8e4) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_c9c8b6e8e4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c9c8b6e8e4 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_c9c8b6e8e4) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_c9c8b6e8e4 ~p ~op ~loc ~style ~explode
    (_x : t_c9c8b6e8e4) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_c9c8b6e8e4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_p_Ptime_t ~p ~op ~loc ~style ~explode (_x : Ptime.t) =
    _string_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct EncBase'.vendor_unix_time)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_Ptime_t ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
      _string_to ~kind:(`Integer) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_Ptime_t ~p ~op ~loc ~style ~explode (_x : Ptime.t) =
    _namevalues_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct EncBase'.vendor_unix_time)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_8c6e63aa54 ~p ~op ~loc ~style ~explode
    (_x : t_8c6e63aa54) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8c6e63aa54)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8c6e63aa54 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_8c6e63aa54) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_8c6e63aa54 ~p ~op ~loc ~style ~explode
    (_x : t_8c6e63aa54) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8c6e63aa54)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_p_Bool ~p ~op ~loc ~style ~explode (_x : bool) =
    _string_of ~kind:( `Boolean)
      ~ctr:(Json_encoding.construct Json_encoding.bool)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_Bool ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Json_encoding.bool) in _string_to
      ~kind:(`Boolean) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_Bool ~p ~op ~loc ~style ~explode (_x : bool) =
    _namevalues_of ~kind:( `Boolean)
      ~ctr:(Json_encoding.construct Json_encoding.bool)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_97be7ade17 ~p ~op ~loc ~style ~explode
    (_x : t_97be7ade17) =
    _string_of ~kind:(
      match _x with
      | T_9c9e30bd0a _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_97be7ade17)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_97be7ade17 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_9c9e30bd0a) in
        Option.map (fun _y : t_97be7ade17 -> T_9c9e30bd0a _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_97be7ade17 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_97be7ade17 ~p ~op ~loc ~style ~explode
    (_x : t_97be7ade17) =
    _namevalues_of ~kind:(
      match _x with
      | T_9c9e30bd0a _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_97be7ade17)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_aa139a393a ~p ~op ~loc ~style ~explode
    (_x : t_aa139a393a) =
    _string_of ~kind:(
      match _x with
      | T_b54379d2c7 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_aa139a393a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_aa139a393a ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_b54379d2c7) in
        Option.map (fun _y : t_aa139a393a -> T_b54379d2c7 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_aa139a393a -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_aa139a393a ~p ~op ~loc ~style ~explode
    (_x : t_aa139a393a) =
    _namevalues_of ~kind:(
      match _x with
      | T_b54379d2c7 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_aa139a393a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_26c973347b ~p ~op ~loc ~style ~explode
    (_x : t_26c973347b) =
    _string_of ~kind:(
      `ObjectN [("", [`Any]); ("email", let _x = _x.email in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_26c973347b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_26c973347b ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_26c973347b) in _string_to
      ~kind:(`ObjectN [("", [`Any]); ("email", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_26c973347b ~p ~op ~loc ~style ~explode
    (_x : t_26c973347b) =
    _namevalues_of ~kind:(
      `ObjectN [("", [`Any]); ("email", let _x = _x.email in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_26c973347b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_b373919511 ~p ~op ~loc ~style ~explode
    (_x : t_b373919511) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b373919511)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b373919511 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b373919511) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b373919511 ~p ~op ~loc ~style ~explode
    (_x : t_b373919511) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b373919511)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f9a2efc89d ~p ~op ~loc ~style ~explode
    (_x : t_f9a2efc89d) =
    _string_of ~kind:(
      match _x with
      | T_62f503fcab _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_f9a2efc89d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f9a2efc89d ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_62f503fcab) in
        Option.map (fun _y : t_f9a2efc89d -> T_62f503fcab _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_f9a2efc89d -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_f9a2efc89d ~p ~op ~loc ~style ~explode
    (_x : t_f9a2efc89d) =
    _namevalues_of ~kind:(
      match _x with
      | T_62f503fcab _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_f9a2efc89d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_d09edd25a8 ~p ~op ~loc ~style ~explode
    (_x : t_d09edd25a8) =
    _string_of ~kind:(
      match _x with
      | T_e86a6266e7 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d09edd25a8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d09edd25a8 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_e86a6266e7) in
        Option.map (fun _y : t_d09edd25a8 -> T_e86a6266e7 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d09edd25a8 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_d09edd25a8 ~p ~op ~loc ~style ~explode
    (_x : t_d09edd25a8) =
    _namevalues_of ~kind:(
      match _x with
      | T_e86a6266e7 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d09edd25a8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_537f0b2797 ~p ~op ~loc ~style ~explode
    (_x : t_537f0b2797) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("shipping_rate", match _x.shipping_rate with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_537f0b2797)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_537f0b2797 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_537f0b2797) in _string_to
      ~kind:(`ObjectN [("", [`Any]); ("shipping_rate", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_537f0b2797 ~p ~op ~loc ~style ~explode
    (_x : t_537f0b2797) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("shipping_rate", match _x.shipping_rate with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_537f0b2797)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_ea63e9747c ~p ~op ~loc ~style ~explode
    (_x : t_ea63e9747c) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_af89a87b4b) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("invoice_line_item", match _x.invoice_line_item with
                   | None -> [] | Some _x -> [`String]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_amounts", match _x.tax_amounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_bf627e1b51 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_0ef46b8218) ->
                                         `Singleton (`Null)) _x))
                                 | T_467cde7fc9 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_ab1fb2277d _x -> `String]);
                  ("type", let _x = _x.type_ in [`String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_ea63e9747c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ea63e9747c ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_ea63e9747c) in _string_to
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", [`Any]); ("amount", [`Integer]);
                       ("description", [`String]);
                       ("invoice_line_item", [`String]);
                       ("quantity", [`Integer]);
                       ("tax_amounts", [`Array [(`List (`Null))];
                                        `String]);
                       ("tax_rates", [`Array [(`List (`Null))];
                                      `String]);
                       ("type", [`String]); ("unit_amount", [`Integer]);
                       ("unit_amount_decimal", [`String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_ea63e9747c ~p ~op ~loc ~style ~explode
    (_x : t_ea63e9747c) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_af89a87b4b) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("invoice_line_item", match _x.invoice_line_item with
                   | None -> [] | Some _x -> [`String]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_amounts", match _x.tax_amounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_bf627e1b51 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_0ef46b8218) ->
                                         `Singleton (`Null)) _x))
                                 | T_467cde7fc9 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_ab1fb2277d _x -> `String]);
                  ("type", let _x = _x.type_ in [`String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_ea63e9747c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_1449eb52f2 ~p ~op ~loc ~style ~explode
    (_x : t_1449eb52f2) =
    _string_of ~kind:( `ObjectN [("", [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_1449eb52f2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1449eb52f2 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_1449eb52f2) in _string_to
      ~kind:(`ObjectN [("", [`String])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_1449eb52f2 ~p ~op ~loc ~style ~explode
    (_x : t_1449eb52f2) =
    _namevalues_of ~kind:( `ObjectN [("", [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_1449eb52f2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_b994932726 ~p ~op ~loc ~style ~explode
    (_x : t_b994932726) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b994932726)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b994932726 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b994932726) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b994932726 ~p ~op ~loc ~style ~explode
    (_x : t_b994932726) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b994932726)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_defa071e89 ~p ~op ~loc ~style ~explode
    (_x : t_defa071e89) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_defa071e89)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_defa071e89 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_defa071e89) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_defa071e89 ~p ~op ~loc ~style ~explode
    (_x : t_defa071e89) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_defa071e89)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_5116f50be5 ~p ~op ~loc ~style ~explode
    (_x : t_5116f50be5) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("shipping_rate", match _x.shipping_rate with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_5116f50be5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_5116f50be5 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_5116f50be5) in _string_to
      ~kind:(`ObjectN [("", [`Any]); ("shipping_rate", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_5116f50be5 ~p ~op ~loc ~style ~explode
    (_x : t_5116f50be5) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("shipping_rate", match _x.shipping_rate with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_5116f50be5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_302b4fc4e4 ~p ~op ~loc ~style ~explode
    (_x : t_302b4fc4e4) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_7b0c385004) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("invoice_line_item", match _x.invoice_line_item with
                   | None -> [] | Some _x -> [`String]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_amounts", match _x.tax_amounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_d7a16a0dc4 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_6c6845026a) ->
                                         `Singleton (`Null)) _x))
                                 | T_78866db489 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_6034f184ea _x -> `String]);
                  ("type", let _x = _x.type_ in [`String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_302b4fc4e4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_302b4fc4e4 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_302b4fc4e4) in _string_to
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", [`Any]); ("amount", [`Integer]);
                       ("description", [`String]);
                       ("invoice_line_item", [`String]);
                       ("quantity", [`Integer]);
                       ("tax_amounts", [`Array [(`List (`Null))];
                                        `String]);
                       ("tax_rates", [`Array [(`List (`Null))];
                                      `String]);
                       ("type", [`String]); ("unit_amount", [`Integer]);
                       ("unit_amount_decimal", [`String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_302b4fc4e4 ~p ~op ~loc ~style ~explode
    (_x : t_302b4fc4e4) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_7b0c385004) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("invoice_line_item", match _x.invoice_line_item with
                   | None -> [] | Some _x -> [`String]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_amounts", match _x.tax_amounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_d7a16a0dc4 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_6c6845026a) ->
                                         `Singleton (`Null)) _x))
                                 | T_78866db489 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_6034f184ea _x -> `String]);
                  ("type", let _x = _x.type_ in [`String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_302b4fc4e4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_0821868d84 ~p ~op ~loc ~style ~explode
    (_x : t_0821868d84) =
    _string_of ~kind:( `ObjectN [("", [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_0821868d84)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0821868d84 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_0821868d84) in _string_to
      ~kind:(`ObjectN [("", [`String])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_0821868d84 ~p ~op ~loc ~style ~explode
    (_x : t_0821868d84) =
    _namevalues_of ~kind:( `ObjectN [("", [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_0821868d84)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_3d8261ebd7 ~p ~op ~loc ~style ~explode
    (_x : t_3d8261ebd7) =
    _string_of ~kind:(
      match _x with
      | T_21225e884d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_3d8261ebd7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_3d8261ebd7 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_21225e884d) in
        Option.map (fun _y : t_3d8261ebd7 -> T_21225e884d _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_3d8261ebd7 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_3d8261ebd7 ~p ~op ~loc ~style ~explode
    (_x : t_3d8261ebd7) =
    _namevalues_of ~kind:(
      match _x with
      | T_21225e884d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_3d8261ebd7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_53e3e26f8d ~p ~op ~loc ~style ~explode
    (_x : t_53e3e26f8d) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_53e3e26f8d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_53e3e26f8d ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_53e3e26f8d) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_53e3e26f8d ~p ~op ~loc ~style ~explode
    (_x : t_53e3e26f8d) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_53e3e26f8d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_4ec070121c ~p ~op ~loc ~style ~explode
    (_x : t_4ec070121c) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4ec070121c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4ec070121c ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4ec070121c) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_4ec070121c ~p ~op ~loc ~style ~explode
    (_x : t_4ec070121c) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4ec070121c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_9387af353f ~p ~op ~loc ~style ~explode
    (_x : t_9387af353f) =
    _string_of ~kind:(
      match _x with
      | T_c3b68b80c4 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9387af353f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9387af353f ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_c3b68b80c4) in
        Option.map (fun _y : t_9387af353f -> T_c3b68b80c4 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9387af353f -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_9387af353f ~p ~op ~loc ~style ~explode
    (_x : t_9387af353f) =
    _namevalues_of ~kind:(
      match _x with
      | T_c3b68b80c4 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9387af353f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_8159efcced ~p ~op ~loc ~style ~explode
    (_x : t_8159efcced) =
    _string_of ~kind:(
      match _x with
      | T_b49c7c27ac _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_8159efcced)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8159efcced ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_b49c7c27ac) in
        Option.map (fun _y : t_8159efcced -> T_b49c7c27ac _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8159efcced -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_8159efcced ~p ~op ~loc ~style ~explode
    (_x : t_8159efcced) =
    _namevalues_of ~kind:(
      match _x with
      | T_b49c7c27ac _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_8159efcced)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f4a5e75dbf ~p ~op ~loc ~style ~explode
    (_x : t_f4a5e75dbf) =
    _string_of ~kind:(
      match _x with
      | T_6158529f95 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_f4a5e75dbf)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f4a5e75dbf ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_6158529f95) in
        Option.map (fun _y : t_f4a5e75dbf -> T_6158529f95 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_f4a5e75dbf -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_f4a5e75dbf ~p ~op ~loc ~style ~explode
    (_x : t_f4a5e75dbf) =
    _namevalues_of ~kind:(
      match _x with
      | T_6158529f95 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_f4a5e75dbf)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_60f6463eff ~p ~op ~loc ~style ~explode
    (_x : t_60f6463eff) =
    _string_of ~kind:(
      match _x with
      | T_94eb4d749d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_60f6463eff)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_60f6463eff ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_94eb4d749d) in
        Option.map (fun _y : t_60f6463eff -> T_94eb4d749d _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_60f6463eff -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_60f6463eff ~p ~op ~loc ~style ~explode
    (_x : t_60f6463eff) =
    _namevalues_of ~kind:(
      match _x with
      | T_94eb4d749d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_60f6463eff)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_9b3a32e051 ~p ~op ~loc ~style ~explode
    (_x : t_9b3a32e051) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9b3a32e051)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9b3a32e051 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_9b3a32e051) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_9b3a32e051 ~p ~op ~loc ~style ~explode
    (_x : t_9b3a32e051) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9b3a32e051)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f7bfa4b4d1 ~p ~op ~loc ~style ~explode
    (_x : t_f7bfa4b4d1) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("customer", match _x.customer with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_f7bfa4b4d1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f7bfa4b4d1 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f7bfa4b4d1) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("account", [`String]);
                ("customer", [`String])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f7bfa4b4d1 ~p ~op ~loc ~style ~explode
    (_x : t_f7bfa4b4d1) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("customer", match _x.customer with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_f7bfa4b4d1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_057e427287 ~p ~op ~loc ~style ~explode
    (_x : t_057e427287) =
    _string_of ~kind:(
      match _x with
      | T_c46138b060 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_057e427287)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_057e427287 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_c46138b060) in
        Option.map (fun _y : t_057e427287 -> T_c46138b060 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_057e427287 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_057e427287 ~p ~op ~loc ~style ~explode
    (_x : t_057e427287) =
    _namevalues_of ~kind:(
      match _x with
      | T_c46138b060 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_057e427287)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_c07bfc89dd ~p ~op ~loc ~style ~explode
    (_x : t_c07bfc89dd) =
    _string_of ~kind:(
      `ObjectN [("", [`Any]); ("after", let _x = _x.after in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_c07bfc89dd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c07bfc89dd ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_c07bfc89dd) in _string_to
      ~kind:(`ObjectN [("", [`Any]); ("after", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_c07bfc89dd ~p ~op ~loc ~style ~explode
    (_x : t_c07bfc89dd) =
    _namevalues_of ~kind:(
      `ObjectN [("", [`Any]); ("after", let _x = _x.after in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_c07bfc89dd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_7806695d68 ~p ~op ~loc ~style ~explode
    (_x : t_7806695d68) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
         ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
         ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
         ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_7806695d68)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_7806695d68 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_7806695d68) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                ("lt", [`Integer]); ("lte", [`Integer])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_7806695d68 ~p ~op ~loc ~style ~explode
    (_x : t_7806695d68) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
         ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
         ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
         ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_7806695d68)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_d8bdb49bf5 ~p ~op ~loc ~style ~explode
    (_x : t_d8bdb49bf5) =
    _string_of ~kind:(
      match _x with
      | T_29f8ba5efa _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d8bdb49bf5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d8bdb49bf5 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_29f8ba5efa) in
        Option.map (fun _y : t_d8bdb49bf5 -> T_29f8ba5efa _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d8bdb49bf5 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_d8bdb49bf5 ~p ~op ~loc ~style ~explode
    (_x : t_d8bdb49bf5) =
    _namevalues_of ~kind:(
      match _x with
      | T_29f8ba5efa _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d8bdb49bf5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f9a8042762 ~p ~op ~loc ~style ~explode
    (_x : t_f9a8042762) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f9a8042762)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f9a8042762 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f9a8042762) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f9a8042762 ~p ~op ~loc ~style ~explode
    (_x : t_f9a8042762) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f9a8042762)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_cd28c8e916 ~p ~op ~loc ~style ~explode
    (_x : t_cd28c8e916) =
    _string_of ~kind:(
      match _x with
      | T_4131fdfdbc _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_cd28c8e916)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_cd28c8e916 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_4131fdfdbc) in
        Option.map (fun _y : t_cd28c8e916 -> T_4131fdfdbc _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_cd28c8e916 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_cd28c8e916 ~p ~op ~loc ~style ~explode
    (_x : t_cd28c8e916) =
    _namevalues_of ~kind:(
      match _x with
      | T_4131fdfdbc _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_cd28c8e916)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_80086f9abe ~p ~op ~loc ~style ~explode
    (_x : t_80086f9abe) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_80086f9abe)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_80086f9abe ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_80086f9abe) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_80086f9abe ~p ~op ~loc ~style ~explode
    (_x : t_80086f9abe) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_80086f9abe)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f1cfc81a85 ~p ~op ~loc ~style ~explode
    (_x : t_f1cfc81a85) =
    _string_of ~kind:(
      match _x with
      | T_7b3ad81e3e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_f1cfc81a85)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f1cfc81a85 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_7b3ad81e3e) in
        Option.map (fun _y : t_f1cfc81a85 -> T_7b3ad81e3e _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_f1cfc81a85 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_f1cfc81a85 ~p ~op ~loc ~style ~explode
    (_x : t_f1cfc81a85) =
    _namevalues_of ~kind:(
      match _x with
      | T_7b3ad81e3e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_f1cfc81a85)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_279dfd61af ~p ~op ~loc ~style ~explode
    (_x : t_279dfd61af) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_279dfd61af)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_279dfd61af ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_279dfd61af) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_279dfd61af ~p ~op ~loc ~style ~explode
    (_x : t_279dfd61af) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_279dfd61af)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_aa3d2b4be3 ~p ~op ~loc ~style ~explode
    (_x : t_aa3d2b4be3) =
    _string_of ~kind:(
      match _x with
      | T_89ef3bf07e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_aa3d2b4be3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_aa3d2b4be3 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_89ef3bf07e) in
        Option.map (fun _y : t_aa3d2b4be3 -> T_89ef3bf07e _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_aa3d2b4be3 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_aa3d2b4be3 ~p ~op ~loc ~style ~explode
    (_x : t_aa3d2b4be3) =
    _namevalues_of ~kind:(
      match _x with
      | T_89ef3bf07e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_aa3d2b4be3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_66ac0a8f50 ~p ~op ~loc ~style ~explode
    (_x : t_66ac0a8f50) =
    _string_of ~kind:(
      match _x with
      | T_574b8da69a _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_66ac0a8f50)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_66ac0a8f50 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_574b8da69a) in
        Option.map (fun _y : t_66ac0a8f50 -> T_574b8da69a _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_66ac0a8f50 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_66ac0a8f50 ~p ~op ~loc ~style ~explode
    (_x : t_66ac0a8f50) =
    _namevalues_of ~kind:(
      match _x with
      | T_574b8da69a _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_66ac0a8f50)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_1247e44e67 ~p ~op ~loc ~style ~explode
    (_x : t_1247e44e67) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_1247e44e67)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1247e44e67 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_1247e44e67) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_1247e44e67 ~p ~op ~loc ~style ~explode
    (_x : t_1247e44e67) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_1247e44e67)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_8a5fd2de38 ~p ~op ~loc ~style ~explode
    (_x : t_8a5fd2de38) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]); ("enabled", let _x = _x.enabled in [`Boolean]);
         ("liability", match _x.liability with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("account", match _x.account with | None -> []
                            | Some _x -> [`String]);
                           ("type", let _x = _x.type_ in [`String])]])])
      ~ctr:(Json_encoding.construct Encoders'.t_8a5fd2de38)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8a5fd2de38 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_8a5fd2de38) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("enabled", [`Boolean]);
                ("liability",
                 [`ObjectN
                    [("", [`Any]); ("account", [`String]);
                     ("type", [`String])]])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_8a5fd2de38 ~p ~op ~loc ~style ~explode
    (_x : t_8a5fd2de38) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]); ("enabled", let _x = _x.enabled in [`Boolean]);
         ("liability", match _x.liability with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("account", match _x.account with | None -> []
                            | Some _x -> [`String]);
                           ("type", let _x = _x.type_ in [`String])]])])
      ~ctr:(Json_encoding.construct Encoders'.t_8a5fd2de38)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_ec4c522682 ~p ~op ~loc ~style ~explode
    (_x : t_ec4c522682) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ec4c522682)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ec4c522682 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_ec4c522682) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_ec4c522682 ~p ~op ~loc ~style ~explode
    (_x : t_ec4c522682) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ec4c522682)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e00ab899ed ~p ~op ~loc ~style ~explode
    (_x : t_e00ab899ed) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("end_behavior", match _x.end_behavior with | None -> []
          | Some _x -> [`String]);
         ("phases", match _x.phases with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_8527d4f3ba) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("add_invoice_items",
                                     match _x.add_invoice_items with
                                     | None -> []
                                     | Some _x -> [`Array
                                                     ((List.map (fun (_x : t_4bc3539de0) ->
                                                         `Singleton (`Null))
                                                         _x))]);
                                    ("application_fee_percent",
                                     match _x.application_fee_percent with
                                     | None -> [] | Some _x -> [`Number]);
                                    ("automatic_tax",
                                     match _x.automatic_tax with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("enabled",
                                                       let _x = _x.enabled in
                                                       [`Null]);
                                                      ("liability",
                                                       match _x.liability with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("billing_cycle_anchor",
                                     match _x.billing_cycle_anchor with
                                     | None -> [] | Some _x -> [`String]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_fcf3f3cc4f _x -> `Null
                                                   | T_e2c0378d25 _x -> `Null]);
                                    ("collection_method",
                                     match _x.collection_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("coupon", match _x.coupon with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_payment_method",
                                     match _x.default_payment_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_tax_rates",
                                     match _x.default_tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_30f5bb2100 _x -> `Null]);
                                    ("description", match _x.description with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | String_ _x -> `Null
                                                   | T_9b7bcd629e _x -> `Null]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_9f51d1fd63 _x -> `Null
                                                   | T_111a9f2365 _x -> `Null]);
                                    ("end_date", match _x.end_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_f939c879ff _x -> `Null]);
                                    ("invoice_settings",
                                     match _x.invoice_settings with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("account_tax_ids",
                                                       match _x.account_tax_ids with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("days_until_due",
                                                       match _x.days_until_due with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("issuer",
                                                       match _x.issuer with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("items", let _x = _x.items in
                                     [`Array
                                        ((List.map (fun (_x : t_1ccaef01fc) ->
                                            `Singleton (`Null)) _x))]);
                                    ("iterations", match _x.iterations with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [`ObjectN [("", [`Null])]]);
                                    ("on_behalf_of",
                                     match _x.on_behalf_of with | None -> []
                                     | Some _x -> [`String]);
                                    ("proration_behavior",
                                     match _x.proration_behavior with
                                     | None -> [] | Some _x -> [`String]);
                                    ("start_date", match _x.start_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_78925a3533 _x -> `Null]);
                                    ("transfer_data",
                                     match _x.transfer_data with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("amount_percent",
                                                       match _x.amount_percent with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("destination",
                                                       let _x = _x.destination in
                                                       [`Null])]]);
                                    ("trial", match _x.trial with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("trial_end", match _x.trial_end with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_8bbff7aec4 _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_e00ab899ed)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e00ab899ed ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e00ab899ed) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("end_behavior", [`String]);
                ("phases",
                 [`Array
                    [(`List
                        (`ObjectN
                           [("", [`Any]);
                            ("add_invoice_items", [`Array [(`List (`Null))]]);
                            ("application_fee_percent", [`Number]);
                            ("automatic_tax",
                             [`ObjectN
                                [("", [`Null]); ("enabled", [`Null]);
                                 ("liability", [`Null])]]);
                            ("billing_cycle_anchor", [`String]);
                            ("billing_thresholds", [`Null;
                                                    `Null]);
                            ("collection_method", [`String]);
                            ("coupon", [`String]);
                            ("default_payment_method", [`String]);
                            ("default_tax_rates", [`Null;
                                                   `Null]);
                            ("description", [`Null;
                                             `Null]);
                            ("discounts", [`Null;
                                           `Null]);
                            ("end_date", [`Null;
                                          `Null]);
                            ("invoice_settings",
                             [`ObjectN
                                [("", [`Null]); ("account_tax_ids", [`Null]);
                                 ("days_until_due", [`Null]);
                                 ("issuer", [`Null])]]);
                            ("items", [`Array [(`List (`Null))]]);
                            ("iterations", [`Integer]);
                            ("metadata", [`ObjectN [("", [`Null])]]);
                            ("on_behalf_of", [`String]);
                            ("proration_behavior", [`String]);
                            ("start_date", [`Null;
                                            `Null]);
                            ("transfer_data",
                             [`ObjectN
                                [("", [`Null]); ("amount_percent", [`Null]);
                                 ("destination", [`Null])]]);
                            ("trial", [`Boolean]);
                            ("trial_end", [`Null;
                                           `Null])]))]]);
                ("proration_behavior", [`String])]) ~dtr ~loc ~style ~explode
      _x
  
  let namevalues_of_t_e00ab899ed ~p ~op ~loc ~style ~explode
    (_x : t_e00ab899ed) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("end_behavior", match _x.end_behavior with | None -> []
          | Some _x -> [`String]);
         ("phases", match _x.phases with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_8527d4f3ba) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("add_invoice_items",
                                     match _x.add_invoice_items with
                                     | None -> []
                                     | Some _x -> [`Array
                                                     ((List.map (fun (_x : t_4bc3539de0) ->
                                                         `Singleton (`Null))
                                                         _x))]);
                                    ("application_fee_percent",
                                     match _x.application_fee_percent with
                                     | None -> [] | Some _x -> [`Number]);
                                    ("automatic_tax",
                                     match _x.automatic_tax with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("enabled",
                                                       let _x = _x.enabled in
                                                       [`Null]);
                                                      ("liability",
                                                       match _x.liability with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("billing_cycle_anchor",
                                     match _x.billing_cycle_anchor with
                                     | None -> [] | Some _x -> [`String]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_fcf3f3cc4f _x -> `Null
                                                   | T_e2c0378d25 _x -> `Null]);
                                    ("collection_method",
                                     match _x.collection_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("coupon", match _x.coupon with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_payment_method",
                                     match _x.default_payment_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_tax_rates",
                                     match _x.default_tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_30f5bb2100 _x -> `Null]);
                                    ("description", match _x.description with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | String_ _x -> `Null
                                                   | T_9b7bcd629e _x -> `Null]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_9f51d1fd63 _x -> `Null
                                                   | T_111a9f2365 _x -> `Null]);
                                    ("end_date", match _x.end_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_f939c879ff _x -> `Null]);
                                    ("invoice_settings",
                                     match _x.invoice_settings with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("account_tax_ids",
                                                       match _x.account_tax_ids with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("days_until_due",
                                                       match _x.days_until_due with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("issuer",
                                                       match _x.issuer with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("items", let _x = _x.items in
                                     [`Array
                                        ((List.map (fun (_x : t_1ccaef01fc) ->
                                            `Singleton (`Null)) _x))]);
                                    ("iterations", match _x.iterations with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [`ObjectN [("", [`Null])]]);
                                    ("on_behalf_of",
                                     match _x.on_behalf_of with | None -> []
                                     | Some _x -> [`String]);
                                    ("proration_behavior",
                                     match _x.proration_behavior with
                                     | None -> [] | Some _x -> [`String]);
                                    ("start_date", match _x.start_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_78925a3533 _x -> `Null]);
                                    ("transfer_data",
                                     match _x.transfer_data with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("amount_percent",
                                                       match _x.amount_percent with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("destination",
                                                       let _x = _x.destination in
                                                       [`Null])]]);
                                    ("trial", match _x.trial with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("trial_end", match _x.trial_end with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_8bbff7aec4 _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_e00ab899ed)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_c97f96140a ~p ~op ~loc ~style ~explode
    (_x : t_c97f96140a) =
    _string_of ~kind:(
      match _x with
      | T_9868ecd2e2 _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_c97f96140a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c97f96140a ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_9868ecd2e2) in
        Option.map (fun _y : t_c97f96140a -> T_9868ecd2e2 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_c97f96140a -> Ptime_t _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_c97f96140a ~p ~op ~loc ~style ~explode
    (_x : t_c97f96140a) =
    _namevalues_of ~kind:(
      match _x with
      | T_9868ecd2e2 _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_c97f96140a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_991e5f6179 ~p ~op ~loc ~style ~explode
    (_x : t_991e5f6179) =
    _string_of ~kind:(
      match _x with
      | Ptime_t _x -> `Integer
      | T_1b74fb906e _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_991e5f6179)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_991e5f6179 ~loc ~style ~explode (_x : string) =
    [(let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_991e5f6179 -> Ptime_t _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_1b74fb906e) in
        Option.map (fun _y : t_991e5f6179 -> T_1b74fb906e _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_991e5f6179 ~p ~op ~loc ~style ~explode
    (_x : t_991e5f6179) =
    _namevalues_of ~kind:(
      match _x with
      | Ptime_t _x -> `Integer
      | T_1b74fb906e _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_991e5f6179)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_0948902ade ~p ~op ~loc ~style ~explode
    (_x : t_0948902ade) =
    _string_of ~kind:(
      match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_35bc81453e _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_0948902ade)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0948902ade ~loc ~style ~explode (_x : string) =
    [(let kind = `Array [(`List (`String))] in
        let dtr = (Json_encoding.destruct
                     (Json_encoding.list Json_encoding.string)) in
        Option.map (fun _y : t_0948902ade -> StringList _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_35bc81453e) in
        Option.map (fun _y : t_0948902ade -> T_35bc81453e _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_0948902ade ~p ~op ~loc ~style ~explode
    (_x : t_0948902ade) =
    _namevalues_of ~kind:(
      match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_35bc81453e _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_0948902ade)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_5bdcc41c2a ~p ~op ~loc ~style ~explode
    (_x : t_5bdcc41c2a) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("billing_cycle_anchor", match _x.billing_cycle_anchor with
          | None -> []
          | Some _x -> [match _x with
                        | T_bea294789e _x -> `String
                        | Ptime_t _x -> `Integer]);
         ("cancel_at", match _x.cancel_at with | None -> []
          | Some _x -> [match _x with
                        | Ptime_t _x -> `Integer
                        | T_b35fff3e2f _x -> `String]);
         ("cancel_at_period_end", match _x.cancel_at_period_end with
          | None -> [] | Some _x -> [`Boolean]);
         ("cancel_now", match _x.cancel_now with | None -> []
          | Some _x -> [`Boolean]);
         ("default_tax_rates", match _x.default_tax_rates with | None -> []
          | Some _x -> [match _x with
                        | StringList _x ->
                          `Array
                            ((List.map (fun (_x : string) ->
                                `Singleton (`String)) _x))
                        | T_97b4206ea3 _x -> `String]);
         ("items", match _x.items with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_5819877672) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_33225edb3a _x -> `Null
                                                   | T_98f2f687bc _x -> `Null]);
                                    ("clear_usage", match _x.clear_usage with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("deleted", match _x.deleted with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_2d42449240 _x -> `Null
                                                   | T_759f36a333 _x -> `Null]);
                                    ("id", match _x.id with | None -> []
                                     | Some _x -> [`String]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_03e2d7b3db _x -> `Null
                                                   | T_acb33ab021 _x -> `Null]);
                                    ("price", match _x.price with
                                     | None -> [] | Some _x -> [`String]);
                                    ("price_data", match _x.price_data with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("currency",
                                                       let _x = _x.currency in
                                                       [`Null]);
                                                      ("product",
                                                       let _x = _x.product in
                                                       [`Null]);
                                                      ("recurring",
                                                       let _x = _x.recurring in
                                                       [`Null]);
                                                      ("tax_behavior",
                                                       match _x.tax_behavior with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount",
                                                       match _x.unit_amount with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount_decimal",
                                                       match _x.unit_amount_decimal with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("quantity", match _x.quantity with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("tax_rates", match _x.tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_ce5fed8dc6 _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String]);
         ("proration_date", match _x.proration_date with | None -> []
          | Some _x -> [`Integer]);
         ("resume_at", match _x.resume_at with | None -> []
          | Some _x -> [`String]);
         ("start_date", match _x.start_date with | None -> []
          | Some _x -> [`Integer]);
         ("trial_end", match _x.trial_end with | None -> []
          | Some _x -> [match _x with
                        | T_0508bf77fd _x -> `String
                        | Ptime_t _x -> `Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_5bdcc41c2a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_5bdcc41c2a ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_5bdcc41c2a) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("billing_cycle_anchor", [`String;
                                                        `Integer]);
                ("cancel_at", [`Integer;
                               `String]);
                ("cancel_at_period_end", [`Boolean]);
                ("cancel_now", [`Boolean]);
                ("default_tax_rates", [`Array [(`List (`String))];
                                       `String]);
                ("items",
                 [`Array
                    [(`List
                        (`ObjectN
                           [("", [`Any]);
                            ("billing_thresholds", [`Null;
                                                    `Null]);
                            ("clear_usage", [`Boolean]);
                            ("deleted", [`Boolean]);
                            ("discounts", [`Null;
                                           `Null]);
                            ("id", [`String]); ("metadata", [`Null;
                                                             `Null]);
                            ("price", [`String]);
                            ("price_data",
                             [`ObjectN
                                [("", [`Null]); ("currency", [`Null]);
                                 ("product", [`Null]);
                                 ("recurring", [`Null]);
                                 ("tax_behavior", [`Null]);
                                 ("unit_amount", [`Null]);
                                 ("unit_amount_decimal", [`Null])]]);
                            ("quantity", [`Integer]);
                            ("tax_rates", [`Null;
                                           `Null])]))]]);
                ("proration_behavior", [`String]);
                ("proration_date", [`Integer]); ("resume_at", [`String]);
                ("start_date", [`Integer]);
                ("trial_end", [`String;
                               `Integer])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_5bdcc41c2a ~p ~op ~loc ~style ~explode
    (_x : t_5bdcc41c2a) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("billing_cycle_anchor", match _x.billing_cycle_anchor with
          | None -> []
          | Some _x -> [match _x with
                        | T_bea294789e _x -> `String
                        | Ptime_t _x -> `Integer]);
         ("cancel_at", match _x.cancel_at with | None -> []
          | Some _x -> [match _x with
                        | Ptime_t _x -> `Integer
                        | T_b35fff3e2f _x -> `String]);
         ("cancel_at_period_end", match _x.cancel_at_period_end with
          | None -> [] | Some _x -> [`Boolean]);
         ("cancel_now", match _x.cancel_now with | None -> []
          | Some _x -> [`Boolean]);
         ("default_tax_rates", match _x.default_tax_rates with | None -> []
          | Some _x -> [match _x with
                        | StringList _x ->
                          `Array
                            ((List.map (fun (_x : string) ->
                                `Singleton (`String)) _x))
                        | T_97b4206ea3 _x -> `String]);
         ("items", match _x.items with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_5819877672) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_33225edb3a _x -> `Null
                                                   | T_98f2f687bc _x -> `Null]);
                                    ("clear_usage", match _x.clear_usage with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("deleted", match _x.deleted with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_2d42449240 _x -> `Null
                                                   | T_759f36a333 _x -> `Null]);
                                    ("id", match _x.id with | None -> []
                                     | Some _x -> [`String]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_03e2d7b3db _x -> `Null
                                                   | T_acb33ab021 _x -> `Null]);
                                    ("price", match _x.price with
                                     | None -> [] | Some _x -> [`String]);
                                    ("price_data", match _x.price_data with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("currency",
                                                       let _x = _x.currency in
                                                       [`Null]);
                                                      ("product",
                                                       let _x = _x.product in
                                                       [`Null]);
                                                      ("recurring",
                                                       let _x = _x.recurring in
                                                       [`Null]);
                                                      ("tax_behavior",
                                                       match _x.tax_behavior with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount",
                                                       match _x.unit_amount with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount_decimal",
                                                       match _x.unit_amount_decimal with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("quantity", match _x.quantity with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("tax_rates", match _x.tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_ce5fed8dc6 _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String]);
         ("proration_date", match _x.proration_date with | None -> []
          | Some _x -> [`Integer]);
         ("resume_at", match _x.resume_at with | None -> []
          | Some _x -> [`String]);
         ("start_date", match _x.start_date with | None -> []
          | Some _x -> [`Integer]);
         ("trial_end", match _x.trial_end with | None -> []
          | Some _x -> [match _x with
                        | T_0508bf77fd _x -> `String
                        | Ptime_t _x -> `Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_5bdcc41c2a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_b0fda171f8 ~p ~op ~loc ~style ~explode
    (_x : t_b0fda171f8) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_aeb37a8a18) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("billing_thresholds", match _x.billing_thresholds with
                   | None -> []
                   | Some _x -> [match _x with
                                 | T_43cfaf3452 _x ->
                                   `ObjectN
                                     [("", [`Null]);
                                      ("usage_gte", let _x = _x.usage_gte in
                                       [`Null])]
                                 | T_1fc4a74891 _x -> `String]);
                  ("clear_usage", match _x.clear_usage with | None -> []
                   | Some _x -> [`Boolean]);
                  ("deleted", match _x.deleted with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_d8df00cf5d _x ->
                                   `Array
                                     ((List.map (fun (_x : t_eeeccfccc0) ->
                                         `Singleton (`Null)) _x))
                                 | T_456d9fa0b3 _x -> `String]);
                  ("id", match _x.id with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_7bd6c963e5 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_79217cf88a _x -> `String]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("recurring", let _x = _x.recurring in
                                     [`ObjectN
                                        [("", [`Null]);
                                         ("interval", let _x = _x.interval in
                                          [`Null]);
                                         ("interval_count",
                                          match _x.interval_count with
                                          | None -> [] | Some _x -> [`Null])]]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_46aae4974b _x -> `String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_b0fda171f8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b0fda171f8 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b0fda171f8) in _string_to
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", [`Any]);
                       ("billing_thresholds",
                        [`ObjectN [("", [`Null]); ("usage_gte", [`Null])];
                         `String]);
                       ("clear_usage", [`Boolean]); ("deleted", [`Boolean]);
                       ("discounts", [`Array [(`List (`Null))];
                                      `String]);
                       ("id", [`String]);
                       ("metadata", [`ObjectN [("", [`Null])];
                                     `String]);
                       ("price", [`String]);
                       ("price_data",
                        [`ObjectN
                           [("", [`Any]); ("currency", [`String]);
                            ("product", [`String]);
                            ("recurring",
                             [`ObjectN
                                [("", [`Null]); ("interval", [`Null]);
                                 ("interval_count", [`Null])]]);
                            ("tax_behavior", [`String]);
                            ("unit_amount", [`Integer]);
                            ("unit_amount_decimal", [`String])]]);
                       ("quantity", [`Integer]);
                       ("tax_rates", [`Array [(`List (`Null))];
                                      `String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b0fda171f8 ~p ~op ~loc ~style ~explode
    (_x : t_b0fda171f8) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_aeb37a8a18) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("billing_thresholds", match _x.billing_thresholds with
                   | None -> []
                   | Some _x -> [match _x with
                                 | T_43cfaf3452 _x ->
                                   `ObjectN
                                     [("", [`Null]);
                                      ("usage_gte", let _x = _x.usage_gte in
                                       [`Null])]
                                 | T_1fc4a74891 _x -> `String]);
                  ("clear_usage", match _x.clear_usage with | None -> []
                   | Some _x -> [`Boolean]);
                  ("deleted", match _x.deleted with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_d8df00cf5d _x ->
                                   `Array
                                     ((List.map (fun (_x : t_eeeccfccc0) ->
                                         `Singleton (`Null)) _x))
                                 | T_456d9fa0b3 _x -> `String]);
                  ("id", match _x.id with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_7bd6c963e5 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_79217cf88a _x -> `String]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("recurring", let _x = _x.recurring in
                                     [`ObjectN
                                        [("", [`Null]);
                                         ("interval", let _x = _x.interval in
                                          [`Null]);
                                         ("interval_count",
                                          match _x.interval_count with
                                          | None -> [] | Some _x -> [`Null])]]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_46aae4974b _x -> `String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_b0fda171f8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_876fb1bc40 ~p ~op ~loc ~style ~explode
    (_x : t_876fb1bc40) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_876fb1bc40)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_876fb1bc40 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_876fb1bc40) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_876fb1bc40 ~p ~op ~loc ~style ~explode
    (_x : t_876fb1bc40) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_876fb1bc40)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_9ad9bd5605 ~p ~op ~loc ~style ~explode
    (_x : t_9ad9bd5605) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9ad9bd5605)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9ad9bd5605 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_9ad9bd5605) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_9ad9bd5605 ~p ~op ~loc ~style ~explode
    (_x : t_9ad9bd5605) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9ad9bd5605)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_915f11076c ~p ~op ~loc ~style ~explode
    (_x : t_915f11076c) =
    _string_of ~kind:(
      match _x with
      | T_69db67d24d _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_915f11076c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_915f11076c ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_69db67d24d) in
        Option.map (fun _y : t_915f11076c -> T_69db67d24d _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_915f11076c -> Ptime_t _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_915f11076c ~p ~op ~loc ~style ~explode
    (_x : t_915f11076c) =
    _namevalues_of ~kind:(
      match _x with
      | T_69db67d24d _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_915f11076c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e3f1f34a06 ~p ~op ~loc ~style ~explode
    (_x : t_e3f1f34a06) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("address", match _x.address with | None -> []
          | Some _x -> [match _x with
                        | T_0452c57eaf _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("city", match _x.city with | None -> []
                              | Some _x -> [`String]);
                             ("country", match _x.country with | None -> []
                              | Some _x -> [`String]);
                             ("line1", match _x.line1 with | None -> []
                              | Some _x -> [`String]);
                             ("line2", match _x.line2 with | None -> []
                              | Some _x -> [`String]);
                             ("postal_code", match _x.postal_code with
                              | None -> [] | Some _x -> [`String]);
                             ("state", match _x.state with | None -> []
                              | Some _x -> [`String])]
                        | T_0dafd7563b _x -> `String]);
         ("shipping", match _x.shipping with | None -> []
          | Some _x -> [match _x with
                        | T_3c3062fd9a _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("address", let _x = _x.address in
                              [`ObjectN
                                 [("", [`Any]);
                                  ("city", match _x.city with | None -> []
                                   | Some _x -> [`String]);
                                  ("country", match _x.country with
                                   | None -> [] | Some _x -> [`String]);
                                  ("line1", match _x.line1 with | None -> []
                                   | Some _x -> [`String]);
                                  ("line2", match _x.line2 with | None -> []
                                   | Some _x -> [`String]);
                                  ("postal_code", match _x.postal_code with
                                   | None -> [] | Some _x -> [`String]);
                                  ("state", match _x.state with | None -> []
                                   | Some _x -> [`String])]]);
                             ("name", let _x = _x.name in [`String]);
                             ("phone", match _x.phone with | None -> []
                              | Some _x -> [`String])]
                        | T_5b26f4839a _x -> `String]);
         ("tax", match _x.tax with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("ip_address", match _x.ip_address with
                            | None -> []
                            | Some _x -> [match _x with
                                          | String_ _x -> `String
                                          | T_af1dd0111d _x -> `String])]]);
         ("tax_exempt", match _x.tax_exempt with | None -> []
          | Some _x -> [`String]);
         ("tax_ids", match _x.tax_ids with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_1e501e7dc4) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("type", let _x = _x.type_ in [`String]);
                                    ("value", let _x = _x.value in [`String])]))
                              _x))])])
      ~ctr:(Json_encoding.construct Encoders'.t_e3f1f34a06)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e3f1f34a06 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e3f1f34a06) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]);
                ("address",
                 [`ObjectN
                    [("", [`Any]); ("city", [`String]);
                     ("country", [`String]); ("line1", [`String]);
                     ("line2", [`String]); ("postal_code", [`String]);
                     ("state", [`String])];
                  `String]);
                ("shipping",
                 [`ObjectN
                    [("", [`Any]);
                     ("address",
                      [`ObjectN
                         [("", [`Any]); ("city", [`String]);
                          ("country", [`String]); ("line1", [`String]);
                          ("line2", [`String]); ("postal_code", [`String]);
                          ("state", [`String])]]);
                     ("name", [`String]); ("phone", [`String])];
                  `String]);
                ("tax",
                 [`ObjectN [("", [`Any]); ("ip_address", [`String;
                                                          `String])]]);
                ("tax_exempt", [`String]);
                ("tax_ids",
                 [`Array
                    [(`List
                        (`ObjectN
                           [("", [`Any]); ("type", [`String]);
                            ("value", [`String])]))]])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e3f1f34a06 ~p ~op ~loc ~style ~explode
    (_x : t_e3f1f34a06) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("address", match _x.address with | None -> []
          | Some _x -> [match _x with
                        | T_0452c57eaf _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("city", match _x.city with | None -> []
                              | Some _x -> [`String]);
                             ("country", match _x.country with | None -> []
                              | Some _x -> [`String]);
                             ("line1", match _x.line1 with | None -> []
                              | Some _x -> [`String]);
                             ("line2", match _x.line2 with | None -> []
                              | Some _x -> [`String]);
                             ("postal_code", match _x.postal_code with
                              | None -> [] | Some _x -> [`String]);
                             ("state", match _x.state with | None -> []
                              | Some _x -> [`String])]
                        | T_0dafd7563b _x -> `String]);
         ("shipping", match _x.shipping with | None -> []
          | Some _x -> [match _x with
                        | T_3c3062fd9a _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("address", let _x = _x.address in
                              [`ObjectN
                                 [("", [`Any]);
                                  ("city", match _x.city with | None -> []
                                   | Some _x -> [`String]);
                                  ("country", match _x.country with
                                   | None -> [] | Some _x -> [`String]);
                                  ("line1", match _x.line1 with | None -> []
                                   | Some _x -> [`String]);
                                  ("line2", match _x.line2 with | None -> []
                                   | Some _x -> [`String]);
                                  ("postal_code", match _x.postal_code with
                                   | None -> [] | Some _x -> [`String]);
                                  ("state", match _x.state with | None -> []
                                   | Some _x -> [`String])]]);
                             ("name", let _x = _x.name in [`String]);
                             ("phone", match _x.phone with | None -> []
                              | Some _x -> [`String])]
                        | T_5b26f4839a _x -> `String]);
         ("tax", match _x.tax with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("ip_address", match _x.ip_address with
                            | None -> []
                            | Some _x -> [match _x with
                                          | String_ _x -> `String
                                          | T_af1dd0111d _x -> `String])]]);
         ("tax_exempt", match _x.tax_exempt with | None -> []
          | Some _x -> [`String]);
         ("tax_ids", match _x.tax_ids with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_1e501e7dc4) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("type", let _x = _x.type_ in [`String]);
                                    ("value", let _x = _x.value in [`String])]))
                              _x))])])
      ~ctr:(Json_encoding.construct Encoders'.t_e3f1f34a06)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_ec989090c2 ~p ~op ~loc ~style ~explode
    (_x : t_ec989090c2) =
    _string_of ~kind:(
      match _x with
      | T_f66f2e78fb _x ->
        `Array
          ((List.map (fun (_x : t_8496089bd2) ->
              `Singleton
                (`ObjectN
                   [("", [`Any]);
                    ("coupon", match _x.coupon with | None -> []
                     | Some _x -> [`String]);
                    ("discount", match _x.discount with | None -> []
                     | Some _x -> [`String]);
                    ("promotion_code", match _x.promotion_code with
                     | None -> [] | Some _x -> [`String])])) _x))
      | T_171fdc29dc _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ec989090c2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ec989090c2 ~loc ~style ~explode (_x : string) =
    [(let kind = `Array
                   [(`List
                       (`ObjectN
                          [("", [`Any]); ("coupon", [`String]);
                           ("discount", [`String]);
                           ("promotion_code", [`String])]))] in
        let dtr = (Json_encoding.destruct Encoders'.t_f66f2e78fb) in
        Option.map (fun _y : t_ec989090c2 -> T_f66f2e78fb _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_171fdc29dc) in
        Option.map (fun _y : t_ec989090c2 -> T_171fdc29dc _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_ec989090c2 ~p ~op ~loc ~style ~explode
    (_x : t_ec989090c2) =
    _namevalues_of ~kind:(
      match _x with
      | T_f66f2e78fb _x ->
        `Array
          ((List.map (fun (_x : t_8496089bd2) ->
              `Singleton
                (`ObjectN
                   [("", [`Any]);
                    ("coupon", match _x.coupon with | None -> []
                     | Some _x -> [`String]);
                    ("discount", match _x.discount with | None -> []
                     | Some _x -> [`String]);
                    ("promotion_code", match _x.promotion_code with
                     | None -> [] | Some _x -> [`String])])) _x))
      | T_171fdc29dc _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ec989090c2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f80679d274 ~p ~op ~loc ~style ~explode
    (_x : t_f80679d274) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_aa55ae2178) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("currency", match _x.currency with | None -> []
                   | Some _x -> [`String]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("discountable", match _x.discountable with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_baf0f88c14 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_6e64dcd9e0) ->
                                         `Singleton (`Null)) _x))
                                 | T_4580487f41 _x -> `String]);
                  ("invoiceitem", match _x.invoiceitem with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_6f4bafa2b8 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_3f2ec43e76 _x -> `String]);
                  ("period", match _x.period with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("end", let _x = _x.end_ in [`Integer]);
                                    ("start", let _x = _x.start in
                                     [`Integer])]]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_behavior", match _x.tax_behavior with | None -> []
                   | Some _x -> [`String]);
                  ("tax_code", match _x.tax_code with | None -> []
                   | Some _x -> [match _x with
                                 | String_ _x -> `String
                                 | T_ea76c4a152 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_ef61320518 _x -> `String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_f80679d274)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f80679d274 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f80679d274) in _string_to
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", [`Any]); ("amount", [`Integer]);
                       ("currency", [`String]); ("description", [`String]);
                       ("discountable", [`Boolean]);
                       ("discounts", [`Array [(`List (`Null))];
                                      `String]);
                       ("invoiceitem", [`String]);
                       ("metadata", [`ObjectN [("", [`Null])];
                                     `String]);
                       ("period",
                        [`ObjectN
                           [("", [`Any]); ("end", [`Integer]);
                            ("start", [`Integer])]]);
                       ("price", [`String]);
                       ("price_data",
                        [`ObjectN
                           [("", [`Any]); ("currency", [`String]);
                            ("product", [`String]);
                            ("tax_behavior", [`String]);
                            ("unit_amount", [`Integer]);
                            ("unit_amount_decimal", [`String])]]);
                       ("quantity", [`Integer]); ("tax_behavior", [`String]);
                       ("tax_code", [`String;
                                     `String]);
                       ("tax_rates", [`Array [(`List (`Null))];
                                      `String]);
                       ("unit_amount", [`Integer]);
                       ("unit_amount_decimal", [`String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f80679d274 ~p ~op ~loc ~style ~explode
    (_x : t_f80679d274) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_aa55ae2178) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("currency", match _x.currency with | None -> []
                   | Some _x -> [`String]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("discountable", match _x.discountable with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_baf0f88c14 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_6e64dcd9e0) ->
                                         `Singleton (`Null)) _x))
                                 | T_4580487f41 _x -> `String]);
                  ("invoiceitem", match _x.invoiceitem with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_6f4bafa2b8 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_3f2ec43e76 _x -> `String]);
                  ("period", match _x.period with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("end", let _x = _x.end_ in [`Integer]);
                                    ("start", let _x = _x.start in
                                     [`Integer])]]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_behavior", match _x.tax_behavior with | None -> []
                   | Some _x -> [`String]);
                  ("tax_code", match _x.tax_code with | None -> []
                   | Some _x -> [match _x with
                                 | String_ _x -> `String
                                 | T_ea76c4a152 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_ef61320518 _x -> `String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_f80679d274)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_32a68f7fa5 ~p ~op ~loc ~style ~explode
    (_x : t_32a68f7fa5) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("type", let _x = _x.type_ in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_32a68f7fa5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_32a68f7fa5 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_32a68f7fa5) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("account", [`String]); ("type", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_32a68f7fa5 ~p ~op ~loc ~style ~explode
    (_x : t_32a68f7fa5) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("type", let _x = _x.type_ in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_32a68f7fa5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_a83913a4f2 ~p ~op ~loc ~style ~explode
    (_x : t_a83913a4f2) =
    _string_of ~kind:(
      match _x with
      | String_ _x -> `String
      | T_e0b9016d7e _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a83913a4f2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a83913a4f2 ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_a83913a4f2 -> String_ _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_e0b9016d7e) in
        Option.map (fun _y : t_a83913a4f2 -> T_e0b9016d7e _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_a83913a4f2 ~p ~op ~loc ~style ~explode
    (_x : t_a83913a4f2) =
    _namevalues_of ~kind:(
      match _x with
      | String_ _x -> `String
      | T_e0b9016d7e _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a83913a4f2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_2fbe7f660e ~p ~op ~loc ~style ~explode
    (_x : t_2fbe7f660e) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]); ("enabled", let _x = _x.enabled in [`Boolean]);
         ("liability", match _x.liability with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("account", match _x.account with | None -> []
                            | Some _x -> [`String]);
                           ("type", let _x = _x.type_ in [`String])]])])
      ~ctr:(Json_encoding.construct Encoders'.t_2fbe7f660e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2fbe7f660e ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_2fbe7f660e) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("enabled", [`Boolean]);
                ("liability",
                 [`ObjectN
                    [("", [`Any]); ("account", [`String]);
                     ("type", [`String])]])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_2fbe7f660e ~p ~op ~loc ~style ~explode
    (_x : t_2fbe7f660e) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]); ("enabled", let _x = _x.enabled in [`Boolean]);
         ("liability", match _x.liability with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("account", match _x.account with | None -> []
                            | Some _x -> [`String]);
                           ("type", let _x = _x.type_ in [`String])]])])
      ~ctr:(Json_encoding.construct Encoders'.t_2fbe7f660e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_fa6f579b99 ~p ~op ~loc ~style ~explode
    (_x : t_fa6f579b99) =
    _string_of ~kind:(
      match _x with
      | String_ _x -> `String
      | T_b7e53e8ff3 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_fa6f579b99)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fa6f579b99 ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_fa6f579b99 -> String_ _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_b7e53e8ff3) in
        Option.map (fun _y : t_fa6f579b99 -> T_b7e53e8ff3 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_fa6f579b99 ~p ~op ~loc ~style ~explode
    (_x : t_fa6f579b99) =
    _namevalues_of ~kind:(
      match _x with
      | String_ _x -> `String
      | T_b7e53e8ff3 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_fa6f579b99)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_3122be5d2a ~p ~op ~loc ~style ~explode
    (_x : t_3122be5d2a) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_3122be5d2a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_3122be5d2a ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_3122be5d2a) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_3122be5d2a ~p ~op ~loc ~style ~explode
    (_x : t_3122be5d2a) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_3122be5d2a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_fd8d01a560 ~p ~op ~loc ~style ~explode
    (_x : t_fd8d01a560) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("end_behavior", match _x.end_behavior with | None -> []
          | Some _x -> [`String]);
         ("phases", match _x.phases with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_e1b3852c81) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("add_invoice_items",
                                     match _x.add_invoice_items with
                                     | None -> []
                                     | Some _x -> [`Array
                                                     ((List.map (fun (_x : t_de0e385969) ->
                                                         `Singleton (`Null))
                                                         _x))]);
                                    ("application_fee_percent",
                                     match _x.application_fee_percent with
                                     | None -> [] | Some _x -> [`Number]);
                                    ("automatic_tax",
                                     match _x.automatic_tax with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("enabled",
                                                       let _x = _x.enabled in
                                                       [`Null]);
                                                      ("liability",
                                                       match _x.liability with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("billing_cycle_anchor",
                                     match _x.billing_cycle_anchor with
                                     | None -> [] | Some _x -> [`String]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_1b4079495a _x -> `Null
                                                   | T_4edf93db52 _x -> `Null]);
                                    ("collection_method",
                                     match _x.collection_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("coupon", match _x.coupon with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_payment_method",
                                     match _x.default_payment_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_tax_rates",
                                     match _x.default_tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_73136db6c1 _x -> `Null]);
                                    ("description", match _x.description with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | String_ _x -> `Null
                                                   | T_ededbab84a _x -> `Null]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_7e453c6eea _x -> `Null
                                                   | T_216683d889 _x -> `Null]);
                                    ("end_date", match _x.end_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_b77919689a _x -> `Null]);
                                    ("invoice_settings",
                                     match _x.invoice_settings with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("account_tax_ids",
                                                       match _x.account_tax_ids with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("days_until_due",
                                                       match _x.days_until_due with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("issuer",
                                                       match _x.issuer with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("items", let _x = _x.items in
                                     [`Array
                                        ((List.map (fun (_x : t_dee69222eb) ->
                                            `Singleton (`Null)) _x))]);
                                    ("iterations", match _x.iterations with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [`ObjectN [("", [`Null])]]);
                                    ("on_behalf_of",
                                     match _x.on_behalf_of with | None -> []
                                     | Some _x -> [`String]);
                                    ("proration_behavior",
                                     match _x.proration_behavior with
                                     | None -> [] | Some _x -> [`String]);
                                    ("start_date", match _x.start_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_03bff8c62d _x -> `Null]);
                                    ("transfer_data",
                                     match _x.transfer_data with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("amount_percent",
                                                       match _x.amount_percent with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("destination",
                                                       let _x = _x.destination in
                                                       [`Null])]]);
                                    ("trial", match _x.trial with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("trial_end", match _x.trial_end with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_fb00d7ae56 _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_fd8d01a560)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fd8d01a560 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_fd8d01a560) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("end_behavior", [`String]);
                ("phases",
                 [`Array
                    [(`List
                        (`ObjectN
                           [("", [`Any]);
                            ("add_invoice_items", [`Array [(`List (`Null))]]);
                            ("application_fee_percent", [`Number]);
                            ("automatic_tax",
                             [`ObjectN
                                [("", [`Null]); ("enabled", [`Null]);
                                 ("liability", [`Null])]]);
                            ("billing_cycle_anchor", [`String]);
                            ("billing_thresholds", [`Null;
                                                    `Null]);
                            ("collection_method", [`String]);
                            ("coupon", [`String]);
                            ("default_payment_method", [`String]);
                            ("default_tax_rates", [`Null;
                                                   `Null]);
                            ("description", [`Null;
                                             `Null]);
                            ("discounts", [`Null;
                                           `Null]);
                            ("end_date", [`Null;
                                          `Null]);
                            ("invoice_settings",
                             [`ObjectN
                                [("", [`Null]); ("account_tax_ids", [`Null]);
                                 ("days_until_due", [`Null]);
                                 ("issuer", [`Null])]]);
                            ("items", [`Array [(`List (`Null))]]);
                            ("iterations", [`Integer]);
                            ("metadata", [`ObjectN [("", [`Null])]]);
                            ("on_behalf_of", [`String]);
                            ("proration_behavior", [`String]);
                            ("start_date", [`Null;
                                            `Null]);
                            ("transfer_data",
                             [`ObjectN
                                [("", [`Null]); ("amount_percent", [`Null]);
                                 ("destination", [`Null])]]);
                            ("trial", [`Boolean]);
                            ("trial_end", [`Null;
                                           `Null])]))]]);
                ("proration_behavior", [`String])]) ~dtr ~loc ~style ~explode
      _x
  
  let namevalues_of_t_fd8d01a560 ~p ~op ~loc ~style ~explode
    (_x : t_fd8d01a560) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("end_behavior", match _x.end_behavior with | None -> []
          | Some _x -> [`String]);
         ("phases", match _x.phases with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_e1b3852c81) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("add_invoice_items",
                                     match _x.add_invoice_items with
                                     | None -> []
                                     | Some _x -> [`Array
                                                     ((List.map (fun (_x : t_de0e385969) ->
                                                         `Singleton (`Null))
                                                         _x))]);
                                    ("application_fee_percent",
                                     match _x.application_fee_percent with
                                     | None -> [] | Some _x -> [`Number]);
                                    ("automatic_tax",
                                     match _x.automatic_tax with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("enabled",
                                                       let _x = _x.enabled in
                                                       [`Null]);
                                                      ("liability",
                                                       match _x.liability with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("billing_cycle_anchor",
                                     match _x.billing_cycle_anchor with
                                     | None -> [] | Some _x -> [`String]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_1b4079495a _x -> `Null
                                                   | T_4edf93db52 _x -> `Null]);
                                    ("collection_method",
                                     match _x.collection_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("coupon", match _x.coupon with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_payment_method",
                                     match _x.default_payment_method with
                                     | None -> [] | Some _x -> [`String]);
                                    ("default_tax_rates",
                                     match _x.default_tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_73136db6c1 _x -> `Null]);
                                    ("description", match _x.description with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | String_ _x -> `Null
                                                   | T_ededbab84a _x -> `Null]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_7e453c6eea _x -> `Null
                                                   | T_216683d889 _x -> `Null]);
                                    ("end_date", match _x.end_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_b77919689a _x -> `Null]);
                                    ("invoice_settings",
                                     match _x.invoice_settings with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("account_tax_ids",
                                                       match _x.account_tax_ids with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("days_until_due",
                                                       match _x.days_until_due with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("issuer",
                                                       match _x.issuer with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("items", let _x = _x.items in
                                     [`Array
                                        ((List.map (fun (_x : t_dee69222eb) ->
                                            `Singleton (`Null)) _x))]);
                                    ("iterations", match _x.iterations with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [`ObjectN [("", [`Null])]]);
                                    ("on_behalf_of",
                                     match _x.on_behalf_of with | None -> []
                                     | Some _x -> [`String]);
                                    ("proration_behavior",
                                     match _x.proration_behavior with
                                     | None -> [] | Some _x -> [`String]);
                                    ("start_date", match _x.start_date with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_03bff8c62d _x -> `Null]);
                                    ("transfer_data",
                                     match _x.transfer_data with | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("amount_percent",
                                                       match _x.amount_percent with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("destination",
                                                       let _x = _x.destination in
                                                       [`Null])]]);
                                    ("trial", match _x.trial with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("trial_end", match _x.trial_end with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | Ptime_t _x -> `Null
                                                   | T_fb00d7ae56 _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_fd8d01a560)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_10fbc38ff9 ~p ~op ~loc ~style ~explode
    (_x : t_10fbc38ff9) =
    _string_of ~kind:(
      match _x with
      | T_c00ffe585f _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_10fbc38ff9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_10fbc38ff9 ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_c00ffe585f) in
        Option.map (fun _y : t_10fbc38ff9 -> T_c00ffe585f _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_10fbc38ff9 -> Ptime_t _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_10fbc38ff9 ~p ~op ~loc ~style ~explode
    (_x : t_10fbc38ff9) =
    _namevalues_of ~kind:(
      match _x with
      | T_c00ffe585f _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_10fbc38ff9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_cfe86625c3 ~p ~op ~loc ~style ~explode
    (_x : t_cfe86625c3) =
    _string_of ~kind:(
      match _x with
      | Ptime_t _x -> `Integer
      | T_b55c6a85e5 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_cfe86625c3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_cfe86625c3 ~loc ~style ~explode (_x : string) =
    [(let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_cfe86625c3 -> Ptime_t _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_b55c6a85e5) in
        Option.map (fun _y : t_cfe86625c3 -> T_b55c6a85e5 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_cfe86625c3 ~p ~op ~loc ~style ~explode
    (_x : t_cfe86625c3) =
    _namevalues_of ~kind:(
      match _x with
      | Ptime_t _x -> `Integer
      | T_b55c6a85e5 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_cfe86625c3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_0852729626 ~p ~op ~loc ~style ~explode
    (_x : t_0852729626) =
    _string_of ~kind:(
      match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_c15b34b7e3 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_0852729626)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0852729626 ~loc ~style ~explode (_x : string) =
    [(let kind = `Array [(`List (`String))] in
        let dtr = (Json_encoding.destruct
                     (Json_encoding.list Json_encoding.string)) in
        Option.map (fun _y : t_0852729626 -> StringList _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_c15b34b7e3) in
        Option.map (fun _y : t_0852729626 -> T_c15b34b7e3 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_0852729626 ~p ~op ~loc ~style ~explode
    (_x : t_0852729626) =
    _namevalues_of ~kind:(
      match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_c15b34b7e3 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_0852729626)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_79f4876735 ~p ~op ~loc ~style ~explode
    (_x : t_79f4876735) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("billing_cycle_anchor", match _x.billing_cycle_anchor with
          | None -> []
          | Some _x -> [match _x with
                        | T_7f55dab405 _x -> `String
                        | Ptime_t _x -> `Integer]);
         ("cancel_at", match _x.cancel_at with | None -> []
          | Some _x -> [match _x with
                        | Ptime_t _x -> `Integer
                        | T_63bf374c06 _x -> `String]);
         ("cancel_at_period_end", match _x.cancel_at_period_end with
          | None -> [] | Some _x -> [`Boolean]);
         ("cancel_now", match _x.cancel_now with | None -> []
          | Some _x -> [`Boolean]);
         ("default_tax_rates", match _x.default_tax_rates with | None -> []
          | Some _x -> [match _x with
                        | StringList _x ->
                          `Array
                            ((List.map (fun (_x : string) ->
                                `Singleton (`String)) _x))
                        | T_c61bf5df59 _x -> `String]);
         ("items", match _x.items with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_6b40ba9af2) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_c2df5b02da _x -> `Null
                                                   | T_e2db266f8b _x -> `Null]);
                                    ("clear_usage", match _x.clear_usage with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("deleted", match _x.deleted with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_f586543d2a _x -> `Null
                                                   | T_26d53ebad5 _x -> `Null]);
                                    ("id", match _x.id with | None -> []
                                     | Some _x -> [`String]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_829e1aad74 _x -> `Null
                                                   | T_e2a7ea97bf _x -> `Null]);
                                    ("price", match _x.price with
                                     | None -> [] | Some _x -> [`String]);
                                    ("price_data", match _x.price_data with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("currency",
                                                       let _x = _x.currency in
                                                       [`Null]);
                                                      ("product",
                                                       let _x = _x.product in
                                                       [`Null]);
                                                      ("recurring",
                                                       let _x = _x.recurring in
                                                       [`Null]);
                                                      ("tax_behavior",
                                                       match _x.tax_behavior with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount",
                                                       match _x.unit_amount with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount_decimal",
                                                       match _x.unit_amount_decimal with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("quantity", match _x.quantity with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("tax_rates", match _x.tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_395c2ab50f _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String]);
         ("proration_date", match _x.proration_date with | None -> []
          | Some _x -> [`Integer]);
         ("resume_at", match _x.resume_at with | None -> []
          | Some _x -> [`String]);
         ("start_date", match _x.start_date with | None -> []
          | Some _x -> [`Integer]);
         ("trial_end", match _x.trial_end with | None -> []
          | Some _x -> [match _x with
                        | T_e8a5b013fe _x -> `String
                        | Ptime_t _x -> `Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_79f4876735)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_79f4876735 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_79f4876735) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("billing_cycle_anchor", [`String;
                                                        `Integer]);
                ("cancel_at", [`Integer;
                               `String]);
                ("cancel_at_period_end", [`Boolean]);
                ("cancel_now", [`Boolean]);
                ("default_tax_rates", [`Array [(`List (`String))];
                                       `String]);
                ("items",
                 [`Array
                    [(`List
                        (`ObjectN
                           [("", [`Any]);
                            ("billing_thresholds", [`Null;
                                                    `Null]);
                            ("clear_usage", [`Boolean]);
                            ("deleted", [`Boolean]);
                            ("discounts", [`Null;
                                           `Null]);
                            ("id", [`String]); ("metadata", [`Null;
                                                             `Null]);
                            ("price", [`String]);
                            ("price_data",
                             [`ObjectN
                                [("", [`Null]); ("currency", [`Null]);
                                 ("product", [`Null]);
                                 ("recurring", [`Null]);
                                 ("tax_behavior", [`Null]);
                                 ("unit_amount", [`Null]);
                                 ("unit_amount_decimal", [`Null])]]);
                            ("quantity", [`Integer]);
                            ("tax_rates", [`Null;
                                           `Null])]))]]);
                ("proration_behavior", [`String]);
                ("proration_date", [`Integer]); ("resume_at", [`String]);
                ("start_date", [`Integer]);
                ("trial_end", [`String;
                               `Integer])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_79f4876735 ~p ~op ~loc ~style ~explode
    (_x : t_79f4876735) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("billing_cycle_anchor", match _x.billing_cycle_anchor with
          | None -> []
          | Some _x -> [match _x with
                        | T_7f55dab405 _x -> `String
                        | Ptime_t _x -> `Integer]);
         ("cancel_at", match _x.cancel_at with | None -> []
          | Some _x -> [match _x with
                        | Ptime_t _x -> `Integer
                        | T_63bf374c06 _x -> `String]);
         ("cancel_at_period_end", match _x.cancel_at_period_end with
          | None -> [] | Some _x -> [`Boolean]);
         ("cancel_now", match _x.cancel_now with | None -> []
          | Some _x -> [`Boolean]);
         ("default_tax_rates", match _x.default_tax_rates with | None -> []
          | Some _x -> [match _x with
                        | StringList _x ->
                          `Array
                            ((List.map (fun (_x : string) ->
                                `Singleton (`String)) _x))
                        | T_c61bf5df59 _x -> `String]);
         ("items", match _x.items with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_6b40ba9af2) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("billing_thresholds",
                                     match _x.billing_thresholds with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_c2df5b02da _x -> `Null
                                                   | T_e2db266f8b _x -> `Null]);
                                    ("clear_usage", match _x.clear_usage with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("deleted", match _x.deleted with
                                     | None -> [] | Some _x -> [`Boolean]);
                                    ("discounts", match _x.discounts with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_f586543d2a _x -> `Null
                                                   | T_26d53ebad5 _x -> `Null]);
                                    ("id", match _x.id with | None -> []
                                     | Some _x -> [`String]);
                                    ("metadata", match _x.metadata with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | T_829e1aad74 _x -> `Null
                                                   | T_e2a7ea97bf _x -> `Null]);
                                    ("price", match _x.price with
                                     | None -> [] | Some _x -> [`String]);
                                    ("price_data", match _x.price_data with
                                     | None -> []
                                     | Some _x -> [`ObjectN
                                                     [("", [`Null]);
                                                      ("currency",
                                                       let _x = _x.currency in
                                                       [`Null]);
                                                      ("product",
                                                       let _x = _x.product in
                                                       [`Null]);
                                                      ("recurring",
                                                       let _x = _x.recurring in
                                                       [`Null]);
                                                      ("tax_behavior",
                                                       match _x.tax_behavior with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount",
                                                       match _x.unit_amount with
                                                       | None -> []
                                                       | Some _x -> [`Null]);
                                                      ("unit_amount_decimal",
                                                       match _x.unit_amount_decimal with
                                                       | None -> []
                                                       | Some _x -> [`Null])]]);
                                    ("quantity", match _x.quantity with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("tax_rates", match _x.tax_rates with
                                     | None -> []
                                     | Some _x -> [match _x with
                                                   | StringList _x -> `Null
                                                   | T_395c2ab50f _x -> `Null])]))
                              _x))]);
         ("proration_behavior", match _x.proration_behavior with | None -> []
          | Some _x -> [`String]);
         ("proration_date", match _x.proration_date with | None -> []
          | Some _x -> [`Integer]);
         ("resume_at", match _x.resume_at with | None -> []
          | Some _x -> [`String]);
         ("start_date", match _x.start_date with | None -> []
          | Some _x -> [`Integer]);
         ("trial_end", match _x.trial_end with | None -> []
          | Some _x -> [match _x with
                        | T_e8a5b013fe _x -> `String
                        | Ptime_t _x -> `Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_79f4876735)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e14347739f ~p ~op ~loc ~style ~explode
    (_x : t_e14347739f) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_7d1d634a95) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("billing_thresholds", match _x.billing_thresholds with
                   | None -> []
                   | Some _x -> [match _x with
                                 | T_1b8cabdd62 _x ->
                                   `ObjectN
                                     [("", [`Null]);
                                      ("usage_gte", let _x = _x.usage_gte in
                                       [`Null])]
                                 | T_1713b7185f _x -> `String]);
                  ("clear_usage", match _x.clear_usage with | None -> []
                   | Some _x -> [`Boolean]);
                  ("deleted", match _x.deleted with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_f85dc498b3 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_f69ed98de2) ->
                                         `Singleton (`Null)) _x))
                                 | T_6763bb7bbe _x -> `String]);
                  ("id", match _x.id with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_9d750c61e7 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_3c592eb40f _x -> `String]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("recurring", let _x = _x.recurring in
                                     [`ObjectN
                                        [("", [`Null]);
                                         ("interval", let _x = _x.interval in
                                          [`Null]);
                                         ("interval_count",
                                          match _x.interval_count with
                                          | None -> [] | Some _x -> [`Null])]]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_875656e4a1 _x -> `String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_e14347739f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e14347739f ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e14347739f) in _string_to
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", [`Any]);
                       ("billing_thresholds",
                        [`ObjectN [("", [`Null]); ("usage_gte", [`Null])];
                         `String]);
                       ("clear_usage", [`Boolean]); ("deleted", [`Boolean]);
                       ("discounts", [`Array [(`List (`Null))];
                                      `String]);
                       ("id", [`String]);
                       ("metadata", [`ObjectN [("", [`Null])];
                                     `String]);
                       ("price", [`String]);
                       ("price_data",
                        [`ObjectN
                           [("", [`Any]); ("currency", [`String]);
                            ("product", [`String]);
                            ("recurring",
                             [`ObjectN
                                [("", [`Null]); ("interval", [`Null]);
                                 ("interval_count", [`Null])]]);
                            ("tax_behavior", [`String]);
                            ("unit_amount", [`Integer]);
                            ("unit_amount_decimal", [`String])]]);
                       ("quantity", [`Integer]);
                       ("tax_rates", [`Array [(`List (`Null))];
                                      `String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e14347739f ~p ~op ~loc ~style ~explode
    (_x : t_e14347739f) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_7d1d634a95) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("billing_thresholds", match _x.billing_thresholds with
                   | None -> []
                   | Some _x -> [match _x with
                                 | T_1b8cabdd62 _x ->
                                   `ObjectN
                                     [("", [`Null]);
                                      ("usage_gte", let _x = _x.usage_gte in
                                       [`Null])]
                                 | T_1713b7185f _x -> `String]);
                  ("clear_usage", match _x.clear_usage with | None -> []
                   | Some _x -> [`Boolean]);
                  ("deleted", match _x.deleted with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_f85dc498b3 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_f69ed98de2) ->
                                         `Singleton (`Null)) _x))
                                 | T_6763bb7bbe _x -> `String]);
                  ("id", match _x.id with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_9d750c61e7 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_3c592eb40f _x -> `String]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("recurring", let _x = _x.recurring in
                                     [`ObjectN
                                        [("", [`Null]);
                                         ("interval", let _x = _x.interval in
                                          [`Null]);
                                         ("interval_count",
                                          match _x.interval_count with
                                          | None -> [] | Some _x -> [`Null])]]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_875656e4a1 _x -> `String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_e14347739f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_c3d0e8c5c0 ~p ~op ~loc ~style ~explode
    (_x : t_c3d0e8c5c0) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_c3d0e8c5c0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c3d0e8c5c0 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_c3d0e8c5c0) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_c3d0e8c5c0 ~p ~op ~loc ~style ~explode
    (_x : t_c3d0e8c5c0) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_c3d0e8c5c0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_04983d6adf ~p ~op ~loc ~style ~explode
    (_x : t_04983d6adf) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_04983d6adf)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_04983d6adf ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_04983d6adf) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_04983d6adf ~p ~op ~loc ~style ~explode
    (_x : t_04983d6adf) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_04983d6adf)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_2952cd25d0 ~p ~op ~loc ~style ~explode
    (_x : t_2952cd25d0) =
    _string_of ~kind:(
      match _x with
      | T_f8c5c86816 _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_2952cd25d0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2952cd25d0 ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_f8c5c86816) in
        Option.map (fun _y : t_2952cd25d0 -> T_f8c5c86816 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_2952cd25d0 -> Ptime_t _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_2952cd25d0 ~p ~op ~loc ~style ~explode
    (_x : t_2952cd25d0) =
    _namevalues_of ~kind:(
      match _x with
      | T_f8c5c86816 _x -> `String
      | Ptime_t _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_2952cd25d0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f6eb04d2e3 ~p ~op ~loc ~style ~explode
    (_x : t_f6eb04d2e3) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("address", match _x.address with | None -> []
          | Some _x -> [match _x with
                        | T_4e08799e12 _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("city", match _x.city with | None -> []
                              | Some _x -> [`String]);
                             ("country", match _x.country with | None -> []
                              | Some _x -> [`String]);
                             ("line1", match _x.line1 with | None -> []
                              | Some _x -> [`String]);
                             ("line2", match _x.line2 with | None -> []
                              | Some _x -> [`String]);
                             ("postal_code", match _x.postal_code with
                              | None -> [] | Some _x -> [`String]);
                             ("state", match _x.state with | None -> []
                              | Some _x -> [`String])]
                        | T_f10ab35d58 _x -> `String]);
         ("shipping", match _x.shipping with | None -> []
          | Some _x -> [match _x with
                        | T_71440b2752 _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("address", let _x = _x.address in
                              [`ObjectN
                                 [("", [`Any]);
                                  ("city", match _x.city with | None -> []
                                   | Some _x -> [`String]);
                                  ("country", match _x.country with
                                   | None -> [] | Some _x -> [`String]);
                                  ("line1", match _x.line1 with | None -> []
                                   | Some _x -> [`String]);
                                  ("line2", match _x.line2 with | None -> []
                                   | Some _x -> [`String]);
                                  ("postal_code", match _x.postal_code with
                                   | None -> [] | Some _x -> [`String]);
                                  ("state", match _x.state with | None -> []
                                   | Some _x -> [`String])]]);
                             ("name", let _x = _x.name in [`String]);
                             ("phone", match _x.phone with | None -> []
                              | Some _x -> [`String])]
                        | T_c7e2f5d7d1 _x -> `String]);
         ("tax", match _x.tax with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("ip_address", match _x.ip_address with
                            | None -> []
                            | Some _x -> [match _x with
                                          | String_ _x -> `String
                                          | T_211b3860a7 _x -> `String])]]);
         ("tax_exempt", match _x.tax_exempt with | None -> []
          | Some _x -> [`String]);
         ("tax_ids", match _x.tax_ids with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_2b21e7c70c) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("type", let _x = _x.type_ in [`String]);
                                    ("value", let _x = _x.value in [`String])]))
                              _x))])])
      ~ctr:(Json_encoding.construct Encoders'.t_f6eb04d2e3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f6eb04d2e3 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f6eb04d2e3) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]);
                ("address",
                 [`ObjectN
                    [("", [`Any]); ("city", [`String]);
                     ("country", [`String]); ("line1", [`String]);
                     ("line2", [`String]); ("postal_code", [`String]);
                     ("state", [`String])];
                  `String]);
                ("shipping",
                 [`ObjectN
                    [("", [`Any]);
                     ("address",
                      [`ObjectN
                         [("", [`Any]); ("city", [`String]);
                          ("country", [`String]); ("line1", [`String]);
                          ("line2", [`String]); ("postal_code", [`String]);
                          ("state", [`String])]]);
                     ("name", [`String]); ("phone", [`String])];
                  `String]);
                ("tax",
                 [`ObjectN [("", [`Any]); ("ip_address", [`String;
                                                          `String])]]);
                ("tax_exempt", [`String]);
                ("tax_ids",
                 [`Array
                    [(`List
                        (`ObjectN
                           [("", [`Any]); ("type", [`String]);
                            ("value", [`String])]))]])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f6eb04d2e3 ~p ~op ~loc ~style ~explode
    (_x : t_f6eb04d2e3) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("address", match _x.address with | None -> []
          | Some _x -> [match _x with
                        | T_4e08799e12 _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("city", match _x.city with | None -> []
                              | Some _x -> [`String]);
                             ("country", match _x.country with | None -> []
                              | Some _x -> [`String]);
                             ("line1", match _x.line1 with | None -> []
                              | Some _x -> [`String]);
                             ("line2", match _x.line2 with | None -> []
                              | Some _x -> [`String]);
                             ("postal_code", match _x.postal_code with
                              | None -> [] | Some _x -> [`String]);
                             ("state", match _x.state with | None -> []
                              | Some _x -> [`String])]
                        | T_f10ab35d58 _x -> `String]);
         ("shipping", match _x.shipping with | None -> []
          | Some _x -> [match _x with
                        | T_71440b2752 _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("address", let _x = _x.address in
                              [`ObjectN
                                 [("", [`Any]);
                                  ("city", match _x.city with | None -> []
                                   | Some _x -> [`String]);
                                  ("country", match _x.country with
                                   | None -> [] | Some _x -> [`String]);
                                  ("line1", match _x.line1 with | None -> []
                                   | Some _x -> [`String]);
                                  ("line2", match _x.line2 with | None -> []
                                   | Some _x -> [`String]);
                                  ("postal_code", match _x.postal_code with
                                   | None -> [] | Some _x -> [`String]);
                                  ("state", match _x.state with | None -> []
                                   | Some _x -> [`String])]]);
                             ("name", let _x = _x.name in [`String]);
                             ("phone", match _x.phone with | None -> []
                              | Some _x -> [`String])]
                        | T_c7e2f5d7d1 _x -> `String]);
         ("tax", match _x.tax with | None -> []
          | Some _x -> [`ObjectN
                          [("", [`Any]);
                           ("ip_address", match _x.ip_address with
                            | None -> []
                            | Some _x -> [match _x with
                                          | String_ _x -> `String
                                          | T_211b3860a7 _x -> `String])]]);
         ("tax_exempt", match _x.tax_exempt with | None -> []
          | Some _x -> [`String]);
         ("tax_ids", match _x.tax_ids with | None -> []
          | Some _x -> [`Array
                          ((List.map (fun (_x : t_2b21e7c70c) ->
                              `Singleton
                                (`ObjectN
                                   [("", [`Any]);
                                    ("type", let _x = _x.type_ in [`String]);
                                    ("value", let _x = _x.value in [`String])]))
                              _x))])])
      ~ctr:(Json_encoding.construct Encoders'.t_f6eb04d2e3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_81e673c9db ~p ~op ~loc ~style ~explode
    (_x : t_81e673c9db) =
    _string_of ~kind:(
      match _x with
      | T_a52f088048 _x ->
        `Array
          ((List.map (fun (_x : t_b14f4ca400) ->
              `Singleton
                (`ObjectN
                   [("", [`Any]);
                    ("coupon", match _x.coupon with | None -> []
                     | Some _x -> [`String]);
                    ("discount", match _x.discount with | None -> []
                     | Some _x -> [`String]);
                    ("promotion_code", match _x.promotion_code with
                     | None -> [] | Some _x -> [`String])])) _x))
      | T_d89f3a5c69 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_81e673c9db)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_81e673c9db ~loc ~style ~explode (_x : string) =
    [(let kind = `Array
                   [(`List
                       (`ObjectN
                          [("", [`Any]); ("coupon", [`String]);
                           ("discount", [`String]);
                           ("promotion_code", [`String])]))] in
        let dtr = (Json_encoding.destruct Encoders'.t_a52f088048) in
        Option.map (fun _y : t_81e673c9db -> T_a52f088048 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_d89f3a5c69) in
        Option.map (fun _y : t_81e673c9db -> T_d89f3a5c69 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_81e673c9db ~p ~op ~loc ~style ~explode
    (_x : t_81e673c9db) =
    _namevalues_of ~kind:(
      match _x with
      | T_a52f088048 _x ->
        `Array
          ((List.map (fun (_x : t_b14f4ca400) ->
              `Singleton
                (`ObjectN
                   [("", [`Any]);
                    ("coupon", match _x.coupon with | None -> []
                     | Some _x -> [`String]);
                    ("discount", match _x.discount with | None -> []
                     | Some _x -> [`String]);
                    ("promotion_code", match _x.promotion_code with
                     | None -> [] | Some _x -> [`String])])) _x))
      | T_d89f3a5c69 _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_81e673c9db)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e3fa4586a5 ~p ~op ~loc ~style ~explode
    (_x : t_e3fa4586a5) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_317cc693fa) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("currency", match _x.currency with | None -> []
                   | Some _x -> [`String]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("discountable", match _x.discountable with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_b6fbd19489 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_eba1896f54) ->
                                         `Singleton (`Null)) _x))
                                 | T_75e192ab3b _x -> `String]);
                  ("invoiceitem", match _x.invoiceitem with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_d5faec94d6 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_ff74322a57 _x -> `String]);
                  ("period", match _x.period with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("end", let _x = _x.end_ in [`Integer]);
                                    ("start", let _x = _x.start in
                                     [`Integer])]]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_behavior", match _x.tax_behavior with | None -> []
                   | Some _x -> [`String]);
                  ("tax_code", match _x.tax_code with | None -> []
                   | Some _x -> [match _x with
                                 | String_ _x -> `String
                                 | T_a2f16d27e9 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_ed5b045fa6 _x -> `String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_e3fa4586a5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e3fa4586a5 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e3fa4586a5) in _string_to
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", [`Any]); ("amount", [`Integer]);
                       ("currency", [`String]); ("description", [`String]);
                       ("discountable", [`Boolean]);
                       ("discounts", [`Array [(`List (`Null))];
                                      `String]);
                       ("invoiceitem", [`String]);
                       ("metadata", [`ObjectN [("", [`Null])];
                                     `String]);
                       ("period",
                        [`ObjectN
                           [("", [`Any]); ("end", [`Integer]);
                            ("start", [`Integer])]]);
                       ("price", [`String]);
                       ("price_data",
                        [`ObjectN
                           [("", [`Any]); ("currency", [`String]);
                            ("product", [`String]);
                            ("tax_behavior", [`String]);
                            ("unit_amount", [`Integer]);
                            ("unit_amount_decimal", [`String])]]);
                       ("quantity", [`Integer]); ("tax_behavior", [`String]);
                       ("tax_code", [`String;
                                     `String]);
                       ("tax_rates", [`Array [(`List (`Null))];
                                      `String]);
                       ("unit_amount", [`Integer]);
                       ("unit_amount_decimal", [`String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e3fa4586a5 ~p ~op ~loc ~style ~explode
    (_x : t_e3fa4586a5) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_317cc693fa) ->
            `Singleton
              (`ObjectN
                 [("", [`Any]);
                  ("amount", match _x.amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("currency", match _x.currency with | None -> []
                   | Some _x -> [`String]);
                  ("description", match _x.description with | None -> []
                   | Some _x -> [`String]);
                  ("discountable", match _x.discountable with | None -> []
                   | Some _x -> [`Boolean]);
                  ("discounts", match _x.discounts with | None -> []
                   | Some _x -> [match _x with
                                 | T_b6fbd19489 _x ->
                                   `Array
                                     ((List.map (fun (_x : t_eba1896f54) ->
                                         `Singleton (`Null)) _x))
                                 | T_75e192ab3b _x -> `String]);
                  ("invoiceitem", match _x.invoiceitem with | None -> []
                   | Some _x -> [`String]);
                  ("metadata", match _x.metadata with | None -> []
                   | Some _x -> [match _x with
                                 | T_d5faec94d6 _x ->
                                   `ObjectN [("", [`Null])]
                                 | T_ff74322a57 _x -> `String]);
                  ("period", match _x.period with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("end", let _x = _x.end_ in [`Integer]);
                                    ("start", let _x = _x.start in
                                     [`Integer])]]);
                  ("price", match _x.price with | None -> []
                   | Some _x -> [`String]);
                  ("price_data", match _x.price_data with | None -> []
                   | Some _x -> [`ObjectN
                                   [("", [`Any]);
                                    ("currency", let _x = _x.currency in
                                     [`String]);
                                    ("product", let _x = _x.product in
                                     [`String]);
                                    ("tax_behavior",
                                     match _x.tax_behavior with | None -> []
                                     | Some _x -> [`String]);
                                    ("unit_amount", match _x.unit_amount with
                                     | None -> [] | Some _x -> [`Integer]);
                                    ("unit_amount_decimal",
                                     match _x.unit_amount_decimal with
                                     | None -> [] | Some _x -> [`String])]]);
                  ("quantity", match _x.quantity with | None -> []
                   | Some _x -> [`Integer]);
                  ("tax_behavior", match _x.tax_behavior with | None -> []
                   | Some _x -> [`String]);
                  ("tax_code", match _x.tax_code with | None -> []
                   | Some _x -> [match _x with
                                 | String_ _x -> `String
                                 | T_a2f16d27e9 _x -> `String]);
                  ("tax_rates", match _x.tax_rates with | None -> []
                   | Some _x -> [match _x with
                                 | StringList _x ->
                                   `Array
                                     ((List.map (fun (_x : string) ->
                                         `Singleton (`Null)) _x))
                                 | T_ed5b045fa6 _x -> `String]);
                  ("unit_amount", match _x.unit_amount with | None -> []
                   | Some _x -> [`Integer]);
                  ("unit_amount_decimal", match _x.unit_amount_decimal with
                   | None -> [] | Some _x -> [`String])])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_e3fa4586a5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_d7df29a1b1 ~p ~op ~loc ~style ~explode
    (_x : t_d7df29a1b1) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("type", let _x = _x.type_ in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_d7df29a1b1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d7df29a1b1 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_d7df29a1b1) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("account", [`String]); ("type", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_d7df29a1b1 ~p ~op ~loc ~style ~explode
    (_x : t_d7df29a1b1) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("type", let _x = _x.type_ in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_d7df29a1b1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_696a3e9ebe ~p ~op ~loc ~style ~explode
    (_x : t_696a3e9ebe) =
    _string_of ~kind:(
      match _x with
      | T_ad3d0907b2 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_696a3e9ebe)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_696a3e9ebe ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_ad3d0907b2) in
        Option.map (fun _y : t_696a3e9ebe -> T_ad3d0907b2 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_696a3e9ebe -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_696a3e9ebe ~p ~op ~loc ~style ~explode
    (_x : t_696a3e9ebe) =
    _namevalues_of ~kind:(
      match _x with
      | T_ad3d0907b2 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_696a3e9ebe)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_4eafe4d677 ~p ~op ~loc ~style ~explode
    (_x : t_4eafe4d677) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4eafe4d677)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4eafe4d677 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4eafe4d677) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_4eafe4d677 ~p ~op ~loc ~style ~explode
    (_x : t_4eafe4d677) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4eafe4d677)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_c208f0f2be ~p ~op ~loc ~style ~explode
    (_x : t_c208f0f2be) =
    _string_of ~kind:(
      match _x with
      | T_63d10e5660 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_c208f0f2be)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c208f0f2be ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_63d10e5660) in
        Option.map (fun _y : t_c208f0f2be -> T_63d10e5660 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c208f0f2be -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_c208f0f2be ~p ~op ~loc ~style ~explode
    (_x : t_c208f0f2be) =
    _namevalues_of ~kind:(
      match _x with
      | T_63d10e5660 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_c208f0f2be)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_42e95c716a ~p ~op ~loc ~style ~explode
    (_x : t_42e95c716a) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_42e95c716a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_42e95c716a ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_42e95c716a) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_42e95c716a ~p ~op ~loc ~style ~explode
    (_x : t_42e95c716a) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_42e95c716a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_04f985ad81 ~p ~op ~loc ~style ~explode
    (_x : t_04f985ad81) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_04f985ad81)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_04f985ad81 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_04f985ad81) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_04f985ad81 ~p ~op ~loc ~style ~explode
    (_x : t_04f985ad81) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_04f985ad81)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e67ab51d5e ~p ~op ~loc ~style ~explode
    (_x : t_e67ab51d5e) =
    _string_of ~kind:(
      match _x with
      | T_0f859376f3 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_e67ab51d5e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e67ab51d5e ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_0f859376f3) in
        Option.map (fun _y : t_e67ab51d5e -> T_0f859376f3 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e67ab51d5e -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_e67ab51d5e ~p ~op ~loc ~style ~explode
    (_x : t_e67ab51d5e) =
    _namevalues_of ~kind:(
      match _x with
      | T_0f859376f3 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_e67ab51d5e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_5cacf53388 ~p ~op ~loc ~style ~explode
    (_x : t_5cacf53388) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_5cacf53388)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_5cacf53388 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_5cacf53388) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_5cacf53388 ~p ~op ~loc ~style ~explode
    (_x : t_5cacf53388) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_5cacf53388)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_b413e28022 ~p ~op ~loc ~style ~explode
    (_x : t_b413e28022) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b413e28022)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b413e28022 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b413e28022) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b413e28022 ~p ~op ~loc ~style ~explode
    (_x : t_b413e28022) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b413e28022)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_1179a06704 ~p ~op ~loc ~style ~explode
    (_x : t_1179a06704) =
    _string_of ~kind:(
      match _x with
      | T_5e5642e567 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_1179a06704)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1179a06704 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_5e5642e567) in
        Option.map (fun _y : t_1179a06704 -> T_5e5642e567 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_1179a06704 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_1179a06704 ~p ~op ~loc ~style ~explode
    (_x : t_1179a06704) =
    _namevalues_of ~kind:(
      match _x with
      | T_5e5642e567 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_1179a06704)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_1da3cd165a ~p ~op ~loc ~style ~explode
    (_x : t_1da3cd165a) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_1da3cd165a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1da3cd165a ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_1da3cd165a) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_1da3cd165a ~p ~op ~loc ~style ~explode
    (_x : t_1da3cd165a) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_1da3cd165a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_1a2156a726 ~p ~op ~loc ~style ~explode
    (_x : t_1a2156a726) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("is_default", match _x.is_default with | None -> []
          | Some _x -> [`Boolean]);
         ("is_platform_default", match _x.is_platform_default with
          | None -> [] | Some _x -> [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_1a2156a726)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1a2156a726 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_1a2156a726) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("is_default", [`Boolean]);
                ("is_platform_default", [`Boolean])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_1a2156a726 ~p ~op ~loc ~style ~explode
    (_x : t_1a2156a726) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("is_default", match _x.is_default with | None -> []
          | Some _x -> [`Boolean]);
         ("is_platform_default", match _x.is_platform_default with
          | None -> [] | Some _x -> [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_1a2156a726)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e7cf5da208 ~p ~op ~loc ~style ~explode
    (_x : t_e7cf5da208) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e7cf5da208)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e7cf5da208 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e7cf5da208) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e7cf5da208 ~p ~op ~loc ~style ~explode
    (_x : t_e7cf5da208) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e7cf5da208)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_8fef9d57c9 ~p ~op ~loc ~style ~explode
    (_x : t_8fef9d57c9) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8fef9d57c9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8fef9d57c9 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_8fef9d57c9) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_8fef9d57c9 ~p ~op ~loc ~style ~explode
    (_x : t_8fef9d57c9) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8fef9d57c9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_849ed17da9 ~p ~op ~loc ~style ~explode
    (_x : t_849ed17da9) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_849ed17da9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_849ed17da9 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_849ed17da9) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_849ed17da9 ~p ~op ~loc ~style ~explode
    (_x : t_849ed17da9) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_849ed17da9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_16deab6c5d ~p ~op ~loc ~style ~explode
    (_x : t_16deab6c5d) =
    _string_of ~kind:(
      match _x with
      | T_679fee6239 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_16deab6c5d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_16deab6c5d ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_679fee6239) in
        Option.map (fun _y : t_16deab6c5d -> T_679fee6239 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_16deab6c5d -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_16deab6c5d ~p ~op ~loc ~style ~explode
    (_x : t_16deab6c5d) =
    _namevalues_of ~kind:(
      match _x with
      | T_679fee6239 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_16deab6c5d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_11e340ed02 ~p ~op ~loc ~style ~explode
    (_x : t_11e340ed02) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_11e340ed02)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_11e340ed02 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_11e340ed02) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_11e340ed02 ~p ~op ~loc ~style ~explode
    (_x : t_11e340ed02) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_11e340ed02)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_0e1a6c56e3 ~p ~op ~loc ~style ~explode
    (_x : t_0e1a6c56e3) =
    _string_of ~kind:(
      match _x with
      | T_9010f53dfa _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_0e1a6c56e3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0e1a6c56e3 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_9010f53dfa) in
        Option.map (fun _y : t_0e1a6c56e3 -> T_9010f53dfa _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_0e1a6c56e3 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_0e1a6c56e3 ~p ~op ~loc ~style ~explode
    (_x : t_0e1a6c56e3) =
    _namevalues_of ~kind:(
      match _x with
      | T_9010f53dfa _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_0e1a6c56e3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_a6d8c1a3b9 ~p ~op ~loc ~style ~explode
    (_x : t_a6d8c1a3b9) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a6d8c1a3b9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a6d8c1a3b9 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_a6d8c1a3b9) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_a6d8c1a3b9 ~p ~op ~loc ~style ~explode
    (_x : t_a6d8c1a3b9) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a6d8c1a3b9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_7916492c96 ~p ~op ~loc ~style ~explode
    (_x : t_7916492c96) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("customer", match _x.customer with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_7916492c96)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_7916492c96 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_7916492c96) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("account", [`String]);
                ("customer", [`String])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_7916492c96 ~p ~op ~loc ~style ~explode
    (_x : t_7916492c96) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("customer", match _x.customer with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_7916492c96)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_ccfa0625d1 ~p ~op ~loc ~style ~explode
    (_x : t_ccfa0625d1) =
    _string_of ~kind:(
      match _x with
      | T_77b9426cb6 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_ccfa0625d1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ccfa0625d1 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_77b9426cb6) in
        Option.map (fun _y : t_ccfa0625d1 -> T_77b9426cb6 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ccfa0625d1 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_ccfa0625d1 ~p ~op ~loc ~style ~explode
    (_x : t_ccfa0625d1) =
    _namevalues_of ~kind:(
      match _x with
      | T_77b9426cb6 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_ccfa0625d1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_6fdb9283b0 ~p ~op ~loc ~style ~explode
    (_x : t_6fdb9283b0) =
    _string_of ~kind:(
      match _x with
      | String_ _x -> `String
      | T_d0cc26608d _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6fdb9283b0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6fdb9283b0 ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_6fdb9283b0 -> String_ _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_d0cc26608d) in
        Option.map (fun _y : t_6fdb9283b0 -> T_d0cc26608d _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_6fdb9283b0 ~p ~op ~loc ~style ~explode
    (_x : t_6fdb9283b0) =
    _namevalues_of ~kind:(
      match _x with
      | String_ _x -> `String
      | T_d0cc26608d _x -> `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6fdb9283b0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_cecc48b8cb ~p ~op ~loc ~style ~explode
    (_x : t_cecc48b8cb) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_cecc48b8cb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_cecc48b8cb ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_cecc48b8cb) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_cecc48b8cb ~p ~op ~loc ~style ~explode
    (_x : t_cecc48b8cb) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_cecc48b8cb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_bd57aeef7a ~p ~op ~loc ~style ~explode
    (_x : t_bd57aeef7a) =
    _string_of ~kind:(
      match _x with
      | T_12ebff1666 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_bd57aeef7a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_bd57aeef7a ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_12ebff1666) in
        Option.map (fun _y : t_bd57aeef7a -> T_12ebff1666 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_bd57aeef7a -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_bd57aeef7a ~p ~op ~loc ~style ~explode
    (_x : t_bd57aeef7a) =
    _namevalues_of ~kind:(
      match _x with
      | T_12ebff1666 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_bd57aeef7a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_d3ce6eb417 ~p ~op ~loc ~style ~explode
    (_x : t_d3ce6eb417) =
    _string_of ~kind:(
      match _x with
      | T_43eb7a27c0 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d3ce6eb417)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d3ce6eb417 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_43eb7a27c0) in
        Option.map (fun _y : t_d3ce6eb417 -> T_43eb7a27c0 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d3ce6eb417 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_d3ce6eb417 ~p ~op ~loc ~style ~explode
    (_x : t_d3ce6eb417) =
    _namevalues_of ~kind:(
      match _x with
      | T_43eb7a27c0 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d3ce6eb417)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_d4fbe66228 ~p ~op ~loc ~style ~explode
    (_x : t_d4fbe66228) =
    _string_of ~kind:(
      match _x with
      | T_ddf2c3a961 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d4fbe66228)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d4fbe66228 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_ddf2c3a961) in
        Option.map (fun _y : t_d4fbe66228 -> T_ddf2c3a961 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d4fbe66228 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_d4fbe66228 ~p ~op ~loc ~style ~explode
    (_x : t_d4fbe66228) =
    _namevalues_of ~kind:(
      match _x with
      | T_ddf2c3a961 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_d4fbe66228)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_6bdfe70bb0 ~p ~op ~loc ~style ~explode
    (_x : t_6bdfe70bb0) =
    _string_of ~kind:(
      match _x with
      | T_5041768af0 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_6bdfe70bb0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6bdfe70bb0 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_5041768af0) in
        Option.map (fun _y : t_6bdfe70bb0 -> T_5041768af0 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_6bdfe70bb0 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_6bdfe70bb0 ~p ~op ~loc ~style ~explode
    (_x : t_6bdfe70bb0) =
    _namevalues_of ~kind:(
      match _x with
      | T_5041768af0 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_6bdfe70bb0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_2774ea4fcd ~p ~op ~loc ~style ~explode
    (_x : t_2774ea4fcd) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2774ea4fcd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2774ea4fcd ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_2774ea4fcd) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_2774ea4fcd ~p ~op ~loc ~style ~explode
    (_x : t_2774ea4fcd) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2774ea4fcd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_0aad2a8fdd ~p ~op ~loc ~style ~explode
    (_x : t_0aad2a8fdd) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("interval", match _x.interval with | None -> []
          | Some _x -> [`String]);
         ("meter", match _x.meter with | None -> [] | Some _x -> [`String]);
         ("usage_type", match _x.usage_type with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_0aad2a8fdd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0aad2a8fdd ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_0aad2a8fdd) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("interval", [`String]); ("meter", [`String]);
                ("usage_type", [`String])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_0aad2a8fdd ~p ~op ~loc ~style ~explode
    (_x : t_0aad2a8fdd) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("interval", match _x.interval with | None -> []
          | Some _x -> [`String]);
         ("meter", match _x.meter with | None -> [] | Some _x -> [`String]);
         ("usage_type", match _x.usage_type with | None -> []
          | Some _x -> [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_0aad2a8fdd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_08493db080 ~p ~op ~loc ~style ~explode
    (_x : t_08493db080) =
    _string_of ~kind:(
      match _x with
      | T_770e6696c9 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_08493db080)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_08493db080 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_770e6696c9) in
        Option.map (fun _y : t_08493db080 -> T_770e6696c9 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_08493db080 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_08493db080 ~p ~op ~loc ~style ~explode
    (_x : t_08493db080) =
    _namevalues_of ~kind:(
      match _x with
      | T_770e6696c9 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_08493db080)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_4c5ec3b7e2 ~p ~op ~loc ~style ~explode
    (_x : t_4c5ec3b7e2) =
    _string_of ~kind:(
      match _x with
      | T_e879f1315e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_4c5ec3b7e2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4c5ec3b7e2 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_e879f1315e) in
        Option.map (fun _y : t_4c5ec3b7e2 -> T_e879f1315e _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_4c5ec3b7e2 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_4c5ec3b7e2 ~p ~op ~loc ~style ~explode
    (_x : t_4c5ec3b7e2) =
    _namevalues_of ~kind:(
      match _x with
      | T_e879f1315e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_4c5ec3b7e2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_2bdd288b56 ~p ~op ~loc ~style ~explode
    (_x : t_2bdd288b56) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2bdd288b56)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2bdd288b56 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_2bdd288b56) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_2bdd288b56 ~p ~op ~loc ~style ~explode
    (_x : t_2bdd288b56) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2bdd288b56)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_33d7eada29 ~p ~op ~loc ~style ~explode
    (_x : t_33d7eada29) =
    _string_of ~kind:(
      match _x with
      | T_117ccb3cf4 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_33d7eada29)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_33d7eada29 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_117ccb3cf4) in
        Option.map (fun _y : t_33d7eada29 -> T_117ccb3cf4 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_33d7eada29 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_33d7eada29 ~p ~op ~loc ~style ~explode
    (_x : t_33d7eada29) =
    _namevalues_of ~kind:(
      match _x with
      | T_117ccb3cf4 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_33d7eada29)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_9ab8527734 ~p ~op ~loc ~style ~explode
    (_x : t_9ab8527734) =
    _string_of ~kind:(
      match _x with
      | T_f4df2ca6e3 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9ab8527734)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9ab8527734 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_f4df2ca6e3) in
        Option.map (fun _y : t_9ab8527734 -> T_f4df2ca6e3 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9ab8527734 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_9ab8527734 ~p ~op ~loc ~style ~explode
    (_x : t_9ab8527734) =
    _namevalues_of ~kind:(
      match _x with
      | T_f4df2ca6e3 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9ab8527734)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_485be60dff ~p ~op ~loc ~style ~explode
    (_x : t_485be60dff) =
    _string_of ~kind:(
      match _x with
      | T_4ae3a56714 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_485be60dff)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_485be60dff ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_4ae3a56714) in
        Option.map (fun _y : t_485be60dff -> T_4ae3a56714 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_485be60dff -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_485be60dff ~p ~op ~loc ~style ~explode
    (_x : t_485be60dff) =
    _namevalues_of ~kind:(
      match _x with
      | T_4ae3a56714 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_485be60dff)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_94f3045432 ~p ~op ~loc ~style ~explode
    (_x : t_94f3045432) =
    _string_of ~kind:(
      match _x with
      | T_4bda2929bc _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_94f3045432)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_94f3045432 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_4bda2929bc) in
        Option.map (fun _y : t_94f3045432 -> T_4bda2929bc _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_94f3045432 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_94f3045432 ~p ~op ~loc ~style ~explode
    (_x : t_94f3045432) =
    _namevalues_of ~kind:(
      match _x with
      | T_4bda2929bc _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_94f3045432)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_bfeba0d067 ~p ~op ~loc ~style ~explode
    (_x : t_bfeba0d067) =
    _string_of ~kind:(
      match _x with
      | T_d0739df243 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_bfeba0d067)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_bfeba0d067 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_d0739df243) in
        Option.map (fun _y : t_bfeba0d067 -> T_d0739df243 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_bfeba0d067 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_bfeba0d067 ~p ~op ~loc ~style ~explode
    (_x : t_bfeba0d067) =
    _namevalues_of ~kind:(
      match _x with
      | T_d0739df243 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_bfeba0d067)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_977617a7a1 ~p ~op ~loc ~style ~explode
    (_x : t_977617a7a1) =
    _string_of ~kind:(
      match _x with
      | T_c4a4990e10 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_977617a7a1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_977617a7a1 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_c4a4990e10) in
        Option.map (fun _y : t_977617a7a1 -> T_c4a4990e10 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_977617a7a1 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_977617a7a1 ~p ~op ~loc ~style ~explode
    (_x : t_977617a7a1) =
    _namevalues_of ~kind:(
      match _x with
      | T_c4a4990e10 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_977617a7a1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_55fe964136 ~p ~op ~loc ~style ~explode
    (_x : t_55fe964136) =
    _string_of ~kind:(
      match _x with
      | T_76bd2f1645 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_55fe964136)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_55fe964136 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_76bd2f1645) in
        Option.map (fun _y : t_55fe964136 -> T_76bd2f1645 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_55fe964136 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_55fe964136 ~p ~op ~loc ~style ~explode
    (_x : t_55fe964136) =
    _namevalues_of ~kind:(
      match _x with
      | T_76bd2f1645 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_55fe964136)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_a26042e7d2 ~p ~op ~loc ~style ~explode
    (_x : t_a26042e7d2) =
    _string_of ~kind:(
      match _x with
      | T_047bcb5a00 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_a26042e7d2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a26042e7d2 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_047bcb5a00) in
        Option.map (fun _y : t_a26042e7d2 -> T_047bcb5a00 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_a26042e7d2 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_a26042e7d2 ~p ~op ~loc ~style ~explode
    (_x : t_a26042e7d2) =
    _namevalues_of ~kind:(
      match _x with
      | T_047bcb5a00 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_a26042e7d2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_098e02745c ~p ~op ~loc ~style ~explode
    (_x : t_098e02745c) =
    _string_of ~kind:(
      match _x with
      | T_3a3e536ba6 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_098e02745c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_098e02745c ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_3a3e536ba6) in
        Option.map (fun _y : t_098e02745c -> T_3a3e536ba6 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_098e02745c -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_098e02745c ~p ~op ~loc ~style ~explode
    (_x : t_098e02745c) =
    _namevalues_of ~kind:(
      match _x with
      | T_3a3e536ba6 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_098e02745c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_b40f11dd64 ~p ~op ~loc ~style ~explode
    (_x : t_b40f11dd64) =
    _string_of ~kind:(
      match _x with
      | T_e6fbfafc9e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_b40f11dd64)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b40f11dd64 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_e6fbfafc9e) in
        Option.map (fun _y : t_b40f11dd64 -> T_e6fbfafc9e _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_b40f11dd64 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_b40f11dd64 ~p ~op ~loc ~style ~explode
    (_x : t_b40f11dd64) =
    _namevalues_of ~kind:(
      match _x with
      | T_e6fbfafc9e _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_b40f11dd64)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_9e8c6bcf30 ~p ~op ~loc ~style ~explode
    (_x : t_9e8c6bcf30) =
    _string_of ~kind:(
      match _x with
      | T_4e99cdf5bf _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9e8c6bcf30)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9e8c6bcf30 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_4e99cdf5bf) in
        Option.map (fun _y : t_9e8c6bcf30 -> T_4e99cdf5bf _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9e8c6bcf30 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_9e8c6bcf30 ~p ~op ~loc ~style ~explode
    (_x : t_9e8c6bcf30) =
    _namevalues_of ~kind:(
      match _x with
      | T_4e99cdf5bf _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9e8c6bcf30)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_a1fe692430 ~p ~op ~loc ~style ~explode
    (_x : t_a1fe692430) =
    _string_of ~kind:(
      match _x with
      | T_89d68ef140 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_a1fe692430)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a1fe692430 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_89d68ef140) in
        Option.map (fun _y : t_a1fe692430 -> T_89d68ef140 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_a1fe692430 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_a1fe692430 ~p ~op ~loc ~style ~explode
    (_x : t_a1fe692430) =
    _namevalues_of ~kind:(
      match _x with
      | T_89d68ef140 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_a1fe692430)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_9aa6380ae2 ~p ~op ~loc ~style ~explode
    (_x : t_9aa6380ae2) =
    _string_of ~kind:(
      match _x with
      | T_6dfe2361cf _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9aa6380ae2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9aa6380ae2 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_6dfe2361cf) in
        Option.map (fun _y : t_9aa6380ae2 -> T_6dfe2361cf _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9aa6380ae2 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_9aa6380ae2 ~p ~op ~loc ~style ~explode
    (_x : t_9aa6380ae2) =
    _namevalues_of ~kind:(
      match _x with
      | T_6dfe2361cf _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9aa6380ae2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_0a6220e696 ~p ~op ~loc ~style ~explode
    (_x : t_0a6220e696) =
    _string_of ~kind:(
      `ObjectN [("", [`Any]); ("enabled", let _x = _x.enabled in [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_0a6220e696)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0a6220e696 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_0a6220e696) in _string_to
      ~kind:(`ObjectN [("", [`Any]); ("enabled", [`Boolean])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_0a6220e696 ~p ~op ~loc ~style ~explode
    (_x : t_0a6220e696) =
    _namevalues_of ~kind:(
      `ObjectN [("", [`Any]); ("enabled", let _x = _x.enabled in [`Boolean])])
      ~ctr:(Json_encoding.construct Encoders'.t_0a6220e696)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f32678b2ef ~p ~op ~loc ~style ~explode
    (_x : t_f32678b2ef) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f32678b2ef)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f32678b2ef ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f32678b2ef) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f32678b2ef ~p ~op ~loc ~style ~explode
    (_x : t_f32678b2ef) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f32678b2ef)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_21215fdec6 ~p ~op ~loc ~style ~explode
    (_x : t_21215fdec6) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_21215fdec6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_21215fdec6 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_21215fdec6) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_21215fdec6 ~p ~op ~loc ~style ~explode
    (_x : t_21215fdec6) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_21215fdec6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_9e4c050846 ~p ~op ~loc ~style ~explode
    (_x : t_9e4c050846) =
    _string_of ~kind:(
      match _x with
      | T_99933d97fe _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9e4c050846)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9e4c050846 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_99933d97fe) in
        Option.map (fun _y : t_9e4c050846 -> T_99933d97fe _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9e4c050846 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_9e4c050846 ~p ~op ~loc ~style ~explode
    (_x : t_9e4c050846) =
    _namevalues_of ~kind:(
      match _x with
      | T_99933d97fe _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_9e4c050846)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_ab910df305 ~p ~op ~loc ~style ~explode
    (_x : t_ab910df305) =
    _string_of ~kind:(
      match _x with
      | T_52a47d24e3 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_ab910df305)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ab910df305 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_52a47d24e3) in
        Option.map (fun _y : t_ab910df305 -> T_52a47d24e3 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ab910df305 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_ab910df305 ~p ~op ~loc ~style ~explode
    (_x : t_ab910df305) =
    _namevalues_of ~kind:(
      match _x with
      | T_52a47d24e3 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_ab910df305)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_81e3b88aad ~p ~op ~loc ~style ~explode
    (_x : t_81e3b88aad) =
    _string_of ~kind:(
      match _x with
      | T_c423040d1d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_81e3b88aad)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_81e3b88aad ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_c423040d1d) in
        Option.map (fun _y : t_81e3b88aad -> T_c423040d1d _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_81e3b88aad -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_81e3b88aad ~p ~op ~loc ~style ~explode
    (_x : t_81e3b88aad) =
    _namevalues_of ~kind:(
      match _x with
      | T_c423040d1d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_81e3b88aad)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_8df96af889 ~p ~op ~loc ~style ~explode
    (_x : t_8df96af889) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8df96af889)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8df96af889 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_8df96af889) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_8df96af889 ~p ~op ~loc ~style ~explode
    (_x : t_8df96af889) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8df96af889)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f47c871206 ~p ~op ~loc ~style ~explode
    (_x : t_f47c871206) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("customer", match _x.customer with | None -> []
          | Some _x -> [`String]);
         ("type", let _x = _x.type_ in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_f47c871206)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f47c871206 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f47c871206) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]); ("account", [`String]);
                ("customer", [`String]); ("type", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f47c871206 ~p ~op ~loc ~style ~explode
    (_x : t_f47c871206) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("account", match _x.account with | None -> []
          | Some _x -> [`String]);
         ("customer", match _x.customer with | None -> []
          | Some _x -> [`String]);
         ("type", let _x = _x.type_ in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_f47c871206)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_08f3f13308 ~p ~op ~loc ~style ~explode
    (_x : t_08f3f13308) =
    _string_of ~kind:(
      match _x with
      | T_0199a192ee _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_08f3f13308)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_08f3f13308 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_0199a192ee) in
        Option.map (fun _y : t_08f3f13308 -> T_0199a192ee _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_08f3f13308 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_08f3f13308 ~p ~op ~loc ~style ~explode
    (_x : t_08f3f13308) =
    _namevalues_of ~kind:(
      match _x with
      | T_0199a192ee _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_08f3f13308)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_b35c50c323 ~p ~op ~loc ~style ~explode
    (_x : t_b35c50c323) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b35c50c323)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b35c50c323 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b35c50c323) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b35c50c323 ~p ~op ~loc ~style ~explode
    (_x : t_b35c50c323) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b35c50c323)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_cdedbe876a ~p ~op ~loc ~style ~explode
    (_x : t_cdedbe876a) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_cdedbe876a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_cdedbe876a ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_cdedbe876a) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_cdedbe876a ~p ~op ~loc ~style ~explode
    (_x : t_cdedbe876a) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_cdedbe876a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_c4b1ad0571 ~p ~op ~loc ~style ~explode
    (_x : t_c4b1ad0571) =
    _string_of ~kind:(
      match _x with
      | T_ac8c38ff95 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_c4b1ad0571)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c4b1ad0571 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_ac8c38ff95) in
        Option.map (fun _y : t_c4b1ad0571 -> T_ac8c38ff95 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c4b1ad0571 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_c4b1ad0571 ~p ~op ~loc ~style ~explode
    (_x : t_c4b1ad0571) =
    _namevalues_of ~kind:(
      match _x with
      | T_ac8c38ff95 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_c4b1ad0571)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_0ffc3b1d59 ~p ~op ~loc ~style ~explode
    (_x : t_0ffc3b1d59) =
    _string_of ~kind:(
      match _x with
      | T_0764eaa89a _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_0ffc3b1d59)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0ffc3b1d59 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_0764eaa89a) in
        Option.map (fun _y : t_0ffc3b1d59 -> T_0764eaa89a _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_0ffc3b1d59 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_0ffc3b1d59 ~p ~op ~loc ~style ~explode
    (_x : t_0ffc3b1d59) =
    _namevalues_of ~kind:(
      match _x with
      | T_0764eaa89a _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_0ffc3b1d59)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_09df6faafb ~p ~op ~loc ~style ~explode
    (_x : t_09df6faafb) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_09df6faafb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_09df6faafb ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_09df6faafb) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_09df6faafb ~p ~op ~loc ~style ~explode
    (_x : t_09df6faafb) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_09df6faafb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_48bbea3497 ~p ~op ~loc ~style ~explode
    (_x : t_48bbea3497) =
    _string_of ~kind:(
      match _x with
      | T_be953a7826 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_48bbea3497)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_48bbea3497 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_be953a7826) in
        Option.map (fun _y : t_48bbea3497 -> T_be953a7826 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_48bbea3497 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_48bbea3497 ~p ~op ~loc ~style ~explode
    (_x : t_48bbea3497) =
    _namevalues_of ~kind:(
      match _x with
      | T_be953a7826 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_48bbea3497)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_bc1bb0e62b ~p ~op ~loc ~style ~explode
    (_x : t_bc1bb0e62b) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_bc1bb0e62b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_bc1bb0e62b ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_bc1bb0e62b) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_bc1bb0e62b ~p ~op ~loc ~style ~explode
    (_x : t_bc1bb0e62b) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_bc1bb0e62b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_8c672cee18 ~p ~op ~loc ~style ~explode
    (_x : t_8c672cee18) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8c672cee18)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8c672cee18 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_8c672cee18) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_8c672cee18 ~p ~op ~loc ~style ~explode
    (_x : t_8c672cee18) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8c672cee18)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_8d4454e1e7 ~p ~op ~loc ~style ~explode
    (_x : t_8d4454e1e7) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8d4454e1e7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8d4454e1e7 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_8d4454e1e7) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_8d4454e1e7 ~p ~op ~loc ~style ~explode
    (_x : t_8d4454e1e7) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_8d4454e1e7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_3d6f926201 ~p ~op ~loc ~style ~explode
    (_x : t_3d6f926201) =
    _string_of ~kind:(
      match _x with
      | T_b43f2b203b _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_3d6f926201)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_3d6f926201 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_b43f2b203b) in
        Option.map (fun _y : t_3d6f926201 -> T_b43f2b203b _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_3d6f926201 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_3d6f926201 ~p ~op ~loc ~style ~explode
    (_x : t_3d6f926201) =
    _namevalues_of ~kind:(
      match _x with
      | T_b43f2b203b _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_3d6f926201)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f9dbcc1790 ~p ~op ~loc ~style ~explode
    (_x : t_f9dbcc1790) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f9dbcc1790)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f9dbcc1790 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f9dbcc1790) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f9dbcc1790 ~p ~op ~loc ~style ~explode
    (_x : t_f9dbcc1790) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f9dbcc1790)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_940f841aad ~p ~op ~loc ~style ~explode
    (_x : t_940f841aad) =
    _string_of ~kind:(
      match _x with
      | T_2922b72a55 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_940f841aad)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_940f841aad ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_2922b72a55) in
        Option.map (fun _y : t_940f841aad -> T_2922b72a55 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_940f841aad -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_940f841aad ~p ~op ~loc ~style ~explode
    (_x : t_940f841aad) =
    _namevalues_of ~kind:(
      match _x with
      | T_2922b72a55 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_940f841aad)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_05ed840b39 ~p ~op ~loc ~style ~explode
    (_x : t_05ed840b39) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_05ed840b39)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_05ed840b39 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_05ed840b39) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_05ed840b39 ~p ~op ~loc ~style ~explode
    (_x : t_05ed840b39) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_05ed840b39)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_57ebe3c863 ~p ~op ~loc ~style ~explode
    (_x : t_57ebe3c863) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_57ebe3c863)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_57ebe3c863 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_57ebe3c863) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_57ebe3c863 ~p ~op ~loc ~style ~explode
    (_x : t_57ebe3c863) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_57ebe3c863)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_d7899c5733 ~p ~op ~loc ~style ~explode
    (_x : t_d7899c5733) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("source_flow_type", let _x = _x.source_flow_type in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_d7899c5733)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d7899c5733 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_d7899c5733) in _string_to
      ~kind:(`ObjectN [("", [`Any]); ("source_flow_type", [`String])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_d7899c5733 ~p ~op ~loc ~style ~explode
    (_x : t_d7899c5733) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("source_flow_type", let _x = _x.source_flow_type in [`String])])
      ~ctr:(Json_encoding.construct Encoders'.t_d7899c5733)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e5e83a4d6b ~p ~op ~loc ~style ~explode
    (_x : t_e5e83a4d6b) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e5e83a4d6b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e5e83a4d6b ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e5e83a4d6b) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e5e83a4d6b ~p ~op ~loc ~style ~explode
    (_x : t_e5e83a4d6b) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e5e83a4d6b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_50ce7697c2 ~p ~op ~loc ~style ~explode
    (_x : t_50ce7697c2) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_50ce7697c2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_50ce7697c2 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_50ce7697c2) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_50ce7697c2 ~p ~op ~loc ~style ~explode
    (_x : t_50ce7697c2) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_50ce7697c2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_4c0b4f6fc6 ~p ~op ~loc ~style ~explode
    (_x : t_4c0b4f6fc6) =
    _string_of ~kind:(
      match _x with
      | T_d3ee36a85d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_4c0b4f6fc6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4c0b4f6fc6 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_d3ee36a85d) in
        Option.map (fun _y : t_4c0b4f6fc6 -> T_d3ee36a85d _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_4c0b4f6fc6 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_4c0b4f6fc6 ~p ~op ~loc ~style ~explode
    (_x : t_4c0b4f6fc6) =
    _namevalues_of ~kind:(
      match _x with
      | T_d3ee36a85d _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_4c0b4f6fc6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_4316f9bdd6 ~p ~op ~loc ~style ~explode
    (_x : t_4316f9bdd6) =
    _string_of ~kind:(
      match _x with
      | T_d189722c39 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_4316f9bdd6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4316f9bdd6 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_d189722c39) in
        Option.map (fun _y : t_4316f9bdd6 -> T_d189722c39 _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_4316f9bdd6 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_4316f9bdd6 ~p ~op ~loc ~style ~explode
    (_x : t_4316f9bdd6) =
    _namevalues_of ~kind:(
      match _x with
      | T_d189722c39 _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_4316f9bdd6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_bc07bb4360 ~p ~op ~loc ~style ~explode
    (_x : t_bc07bb4360) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_bc07bb4360)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_bc07bb4360 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_bc07bb4360) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_bc07bb4360 ~p ~op ~loc ~style ~explode
    (_x : t_bc07bb4360) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_bc07bb4360)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_e0080bc683 ~p ~op ~loc ~style ~explode
    (_x : t_e0080bc683) =
    _string_of ~kind:(
      match _x with
      | T_b64b437f2c _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_e0080bc683)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e0080bc683 ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                    ("lt", [`Integer]); ("lte", [`Integer])] in
        let dtr = (Json_encoding.destruct Encoders'.t_b64b437f2c) in
        Option.map (fun _y : t_e0080bc683 -> T_b64b437f2c _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e0080bc683 -> Int _y)
          (_string_to ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_opt Option.is_some
  
  let namevalues_of_t_e0080bc683 ~p ~op ~loc ~style ~explode
    (_x : t_e0080bc683) =
    _namevalues_of ~kind:(
      match _x with
      | T_b64b437f2c _x ->
        `ObjectN
          [("", [`Any]);
           ("gt", match _x.gt with | None -> [] | Some _x -> [`Integer]);
           ("gte", match _x.gte with | None -> [] | Some _x -> [`Integer]);
           ("lt", match _x.lt with | None -> [] | Some _x -> [`Integer]);
           ("lte", match _x.lte with | None -> [] | Some _x -> [`Integer])]
      | Int _x -> `Integer)
      ~ctr:(Json_encoding.construct Encoders'.t_e0080bc683)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_04484736af ~p ~op ~loc ~style ~explode
    (_x : t_04484736af) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_04484736af)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_04484736af ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_04484736af) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_04484736af ~p ~op ~loc ~style ~explode
    (_x : t_04484736af) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_04484736af)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_6695658151 ~p ~op ~loc ~style ~explode
    (_x : t_6695658151) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6695658151)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6695658151 ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_6695658151) in _string_to
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_6695658151 ~p ~op ~loc ~style ~explode
    (_x : t_6695658151) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6695658151)
      ~p ~op ~loc ~style ~explode _x
  
  let string_of_t_f88b9725ad ~p ~op ~loc ~style ~explode
    (_x : t_f88b9725ad) =
    _string_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("posted_at", match _x.posted_at with | None -> []
          | Some _x -> [match _x with
                        | T_50de03f409 _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("gt", match _x.gt with | None -> []
                              | Some _x -> [`Integer]);
                             ("gte", match _x.gte with | None -> []
                              | Some _x -> [`Integer]);
                             ("lt", match _x.lt with | None -> []
                              | Some _x -> [`Integer]);
                             ("lte", match _x.lte with | None -> []
                              | Some _x -> [`Integer])]
                        | Int _x -> `Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_f88b9725ad)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f88b9725ad ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f88b9725ad) in _string_to
      ~kind:(`ObjectN
               [("", [`Any]);
                ("posted_at",
                 [`ObjectN
                    [("", [`Any]); ("gt", [`Integer]); ("gte", [`Integer]);
                     ("lt", [`Integer]); ("lte", [`Integer])];
                  `Integer])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f88b9725ad ~p ~op ~loc ~style ~explode
    (_x : t_f88b9725ad) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", [`Any]);
         ("posted_at", match _x.posted_at with | None -> []
          | Some _x -> [match _x with
                        | T_50de03f409 _x ->
                          `ObjectN
                            [("", [`Any]);
                             ("gt", match _x.gt with | None -> []
                              | Some _x -> [`Integer]);
                             ("gte", match _x.gte with | None -> []
                              | Some _x -> [`Integer]);
                             ("lt", match _x.lt with | None -> []
                              | Some _x -> [`Integer]);
                             ("lte", match _x.lte with | None -> []
                              | Some _x -> [`Integer])]
                        | Int _x -> `Integer])])
      ~ctr:(Json_encoding.construct Encoders'.t_f88b9725ad)
      ~p ~op ~loc ~style ~explode _x
  
  
end