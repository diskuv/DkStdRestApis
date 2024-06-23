(** {1 Parameters} *)

module ParamSerDe' = struct

  open StripeTypes
  open StripeEncdrs
  
  let _loosen_spec_enable_deepobject_array = true
  exception Invalid_parameter of string
  
  let _jfield_to_s kind ~n = match kind with
    | `ObjectN okinds -> begin
      match List.assoc_opt n okinds with
      | Some okind -> EncBase'.json_to_string okind
      | _ -> EncBase'.json_to_string (match List.assoc_opt "" okinds with
        | Some okind -> okind
        | _ -> `Null)
      end
    | _ -> EncBase'.json_to_string `Null
  
  let _jitem_to_s kind idx v = match kind with
    | `Array [`List ikind] -> EncBase'.json_to_string ikind v
    | `Array akind ->
      let vs = Ezjsonm.get_list Fun.id v in
      let kinds = EncBase'.singletons_of_array ~len:(min idx (List.length vs)) akind in
      let kindarr = Array.of_list kinds in
      EncBase'.json_to_string (kindarr.(idx)) v
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
        | `Path, `Label, false, `O o -> String.concat "" (List.map (fun (n, v) -> "." ^ n ^ "." ^ jfield_to_s ~n v) o)
        | `Path, `Label, true, `O o -> String.concat "" (List.map (fun (n, v) -> "." ^ n ^ "=" ^ jfield_to_s ~n v) o)
        | `Path, `Label, _, _ -> j_to_s j
        | `Path, `Simple, _, `A a -> String.concat "," (List.mapi jitem_to_s a)
        | `Path, `Simple, false, `O o -> String.concat "," (List.map (fun (n, v) -> n ^ "," ^ jfield_to_s ~n v) o)
        | `Path, `Simple, true, `O o -> String.concat "," (List.map (fun (n, v) -> n ^ "=" ^ jfield_to_s ~n v) o)
        | `Path, `Simple, _, _ -> j_to_s j
        (* | _ -> raise (Invalid_parameter m) *)
  
  (* For [Path] *)
  let _string_to ~kind ~dtr ~p =
    let s_to_j = EncBase'.string_to_json ~p kind in
    fun ~(loc:[`Path]) ~(style:[`Label | `Matrix | `Simple]) ~explode s ->
      let jopt =
        match (loc, style, explode) with
        | `Path, `Matrix, false -> s_to_j ~sep:';' ~inner_sep:'=' ~value_sep:(Some ',') ~leading:true s
        | `Path, `Matrix, true -> s_to_j ~sep:';' ~inner_sep:'=' ~value_sep:None ~leading:true s
        | `Path, `Label, false -> s_to_j ~sep:'.' ~inner_sep:'.' ~value_sep:None ~leading:true s
        | `Path, `Label, true -> s_to_j ~sep:'.' ~inner_sep:'=' ~value_sep:None ~leading:true s
        | `Path, `Simple, false -> s_to_j ~sep:',' ~inner_sep:',' ~value_sep:None ~leading:false s
        | `Path, `Simple, true -> s_to_j ~sep:',' ~inner_sep:'=' ~value_sep:None ~leading:false s
      in
      match jopt with
      | None, _ -> None
      | Some j, _ -> (
          try Some (dtr j) with Json_encoding.Cannot_destruct _ -> None)
  
  (* For [Query], [Cookie] and [Header] *)
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
          else raise (Invalid_parameter m)
        | _ -> raise (Invalid_parameter m)
  
  (* For [Query], [Cookie] and [Header]. *)
  let _namevalues_to ~kind ~p =
    let nvs_to_j = EncBase'.nvs_to_json ~kind ~p:(`Shallow p) in
    fun ~dtr ~loc ~style ~explode nvs ->
      let jopt =
        match (loc, style, explode) with
        | (`Query|`Cookie), `Form, false -> nvs_to_j ~sep:',' ~inner_sep:',' ~value_sep:None ~leading:false nvs
        | (`Query|`Cookie), `Form, true -> EncBase'.nvs_to_json ~kind ~p:`None ~sep:'&' ~inner_sep:'=' ~value_sep:None ~leading:false nvs
        | `Header, `Simple, false -> nvs_to_j ~sep:',' ~inner_sep:','  ~value_sep:None ~leading:false nvs
        | `Header, `Simple, true -> nvs_to_j ~sep:',' ~inner_sep:'=' ~value_sep:None ~leading:false nvs
        | `Query, `SpaceDelimited, false -> nvs_to_j ~sep:' ' ~inner_sep:' ' ~value_sep:None ~leading:false nvs
        | `Query, `PipeDelimited, false -> nvs_to_j ~sep:'|' ~inner_sep:'|' ~value_sep:None ~leading:false nvs
        | `Query, `DeepObject, true -> EncBase'.nvs_to_json ~kind ~p:(`Deep p) ~sep:'&' ~inner_sep:'=' ~value_sep:None ~leading:false nvs
        | _ -> None
      in
      match jopt with
      | None -> None
      | Some j -> (
          try Some (dtr j) with Json_encoding.Cannot_destruct _ -> None)
  
  
  
  let string_of_p_StringList ~p ~op ~loc ~style ~explode (_x : string list) =
    _string_of ~kind:(
      `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x)))
      ~ctr:(Json_encoding.construct (Json_encoding.list Json_encoding.string))
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_StringList ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct
                 (Json_encoding.list Json_encoding.string)) in _string_to ~p
      ~kind:(`Array [(`List (`String))]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_StringList ~p ~op ~loc ~style ~explode
    (_x : string list) =
    _namevalues_of ~kind:(
      `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x)))
      ~ctr:(Json_encoding.construct (Json_encoding.list Json_encoding.string))
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_p_StringList ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct
                 (Json_encoding.list Json_encoding.string)) in
      _namevalues_to ~p ~kind:(`Array [(`List (`String))])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_ab8d71cc96 ~p ~op ~loc ~style ~explode
    (_x : t_ab8d71cc96) =
    _string_of ~kind:(
      begin match _x with
      | T_6aa55531fa _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ab8d71cc96)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ab8d71cc96 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6aa55531fa) in
        Option.map (fun _y : t_ab8d71cc96 -> T_6aa55531fa _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ab8d71cc96 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_ab8d71cc96 ~p ~op ~loc ~style ~explode
    (_x : t_ab8d71cc96) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_6aa55531fa _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ab8d71cc96)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ab8d71cc96 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6aa55531fa) in
        Option.map (fun _y : t_ab8d71cc96 -> T_6aa55531fa _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ab8d71cc96 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_p_String_ ~p ~op ~loc ~style ~explode (_x : string) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Json_encoding.string)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_String_ ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Json_encoding.string) in _string_to ~p
      ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_String_ ~p ~op ~loc ~style ~explode (_x : string) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Json_encoding.string)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_p_String_ ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Json_encoding.string) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_p_Int ~p ~op ~loc ~style ~explode (_x : int) =
    _string_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct Json_encoding.int)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_Int ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Json_encoding.int) in _string_to ~p
      ~kind:(`Integer) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_Int ~p ~op ~loc ~style ~explode (_x : int) =
    _namevalues_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct Json_encoding.int)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_p_Int ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Json_encoding.int) in _namevalues_to ~p
      ~kind:(`Integer) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_4e604540e7 ~p ~op ~loc ~style ~explode
    (_x : t_4e604540e7) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4e604540e7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4e604540e7 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4e604540e7) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_4e604540e7 ~p ~op ~loc ~style ~explode
    (_x : t_4e604540e7) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4e604540e7)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_4e604540e7 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_4e604540e7) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_8dfdeac1ad ~p ~op ~loc ~style ~explode
    (_x : t_8dfdeac1ad) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("director", begin match _x.director with | None -> `Null
          | Some _x -> `Boolean end);
         ("executive", begin match _x.executive with | None -> `Null
          | Some _x -> `Boolean end);
         ("legal_guardian", begin match _x.legal_guardian with
          | None -> `Null | Some _x -> `Boolean end);
         ("owner", begin match _x.owner with | None -> `Null | Some _x ->
          `Boolean end);
         ("representative", begin match _x.representative with
          | None -> `Null | Some _x -> `Boolean end)])
      ~ctr:(Json_encoding.construct Encoders'.t_8dfdeac1ad)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8dfdeac1ad ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_8dfdeac1ad) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("director", `Boolean); ("executive", `Boolean);
                ("legal_guardian", `Boolean); ("owner", `Boolean);
                ("representative", `Boolean)]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_8dfdeac1ad ~p ~op ~loc ~style ~explode
    (_x : t_8dfdeac1ad) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("director", begin match _x.director with | None -> `Null
          | Some _x -> `Boolean end);
         ("executive", begin match _x.executive with | None -> `Null
          | Some _x -> `Boolean end);
         ("legal_guardian", begin match _x.legal_guardian with
          | None -> `Null | Some _x -> `Boolean end);
         ("owner", begin match _x.owner with | None -> `Null | Some _x ->
          `Boolean end);
         ("representative", begin match _x.representative with
          | None -> `Null | Some _x -> `Boolean end)])
      ~ctr:(Json_encoding.construct Encoders'.t_8dfdeac1ad)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_8dfdeac1ad ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_8dfdeac1ad) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("director", `Boolean); ("executive", `Boolean);
                ("legal_guardian", `Boolean); ("owner", `Boolean);
                ("representative", `Boolean)]) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_52a890434c ~p ~op ~loc ~style ~explode
    (_x : t_52a890434c) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("director", begin match _x.director with | None -> `Null
          | Some _x -> `Boolean end);
         ("executive", begin match _x.executive with | None -> `Null
          | Some _x -> `Boolean end);
         ("legal_guardian", begin match _x.legal_guardian with
          | None -> `Null | Some _x -> `Boolean end);
         ("owner", begin match _x.owner with | None -> `Null | Some _x ->
          `Boolean end);
         ("representative", begin match _x.representative with
          | None -> `Null | Some _x -> `Boolean end)])
      ~ctr:(Json_encoding.construct Encoders'.t_52a890434c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_52a890434c ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_52a890434c) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("director", `Boolean); ("executive", `Boolean);
                ("legal_guardian", `Boolean); ("owner", `Boolean);
                ("representative", `Boolean)]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_52a890434c ~p ~op ~loc ~style ~explode
    (_x : t_52a890434c) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("director", begin match _x.director with | None -> `Null
          | Some _x -> `Boolean end);
         ("executive", begin match _x.executive with | None -> `Null
          | Some _x -> `Boolean end);
         ("legal_guardian", begin match _x.legal_guardian with
          | None -> `Null | Some _x -> `Boolean end);
         ("owner", begin match _x.owner with | None -> `Null | Some _x ->
          `Boolean end);
         ("representative", begin match _x.representative with
          | None -> `Null | Some _x -> `Boolean end)])
      ~ctr:(Json_encoding.construct Encoders'.t_52a890434c)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_52a890434c ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_52a890434c) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("director", `Boolean); ("executive", `Boolean);
                ("legal_guardian", `Boolean); ("owner", `Boolean);
                ("representative", `Boolean)]) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_9d68bdd713 ~p ~op ~loc ~style ~explode
    (_x : t_9d68bdd713) =
    _string_of ~kind:(
      begin match _x with
      | T_6705dd5948 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_9d68bdd713)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9d68bdd713 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6705dd5948) in
        Option.map (fun _y : t_9d68bdd713 -> T_6705dd5948 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9d68bdd713 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_9d68bdd713 ~p ~op ~loc ~style ~explode
    (_x : t_9d68bdd713) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_6705dd5948 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_9d68bdd713)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_9d68bdd713 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6705dd5948) in
        Option.map (fun _y : t_9d68bdd713 -> T_6705dd5948 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9d68bdd713 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_4a4b8daa1f ~p ~op ~loc ~style ~explode
    (_x : t_4a4b8daa1f) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any); ("type", let _x = _x.type_ in `String);
         ("user", begin match _x.user with | None -> `Null | Some _x ->
          `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_4a4b8daa1f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4a4b8daa1f ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4a4b8daa1f) in
      _string_to ~p
      ~kind:(`ObjectN [("", `Any); ("type", `String); ("user", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_4a4b8daa1f ~p ~op ~loc ~style ~explode
    (_x : t_4a4b8daa1f) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any); ("type", let _x = _x.type_ in `String);
         ("user", begin match _x.user with | None -> `Null | Some _x ->
          `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_4a4b8daa1f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_4a4b8daa1f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_4a4b8daa1f) in
      _namevalues_to ~p
      ~kind:(`ObjectN [("", `Any); ("type", `String); ("user", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_4ce91395e3 ~p ~op ~loc ~style ~explode
    (_x : t_4ce91395e3) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any); ("type", let _x = _x.type_ in `String);
         ("user", begin match _x.user with | None -> `Null | Some _x ->
          `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_4ce91395e3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4ce91395e3 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4ce91395e3) in
      _string_to ~p
      ~kind:(`ObjectN [("", `Any); ("type", `String); ("user", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_4ce91395e3 ~p ~op ~loc ~style ~explode
    (_x : t_4ce91395e3) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any); ("type", let _x = _x.type_ in `String);
         ("user", begin match _x.user with | None -> `Null | Some _x ->
          `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_4ce91395e3)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_4ce91395e3 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_4ce91395e3) in
      _namevalues_to ~p
      ~kind:(`ObjectN [("", `Any); ("type", `String); ("user", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_01b97bcd1e ~p ~op ~loc ~style ~explode
    (_x : t_01b97bcd1e) =
    _string_of ~kind:(
      begin match _x with
      | T_51dd3dd546 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_01b97bcd1e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_01b97bcd1e ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_51dd3dd546) in
        Option.map (fun _y : t_01b97bcd1e -> T_51dd3dd546 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_01b97bcd1e -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_01b97bcd1e ~p ~op ~loc ~style ~explode
    (_x : t_01b97bcd1e) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_51dd3dd546 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_01b97bcd1e)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_01b97bcd1e ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_51dd3dd546) in
        Option.map (fun _y : t_01b97bcd1e -> T_51dd3dd546 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_01b97bcd1e -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_e84cc44f8f ~p ~op ~loc ~style ~explode
    (_x : t_e84cc44f8f) =
    _string_of ~kind:(
      begin match _x with
      | T_b5ed09371b _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_e84cc44f8f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e84cc44f8f ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_b5ed09371b) in
        Option.map (fun _y : t_e84cc44f8f -> T_b5ed09371b _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e84cc44f8f -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_e84cc44f8f ~p ~op ~loc ~style ~explode
    (_x : t_e84cc44f8f) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_b5ed09371b _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_e84cc44f8f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e84cc44f8f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_b5ed09371b) in
        Option.map (fun _y : t_e84cc44f8f -> T_b5ed09371b _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e84cc44f8f -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_b478178155 ~p ~op ~loc ~style ~explode
    (_x : t_b478178155) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b478178155)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b478178155 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b478178155) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b478178155 ~p ~op ~loc ~style ~explode
    (_x : t_b478178155) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b478178155)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b478178155 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_b478178155) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_p_Ptime_t ~p ~op ~loc ~style ~explode (_x : Ptime.t) =
    _string_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct EncBase'.vendor_unix_time)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_Ptime_t ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
      _string_to ~p ~kind:(`Integer) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_Ptime_t ~p ~op ~loc ~style ~explode (_x : Ptime.t) =
    _namevalues_of ~kind:( `Integer)
      ~ctr:(Json_encoding.construct EncBase'.vendor_unix_time)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_p_Ptime_t ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
      _namevalues_to ~p ~kind:(`Integer) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_2ef034b1cb ~p ~op ~loc ~style ~explode
    (_x : t_2ef034b1cb) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2ef034b1cb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2ef034b1cb ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_2ef034b1cb) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_2ef034b1cb ~p ~op ~loc ~style ~explode
    (_x : t_2ef034b1cb) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2ef034b1cb)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_2ef034b1cb ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_2ef034b1cb) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_p_Bool ~p ~op ~loc ~style ~explode (_x : bool) =
    _string_of ~kind:( `Boolean)
      ~ctr:(Json_encoding.construct Json_encoding.bool)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_p_Bool ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Json_encoding.bool) in _string_to ~p
      ~kind:(`Boolean) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_p_Bool ~p ~op ~loc ~style ~explode (_x : bool) =
    _namevalues_of ~kind:( `Boolean)
      ~ctr:(Json_encoding.construct Json_encoding.bool)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_p_Bool ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Json_encoding.bool) in
      _namevalues_to ~p ~kind:(`Boolean) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_9b9d8d63ef ~p ~op ~loc ~style ~explode
    (_x : t_9b9d8d63ef) =
    _string_of ~kind:(
      begin match _x with
      | T_143678df45 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_9b9d8d63ef)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9b9d8d63ef ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_143678df45) in
        Option.map (fun _y : t_9b9d8d63ef -> T_143678df45 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9b9d8d63ef -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_9b9d8d63ef ~p ~op ~loc ~style ~explode
    (_x : t_9b9d8d63ef) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_143678df45 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_9b9d8d63ef)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_9b9d8d63ef ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_143678df45) in
        Option.map (fun _y : t_9b9d8d63ef -> T_143678df45 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9b9d8d63ef -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_0b0b7bdb7e ~p ~op ~loc ~style ~explode
    (_x : t_0b0b7bdb7e) =
    _string_of ~kind:(
      begin match _x with
      | T_7da3863d89 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_0b0b7bdb7e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0b0b7bdb7e ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_7da3863d89) in
        Option.map (fun _y : t_0b0b7bdb7e -> T_7da3863d89 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_0b0b7bdb7e -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_0b0b7bdb7e ~p ~op ~loc ~style ~explode
    (_x : t_0b0b7bdb7e) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_7da3863d89 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_0b0b7bdb7e)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_0b0b7bdb7e ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_7da3863d89) in
        Option.map (fun _y : t_0b0b7bdb7e -> T_7da3863d89 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_0b0b7bdb7e -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_12bf81c281 ~p ~op ~loc ~style ~explode
    (_x : t_12bf81c281) =
    _string_of ~kind:(
      `ObjectN [("", `Any); ("email", let _x = _x.email in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_12bf81c281)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_12bf81c281 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_12bf81c281) in
      _string_to ~p ~kind:(`ObjectN [("", `Any); ("email", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_12bf81c281 ~p ~op ~loc ~style ~explode
    (_x : t_12bf81c281) =
    _namevalues_of ~kind:(
      `ObjectN [("", `Any); ("email", let _x = _x.email in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_12bf81c281)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_12bf81c281 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_12bf81c281) in
      _namevalues_to ~p ~kind:(`ObjectN [("", `Any); ("email", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_976c399de7 ~p ~op ~loc ~style ~explode
    (_x : t_976c399de7) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_976c399de7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_976c399de7 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_976c399de7) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_976c399de7 ~p ~op ~loc ~style ~explode
    (_x : t_976c399de7) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_976c399de7)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_976c399de7 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_976c399de7) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_8efa015a15 ~p ~op ~loc ~style ~explode
    (_x : t_8efa015a15) =
    _string_of ~kind:(
      begin match _x with
      | T_13e89791f1 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8efa015a15)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8efa015a15 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_13e89791f1) in
        Option.map (fun _y : t_8efa015a15 -> T_13e89791f1 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8efa015a15 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_8efa015a15 ~p ~op ~loc ~style ~explode
    (_x : t_8efa015a15) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_13e89791f1 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8efa015a15)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_8efa015a15 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_13e89791f1) in
        Option.map (fun _y : t_8efa015a15 -> T_13e89791f1 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8efa015a15 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_d1452a2e6d ~p ~op ~loc ~style ~explode
    (_x : t_d1452a2e6d) =
    _string_of ~kind:(
      begin match _x with
      | T_753b9f6893 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_d1452a2e6d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d1452a2e6d ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_753b9f6893) in
        Option.map (fun _y : t_d1452a2e6d -> T_753b9f6893 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d1452a2e6d -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_d1452a2e6d ~p ~op ~loc ~style ~explode
    (_x : t_d1452a2e6d) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_753b9f6893 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_d1452a2e6d)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_d1452a2e6d ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_753b9f6893) in
        Option.map (fun _y : t_d1452a2e6d -> T_753b9f6893 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d1452a2e6d -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_c4284eadfd ~p ~op ~loc ~style ~explode
    (_x : t_c4284eadfd) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("shipping_rate", begin match _x.shipping_rate with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_c4284eadfd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c4284eadfd ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_c4284eadfd) in
      _string_to ~p ~kind:(`ObjectN [("", `Any); ("shipping_rate", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_c4284eadfd ~p ~op ~loc ~style ~explode
    (_x : t_c4284eadfd) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("shipping_rate", begin match _x.shipping_rate with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_c4284eadfd)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c4284eadfd ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_c4284eadfd) in
      _namevalues_to ~p
      ~kind:(`ObjectN [("", `Any); ("shipping_rate", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_c086813c51 ~p ~op ~loc ~style ~explode
    (_x : t_c086813c51) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_cbe4f5acc3) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("invoice_line_item", begin match _x.invoice_line_item with
                   | None -> `Null | Some _x -> `String end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_amounts", begin match _x.tax_amounts with
                   | None -> `Null | Some _x ->
                   begin match _x with
                   | T_4eafa82af0 _x ->
                     `Array
                       ((List.map (fun (_x : t_ec0691360c) ->
                           `Singleton (`Null)) _x))
                   | T_603594d6a9 _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_3075582534 _x -> `String
                   end end);
                  ("type", let _x = _x.type_ in `String);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_c086813c51)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c086813c51 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_c086813c51) in
      _string_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("description", `String);
                       ("invoice_line_item", `String);
                       ("quantity", `Integer);
                       ("tax_amounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("type", `String); ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_c086813c51 ~p ~op ~loc ~style ~explode
    (_x : t_c086813c51) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_cbe4f5acc3) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("invoice_line_item", begin match _x.invoice_line_item with
                   | None -> `Null | Some _x -> `String end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_amounts", begin match _x.tax_amounts with
                   | None -> `Null | Some _x ->
                   begin match _x with
                   | T_4eafa82af0 _x ->
                     `Array
                       ((List.map (fun (_x : t_ec0691360c) ->
                           `Singleton (`Null)) _x))
                   | T_603594d6a9 _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_3075582534 _x -> `String
                   end end);
                  ("type", let _x = _x.type_ in `String);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_c086813c51)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c086813c51 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_c086813c51) in
      _namevalues_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("description", `String);
                       ("invoice_line_item", `String);
                       ("quantity", `Integer);
                       ("tax_amounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("type", `String); ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_fa0c590277 ~p ~op ~loc ~style ~explode
    (_x : t_fa0c590277) =
    _string_of ~kind:( `ObjectN [("", `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_fa0c590277)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fa0c590277 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_fa0c590277) in
      _string_to ~p ~kind:(`ObjectN [("", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_fa0c590277 ~p ~op ~loc ~style ~explode
    (_x : t_fa0c590277) =
    _namevalues_of ~kind:( `ObjectN [("", `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_fa0c590277)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_fa0c590277 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_fa0c590277) in
      _namevalues_to ~p ~kind:(`ObjectN [("", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_935a453a3f ~p ~op ~loc ~style ~explode
    (_x : t_935a453a3f) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_935a453a3f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_935a453a3f ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_935a453a3f) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_935a453a3f ~p ~op ~loc ~style ~explode
    (_x : t_935a453a3f) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_935a453a3f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_935a453a3f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_935a453a3f) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_5780adc875 ~p ~op ~loc ~style ~explode
    (_x : t_5780adc875) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_5780adc875)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_5780adc875 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_5780adc875) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_5780adc875 ~p ~op ~loc ~style ~explode
    (_x : t_5780adc875) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_5780adc875)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_5780adc875 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_5780adc875) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_7d4b84944c ~p ~op ~loc ~style ~explode
    (_x : t_7d4b84944c) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("shipping_rate", begin match _x.shipping_rate with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_7d4b84944c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_7d4b84944c ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_7d4b84944c) in
      _string_to ~p ~kind:(`ObjectN [("", `Any); ("shipping_rate", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_7d4b84944c ~p ~op ~loc ~style ~explode
    (_x : t_7d4b84944c) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("shipping_rate", begin match _x.shipping_rate with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_7d4b84944c)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_7d4b84944c ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_7d4b84944c) in
      _namevalues_to ~p
      ~kind:(`ObjectN [("", `Any); ("shipping_rate", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_3acf43dc3b ~p ~op ~loc ~style ~explode
    (_x : t_3acf43dc3b) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_595b5537ad) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("invoice_line_item", begin match _x.invoice_line_item with
                   | None -> `Null | Some _x -> `String end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_amounts", begin match _x.tax_amounts with
                   | None -> `Null | Some _x ->
                   begin match _x with
                   | T_2c60c8fbae _x ->
                     `Array
                       ((List.map (fun (_x : t_1b02f7c5eb) ->
                           `Singleton (`Null)) _x))
                   | T_aa81a712ee _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_1514a7e869 _x -> `String
                   end end);
                  ("type", let _x = _x.type_ in `String);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_3acf43dc3b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_3acf43dc3b ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_3acf43dc3b) in
      _string_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("description", `String);
                       ("invoice_line_item", `String);
                       ("quantity", `Integer);
                       ("tax_amounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("type", `String); ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_3acf43dc3b ~p ~op ~loc ~style ~explode
    (_x : t_3acf43dc3b) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_595b5537ad) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("invoice_line_item", begin match _x.invoice_line_item with
                   | None -> `Null | Some _x -> `String end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_amounts", begin match _x.tax_amounts with
                   | None -> `Null | Some _x ->
                   begin match _x with
                   | T_2c60c8fbae _x ->
                     `Array
                       ((List.map (fun (_x : t_1b02f7c5eb) ->
                           `Singleton (`Null)) _x))
                   | T_aa81a712ee _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_1514a7e869 _x -> `String
                   end end);
                  ("type", let _x = _x.type_ in `String);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_3acf43dc3b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_3acf43dc3b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_3acf43dc3b) in
      _namevalues_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("description", `String);
                       ("invoice_line_item", `String);
                       ("quantity", `Integer);
                       ("tax_amounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("type", `String); ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_2551b208c1 ~p ~op ~loc ~style ~explode
    (_x : t_2551b208c1) =
    _string_of ~kind:( `ObjectN [("", `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_2551b208c1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2551b208c1 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_2551b208c1) in
      _string_to ~p ~kind:(`ObjectN [("", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_2551b208c1 ~p ~op ~loc ~style ~explode
    (_x : t_2551b208c1) =
    _namevalues_of ~kind:( `ObjectN [("", `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_2551b208c1)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_2551b208c1 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_2551b208c1) in
      _namevalues_to ~p ~kind:(`ObjectN [("", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_8cd3871a8a ~p ~op ~loc ~style ~explode
    (_x : t_8cd3871a8a) =
    _string_of ~kind:(
      begin match _x with
      | T_29c5820fae _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8cd3871a8a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8cd3871a8a ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_29c5820fae) in
        Option.map (fun _y : t_8cd3871a8a -> T_29c5820fae _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8cd3871a8a -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_8cd3871a8a ~p ~op ~loc ~style ~explode
    (_x : t_8cd3871a8a) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_29c5820fae _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8cd3871a8a)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_8cd3871a8a ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_29c5820fae) in
        Option.map (fun _y : t_8cd3871a8a -> T_29c5820fae _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8cd3871a8a -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_4f49500d45 ~p ~op ~loc ~style ~explode
    (_x : t_4f49500d45) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4f49500d45)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4f49500d45 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4f49500d45) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_4f49500d45 ~p ~op ~loc ~style ~explode
    (_x : t_4f49500d45) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4f49500d45)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_4f49500d45 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_4f49500d45) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_e03d9a444b ~p ~op ~loc ~style ~explode
    (_x : t_e03d9a444b) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e03d9a444b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e03d9a444b ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e03d9a444b) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e03d9a444b ~p ~op ~loc ~style ~explode
    (_x : t_e03d9a444b) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e03d9a444b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e03d9a444b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_e03d9a444b) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_d8f80ab533 ~p ~op ~loc ~style ~explode
    (_x : t_d8f80ab533) =
    _string_of ~kind:(
      begin match _x with
      | T_05166771dc _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_d8f80ab533)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d8f80ab533 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_05166771dc) in
        Option.map (fun _y : t_d8f80ab533 -> T_05166771dc _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d8f80ab533 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_d8f80ab533 ~p ~op ~loc ~style ~explode
    (_x : t_d8f80ab533) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_05166771dc _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_d8f80ab533)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_d8f80ab533 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_05166771dc) in
        Option.map (fun _y : t_d8f80ab533 -> T_05166771dc _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_d8f80ab533 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_194d7f2624 ~p ~op ~loc ~style ~explode
    (_x : t_194d7f2624) =
    _string_of ~kind:(
      begin match _x with
      | T_4a62a84b9e _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_194d7f2624)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_194d7f2624 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_4a62a84b9e) in
        Option.map (fun _y : t_194d7f2624 -> T_4a62a84b9e _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_194d7f2624 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_194d7f2624 ~p ~op ~loc ~style ~explode
    (_x : t_194d7f2624) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_4a62a84b9e _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_194d7f2624)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_194d7f2624 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_4a62a84b9e) in
        Option.map (fun _y : t_194d7f2624 -> T_4a62a84b9e _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_194d7f2624 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_76bf2704bf ~p ~op ~loc ~style ~explode
    (_x : t_76bf2704bf) =
    _string_of ~kind:(
      begin match _x with
      | T_019e891337 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_76bf2704bf)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_76bf2704bf ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_019e891337) in
        Option.map (fun _y : t_76bf2704bf -> T_019e891337 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_76bf2704bf -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_76bf2704bf ~p ~op ~loc ~style ~explode
    (_x : t_76bf2704bf) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_019e891337 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_76bf2704bf)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_76bf2704bf ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_019e891337) in
        Option.map (fun _y : t_76bf2704bf -> T_019e891337 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_76bf2704bf -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_2657bcad54 ~p ~op ~loc ~style ~explode
    (_x : t_2657bcad54) =
    _string_of ~kind:(
      begin match _x with
      | T_872f5780fe _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_2657bcad54)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2657bcad54 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_872f5780fe) in
        Option.map (fun _y : t_2657bcad54 -> T_872f5780fe _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_2657bcad54 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_2657bcad54 ~p ~op ~loc ~style ~explode
    (_x : t_2657bcad54) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_872f5780fe _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_2657bcad54)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_2657bcad54 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_872f5780fe) in
        Option.map (fun _y : t_2657bcad54 -> T_872f5780fe _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_2657bcad54 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_512e4129fd ~p ~op ~loc ~style ~explode
    (_x : t_512e4129fd) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_512e4129fd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_512e4129fd ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_512e4129fd) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_512e4129fd ~p ~op ~loc ~style ~explode
    (_x : t_512e4129fd) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_512e4129fd)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_512e4129fd ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_512e4129fd) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_32de9e954f ~p ~op ~loc ~style ~explode
    (_x : t_32de9e954f) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("customer", begin match _x.customer with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_32de9e954f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_32de9e954f ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_32de9e954f) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("account", `String); ("customer", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_32de9e954f ~p ~op ~loc ~style ~explode
    (_x : t_32de9e954f) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("customer", begin match _x.customer with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_32de9e954f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_32de9e954f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_32de9e954f) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("account", `String); ("customer", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_656d49aac0 ~p ~op ~loc ~style ~explode
    (_x : t_656d49aac0) =
    _string_of ~kind:(
      begin match _x with
      | T_3a94416575 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_656d49aac0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_656d49aac0 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_3a94416575) in
        Option.map (fun _y : t_656d49aac0 -> T_3a94416575 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_656d49aac0 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_656d49aac0 ~p ~op ~loc ~style ~explode
    (_x : t_656d49aac0) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_3a94416575 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_656d49aac0)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_656d49aac0 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_3a94416575) in
        Option.map (fun _y : t_656d49aac0 -> T_3a94416575 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_656d49aac0 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_83d5590180 ~p ~op ~loc ~style ~explode
    (_x : t_83d5590180) =
    _string_of ~kind:(
      `ObjectN [("", `Any); ("after", let _x = _x.after in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_83d5590180)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_83d5590180 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_83d5590180) in
      _string_to ~p ~kind:(`ObjectN [("", `Any); ("after", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_83d5590180 ~p ~op ~loc ~style ~explode
    (_x : t_83d5590180) =
    _namevalues_of ~kind:(
      `ObjectN [("", `Any); ("after", let _x = _x.after in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_83d5590180)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_83d5590180 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_83d5590180) in
      _namevalues_to ~p ~kind:(`ObjectN [("", `Any); ("after", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_a63e6bd7de ~p ~op ~loc ~style ~explode
    (_x : t_a63e6bd7de) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("gt", begin match _x.gt with | None -> `Null | Some _x -> `Integer
          end);
         ("gte", begin match _x.gte with | None -> `Null | Some _x ->
          `Integer end);
         ("lt", begin match _x.lt with | None -> `Null | Some _x -> `Integer
          end);
         ("lte", begin match _x.lte with | None -> `Null | Some _x ->
          `Integer end)])
      ~ctr:(Json_encoding.construct Encoders'.t_a63e6bd7de)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a63e6bd7de ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_a63e6bd7de) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("gt", `Integer); ("gte", `Integer);
                ("lt", `Integer); ("lte", `Integer)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_a63e6bd7de ~p ~op ~loc ~style ~explode
    (_x : t_a63e6bd7de) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("gt", begin match _x.gt with | None -> `Null | Some _x -> `Integer
          end);
         ("gte", begin match _x.gte with | None -> `Null | Some _x ->
          `Integer end);
         ("lt", begin match _x.lt with | None -> `Null | Some _x -> `Integer
          end);
         ("lte", begin match _x.lte with | None -> `Null | Some _x ->
          `Integer end)])
      ~ctr:(Json_encoding.construct Encoders'.t_a63e6bd7de)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_a63e6bd7de ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_a63e6bd7de) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("gt", `Integer); ("gte", `Integer);
                ("lt", `Integer); ("lte", `Integer)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_b566f1b6bc ~p ~op ~loc ~style ~explode
    (_x : t_b566f1b6bc) =
    _string_of ~kind:(
      begin match _x with
      | T_862da605ac _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_b566f1b6bc)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b566f1b6bc ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_862da605ac) in
        Option.map (fun _y : t_b566f1b6bc -> T_862da605ac _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_b566f1b6bc -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_b566f1b6bc ~p ~op ~loc ~style ~explode
    (_x : t_b566f1b6bc) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_862da605ac _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_b566f1b6bc)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b566f1b6bc ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_862da605ac) in
        Option.map (fun _y : t_b566f1b6bc -> T_862da605ac _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_b566f1b6bc -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_53be0c51ff ~p ~op ~loc ~style ~explode
    (_x : t_53be0c51ff) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_53be0c51ff)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_53be0c51ff ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_53be0c51ff) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_53be0c51ff ~p ~op ~loc ~style ~explode
    (_x : t_53be0c51ff) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_53be0c51ff)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_53be0c51ff ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_53be0c51ff) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_8333ac0d50 ~p ~op ~loc ~style ~explode
    (_x : t_8333ac0d50) =
    _string_of ~kind:(
      begin match _x with
      | T_da2453d199 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8333ac0d50)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8333ac0d50 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_da2453d199) in
        Option.map (fun _y : t_8333ac0d50 -> T_da2453d199 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8333ac0d50 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_8333ac0d50 ~p ~op ~loc ~style ~explode
    (_x : t_8333ac0d50) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_da2453d199 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8333ac0d50)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_8333ac0d50 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_da2453d199) in
        Option.map (fun _y : t_8333ac0d50 -> T_da2453d199 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8333ac0d50 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_060ea1562f ~p ~op ~loc ~style ~explode
    (_x : t_060ea1562f) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_060ea1562f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_060ea1562f ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_060ea1562f) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_060ea1562f ~p ~op ~loc ~style ~explode
    (_x : t_060ea1562f) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_060ea1562f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_060ea1562f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_060ea1562f) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_7181394bbb ~p ~op ~loc ~style ~explode
    (_x : t_7181394bbb) =
    _string_of ~kind:(
      begin match _x with
      | T_6c4276e1a4 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_7181394bbb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_7181394bbb ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6c4276e1a4) in
        Option.map (fun _y : t_7181394bbb -> T_6c4276e1a4 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_7181394bbb -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_7181394bbb ~p ~op ~loc ~style ~explode
    (_x : t_7181394bbb) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_6c4276e1a4 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_7181394bbb)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_7181394bbb ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6c4276e1a4) in
        Option.map (fun _y : t_7181394bbb -> T_6c4276e1a4 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_7181394bbb -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_3d308e0087 ~p ~op ~loc ~style ~explode
    (_x : t_3d308e0087) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_3d308e0087)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_3d308e0087 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_3d308e0087) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_3d308e0087 ~p ~op ~loc ~style ~explode
    (_x : t_3d308e0087) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_3d308e0087)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_3d308e0087 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_3d308e0087) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_5fae893ff7 ~p ~op ~loc ~style ~explode
    (_x : t_5fae893ff7) =
    _string_of ~kind:(
      begin match _x with
      | T_eb52c2c010 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_5fae893ff7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_5fae893ff7 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_eb52c2c010) in
        Option.map (fun _y : t_5fae893ff7 -> T_eb52c2c010 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_5fae893ff7 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_5fae893ff7 ~p ~op ~loc ~style ~explode
    (_x : t_5fae893ff7) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_eb52c2c010 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_5fae893ff7)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_5fae893ff7 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_eb52c2c010) in
        Option.map (fun _y : t_5fae893ff7 -> T_eb52c2c010 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_5fae893ff7 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_e8ff8d2aa0 ~p ~op ~loc ~style ~explode
    (_x : t_e8ff8d2aa0) =
    _string_of ~kind:(
      begin match _x with
      | T_4c63ae17d6 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_e8ff8d2aa0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e8ff8d2aa0 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_4c63ae17d6) in
        Option.map (fun _y : t_e8ff8d2aa0 -> T_4c63ae17d6 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e8ff8d2aa0 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_e8ff8d2aa0 ~p ~op ~loc ~style ~explode
    (_x : t_e8ff8d2aa0) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_4c63ae17d6 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_e8ff8d2aa0)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e8ff8d2aa0 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_4c63ae17d6) in
        Option.map (fun _y : t_e8ff8d2aa0 -> T_4c63ae17d6 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e8ff8d2aa0 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_2e0259263b ~p ~op ~loc ~style ~explode
    (_x : t_2e0259263b) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2e0259263b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2e0259263b ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_2e0259263b) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_2e0259263b ~p ~op ~loc ~style ~explode
    (_x : t_2e0259263b) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_2e0259263b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_2e0259263b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_2e0259263b) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_a51466ec35 ~p ~op ~loc ~style ~explode
    (_x : t_a51466ec35) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any); ("enabled", let _x = _x.enabled in `Boolean);
         ("liability", begin match _x.liability with | None -> `Null
          | Some _x ->
          `ObjectN
            [("", `Any);
             ("account", begin match _x.account with | None -> `Null
              | Some _x -> `String end);
             ("type", let _x = _x.type_ in `String)]
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_a51466ec35)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a51466ec35 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_a51466ec35) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("enabled", `Boolean);
                ("liability",
                 `ObjectN
                   [("", `Any); ("account", `String); ("type", `String)])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_a51466ec35 ~p ~op ~loc ~style ~explode
    (_x : t_a51466ec35) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any); ("enabled", let _x = _x.enabled in `Boolean);
         ("liability", begin match _x.liability with | None -> `Null
          | Some _x ->
          `ObjectN
            [("", `Any);
             ("account", begin match _x.account with | None -> `Null
              | Some _x -> `String end);
             ("type", let _x = _x.type_ in `String)]
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_a51466ec35)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_a51466ec35 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_a51466ec35) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("enabled", `Boolean);
                ("liability",
                 `ObjectN
                   [("", `Any); ("account", `String); ("type", `String)])])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_50aa3198c9 ~p ~op ~loc ~style ~explode
    (_x : t_50aa3198c9) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_50aa3198c9)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_50aa3198c9 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_50aa3198c9) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_50aa3198c9 ~p ~op ~loc ~style ~explode
    (_x : t_50aa3198c9) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_50aa3198c9)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_50aa3198c9 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_50aa3198c9) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_c4767cb749 ~p ~op ~loc ~style ~explode
    (_x : t_c4767cb749) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("end_behavior", begin match _x.end_behavior with | None -> `Null
          | Some _x -> `String end);
         ("phases", begin match _x.phases with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_94553cb579) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("add_invoice_items",
                       begin match _x.add_invoice_items with | None -> `Null
                       | Some _x ->
                       `Array
                         ((List.map (fun (_x : t_32fe86aa85) ->
                             `Singleton (`Null)) _x))
                       end);
                      ("application_fee_percent",
                       begin match _x.application_fee_percent with
                       | None -> `Null | Some _x -> `Number end);
                      ("automatic_tax", begin match _x.automatic_tax with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("enabled", let _x = _x.enabled in `Null);
                          ("liability", begin match _x.liability with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("billing_cycle_anchor",
                       begin match _x.billing_cycle_anchor with
                       | None -> `Null | Some _x -> `String end);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_ea5c39759d _x -> `Null
                       | T_0829cab37e _x -> `Null
                       end end);
                      ("collection_method",
                       begin match _x.collection_method with | None -> `Null
                       | Some _x -> `String end);
                      ("coupon", begin match _x.coupon with | None -> `Null
                       | Some _x -> `String end);
                      ("default_payment_method",
                       begin match _x.default_payment_method with
                       | None -> `Null | Some _x -> `String end);
                      ("default_tax_rates",
                       begin match _x.default_tax_rates with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_cccdb8d4f1 _x -> `Null
                       end end);
                      ("description", begin match _x.description with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | String_ _x -> `Null
                       | T_dae89e1d87 _x -> `Null
                       end end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_f72277021f _x -> `Null
                       | T_a1930a67fa _x -> `Null
                       end end);
                      ("end_date", begin match _x.end_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_2fa9ab7487 _x -> `Null
                       end end);
                      ("invoice_settings",
                       begin match _x.invoice_settings with | None -> `Null
                       | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("account_tax_ids",
                           begin match _x.account_tax_ids with
                           | None -> `Null | Some _x -> `Null end);
                          ("days_until_due",
                           begin match _x.days_until_due with | None -> `Null
                           | Some _x -> `Null end);
                          ("issuer", begin match _x.issuer with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("items", let _x = _x.items in
                       `Array
                         ((List.map (fun (_x : t_1f4b985808) ->
                             `Singleton (`Null)) _x)));
                      ("iterations", begin match _x.iterations with
                       | None -> `Null | Some _x -> `Integer end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x -> `ObjectN [("", `Null)]
                       end);
                      ("on_behalf_of", begin match _x.on_behalf_of with
                       | None -> `Null | Some _x -> `String end);
                      ("proration_behavior",
                       begin match _x.proration_behavior with | None -> `Null
                       | Some _x -> `String end);
                      ("start_date", begin match _x.start_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_d0a68ee4b5 _x -> `Null
                       end end);
                      ("transfer_data", begin match _x.transfer_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("amount_percent",
                           begin match _x.amount_percent with | None -> `Null
                           | Some _x -> `Null end);
                          ("destination", let _x = _x.destination in `Null)]
                       end);
                      ("trial", begin match _x.trial with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("trial_end", begin match _x.trial_end with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_ea977ed81f _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_c4767cb749)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c4767cb749 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_c4767cb749) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("end_behavior", `String);
                ("phases",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("add_invoice_items", `Array [(`List (`Null))]);
                           ("application_fee_percent", `Number);
                           ("automatic_tax",
                            `ObjectN
                              [("", `Null); ("enabled", `Null);
                               ("liability", `Null)]);
                           ("billing_cycle_anchor", `String);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("collection_method", `String);
                           ("coupon", `String);
                           ("default_payment_method", `String);
                           ("default_tax_rates", `Choice
                                                 [`Null; `Null]);
                           ("description", `Choice
                                           [`Null; `Null]);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("end_date", `Choice
                                        [`Null; `Null]);
                           ("invoice_settings",
                            `ObjectN
                              [("", `Null); ("account_tax_ids", `Null);
                               ("days_until_due", `Null); ("issuer", `Null)]);
                           ("items", `Array [(`List (`Null))]);
                           ("iterations", `Integer);
                           ("metadata", `ObjectN [("", `Null)]);
                           ("on_behalf_of", `String);
                           ("proration_behavior", `String);
                           ("start_date", `Choice
                                          [`Null; `Null]);
                           ("transfer_data",
                            `ObjectN
                              [("", `Null); ("amount_percent", `Null);
                               ("destination", `Null)]);
                           ("trial", `Boolean);
                           ("trial_end", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String)]) ~dtr ~loc ~style ~explode
      _x
  
  let namevalues_of_t_c4767cb749 ~p ~op ~loc ~style ~explode
    (_x : t_c4767cb749) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("end_behavior", begin match _x.end_behavior with | None -> `Null
          | Some _x -> `String end);
         ("phases", begin match _x.phases with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_94553cb579) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("add_invoice_items",
                       begin match _x.add_invoice_items with | None -> `Null
                       | Some _x ->
                       `Array
                         ((List.map (fun (_x : t_32fe86aa85) ->
                             `Singleton (`Null)) _x))
                       end);
                      ("application_fee_percent",
                       begin match _x.application_fee_percent with
                       | None -> `Null | Some _x -> `Number end);
                      ("automatic_tax", begin match _x.automatic_tax with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("enabled", let _x = _x.enabled in `Null);
                          ("liability", begin match _x.liability with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("billing_cycle_anchor",
                       begin match _x.billing_cycle_anchor with
                       | None -> `Null | Some _x -> `String end);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_ea5c39759d _x -> `Null
                       | T_0829cab37e _x -> `Null
                       end end);
                      ("collection_method",
                       begin match _x.collection_method with | None -> `Null
                       | Some _x -> `String end);
                      ("coupon", begin match _x.coupon with | None -> `Null
                       | Some _x -> `String end);
                      ("default_payment_method",
                       begin match _x.default_payment_method with
                       | None -> `Null | Some _x -> `String end);
                      ("default_tax_rates",
                       begin match _x.default_tax_rates with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_cccdb8d4f1 _x -> `Null
                       end end);
                      ("description", begin match _x.description with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | String_ _x -> `Null
                       | T_dae89e1d87 _x -> `Null
                       end end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_f72277021f _x -> `Null
                       | T_a1930a67fa _x -> `Null
                       end end);
                      ("end_date", begin match _x.end_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_2fa9ab7487 _x -> `Null
                       end end);
                      ("invoice_settings",
                       begin match _x.invoice_settings with | None -> `Null
                       | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("account_tax_ids",
                           begin match _x.account_tax_ids with
                           | None -> `Null | Some _x -> `Null end);
                          ("days_until_due",
                           begin match _x.days_until_due with | None -> `Null
                           | Some _x -> `Null end);
                          ("issuer", begin match _x.issuer with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("items", let _x = _x.items in
                       `Array
                         ((List.map (fun (_x : t_1f4b985808) ->
                             `Singleton (`Null)) _x)));
                      ("iterations", begin match _x.iterations with
                       | None -> `Null | Some _x -> `Integer end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x -> `ObjectN [("", `Null)]
                       end);
                      ("on_behalf_of", begin match _x.on_behalf_of with
                       | None -> `Null | Some _x -> `String end);
                      ("proration_behavior",
                       begin match _x.proration_behavior with | None -> `Null
                       | Some _x -> `String end);
                      ("start_date", begin match _x.start_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_d0a68ee4b5 _x -> `Null
                       end end);
                      ("transfer_data", begin match _x.transfer_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("amount_percent",
                           begin match _x.amount_percent with | None -> `Null
                           | Some _x -> `Null end);
                          ("destination", let _x = _x.destination in `Null)]
                       end);
                      ("trial", begin match _x.trial with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("trial_end", begin match _x.trial_end with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_ea977ed81f _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_c4767cb749)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c4767cb749 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_c4767cb749) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("end_behavior", `String);
                ("phases",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("add_invoice_items", `Array [(`List (`Null))]);
                           ("application_fee_percent", `Number);
                           ("automatic_tax",
                            `ObjectN
                              [("", `Null); ("enabled", `Null);
                               ("liability", `Null)]);
                           ("billing_cycle_anchor", `String);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("collection_method", `String);
                           ("coupon", `String);
                           ("default_payment_method", `String);
                           ("default_tax_rates", `Choice
                                                 [`Null; `Null]);
                           ("description", `Choice
                                           [`Null; `Null]);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("end_date", `Choice
                                        [`Null; `Null]);
                           ("invoice_settings",
                            `ObjectN
                              [("", `Null); ("account_tax_ids", `Null);
                               ("days_until_due", `Null); ("issuer", `Null)]);
                           ("items", `Array [(`List (`Null))]);
                           ("iterations", `Integer);
                           ("metadata", `ObjectN [("", `Null)]);
                           ("on_behalf_of", `String);
                           ("proration_behavior", `String);
                           ("start_date", `Choice
                                          [`Null; `Null]);
                           ("transfer_data",
                            `ObjectN
                              [("", `Null); ("amount_percent", `Null);
                               ("destination", `Null)]);
                           ("trial", `Boolean);
                           ("trial_end", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String)]) ~dtr ~loc ~style ~explode
      _x
  
  let string_of_t_312ad6306f ~p ~op ~loc ~style ~explode
    (_x : t_312ad6306f) =
    _string_of ~kind:(
      begin match _x with
      | T_13d6b80cc4 _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_312ad6306f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_312ad6306f ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_13d6b80cc4) in
        Option.map (fun _y : t_312ad6306f -> T_13d6b80cc4 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_312ad6306f -> Ptime_t _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_312ad6306f ~p ~op ~loc ~style ~explode
    (_x : t_312ad6306f) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_13d6b80cc4 _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_312ad6306f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_312ad6306f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_13d6b80cc4) in
        Option.map (fun _y : t_312ad6306f -> T_13d6b80cc4 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_312ad6306f -> Ptime_t _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_b9ba448b2f ~p ~op ~loc ~style ~explode
    (_x : t_b9ba448b2f) =
    _string_of ~kind:(
      begin match _x with
      | Ptime_t _x -> `Integer
      | T_97816224ba _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_b9ba448b2f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b9ba448b2f ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_b9ba448b2f -> Ptime_t _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_97816224ba) in
        Option.map (fun _y : t_b9ba448b2f -> T_97816224ba _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_b9ba448b2f ~p ~op ~loc ~style ~explode
    (_x : t_b9ba448b2f) =
    _namevalues_of ~kind:(
      begin match _x with
      | Ptime_t _x -> `Integer
      | T_97816224ba _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_b9ba448b2f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b9ba448b2f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_b9ba448b2f -> Ptime_t _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_97816224ba) in
        Option.map (fun _y : t_b9ba448b2f -> T_97816224ba _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_0178e1face ~p ~op ~loc ~style ~explode
    (_x : t_0178e1face) =
    _string_of ~kind:(
      begin match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_177b598972 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_0178e1face)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_0178e1face ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `Array [(`List (`String))] in
        let dtr = (Json_encoding.destruct
                     (Json_encoding.list Json_encoding.string)) in
        Option.map (fun _y : t_0178e1face -> StringList _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_177b598972) in
        Option.map (fun _y : t_0178e1face -> T_177b598972 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_0178e1face ~p ~op ~loc ~style ~explode
    (_x : t_0178e1face) =
    _namevalues_of ~kind:(
      begin match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_177b598972 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_0178e1face)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_0178e1face ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `Array [(`List (`String))] in
        let dtr = (Json_encoding.destruct
                     (Json_encoding.list Json_encoding.string)) in
        Option.map (fun _y : t_0178e1face -> StringList _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_177b598972) in
        Option.map (fun _y : t_0178e1face -> T_177b598972 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_baccbfb036 ~p ~op ~loc ~style ~explode
    (_x : t_baccbfb036) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("billing_cycle_anchor", begin match _x.billing_cycle_anchor with
          | None -> `Null | Some _x ->
          begin match _x with
          | T_e7638b8884 _x -> `String
          | Ptime_t _x -> `Integer
          end end);
         ("cancel_at", begin match _x.cancel_at with | None -> `Null
          | Some _x ->
          begin match _x with
          | Ptime_t _x -> `Integer
          | T_8914431bd5 _x -> `String
          end end);
         ("cancel_at_period_end", begin match _x.cancel_at_period_end with
          | None -> `Null | Some _x -> `Boolean end);
         ("cancel_now", begin match _x.cancel_now with | None -> `Null
          | Some _x -> `Boolean end);
         ("default_tax_rates", begin match _x.default_tax_rates with
          | None -> `Null | Some _x ->
          begin match _x with
          | StringList _x ->
            `Array
              ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
          | T_49afdb843d _x -> `String
          end end);
         ("items", begin match _x.items with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_47a4de758c) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_59dcf31c18 _x -> `Null
                       | T_1a992df9bb _x -> `Null
                       end end);
                      ("clear_usage", begin match _x.clear_usage with
                       | None -> `Null | Some _x -> `Boolean end);
                      ("deleted", begin match _x.deleted with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_d7b1f5cd74 _x -> `Null
                       | T_e9d9a84421 _x -> `Null
                       end end);
                      ("id", begin match _x.id with | None -> `Null
                       | Some _x -> `String end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_7948b96036 _x -> `Null
                       | T_bfa9c3016a _x -> `Null
                       end end);
                      ("price", begin match _x.price with | None -> `Null
                       | Some _x -> `String end);
                      ("price_data", begin match _x.price_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("currency", let _x = _x.currency in `Null);
                          ("product", let _x = _x.product in `Null);
                          ("recurring", let _x = _x.recurring in `Null);
                          ("tax_behavior", begin match _x.tax_behavior with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount", begin match _x.unit_amount with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount_decimal",
                           begin match _x.unit_amount_decimal with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("quantity", begin match _x.quantity with
                       | None -> `Null | Some _x -> `Integer end);
                      ("tax_rates", begin match _x.tax_rates with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_8c176ec44f _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end);
         ("proration_date", begin match _x.proration_date with
          | None -> `Null | Some _x -> `Integer end);
         ("resume_at", begin match _x.resume_at with | None -> `Null
          | Some _x -> `String end);
         ("start_date", begin match _x.start_date with | None -> `Null
          | Some _x -> `Integer end);
         ("trial_end", begin match _x.trial_end with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_74d5102f8e _x -> `String
          | Ptime_t _x -> `Integer
          end end)]) ~ctr:(Json_encoding.construct Encoders'.t_baccbfb036)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_baccbfb036 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_baccbfb036) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("billing_cycle_anchor", `Choice
                                         [`String; `Integer]);
                ("cancel_at", `Choice
                              [`Integer; `String]);
                ("cancel_at_period_end", `Boolean); ("cancel_now", `Boolean);
                ("default_tax_rates",
                 `Choice
                 [`Array [(`List (`String))]; `String]);
                ("items",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("clear_usage", `Boolean); ("deleted", `Boolean);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("id", `String);
                           ("metadata", `Choice
                                        [`Null; `Null]);
                           ("price", `String);
                           ("price_data",
                            `ObjectN
                              [("", `Null); ("currency", `Null);
                               ("product", `Null); ("recurring", `Null);
                               ("tax_behavior", `Null);
                               ("unit_amount", `Null);
                               ("unit_amount_decimal", `Null)]);
                           ("quantity", `Integer);
                           ("tax_rates", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String);
                ("proration_date", `Integer); ("resume_at", `String);
                ("start_date", `Integer);
                ("trial_end", `Choice
                              [`String; `Integer])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_baccbfb036 ~p ~op ~loc ~style ~explode
    (_x : t_baccbfb036) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("billing_cycle_anchor", begin match _x.billing_cycle_anchor with
          | None -> `Null | Some _x ->
          begin match _x with
          | T_e7638b8884 _x -> `String
          | Ptime_t _x -> `Integer
          end end);
         ("cancel_at", begin match _x.cancel_at with | None -> `Null
          | Some _x ->
          begin match _x with
          | Ptime_t _x -> `Integer
          | T_8914431bd5 _x -> `String
          end end);
         ("cancel_at_period_end", begin match _x.cancel_at_period_end with
          | None -> `Null | Some _x -> `Boolean end);
         ("cancel_now", begin match _x.cancel_now with | None -> `Null
          | Some _x -> `Boolean end);
         ("default_tax_rates", begin match _x.default_tax_rates with
          | None -> `Null | Some _x ->
          begin match _x with
          | StringList _x ->
            `Array
              ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
          | T_49afdb843d _x -> `String
          end end);
         ("items", begin match _x.items with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_47a4de758c) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_59dcf31c18 _x -> `Null
                       | T_1a992df9bb _x -> `Null
                       end end);
                      ("clear_usage", begin match _x.clear_usage with
                       | None -> `Null | Some _x -> `Boolean end);
                      ("deleted", begin match _x.deleted with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_d7b1f5cd74 _x -> `Null
                       | T_e9d9a84421 _x -> `Null
                       end end);
                      ("id", begin match _x.id with | None -> `Null
                       | Some _x -> `String end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_7948b96036 _x -> `Null
                       | T_bfa9c3016a _x -> `Null
                       end end);
                      ("price", begin match _x.price with | None -> `Null
                       | Some _x -> `String end);
                      ("price_data", begin match _x.price_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("currency", let _x = _x.currency in `Null);
                          ("product", let _x = _x.product in `Null);
                          ("recurring", let _x = _x.recurring in `Null);
                          ("tax_behavior", begin match _x.tax_behavior with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount", begin match _x.unit_amount with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount_decimal",
                           begin match _x.unit_amount_decimal with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("quantity", begin match _x.quantity with
                       | None -> `Null | Some _x -> `Integer end);
                      ("tax_rates", begin match _x.tax_rates with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_8c176ec44f _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end);
         ("proration_date", begin match _x.proration_date with
          | None -> `Null | Some _x -> `Integer end);
         ("resume_at", begin match _x.resume_at with | None -> `Null
          | Some _x -> `String end);
         ("start_date", begin match _x.start_date with | None -> `Null
          | Some _x -> `Integer end);
         ("trial_end", begin match _x.trial_end with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_74d5102f8e _x -> `String
          | Ptime_t _x -> `Integer
          end end)]) ~ctr:(Json_encoding.construct Encoders'.t_baccbfb036)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_baccbfb036 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_baccbfb036) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("billing_cycle_anchor", `Choice
                                         [`String; `Integer]);
                ("cancel_at", `Choice
                              [`Integer; `String]);
                ("cancel_at_period_end", `Boolean); ("cancel_now", `Boolean);
                ("default_tax_rates",
                 `Choice
                 [`Array [(`List (`String))]; `String]);
                ("items",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("clear_usage", `Boolean); ("deleted", `Boolean);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("id", `String);
                           ("metadata", `Choice
                                        [`Null; `Null]);
                           ("price", `String);
                           ("price_data",
                            `ObjectN
                              [("", `Null); ("currency", `Null);
                               ("product", `Null); ("recurring", `Null);
                               ("tax_behavior", `Null);
                               ("unit_amount", `Null);
                               ("unit_amount_decimal", `Null)]);
                           ("quantity", `Integer);
                           ("tax_rates", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String);
                ("proration_date", `Integer); ("resume_at", `String);
                ("start_date", `Integer);
                ("trial_end", `Choice
                              [`String; `Integer])])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_671de99c34 ~p ~op ~loc ~style ~explode
    (_x : t_671de99c34) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_5935fcbdc2) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("billing_thresholds",
                   begin match _x.billing_thresholds with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_bb54fadb7d _x ->
                     `ObjectN
                       [("", `Null);
                        ("usage_gte", let _x = _x.usage_gte in `Null)]
                   | T_3728c21bc2 _x -> `String
                   end end);
                  ("clear_usage", begin match _x.clear_usage with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("deleted", begin match _x.deleted with | None -> `Null
                   | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_adbf82df0f _x ->
                     `Array
                       ((List.map (fun (_x : t_75b0a73940) ->
                           `Singleton (`Null)) _x))
                   | T_12c9506282 _x -> `String
                   end end);
                  ("id", begin match _x.id with | None -> `Null | Some _x ->
                   `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_87d859f618 _x -> `ObjectN [("", `Null)]
                   | T_5e8ddbf64f _x -> `String
                   end end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("recurring", let _x = _x.recurring in
                       `ObjectN
                         [("", `Null);
                          ("interval", let _x = _x.interval in `Null);
                          ("interval_count",
                           begin match _x.interval_count with | None -> `Null
                           | Some _x -> `Null end)]);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_6755408a65 _x -> `String
                   end end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_671de99c34)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_671de99c34 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_671de99c34) in
      _string_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any);
                       ("billing_thresholds",
                        `Choice
                        [`ObjectN [("", `Null); ("usage_gte", `Null)];
                         `String]);
                       ("clear_usage", `Boolean); ("deleted", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("id", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String);
                           ("recurring",
                            `ObjectN
                              [("", `Null); ("interval", `Null);
                               ("interval_count", `Null)]);
                           ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_671de99c34 ~p ~op ~loc ~style ~explode
    (_x : t_671de99c34) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_5935fcbdc2) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("billing_thresholds",
                   begin match _x.billing_thresholds with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_bb54fadb7d _x ->
                     `ObjectN
                       [("", `Null);
                        ("usage_gte", let _x = _x.usage_gte in `Null)]
                   | T_3728c21bc2 _x -> `String
                   end end);
                  ("clear_usage", begin match _x.clear_usage with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("deleted", begin match _x.deleted with | None -> `Null
                   | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_adbf82df0f _x ->
                     `Array
                       ((List.map (fun (_x : t_75b0a73940) ->
                           `Singleton (`Null)) _x))
                   | T_12c9506282 _x -> `String
                   end end);
                  ("id", begin match _x.id with | None -> `Null | Some _x ->
                   `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_87d859f618 _x -> `ObjectN [("", `Null)]
                   | T_5e8ddbf64f _x -> `String
                   end end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("recurring", let _x = _x.recurring in
                       `ObjectN
                         [("", `Null);
                          ("interval", let _x = _x.interval in `Null);
                          ("interval_count",
                           begin match _x.interval_count with | None -> `Null
                           | Some _x -> `Null end)]);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_6755408a65 _x -> `String
                   end end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_671de99c34)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_671de99c34 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_671de99c34) in
      _namevalues_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any);
                       ("billing_thresholds",
                        `Choice
                        [`ObjectN [("", `Null); ("usage_gte", `Null)];
                         `String]);
                       ("clear_usage", `Boolean); ("deleted", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("id", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String);
                           ("recurring",
                            `ObjectN
                              [("", `Null); ("interval", `Null);
                               ("interval_count", `Null)]);
                           ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_6dff880c24 ~p ~op ~loc ~style ~explode
    (_x : t_6dff880c24) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6dff880c24)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6dff880c24 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_6dff880c24) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_6dff880c24 ~p ~op ~loc ~style ~explode
    (_x : t_6dff880c24) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6dff880c24)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_6dff880c24 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_6dff880c24) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_50e9f09abe ~p ~op ~loc ~style ~explode
    (_x : t_50e9f09abe) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_50e9f09abe)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_50e9f09abe ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_50e9f09abe) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_50e9f09abe ~p ~op ~loc ~style ~explode
    (_x : t_50e9f09abe) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_50e9f09abe)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_50e9f09abe ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_50e9f09abe) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_30748e2d12 ~p ~op ~loc ~style ~explode
    (_x : t_30748e2d12) =
    _string_of ~kind:(
      begin match _x with
      | T_e18674598e _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_30748e2d12)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_30748e2d12 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_e18674598e) in
        Option.map (fun _y : t_30748e2d12 -> T_e18674598e _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_30748e2d12 -> Ptime_t _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_30748e2d12 ~p ~op ~loc ~style ~explode
    (_x : t_30748e2d12) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_e18674598e _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_30748e2d12)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_30748e2d12 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_e18674598e) in
        Option.map (fun _y : t_30748e2d12 -> T_e18674598e _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_30748e2d12 -> Ptime_t _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_c87881fc5c ~p ~op ~loc ~style ~explode
    (_x : t_c87881fc5c) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("address", begin match _x.address with | None -> `Null | Some _x ->
          begin match _x with
          | T_d2b7d5933e _x ->
            `ObjectN
              [("", `Any);
               ("city", begin match _x.city with | None -> `Null | Some _x ->
                `String end);
               ("country", begin match _x.country with | None -> `Null
                | Some _x -> `String end);
               ("line1", begin match _x.line1 with | None -> `Null
                | Some _x -> `String end);
               ("line2", begin match _x.line2 with | None -> `Null
                | Some _x -> `String end);
               ("postal_code", begin match _x.postal_code with
                | None -> `Null | Some _x -> `String end);
               ("state", begin match _x.state with | None -> `Null
                | Some _x -> `String end)]
          | T_24529d85b7 _x -> `String
          end end);
         ("shipping", begin match _x.shipping with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_d2075e2bfe _x ->
            `ObjectN
              [("", `Any);
               ("address", let _x = _x.address in
                `ObjectN
                  [("", `Any);
                   ("city", begin match _x.city with | None -> `Null
                    | Some _x -> `String end);
                   ("country", begin match _x.country with | None -> `Null
                    | Some _x -> `String end);
                   ("line1", begin match _x.line1 with | None -> `Null
                    | Some _x -> `String end);
                   ("line2", begin match _x.line2 with | None -> `Null
                    | Some _x -> `String end);
                   ("postal_code", begin match _x.postal_code with
                    | None -> `Null | Some _x -> `String end);
                   ("state", begin match _x.state with | None -> `Null
                    | Some _x -> `String end)]);
               ("name", let _x = _x.name in `String);
               ("phone", begin match _x.phone with | None -> `Null
                | Some _x -> `String end)]
          | T_6a6f426354 _x -> `String
          end end);
         ("tax", begin match _x.tax with | None -> `Null | Some _x ->
          `ObjectN
            [("", `Any);
             ("ip_address", begin match _x.ip_address with | None -> `Null
              | Some _x ->
              begin match _x with
              | String_ _x -> `String
              | T_ea916b88e8 _x -> `String
              end end)]
          end);
         ("tax_exempt", begin match _x.tax_exempt with | None -> `Null
          | Some _x -> `String end);
         ("tax_ids", begin match _x.tax_ids with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_88c656de84) ->
                `Singleton
                  (`ObjectN
                     [("", `Any); ("type", let _x = _x.type_ in `String);
                      ("value", let _x = _x.value in `String)])) _x))
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_c87881fc5c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c87881fc5c ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_c87881fc5c) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("address",
                 `Choice
                 [`ObjectN
                    [("", `Any); ("city", `String); ("country", `String);
                     ("line1", `String); ("line2", `String);
                     ("postal_code", `String); ("state", `String)];
                  `String]);
                ("shipping",
                 `Choice
                 [`ObjectN
                    [("", `Any);
                     ("address",
                      `ObjectN
                        [("", `Any); ("city", `String); ("country", `String);
                         ("line1", `String); ("line2", `String);
                         ("postal_code", `String); ("state", `String)]);
                     ("name", `String); ("phone", `String)];
                  `String]);
                ("tax",
                 `ObjectN
                   [("", `Any); ("ip_address", `Choice
                                               [`String; `String])]);
                ("tax_exempt", `String);
                ("tax_ids",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("type", `String); ("value", `String)]))])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_c87881fc5c ~p ~op ~loc ~style ~explode
    (_x : t_c87881fc5c) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("address", begin match _x.address with | None -> `Null | Some _x ->
          begin match _x with
          | T_d2b7d5933e _x ->
            `ObjectN
              [("", `Any);
               ("city", begin match _x.city with | None -> `Null | Some _x ->
                `String end);
               ("country", begin match _x.country with | None -> `Null
                | Some _x -> `String end);
               ("line1", begin match _x.line1 with | None -> `Null
                | Some _x -> `String end);
               ("line2", begin match _x.line2 with | None -> `Null
                | Some _x -> `String end);
               ("postal_code", begin match _x.postal_code with
                | None -> `Null | Some _x -> `String end);
               ("state", begin match _x.state with | None -> `Null
                | Some _x -> `String end)]
          | T_24529d85b7 _x -> `String
          end end);
         ("shipping", begin match _x.shipping with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_d2075e2bfe _x ->
            `ObjectN
              [("", `Any);
               ("address", let _x = _x.address in
                `ObjectN
                  [("", `Any);
                   ("city", begin match _x.city with | None -> `Null
                    | Some _x -> `String end);
                   ("country", begin match _x.country with | None -> `Null
                    | Some _x -> `String end);
                   ("line1", begin match _x.line1 with | None -> `Null
                    | Some _x -> `String end);
                   ("line2", begin match _x.line2 with | None -> `Null
                    | Some _x -> `String end);
                   ("postal_code", begin match _x.postal_code with
                    | None -> `Null | Some _x -> `String end);
                   ("state", begin match _x.state with | None -> `Null
                    | Some _x -> `String end)]);
               ("name", let _x = _x.name in `String);
               ("phone", begin match _x.phone with | None -> `Null
                | Some _x -> `String end)]
          | T_6a6f426354 _x -> `String
          end end);
         ("tax", begin match _x.tax with | None -> `Null | Some _x ->
          `ObjectN
            [("", `Any);
             ("ip_address", begin match _x.ip_address with | None -> `Null
              | Some _x ->
              begin match _x with
              | String_ _x -> `String
              | T_ea916b88e8 _x -> `String
              end end)]
          end);
         ("tax_exempt", begin match _x.tax_exempt with | None -> `Null
          | Some _x -> `String end);
         ("tax_ids", begin match _x.tax_ids with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_88c656de84) ->
                `Singleton
                  (`ObjectN
                     [("", `Any); ("type", let _x = _x.type_ in `String);
                      ("value", let _x = _x.value in `String)])) _x))
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_c87881fc5c)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c87881fc5c ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_c87881fc5c) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("address",
                 `Choice
                 [`ObjectN
                    [("", `Any); ("city", `String); ("country", `String);
                     ("line1", `String); ("line2", `String);
                     ("postal_code", `String); ("state", `String)];
                  `String]);
                ("shipping",
                 `Choice
                 [`ObjectN
                    [("", `Any);
                     ("address",
                      `ObjectN
                        [("", `Any); ("city", `String); ("country", `String);
                         ("line1", `String); ("line2", `String);
                         ("postal_code", `String); ("state", `String)]);
                     ("name", `String); ("phone", `String)];
                  `String]);
                ("tax",
                 `ObjectN
                   [("", `Any); ("ip_address", `Choice
                                               [`String; `String])]);
                ("tax_exempt", `String);
                ("tax_ids",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("type", `String); ("value", `String)]))])])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_873409613d ~p ~op ~loc ~style ~explode
    (_x : t_873409613d) =
    _string_of ~kind:(
      begin match _x with
      | T_31c19e5e08 _x ->
        `Array
          ((List.map (fun (_x : t_97829cf2e1) ->
              `Singleton
                (`ObjectN
                   [("", `Any);
                    ("coupon", begin match _x.coupon with | None -> `Null
                     | Some _x -> `String end);
                    ("discount", begin match _x.discount with | None -> `Null
                     | Some _x -> `String end);
                    ("promotion_code", begin match _x.promotion_code with
                     | None -> `Null | Some _x -> `String end)])) _x))
      | T_e023a69498 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_873409613d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_873409613d ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("coupon", `String);
                           ("discount", `String);
                           ("promotion_code", `String)]))] in
        let dtr = (Json_encoding.destruct Encoders'.t_31c19e5e08) in
        Option.map (fun _y : t_873409613d -> T_31c19e5e08 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_e023a69498) in
        Option.map (fun _y : t_873409613d -> T_e023a69498 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_873409613d ~p ~op ~loc ~style ~explode
    (_x : t_873409613d) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_31c19e5e08 _x ->
        `Array
          ((List.map (fun (_x : t_97829cf2e1) ->
              `Singleton
                (`ObjectN
                   [("", `Any);
                    ("coupon", begin match _x.coupon with | None -> `Null
                     | Some _x -> `String end);
                    ("discount", begin match _x.discount with | None -> `Null
                     | Some _x -> `String end);
                    ("promotion_code", begin match _x.promotion_code with
                     | None -> `Null | Some _x -> `String end)])) _x))
      | T_e023a69498 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_873409613d)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_873409613d ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("coupon", `String);
                           ("discount", `String);
                           ("promotion_code", `String)]))] in
        let dtr = (Json_encoding.destruct Encoders'.t_31c19e5e08) in
        Option.map (fun _y : t_873409613d -> T_31c19e5e08 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_e023a69498) in
        Option.map (fun _y : t_873409613d -> T_e023a69498 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_96382fbbc2 ~p ~op ~loc ~style ~explode
    (_x : t_96382fbbc2) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_e9893b909b) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("currency", begin match _x.currency with | None -> `Null
                   | Some _x -> `String end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("discountable", begin match _x.discountable with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_5d3f83cf9a _x ->
                     `Array
                       ((List.map (fun (_x : t_f4df748cbc) ->
                           `Singleton (`Null)) _x))
                   | T_a76752bf7d _x -> `String
                   end end);
                  ("invoiceitem", begin match _x.invoiceitem with
                   | None -> `Null | Some _x -> `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_2914026972 _x -> `ObjectN [("", `Null)]
                   | T_67827e70c9 _x -> `String
                   end end);
                  ("period", begin match _x.period with | None -> `Null
                   | Some _x ->
                   `ObjectN
                     [("", `Any); ("end", let _x = _x.end_ in `Integer);
                      ("start", let _x = _x.start in `Integer)]
                   end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_behavior", begin match _x.tax_behavior with
                   | None -> `Null | Some _x -> `String end);
                  ("tax_code", begin match _x.tax_code with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | String_ _x -> `String
                   | T_9f3605bc0e _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_1281decff3 _x -> `String
                   end end);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_96382fbbc2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_96382fbbc2 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_96382fbbc2) in
      _string_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("currency", `String); ("description", `String);
                       ("discountable", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("invoiceitem", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("period",
                        `ObjectN
                          [("", `Any); ("end", `Integer);
                           ("start", `Integer)]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String); ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer); ("tax_behavior", `String);
                       ("tax_code", `Choice
                                    [`String; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_96382fbbc2 ~p ~op ~loc ~style ~explode
    (_x : t_96382fbbc2) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_e9893b909b) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("currency", begin match _x.currency with | None -> `Null
                   | Some _x -> `String end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("discountable", begin match _x.discountable with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_5d3f83cf9a _x ->
                     `Array
                       ((List.map (fun (_x : t_f4df748cbc) ->
                           `Singleton (`Null)) _x))
                   | T_a76752bf7d _x -> `String
                   end end);
                  ("invoiceitem", begin match _x.invoiceitem with
                   | None -> `Null | Some _x -> `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_2914026972 _x -> `ObjectN [("", `Null)]
                   | T_67827e70c9 _x -> `String
                   end end);
                  ("period", begin match _x.period with | None -> `Null
                   | Some _x ->
                   `ObjectN
                     [("", `Any); ("end", let _x = _x.end_ in `Integer);
                      ("start", let _x = _x.start in `Integer)]
                   end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_behavior", begin match _x.tax_behavior with
                   | None -> `Null | Some _x -> `String end);
                  ("tax_code", begin match _x.tax_code with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | String_ _x -> `String
                   | T_9f3605bc0e _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_1281decff3 _x -> `String
                   end end);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_96382fbbc2)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_96382fbbc2 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_96382fbbc2) in
      _namevalues_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("currency", `String); ("description", `String);
                       ("discountable", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("invoiceitem", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("period",
                        `ObjectN
                          [("", `Any); ("end", `Integer);
                           ("start", `Integer)]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String); ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer); ("tax_behavior", `String);
                       ("tax_code", `Choice
                                    [`String; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_266682ce3a ~p ~op ~loc ~style ~explode
    (_x : t_266682ce3a) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("type", let _x = _x.type_ in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_266682ce3a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_266682ce3a ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_266682ce3a) in
      _string_to ~p
      ~kind:(`ObjectN [("", `Any); ("account", `String); ("type", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_266682ce3a ~p ~op ~loc ~style ~explode
    (_x : t_266682ce3a) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("type", let _x = _x.type_ in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_266682ce3a)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_266682ce3a ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_266682ce3a) in
      _namevalues_to ~p
      ~kind:(`ObjectN [("", `Any); ("account", `String); ("type", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_9aa5bd9e73 ~p ~op ~loc ~style ~explode
    (_x : t_9aa5bd9e73) =
    _string_of ~kind:(
      begin match _x with
      | String_ _x -> `String
      | T_037795d400 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_9aa5bd9e73)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9aa5bd9e73 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_9aa5bd9e73 -> String_ _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_037795d400) in
        Option.map (fun _y : t_9aa5bd9e73 -> T_037795d400 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_9aa5bd9e73 ~p ~op ~loc ~style ~explode
    (_x : t_9aa5bd9e73) =
    _namevalues_of ~kind:(
      begin match _x with
      | String_ _x -> `String
      | T_037795d400 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_9aa5bd9e73)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_9aa5bd9e73 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_9aa5bd9e73 -> String_ _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_037795d400) in
        Option.map (fun _y : t_9aa5bd9e73 -> T_037795d400 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_25cc6e6754 ~p ~op ~loc ~style ~explode
    (_x : t_25cc6e6754) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any); ("enabled", let _x = _x.enabled in `Boolean);
         ("liability", begin match _x.liability with | None -> `Null
          | Some _x ->
          `ObjectN
            [("", `Any);
             ("account", begin match _x.account with | None -> `Null
              | Some _x -> `String end);
             ("type", let _x = _x.type_ in `String)]
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_25cc6e6754)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_25cc6e6754 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_25cc6e6754) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("enabled", `Boolean);
                ("liability",
                 `ObjectN
                   [("", `Any); ("account", `String); ("type", `String)])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_25cc6e6754 ~p ~op ~loc ~style ~explode
    (_x : t_25cc6e6754) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any); ("enabled", let _x = _x.enabled in `Boolean);
         ("liability", begin match _x.liability with | None -> `Null
          | Some _x ->
          `ObjectN
            [("", `Any);
             ("account", begin match _x.account with | None -> `Null
              | Some _x -> `String end);
             ("type", let _x = _x.type_ in `String)]
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_25cc6e6754)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_25cc6e6754 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_25cc6e6754) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("enabled", `Boolean);
                ("liability",
                 `ObjectN
                   [("", `Any); ("account", `String); ("type", `String)])])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_d3294049d8 ~p ~op ~loc ~style ~explode
    (_x : t_d3294049d8) =
    _string_of ~kind:(
      begin match _x with
      | String_ _x -> `String
      | T_bf81051835 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_d3294049d8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d3294049d8 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_d3294049d8 -> String_ _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_bf81051835) in
        Option.map (fun _y : t_d3294049d8 -> T_bf81051835 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_d3294049d8 ~p ~op ~loc ~style ~explode
    (_x : t_d3294049d8) =
    _namevalues_of ~kind:(
      begin match _x with
      | String_ _x -> `String
      | T_bf81051835 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_d3294049d8)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_d3294049d8 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_d3294049d8 -> String_ _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_bf81051835) in
        Option.map (fun _y : t_d3294049d8 -> T_bf81051835 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_f90350482b ~p ~op ~loc ~style ~explode
    (_x : t_f90350482b) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f90350482b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f90350482b ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f90350482b) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f90350482b ~p ~op ~loc ~style ~explode
    (_x : t_f90350482b) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f90350482b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_f90350482b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_f90350482b) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_4592f6749b ~p ~op ~loc ~style ~explode
    (_x : t_4592f6749b) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("end_behavior", begin match _x.end_behavior with | None -> `Null
          | Some _x -> `String end);
         ("phases", begin match _x.phases with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_e083edf3b7) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("add_invoice_items",
                       begin match _x.add_invoice_items with | None -> `Null
                       | Some _x ->
                       `Array
                         ((List.map (fun (_x : t_09ff00d63c) ->
                             `Singleton (`Null)) _x))
                       end);
                      ("application_fee_percent",
                       begin match _x.application_fee_percent with
                       | None -> `Null | Some _x -> `Number end);
                      ("automatic_tax", begin match _x.automatic_tax with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("enabled", let _x = _x.enabled in `Null);
                          ("liability", begin match _x.liability with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("billing_cycle_anchor",
                       begin match _x.billing_cycle_anchor with
                       | None -> `Null | Some _x -> `String end);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_4a54b9013a _x -> `Null
                       | T_0303c42b80 _x -> `Null
                       end end);
                      ("collection_method",
                       begin match _x.collection_method with | None -> `Null
                       | Some _x -> `String end);
                      ("coupon", begin match _x.coupon with | None -> `Null
                       | Some _x -> `String end);
                      ("default_payment_method",
                       begin match _x.default_payment_method with
                       | None -> `Null | Some _x -> `String end);
                      ("default_tax_rates",
                       begin match _x.default_tax_rates with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_3bde3b5055 _x -> `Null
                       end end);
                      ("description", begin match _x.description with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | String_ _x -> `Null
                       | T_1fc7f5fe2a _x -> `Null
                       end end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_fcfd9a8504 _x -> `Null
                       | T_ec6ad958e1 _x -> `Null
                       end end);
                      ("end_date", begin match _x.end_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_1741fe7401 _x -> `Null
                       end end);
                      ("invoice_settings",
                       begin match _x.invoice_settings with | None -> `Null
                       | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("account_tax_ids",
                           begin match _x.account_tax_ids with
                           | None -> `Null | Some _x -> `Null end);
                          ("days_until_due",
                           begin match _x.days_until_due with | None -> `Null
                           | Some _x -> `Null end);
                          ("issuer", begin match _x.issuer with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("items", let _x = _x.items in
                       `Array
                         ((List.map (fun (_x : t_5c2bdfb2a5) ->
                             `Singleton (`Null)) _x)));
                      ("iterations", begin match _x.iterations with
                       | None -> `Null | Some _x -> `Integer end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x -> `ObjectN [("", `Null)]
                       end);
                      ("on_behalf_of", begin match _x.on_behalf_of with
                       | None -> `Null | Some _x -> `String end);
                      ("proration_behavior",
                       begin match _x.proration_behavior with | None -> `Null
                       | Some _x -> `String end);
                      ("start_date", begin match _x.start_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_fb19f83934 _x -> `Null
                       end end);
                      ("transfer_data", begin match _x.transfer_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("amount_percent",
                           begin match _x.amount_percent with | None -> `Null
                           | Some _x -> `Null end);
                          ("destination", let _x = _x.destination in `Null)]
                       end);
                      ("trial", begin match _x.trial with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("trial_end", begin match _x.trial_end with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_dec2d4e644 _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_4592f6749b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4592f6749b ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4592f6749b) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("end_behavior", `String);
                ("phases",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("add_invoice_items", `Array [(`List (`Null))]);
                           ("application_fee_percent", `Number);
                           ("automatic_tax",
                            `ObjectN
                              [("", `Null); ("enabled", `Null);
                               ("liability", `Null)]);
                           ("billing_cycle_anchor", `String);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("collection_method", `String);
                           ("coupon", `String);
                           ("default_payment_method", `String);
                           ("default_tax_rates", `Choice
                                                 [`Null; `Null]);
                           ("description", `Choice
                                           [`Null; `Null]);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("end_date", `Choice
                                        [`Null; `Null]);
                           ("invoice_settings",
                            `ObjectN
                              [("", `Null); ("account_tax_ids", `Null);
                               ("days_until_due", `Null); ("issuer", `Null)]);
                           ("items", `Array [(`List (`Null))]);
                           ("iterations", `Integer);
                           ("metadata", `ObjectN [("", `Null)]);
                           ("on_behalf_of", `String);
                           ("proration_behavior", `String);
                           ("start_date", `Choice
                                          [`Null; `Null]);
                           ("transfer_data",
                            `ObjectN
                              [("", `Null); ("amount_percent", `Null);
                               ("destination", `Null)]);
                           ("trial", `Boolean);
                           ("trial_end", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String)]) ~dtr ~loc ~style ~explode
      _x
  
  let namevalues_of_t_4592f6749b ~p ~op ~loc ~style ~explode
    (_x : t_4592f6749b) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("end_behavior", begin match _x.end_behavior with | None -> `Null
          | Some _x -> `String end);
         ("phases", begin match _x.phases with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_e083edf3b7) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("add_invoice_items",
                       begin match _x.add_invoice_items with | None -> `Null
                       | Some _x ->
                       `Array
                         ((List.map (fun (_x : t_09ff00d63c) ->
                             `Singleton (`Null)) _x))
                       end);
                      ("application_fee_percent",
                       begin match _x.application_fee_percent with
                       | None -> `Null | Some _x -> `Number end);
                      ("automatic_tax", begin match _x.automatic_tax with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("enabled", let _x = _x.enabled in `Null);
                          ("liability", begin match _x.liability with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("billing_cycle_anchor",
                       begin match _x.billing_cycle_anchor with
                       | None -> `Null | Some _x -> `String end);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_4a54b9013a _x -> `Null
                       | T_0303c42b80 _x -> `Null
                       end end);
                      ("collection_method",
                       begin match _x.collection_method with | None -> `Null
                       | Some _x -> `String end);
                      ("coupon", begin match _x.coupon with | None -> `Null
                       | Some _x -> `String end);
                      ("default_payment_method",
                       begin match _x.default_payment_method with
                       | None -> `Null | Some _x -> `String end);
                      ("default_tax_rates",
                       begin match _x.default_tax_rates with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_3bde3b5055 _x -> `Null
                       end end);
                      ("description", begin match _x.description with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | String_ _x -> `Null
                       | T_1fc7f5fe2a _x -> `Null
                       end end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_fcfd9a8504 _x -> `Null
                       | T_ec6ad958e1 _x -> `Null
                       end end);
                      ("end_date", begin match _x.end_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_1741fe7401 _x -> `Null
                       end end);
                      ("invoice_settings",
                       begin match _x.invoice_settings with | None -> `Null
                       | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("account_tax_ids",
                           begin match _x.account_tax_ids with
                           | None -> `Null | Some _x -> `Null end);
                          ("days_until_due",
                           begin match _x.days_until_due with | None -> `Null
                           | Some _x -> `Null end);
                          ("issuer", begin match _x.issuer with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("items", let _x = _x.items in
                       `Array
                         ((List.map (fun (_x : t_5c2bdfb2a5) ->
                             `Singleton (`Null)) _x)));
                      ("iterations", begin match _x.iterations with
                       | None -> `Null | Some _x -> `Integer end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x -> `ObjectN [("", `Null)]
                       end);
                      ("on_behalf_of", begin match _x.on_behalf_of with
                       | None -> `Null | Some _x -> `String end);
                      ("proration_behavior",
                       begin match _x.proration_behavior with | None -> `Null
                       | Some _x -> `String end);
                      ("start_date", begin match _x.start_date with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_fb19f83934 _x -> `Null
                       end end);
                      ("transfer_data", begin match _x.transfer_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("amount_percent",
                           begin match _x.amount_percent with | None -> `Null
                           | Some _x -> `Null end);
                          ("destination", let _x = _x.destination in `Null)]
                       end);
                      ("trial", begin match _x.trial with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("trial_end", begin match _x.trial_end with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | Ptime_t _x -> `Null
                       | T_dec2d4e644 _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_4592f6749b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_4592f6749b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_4592f6749b) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("end_behavior", `String);
                ("phases",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("add_invoice_items", `Array [(`List (`Null))]);
                           ("application_fee_percent", `Number);
                           ("automatic_tax",
                            `ObjectN
                              [("", `Null); ("enabled", `Null);
                               ("liability", `Null)]);
                           ("billing_cycle_anchor", `String);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("collection_method", `String);
                           ("coupon", `String);
                           ("default_payment_method", `String);
                           ("default_tax_rates", `Choice
                                                 [`Null; `Null]);
                           ("description", `Choice
                                           [`Null; `Null]);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("end_date", `Choice
                                        [`Null; `Null]);
                           ("invoice_settings",
                            `ObjectN
                              [("", `Null); ("account_tax_ids", `Null);
                               ("days_until_due", `Null); ("issuer", `Null)]);
                           ("items", `Array [(`List (`Null))]);
                           ("iterations", `Integer);
                           ("metadata", `ObjectN [("", `Null)]);
                           ("on_behalf_of", `String);
                           ("proration_behavior", `String);
                           ("start_date", `Choice
                                          [`Null; `Null]);
                           ("transfer_data",
                            `ObjectN
                              [("", `Null); ("amount_percent", `Null);
                               ("destination", `Null)]);
                           ("trial", `Boolean);
                           ("trial_end", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String)]) ~dtr ~loc ~style ~explode
      _x
  
  let string_of_t_ed69666899 ~p ~op ~loc ~style ~explode
    (_x : t_ed69666899) =
    _string_of ~kind:(
      begin match _x with
      | T_055dff077f _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ed69666899)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ed69666899 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_055dff077f) in
        Option.map (fun _y : t_ed69666899 -> T_055dff077f _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_ed69666899 -> Ptime_t _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_ed69666899 ~p ~op ~loc ~style ~explode
    (_x : t_ed69666899) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_055dff077f _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ed69666899)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ed69666899 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_055dff077f) in
        Option.map (fun _y : t_ed69666899 -> T_055dff077f _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_ed69666899 -> Ptime_t _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_1d44572454 ~p ~op ~loc ~style ~explode
    (_x : t_1d44572454) =
    _string_of ~kind:(
      begin match _x with
      | Ptime_t _x -> `Integer
      | T_0c25334bd1 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_1d44572454)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1d44572454 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_1d44572454 -> Ptime_t _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_0c25334bd1) in
        Option.map (fun _y : t_1d44572454 -> T_0c25334bd1 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_1d44572454 ~p ~op ~loc ~style ~explode
    (_x : t_1d44572454) =
    _namevalues_of ~kind:(
      begin match _x with
      | Ptime_t _x -> `Integer
      | T_0c25334bd1 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_1d44572454)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_1d44572454 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_1d44572454 -> Ptime_t _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_0c25334bd1) in
        Option.map (fun _y : t_1d44572454 -> T_0c25334bd1 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_8f5fa696e4 ~p ~op ~loc ~style ~explode
    (_x : t_8f5fa696e4) =
    _string_of ~kind:(
      begin match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_124a01eb97 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_8f5fa696e4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8f5fa696e4 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `Array [(`List (`String))] in
        let dtr = (Json_encoding.destruct
                     (Json_encoding.list Json_encoding.string)) in
        Option.map (fun _y : t_8f5fa696e4 -> StringList _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_124a01eb97) in
        Option.map (fun _y : t_8f5fa696e4 -> T_124a01eb97 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_8f5fa696e4 ~p ~op ~loc ~style ~explode
    (_x : t_8f5fa696e4) =
    _namevalues_of ~kind:(
      begin match _x with
      | StringList _x ->
        `Array ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
      | T_124a01eb97 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_8f5fa696e4)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_8f5fa696e4 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `Array [(`List (`String))] in
        let dtr = (Json_encoding.destruct
                     (Json_encoding.list Json_encoding.string)) in
        Option.map (fun _y : t_8f5fa696e4 -> StringList _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_124a01eb97) in
        Option.map (fun _y : t_8f5fa696e4 -> T_124a01eb97 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_7cca4f7dbf ~p ~op ~loc ~style ~explode
    (_x : t_7cca4f7dbf) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("billing_cycle_anchor", begin match _x.billing_cycle_anchor with
          | None -> `Null | Some _x ->
          begin match _x with
          | T_ac476c6fff _x -> `String
          | Ptime_t _x -> `Integer
          end end);
         ("cancel_at", begin match _x.cancel_at with | None -> `Null
          | Some _x ->
          begin match _x with
          | Ptime_t _x -> `Integer
          | T_c16049800d _x -> `String
          end end);
         ("cancel_at_period_end", begin match _x.cancel_at_period_end with
          | None -> `Null | Some _x -> `Boolean end);
         ("cancel_now", begin match _x.cancel_now with | None -> `Null
          | Some _x -> `Boolean end);
         ("default_tax_rates", begin match _x.default_tax_rates with
          | None -> `Null | Some _x ->
          begin match _x with
          | StringList _x ->
            `Array
              ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
          | T_5a615d7a6b _x -> `String
          end end);
         ("items", begin match _x.items with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_3c6c982289) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_4eadbfd4e1 _x -> `Null
                       | T_2571d9f0a2 _x -> `Null
                       end end);
                      ("clear_usage", begin match _x.clear_usage with
                       | None -> `Null | Some _x -> `Boolean end);
                      ("deleted", begin match _x.deleted with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_0f0fa94121 _x -> `Null
                       | T_47922f83c1 _x -> `Null
                       end end);
                      ("id", begin match _x.id with | None -> `Null
                       | Some _x -> `String end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_1b95510dbf _x -> `Null
                       | T_8ea0737343 _x -> `Null
                       end end);
                      ("price", begin match _x.price with | None -> `Null
                       | Some _x -> `String end);
                      ("price_data", begin match _x.price_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("currency", let _x = _x.currency in `Null);
                          ("product", let _x = _x.product in `Null);
                          ("recurring", let _x = _x.recurring in `Null);
                          ("tax_behavior", begin match _x.tax_behavior with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount", begin match _x.unit_amount with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount_decimal",
                           begin match _x.unit_amount_decimal with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("quantity", begin match _x.quantity with
                       | None -> `Null | Some _x -> `Integer end);
                      ("tax_rates", begin match _x.tax_rates with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_0527cbbae2 _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end);
         ("proration_date", begin match _x.proration_date with
          | None -> `Null | Some _x -> `Integer end);
         ("resume_at", begin match _x.resume_at with | None -> `Null
          | Some _x -> `String end);
         ("start_date", begin match _x.start_date with | None -> `Null
          | Some _x -> `Integer end);
         ("trial_end", begin match _x.trial_end with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_24f72174d8 _x -> `String
          | Ptime_t _x -> `Integer
          end end)]) ~ctr:(Json_encoding.construct Encoders'.t_7cca4f7dbf)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_7cca4f7dbf ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_7cca4f7dbf) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("billing_cycle_anchor", `Choice
                                         [`String; `Integer]);
                ("cancel_at", `Choice
                              [`Integer; `String]);
                ("cancel_at_period_end", `Boolean); ("cancel_now", `Boolean);
                ("default_tax_rates",
                 `Choice
                 [`Array [(`List (`String))]; `String]);
                ("items",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("clear_usage", `Boolean); ("deleted", `Boolean);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("id", `String);
                           ("metadata", `Choice
                                        [`Null; `Null]);
                           ("price", `String);
                           ("price_data",
                            `ObjectN
                              [("", `Null); ("currency", `Null);
                               ("product", `Null); ("recurring", `Null);
                               ("tax_behavior", `Null);
                               ("unit_amount", `Null);
                               ("unit_amount_decimal", `Null)]);
                           ("quantity", `Integer);
                           ("tax_rates", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String);
                ("proration_date", `Integer); ("resume_at", `String);
                ("start_date", `Integer);
                ("trial_end", `Choice
                              [`String; `Integer])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_7cca4f7dbf ~p ~op ~loc ~style ~explode
    (_x : t_7cca4f7dbf) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("billing_cycle_anchor", begin match _x.billing_cycle_anchor with
          | None -> `Null | Some _x ->
          begin match _x with
          | T_ac476c6fff _x -> `String
          | Ptime_t _x -> `Integer
          end end);
         ("cancel_at", begin match _x.cancel_at with | None -> `Null
          | Some _x ->
          begin match _x with
          | Ptime_t _x -> `Integer
          | T_c16049800d _x -> `String
          end end);
         ("cancel_at_period_end", begin match _x.cancel_at_period_end with
          | None -> `Null | Some _x -> `Boolean end);
         ("cancel_now", begin match _x.cancel_now with | None -> `Null
          | Some _x -> `Boolean end);
         ("default_tax_rates", begin match _x.default_tax_rates with
          | None -> `Null | Some _x ->
          begin match _x with
          | StringList _x ->
            `Array
              ((List.map (fun (_x : string) -> `Singleton (`String)) _x))
          | T_5a615d7a6b _x -> `String
          end end);
         ("items", begin match _x.items with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_3c6c982289) ->
                `Singleton
                  (`ObjectN
                     [("", `Any);
                      ("billing_thresholds",
                       begin match _x.billing_thresholds with | None -> `Null
                       | Some _x ->
                       begin match _x with
                       | T_4eadbfd4e1 _x -> `Null
                       | T_2571d9f0a2 _x -> `Null
                       end end);
                      ("clear_usage", begin match _x.clear_usage with
                       | None -> `Null | Some _x -> `Boolean end);
                      ("deleted", begin match _x.deleted with | None -> `Null
                       | Some _x -> `Boolean end);
                      ("discounts", begin match _x.discounts with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_0f0fa94121 _x -> `Null
                       | T_47922f83c1 _x -> `Null
                       end end);
                      ("id", begin match _x.id with | None -> `Null
                       | Some _x -> `String end);
                      ("metadata", begin match _x.metadata with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | T_1b95510dbf _x -> `Null
                       | T_8ea0737343 _x -> `Null
                       end end);
                      ("price", begin match _x.price with | None -> `Null
                       | Some _x -> `String end);
                      ("price_data", begin match _x.price_data with
                       | None -> `Null | Some _x ->
                       `ObjectN
                         [("", `Null);
                          ("currency", let _x = _x.currency in `Null);
                          ("product", let _x = _x.product in `Null);
                          ("recurring", let _x = _x.recurring in `Null);
                          ("tax_behavior", begin match _x.tax_behavior with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount", begin match _x.unit_amount with
                           | None -> `Null | Some _x -> `Null end);
                          ("unit_amount_decimal",
                           begin match _x.unit_amount_decimal with
                           | None -> `Null | Some _x -> `Null end)]
                       end);
                      ("quantity", begin match _x.quantity with
                       | None -> `Null | Some _x -> `Integer end);
                      ("tax_rates", begin match _x.tax_rates with
                       | None -> `Null | Some _x ->
                       begin match _x with
                       | StringList _x -> `Null
                       | T_0527cbbae2 _x -> `Null
                       end end)])) _x))
          end);
         ("proration_behavior", begin match _x.proration_behavior with
          | None -> `Null | Some _x -> `String end);
         ("proration_date", begin match _x.proration_date with
          | None -> `Null | Some _x -> `Integer end);
         ("resume_at", begin match _x.resume_at with | None -> `Null
          | Some _x -> `String end);
         ("start_date", begin match _x.start_date with | None -> `Null
          | Some _x -> `Integer end);
         ("trial_end", begin match _x.trial_end with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_24f72174d8 _x -> `String
          | Ptime_t _x -> `Integer
          end end)]) ~ctr:(Json_encoding.construct Encoders'.t_7cca4f7dbf)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_7cca4f7dbf ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_7cca4f7dbf) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("billing_cycle_anchor", `Choice
                                         [`String; `Integer]);
                ("cancel_at", `Choice
                              [`Integer; `String]);
                ("cancel_at_period_end", `Boolean); ("cancel_now", `Boolean);
                ("default_tax_rates",
                 `Choice
                 [`Array [(`List (`String))]; `String]);
                ("items",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any);
                           ("billing_thresholds", `Choice
                                                  [`Null; `Null]);
                           ("clear_usage", `Boolean); ("deleted", `Boolean);
                           ("discounts", `Choice
                                         [`Null; `Null]);
                           ("id", `String);
                           ("metadata", `Choice
                                        [`Null; `Null]);
                           ("price", `String);
                           ("price_data",
                            `ObjectN
                              [("", `Null); ("currency", `Null);
                               ("product", `Null); ("recurring", `Null);
                               ("tax_behavior", `Null);
                               ("unit_amount", `Null);
                               ("unit_amount_decimal", `Null)]);
                           ("quantity", `Integer);
                           ("tax_rates", `Choice
                                         [`Null; `Null])]))]);
                ("proration_behavior", `String);
                ("proration_date", `Integer); ("resume_at", `String);
                ("start_date", `Integer);
                ("trial_end", `Choice
                              [`String; `Integer])])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_1d8dcb799a ~p ~op ~loc ~style ~explode
    (_x : t_1d8dcb799a) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_1374dad90f) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("billing_thresholds",
                   begin match _x.billing_thresholds with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_ddb7e86f56 _x ->
                     `ObjectN
                       [("", `Null);
                        ("usage_gte", let _x = _x.usage_gte in `Null)]
                   | T_8d4566be9e _x -> `String
                   end end);
                  ("clear_usage", begin match _x.clear_usage with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("deleted", begin match _x.deleted with | None -> `Null
                   | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_578889ed69 _x ->
                     `Array
                       ((List.map (fun (_x : t_09f523f9f5) ->
                           `Singleton (`Null)) _x))
                   | T_849ac8a715 _x -> `String
                   end end);
                  ("id", begin match _x.id with | None -> `Null | Some _x ->
                   `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_d340f0bf3a _x -> `ObjectN [("", `Null)]
                   | T_fea0a35e16 _x -> `String
                   end end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("recurring", let _x = _x.recurring in
                       `ObjectN
                         [("", `Null);
                          ("interval", let _x = _x.interval in `Null);
                          ("interval_count",
                           begin match _x.interval_count with | None -> `Null
                           | Some _x -> `Null end)]);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_7582a29c79 _x -> `String
                   end end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_1d8dcb799a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1d8dcb799a ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_1d8dcb799a) in
      _string_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any);
                       ("billing_thresholds",
                        `Choice
                        [`ObjectN [("", `Null); ("usage_gte", `Null)];
                         `String]);
                       ("clear_usage", `Boolean); ("deleted", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("id", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String);
                           ("recurring",
                            `ObjectN
                              [("", `Null); ("interval", `Null);
                               ("interval_count", `Null)]);
                           ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_1d8dcb799a ~p ~op ~loc ~style ~explode
    (_x : t_1d8dcb799a) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_1374dad90f) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("billing_thresholds",
                   begin match _x.billing_thresholds with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_ddb7e86f56 _x ->
                     `ObjectN
                       [("", `Null);
                        ("usage_gte", let _x = _x.usage_gte in `Null)]
                   | T_8d4566be9e _x -> `String
                   end end);
                  ("clear_usage", begin match _x.clear_usage with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("deleted", begin match _x.deleted with | None -> `Null
                   | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_578889ed69 _x ->
                     `Array
                       ((List.map (fun (_x : t_09f523f9f5) ->
                           `Singleton (`Null)) _x))
                   | T_849ac8a715 _x -> `String
                   end end);
                  ("id", begin match _x.id with | None -> `Null | Some _x ->
                   `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_d340f0bf3a _x -> `ObjectN [("", `Null)]
                   | T_fea0a35e16 _x -> `String
                   end end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("recurring", let _x = _x.recurring in
                       `ObjectN
                         [("", `Null);
                          ("interval", let _x = _x.interval in `Null);
                          ("interval_count",
                           begin match _x.interval_count with | None -> `Null
                           | Some _x -> `Null end)]);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_7582a29c79 _x -> `String
                   end end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_1d8dcb799a)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_1d8dcb799a ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_1d8dcb799a) in
      _namevalues_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any);
                       ("billing_thresholds",
                        `Choice
                        [`ObjectN [("", `Null); ("usage_gte", `Null)];
                         `String]);
                       ("clear_usage", `Boolean); ("deleted", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("id", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String);
                           ("recurring",
                            `ObjectN
                              [("", `Null); ("interval", `Null);
                               ("interval_count", `Null)]);
                           ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String])]))])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_b3cdff625a ~p ~op ~loc ~style ~explode
    (_x : t_b3cdff625a) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b3cdff625a)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b3cdff625a ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b3cdff625a) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b3cdff625a ~p ~op ~loc ~style ~explode
    (_x : t_b3cdff625a) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b3cdff625a)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b3cdff625a ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_b3cdff625a) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_f0d6f3faa7 ~p ~op ~loc ~style ~explode
    (_x : t_f0d6f3faa7) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f0d6f3faa7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f0d6f3faa7 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f0d6f3faa7) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f0d6f3faa7 ~p ~op ~loc ~style ~explode
    (_x : t_f0d6f3faa7) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_f0d6f3faa7)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_f0d6f3faa7 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_f0d6f3faa7) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_8a9e68982d ~p ~op ~loc ~style ~explode
    (_x : t_8a9e68982d) =
    _string_of ~kind:(
      begin match _x with
      | T_b12bfcf7b3 _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8a9e68982d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8a9e68982d ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_b12bfcf7b3) in
        Option.map (fun _y : t_8a9e68982d -> T_b12bfcf7b3 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_8a9e68982d -> Ptime_t _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_8a9e68982d ~p ~op ~loc ~style ~explode
    (_x : t_8a9e68982d) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_b12bfcf7b3 _x -> `String
      | Ptime_t _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8a9e68982d)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_8a9e68982d ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_b12bfcf7b3) in
        Option.map (fun _y : t_8a9e68982d -> T_b12bfcf7b3 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct EncBase'.vendor_unix_time) in
        Option.map (fun _y : t_8a9e68982d -> Ptime_t _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_428991b112 ~p ~op ~loc ~style ~explode
    (_x : t_428991b112) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("address", begin match _x.address with | None -> `Null | Some _x ->
          begin match _x with
          | T_74b30847f1 _x ->
            `ObjectN
              [("", `Any);
               ("city", begin match _x.city with | None -> `Null | Some _x ->
                `String end);
               ("country", begin match _x.country with | None -> `Null
                | Some _x -> `String end);
               ("line1", begin match _x.line1 with | None -> `Null
                | Some _x -> `String end);
               ("line2", begin match _x.line2 with | None -> `Null
                | Some _x -> `String end);
               ("postal_code", begin match _x.postal_code with
                | None -> `Null | Some _x -> `String end);
               ("state", begin match _x.state with | None -> `Null
                | Some _x -> `String end)]
          | T_2e65d33e0b _x -> `String
          end end);
         ("shipping", begin match _x.shipping with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_743992c530 _x ->
            `ObjectN
              [("", `Any);
               ("address", let _x = _x.address in
                `ObjectN
                  [("", `Any);
                   ("city", begin match _x.city with | None -> `Null
                    | Some _x -> `String end);
                   ("country", begin match _x.country with | None -> `Null
                    | Some _x -> `String end);
                   ("line1", begin match _x.line1 with | None -> `Null
                    | Some _x -> `String end);
                   ("line2", begin match _x.line2 with | None -> `Null
                    | Some _x -> `String end);
                   ("postal_code", begin match _x.postal_code with
                    | None -> `Null | Some _x -> `String end);
                   ("state", begin match _x.state with | None -> `Null
                    | Some _x -> `String end)]);
               ("name", let _x = _x.name in `String);
               ("phone", begin match _x.phone with | None -> `Null
                | Some _x -> `String end)]
          | T_7287935059 _x -> `String
          end end);
         ("tax", begin match _x.tax with | None -> `Null | Some _x ->
          `ObjectN
            [("", `Any);
             ("ip_address", begin match _x.ip_address with | None -> `Null
              | Some _x ->
              begin match _x with
              | String_ _x -> `String
              | T_9dc7923852 _x -> `String
              end end)]
          end);
         ("tax_exempt", begin match _x.tax_exempt with | None -> `Null
          | Some _x -> `String end);
         ("tax_ids", begin match _x.tax_ids with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_dc0dec9e3d) ->
                `Singleton
                  (`ObjectN
                     [("", `Any); ("type", let _x = _x.type_ in `String);
                      ("value", let _x = _x.value in `String)])) _x))
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_428991b112)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_428991b112 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_428991b112) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("address",
                 `Choice
                 [`ObjectN
                    [("", `Any); ("city", `String); ("country", `String);
                     ("line1", `String); ("line2", `String);
                     ("postal_code", `String); ("state", `String)];
                  `String]);
                ("shipping",
                 `Choice
                 [`ObjectN
                    [("", `Any);
                     ("address",
                      `ObjectN
                        [("", `Any); ("city", `String); ("country", `String);
                         ("line1", `String); ("line2", `String);
                         ("postal_code", `String); ("state", `String)]);
                     ("name", `String); ("phone", `String)];
                  `String]);
                ("tax",
                 `ObjectN
                   [("", `Any); ("ip_address", `Choice
                                               [`String; `String])]);
                ("tax_exempt", `String);
                ("tax_ids",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("type", `String); ("value", `String)]))])])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_428991b112 ~p ~op ~loc ~style ~explode
    (_x : t_428991b112) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("address", begin match _x.address with | None -> `Null | Some _x ->
          begin match _x with
          | T_74b30847f1 _x ->
            `ObjectN
              [("", `Any);
               ("city", begin match _x.city with | None -> `Null | Some _x ->
                `String end);
               ("country", begin match _x.country with | None -> `Null
                | Some _x -> `String end);
               ("line1", begin match _x.line1 with | None -> `Null
                | Some _x -> `String end);
               ("line2", begin match _x.line2 with | None -> `Null
                | Some _x -> `String end);
               ("postal_code", begin match _x.postal_code with
                | None -> `Null | Some _x -> `String end);
               ("state", begin match _x.state with | None -> `Null
                | Some _x -> `String end)]
          | T_2e65d33e0b _x -> `String
          end end);
         ("shipping", begin match _x.shipping with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_743992c530 _x ->
            `ObjectN
              [("", `Any);
               ("address", let _x = _x.address in
                `ObjectN
                  [("", `Any);
                   ("city", begin match _x.city with | None -> `Null
                    | Some _x -> `String end);
                   ("country", begin match _x.country with | None -> `Null
                    | Some _x -> `String end);
                   ("line1", begin match _x.line1 with | None -> `Null
                    | Some _x -> `String end);
                   ("line2", begin match _x.line2 with | None -> `Null
                    | Some _x -> `String end);
                   ("postal_code", begin match _x.postal_code with
                    | None -> `Null | Some _x -> `String end);
                   ("state", begin match _x.state with | None -> `Null
                    | Some _x -> `String end)]);
               ("name", let _x = _x.name in `String);
               ("phone", begin match _x.phone with | None -> `Null
                | Some _x -> `String end)]
          | T_7287935059 _x -> `String
          end end);
         ("tax", begin match _x.tax with | None -> `Null | Some _x ->
          `ObjectN
            [("", `Any);
             ("ip_address", begin match _x.ip_address with | None -> `Null
              | Some _x ->
              begin match _x with
              | String_ _x -> `String
              | T_9dc7923852 _x -> `String
              end end)]
          end);
         ("tax_exempt", begin match _x.tax_exempt with | None -> `Null
          | Some _x -> `String end);
         ("tax_ids", begin match _x.tax_ids with | None -> `Null | Some _x ->
          `Array
            ((List.map (fun (_x : t_dc0dec9e3d) ->
                `Singleton
                  (`ObjectN
                     [("", `Any); ("type", let _x = _x.type_ in `String);
                      ("value", let _x = _x.value in `String)])) _x))
          end)]) ~ctr:(Json_encoding.construct Encoders'.t_428991b112)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_428991b112 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_428991b112) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("address",
                 `Choice
                 [`ObjectN
                    [("", `Any); ("city", `String); ("country", `String);
                     ("line1", `String); ("line2", `String);
                     ("postal_code", `String); ("state", `String)];
                  `String]);
                ("shipping",
                 `Choice
                 [`ObjectN
                    [("", `Any);
                     ("address",
                      `ObjectN
                        [("", `Any); ("city", `String); ("country", `String);
                         ("line1", `String); ("line2", `String);
                         ("postal_code", `String); ("state", `String)]);
                     ("name", `String); ("phone", `String)];
                  `String]);
                ("tax",
                 `ObjectN
                   [("", `Any); ("ip_address", `Choice
                                               [`String; `String])]);
                ("tax_exempt", `String);
                ("tax_ids",
                 `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("type", `String); ("value", `String)]))])])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_ec12d0adc5 ~p ~op ~loc ~style ~explode
    (_x : t_ec12d0adc5) =
    _string_of ~kind:(
      begin match _x with
      | T_ce45932982 _x ->
        `Array
          ((List.map (fun (_x : t_b90aee23ca) ->
              `Singleton
                (`ObjectN
                   [("", `Any);
                    ("coupon", begin match _x.coupon with | None -> `Null
                     | Some _x -> `String end);
                    ("discount", begin match _x.discount with | None -> `Null
                     | Some _x -> `String end);
                    ("promotion_code", begin match _x.promotion_code with
                     | None -> `Null | Some _x -> `String end)])) _x))
      | T_4884e1ec72 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_ec12d0adc5)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ec12d0adc5 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("coupon", `String);
                           ("discount", `String);
                           ("promotion_code", `String)]))] in
        let dtr = (Json_encoding.destruct Encoders'.t_ce45932982) in
        Option.map (fun _y : t_ec12d0adc5 -> T_ce45932982 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_4884e1ec72) in
        Option.map (fun _y : t_ec12d0adc5 -> T_4884e1ec72 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_ec12d0adc5 ~p ~op ~loc ~style ~explode
    (_x : t_ec12d0adc5) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_ce45932982 _x ->
        `Array
          ((List.map (fun (_x : t_b90aee23ca) ->
              `Singleton
                (`ObjectN
                   [("", `Any);
                    ("coupon", begin match _x.coupon with | None -> `Null
                     | Some _x -> `String end);
                    ("discount", begin match _x.discount with | None -> `Null
                     | Some _x -> `String end);
                    ("promotion_code", begin match _x.promotion_code with
                     | None -> `Null | Some _x -> `String end)])) _x))
      | T_4884e1ec72 _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_ec12d0adc5)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ec12d0adc5 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `Array
                   [(`List
                       (`ObjectN
                          [("", `Any); ("coupon", `String);
                           ("discount", `String);
                           ("promotion_code", `String)]))] in
        let dtr = (Json_encoding.destruct Encoders'.t_ce45932982) in
        Option.map (fun _y : t_ec12d0adc5 -> T_ce45932982 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_4884e1ec72) in
        Option.map (fun _y : t_ec12d0adc5 -> T_4884e1ec72 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_6de1e50279 ~p ~op ~loc ~style ~explode
    (_x : t_6de1e50279) =
    _string_of ~kind:(
      `Array
        ((List.map (fun (_x : t_2f6b36b766) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("currency", begin match _x.currency with | None -> `Null
                   | Some _x -> `String end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("discountable", begin match _x.discountable with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_6b1c6a8d52 _x ->
                     `Array
                       ((List.map (fun (_x : t_a39e51dc77) ->
                           `Singleton (`Null)) _x))
                   | T_251f58b73c _x -> `String
                   end end);
                  ("invoiceitem", begin match _x.invoiceitem with
                   | None -> `Null | Some _x -> `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_4fd7b468a5 _x -> `ObjectN [("", `Null)]
                   | T_b31a36a7b0 _x -> `String
                   end end);
                  ("period", begin match _x.period with | None -> `Null
                   | Some _x ->
                   `ObjectN
                     [("", `Any); ("end", let _x = _x.end_ in `Integer);
                      ("start", let _x = _x.start in `Integer)]
                   end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_behavior", begin match _x.tax_behavior with
                   | None -> `Null | Some _x -> `String end);
                  ("tax_code", begin match _x.tax_code with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | String_ _x -> `String
                   | T_71c3497b63 _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_754223ab79 _x -> `String
                   end end);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_6de1e50279)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6de1e50279 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_6de1e50279) in
      _string_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("currency", `String); ("description", `String);
                       ("discountable", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("invoiceitem", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("period",
                        `ObjectN
                          [("", `Any); ("end", `Integer);
                           ("start", `Integer)]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String); ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer); ("tax_behavior", `String);
                       ("tax_code", `Choice
                                    [`String; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_6de1e50279 ~p ~op ~loc ~style ~explode
    (_x : t_6de1e50279) =
    _namevalues_of ~kind:(
      `Array
        ((List.map (fun (_x : t_2f6b36b766) ->
            `Singleton
              (`ObjectN
                 [("", `Any);
                  ("amount", begin match _x.amount with | None -> `Null
                   | Some _x -> `Integer end);
                  ("currency", begin match _x.currency with | None -> `Null
                   | Some _x -> `String end);
                  ("description", begin match _x.description with
                   | None -> `Null | Some _x -> `String end);
                  ("discountable", begin match _x.discountable with
                   | None -> `Null | Some _x -> `Boolean end);
                  ("discounts", begin match _x.discounts with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_6b1c6a8d52 _x ->
                     `Array
                       ((List.map (fun (_x : t_a39e51dc77) ->
                           `Singleton (`Null)) _x))
                   | T_251f58b73c _x -> `String
                   end end);
                  ("invoiceitem", begin match _x.invoiceitem with
                   | None -> `Null | Some _x -> `String end);
                  ("metadata", begin match _x.metadata with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | T_4fd7b468a5 _x -> `ObjectN [("", `Null)]
                   | T_b31a36a7b0 _x -> `String
                   end end);
                  ("period", begin match _x.period with | None -> `Null
                   | Some _x ->
                   `ObjectN
                     [("", `Any); ("end", let _x = _x.end_ in `Integer);
                      ("start", let _x = _x.start in `Integer)]
                   end);
                  ("price", begin match _x.price with | None -> `Null
                   | Some _x -> `String end);
                  ("price_data", begin match _x.price_data with
                   | None -> `Null | Some _x ->
                   `ObjectN
                     [("", `Any);
                      ("currency", let _x = _x.currency in `String);
                      ("product", let _x = _x.product in `String);
                      ("tax_behavior", begin match _x.tax_behavior with
                       | None -> `Null | Some _x -> `String end);
                      ("unit_amount", begin match _x.unit_amount with
                       | None -> `Null | Some _x -> `Integer end);
                      ("unit_amount_decimal",
                       begin match _x.unit_amount_decimal with
                       | None -> `Null | Some _x -> `String end)]
                   end);
                  ("quantity", begin match _x.quantity with | None -> `Null
                   | Some _x -> `Integer end);
                  ("tax_behavior", begin match _x.tax_behavior with
                   | None -> `Null | Some _x -> `String end);
                  ("tax_code", begin match _x.tax_code with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | String_ _x -> `String
                   | T_71c3497b63 _x -> `String
                   end end);
                  ("tax_rates", begin match _x.tax_rates with | None -> `Null
                   | Some _x ->
                   begin match _x with
                   | StringList _x ->
                     `Array
                       ((List.map (fun (_x : string) -> `Singleton (`Null))
                           _x))
                   | T_754223ab79 _x -> `String
                   end end);
                  ("unit_amount", begin match _x.unit_amount with
                   | None -> `Null | Some _x -> `Integer end);
                  ("unit_amount_decimal",
                   begin match _x.unit_amount_decimal with | None -> `Null
                   | Some _x -> `String end)])) _x)))
      ~ctr:(Json_encoding.construct Encoders'.t_6de1e50279)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_6de1e50279 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_6de1e50279) in
      _namevalues_to ~p
      ~kind:(`Array
               [(`List
                   (`ObjectN
                      [("", `Any); ("amount", `Integer);
                       ("currency", `String); ("description", `String);
                       ("discountable", `Boolean);
                       ("discounts",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("invoiceitem", `String);
                       ("metadata",
                        `Choice
                        [`ObjectN [("", `Null)]; `String]);
                       ("period",
                        `ObjectN
                          [("", `Any); ("end", `Integer);
                           ("start", `Integer)]);
                       ("price", `String);
                       ("price_data",
                        `ObjectN
                          [("", `Any); ("currency", `String);
                           ("product", `String); ("tax_behavior", `String);
                           ("unit_amount", `Integer);
                           ("unit_amount_decimal", `String)]);
                       ("quantity", `Integer); ("tax_behavior", `String);
                       ("tax_code", `Choice
                                    [`String; `String]);
                       ("tax_rates",
                        `Choice
                        [`Array [(`List (`Null))]; `String]);
                       ("unit_amount", `Integer);
                       ("unit_amount_decimal", `String)]))])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_f2803c8215 ~p ~op ~loc ~style ~explode
    (_x : t_f2803c8215) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("type", let _x = _x.type_ in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_f2803c8215)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_f2803c8215 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_f2803c8215) in
      _string_to ~p
      ~kind:(`ObjectN [("", `Any); ("account", `String); ("type", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_f2803c8215 ~p ~op ~loc ~style ~explode
    (_x : t_f2803c8215) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("type", let _x = _x.type_ in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_f2803c8215)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_f2803c8215 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_f2803c8215) in
      _namevalues_to ~p
      ~kind:(`ObjectN [("", `Any); ("account", `String); ("type", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_28b874eafa ~p ~op ~loc ~style ~explode
    (_x : t_28b874eafa) =
    _string_of ~kind:(
      begin match _x with
      | T_aa2791e997 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_28b874eafa)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_28b874eafa ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_aa2791e997) in
        Option.map (fun _y : t_28b874eafa -> T_aa2791e997 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_28b874eafa -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_28b874eafa ~p ~op ~loc ~style ~explode
    (_x : t_28b874eafa) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_aa2791e997 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_28b874eafa)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_28b874eafa ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_aa2791e997) in
        Option.map (fun _y : t_28b874eafa -> T_aa2791e997 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_28b874eafa -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_e7c047a717 ~p ~op ~loc ~style ~explode
    (_x : t_e7c047a717) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e7c047a717)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e7c047a717 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e7c047a717) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e7c047a717 ~p ~op ~loc ~style ~explode
    (_x : t_e7c047a717) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e7c047a717)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e7c047a717 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_e7c047a717) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_5e23dd8315 ~p ~op ~loc ~style ~explode
    (_x : t_5e23dd8315) =
    _string_of ~kind:(
      begin match _x with
      | T_6da4b6eaaf _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_5e23dd8315)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_5e23dd8315 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6da4b6eaaf) in
        Option.map (fun _y : t_5e23dd8315 -> T_6da4b6eaaf _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_5e23dd8315 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_5e23dd8315 ~p ~op ~loc ~style ~explode
    (_x : t_5e23dd8315) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_6da4b6eaaf _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_5e23dd8315)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_5e23dd8315 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6da4b6eaaf) in
        Option.map (fun _y : t_5e23dd8315 -> T_6da4b6eaaf _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_5e23dd8315 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_d05018810d ~p ~op ~loc ~style ~explode
    (_x : t_d05018810d) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_d05018810d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d05018810d ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_d05018810d) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_d05018810d ~p ~op ~loc ~style ~explode
    (_x : t_d05018810d) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_d05018810d)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_d05018810d ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_d05018810d) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_9b139b4e87 ~p ~op ~loc ~style ~explode
    (_x : t_9b139b4e87) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9b139b4e87)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9b139b4e87 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_9b139b4e87) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_9b139b4e87 ~p ~op ~loc ~style ~explode
    (_x : t_9b139b4e87) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9b139b4e87)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_9b139b4e87 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_9b139b4e87) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_fd32e6450f ~p ~op ~loc ~style ~explode
    (_x : t_fd32e6450f) =
    _string_of ~kind:(
      begin match _x with
      | T_c2905ff31f _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_fd32e6450f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fd32e6450f ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_c2905ff31f) in
        Option.map (fun _y : t_fd32e6450f -> T_c2905ff31f _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_fd32e6450f -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_fd32e6450f ~p ~op ~loc ~style ~explode
    (_x : t_fd32e6450f) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_c2905ff31f _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_fd32e6450f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_fd32e6450f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_c2905ff31f) in
        Option.map (fun _y : t_fd32e6450f -> T_c2905ff31f _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_fd32e6450f -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_e2b1b56eac ~p ~op ~loc ~style ~explode
    (_x : t_e2b1b56eac) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e2b1b56eac)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e2b1b56eac ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e2b1b56eac) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e2b1b56eac ~p ~op ~loc ~style ~explode
    (_x : t_e2b1b56eac) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e2b1b56eac)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e2b1b56eac ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_e2b1b56eac) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_e42a3414c1 ~p ~op ~loc ~style ~explode
    (_x : t_e42a3414c1) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e42a3414c1)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e42a3414c1 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e42a3414c1) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e42a3414c1 ~p ~op ~loc ~style ~explode
    (_x : t_e42a3414c1) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e42a3414c1)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e42a3414c1 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_e42a3414c1) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_be86c12b19 ~p ~op ~loc ~style ~explode
    (_x : t_be86c12b19) =
    _string_of ~kind:(
      begin match _x with
      | T_acdb658c9d _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_be86c12b19)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_be86c12b19 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_acdb658c9d) in
        Option.map (fun _y : t_be86c12b19 -> T_acdb658c9d _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_be86c12b19 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_be86c12b19 ~p ~op ~loc ~style ~explode
    (_x : t_be86c12b19) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_acdb658c9d _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_be86c12b19)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_be86c12b19 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_acdb658c9d) in
        Option.map (fun _y : t_be86c12b19 -> T_acdb658c9d _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_be86c12b19 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_4b1fd7d313 ~p ~op ~loc ~style ~explode
    (_x : t_4b1fd7d313) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4b1fd7d313)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_4b1fd7d313 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_4b1fd7d313) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_4b1fd7d313 ~p ~op ~loc ~style ~explode
    (_x : t_4b1fd7d313) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_4b1fd7d313)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_4b1fd7d313 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_4b1fd7d313) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_6565ec2878 ~p ~op ~loc ~style ~explode
    (_x : t_6565ec2878) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("is_default", begin match _x.is_default with | None -> `Null
          | Some _x -> `Boolean end);
         ("is_platform_default", begin match _x.is_platform_default with
          | None -> `Null | Some _x -> `Boolean end)])
      ~ctr:(Json_encoding.construct Encoders'.t_6565ec2878)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6565ec2878 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_6565ec2878) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("is_default", `Boolean);
                ("is_platform_default", `Boolean)]) ~dtr ~loc ~style ~explode
      _x
  
  let namevalues_of_t_6565ec2878 ~p ~op ~loc ~style ~explode
    (_x : t_6565ec2878) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("is_default", begin match _x.is_default with | None -> `Null
          | Some _x -> `Boolean end);
         ("is_platform_default", begin match _x.is_platform_default with
          | None -> `Null | Some _x -> `Boolean end)])
      ~ctr:(Json_encoding.construct Encoders'.t_6565ec2878)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_6565ec2878 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_6565ec2878) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("is_default", `Boolean);
                ("is_platform_default", `Boolean)]) ~dtr ~loc ~style ~explode
      _x
  
  let string_of_t_386a8a09fc ~p ~op ~loc ~style ~explode
    (_x : t_386a8a09fc) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_386a8a09fc)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_386a8a09fc ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_386a8a09fc) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_386a8a09fc ~p ~op ~loc ~style ~explode
    (_x : t_386a8a09fc) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_386a8a09fc)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_386a8a09fc ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_386a8a09fc) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_3d82a38285 ~p ~op ~loc ~style ~explode
    (_x : t_3d82a38285) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_3d82a38285)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_3d82a38285 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_3d82a38285) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_3d82a38285 ~p ~op ~loc ~style ~explode
    (_x : t_3d82a38285) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_3d82a38285)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_3d82a38285 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_3d82a38285) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_00d4956c80 ~p ~op ~loc ~style ~explode
    (_x : t_00d4956c80) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_00d4956c80)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_00d4956c80 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_00d4956c80) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_00d4956c80 ~p ~op ~loc ~style ~explode
    (_x : t_00d4956c80) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_00d4956c80)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_00d4956c80 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_00d4956c80) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_a985a64df2 ~p ~op ~loc ~style ~explode
    (_x : t_a985a64df2) =
    _string_of ~kind:(
      begin match _x with
      | T_b6181859f2 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_a985a64df2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a985a64df2 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_b6181859f2) in
        Option.map (fun _y : t_a985a64df2 -> T_b6181859f2 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_a985a64df2 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_a985a64df2 ~p ~op ~loc ~style ~explode
    (_x : t_a985a64df2) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_b6181859f2 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_a985a64df2)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_a985a64df2 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_b6181859f2) in
        Option.map (fun _y : t_a985a64df2 -> T_b6181859f2 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_a985a64df2 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_9463e4703f ~p ~op ~loc ~style ~explode
    (_x : t_9463e4703f) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9463e4703f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9463e4703f ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_9463e4703f) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_9463e4703f ~p ~op ~loc ~style ~explode
    (_x : t_9463e4703f) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_9463e4703f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_9463e4703f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_9463e4703f) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_a6ed41322f ~p ~op ~loc ~style ~explode
    (_x : t_a6ed41322f) =
    _string_of ~kind:(
      begin match _x with
      | T_0bcc6214c1 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_a6ed41322f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a6ed41322f ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_0bcc6214c1) in
        Option.map (fun _y : t_a6ed41322f -> T_0bcc6214c1 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_a6ed41322f -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_a6ed41322f ~p ~op ~loc ~style ~explode
    (_x : t_a6ed41322f) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_0bcc6214c1 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_a6ed41322f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_a6ed41322f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_0bcc6214c1) in
        Option.map (fun _y : t_a6ed41322f -> T_0bcc6214c1 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_a6ed41322f -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_a13fab6ce7 ~p ~op ~loc ~style ~explode
    (_x : t_a13fab6ce7) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a13fab6ce7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a13fab6ce7 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_a13fab6ce7) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_a13fab6ce7 ~p ~op ~loc ~style ~explode
    (_x : t_a13fab6ce7) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a13fab6ce7)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_a13fab6ce7 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_a13fab6ce7) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_89676accde ~p ~op ~loc ~style ~explode
    (_x : t_89676accde) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("customer", begin match _x.customer with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_89676accde)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_89676accde ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_89676accde) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("account", `String); ("customer", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_89676accde ~p ~op ~loc ~style ~explode
    (_x : t_89676accde) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("customer", begin match _x.customer with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_89676accde)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_89676accde ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_89676accde) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("account", `String); ("customer", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_94e066c621 ~p ~op ~loc ~style ~explode
    (_x : t_94e066c621) =
    _string_of ~kind:(
      begin match _x with
      | T_92de2c55be _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_94e066c621)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_94e066c621 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_92de2c55be) in
        Option.map (fun _y : t_94e066c621 -> T_92de2c55be _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_94e066c621 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_94e066c621 ~p ~op ~loc ~style ~explode
    (_x : t_94e066c621) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_92de2c55be _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_94e066c621)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_94e066c621 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_92de2c55be) in
        Option.map (fun _y : t_94e066c621 -> T_92de2c55be _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_94e066c621 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_41b2207b76 ~p ~op ~loc ~style ~explode
    (_x : t_41b2207b76) =
    _string_of ~kind:(
      begin match _x with
      | String_ _x -> `String
      | T_15b592bd4c _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_41b2207b76)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_41b2207b76 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_41b2207b76 -> String_ _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_15b592bd4c) in
        Option.map (fun _y : t_41b2207b76 -> T_15b592bd4c _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_41b2207b76 ~p ~op ~loc ~style ~explode
    (_x : t_41b2207b76) =
    _namevalues_of ~kind:(
      begin match _x with
      | String_ _x -> `String
      | T_15b592bd4c _x -> `String
      end) ~ctr:(Json_encoding.construct Encoders'.t_41b2207b76)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_41b2207b76 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `String in
        let dtr = (Json_encoding.destruct Json_encoding.string) in
        Option.map (fun _y : t_41b2207b76 -> String_ _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `String in
        let dtr = (Json_encoding.destruct Encoders'.t_15b592bd4c) in
        Option.map (fun _y : t_41b2207b76 -> T_15b592bd4c _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_735ee27343 ~p ~op ~loc ~style ~explode
    (_x : t_735ee27343) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_735ee27343)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_735ee27343 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_735ee27343) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_735ee27343 ~p ~op ~loc ~style ~explode
    (_x : t_735ee27343) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_735ee27343)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_735ee27343 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_735ee27343) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_dfda23638e ~p ~op ~loc ~style ~explode
    (_x : t_dfda23638e) =
    _string_of ~kind:(
      begin match _x with
      | T_f942927e0c _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_dfda23638e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_dfda23638e ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f942927e0c) in
        Option.map (fun _y : t_dfda23638e -> T_f942927e0c _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_dfda23638e -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_dfda23638e ~p ~op ~loc ~style ~explode
    (_x : t_dfda23638e) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_f942927e0c _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_dfda23638e)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_dfda23638e ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f942927e0c) in
        Option.map (fun _y : t_dfda23638e -> T_f942927e0c _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_dfda23638e -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_78f6837d46 ~p ~op ~loc ~style ~explode
    (_x : t_78f6837d46) =
    _string_of ~kind:(
      begin match _x with
      | T_fb7e928310 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_78f6837d46)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_78f6837d46 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_fb7e928310) in
        Option.map (fun _y : t_78f6837d46 -> T_fb7e928310 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_78f6837d46 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_78f6837d46 ~p ~op ~loc ~style ~explode
    (_x : t_78f6837d46) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_fb7e928310 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_78f6837d46)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_78f6837d46 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_fb7e928310) in
        Option.map (fun _y : t_78f6837d46 -> T_fb7e928310 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_78f6837d46 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_c85fb118c6 ~p ~op ~loc ~style ~explode
    (_x : t_c85fb118c6) =
    _string_of ~kind:(
      begin match _x with
      | T_918279e85e _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c85fb118c6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c85fb118c6 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_918279e85e) in
        Option.map (fun _y : t_c85fb118c6 -> T_918279e85e _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c85fb118c6 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_c85fb118c6 ~p ~op ~loc ~style ~explode
    (_x : t_c85fb118c6) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_918279e85e _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c85fb118c6)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c85fb118c6 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_918279e85e) in
        Option.map (fun _y : t_c85fb118c6 -> T_918279e85e _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c85fb118c6 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_5c423aae2d ~p ~op ~loc ~style ~explode
    (_x : t_5c423aae2d) =
    _string_of ~kind:(
      begin match _x with
      | T_156f1957fc _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_5c423aae2d)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_5c423aae2d ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_156f1957fc) in
        Option.map (fun _y : t_5c423aae2d -> T_156f1957fc _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_5c423aae2d -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_5c423aae2d ~p ~op ~loc ~style ~explode
    (_x : t_5c423aae2d) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_156f1957fc _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_5c423aae2d)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_5c423aae2d ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_156f1957fc) in
        Option.map (fun _y : t_5c423aae2d -> T_156f1957fc _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_5c423aae2d -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_806c738ca8 ~p ~op ~loc ~style ~explode
    (_x : t_806c738ca8) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_806c738ca8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_806c738ca8 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_806c738ca8) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_806c738ca8 ~p ~op ~loc ~style ~explode
    (_x : t_806c738ca8) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_806c738ca8)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_806c738ca8 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_806c738ca8) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_54d3503cdb ~p ~op ~loc ~style ~explode
    (_x : t_54d3503cdb) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("interval", begin match _x.interval with | None -> `Null
          | Some _x -> `String end);
         ("meter", begin match _x.meter with | None -> `Null | Some _x ->
          `String end);
         ("usage_type", begin match _x.usage_type with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_54d3503cdb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_54d3503cdb ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_54d3503cdb) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("interval", `String); ("meter", `String);
                ("usage_type", `String)]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_54d3503cdb ~p ~op ~loc ~style ~explode
    (_x : t_54d3503cdb) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("interval", begin match _x.interval with | None -> `Null
          | Some _x -> `String end);
         ("meter", begin match _x.meter with | None -> `Null | Some _x ->
          `String end);
         ("usage_type", begin match _x.usage_type with | None -> `Null
          | Some _x -> `String end)])
      ~ctr:(Json_encoding.construct Encoders'.t_54d3503cdb)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_54d3503cdb ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_54d3503cdb) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("interval", `String); ("meter", `String);
                ("usage_type", `String)]) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_c04b129744 ~p ~op ~loc ~style ~explode
    (_x : t_c04b129744) =
    _string_of ~kind:(
      begin match _x with
      | T_818f6dfe63 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c04b129744)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c04b129744 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_818f6dfe63) in
        Option.map (fun _y : t_c04b129744 -> T_818f6dfe63 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c04b129744 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_c04b129744 ~p ~op ~loc ~style ~explode
    (_x : t_c04b129744) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_818f6dfe63 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c04b129744)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c04b129744 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_818f6dfe63) in
        Option.map (fun _y : t_c04b129744 -> T_818f6dfe63 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c04b129744 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_1d3358b59f ~p ~op ~loc ~style ~explode
    (_x : t_1d3358b59f) =
    _string_of ~kind:(
      begin match _x with
      | T_5a93111bd9 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_1d3358b59f)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1d3358b59f ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_5a93111bd9) in
        Option.map (fun _y : t_1d3358b59f -> T_5a93111bd9 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_1d3358b59f -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_1d3358b59f ~p ~op ~loc ~style ~explode
    (_x : t_1d3358b59f) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_5a93111bd9 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_1d3358b59f)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_1d3358b59f ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_5a93111bd9) in
        Option.map (fun _y : t_1d3358b59f -> T_5a93111bd9 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_1d3358b59f -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_b2c88d22a6 ~p ~op ~loc ~style ~explode
    (_x : t_b2c88d22a6) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b2c88d22a6)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b2c88d22a6 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b2c88d22a6) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b2c88d22a6 ~p ~op ~loc ~style ~explode
    (_x : t_b2c88d22a6) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_b2c88d22a6)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b2c88d22a6 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_b2c88d22a6) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_902cd52d55 ~p ~op ~loc ~style ~explode
    (_x : t_902cd52d55) =
    _string_of ~kind:(
      begin match _x with
      | T_0942bf866d _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_902cd52d55)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_902cd52d55 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_0942bf866d) in
        Option.map (fun _y : t_902cd52d55 -> T_0942bf866d _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_902cd52d55 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_902cd52d55 ~p ~op ~loc ~style ~explode
    (_x : t_902cd52d55) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_0942bf866d _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_902cd52d55)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_902cd52d55 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_0942bf866d) in
        Option.map (fun _y : t_902cd52d55 -> T_0942bf866d _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_902cd52d55 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_438f8e3e47 ~p ~op ~loc ~style ~explode
    (_x : t_438f8e3e47) =
    _string_of ~kind:(
      begin match _x with
      | T_6c02505772 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_438f8e3e47)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_438f8e3e47 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6c02505772) in
        Option.map (fun _y : t_438f8e3e47 -> T_6c02505772 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_438f8e3e47 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_438f8e3e47 ~p ~op ~loc ~style ~explode
    (_x : t_438f8e3e47) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_6c02505772 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_438f8e3e47)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_438f8e3e47 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_6c02505772) in
        Option.map (fun _y : t_438f8e3e47 -> T_6c02505772 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_438f8e3e47 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_b205fb898b ~p ~op ~loc ~style ~explode
    (_x : t_b205fb898b) =
    _string_of ~kind:(
      begin match _x with
      | T_f99ca180bb _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_b205fb898b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b205fb898b ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f99ca180bb) in
        Option.map (fun _y : t_b205fb898b -> T_f99ca180bb _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_b205fb898b -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_b205fb898b ~p ~op ~loc ~style ~explode
    (_x : t_b205fb898b) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_f99ca180bb _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_b205fb898b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b205fb898b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f99ca180bb) in
        Option.map (fun _y : t_b205fb898b -> T_f99ca180bb _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_b205fb898b -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_c862988285 ~p ~op ~loc ~style ~explode
    (_x : t_c862988285) =
    _string_of ~kind:(
      begin match _x with
      | T_a7f5f95f2b _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c862988285)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c862988285 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_a7f5f95f2b) in
        Option.map (fun _y : t_c862988285 -> T_a7f5f95f2b _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c862988285 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_c862988285 ~p ~op ~loc ~style ~explode
    (_x : t_c862988285) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_a7f5f95f2b _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c862988285)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c862988285 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_a7f5f95f2b) in
        Option.map (fun _y : t_c862988285 -> T_a7f5f95f2b _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c862988285 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_95b7b73ff8 ~p ~op ~loc ~style ~explode
    (_x : t_95b7b73ff8) =
    _string_of ~kind:(
      begin match _x with
      | T_3e044be4e2 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_95b7b73ff8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_95b7b73ff8 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_3e044be4e2) in
        Option.map (fun _y : t_95b7b73ff8 -> T_3e044be4e2 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_95b7b73ff8 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_95b7b73ff8 ~p ~op ~loc ~style ~explode
    (_x : t_95b7b73ff8) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_3e044be4e2 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_95b7b73ff8)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_95b7b73ff8 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_3e044be4e2) in
        Option.map (fun _y : t_95b7b73ff8 -> T_3e044be4e2 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_95b7b73ff8 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_91991d11ce ~p ~op ~loc ~style ~explode
    (_x : t_91991d11ce) =
    _string_of ~kind:(
      begin match _x with
      | T_d267e559df _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_91991d11ce)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_91991d11ce ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_d267e559df) in
        Option.map (fun _y : t_91991d11ce -> T_d267e559df _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_91991d11ce -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_91991d11ce ~p ~op ~loc ~style ~explode
    (_x : t_91991d11ce) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_d267e559df _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_91991d11ce)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_91991d11ce ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_d267e559df) in
        Option.map (fun _y : t_91991d11ce -> T_d267e559df _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_91991d11ce -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_62ebe57aa0 ~p ~op ~loc ~style ~explode
    (_x : t_62ebe57aa0) =
    _string_of ~kind:(
      begin match _x with
      | T_c8869a1e90 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_62ebe57aa0)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_62ebe57aa0 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_c8869a1e90) in
        Option.map (fun _y : t_62ebe57aa0 -> T_c8869a1e90 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_62ebe57aa0 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_62ebe57aa0 ~p ~op ~loc ~style ~explode
    (_x : t_62ebe57aa0) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_c8869a1e90 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_62ebe57aa0)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_62ebe57aa0 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_c8869a1e90) in
        Option.map (fun _y : t_62ebe57aa0 -> T_c8869a1e90 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_62ebe57aa0 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_47dcc46a23 ~p ~op ~loc ~style ~explode
    (_x : t_47dcc46a23) =
    _string_of ~kind:(
      begin match _x with
      | T_0a007f4c7b _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_47dcc46a23)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_47dcc46a23 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_0a007f4c7b) in
        Option.map (fun _y : t_47dcc46a23 -> T_0a007f4c7b _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_47dcc46a23 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_47dcc46a23 ~p ~op ~loc ~style ~explode
    (_x : t_47dcc46a23) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_0a007f4c7b _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_47dcc46a23)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_47dcc46a23 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_0a007f4c7b) in
        Option.map (fun _y : t_47dcc46a23 -> T_0a007f4c7b _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_47dcc46a23 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_3115fde6ed ~p ~op ~loc ~style ~explode
    (_x : t_3115fde6ed) =
    _string_of ~kind:(
      begin match _x with
      | T_8e372f4d99 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_3115fde6ed)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_3115fde6ed ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8e372f4d99) in
        Option.map (fun _y : t_3115fde6ed -> T_8e372f4d99 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_3115fde6ed -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_3115fde6ed ~p ~op ~loc ~style ~explode
    (_x : t_3115fde6ed) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_8e372f4d99 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_3115fde6ed)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_3115fde6ed ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8e372f4d99) in
        Option.map (fun _y : t_3115fde6ed -> T_8e372f4d99 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_3115fde6ed -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_edb9c04aba ~p ~op ~loc ~style ~explode
    (_x : t_edb9c04aba) =
    _string_of ~kind:(
      begin match _x with
      | T_8dfd931249 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_edb9c04aba)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_edb9c04aba ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8dfd931249) in
        Option.map (fun _y : t_edb9c04aba -> T_8dfd931249 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_edb9c04aba -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_edb9c04aba ~p ~op ~loc ~style ~explode
    (_x : t_edb9c04aba) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_8dfd931249 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_edb9c04aba)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_edb9c04aba ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8dfd931249) in
        Option.map (fun _y : t_edb9c04aba -> T_8dfd931249 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_edb9c04aba -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_b053f4a10e ~p ~op ~loc ~style ~explode
    (_x : t_b053f4a10e) =
    _string_of ~kind:(
      begin match _x with
      | T_7e5129cd97 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_b053f4a10e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b053f4a10e ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_7e5129cd97) in
        Option.map (fun _y : t_b053f4a10e -> T_7e5129cd97 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_b053f4a10e -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_b053f4a10e ~p ~op ~loc ~style ~explode
    (_x : t_b053f4a10e) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_7e5129cd97 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_b053f4a10e)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b053f4a10e ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_7e5129cd97) in
        Option.map (fun _y : t_b053f4a10e -> T_7e5129cd97 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_b053f4a10e -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_8dd946adeb ~p ~op ~loc ~style ~explode
    (_x : t_8dd946adeb) =
    _string_of ~kind:(
      begin match _x with
      | T_704964c184 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8dd946adeb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_8dd946adeb ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_704964c184) in
        Option.map (fun _y : t_8dd946adeb -> T_704964c184 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8dd946adeb -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_8dd946adeb ~p ~op ~loc ~style ~explode
    (_x : t_8dd946adeb) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_704964c184 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_8dd946adeb)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_8dd946adeb ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_704964c184) in
        Option.map (fun _y : t_8dd946adeb -> T_704964c184 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_8dd946adeb -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_e65a202262 ~p ~op ~loc ~style ~explode
    (_x : t_e65a202262) =
    _string_of ~kind:(
      begin match _x with
      | T_153437e623 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_e65a202262)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e65a202262 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_153437e623) in
        Option.map (fun _y : t_e65a202262 -> T_153437e623 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e65a202262 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_e65a202262 ~p ~op ~loc ~style ~explode
    (_x : t_e65a202262) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_153437e623 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_e65a202262)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e65a202262 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_153437e623) in
        Option.map (fun _y : t_e65a202262 -> T_153437e623 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_e65a202262 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_b1188f80a4 ~p ~op ~loc ~style ~explode
    (_x : t_b1188f80a4) =
    _string_of ~kind:(
      `ObjectN [("", `Any); ("enabled", let _x = _x.enabled in `Boolean)])
      ~ctr:(Json_encoding.construct Encoders'.t_b1188f80a4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b1188f80a4 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b1188f80a4) in
      _string_to ~p ~kind:(`ObjectN [("", `Any); ("enabled", `Boolean)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b1188f80a4 ~p ~op ~loc ~style ~explode
    (_x : t_b1188f80a4) =
    _namevalues_of ~kind:(
      `ObjectN [("", `Any); ("enabled", let _x = _x.enabled in `Boolean)])
      ~ctr:(Json_encoding.construct Encoders'.t_b1188f80a4)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b1188f80a4 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_b1188f80a4) in
      _namevalues_to ~p ~kind:(`ObjectN [("", `Any); ("enabled", `Boolean)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_d2098cac25 ~p ~op ~loc ~style ~explode
    (_x : t_d2098cac25) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_d2098cac25)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_d2098cac25 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_d2098cac25) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_d2098cac25 ~p ~op ~loc ~style ~explode
    (_x : t_d2098cac25) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_d2098cac25)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_d2098cac25 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_d2098cac25) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_a6b80cd539 ~p ~op ~loc ~style ~explode
    (_x : t_a6b80cd539) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a6b80cd539)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_a6b80cd539 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_a6b80cd539) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_a6b80cd539 ~p ~op ~loc ~style ~explode
    (_x : t_a6b80cd539) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_a6b80cd539)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_a6b80cd539 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_a6b80cd539) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_900f55e0e4 ~p ~op ~loc ~style ~explode
    (_x : t_900f55e0e4) =
    _string_of ~kind:(
      begin match _x with
      | T_c53ac2a9cd _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_900f55e0e4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_900f55e0e4 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_c53ac2a9cd) in
        Option.map (fun _y : t_900f55e0e4 -> T_c53ac2a9cd _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_900f55e0e4 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_900f55e0e4 ~p ~op ~loc ~style ~explode
    (_x : t_900f55e0e4) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_c53ac2a9cd _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_900f55e0e4)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_900f55e0e4 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_c53ac2a9cd) in
        Option.map (fun _y : t_900f55e0e4 -> T_c53ac2a9cd _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_900f55e0e4 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_dad4a4ac5b ~p ~op ~loc ~style ~explode
    (_x : t_dad4a4ac5b) =
    _string_of ~kind:(
      begin match _x with
      | T_d1eb4a98f5 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_dad4a4ac5b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_dad4a4ac5b ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_d1eb4a98f5) in
        Option.map (fun _y : t_dad4a4ac5b -> T_d1eb4a98f5 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_dad4a4ac5b -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_dad4a4ac5b ~p ~op ~loc ~style ~explode
    (_x : t_dad4a4ac5b) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_d1eb4a98f5 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_dad4a4ac5b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_dad4a4ac5b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_d1eb4a98f5) in
        Option.map (fun _y : t_dad4a4ac5b -> T_d1eb4a98f5 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_dad4a4ac5b -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_dce85b0bb2 ~p ~op ~loc ~style ~explode
    (_x : t_dce85b0bb2) =
    _string_of ~kind:(
      begin match _x with
      | T_f89b48aae9 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_dce85b0bb2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_dce85b0bb2 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f89b48aae9) in
        Option.map (fun _y : t_dce85b0bb2 -> T_f89b48aae9 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_dce85b0bb2 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_dce85b0bb2 ~p ~op ~loc ~style ~explode
    (_x : t_dce85b0bb2) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_f89b48aae9 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_dce85b0bb2)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_dce85b0bb2 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f89b48aae9) in
        Option.map (fun _y : t_dce85b0bb2 -> T_f89b48aae9 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_dce85b0bb2 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_66eb6371e3 ~p ~op ~loc ~style ~explode
    (_x : t_66eb6371e3) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_66eb6371e3)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_66eb6371e3 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_66eb6371e3) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_66eb6371e3 ~p ~op ~loc ~style ~explode
    (_x : t_66eb6371e3) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_66eb6371e3)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_66eb6371e3 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_66eb6371e3) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_17690afaaf ~p ~op ~loc ~style ~explode
    (_x : t_17690afaaf) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("customer", begin match _x.customer with | None -> `Null
          | Some _x -> `String end);
         ("type", let _x = _x.type_ in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_17690afaaf)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_17690afaaf ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_17690afaaf) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("account", `String); ("customer", `String);
                ("type", `String)]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_17690afaaf ~p ~op ~loc ~style ~explode
    (_x : t_17690afaaf) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("account", begin match _x.account with | None -> `Null | Some _x ->
          `String end);
         ("customer", begin match _x.customer with | None -> `Null
          | Some _x -> `String end);
         ("type", let _x = _x.type_ in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_17690afaaf)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_17690afaaf ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_17690afaaf) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any); ("account", `String); ("customer", `String);
                ("type", `String)]) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_ef1df523db ~p ~op ~loc ~style ~explode
    (_x : t_ef1df523db) =
    _string_of ~kind:(
      begin match _x with
      | T_81c4215133 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ef1df523db)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ef1df523db ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_81c4215133) in
        Option.map (fun _y : t_ef1df523db -> T_81c4215133 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ef1df523db -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_ef1df523db ~p ~op ~loc ~style ~explode
    (_x : t_ef1df523db) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_81c4215133 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ef1df523db)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ef1df523db ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_81c4215133) in
        Option.map (fun _y : t_ef1df523db -> T_81c4215133 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ef1df523db -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_477c159e47 ~p ~op ~loc ~style ~explode
    (_x : t_477c159e47) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_477c159e47)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_477c159e47 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_477c159e47) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_477c159e47 ~p ~op ~loc ~style ~explode
    (_x : t_477c159e47) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_477c159e47)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_477c159e47 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_477c159e47) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_76a6cf8994 ~p ~op ~loc ~style ~explode
    (_x : t_76a6cf8994) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_76a6cf8994)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_76a6cf8994 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_76a6cf8994) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_76a6cf8994 ~p ~op ~loc ~style ~explode
    (_x : t_76a6cf8994) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_76a6cf8994)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_76a6cf8994 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_76a6cf8994) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_6684cf5aa7 ~p ~op ~loc ~style ~explode
    (_x : t_6684cf5aa7) =
    _string_of ~kind:(
      begin match _x with
      | T_1149c5a72c _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_6684cf5aa7)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6684cf5aa7 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_1149c5a72c) in
        Option.map (fun _y : t_6684cf5aa7 -> T_1149c5a72c _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_6684cf5aa7 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_6684cf5aa7 ~p ~op ~loc ~style ~explode
    (_x : t_6684cf5aa7) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_1149c5a72c _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_6684cf5aa7)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_6684cf5aa7 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_1149c5a72c) in
        Option.map (fun _y : t_6684cf5aa7 -> T_1149c5a72c _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_6684cf5aa7 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_ca787dca43 ~p ~op ~loc ~style ~explode
    (_x : t_ca787dca43) =
    _string_of ~kind:(
      begin match _x with
      | T_5a45966f13 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ca787dca43)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ca787dca43 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_5a45966f13) in
        Option.map (fun _y : t_ca787dca43 -> T_5a45966f13 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ca787dca43 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_ca787dca43 ~p ~op ~loc ~style ~explode
    (_x : t_ca787dca43) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_5a45966f13 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_ca787dca43)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ca787dca43 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_5a45966f13) in
        Option.map (fun _y : t_ca787dca43 -> T_5a45966f13 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_ca787dca43 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_ce1d711154 ~p ~op ~loc ~style ~explode
    (_x : t_ce1d711154) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ce1d711154)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ce1d711154 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_ce1d711154) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_ce1d711154 ~p ~op ~loc ~style ~explode
    (_x : t_ce1d711154) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ce1d711154)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ce1d711154 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_ce1d711154) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_c5c22bba1c ~p ~op ~loc ~style ~explode
    (_x : t_c5c22bba1c) =
    _string_of ~kind:(
      begin match _x with
      | T_5b06d5fa4f _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c5c22bba1c)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_c5c22bba1c ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_5b06d5fa4f) in
        Option.map (fun _y : t_c5c22bba1c -> T_5b06d5fa4f _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c5c22bba1c -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_c5c22bba1c ~p ~op ~loc ~style ~explode
    (_x : t_c5c22bba1c) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_5b06d5fa4f _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_c5c22bba1c)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_c5c22bba1c ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_5b06d5fa4f) in
        Option.map (fun _y : t_c5c22bba1c -> T_5b06d5fa4f _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_c5c22bba1c -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_de06274f51 ~p ~op ~loc ~style ~explode
    (_x : t_de06274f51) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_de06274f51)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_de06274f51 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_de06274f51) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_de06274f51 ~p ~op ~loc ~style ~explode
    (_x : t_de06274f51) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_de06274f51)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_de06274f51 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_de06274f51) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_ed1d428d1e ~p ~op ~loc ~style ~explode
    (_x : t_ed1d428d1e) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ed1d428d1e)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ed1d428d1e ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_ed1d428d1e) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_ed1d428d1e ~p ~op ~loc ~style ~explode
    (_x : t_ed1d428d1e) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ed1d428d1e)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ed1d428d1e ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_ed1d428d1e) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_e27a5a13e2 ~p ~op ~loc ~style ~explode
    (_x : t_e27a5a13e2) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e27a5a13e2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e27a5a13e2 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e27a5a13e2) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e27a5a13e2 ~p ~op ~loc ~style ~explode
    (_x : t_e27a5a13e2) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e27a5a13e2)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e27a5a13e2 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_e27a5a13e2) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_9096085c5b ~p ~op ~loc ~style ~explode
    (_x : t_9096085c5b) =
    _string_of ~kind:(
      begin match _x with
      | T_8bc900449d _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_9096085c5b)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_9096085c5b ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8bc900449d) in
        Option.map (fun _y : t_9096085c5b -> T_8bc900449d _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9096085c5b -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_9096085c5b ~p ~op ~loc ~style ~explode
    (_x : t_9096085c5b) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_8bc900449d _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_9096085c5b)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_9096085c5b ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8bc900449d) in
        Option.map (fun _y : t_9096085c5b -> T_8bc900449d _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_9096085c5b -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_bda61eb198 ~p ~op ~loc ~style ~explode
    (_x : t_bda61eb198) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_bda61eb198)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_bda61eb198 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_bda61eb198) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_bda61eb198 ~p ~op ~loc ~style ~explode
    (_x : t_bda61eb198) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_bda61eb198)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_bda61eb198 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_bda61eb198) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_fd0b444db8 ~p ~op ~loc ~style ~explode
    (_x : t_fd0b444db8) =
    _string_of ~kind:(
      begin match _x with
      | T_34d236a9e5 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_fd0b444db8)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fd0b444db8 ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_34d236a9e5) in
        Option.map (fun _y : t_fd0b444db8 -> T_34d236a9e5 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_fd0b444db8 -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_fd0b444db8 ~p ~op ~loc ~style ~explode
    (_x : t_fd0b444db8) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_34d236a9e5 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_fd0b444db8)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_fd0b444db8 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_34d236a9e5) in
        Option.map (fun _y : t_fd0b444db8 -> T_34d236a9e5 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_fd0b444db8 -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_e134f021a2 ~p ~op ~loc ~style ~explode
    (_x : t_e134f021a2) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e134f021a2)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_e134f021a2 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_e134f021a2) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_e134f021a2 ~p ~op ~loc ~style ~explode
    (_x : t_e134f021a2) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_e134f021a2)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_e134f021a2 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_e134f021a2) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_7e023d3347 ~p ~op ~loc ~style ~explode
    (_x : t_7e023d3347) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_7e023d3347)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_7e023d3347 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_7e023d3347) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_7e023d3347 ~p ~op ~loc ~style ~explode
    (_x : t_7e023d3347) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_7e023d3347)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_7e023d3347 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_7e023d3347) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_99ae8c4576 ~p ~op ~loc ~style ~explode
    (_x : t_99ae8c4576) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("source_flow_type", let _x = _x.source_flow_type in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_99ae8c4576)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_99ae8c4576 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_99ae8c4576) in
      _string_to ~p
      ~kind:(`ObjectN [("", `Any); ("source_flow_type", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_99ae8c4576 ~p ~op ~loc ~style ~explode
    (_x : t_99ae8c4576) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("source_flow_type", let _x = _x.source_flow_type in `String)])
      ~ctr:(Json_encoding.construct Encoders'.t_99ae8c4576)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_99ae8c4576 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_99ae8c4576) in
      _namevalues_to ~p
      ~kind:(`ObjectN [("", `Any); ("source_flow_type", `String)])
      ~dtr ~loc ~style ~explode _x
  
  let string_of_t_ae96c50be4 ~p ~op ~loc ~style ~explode
    (_x : t_ae96c50be4) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ae96c50be4)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_ae96c50be4 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_ae96c50be4) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_ae96c50be4 ~p ~op ~loc ~style ~explode
    (_x : t_ae96c50be4) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_ae96c50be4)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_ae96c50be4 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_ae96c50be4) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_fe86a832fb ~p ~op ~loc ~style ~explode
    (_x : t_fe86a832fb) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_fe86a832fb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fe86a832fb ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_fe86a832fb) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_fe86a832fb ~p ~op ~loc ~style ~explode
    (_x : t_fe86a832fb) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_fe86a832fb)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_fe86a832fb ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_fe86a832fb) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_01243047ea ~p ~op ~loc ~style ~explode
    (_x : t_01243047ea) =
    _string_of ~kind:(
      begin match _x with
      | T_8a5740cf7a _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_01243047ea)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_01243047ea ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8a5740cf7a) in
        Option.map (fun _y : t_01243047ea -> T_8a5740cf7a _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_01243047ea -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_01243047ea ~p ~op ~loc ~style ~explode
    (_x : t_01243047ea) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_8a5740cf7a _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_01243047ea)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_01243047ea ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_8a5740cf7a) in
        Option.map (fun _y : t_01243047ea -> T_8a5740cf7a _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_01243047ea -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_fa33b5a2bd ~p ~op ~loc ~style ~explode
    (_x : t_fa33b5a2bd) =
    _string_of ~kind:(
      begin match _x with
      | T_99d78357ad _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_fa33b5a2bd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_fa33b5a2bd ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_99d78357ad) in
        Option.map (fun _y : t_fa33b5a2bd -> T_99d78357ad _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_fa33b5a2bd -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_fa33b5a2bd ~p ~op ~loc ~style ~explode
    (_x : t_fa33b5a2bd) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_99d78357ad _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_fa33b5a2bd)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_fa33b5a2bd ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_99d78357ad) in
        Option.map (fun _y : t_fa33b5a2bd -> T_99d78357ad _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_fa33b5a2bd -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_1f08681071 ~p ~op ~loc ~style ~explode
    (_x : t_1f08681071) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_1f08681071)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_1f08681071 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_1f08681071) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_1f08681071 ~p ~op ~loc ~style ~explode
    (_x : t_1f08681071) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_1f08681071)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_1f08681071 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_1f08681071) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_2bb01eccbd ~p ~op ~loc ~style ~explode
    (_x : t_2bb01eccbd) =
    _string_of ~kind:(
      begin match _x with
      | T_f34b1e7619 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_2bb01eccbd)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_2bb01eccbd ~p ~loc ~style ~explode (_x : string) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f34b1e7619) in
        Option.map (fun _y : t_2bb01eccbd -> T_f34b1e7619 _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_2bb01eccbd -> Int _y)
          (_string_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let namevalues_of_t_2bb01eccbd ~p ~op ~loc ~style ~explode
    (_x : t_2bb01eccbd) =
    _namevalues_of ~kind:(
      begin match _x with
      | T_f34b1e7619 _x ->
        `ObjectN
          [("", `Any);
           ("gt", begin match _x.gt with | None -> `Null | Some _x ->
            `Integer end);
           ("gte", begin match _x.gte with | None -> `Null | Some _x ->
            `Integer end);
           ("lt", begin match _x.lt with | None -> `Null | Some _x ->
            `Integer end);
           ("lte", begin match _x.lte with | None -> `Null | Some _x ->
            `Integer end)]
      | Int _x -> `Integer
      end) ~ctr:(Json_encoding.construct Encoders'.t_2bb01eccbd)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_2bb01eccbd ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    [(let kind = `ObjectN
                   [("", `Any); ("gt", `Integer); ("gte", `Integer);
                    ("lt", `Integer); ("lte", `Integer)] in
        let dtr = (Json_encoding.destruct Encoders'.t_f34b1e7619) in
        Option.map (fun _y : t_2bb01eccbd -> T_f34b1e7619 _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x));
     (let kind = `Integer in
        let dtr = (Json_encoding.destruct Json_encoding.int) in
        Option.map (fun _y : t_2bb01eccbd -> Int _y)
          (_namevalues_to ~p ~kind ~dtr ~loc ~style ~explode _x))]
      |> List.find_map Fun.id
  
  let string_of_t_65e58eb6da ~p ~op ~loc ~style ~explode
    (_x : t_65e58eb6da) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_65e58eb6da)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_65e58eb6da ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_65e58eb6da) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_65e58eb6da ~p ~op ~loc ~style ~explode
    (_x : t_65e58eb6da) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_65e58eb6da)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_65e58eb6da ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_65e58eb6da) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_6f04380d09 ~p ~op ~loc ~style ~explode
    (_x : t_6f04380d09) =
    _string_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6f04380d09)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_6f04380d09 ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_6f04380d09) in
      _string_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_6f04380d09 ~p ~op ~loc ~style ~explode
    (_x : t_6f04380d09) =
    _namevalues_of ~kind:( `String)
      ~ctr:(Json_encoding.construct Encoders'.t_6f04380d09)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_6f04380d09 ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_6f04380d09) in
      _namevalues_to ~p ~kind:(`String) ~dtr ~loc ~style ~explode _x
  
  let string_of_t_b458f1b6fb ~p ~op ~loc ~style ~explode
    (_x : t_b458f1b6fb) =
    _string_of ~kind:(
      `ObjectN
        [("", `Any);
         ("posted_at", begin match _x.posted_at with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_9dc26ca392 _x ->
            `ObjectN
              [("", `Any);
               ("gt", begin match _x.gt with | None -> `Null | Some _x ->
                `Integer end);
               ("gte", begin match _x.gte with | None -> `Null | Some _x ->
                `Integer end);
               ("lt", begin match _x.lt with | None -> `Null | Some _x ->
                `Integer end);
               ("lte", begin match _x.lte with | None -> `Null | Some _x ->
                `Integer end)]
          | Int _x -> `Integer
          end end)]) ~ctr:(Json_encoding.construct Encoders'.t_b458f1b6fb)
      ~p ~op ~loc ~style ~explode _x
  
  let string_to_t_b458f1b6fb ~p ~loc ~style ~explode (_x : string) =
    let dtr = (Json_encoding.destruct Encoders'.t_b458f1b6fb) in
      _string_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("posted_at",
                 `Choice
                 [`ObjectN
                    [("", `Any); ("gt", `Integer); ("gte", `Integer);
                     ("lt", `Integer); ("lte", `Integer)];
                  `Integer])]) ~dtr ~loc ~style ~explode _x
  
  let namevalues_of_t_b458f1b6fb ~p ~op ~loc ~style ~explode
    (_x : t_b458f1b6fb) =
    _namevalues_of ~kind:(
      `ObjectN
        [("", `Any);
         ("posted_at", begin match _x.posted_at with | None -> `Null
          | Some _x ->
          begin match _x with
          | T_9dc26ca392 _x ->
            `ObjectN
              [("", `Any);
               ("gt", begin match _x.gt with | None -> `Null | Some _x ->
                `Integer end);
               ("gte", begin match _x.gte with | None -> `Null | Some _x ->
                `Integer end);
               ("lt", begin match _x.lt with | None -> `Null | Some _x ->
                `Integer end);
               ("lte", begin match _x.lte with | None -> `Null | Some _x ->
                `Integer end)]
          | Int _x -> `Integer
          end end)]) ~ctr:(Json_encoding.construct Encoders'.t_b458f1b6fb)
      ~p ~op ~loc ~style ~explode _x
  
  let namevalues_to_t_b458f1b6fb ~p ~loc ~style ~explode
    (_x : (string*string) list) =
    let dtr = (Json_encoding.destruct Encoders'.t_b458f1b6fb) in
      _namevalues_to ~p
      ~kind:(`ObjectN
               [("", `Any);
                ("posted_at",
                 `Choice
                 [`ObjectN
                    [("", `Any); ("gt", `Integer); ("gte", `Integer);
                     ("lt", `Integer); ("lte", `Integer)];
                  `Integer])]) ~dtr ~loc ~style ~explode _x
  
  
end