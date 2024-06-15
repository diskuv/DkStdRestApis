module type AGENT = sig
  type 'a thread = [`Curl_command of string | `Never of 'a]

  val execute :
       [ `Headers of (string * string) list
       | `HttpVersion_1_1
       | `Method of
         [`DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT | `TRACE]
       | `Path of string
       | `RequestBody of string
       | `Server of string ]
       list
    -> ( [> `Headers of (string * string) list
         | `HttpStatus of int
         | `ResponseBody of string ]
         list
       , [> `Invalid_agent_attribute of string
         | `Nonconforming_response of string * string option
         | `Unparseable_response of string * ((int * int) * (int * int)) option
         ] )
       result
       thread

  val bind : 'a thread -> ('a -> 'b thread) -> 'b thread

  val return : 'c -> 'c thread
end

(** [create_agent ?server_url ?bearer ?headers ()] creates a
    web agent which returns an [Curl command] with the [command]
    containing the curl command line that replicates the request arguments.
    No real web request is ever made and the [Never _] response is never
    given back. *)
let create_agent ?server_url ?cacerts ?verbose ?bearer ?(headers = []) () =
  let initial_headers =
    (* Content-Type and Accept set by curl --json. *)
    Http.Header.of_list [("user-agent", "DkStdRestApis/0.4")]
  in
  let initial_headers = Http.Header.add_list initial_headers headers in
  let initial_headers =
    match bearer with
    | Some b ->
        Http.Header.add initial_headers "authorization" ("Bearer " ^ b)
    | None ->
        initial_headers
  in
  (* End-user error! Unrecoverable without fixing code. *)
  let exception Invalid_agent_attribute of string in
  let of_ref str r =
    match !r with Some v -> v | None -> raise (Invalid_agent_attribute str)
  in
  let execute attributes =
    let current_headers = ref initial_headers in
    let current_server : string option ref = ref server_url in
    let current_path : string option ref = ref None in
    (* Subset of Http.Method.t. No [`CONNECT] and no [`Other of string] *)
    let current_method :
        [`DELETE | `GET | `HEAD | `OPTIONS | `PATCH | `POST | `PUT | `TRACE]
        option
        ref =
      ref None
    in
    let current_request_body : string option ref = ref None in
    let args0 = ref [] in
    List.iter
      (function
        | `Headers headers ->
            current_headers := Http.Header.add_list !current_headers headers
        | `Server url ->
            current_server := Some url
        | `Path path ->
            current_path := Some path
        | `Method method_ ->
            current_method := Some method_
        | `RequestBody body ->
            current_request_body := Some body
        | `HttpVersion_1_1 ->
            (* The HTTP version is not part of the OpenAPI spec, but need to
               let VENDOR modules define it so that REST services like AWS SES
               that need HTTP 1.1 can operate. *)
            args0 := "--http1.1" :: !args0
        | _ ->
            () )
      attributes ;
    let uri =
      of_ref "Missing `Server" current_server
      ^ of_ref "Missing `Path" current_path
    in
    let meth : Http.Method.t =
      (of_ref "Missing `Method" current_method :> Http.Method.t)
    in
    (* Setup formatter *)
    let buf = Buffer.create 1024 in
    let ppf = Format.formatter_of_buffer buf in
    let br ppf () =
      (* When there is a break in the line, add a [ \] at
         the end of the line. Otherwise add a space character. *)
      Format.pp_print_custom_break ~fits:("", 1, "") ~breaks:(" \\", 2, "") ppf
    in
    (* Queue up the GET/POST/etc. command *)
    let pre = Queue.create () in
    let main = Queue.create () in
    let pre_a v = Queue.add v pre in
    let a v = Queue.add v main in
    (* [curl] *)
    a (`S "curl") ;
    (* [https://...] *)
    a (`Q uri) ;
    (* [-v] *)
    ( match verbose with
    | Some 1 ->
        a (`S "-v")
    | Some level when level >= 2 ->
        a (`S "-v") ;
        a (`S "--trace-ascii -")
    | _ ->
        () ) ;
    (* [-H ...] *)
    Http.Header.iter
      (fun n v ->
        (* try to keep [-H] and [name: value] together for aesthetics.
           Use a box b/c it is ok (especially if line would wrap)
           -H and 'name: value'. *)
        let nv = Format.asprintf "%s: %s" n v in
        a
          (`P
            ( (fun ppf nv' ->
                Format.fprintf ppf "@[-H%a%s@]" br () (Filename.quote nv') )
            , nv ) ) )
      !current_headers ;
    (* [--json ...] *)
    ( match !current_request_body with
    | None ->
        ()
    | Some body ->
        (* [@-] reads from stdin *)
        a (`S "--json @-") ;
        (* format stdin *)
        let quoted_body = Filename.quote body in
        let br' ppf () =
          (* When there is a break in the line, add a ['\ '] where the
             space is a newline character. That only works on Unix. *)
          Format.pp_print_custom_break ~fits:("", 1, "")
            ~breaks:("'\\", 0, "' ") ppf
        in
        let terms = String.split_on_char ' ' quoted_body in
        pre_a (`S {|printf "%s\n"|}) ;
        pre_a
          (`P (Format.pp_print_list ~pp_sep:br' Format.pp_print_string, terms)) ;
        pre_a (`S "|") ) ;
    (* [-X POST] *)
    ( match meth with
    | `GET ->
        ()
    | _ ->
        a (`S (Format.asprintf "-X %a" Http.Method.pp meth)) ) ;
    (* [--http1.1 ...] *)
    List.iter (fun i -> a (`S i)) !args0 ;
    (* [--cacert] *)
    ( match cacerts with
    | Some cacerts ->
        a (`S ("--cacert " ^ cacerts))
    | None ->
        () ) ;
    (* Render the command with formatting and line continuation characters *)
    let dump_q q =
      Format.pp_print_list ~pp_sep:br
        (fun ppf -> function
          | `S s ->
              Format.pp_print_string ppf s
          | `Q s ->
              Format.pp_print_string ppf (Filename.quote s)
          | `P (pp, v) ->
              pp ppf v )
        ppf
        (Queue.to_seq q |> List.of_seq)
    in
    dump_q pre ;
    dump_q main ;
    Format.pp_print_flush ppf () ;
    let cmd = Buffer.to_bytes buf |> String.of_bytes in
    `Curl_command cmd
  in
  ( module struct
    type 'a thread = [`Curl_command of string | `Never of 'a]

    let execute = execute

    let bind m f =
      match m with `Never v -> f v | `Curl_command v -> `Curl_command v

    let return v = `Never v
  end : AGENT )
