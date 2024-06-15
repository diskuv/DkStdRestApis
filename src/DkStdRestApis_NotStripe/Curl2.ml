(* Naming:
   Curl0 - low-level Curl client; thin wrapper around libcurl
   Curl1 - higher-level Curl client with Lwt threading
   Curl2 - HTTP agent for REST clients over curl + Lwt *)

module Curl0 = Curl
module Curl1 = Cohttp_curl_lwt

module type AGENT = sig
  type 'a thread = 'a Lwt.t

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

(** [create_cohttp_curl_lwt_agent ?server_url ?timeout_ms ()] creates
    a Cohttp Curl Lwt web agent for DkStdRestApis.

    [timeout_ms] defaults to 5 seconds.

    [cacerts] is the location of the SSL certificate authority certificates for cURL
    if it uses OpenSSL (ie. Linux) rather than a native SSL (ie. Windows
    and macOS). Typically called ["ca-certificates.crt"] or ["ca-build.crt"]. *)
let create_cohttp_curl_lwt_agent ?server_url ?(timeout_ms = 5000) ?cacerts
    ?verbose ?bearer ?(headers = []) () =
  let initial_headers =
    Http.Header.of_list
      [ ("user-agent", "DkStdRestApis/0.4")
      ; ("accept", "application/json")
      ; ("accept-charset", "UTF-8") ]
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
    let current_http_version = ref None in
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
            current_http_version := Some Curl0.HTTP_VERSION_1_1
        | _ ->
            () )
      attributes ;
    try
      let uri =
        of_ref "Missing `Server" current_server
        ^ of_ref "Missing `Path" current_path
      in
      (* Do GET/POST/etc. *)
      let reply =
        let context = Curl1.Context.create () in
        let meth : Http.Method.t =
          (of_ref "Missing `Method" current_method :> Http.Method.t)
        in
        let request =
          (* nit: Unclear why ~input:(Curl1.Source.string data) is not working for POST data.
             It does nothing. Confirm with local docker of https://httpbin.org/#/
             So use [Curl0.set_postfields] *)
          Curl1.Request.create ~timeout_ms ~headers:!current_headers meth ~uri
            ~input:Curl1.Source.empty ~output:Curl1.Sink.string
        in
        let curl0 = Curl1.Request.Expert.curl request in
        ( match verbose with
        | Some level when level >= 1 ->
            Curl0.set_verbose curl0 true ;
            if level >= 2 then
              Curl0.set_debugfunction curl0 (fun _curl0 dtype s ->
                  match dtype with
                  | Curl0.DEBUGTYPE_DATA_IN ->
                      prerr_endline ("CURLIN: " ^ s)
                  | Curl0.DEBUGTYPE_DATA_OUT ->
                      prerr_endline ("CURLOUT: " ^ s)
                  | _ ->
                      () )
        | _ ->
            () ) ;
        ( match !current_request_body with
        | Some data ->
            Curl0.set_postfields curl0 data
        | None ->
            () ) ;
        ( match !current_http_version with
        | Some v ->
            Curl0.set_httpversion curl0 v
        | None ->
            () ) ;
        ( match cacerts with
        | Some cacerts ->
            Curl0.set_cainfo curl0 cacerts
        | None ->
            () ) ;
        Curl1.submit context request
      in
      let ( let* ) = Lwt.bind in
      let* resp, response_body =
        Lwt.both (Curl1.Response.response reply) (Curl1.Response.body reply)
      in
      let status : Http.Status.t = Http.Response.status resp in
      let response_headers = Http.Response.headers resp in
      Lwt.return
        (Ok
           [ `HttpStatus (Http.Status.to_int status)
           ; `Headers (Http.Header.to_list response_headers)
           ; `ResponseBody response_body ] )
    with Invalid_agent_attribute msg ->
      Lwt.return (Error (`Invalid_agent_attribute msg))
  in
  ( module struct
    type 'a thread = 'a Lwt.t

    let execute = execute

    let bind = Lwt.bind

    let return = Lwt.return
  end : AGENT )
