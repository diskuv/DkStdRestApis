(* STEP 1: Module imports and aliases. *)
open DkStdRestApis_NotStripe
module BodySerDe' = StripeBodies.BodySerDe'
module Paths' = StripePaths.Paths'
module Encoders' = StripeEncdrs.Encoders'

(* STEP 2: Supply all the OpenAPI values needed for DkStdRestApis
   to auto-respond when the request is not valid, for any operation..
   Stripe shares the "error" record amongst all of its operations
   (very tidy!) so only one value (the "error" record) is needed. *)
module Defaults = struct
  let error : StripeTypes.error =
    { additional= []
    ; error=
        { additional= []
        ; charge= None
        ; code= None
        ; decline_code= None
        ; type_= Api_error
        ; source= None
        ; setup_intent= None
        ; request_log_url= None
        ; payment_method_type= None
        ; payment_method= None
        ; payment_intent= None
        ; param= None
        ; doc_url= None
        ; message= Some "API is not implemented" } }
end

(* STEP 3. Base handlers are provided to auto-respond to any operation
   with the default error defined in STEP 2.
   Override whichever handlers we want to mock. In our example, we
   do a simplistic in-memory database so we can test the REST behavior
   using Curl. *)
module Handlers = struct
  include Stripe.BaseHandlers (Defaults)
  module CustomerTable = Map.Make (String)

  let cust_table = ref CustomerTable.empty

  let postCustomersCustomer ~customer = function
    | Some (`FormUrlEncoded (req : StripeTypes.t_ccfda63fd5)) ->
        (* Input [cust'] does not match the type required by
           [CustomerTable.add] *)
        let cust_obj : StripeTypes.customer =
          { additional= []
          ; address= None (* types incompatible *)
          ; balance= req.balance
          ; cash_balance= None (* types incompatible *)
          ; created= Ptime_clock.now ()
          ; currency= None
          ; default_source= None (* types incompatible *)
          ; delinquent= None
          ; description= req.description
          ; discount= None
          ; email= req.email
          ; id= customer
          ; invoice_credit_balance= None
          ; invoice_prefix= req.invoice_prefix
          ; invoice_settings= None (* types incompatible *)
          ; livemode= false
          ; metadata= None (* types incompatible *)
          ; name= req.name
          ; next_invoice_sequence= req.next_invoice_sequence
          ; object_= Customer
          ; phone= req.phone
          ; preferred_locales= req.preferred_locales
          ; shipping= None (* types incompatible *)
          ; sources= None
          ; subscriptions= None
          ; tax= None (* types incompatible *)
          ; tax_exempt= None (* types incompatible *)
          ; tax_ids= None
          ; test_clock= None }
        in
        let cust_record : StripeTypes.t_a33375052d = Customer cust_obj in
        cust_table := CustomerTable.add customer cust_record !cust_table ;
        Ok (`CH_OK (cust_obj, []))
    | None ->
        Ok
          (`CH_Default
            ( 501
            , Defaults.error
            , [ `Http_header
                  ("x-dksdk-api-operation", "OVERRIDDEN-PostCustomersCustomer")
              ] ) )

  let getCustomersCustomer ~customer ?expand:_ _req =
    match CustomerTable.find_opt customer !cust_table with
    | Some cust_record ->
        Ok (`CH_OK (cust_record, []))
    | None ->
        Ok
          (`CH_Default
            ( 404
            , { Defaults.error with
                error=
                  { Defaults.error.error with
                    message=
                      Some (Printf.sprintf "Customer %s not found." customer) }
              }
            , [ `Http_header
                  ("x-dksdk-api-operation", "OVERRIDDEN-GetCustomersCustomer")
              ] ) )
end

(* STEP 4. Create the server that delegates to the Handlers and responds with
   Tiny_httpd's [Response.t] type. The server exposes [Server._router]
   which is a [Routes.router].
   Confer: https://v3.ocaml.org/p/routes/latest/doc/index.html *)
module Server =
  Stripe.Server
    (Handlers)
    (struct
      type response = Tiny_httpd.Response.t
    end)

(* STEP 5. Make the handler / event loop / whatever will drive the Server
   Router. *)
let handler (req : Tiny_httpd.IO.Input.t Tiny_httpd.Request.t) =
  (* Tiny_httpd.Request.path is from the HTTP/1.1 line so includes query params
     and fragments, but not scheme/host/port *)
  let uri = Uri.of_string (Tiny_httpd.Request.path req) in
  let uri = Uri.with_host uri (Some (Tiny_httpd.Request.host req)) in
  let uri = Uri.with_scheme uri (Some "http") in
  let router = Server._router (Tiny_httpd.Request.meth req) in
  match Routes.match' router ~target:(Uri.path uri) with
  | Routes.FullMatch f_handle | Routes.MatchWithTrailingSlash f_handle ->
      let headers : (string * string) list = Tiny_httpd.Request.headers req in
      let body_full = Tiny_httpd.Request.read_body_full req in
      let body = body_full.body in
      let _resp ~code ~headers body =
        let headers =
          List.fold_right
            (fun (name, value) hdrs -> Tiny_httpd.Headers.set name value hdrs)
            headers Tiny_httpd.Headers.empty
        in
        Tiny_httpd.Response.make_string ~code ~headers (Ok body)
      in
      begin
        match Tiny_httpd.Request.get_header req "content-type" with
        | Some "application/x-www-form-urlencoded" ->
            f_handle uri _resp `FormUrlEncoded `Json headers (Some body)
        | None | Some "" | Some "application/json" ->
            f_handle uri _resp `Json `Json headers (Some body)
        | _ ->
            Tiny_httpd.Response.make_string ~code:400
              (Ok
                 "The Content-Type header must be 'application/json' or \
                  'application/x-www-form-urlencoded'." )
      end
  | Routes.NoMatch ->
      Tiny_httpd.Response.fail ~code:404 "Route not found in API specification."

let () =
  let server = Tiny_httpd.create () in
  Tiny_httpd.set_top_handler server handler ;
  Printf.printf "listening on http://%s:%d\n%!" (Tiny_httpd.addr server)
    (Tiny_httpd.port server) ;
  match Tiny_httpd.run server with Ok () -> () | Error e -> raise e
