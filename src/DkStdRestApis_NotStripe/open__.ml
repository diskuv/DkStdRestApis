(* CHANGE NOTICE
   1. Modify {!StandardExt.caseize} documentation and body to
   translate any module name you see below when you add/change them. *)

(* Used only by DkCoder since excluded in [./dune] *)

module String = Tr1Stdlib_V414Base.String
module Option = Tr1Stdlib_V414Base.Option
module List = Tr1Stdlib_V414Base.List
module Routes = Tr1Routes_Std.Routes
module Uri = Tr1Uri_Std.Uri
module Fun = Tr1Stdlib_V414Base.Fun
module Buffer = Tr1Stdlib_V414Base.Buffer
module Format = Tr1Stdlib_V414CRuntime.Format
module Stringext = Tr1String_Ext.Stringext
module Map = Tr1Stdlib_V414Base.Map
module Array = Tr1Stdlib_V414Base.Array

(* Used by StripeExt *)
module Printf = Tr1Stdlib_V414CRuntime.Printf
module Float = Tr1Stdlib_V414Base.Float
module Int64 = Tr1Stdlib_V414Base.Int64
