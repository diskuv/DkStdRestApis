# DkCoder REST APIs

`DkStdRestApis` is a monorepo project containing generated REST client code for multiple REST services.
The intent is to be a one-stop shop for the common REST services for OCaml users, especially anyone using [DkCoder](https://github.com/diskuv/dkcoder#readme).

> If you would like to support this project, please consider adding a star to the [DkCoder GitHub project](https://github.com/diskuv/dkcoder). *That is also the place to leave issues and feedback!*

## Documents

| Title     | Link                                                                           |
| --------- | ------------------------------------------------------------------------------ |
| NotStripe | [src/DkStdRestApis_NotStripe/README.md](src/DkStdRestApis_NotStripe/README.md) |

## Licenses

The generated clients in `src/` are liberally licensed with the [LICENSE-Apache2](./LICENSE-Apache2) Apache 2.0 license.

The `dk`, `dk.cmd` and `__dk.cmake` build tools are [LICENSE-OSL3](./LICENSE-OSL3) OSL 3.0 licensed with prompts for additional licenses for the OCaml-modified LGPL license and DkSDK license.

## Quick Start

> Prerequisites: opam 2.2.0+. On Unix that is `bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh) --version 2.2.0~beta3"` and on Windows that is `winget install opam`

Checkout the project:

```shell
git clone https://github.com/diskuv/DkStdRestApis.git
cd DkStdRestApis
```

and create an exploratory opam switch:

```sh
# DkStdRestApis requires OCaml 5.2.0+ until OCaml 4.12.3
# (https://github.com/ocaml/ocaml/pull/13204) is released. DkStdRestApis will be
# published to opam once APIs + dependencies stabilize.
opam switch create . 5.2.0 --no-install

# Small set of transitive dependencies; 5 of 21 are build-time dependencies.
#  ocamlbuild, dune, ocamlfind, stringext, routes, ocaml-syntax-shims, csexp,
#  topkg, dune-configurator, ptime, fmt, bigstringaf, cstruct, angstrom, hex,
#  uri, json-data-encoding, jsonm, sexplib0, uutf, ezjsonm,.
opam install --subpath src/ . --yes --deps-only

# We'll add some backends and dev tools.
#  1. DkStdRestApis has scaffolding for REST servers. Bring your own
#  web server backend. Includes an example for [tiny_httpd] (+3 dependencies).
#  2. REST clients are generated. Bring your own web agent backend. Includes an
#  example for an unreleased [cohttp-curl-lwt] (+9 deps).
opam pin add cohttp-curl git+https://github.com/mirage/ocaml-cohttp.git#77fb272f8eac61b9d94067450c75b58fe4c2e122 --yes
opam pin add tiny_httpd git+https://github.com/c-cube/tiny_httpd.git#9eb3cbfc70d112d09eccada835667b76d1f758f6 --yes
opam install cohttp-curl-lwt utop ocaml-lsp-server odoc --yes
```

and then start exploring the [Stripe REST API](https://docs.stripe.com/api)
from its [OpenAPI 3.0.0 specification](https://github.com/stripe/openapi#readme):

```ocaml
$ opam exec dune utop

#require "DkStdRestApis_NotStripe" ;;
#require "DkStdRestApis_NotStripe.Clients" ;;
open DkStdRestApis_NotStripe ;;
open DkStdRestApis_NotStripe_C ;;

(* Do you have a Stripe account? Then:
   1. Replace the [bearer].
   2. Add ~cacerts="/etc/ssl/certs/ca-certificates.crt" on Linux; aka. ca-build.crt.

   Otherwise skip to the "Print Curl" section! *)
module Agent = (val Curl2.create_cohttp_curl_lwt_agent ~server_url:"https://api.stripe.com" ~bearer:"YOUR_STRIPE_API_TEST_SECRET_KEY" ~headers:[("stripe-version", "2024-04-10")] ()) ;;
module StripeClient = Stripe.Client (Agent) ;;

StripeClient.getCustomers ~limit:1 None ;;

(* Print Curl: Print 'curl ...' commands rather than trying to execute the web request. *)

module Agent = (val CurlCmd.create_agent ~server_url:"https://api.stripe.com" ~bearer:"YOUR_STRIPE_API_TEST_SECRET_KEY" ~headers:[("stripe-version", "2024-04-10")] ()) ;;
module StripeClient = Stripe.Client (Agent) ;;

CurlCmd.print @@ StripeClient.getCustomers ~limit:1 None ;;

#quit ;;
```

Okay, we have a way to print Curl commands.

Let's start the server in [ServerTiny.ml](src/DkStdRestApis_NotStripe/ServerTiny.ml):

```sh
$ opam exec -- dune exec src/DkStdRestApis_NotStripe/ServerTiny.exe
listening on http://127.0.0.1:8080
```

and run a couple Curl commands in **another terminal**:

```sh
$ curl -H "Content-Type: application/x-www-form-urlencoded" http://127.0.0.1:8080/v1/customers/123
{"error":{"message":"Customer 123 not found.","type":"api_error"}}

$ curl -d "email=elves@northpole.ca" -d "name=Santa Claus" http://127.0.0.1:8080/v1/customers/123
{"created":1719209196,"email":"elves@northpole.ca","id":"123","livemode":false,"name":"Santa Claus","object":"customer"}

$ curl -H "Content-Type: application/x-www-form-urlencoded" http://127.0.0.1:8080/v1/customers/123
{"created":1719209196,"email":"elves@northpole.ca","id":"123","livemode":false,"name":"Santa Claus","object":"customer"}
```
