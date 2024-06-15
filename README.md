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

In a Unix shell we'll create an exploratory opam switch:

```sh
# Tr1RestApis requires OCaml 5.2.0+ until OCaml 4.12.3
# (https://github.com/ocaml/ocaml/pull/13204) is released. Tr1RestApis will be
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
opam install tiny_httpd cohttp-curl-lwt utop ocaml-lsp-server odoc --yes
```

and then start exploring the [Stripe REST API](https://docs.stripe.com/api)
from its [OpenAPI 3.0.0 specification](https://github.com/stripe/openapi#readme):

```ocaml
$ git clone XXXX
$ opam exec dune utop

#require "DkStdRestApis_NotStripe" ;;
#require "DkStdRestApis_NotStripe.X" ;;
open DkStdRestApis_NotStripe ;;
open DkStdRestApis_NotStripe_X ;;

(* Add ~cacerts="/etc/ssl/certs/ca-certificates.crt" on Linux; aka. ca-build.crt. *)
module Agent = (val Curl2.create_cohttp_curl_lwt_agent ~server_url:"https://api.stripe.com" ~bearer:"YOUR_STRIPE_API_TEST_SECRET_KEY" ~headers:[("stripe-version", "2024-04-10")] ()) ;;
module StripeClient = Stripe.Client (Agent) ;;

StripeClient.getCustomers ~limit:1 None ;;

(* Print 'curl ...' commands. *)

module Agent = (val CurlCmd.create_agent ~server_url:"https://api.stripe.com" ~bearer:"YOUR_STRIPE_API_TEST_SECRET_KEY" ~headers:[("stripe-version", "2024-04-10")] ()) ;;
module StripeClient = Stripe.Client (Agent) ;;

match StripeClient.getCustomers ~limit:1 None with 
| `Curl_command s -> print_endline s
| `Never _ -> assert false ;;
```
