(lang dune 3.0)

(name DkStdRestApis_NotStripe)
(version 0.4.0)

; Generation of opam files interferes with DkCoder's internal use
; of dune (perhaps some new dune flag is needed?). So ...
; 1. We use the `./dk DkRune_V0_4.Run --generator dune` option
; to compile in a different project directory when using DkCoder.
; 2. We use the opam 2.2.0+ install --subpath option to only
; consider opam files in the non-traditional src/ folder.
;(generate_opam_files true)

(license "OSL-3.0")
(maintainers "Diskuv, Inc. <opensource+dkml@support.diskuv.com>")
(authors "Diskuv, Inc. <opensource+dkml@support.diskuv.com>")
(homepage "https://diskuv.com/dksdk/coder/2024-intro-scripting/")
(bug_reports "https://github.com/diskuv/dkcoder/issues")

(package
  (name DkStdRestApis_NotStripe)
  (synopsis "REST API for Stripe")
  ; Small set of transitive dependencies; 5 of 20 are build-time dependencies:
  ; ocamlbuild, dune, ocamlfind, stringext, routes, ocaml-syntax-shims, csexp,
  ; topkg, dune-configurator, ptime, fmt, bigstringaf, cstruct, angstrom, hex,
  ; uri, json-data-encoding, jsonm, sexplib0, ezjsonm.
  (depends
    (ocaml (>= 4.14))
    (ezjsonm (>= 1.3.0))
    (json-data-encoding (>= 1.0.1))
    (ptime (>= 1.1.0))
    (routes (>= 2.0.0))
    (uri (>= 4.4.0))))
