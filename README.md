# Uber REST APIs

`DkUberRestApis` is a project containing generated REST client code for multiple REST services.
The intent is to be a one-stop shop for the common REST services for OCaml users, especially anyone using [DkCoder](https://github.com/diskuv/dkcoder#readme).

## Design

The foundation is the [json-data-encoding](https://gitlab.com/nomadic-labs/data-encoding) package.

For REST services built on top of JSON Schema, `json-data-encoding` is sufficient to describe the requests and responses.

For REST services built on top of OpenAPI, DkUberRestApis adds build tools so that OCaml client code is generated.

For other REST services, DkUberRestApis provides some utilities to help describe those services.

Some design goals:

- The generated clients should be publishable in OCaml's main opam repository.
- Support vendor-specific extensions to the standards. *This goal means supporting [Stripe's extensions to OpenAPI](https://github.com/stripe/openapi) and AWS SigV4 headers*
- Have a small number of liberally-licensed runtime dependencies. *This goal rules out the `data-encoding` package with its numerous dependencies especially its non-liberal zarith dependency.*
- Keep the cross-language interoperability goals for DkCoder and DkSDK by being able to generate non-OCaml clients. *This goal means there should be an intermediate representation of the REST schema.*
- Don't repeat the downloading and parsing logic of REST specifications, and generation logic of REST clients. *This goal is why there is an "uber" client. An alternative is to commonalize the logic into libraries, which will be developed over time.*

Some desires but not hard-and-fast design goals:

- The REST APIs should be not just for REST clients but also for writing your own REST services. *That is, try not to throw away the bidirectionality of REST encoders.*

## Licenses

The `dk`, `dk.cmd` and `__dk.cmake` build tools are [LICENSE-OSL3](./LICENSE-OSL3) OSL 3.0 licensed with prompts for additional licenses for the OCaml-modified LGPL license and DKSDK license. That is a complex mix of free and copyleft licenses so ...

The project source code under `src/` and generated clients are liberally licensed with the [LICENSE-Apache2](./LICENSE-Apache2) Apache 2.0 license.

## APIs

> The generation requires code that is in DkCoder 0.4 which is not released as of 2024-05-27

The generated OCaml clients are in [src/DkUberRestApis_Std](src/DkUberRestApis_Std).

### Compiling an API with DkCoder

Verify there are no code generation issues by compiling an API like Stripe's with:

```sh
git clean -d -f -x _opam _build "#s" src/DkUberRestApis_Std/
./dk DkRun_V0_4.Run src/DkUberRestApis_Std/Stripe.ml
```

### Compiling all APIs with Dune

On Unix including Cygwin:

```sh
git clean -d -f -x _opam _build "#s" src/DkUberRestApis_Std/
opam switch create . 4.14.2 --no-install
opam install dune json-data-encoding ptime ezjsonm -y
cp dune-project.in dune-project
cp src/DkUberRestApis_Std/dune.in src/DkUberRestApis_Std/dune
opam exec -- dune build
```

On Windows in Command Prompt with DkML or opam 2.2:

```dosbatch
git clean -d -f -x _opam _build "#s" src

REM In opam 2.2, use the following insteaD:
REM    opam switch create . 4.14.2 --no-install
dkml init

opam install dune json-data-encoding ptime ezjsonm -y
COPY dune-project.in dune-project
COPY src/DkUberRestApis_Std/dune.in src/DkUberRestApis_Std/dune
opam exec -- dune build
```

Then do:

```sh
opam switch create . 4.14.2
opam install dune
opam exec -- dune build DkUberRestApis_Std.opam
opam install ./DkUberRestApis_Std.opam --deps-only
opam exec -- dune build
```

### Generating Stripe

Download the Stripe schema and generate OCaml encoders with:

```sh
./dk DkRun_V0_4.Run src/DkUberRestApis_Gen/StripeGen.ml --ml src/DkUberRestApis_Std/Stripe.ml --include-odoc
```

Stripe updates frequently (often once a day).
Command-line options exist to get a newer Stripe schema than what is available in `src/DkUberRestApis_Std/Stripe.ml`.
