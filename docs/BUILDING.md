# Building

> The generation requires code that is in DkCoder 0.4 which is not released as of 2024-05-27

The generated OCaml clients are in [src/DkStdRestApis_NotStripe](src/DkStdRestApis_NotStripe).

## Compiling an API with DkCoder

Verify there are no code generation issues by compiling an API like Stripe's with:

```sh
git clean -d -f -x _opam _build "#s" src/DkStdRestApis_NotStripe/
./dk DkRun_V0_4.Run src/DkStdRestApis_NotStripe/Stripe.ml
```

## Compiling all APIs with Dune

On Unix including Cygwin:

```sh
git clean -d -f -x _opam _build "#s" src/DkStdRestApis_NotStripe/
opam switch create . 4.14.2 --no-install
opam install dune json-data-encoding ptime ezjsonm -y
cp dune-project.in dune-project
cp src/DkStdRestApis_NotStripe/dune.in src/DkStdRestApis_NotStripe/dune
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
COPY src/DkStdRestApis_NotStripe/dune.in src/DkStdRestApis_NotStripe/dune
opam exec -- dune build
```
