# NotStripe (unofficial port of Stripe)

## Regenerating

Stripe updates frequently (often once a day)!

Download the Stripe schema and generate OCaml encoders with:

```sh
./dk DkRun_Project.Run --generator dune -- DkStdRestApis_Gen.StripeGen gen --many-files --ml src/DkStdRestApis_NotStripe/Stripe.ml --include-odoc
```

Command-line options (`--help`) are available to get a newer Stripe schema than what is available in `src/DkStdRestApis_NotStripe/Stripe.ml`.
