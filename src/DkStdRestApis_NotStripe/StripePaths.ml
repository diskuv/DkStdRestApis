(** {1 Paths} **)

module Paths' = struct
  open StripeParams
  let _lift_read_error e =
    let d = Ezjsonm.read_error_description e in
    let l = Ezjsonm.read_error_location e in
    `Unparseable_response (d, l)
  
  let _lift_cannot_destruct (path, exn) =
    let d =
      Format.asprintf "%a" (Json_encoding.print_error ?print_unknown:None) exn
    in
    let l =
      try Some (Json_query.json_pointer_of_path path)
      with Json_query.Unsupported_path_item _ -> None
    in
    `Nonconforming_response (d, l)
  
(** <p>A webhook endpoint must have a <code>url</code> and a list of <code>enabled_events</code>. You may optionally specify the Boolean <code>connect</code> parameter. If set to true, then a Connect webhook endpoint that notifies the specified <code>url</code> about events from all connected accounts is created; otherwise an account webhook endpoint that notifies the specified <code>url</code> only about events from your account is created. You can also create webhook endpoints in the <a href="https://dashboard.stripe.com/account/webhooks">webhooks settings</a> section of the Dashboard.</p>
    
    @see "openapi/spec3.json" /v1/webhook_endpoints *)
let postWebhookEndpoints () = Routes.(s "v1" / s "webhook_endpoints" /? nil)

(** <p>Returns a list of your webhook endpoints.</p>
    
    @see "openapi/spec3.json" /v1/webhook_endpoints *)
let getWebhookEndpoints () = Routes.(s "v1" / s "webhook_endpoints" /? nil)

(** <p>Retrieves a list of Transaction objects.</p>
    
    @see "openapi/spec3.json" /v1/treasury/transactions *)
let getTreasuryTransactions () =
  Routes.(s "v1" / s "treasury" / s "transactions" /? nil)

(** <p>Creates an InboundTransfer.</p>
    
    @see "openapi/spec3.json" /v1/treasury/inbound_transfers *)
let postTreasuryInboundTransfers () =
  Routes.(s "v1" / s "treasury" / s "inbound_transfers" /? nil)

(** <p>Returns a list of InboundTransfers sent from the specified FinancialAccount.</p>
    
    @see "openapi/spec3.json" /v1/treasury/inbound_transfers *)
let getTreasuryInboundTransfers () =
  Routes.(s "v1" / s "treasury" / s "inbound_transfers" /? nil)

(** <p>Reverses a ReceivedDebit and creates a DebitReversal object.</p>
    
    @see "openapi/spec3.json" /v1/treasury/debit_reversals *)
let postTreasuryDebitReversals () =
  Routes.(s "v1" / s "treasury" / s "debit_reversals" /? nil)

(** <p>Returns a list of DebitReversals.</p>
    
    @see "openapi/spec3.json" /v1/treasury/debit_reversals *)
let getTreasuryDebitReversals () =
  Routes.(s "v1" / s "treasury" / s "debit_reversals" /? nil)

(** <p>Allows the user to capture an arbitrary amount, also known as a forced capture.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/issuing/transactions/create_force_capture *)
let postTestHelpersIssuingTransactionsCreateForceCapture () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "transactions"
    / s "create_force_capture" /? nil)

(** <p>Returns a list of <code>Review</code> objects that have <code>open</code> set to <code>true</code>. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/reviews *)
let getReviews () = Routes.(s "v1" / s "reviews" /? nil)

(** <p>Creates a new price for an existing product. The price can be recurring or one-time.</p>
    
    @see "openapi/spec3.json" /v1/prices *)
let postPrices () = Routes.(s "v1" / s "prices" /? nil)

(** <p>Returns a list of your active prices, excluding <a href="/docs/products-prices/pricing-models#inline-pricing">inline prices</a>. For the list of inactive prices, set <code>active</code> to false.</p>
    
    @see "openapi/spec3.json" /v1/prices *)
let getPrices () = Routes.(s "v1" / s "prices" /? nil)

(** <p>Returns a list of objects that contain the rates at which foreign currencies are converted to one another. Only shows the currencies for which Stripe supports.</p>
    
    @see "openapi/spec3.json" /v1/exchange_rates *)
let getExchangeRates () = Routes.(s "v1" / s "exchange_rates" /? nil)

(** <p>Creates a Session object.</p>
    
    @see "openapi/spec3.json" /v1/checkout/sessions *)
let postCheckoutSessions () =
  Routes.(s "v1" / s "checkout" / s "sessions" /? nil)

(** <p>Returns a list of Checkout Sessions.</p>
    
    @see "openapi/spec3.json" /v1/checkout/sessions *)
let getCheckoutSessions () =
  Routes.(s "v1" / s "checkout" / s "sessions" /? nil)

(** <p>Creates a personalization design object.</p>
    
    @see "openapi/spec3.json" /v1/issuing/personalization_designs *)
let postIssuingPersonalizationDesigns () =
  Routes.(s "v1" / s "issuing" / s "personalization_designs" /? nil)

(** <p>Returns a list of personalization design objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/issuing/personalization_designs *)
let getIssuingPersonalizationDesigns () =
  Routes.(s "v1" / s "issuing" / s "personalization_designs" /? nil)

(** <p>Creates an item to be added to a draft invoice (up to 250 items per invoice). If no invoice is specified, the item will be on the next invoice created for the customer specified.</p>
    
    @see "openapi/spec3.json" /v1/invoiceitems *)
let postInvoiceitems () = Routes.(s "v1" / s "invoiceitems" /? nil)

(** <p>Returns a list of your invoice items. Invoice items are returned sorted by creation date, with the most recently created invoice items appearing first.</p>
    
    @see "openapi/spec3.json" /v1/invoiceitems *)
let getInvoiceitems () = Routes.(s "v1" / s "invoiceitems" /? nil)

(** <p>Retrieves the details of an account.</p>
    
    @see "openapi/spec3.json" /v1/account *)
let getAccount () = Routes.(s "v1" / s "account" /? nil)

(** <p>Creates a short-lived API key for a given resource.</p>
    
    @see "openapi/spec3.json" /v1/ephemeral_keys *)
let postEphemeralKeys () = Routes.(s "v1" / s "ephemeral_keys" /? nil)

(** <p>Search for customers you’ve previously created using Stripe’s <a href="/docs/search#search-query-language">Search Query Language</a>.
    Don’t use search in read-after-write flows where strict consistency is necessary. Under normal operating
    conditions, data is searchable in less than a minute. Occasionally, propagation of new or updated data can be up
    to an hour behind during outages. Search functionality is not available to merchants in India.</p>
    
    @see "openapi/spec3.json" /v1/customers/search *)
let getCustomersSearch () =
  Routes.(s "v1" / s "customers" / s "search" /? nil)

(** <p>Retrieves the current account balance, based on the authentication that was used to make the request.
     For a sample request, see <a href="/docs/connect/account-balances#accounting-for-negative-balances">Accounting for negative balances</a>.</p>
    
    @see "openapi/spec3.json" /v1/balance *)
let getBalance () = Routes.(s "v1" / s "balance" /? nil)

(** <p>Use this endpoint to simulate a test mode ReceivedCredit initiated by a third party. In live mode, you can’t directly create ReceivedCredits initiated by third parties.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/treasury/received_credits *)
let postTestHelpersTreasuryReceivedCredits () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "received_credits"
    /? nil)

(** <p>Search for subscriptions you’ve previously created using Stripe’s <a href="/docs/search#search-query-language">Search Query Language</a>.
    Don’t use search in read-after-write flows where strict consistency is necessary. Under normal operating
    conditions, data is searchable in less than a minute. Occasionally, propagation of new or updated data can be up
    to an hour behind during outages. Search functionality is not available to merchants in India.</p>
    
    @see "openapi/spec3.json" /v1/subscriptions/search *)
let getSubscriptionsSearch () =
  Routes.(s "v1" / s "subscriptions" / s "search" /? nil)

(** <p>When you create a new refund, you must specify a Charge or a PaymentIntent object on which to create it.</p>
    
    <p>Creating a new refund will refund a charge that has previously been created but not yet refunded.
    Funds will be refunded to the credit or debit card that was originally charged.</p>
    
    <p>You can optionally refund only part of a charge.
    You can do so multiple times, until the entire charge has been refunded.</p>
    
    <p>Once entirely refunded, a charge can’t be refunded again.
    This method will raise an error when called on an already-refunded charge,
    or when trying to refund more money than is left on a charge.</p>
    
    @see "openapi/spec3.json" /v1/refunds *)
let postRefunds () = Routes.(s "v1" / s "refunds" /? nil)

(** <p>Returns a list of all refunds you created. We return the refunds in sorted order, with the most recent refunds appearing first The 10 most recent refunds are always available by default on the Charge object.</p>
    
    @see "openapi/spec3.json" /v1/refunds *)
let getRefunds () = Routes.(s "v1" / s "refunds" /? nil)

(** <p>Create or replace a secret in the secret store.</p>
    
    @see "openapi/spec3.json" /v1/apps/secrets *)
let postAppsSecrets () = Routes.(s "v1" / s "apps" / s "secrets" /? nil)

(** <p>List all secrets stored on the given scope.</p>
    
    @see "openapi/spec3.json" /v1/apps/secrets *)
let getAppsSecrets () = Routes.(s "v1" / s "apps" / s "secrets" /? nil)

(** <p>Search for PaymentIntents you’ve previously created using Stripe’s <a href="/docs/search#search-query-language">Search Query Language</a>.
    Don’t use search in read-after-write flows where strict consistency is necessary. Under normal operating
    conditions, data is searchable in less than a minute. Occasionally, propagation of new or updated data can be up
    to an hour behind during outages. Search functionality is not available to merchants in India.</p>
    
    @see "openapi/spec3.json" /v1/payment_intents/search *)
let getPaymentIntentsSearch () =
  Routes.(s "v1" / s "payment_intents" / s "search" /? nil)

(** <p>Retrieve a list of active entitlements for a customer</p>
    
    @see "openapi/spec3.json" /v1/entitlements/active_entitlements *)
let getEntitlementsActiveEntitlements () =
  Routes.(s "v1" / s "entitlements" / s "active_entitlements" /? nil)

(** <p>Creates a new <code>Configuration</code> object.</p>
    
    @see "openapi/spec3.json" /v1/terminal/configurations *)
let postTerminalConfigurations () =
  Routes.(s "v1" / s "terminal" / s "configurations" /? nil)

(** <p>Returns a list of <code>Configuration</code> objects.</p>
    
    @see "openapi/spec3.json" /v1/terminal/configurations *)
let getTerminalConfigurations () =
  Routes.(s "v1" / s "terminal" / s "configurations" /? nil)

(** <p>Creates a new subscription schedule object. Each customer can have up to 500 active or scheduled subscriptions.</p>
    
    @see "openapi/spec3.json" /v1/subscription_schedules *)
let postSubscriptionSchedules () =
  Routes.(s "v1" / s "subscription_schedules" /? nil)

(** <p>Retrieves the list of your subscription schedules.</p>
    
    @see "openapi/spec3.json" /v1/subscription_schedules *)
let getSubscriptionSchedules () =
  Routes.(s "v1" / s "subscription_schedules" /? nil)

(** <p>Creates a new object and begin running the report. (Certain report types require a <a href="https://stripe.com/docs/keys#test-live-modes">live-mode API key</a>.)</p>
    
    @see "openapi/spec3.json" /v1/reporting/report_runs *)
let postReportingReportRuns () =
  Routes.(s "v1" / s "reporting" / s "report_runs" /? nil)

(** <p>Returns a list of Report Runs, with the most recent appearing first.</p>
    
    @see "openapi/spec3.json" /v1/reporting/report_runs *)
let getReportingReportRuns () =
  Routes.(s "v1" / s "reporting" / s "report_runs" /? nil)

(** <p>Creates a Climate order object for a given Climate product. The order will be processed immediately
    after creation and payment will be deducted your Stripe balance.</p>
    
    @see "openapi/spec3.json" /v1/climate/orders *)
let postClimateOrders () = Routes.(s "v1" / s "climate" / s "orders" /? nil)

(** <p>Lists all Climate order objects. The orders are returned sorted by creation date, with the
    most recently created orders appearing first.</p>
    
    @see "openapi/spec3.json" /v1/climate/orders *)
let getClimateOrders () = Routes.(s "v1" / s "climate" / s "orders" /? nil)

(** <p>You can now model subscriptions more flexibly using the <a href="#prices">Prices API</a>. It replaces the Plans API and is backwards compatible to simplify your migration.</p>
    
    @see "openapi/spec3.json" /v1/plans *)
let postPlans () = Routes.(s "v1" / s "plans" /? nil)

(** <p>Returns a list of your plans.</p>
    
    @see "openapi/spec3.json" /v1/plans *)
let getPlans () = Routes.(s "v1" / s "plans" /? nil)

(** <p>Lists all Issuing <code>Token</code> objects for a given card.</p>
    
    @see "openapi/spec3.json" /v1/issuing/tokens *)
let getIssuingTokens () = Routes.(s "v1" / s "issuing" / s "tokens" /? nil)

(** <p>When retrieving an upcoming invoice, you’ll get a <strong>lines</strong> property containing the total count of line items and the first handful of those items. There is also a URL where you can retrieve the full (paginated) list of line items.</p>
    
    @see "openapi/spec3.json" /v1/invoices/upcoming/lines *)
let getInvoicesUpcomingLines () =
  Routes.(s "v1" / s "invoices" / s "upcoming" / s "lines" /? nil)

(** <p>Use this endpoint to simulate a test mode ReceivedDebit initiated by a third party. In live mode, you can’t directly create ReceivedDebits initiated by third parties.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/treasury/received_debits *)
let postTestHelpersTreasuryReceivedDebits () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "received_debits"
    /? nil)

(** <p>Allows the user to refund an arbitrary amount, also known as a unlinked refund.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/issuing/transactions/create_unlinked_refund *)
let postTestHelpersIssuingTransactionsCreateUnlinkedRefund () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "transactions"
    / s "create_unlinked_refund" /? nil)

(** <p>Create a test-mode authorization.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/issuing/authorizations *)
let postTestHelpersIssuingAuthorizations () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "authorizations"
    /? nil)

(** <p>Creates a new subscription on an existing customer. Each customer can have up to 500 active or scheduled subscriptions.</p>
    
    <p>When you create a subscription with <code>collection_method=charge_automatically</code>, the first invoice is finalized as part of the request.
    The <code>payment_behavior</code> parameter determines the exact behavior of the initial payment.</p>
    
    <p>To start subscriptions where the first invoice always begins in a <code>draft</code> status, use <a href="/docs/billing/subscriptions/subscription-schedules#managing">subscription schedules</a> instead.
    Schedules provide the flexibility to model more complex billing configurations that change over time.</p>
    
    @see "openapi/spec3.json" /v1/subscriptions *)
let postSubscriptions () = Routes.(s "v1" / s "subscriptions" /? nil)

(** <p>By default, returns a list of subscriptions that have not been canceled. In order to list canceled subscriptions, specify <code>status=canceled</code>.</p>
    
    @see "openapi/spec3.json" /v1/subscriptions *)
let getSubscriptions () = Routes.(s "v1" / s "subscriptions" /? nil)

(** <p>List events, going back up to 30 days. Each event data is rendered according to Stripe API version at its creation time, specified in <a href="https://docs.stripe.com/api/events/object">event object</a> <code>api_version</code> attribute (not according to your current Stripe API version or <code>Stripe-Version</code> header).</p>
    
    @see "openapi/spec3.json" /v1/events *)
let getEvents () = Routes.(s "v1" / s "events" /? nil)

(** <p>Creates a configuration that describes the functionality and behavior of a PortalSession</p>
    
    @see "openapi/spec3.json" /v1/billing_portal/configurations *)
let postBillingPortalConfigurations () =
  Routes.(s "v1" / s "billing_portal" / s "configurations" /? nil)

(** <p>Returns a list of configurations that describe the functionality of the customer portal.</p>
    
    @see "openapi/spec3.json" /v1/billing_portal/configurations *)
let getBillingPortalConfigurations () =
  Routes.(s "v1" / s "billing_portal" / s "configurations" /? nil)

(** <p>Returns a list of transactions that have contributed to the Stripe account balance (e.g., charges, transfers, and so forth). The transactions are returned in sorted order, with the most recent transactions appearing first.</p>
    
    <p>Note that this endpoint was previously called “Balance history” and used the path <code>/v1/balance/history</code>.</p>
    
    @see "openapi/spec3.json" /v1/balance/history *)
let getBalanceHistory () = Routes.(s "v1" / s "balance" / s "history" /? nil)

(** <p>At any time, you can preview the upcoming invoice for a customer. This will show you all the charges that are pending, including subscription renewal charges, invoice item charges, etc. It will also show you any discounts that are applicable to the invoice.</p>
    
    <p>Note that when you are viewing an upcoming invoice, you are simply viewing a preview – the invoice has not yet been created. As such, the upcoming invoice will not show up in invoice listing calls, and you cannot use the API to pay or edit the invoice. If you want to change the amount that your customer will be billed, you can add, remove, or update pending invoice items, or update the customer’s discount.</p>
    
    <p>You can preview the effects of updating a subscription, including a preview of what proration will take place. To ensure that the actual proration is calculated exactly the same as the previewed proration, you should pass the <code>subscription_details.proration_date</code> parameter when doing the actual subscription update. The recommended way to get only the prorations being previewed is to consider only proration line items where <code>period\[start\]</code> is equal to the <code>subscription_details.proration_date</code> value passed in the request. </p>
    
    <p>Note: Currency conversion calculations use the latest exchange rates. Exchange rates may vary between the time of the preview and the time of the actual invoice creation. <a href="https://docs.stripe.com/currencies/conversions">Learn more</a></p>
    
    @see "openapi/spec3.json" /v1/invoices/create_preview *)
let postInvoicesCreatePreview () =
  Routes.(s "v1" / s "invoices" / s "create_preview" /? nil)

(** <p>Creates a billing meter event adjustment</p>
    
    @see "openapi/spec3.json" /v1/billing/meter_event_adjustments *)
let postBillingMeterEventAdjustments () =
  Routes.(s "v1" / s "billing" / s "meter_event_adjustments" /? nil)

(** <p>To connect to a reader the Stripe Terminal SDK needs to retrieve a short-lived connection token from Stripe, proxied through your server. On your backend, add an endpoint that creates and returns a connection token.</p>
    
    @see "openapi/spec3.json" /v1/terminal/connection_tokens *)
let postTerminalConnectionTokens () =
  Routes.(s "v1" / s "terminal" / s "connection_tokens" /? nil)

(** <p>Returns a list of physical bundle objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/issuing/physical_bundles *)
let getIssuingPhysicalBundles () =
  Routes.(s "v1" / s "issuing" / s "physical_bundles" /? nil)

(** <p>Finds a secret in the secret store by name and scope.</p>
    
    @see "openapi/spec3.json" /v1/apps/secrets/find *)
let getAppsSecretsFind () =
  Routes.(s "v1" / s "apps" / s "secrets" / s "find" /? nil)

(** <p>Creates a feature</p>
    
    @see "openapi/spec3.json" /v1/entitlements/features *)
let postEntitlementsFeatures () =
  Routes.(s "v1" / s "entitlements" / s "features" /? nil)

(** <p>Retrieve a list of features</p>
    
    @see "openapi/spec3.json" /v1/entitlements/features *)
let getEntitlementsFeatures () =
  Routes.(s "v1" / s "entitlements" / s "features" /? nil)

(** <p>Creates a payment link.</p>
    
    @see "openapi/spec3.json" /v1/payment_links *)
let postPaymentLinks () = Routes.(s "v1" / s "payment_links" /? nil)

(** <p>Returns a list of your payment links.</p>
    
    @see "openapi/spec3.json" /v1/payment_links *)
let getPaymentLinks () = Routes.(s "v1" / s "payment_links" /? nil)

(** <p>Creates a new Issuing <code>Cardholder</code> object that can be issued cards.</p>
    
    @see "openapi/spec3.json" /v1/issuing/cardholders *)
let postIssuingCardholders () =
  Routes.(s "v1" / s "issuing" / s "cardholders" /? nil)

(** <p>Returns a list of Issuing <code>Cardholder</code> objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/issuing/cardholders *)
let getIssuingCardholders () =
  Routes.(s "v1" / s "issuing" / s "cardholders" /? nil)

(** <p>When retrieving a credit note preview, you’ll get a <strong>lines</strong> property containing the first handful of those items. This URL you can retrieve the full (paginated) list of line items.</p>
    
    @see "openapi/spec3.json" /v1/credit_notes/preview/lines *)
let getCreditNotesPreviewLines () =
  Routes.(s "v1" / s "credit_notes" / s "preview" / s "lines" /? nil)

(** <p>Updates Tax <code>Settings</code> parameters used in tax calculations. All parameters are editable but none can be removed once set.</p>
    
    @see "openapi/spec3.json" /v1/tax/settings *)
let postTaxSettings () = Routes.(s "v1" / s "tax" / s "settings" /? nil)

(** <p>Retrieves Tax <code>Settings</code> for a merchant.</p>
    
    @see "openapi/spec3.json" /v1/tax/settings *)
let getTaxSettings () = Routes.(s "v1" / s "tax" / s "settings" /? nil)

(** <p>Calculates tax based on input and returns a Tax <code>Calculation</code> object.</p>
    
    @see "openapi/spec3.json" /v1/tax/calculations *)
let postTaxCalculations () =
  Routes.(s "v1" / s "tax" / s "calculations" /? nil)

(** <p>Returns a list of your disputes.</p>
    
    @see "openapi/spec3.json" /v1/disputes *)
let getDisputes () = Routes.(s "v1" / s "disputes" /? nil)

(** <p>At any time, you can preview the upcoming invoice for a customer. This will show you all the charges that are pending, including subscription renewal charges, invoice item charges, etc. It will also show you any discounts that are applicable to the invoice.</p>
    
    <p>Note that when you are viewing an upcoming invoice, you are simply viewing a preview – the invoice has not yet been created. As such, the upcoming invoice will not show up in invoice listing calls, and you cannot use the API to pay or edit the invoice. If you want to change the amount that your customer will be billed, you can add, remove, or update pending invoice items, or update the customer’s discount.</p>
    
    <p>You can preview the effects of updating a subscription, including a preview of what proration will take place. To ensure that the actual proration is calculated exactly the same as the previewed proration, you should pass the <code>subscription_details.proration_date</code> parameter when doing the actual subscription update. The recommended way to get only the prorations being previewed is to consider only proration line items where <code>period\[start\]</code> is equal to the <code>subscription_details.proration_date</code> value passed in the request.</p>
    
    <p>Note: Currency conversion calculations use the latest exchange rates. Exchange rates may vary between the time of the preview and the time of the actual invoice creation. <a href="https://docs.stripe.com/currencies/conversions">Learn more</a></p>
    
    @see "openapi/spec3.json" /v1/invoices/upcoming *)
let getInvoicesUpcoming () =
  Routes.(s "v1" / s "invoices" / s "upcoming" /? nil)

(** <p>Create an apple pay domain.</p>
    
    @see "openapi/spec3.json" /v1/apple_pay/domains *)
let postApplePayDomains () =
  Routes.(s "v1" / s "apple_pay" / s "domains" /? nil)

(** <p>List apple pay domains.</p>
    
    @see "openapi/spec3.json" /v1/apple_pay/domains *)
let getApplePayDomains () =
  Routes.(s "v1" / s "apple_pay" / s "domains" /? nil)

(** <p>Creates a new <code>ValueList</code> object, which can then be referenced in rules.</p>
    
    @see "openapi/spec3.json" /v1/radar/value_lists *)
let postRadarValueLists () =
  Routes.(s "v1" / s "radar" / s "value_lists" /? nil)

(** <p>Returns a list of <code>ValueList</code> objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/radar/value_lists *)
let getRadarValueLists () =
  Routes.(s "v1" / s "radar" / s "value_lists" /? nil)

(** <p>To launch the Financial Connections authorization flow, create a <code>Session</code>. The session’s <code>client_secret</code> can be used to launch the flow using Stripe.js.</p>
    
    @see "openapi/spec3.json" /v1/link_account_sessions *)
let postLinkAccountSessions () =
  Routes.(s "v1" / s "link_account_sessions" /? nil)

(** <p>Creates a new file link object.</p>
    
    @see "openapi/spec3.json" /v1/file_links *)
let postFileLinks () = Routes.(s "v1" / s "file_links" /? nil)

(** <p>Returns a list of file links.</p>
    
    @see "openapi/spec3.json" /v1/file_links *)
let getFileLinks () = Routes.(s "v1" / s "file_links" /? nil)

(** <p>You can create coupons easily via the <a href="https://dashboard.stripe.com/coupons">coupon management</a> page of the Stripe dashboard. Coupon creation is also accessible via the API if you need to create coupons on the fly.</p>
    
    <p>A coupon has either a <code>percent_off</code> or an <code>amount_off</code> and <code>currency</code>. If you set an <code>amount_off</code>, that amount will be subtracted from any invoice’s subtotal. For example, an invoice with a subtotal of <currency>100</currency> will have a final total of <currency>0</currency> if a coupon with an <code>amount_off</code> of <amount>200</amount> is applied to it and an invoice with a subtotal of <currency>300</currency> will have a final total of <currency>100</currency> if a coupon with an <code>amount_off</code> of <amount>200</amount> is applied to it.</p>
    
    @see "openapi/spec3.json" /v1/coupons *)
let postCoupons () = Routes.(s "v1" / s "coupons" /? nil)

(** <p>Returns a list of your coupons.</p>
    
    @see "openapi/spec3.json" /v1/coupons *)
let getCoupons () = Routes.(s "v1" / s "coupons" /? nil)

(** <p>Returns a list of Issuing <code>Transaction</code> objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/issuing/transactions *)
let getIssuingTransactions () =
  Routes.(s "v1" / s "issuing" / s "transactions" /? nil)

(** <p>Returns a list of Issuing <code>Authorization</code> objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/issuing/authorizations *)
let getIssuingAuthorizations () =
  Routes.(s "v1" / s "issuing" / s "authorizations" /? nil)

(** <p>Retrieves a list of TransactionEntry objects.</p>
    
    @see "openapi/spec3.json" /v1/treasury/transaction_entries *)
let getTreasuryTransactionEntries () =
  Routes.(s "v1" / s "treasury" / s "transaction_entries" /? nil)

(** <p>Returns a list of ReceivedDebits.</p>
    
    @see "openapi/spec3.json" /v1/treasury/received_debits *)
let getTreasuryReceivedDebits () =
  Routes.(s "v1" / s "treasury" / s "received_debits" /? nil)

(** <p>Creates an OutboundTransfer.</p>
    
    @see "openapi/spec3.json" /v1/treasury/outbound_transfers *)
let postTreasuryOutboundTransfers () =
  Routes.(s "v1" / s "treasury" / s "outbound_transfers" /? nil)

(** <p>Returns a list of OutboundTransfers sent from the specified FinancialAccount.</p>
    
    @see "openapi/spec3.json" /v1/treasury/outbound_transfers *)
let getTreasuryOutboundTransfers () =
  Routes.(s "v1" / s "treasury" / s "outbound_transfers" /? nil)

(** <p>To send funds from your Stripe account to a connected account, you create a new transfer object. Your <a href="#balance">Stripe balance</a> must be able to cover the transfer amount, or you’ll receive an “Insufficient Funds” error.</p>
    
    @see "openapi/spec3.json" /v1/transfers *)
let postTransfers () = Routes.(s "v1" / s "transfers" /? nil)

(** <p>Returns a list of existing transfers sent to connected accounts. The transfers are returned in sorted order, with the most recently created transfers appearing first.</p>
    
    @see "openapi/spec3.json" /v1/transfers *)
let getTransfers () = Routes.(s "v1" / s "transfers" /? nil)

(** <p>Creates a new shipping rate object.</p>
    
    @see "openapi/spec3.json" /v1/shipping_rates *)
let postShippingRates () = Routes.(s "v1" / s "shipping_rates" /? nil)

(** <p>Returns a list of your shipping rates.</p>
    
    @see "openapi/spec3.json" /v1/shipping_rates *)
let getShippingRates () = Routes.(s "v1" / s "shipping_rates" /? nil)

(** <p>Creates an Issuing <code>Card</code> object.</p>
    
    @see "openapi/spec3.json" /v1/issuing/cards *)
let postIssuingCards () = Routes.(s "v1" / s "issuing" / s "cards" /? nil)

(** <p>Returns a list of Issuing <code>Card</code> objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/issuing/cards *)
let getIssuingCards () = Routes.(s "v1" / s "issuing" / s "cards" /? nil)

(** <p>Get a preview of a credit note without creating it.</p>
    
    @see "openapi/spec3.json" /v1/credit_notes/preview *)
let getCreditNotesPreview () =
  Routes.(s "v1" / s "credit_notes" / s "preview" /? nil)

(** <p>Creates a AccountSession object that includes a single-use token that the platform can use on their front-end to grant client-side API access.</p>
    
    @see "openapi/spec3.json" /v1/account_sessions *)
let postAccountSessions () = Routes.(s "v1" / s "account_sessions" /? nil)

(** <p>Returns a list of Financial Connections <code>Account</code> objects.</p>
    
    @see "openapi/spec3.json" /v1/linked_accounts *)
let getLinkedAccounts () = Routes.(s "v1" / s "linked_accounts" /? nil)

(** <p>Returns a list of Financial Connections <code>Account</code> objects.</p>
    
    @see "openapi/spec3.json" /v1/financial_connections/accounts *)
let getFinancialConnectionsAccounts () =
  Routes.(s "v1" / s "financial_connections" / s "accounts" /? nil)

(** <p>Search for charges you’ve previously created using Stripe’s <a href="/docs/search#search-query-language">Search Query Language</a>.
    Don’t use search in read-after-write flows where strict consistency is necessary. Under normal operating
    conditions, data is searchable in less than a minute. Occasionally, propagation of new or updated data can be up
    to an hour behind during outages. Search functionality is not available to merchants in India.</p>
    
    @see "openapi/spec3.json" /v1/charges/search *)
let getChargesSearch () = Routes.(s "v1" / s "charges" / s "search" /? nil)

(** <p>Creates a new test clock that can be attached to new customers and quotes.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/test_clocks *)
let postTestHelpersTestClocks () =
  Routes.(s "v1" / s "test_helpers" / s "test_clocks" /? nil)

(** <p>Returns a list of your test clocks.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/test_clocks *)
let getTestHelpersTestClocks () =
  Routes.(s "v1" / s "test_helpers" / s "test_clocks" /? nil)

(** <p>Creates a Tax <code>Transaction</code> from a calculation.</p>
    
    @see "openapi/spec3.json" /v1/tax/transactions/create_from_calculation *)
let postTaxTransactionsCreateFromCalculation () =
  Routes.(s "v1" / s "tax" / s "transactions" / s "create_from_calculation"
    /? nil)

(** <p>Creates a payment method configuration</p>
    
    @see "openapi/spec3.json" /v1/payment_method_configurations *)
let postPaymentMethodConfigurations () =
  Routes.(s "v1" / s "payment_method_configurations" /? nil)

(** <p>List payment method configurations</p>
    
    @see "openapi/spec3.json" /v1/payment_method_configurations *)
let getPaymentMethodConfigurations () =
  Routes.(s "v1" / s "payment_method_configurations" /? nil)

(** <p>Creates a VerificationSession object.</p>
    
    <p>After the VerificationSession is created, display a verification modal using the session <code>client_secret</code> or send your users to the session’s <code>url</code>.</p>
    
    <p>If your API key is in test mode, verification checks won’t actually process, though everything else will occur as if in live mode.</p>
    
    <p>Related guide: <a href="/docs/identity/verify-identity-documents">Verify your users’ identity documents</a></p>
    
    @see "openapi/spec3.json" /v1/identity/verification_sessions *)
let postIdentityVerificationSessions () =
  Routes.(s "v1" / s "identity" / s "verification_sessions" /? nil)

(** <p>Returns a list of VerificationSessions</p>
    
    @see "openapi/spec3.json" /v1/identity/verification_sessions *)
let getIdentityVerificationSessions () =
  Routes.(s "v1" / s "identity" / s "verification_sessions" /? nil)

(** <p>Lists all available Climate supplier objects.</p>
    
    @see "openapi/spec3.json" /v1/climate/suppliers *)
let getClimateSuppliers () =
  Routes.(s "v1" / s "climate" / s "suppliers" /? nil)

(** <p>Returns a list of transactions that have contributed to the Stripe account balance (e.g., charges, transfers, and so forth). The transactions are returned in sorted order, with the most recent transactions appearing first.</p>
    
    <p>Note that this endpoint was previously called “Balance history” and used the path <code>/v1/balance/history</code>.</p>
    
    @see "openapi/spec3.json" /v1/balance_transactions *)
let getBalanceTransactions () =
  Routes.(s "v1" / s "balance_transactions" /? nil)

(** <p>Creates an Issuing <code>Dispute</code> object. Individual pieces of evidence within the <code>evidence</code> object are optional at this point. Stripe only validates that required evidence is present during submission. Refer to <a href="/docs/issuing/purchases/disputes#dispute-reasons-and-evidence">Dispute reasons and evidence</a> for more details about evidence requirements.</p>
    
    @see "openapi/spec3.json" /v1/issuing/disputes *)
let postIssuingDisputes () =
  Routes.(s "v1" / s "issuing" / s "disputes" /? nil)

(** <p>Returns a list of Issuing <code>Dispute</code> objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/issuing/disputes *)
let getIssuingDisputes () =
  Routes.(s "v1" / s "issuing" / s "disputes" /? nil)

(** <p>Creates a billing meter</p>
    
    @see "openapi/spec3.json" /v1/billing/meters *)
let postBillingMeters () = Routes.(s "v1" / s "billing" / s "meters" /? nil)

(** <p>Retrieve a list of billing meters.</p>
    
    @see "openapi/spec3.json" /v1/billing/meters *)
let getBillingMeters () = Routes.(s "v1" / s "billing" / s "meters" /? nil)

(** <p>Creates a customer session object that includes a single-use client secret that you can use on your front-end to grant client-side API access for certain customer resources.</p>
    
    @see "openapi/spec3.json" /v1/customer_sessions *)
let postCustomerSessions () = Routes.(s "v1" / s "customer_sessions" /? nil)

(** <p>Search for products you’ve previously created using Stripe’s <a href="/docs/search#search-query-language">Search Query Language</a>.
    Don’t use search in read-after-write flows where strict consistency is necessary. Under normal operating
    conditions, data is searchable in less than a minute. Occasionally, propagation of new or updated data can be up
    to an hour behind during outages. Search functionality is not available to merchants in India.</p>
    
    @see "openapi/spec3.json" /v1/products/search *)
let getProductsSearch () = Routes.(s "v1" / s "products" / s "search" /? nil)

(** <p>Creates a ForwardingRequest object.</p>
    
    @see "openapi/spec3.json" /v1/forwarding/requests *)
let postForwardingRequests () =
  Routes.(s "v1" / s "forwarding" / s "requests" /? nil)

(** <p>Lists all ForwardingRequest objects.</p>
    
    @see "openapi/spec3.json" /v1/forwarding/requests *)
let getForwardingRequests () =
  Routes.(s "v1" / s "forwarding" / s "requests" /? nil)

(** <p>Creates a new FinancialAccount. For now, each connected account can only have one FinancialAccount.</p>
    
    @see "openapi/spec3.json" /v1/treasury/financial_accounts *)
let postTreasuryFinancialAccounts () =
  Routes.(s "v1" / s "treasury" / s "financial_accounts" /? nil)

(** <p>Returns a list of FinancialAccounts.</p>
    
    @see "openapi/spec3.json" /v1/treasury/financial_accounts *)
let getTreasuryFinancialAccounts () =
  Routes.(s "v1" / s "treasury" / s "financial_accounts" /? nil)

(** <p>Creates a single-use token that represents a bank account’s details.
    You can use this token with any API method in place of a bank account dictionary. You can only use this token once. To do so, attach it to a <a href="#accounts">connected account</a> where <a href="/api/accounts/object#account_object-controller-requirement_collection">controller.requirement_collection</a> is <code>application</code>, which includes Custom accounts.</p>
    
    @see "openapi/spec3.json" /v1/tokens *)
let postTokens () = Routes.(s "v1" / s "tokens" /? nil)

(** <p>Creates a new tax rate.</p>
    
    @see "openapi/spec3.json" /v1/tax_rates *)
let postTaxRates () = Routes.(s "v1" / s "tax_rates" /? nil)

(** <p>Returns a list of your tax rates. Tax rates are returned sorted by creation date, with the most recently created tax rates appearing first.</p>
    
    @see "openapi/spec3.json" /v1/tax_rates *)
let getTaxRates () = Routes.(s "v1" / s "tax_rates" /? nil)

(** <p>Returns a list of scheduled query runs.</p>
    
    @see "openapi/spec3.json" /v1/sigma/scheduled_query_runs *)
let getSigmaScheduledQueryRuns () =
  Routes.(s "v1" / s "sigma" / s "scheduled_query_runs" /? nil)

(** <p>A quote models prices and services for a customer. Default options for <code>header</code>, <code>description</code>, <code>footer</code>, and <code>expires_at</code> can be set in the dashboard via the <a href="https://dashboard.stripe.com/settings/billing/quote">quote template</a>.</p>
    
    @see "openapi/spec3.json" /v1/quotes *)
let postQuotes () = Routes.(s "v1" / s "quotes" /? nil)

(** <p>Returns a list of your quotes.</p>
    
    @see "openapi/spec3.json" /v1/quotes *)
let getQuotes () = Routes.(s "v1" / s "quotes" /? nil)

(** <p>Creates a new customer object.</p>
    
    @see "openapi/spec3.json" /v1/customers *)
let postCustomers () = Routes.(s "v1" / s "customers" /? nil)

(** <p>Returns a list of your customers. The customers are returned sorted by creation date, with the most recent customers appearing first.</p>
    
    @see "openapi/spec3.json" /v1/customers *)
let getCustomers () = Routes.(s "v1" / s "customers" /? nil)

(** <p>Returns a list of application fees you’ve previously collected. The application fees are returned in sorted order, with the most recent fees appearing first.</p>
    
    @see "openapi/spec3.json" /v1/application_fees *)
let getApplicationFees () = Routes.(s "v1" / s "application_fees" /? nil)

(** <p>Creates a new source object.</p>
    
    @see "openapi/spec3.json" /v1/sources *)
let postSources () = Routes.(s "v1" / s "sources" /? nil)

(** <p>Returns a list of SetupAttempts that associate with a provided SetupIntent.</p>
    
    @see "openapi/spec3.json" /v1/setup_attempts *)
let getSetupAttempts () = Routes.(s "v1" / s "setup_attempts" /? nil)

(** <p>Creates a PaymentIntent object.</p>
    
    <p>After the PaymentIntent is created, attach a payment method and <a href="/docs/api/payment_intents/confirm">confirm</a>
    to continue the payment. Learn more about <a href="/docs/payments/payment-intents">the available payment flows
    with the Payment Intents API</a>.</p>
    
    <p>When you use <code>confirm=true</code> during creation, it’s equivalent to creating
    and confirming the PaymentIntent in the same call. You can use any parameters
    available in the <a href="/docs/api/payment_intents/confirm">confirm API</a> when you supply
    <code>confirm=true</code>.</p>
    
    @see "openapi/spec3.json" /v1/payment_intents *)
let postPaymentIntents () = Routes.(s "v1" / s "payment_intents" /? nil)

(** <p>Returns a list of PaymentIntents.</p>
    
    @see "openapi/spec3.json" /v1/payment_intents *)
let getPaymentIntents () = Routes.(s "v1" / s "payment_intents" /? nil)

(** <p>Returns a list of Financial Connections <code>Transaction</code> objects.</p>
    
    @see "openapi/spec3.json" /v1/financial_connections/transactions *)
let getFinancialConnectionsTransactions () =
  Routes.(s "v1" / s "financial_connections" / s "transactions" /? nil)

(** <p>A list of <a href="https://stripe.com/docs/tax/tax-categories">all tax codes available</a> to add to Products in order to allow specific tax calculations.</p>
    
    @see "openapi/spec3.json" /v1/tax_codes *)
let getTaxCodes () = Routes.(s "v1" / s "tax_codes" /? nil)

(** <p>Creates a new product object.</p>
    
    @see "openapi/spec3.json" /v1/products *)
let postProducts () = Routes.(s "v1" / s "products" /? nil)

(** <p>Returns a list of your products. The products are returned sorted by creation date, with the most recently created products appearing first.</p>
    
    @see "openapi/spec3.json" /v1/products *)
let getProducts () = Routes.(s "v1" / s "products" /? nil)

(** <p>Creates a session of the customer portal.</p>
    
    @see "openapi/spec3.json" /v1/billing_portal/sessions *)
let postBillingPortalSessions () =
  Routes.(s "v1" / s "billing_portal" / s "sessions" /? nil)

(** <p>This method is no longer recommended—use the <a href="/docs/api/payment_intents">Payment Intents API</a>
    to initiate a new payment instead. Confirmation of the PaymentIntent creates the <code>Charge</code>
    object used to request payment.</p>
    
    @see "openapi/spec3.json" /v1/charges *)
let postCharges () = Routes.(s "v1" / s "charges" /? nil)

(** <p>Returns a list of charges you’ve previously created. The charges are returned in sorted order, with the most recent charges appearing first.</p>
    
    @see "openapi/spec3.json" /v1/charges *)
let getCharges () = Routes.(s "v1" / s "charges" /? nil)

(** <p>Creates a new <code>ValueListItem</code> object, which is added to the specified parent value list.</p>
    
    @see "openapi/spec3.json" /v1/radar/value_list_items *)
let postRadarValueListItems () =
  Routes.(s "v1" / s "radar" / s "value_list_items" /? nil)

(** <p>Returns a list of <code>ValueListItem</code> objects. The objects are sorted in descending order by creation date, with the most recently created object appearing first.</p>
    
    @see "openapi/spec3.json" /v1/radar/value_list_items *)
let getRadarValueListItems () =
  Routes.(s "v1" / s "radar" / s "value_list_items" /? nil)

(** <p>Creates a payment method domain.</p>
    
    @see "openapi/spec3.json" /v1/payment_method_domains *)
let postPaymentMethodDomains () =
  Routes.(s "v1" / s "payment_method_domains" /? nil)

(** <p>Lists the details of existing payment method domains.</p>
    
    @see "openapi/spec3.json" /v1/payment_method_domains *)
let getPaymentMethodDomains () =
  Routes.(s "v1" / s "payment_method_domains" /? nil)

(** <p>Returns a list of ReceivedCredits.</p>
    
    @see "openapi/spec3.json" /v1/treasury/received_credits *)
let getTreasuryReceivedCredits () =
  Routes.(s "v1" / s "treasury" / s "received_credits" /? nil)

(** <p>Creates an OutboundPayment.</p>
    
    @see "openapi/spec3.json" /v1/treasury/outbound_payments *)
let postTreasuryOutboundPayments () =
  Routes.(s "v1" / s "treasury" / s "outbound_payments" /? nil)

(** <p>Returns a list of OutboundPayments sent from the specified FinancialAccount.</p>
    
    @see "openapi/spec3.json" /v1/treasury/outbound_payments *)
let getTreasuryOutboundPayments () =
  Routes.(s "v1" / s "treasury" / s "outbound_payments" /? nil)

(** <p>Creates a new <code>Reader</code> object.</p>
    
    @see "openapi/spec3.json" /v1/terminal/readers *)
let postTerminalReaders () =
  Routes.(s "v1" / s "terminal" / s "readers" /? nil)

(** <p>Returns a list of <code>Reader</code> objects.</p>
    
    @see "openapi/spec3.json" /v1/terminal/readers *)
let getTerminalReaders () =
  Routes.(s "v1" / s "terminal" / s "readers" /? nil)

(** <p>Creates a new account or customer <code>tax_id</code> object.</p>
    
    @see "openapi/spec3.json" /v1/tax_ids *)
let postTaxIds () = Routes.(s "v1" / s "tax_ids" /? nil)

(** <p>Returns a list of tax IDs.</p>
    
    @see "openapi/spec3.json" /v1/tax_ids *)
let getTaxIds () = Routes.(s "v1" / s "tax_ids" /? nil)

(** <p>Creates a new Tax <code>Registration</code> object.</p>
    
    @see "openapi/spec3.json" /v1/tax/registrations *)
let postTaxRegistrations () =
  Routes.(s "v1" / s "tax" / s "registrations" /? nil)

(** <p>Returns a list of Tax <code>Registration</code> objects.</p>
    
    @see "openapi/spec3.json" /v1/tax/registrations *)
let getTaxRegistrations () =
  Routes.(s "v1" / s "tax" / s "registrations" /? nil)

(** <p>Creates a billing meter event</p>
    
    @see "openapi/spec3.json" /v1/billing/meter_events *)
let postBillingMeterEvents () =
  Routes.(s "v1" / s "billing" / s "meter_events" /? nil)

(** <p>Creates a SetupIntent object.</p>
    
    <p>After you create the SetupIntent, attach a payment method and <a href="/docs/api/setup_intents/confirm">confirm</a>
    it to collect any required permissions to charge the payment method later.</p>
    
    @see "openapi/spec3.json" /v1/setup_intents *)
let postSetupIntents () = Routes.(s "v1" / s "setup_intents" /? nil)

(** <p>Returns a list of SetupIntents.</p>
    
    @see "openapi/spec3.json" /v1/setup_intents *)
let getSetupIntents () = Routes.(s "v1" / s "setup_intents" /? nil)

(** <p>With <a href="/docs/connect">Connect</a>, you can create Stripe accounts for your users.
    To do this, you’ll first need to <a href="https://dashboard.stripe.com/account/applications/settings">register your platform</a>.</p>
    
    <p>If you’ve already collected information for your connected accounts, you <a href="/docs/connect/best-practices#onboarding">can prefill that information</a> when
    creating the account. Connect Onboarding won’t ask for the prefilled information during account onboarding.
    You can prefill any information on the account.</p>
    
    @see "openapi/spec3.json" /v1/accounts *)
let postAccounts () = Routes.(s "v1" / s "accounts" /? nil)

(** <p>Returns a list of accounts connected to your platform via <a href="/docs/connect">Connect</a>. If you’re not a platform, the list is empty.</p>
    
    @see "openapi/spec3.json" /v1/accounts *)
let getAccounts () = Routes.(s "v1" / s "accounts" /? nil)

(** <p>Search for prices you’ve previously created using Stripe’s <a href="/docs/search#search-query-language">Search Query Language</a>.
    Don’t use search in read-after-write flows where strict consistency is necessary. Under normal operating
    conditions, data is searchable in less than a minute. Occasionally, propagation of new or updated data can be up
    to an hour behind during outages. Search functionality is not available to merchants in India.</p>
    
    @see "openapi/spec3.json" /v1/prices/search *)
let getPricesSearch () = Routes.(s "v1" / s "prices" / s "search" /? nil)

(** <p>To upload a file to Stripe, you need to send a request of type <code>multipart/form-data</code>. Include the file you want to upload in the request, and the parameters for creating a file.</p>
    
    <p>All of Stripe’s officially supported Client libraries support sending <code>multipart/form-data</code>.</p>
    
    @see "openapi/spec3.json" /v1/files *)
let postFiles () = Routes.(s "v1" / s "files" /? nil)

(** <p>Returns a list of the files that your account has access to. Stripe sorts and returns the files by their creation dates, placing the most recently created files at the top.</p>
    
    @see "openapi/spec3.json" /v1/files *)
let getFiles () = Routes.(s "v1" / s "files" /? nil)

(** <p>Returns a full list of Report Types.</p>
    
    @see "openapi/spec3.json" /v1/reporting/report_types *)
let getReportingReportTypes () =
  Routes.(s "v1" / s "reporting" / s "report_types" /? nil)

(** <p>This endpoint creates a draft invoice for a given customer. The invoice remains a draft until you <a href="#finalize_invoice">finalize</a> the invoice, which allows you to <a href="#pay_invoice">pay</a> or <a href="#send_invoice">send</a> the invoice to your customers.</p>
    
    @see "openapi/spec3.json" /v1/invoices *)
let postInvoices () = Routes.(s "v1" / s "invoices" /? nil)

(** <p>You can list all invoices, or list the invoices for a specific customer. The invoices are returned sorted by creation date, with the most recently created invoices appearing first.</p>
    
    @see "openapi/spec3.json" /v1/invoices *)
let getInvoices () = Routes.(s "v1" / s "invoices" /? nil)

(** <p>Lists all Country Spec objects available in the API.</p>
    
    @see "openapi/spec3.json" /v1/country_specs *)
let getCountrySpecs () = Routes.(s "v1" / s "country_specs" /? nil)

(** <p>Deletes a secret from the secret store by name and scope.</p>
    
    @see "openapi/spec3.json" /v1/apps/secrets/delete *)
let postAppsSecretsDelete () =
  Routes.(s "v1" / s "apps" / s "secrets" / s "delete" /? nil)

(** <p>Creates an AccountLink object that includes a single-use Stripe URL that the platform can redirect their user to in order to take them through the Connect Onboarding flow.</p>
    
    @see "openapi/spec3.json" /v1/account_links *)
let postAccountLinks () = Routes.(s "v1" / s "account_links" /? nil)

(** <p>Creates a PaymentMethod object. Read the <a href="/docs/stripe-js/reference#stripe-create-payment-method">Stripe.js reference</a> to learn how to create PaymentMethods via Stripe.js.</p>
    
    <p>Instead of creating a PaymentMethod directly, we recommend using the <a href="/docs/payments/accept-a-payment">PaymentIntents</a> API to accept a payment immediately or the <a href="/docs/payments/save-and-reuse">SetupIntent</a> API to collect payment method details ahead of a future payment.</p>
    
    @see "openapi/spec3.json" /v1/payment_methods *)
let postPaymentMethods () = Routes.(s "v1" / s "payment_methods" /? nil)

(** <p>Returns a list of PaymentMethods for Treasury flows. If you want to list the PaymentMethods attached to a Customer for payments, you should use the <a href="/docs/api/payment_methods/customer_list">List a Customer’s PaymentMethods</a> API instead.</p>
    
    @see "openapi/spec3.json" /v1/payment_methods *)
let getPaymentMethods () = Routes.(s "v1" / s "payment_methods" /? nil)

(** <p>To launch the Financial Connections authorization flow, create a <code>Session</code>. The session’s <code>client_secret</code> can be used to launch the flow using Stripe.js.</p>
    
    @see "openapi/spec3.json" /v1/financial_connections/sessions *)
let postFinancialConnectionsSessions () =
  Routes.(s "v1" / s "financial_connections" / s "sessions" /? nil)

(** <p>Reverses a ReceivedCredit and creates a CreditReversal object.</p>
    
    @see "openapi/spec3.json" /v1/treasury/credit_reversals *)
let postTreasuryCreditReversals () =
  Routes.(s "v1" / s "treasury" / s "credit_reversals" /? nil)

(** <p>Returns a list of CreditReversals.</p>
    
    @see "openapi/spec3.json" /v1/treasury/credit_reversals *)
let getTreasuryCreditReversals () =
  Routes.(s "v1" / s "treasury" / s "credit_reversals" /? nil)

(** <p>Top up the balance of an account</p>
    
    @see "openapi/spec3.json" /v1/topups *)
let postTopups () = Routes.(s "v1" / s "topups" /? nil)

(** <p>Returns a list of top-ups.</p>
    
    @see "openapi/spec3.json" /v1/topups *)
let getTopups () = Routes.(s "v1" / s "topups" /? nil)

(** <p>Creates a new <code>Location</code> object.
    For further details, including which address fields are required in each country, see the <a href="/docs/terminal/fleet/locations">Manage locations</a> guide.</p>
    
    @see "openapi/spec3.json" /v1/terminal/locations *)
let postTerminalLocations () =
  Routes.(s "v1" / s "terminal" / s "locations" /? nil)

(** <p>Returns a list of <code>Location</code> objects.</p>
    
    @see "openapi/spec3.json" /v1/terminal/locations *)
let getTerminalLocations () =
  Routes.(s "v1" / s "terminal" / s "locations" /? nil)

(** <p>Search for invoices you’ve previously created using Stripe’s <a href="/docs/search#search-query-language">Search Query Language</a>.
    Don’t use search in read-after-write flows where strict consistency is necessary. Under normal operating
    conditions, data is searchable in less than a minute. Occasionally, propagation of new or updated data can be up
    to an hour behind during outages. Search functionality is not available to merchants in India.</p>
    
    @see "openapi/spec3.json" /v1/invoices/search *)
let getInvoicesSearch () = Routes.(s "v1" / s "invoices" / s "search" /? nil)

(** <p>Partially or fully reverses a previously created <code>Transaction</code>.</p>
    
    @see "openapi/spec3.json" /v1/tax/transactions/create_reversal *)
let postTaxTransactionsCreateReversal () =
  Routes.(s "v1" / s "tax" / s "transactions" / s "create_reversal" /? nil)

(** <p>Returns a list of early fraud warnings.</p>
    
    @see "openapi/spec3.json" /v1/radar/early_fraud_warnings *)
let getRadarEarlyFraudWarnings () =
  Routes.(s "v1" / s "radar" / s "early_fraud_warnings" /? nil)

(** <p>To send funds to your own bank account, create a new payout object. Your <a href="#balance">Stripe balance</a> must cover the payout amount. If it doesn’t, you receive an “Insufficient Funds” error.</p>
    
    <p>If your API key is in test mode, money won’t actually be sent, though every other action occurs as if you’re in live mode.</p>
    
    <p>If you create a manual payout on a Stripe account that uses multiple payment source types, you need to specify the source type balance that the payout draws from. The <a href="#balance_object">balance object</a> details available and pending amounts by source type.</p>
    
    @see "openapi/spec3.json" /v1/payouts *)
let postPayouts () = Routes.(s "v1" / s "payouts" /? nil)

(** <p>Returns a list of existing payouts sent to third-party bank accounts or payouts that Stripe sent to you. The payouts return in sorted order, with the most recently created payouts appearing first.</p>
    
    @see "openapi/spec3.json" /v1/payouts *)
let getPayouts () = Routes.(s "v1" / s "payouts" /? nil)

(** <p>A promotion code points to a coupon. You can optionally restrict the code to a specific customer, redemption limit, and expiration date.</p>
    
    @see "openapi/spec3.json" /v1/promotion_codes *)
let postPromotionCodes () = Routes.(s "v1" / s "promotion_codes" /? nil)

(** <p>Returns a list of your promotion codes.</p>
    
    @see "openapi/spec3.json" /v1/promotion_codes *)
let getPromotionCodes () = Routes.(s "v1" / s "promotion_codes" /? nil)

(** <p>Creates a test mode Confirmation Token server side for your integration tests.</p>
    
    @see "openapi/spec3.json" /v1/test_helpers/confirmation_tokens *)
let postTestHelpersConfirmationTokens () =
  Routes.(s "v1" / s "test_helpers" / s "confirmation_tokens" /? nil)

(** <p>Adds a new item to an existing subscription. No existing items will be changed or replaced.</p>
    
    @see "openapi/spec3.json" /v1/subscription_items *)
let postSubscriptionItems () =
  Routes.(s "v1" / s "subscription_items" /? nil)

(** <p>Returns a list of your subscription items for a given subscription.</p>
    
    @see "openapi/spec3.json" /v1/subscription_items *)
let getSubscriptionItems () = Routes.(s "v1" / s "subscription_items" /? nil)

(** <p>List all verification reports.</p>
    
    @see "openapi/spec3.json" /v1/identity/verification_reports *)
let getIdentityVerificationReports () =
  Routes.(s "v1" / s "identity" / s "verification_reports" /? nil)

(** <p>Issue a credit note to adjust the amount of a finalized invoice. For a <code>status=open</code> invoice, a credit note reduces
    its <code>amount_due</code>. For a <code>status=paid</code> invoice, a credit note does not affect its <code>amount_due</code>. Instead, it can result
    in any combination of the following:</p>
    
    <ul>
    <li>Refund: create a new refund (using <code>refund_amount</code>) or link an existing refund (using <code>refund</code>).</li>
    <li>Customer balance credit: credit the customer’s balance (using <code>credit_amount</code>) which will be automatically applied to their next invoice when it’s finalized.</li>
    <li>Outside of Stripe credit: record the amount that is or will be credited outside of Stripe (using <code>out_of_band_amount</code>).</li>
    </ul>
    
    <p>For post-payment credit notes the sum of the refund, credit and outside of Stripe amounts must equal the credit note total.</p>
    
    <p>You may issue multiple credit notes for an invoice. Each credit note will increment the invoice’s <code>pre_payment_credit_notes_amount</code>
    or <code>post_payment_credit_notes_amount</code> depending on its <code>status</code> at the time of credit note creation.</p>
    
    @see "openapi/spec3.json" /v1/credit_notes *)
let postCreditNotes () = Routes.(s "v1" / s "credit_notes" /? nil)

(** <p>Returns a list of credit notes.</p>
    
    @see "openapi/spec3.json" /v1/credit_notes *)
let getCreditNotes () = Routes.(s "v1" / s "credit_notes" /? nil)

(** <p>Lists all available Climate product objects.</p>
    
    @see "openapi/spec3.json" /v1/climate/products *)
let getClimateProducts () =
  Routes.(s "v1" / s "climate" / s "products" /? nil)

(** <p>You can also delete webhook endpoints via the <a href="https://dashboard.stripe.com/account/webhooks">webhook endpoint management</a> page of the Stripe dashboard.</p>
    
    @param webhook_endpoint webhook_endpoint
    @see "openapi/spec3.json" /v1/webhook_endpoints/\{webhook_endpoint\} *)
let deleteWebhookEndpointsWebhookEndpoint () =
  Routes.(s "v1" / s "webhook_endpoints"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"webhook_endpoint" ~op:"/v1/webhook_endpoints/{webhook_endpoint}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"webhook_endpoint" ~loc:`Path ~style:`Simple ~explode:false)
        ":webhook_endpoint" /? nil)

(** <p>Updates the webhook endpoint. You may edit the <code>url</code>, the list of <code>enabled_events</code>, and the status of your endpoint.</p>
    
    @param webhook_endpoint webhook_endpoint
    @see "openapi/spec3.json" /v1/webhook_endpoints/\{webhook_endpoint\} *)
let postWebhookEndpointsWebhookEndpoint () =
  Routes.(s "v1" / s "webhook_endpoints"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"webhook_endpoint" ~op:"/v1/webhook_endpoints/{webhook_endpoint}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"webhook_endpoint" ~loc:`Path ~style:`Simple ~explode:false)
        ":webhook_endpoint" /? nil)

(** <p>Retrieves the webhook endpoint with the given ID.</p>
    
    @param webhook_endpoint webhook_endpoint
    @see "openapi/spec3.json" /v1/webhook_endpoints/\{webhook_endpoint\} *)
let getWebhookEndpointsWebhookEndpoint () =
  Routes.(s "v1" / s "webhook_endpoints"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"webhook_endpoint" ~op:"/v1/webhook_endpoints/{webhook_endpoint}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"webhook_endpoint" ~loc:`Path ~style:`Simple ~explode:false)
        ":webhook_endpoint" /? nil)

(** <p>Increment a test-mode Authorization.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/test_helpers/issuing/authorizations/\{authorization\}/increment *)
let postTestHelpersIssuingAuthorizationsAuthorizationIncrement () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/test_helpers/issuing/authorizations/{authorization}/increment" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" / s "increment" /? nil)

(** <p>Permanently deletes a customer. It cannot be undone. Also immediately cancels any active subscriptions on the customer.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\} *)
let deleteCustomersCustomer () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" /? nil)

(** <p>Updates the specified customer by setting the values of the parameters passed. Any parameters not provided will be left unchanged. For example, if you pass the <strong>source</strong> parameter, that becomes the customer’s active source (e.g., a card) to be used for all charges in the future. When you update a customer to a new valid card source by passing the <strong>source</strong> parameter: for each of the customer’s current subscriptions, if the subscription bills automatically and is in the <code>past_due</code> state, then the latest open invoice for the subscription with automatic collection enabled will be retried. This retry will not count as an automatic retry, and will not affect the next regularly scheduled payment for the invoice. Changing the <strong>default_source</strong> for a customer will not trigger this behavior.</p>
    
    <p>This request accepts mostly the same arguments as the customer creation call.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\} *)
let postCustomersCustomer () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" /? nil)

(** <p>Retrieves a Customer object.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\} *)
let getCustomersCustomer () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" /? nil)

(** <p>Deleting plans means new subscribers can’t be added. Existing subscribers aren’t affected.</p>
    
    @param plan plan
    @see "openapi/spec3.json" /v1/plans/\{plan\} *)
let deletePlansPlan () =
  Routes.(s "v1" / s "plans"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"plan" ~op:"/v1/plans/{plan}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"plan" ~loc:`Path ~style:`Simple ~explode:false)
        ":plan" /? nil)

(** <p>Updates the specified plan by setting the values of the parameters passed. Any parameters not provided are left unchanged. By design, you cannot change a plan’s ID, amount, currency, or billing cycle.</p>
    
    @param plan plan
    @see "openapi/spec3.json" /v1/plans/\{plan\} *)
let postPlansPlan () =
  Routes.(s "v1" / s "plans"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"plan" ~op:"/v1/plans/{plan}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"plan" ~loc:`Path ~style:`Simple ~explode:false)
        ":plan" /? nil)

(** <p>Retrieves the plan with the given ID.</p>
    
    @param plan plan
    @see "openapi/spec3.json" /v1/plans/\{plan\} *)
let getPlansPlan () =
  Routes.(s "v1" / s "plans"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"plan" ~op:"/v1/plans/{plan}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"plan" ~loc:`Path ~style:`Simple ~explode:false)
        ":plan" /? nil)

(** <p>With <a href="/connect">Connect</a>, you can delete accounts you manage.</p>
    
    <p>Test-mode accounts can be deleted at any time.</p>
    
    <p>Live-mode accounts where Stripe is responsible for negative account balances cannot be deleted, which includes Standard accounts. Live-mode accounts where your platform is liable for negative account balances, which includes Custom and Express accounts, can be deleted when all <a href="/api/balance/balanace_object">balances</a> are zero.</p>
    
    <p>If you want to delete your own account, use the <a href="https://dashboard.stripe.com/settings/account">account information tab in your account settings</a> instead.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\} *)
let deleteAccountsAccount () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" /? nil)

(** <p>Updates a <a href="/connect/accounts">connected account</a> by setting the values of the parameters passed. Any parameters not provided are
    left unchanged.</p>
    
    <p>For accounts where <a href="/api/accounts/object#account_object-controller-requirement_collection">controller.requirement_collection</a>
    is <code>application</code>, which includes Custom accounts, you can update any information on the account.</p>
    
    <p>For accounts where <a href="/api/accounts/object#account_object-controller-requirement_collection">controller.requirement_collection</a>
    is <code>stripe</code>, which includes Standard and Express accounts, you can update all information until you create
    an <a href="/api/account_links">Account Link</a> or <a href="/api/account_sessions">Account Session</a> to start Connect onboarding,
    after which some properties can no longer be updated.</p>
    
    <p>To update your own account, use the <a href="https://dashboard.stripe.com/settings/account">Dashboard</a>. Refer to our
    <a href="/docs/connect/updating-accounts">Connect</a> documentation to learn more about updating accounts.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\} *)
let postAccountsAccount () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" /? nil)

(** <p>Retrieves the details of an account.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\} *)
let getAccountsAccount () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" /? nil)

(** <p>Cancels an InboundTransfer.</p>
    
    @param inbound_transfer inbound_transfer
    @see "openapi/spec3.json" /v1/treasury/inbound_transfers/\{inbound_transfer\}/cancel *)
let postTreasuryInboundTransfersInboundTransferCancel () =
  Routes.(s "v1" / s "treasury" / s "inbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"inbound_transfer" ~op:"/v1/treasury/inbound_transfers/{inbound_transfer}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"inbound_transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":inbound_transfer" / s "cancel" /? nil)

(** <p>Deletes a <code>Configuration</code> object.</p>
    
    @param configuration configuration
    @see "openapi/spec3.json" /v1/terminal/configurations/\{configuration\} *)
let deleteTerminalConfigurationsConfiguration () =
  Routes.(s "v1" / s "terminal" / s "configurations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"configuration" ~op:"/v1/terminal/configurations/{configuration}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"configuration" ~loc:`Path ~style:`Simple ~explode:false)
        ":configuration" /? nil)

(** <p>Updates a new <code>Configuration</code> object.</p>
    
    @param configuration configuration
    @see "openapi/spec3.json" /v1/terminal/configurations/\{configuration\} *)
let postTerminalConfigurationsConfiguration () =
  Routes.(s "v1" / s "terminal" / s "configurations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"configuration" ~op:"/v1/terminal/configurations/{configuration}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"configuration" ~loc:`Path ~style:`Simple ~explode:false)
        ":configuration" /? nil)

(** <p>Retrieves a <code>Configuration</code> object.</p>
    
    @param configuration configuration
    @see "openapi/spec3.json" /v1/terminal/configurations/\{configuration\} *)
let getTerminalConfigurationsConfiguration () =
  Routes.(s "v1" / s "terminal" / s "configurations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"configuration" ~op:"/v1/terminal/configurations/{configuration}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"configuration" ~loc:`Path ~style:`Simple ~explode:false)
        ":configuration" /? nil)

(** <p>Retrieves the details of a Financial Connections <code>Session</code></p>
    
    @param session session
    @see "openapi/spec3.json" /v1/financial_connections/sessions/\{session\} *)
let getFinancialConnectionsSessionsSession () =
  Routes.(s "v1" / s "financial_connections" / s "sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/financial_connections/sessions/{session}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" /? nil)

(** <p>Finalizes the quote.</p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\}/finalize *)
let postQuotesQuoteFinalize () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}/finalize" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" / s "finalize" /? nil)

(** <p>Starts advancing a test clock to a specified time in the future. Advancement is done when status changes to <code>Ready</code>.</p>
    
    @param test_clock test_clock
    @see "openapi/spec3.json" /v1/test_helpers/test_clocks/\{test_clock\}/advance *)
let postTestHelpersTestClocksTestClockAdvance () =
  Routes.(s "v1" / s "test_helpers" / s "test_clocks"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"test_clock" ~op:"/v1/test_helpers/test_clocks/{test_clock}/advance" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"test_clock" ~loc:`Path ~style:`Simple ~explode:false)
        ":test_clock" / s "advance" /? nil)

(** <p>List source transactions for a given source.</p>
    
    @param source source
    @see "openapi/spec3.json" /v1/sources/\{source\}/source_transactions *)
let getSourcesSourceSourceTransactions () =
  Routes.(s "v1" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"source" ~op:"/v1/sources/{source}/source_transactions" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"source" ~loc:`Path ~style:`Simple ~explode:false)
        ":source" / s "source_transactions" /? nil)

(** <p>Updates the specified charge by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param charge charge
    @see "openapi/spec3.json" /v1/charges/\{charge\} *)
let postChargesCharge () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" /? nil)

(** <p>Retrieves the details of a charge that has previously been created. Supply the unique charge ID that was returned from your previous request, and Stripe will return the corresponding charge information. The same information is returned when creating or refunding the charge.</p>
    
    @param charge charge
    @see "openapi/spec3.json" /v1/charges/\{charge\} *)
let getChargesCharge () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" /? nil)

(** <p>Updates a payment link.</p>
    
    @param payment_link payment_link
    @see "openapi/spec3.json" /v1/payment_links/\{payment_link\} *)
let postPaymentLinksPaymentLink () =
  Routes.(s "v1" / s "payment_links"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_link" ~op:"/v1/payment_links/{payment_link}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_link" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_link" /? nil)

(** <p>Retrieve a payment link.</p>
    
    @param payment_link payment_link
    @see "openapi/spec3.json" /v1/payment_links/\{payment_link\} *)
let getPaymentLinksPaymentLink () =
  Routes.(s "v1" / s "payment_links"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_link" ~op:"/v1/payment_links/{payment_link}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_link" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_link" /? nil)

(** <p>Retrieves the details of an existing ReceivedCredit by passing the unique ReceivedCredit ID from the ReceivedCredit list.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/treasury/received_credits/\{id\} *)
let getTreasuryReceivedCreditsId () =
  Routes.(s "v1" / s "treasury" / s "received_credits"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/treasury/received_credits/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates a test mode created OutboundTransfer with tracking details. The OutboundTransfer must not be cancelable, and cannot be in the <code>canceled</code> or <code>failed</code> states.</p>
    
    @param outbound_transfer outbound_transfer
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_transfers/\{outbound_transfer\} *)
let postTestHelpersTreasuryOutboundTransfersOutboundTransfer () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"outbound_transfer" ~op:"/v1/test_helpers/treasury/outbound_transfers/{outbound_transfer}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"outbound_transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":outbound_transfer" /? nil)

(** <p>Updates an existing subscription schedule.</p>
    
    @param schedule schedule
    @see "openapi/spec3.json" /v1/subscription_schedules/\{schedule\} *)
let postSubscriptionSchedulesSchedule () =
  Routes.(s "v1" / s "subscription_schedules"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"schedule" ~op:"/v1/subscription_schedules/{schedule}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"schedule" ~loc:`Path ~style:`Simple ~explode:false)
        ":schedule" /? nil)

(** <p>Retrieves the details of an existing subscription schedule. You only need to supply the unique subscription schedule identifier that was returned upon subscription schedule creation.</p>
    
    @param schedule schedule
    @see "openapi/spec3.json" /v1/subscription_schedules/\{schedule\} *)
let getSubscriptionSchedulesSchedule () =
  Routes.(s "v1" / s "subscription_schedules"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"schedule" ~op:"/v1/subscription_schedules/{schedule}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"schedule" ~loc:`Path ~style:`Simple ~explode:false)
        ":schedule" /? nil)

(** <p>Updates a VerificationSession object.</p>
    
    <p>When the session status is <code>requires_input</code>, you can use this method to update the
    verification check and options.</p>
    
    @param session session
    @see "openapi/spec3.json" /v1/identity/verification_sessions/\{session\} *)
let postIdentityVerificationSessionsSession () =
  Routes.(s "v1" / s "identity" / s "verification_sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/identity/verification_sessions/{session}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" /? nil)

(** <p>Retrieves the details of a VerificationSession that was previously created.</p>
    
    <p>When the session status is <code>requires_input</code>, you can use this method to retrieve a valid
    <code>client_secret</code> or <code>url</code> to allow re-submission.</p>
    
    @param session session
    @see "openapi/spec3.json" /v1/identity/verification_sessions/\{session\} *)
let getIdentityVerificationSessionsSession () =
  Routes.(s "v1" / s "identity" / s "verification_sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/identity/verification_sessions/{session}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" /? nil)

(** <p>Updates a PaymentMethod object. A PaymentMethod must be attached a customer to be updated.</p>
    
    @param payment_method payment_method
    @see "openapi/spec3.json" /v1/payment_methods/\{payment_method\} *)
let postPaymentMethodsPaymentMethod () =
  Routes.(s "v1" / s "payment_methods"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method" ~op:"/v1/payment_methods/{payment_method}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method" /? nil)

(** <p>Retrieves a PaymentMethod object attached to the StripeAccount. To retrieve a payment method attached to a Customer, you should use <a href="/docs/api/payment_methods/customer">Retrieve a Customer’s PaymentMethods</a></p>
    
    @param payment_method payment_method
    @see "openapi/spec3.json" /v1/payment_methods/\{payment_method\} *)
let getPaymentMethodsPaymentMethod () =
  Routes.(s "v1" / s "payment_methods"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method" ~op:"/v1/payment_methods/{payment_method}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method" /? nil)

(** <p>Retrieves a Session object.</p>
    
    @param session session
    @see "openapi/spec3.json" /v1/checkout/sessions/\{session\} *)
let getCheckoutSessionsSession () =
  Routes.(s "v1" / s "checkout" / s "sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/checkout/sessions/{session}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" /? nil)

(** <p>Initiates a setup intent flow on a Reader.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\}/process_setup_intent *)
let postTerminalReadersReaderProcessSetupIntent () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}/process_setup_intent" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" / s "process_setup_intent" /? nil)

(** <p>Deletes a <code>ValueList</code> object, also deleting any items contained within the value list. To be deleted, a value list must not be referenced in any rules.</p>
    
    @param value_list value_list
    @see "openapi/spec3.json" /v1/radar/value_lists/\{value_list\} *)
let deleteRadarValueListsValueList () =
  Routes.(s "v1" / s "radar" / s "value_lists"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"value_list" ~op:"/v1/radar/value_lists/{value_list}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"value_list" ~loc:`Path ~style:`Simple ~explode:false)
        ":value_list" /? nil)

(** <p>Updates a <code>ValueList</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged. Note that <code>item_type</code> is immutable.</p>
    
    @param value_list value_list
    @see "openapi/spec3.json" /v1/radar/value_lists/\{value_list\} *)
let postRadarValueListsValueList () =
  Routes.(s "v1" / s "radar" / s "value_lists"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"value_list" ~op:"/v1/radar/value_lists/{value_list}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"value_list" ~loc:`Path ~style:`Simple ~explode:false)
        ":value_list" /? nil)

(** <p>Retrieves a <code>ValueList</code> object.</p>
    
    @param value_list value_list
    @see "openapi/spec3.json" /v1/radar/value_lists/\{value_list\} *)
let getRadarValueListsValueList () =
  Routes.(s "v1" / s "radar" / s "value_lists"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"value_list" ~op:"/v1/radar/value_lists/{value_list}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"value_list" ~loc:`Path ~style:`Simple ~explode:false)
        ":value_list" /? nil)

(** <p>When you get a dispute, contacting your customer is always the best first step. If that doesn’t work, you can submit evidence to help us resolve the dispute in your favor. You can do this in your <a href="https://dashboard.stripe.com/disputes">dashboard</a>, but if you prefer, you can use the API to submit evidence programmatically.</p>
    
    <p>Depending on your dispute type, different evidence fields will give you a better chance of winning your dispute. To figure out which evidence fields to provide, see our <a href="/docs/disputes/categories">guide to dispute types</a>.</p>
    
    @param dispute dispute
    @see "openapi/spec3.json" /v1/disputes/\{dispute\} *)
let postDisputesDispute () =
  Routes.(s "v1" / s "disputes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"dispute" ~op:"/v1/disputes/{dispute}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"dispute" ~loc:`Path ~style:`Simple ~explode:false)
        ":dispute" /? nil)

(** <p>Retrieves the dispute with the given ID.</p>
    
    @param dispute dispute
    @see "openapi/spec3.json" /v1/disputes/\{dispute\} *)
let getDisputesDispute () =
  Routes.(s "v1" / s "disputes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"dispute" ~op:"/v1/disputes/{dispute}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"dispute" ~loc:`Path ~style:`Simple ~explode:false)
        ":dispute" /? nil)

(** <p>Retrieves a physical bundle object.</p>
    
    @param physical_bundle physical_bundle
    @see "openapi/spec3.json" /v1/issuing/physical_bundles/\{physical_bundle\} *)
let getIssuingPhysicalBundlesPhysicalBundle () =
  Routes.(s "v1" / s "issuing" / s "physical_bundles"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"physical_bundle" ~op:"/v1/issuing/physical_bundles/{physical_bundle}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"physical_bundle" ~loc:`Path ~style:`Simple ~explode:false)
        ":physical_bundle" /? nil)

(** <p>Retrieves the details of an existing CreditReversal by passing the unique CreditReversal ID from either the CreditReversal creation request or CreditReversal list</p>
    
    @param credit_reversal credit_reversal
    @see "openapi/spec3.json" /v1/treasury/credit_reversals/\{credit_reversal\} *)
let getTreasuryCreditReversalsCreditReversal () =
  Routes.(s "v1" / s "treasury" / s "credit_reversals"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"credit_reversal" ~op:"/v1/treasury/credit_reversals/{credit_reversal}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"credit_reversal" ~loc:`Path ~style:`Simple ~explode:false)
        ":credit_reversal" /? nil)

(** <p>Retrieves the line items of a committed standalone transaction as a collection.</p>
    
    @param transaction transaction
    @see "openapi/spec3.json" /v1/tax/transactions/\{transaction\}/line_items *)
let getTaxTransactionsTransactionLineItems () =
  Routes.(s "v1" / s "tax" / s "transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/tax/transactions/{transaction}/line_items" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" / s "line_items" /? nil)

(** <p>Stripe will automatically send invoices to customers according to your <a href="https://dashboard.stripe.com/account/billing/automatic">subscriptions settings</a>. However, if you’d like to manually send an invoice to your customer out of the normal schedule, you can do so. When sending invoices that have already been paid, there will be no reference to the payment in the email.</p>
    
    <p>Requests made in test-mode result in no emails being sent, despite sending an <code>invoice.sent</code> event.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\}/send *)
let postInvoicesInvoiceSend () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}/send" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" / s "send" /? nil)

(** <p>Deactivates a billing meter</p>
    
    @param id Unique identifier for the object.
    @see "openapi/spec3.json" /v1/billing/meters/\{id\}/deactivate *)
let postBillingMetersIdDeactivate () =
  Routes.(s "v1" / s "billing" / s "meters"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/billing/meters/{id}/deactivate" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "deactivate" /? nil)

(** <p>Updates the <code>status</code> of the specified testmode personalization design object to <code>rejected</code>.</p>
    
    @param personalization_design personalization_design
    @see "openapi/spec3.json" /v1/test_helpers/issuing/personalization_designs/\{personalization_design\}/reject *)
let postTestHelpersIssuingPersonalizationDesignsPersonalizationDesignReject () =
  Routes.(s "v1" / s "test_helpers" / s "issuing"
    / s "personalization_designs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"personalization_design" ~op:"/v1/test_helpers/issuing/personalization_designs/{personalization_design}/reject" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"personalization_design" ~loc:`Path ~style:`Simple ~explode:false)
        ":personalization_design" / s "reject" /? nil)

(** <p>Verifies microdeposits on a SetupIntent object.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/setup_intents/\{intent\}/verify_microdeposits *)
let postSetupIntentsIntentVerifyMicrodeposits () =
  Routes.(s "v1" / s "setup_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/setup_intents/{intent}/verify_microdeposits" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "verify_microdeposits" /? nil)

(** <p>Changes the settings on a customer’s cash balance.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cash_balance *)
let postCustomersCustomerCashBalance () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cash_balance" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cash_balance" /? nil)

(** <p>Retrieves a customer’s cash balance.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cash_balance *)
let getCustomersCustomerCashBalance () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cash_balance" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cash_balance" /? nil)

(** <p>Updates properties on a PaymentIntent object without confirming.</p>
    
    <p>Depending on which properties you update, you might need to confirm the
    PaymentIntent again. For example, updating the <code>payment_method</code>
    always requires you to confirm the PaymentIntent again. If you prefer to
    update and confirm at the same time, we recommend updating properties through
    the <a href="/docs/api/payment_intents/confirm">confirm API</a> instead.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\} *)
let postPaymentIntentsIntent () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" /? nil)

(** <p>Retrieves the details of a PaymentIntent that has previously been created. </p>
    
    <p>You can retrieve a PaymentIntent client-side using a publishable key when the <code>client_secret</code> is in the query string. </p>
    
    <p>If you retrieve a PaymentIntent with a publishable key, it only returns a subset of properties. Refer to the <a href="#payment_intent_object">payment intent</a> object reference for more details.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\} *)
let getPaymentIntentsIntent () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" /? nil)

(** <p>Retrieves a TransactionEntry object.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/treasury/transaction_entries/\{id\} *)
let getTreasuryTransactionEntriesId () =
  Routes.(s "v1" / s "treasury" / s "transaction_entries"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/treasury/transaction_entries/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Deletes an invoice item, removing it from an invoice. Deleting invoice items is only possible when they’re not attached to invoices, or if it’s attached to a draft invoice.</p>
    
    @param invoiceitem invoiceitem
    @see "openapi/spec3.json" /v1/invoiceitems/\{invoiceitem\} *)
let deleteInvoiceitemsInvoiceitem () =
  Routes.(s "v1" / s "invoiceitems"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoiceitem" ~op:"/v1/invoiceitems/{invoiceitem}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoiceitem" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoiceitem" /? nil)

(** <p>Updates the amount or description of an invoice item on an upcoming invoice. Updating an invoice item is only possible before the invoice it’s attached to is closed.</p>
    
    @param invoiceitem invoiceitem
    @see "openapi/spec3.json" /v1/invoiceitems/\{invoiceitem\} *)
let postInvoiceitemsInvoiceitem () =
  Routes.(s "v1" / s "invoiceitems"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoiceitem" ~op:"/v1/invoiceitems/{invoiceitem}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoiceitem" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoiceitem" /? nil)

(** <p>Retrieves the invoice item with the given ID.</p>
    
    @param invoiceitem invoiceitem
    @see "openapi/spec3.json" /v1/invoiceitems/\{invoiceitem\} *)
let getInvoiceitemsInvoiceitem () =
  Routes.(s "v1" / s "invoiceitems"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoiceitem" ~op:"/v1/invoiceitems/{invoiceitem}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoiceitem" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoiceitem" /? nil)

(** <p>Create an incoming testmode bank transfer</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/test_helpers/customers/\{customer\}/fund_cash_balance *)
let postTestHelpersCustomersCustomerFundCashBalance () =
  Routes.(s "v1" / s "test_helpers" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/test_helpers/customers/{customer}/fund_cash_balance" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "fund_cash_balance" /? nil)

(** <p>Updates the specified order by setting the values of the parameters passed.</p>
    
    @param order Unique identifier of the order.
    @see "openapi/spec3.json" /v1/climate/orders/\{order\} *)
let postClimateOrdersOrder () =
  Routes.(s "v1" / s "climate" / s "orders"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"order" ~op:"/v1/climate/orders/{order}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"order" ~loc:`Path ~style:`Simple ~explode:false)
        ":order" /? nil)

(** <p>Retrieves the details of a Climate order object with the given ID.</p>
    
    @param order Unique identifier of the order.
    @see "openapi/spec3.json" /v1/climate/orders/\{order\} *)
let getClimateOrdersOrder () =
  Routes.(s "v1" / s "climate" / s "orders"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"order" ~op:"/v1/climate/orders/{order}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"order" ~loc:`Path ~style:`Simple ~explode:false)
        ":order" /? nil)

(** <p>Retrieve funding instructions for a customer cash balance. If funding instructions do not yet exist for the customer, new
    funding instructions will be created. If funding instructions have already been created for a given customer, the same
    funding instructions will be retrieved. In other words, we will return the same funding instructions each time.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/funding_instructions *)
let postCustomersCustomerFundingInstructions () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/funding_instructions" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "funding_instructions" /? nil)

(** <p>Updates the specified payout by setting the values of the parameters you pass. We don’t change parameters that you don’t provide. This request only accepts the metadata as arguments.</p>
    
    @param payout payout
    @see "openapi/spec3.json" /v1/payouts/\{payout\} *)
let postPayoutsPayout () =
  Routes.(s "v1" / s "payouts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payout" ~op:"/v1/payouts/{payout}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payout" ~loc:`Path ~style:`Simple ~explode:false)
        ":payout" /? nil)

(** <p>Retrieves the details of an existing payout. Supply the unique payout ID from either a payout creation request or the payout list. Stripe returns the corresponding payout information.</p>
    
    @param payout payout
    @see "openapi/spec3.json" /v1/payouts/\{payout\} *)
let getPayoutsPayout () =
  Routes.(s "v1" / s "payouts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payout" ~op:"/v1/payouts/{payout}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payout" ~loc:`Path ~style:`Simple ~explode:false)
        ":payout" /? nil)

(** <p>Creates a new person.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/people *)
let postAccountsAccountPeople () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/people" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "people" /? nil)

(** <p>Returns a list of people associated with the account’s legal entity. The people are returned sorted by creation date, with the most recent people appearing first.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/people *)
let getAccountsAccountPeople () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/people" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "people" /? nil)

(** <p>Updates the details of a FinancialAccount.</p>
    
    @param financial_account financial_account
    @see "openapi/spec3.json" /v1/treasury/financial_accounts/\{financial_account\} *)
let postTreasuryFinancialAccountsFinancialAccount () =
  Routes.(s "v1" / s "treasury" / s "financial_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"financial_account" ~op:"/v1/treasury/financial_accounts/{financial_account}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"financial_account" ~loc:`Path ~style:`Simple ~explode:false)
        ":financial_account" /? nil)

(** <p>Retrieves the details of a FinancialAccount.</p>
    
    @param financial_account financial_account
    @see "openapi/spec3.json" /v1/treasury/financial_accounts/\{financial_account\} *)
let getTreasuryFinancialAccountsFinancialAccount () =
  Routes.(s "v1" / s "treasury" / s "financial_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"financial_account" ~op:"/v1/treasury/financial_accounts/{financial_account}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"financial_account" ~loc:`Path ~style:`Simple ~explode:false)
        ":financial_account" /? nil)

(** <p>Expire a refund with a status of <code>requires_action</code>.</p>
    
    @param refund refund
    @see "openapi/spec3.json" /v1/test_helpers/refunds/\{refund\}/expire *)
let postTestHelpersRefundsRefundExpire () =
  Routes.(s "v1" / s "test_helpers" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"refund" ~op:"/v1/test_helpers/refunds/{refund}/expire" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"refund" ~loc:`Path ~style:`Simple ~explode:false)
        ":refund" / s "expire" /? nil)

(** <p>Retrieves the details of an scheduled query run.</p>
    
    @param scheduled_query_run scheduled_query_run
    @see "openapi/spec3.json" /v1/sigma/scheduled_query_runs/\{scheduled_query_run\} *)
let getSigmaScheduledQueryRunsScheduledQueryRun () =
  Routes.(s "v1" / s "sigma" / s "scheduled_query_runs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"scheduled_query_run" ~op:"/v1/sigma/scheduled_query_runs/{scheduled_query_run}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"scheduled_query_run" ~loc:`Path ~style:`Simple ~explode:false)
        ":scheduled_query_run" /? nil)

(** <p>Refreshes the data associated with a Financial Connections <code>Account</code>.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/financial_connections/accounts/\{account\}/refresh *)
let postFinancialConnectionsAccountsAccountRefresh () =
  Routes.(s "v1" / s "financial_connections" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/financial_connections/accounts/{account}/refresh" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "refresh" /? nil)

(** <p>Confirm that your customer intends to pay with current or provided
    payment method. Upon confirmation, the PaymentIntent will attempt to initiate
    a payment.
    If the selected payment method requires additional authentication steps, the
    PaymentIntent will transition to the <code>requires_action</code> status and
    suggest additional actions via <code>next_action</code>. If payment fails,
    the PaymentIntent transitions to the <code>requires_payment_method</code> status or the
    <code>canceled</code> status if the confirmation limit is reached. If
    payment succeeds, the PaymentIntent will transition to the <code>succeeded</code>
    status (or <code>requires_capture</code>, if <code>capture_method</code> is set to <code>manual</code>).
    If the <code>confirmation_method</code> is <code>automatic</code>, payment may be attempted
    using our <a href="/docs/stripe-js/reference#stripe-handle-card-payment">client SDKs</a>
    and the PaymentIntent’s <a href="#payment_intent_object-client_secret">client_secret</a>.
    After <code>next_action</code>s are handled by the client, no additional
    confirmation is required to complete the payment.
    If the <code>confirmation_method</code> is <code>manual</code>, all payment attempts must be
    initiated using a secret key.
    If any actions are required for the payment, the PaymentIntent will
    return to the <code>requires_confirmation</code> state
    after those actions are completed. Your server needs to then
    explicitly re-confirm the PaymentIntent to initiate the next payment
    attempt.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\}/confirm *)
let postPaymentIntentsIntentConfirm () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}/confirm" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "confirm" /? nil)

(** <p>Updates a configuration that describes the functionality of the customer portal.</p>
    
    @param configuration configuration
    @see "openapi/spec3.json" /v1/billing_portal/configurations/\{configuration\} *)
let postBillingPortalConfigurationsConfiguration () =
  Routes.(s "v1" / s "billing_portal" / s "configurations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"configuration" ~op:"/v1/billing_portal/configurations/{configuration}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"configuration" ~loc:`Path ~style:`Simple ~explode:false)
        ":configuration" /? nil)

(** <p>Retrieves a configuration that describes the functionality of the customer portal.</p>
    
    @param configuration configuration
    @see "openapi/spec3.json" /v1/billing_portal/configurations/\{configuration\} *)
let getBillingPortalConfigurationsConfiguration () =
  Routes.(s "v1" / s "billing_portal" / s "configurations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"configuration" ~op:"/v1/billing_portal/configurations/{configuration}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"configuration" ~loc:`Path ~style:`Simple ~explode:false)
        ":configuration" /? nil)

(** <p>Deletes an existing account or customer <code>tax_id</code> object.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/tax_ids/\{id\} *)
let deleteTaxIdsId () =
  Routes.(s "v1" / s "tax_ids"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/tax_ids/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves an account or customer <code>tax_id</code> object.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/tax_ids/\{id\} *)
let getTaxIdsId () =
  Routes.(s "v1" / s "tax_ids"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/tax_ids/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>A quote models prices and services for a customer.</p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\} *)
let postQuotesQuote () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" /? nil)

(** <p>Retrieves the quote with the given ID.</p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\} *)
let getQuotesQuote () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" /? nil)

(** <p>Marks a credit note as void. Learn more about <a href="/docs/billing/invoices/credit-notes#voiding">voiding credit notes</a>.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/credit_notes/\{id\}/void *)
let postCreditNotesIdVoid () =
  Routes.(s "v1" / s "credit_notes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/credit_notes/{id}/void" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "void" /? nil)

(** <p>\[Deprecated\] Approves a pending Issuing <code>Authorization</code> object. This request should be made within the timeout window of the <a href="/docs/issuing/controls/real-time-authorizations">real-time authorization</a> flow. 
    This method is deprecated. Instead, <a href="/docs/issuing/controls/real-time-authorizations#authorization-handling">respond directly to the webhook request to approve an authorization</a>.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/issuing/authorizations/\{authorization\}/approve *)
let postIssuingAuthorizationsAuthorizationApprove () =
  Routes.(s "v1" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/issuing/authorizations/{authorization}/approve" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" / s "approve" /? nil)

(** <p>Retrieves a ForwardingRequest object.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/forwarding/requests/\{id\} *)
let getForwardingRequestsId () =
  Routes.(s "v1" / s "forwarding" / s "requests"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/forwarding/requests/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves the details of an early fraud warning that has previously been created. </p>
    
    <p>Please refer to the <a href="#early_fraud_warning_object">early fraud warning</a> object reference for more details.</p>
    
    @param early_fraud_warning early_fraud_warning
    @see "openapi/spec3.json" /v1/radar/early_fraud_warnings/\{early_fraud_warning\} *)
let getRadarEarlyFraudWarningsEarlyFraudWarning () =
  Routes.(s "v1" / s "radar" / s "early_fraud_warnings"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"early_fraud_warning" ~op:"/v1/radar/early_fraud_warnings/{early_fraud_warning}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"early_fraud_warning" ~loc:`Path ~style:`Simple ~explode:false)
        ":early_fraud_warning" /? nil)

(** <p>Updates a test mode created OutboundPayment with tracking details. The OutboundPayment must not be cancelable, and cannot be in the <code>canceled</code> or <code>failed</code> states.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_payments/\{id\} *)
let postTestHelpersTreasuryOutboundPaymentsId () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_payments"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/test_helpers/treasury/outbound_payments/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Deletes an item from the subscription. Removing a subscription item from a subscription will not cancel the subscription.</p>
    
    @param item item
    @see "openapi/spec3.json" /v1/subscription_items/\{item\} *)
let deleteSubscriptionItemsItem () =
  Routes.(s "v1" / s "subscription_items"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"item" ~op:"/v1/subscription_items/{item}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"item" ~loc:`Path ~style:`Simple ~explode:false)
        ":item" /? nil)

(** <p>Updates the plan or quantity of an item on a current subscription.</p>
    
    @param item item
    @see "openapi/spec3.json" /v1/subscription_items/\{item\} *)
let postSubscriptionItemsItem () =
  Routes.(s "v1" / s "subscription_items"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"item" ~op:"/v1/subscription_items/{item}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"item" ~loc:`Path ~style:`Simple ~explode:false)
        ":item" /? nil)

(** <p>Retrieves the subscription item with the given ID.</p>
    
    @param item item
    @see "openapi/spec3.json" /v1/subscription_items/\{item\} *)
let getSubscriptionItemsItem () =
  Routes.(s "v1" / s "subscription_items"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"item" ~op:"/v1/subscription_items/{item}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"item" ~loc:`Path ~style:`Simple ~explode:false)
        ":item" /? nil)

(** <p>When you create a new refund, you must specify either a Charge or a PaymentIntent object.</p>
    
    <p>This action refunds a previously created charge that’s not refunded yet.
    Funds are refunded to the credit or debit card that’s originally charged.</p>
    
    <p>You can optionally refund only part of a charge.
    You can repeat this until the entire charge is refunded.</p>
    
    <p>After you entirely refund a charge, you can’t refund it again.
    This method raises an error when it’s called on an already-refunded charge,
    or when you attempt to refund more money than is left on a charge.</p>
    
    @param charge The identifier of the charge to refund.
    @see "openapi/spec3.json" /v1/charges/\{charge\}/refund *)
let postChargesChargeRefund () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/refund" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "refund" /? nil)

(** <p>Submits an Issuing <code>Dispute</code> to the card network. Stripe validates that all evidence fields required for the dispute’s reason are present. For more details, see <a href="/docs/issuing/purchases/disputes#dispute-reasons-and-evidence">Dispute reasons and evidence</a>.</p>
    
    @param dispute dispute
    @see "openapi/spec3.json" /v1/issuing/disputes/\{dispute\}/submit *)
let postIssuingDisputesDisputeSubmit () =
  Routes.(s "v1" / s "issuing" / s "disputes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"dispute" ~op:"/v1/issuing/disputes/{dispute}/submit" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"dispute" ~loc:`Path ~style:`Simple ~explode:false)
        ":dispute" / s "submit" /? nil)

(** <p>When you create a new reversal, you must specify a transfer to create it on.</p>
    
    <p>When reversing transfers, you can optionally reverse part of the transfer. You can do so as many times as you wish until the entire transfer has been reversed.</p>
    
    <p>Once entirely reversed, a transfer can’t be reversed again. This method will return an error when called on an already-reversed transfer, or when trying to reverse more money than is left on a transfer.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/transfers/\{id\}/reversals *)
let postTransfersIdReversals () =
  Routes.(s "v1" / s "transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/transfers/{id}/reversals" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "reversals" /? nil)

(** <p>You can see a list of the reversals belonging to a specific transfer. Note that the 10 most recent reversals are always available by default on the transfer object. If you need more than those 10, you can use this API method and the <code>limit</code> and <code>starting_after</code> parameters to page through additional reversals.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/transfers/\{id\}/reversals *)
let getTransfersIdReversals () =
  Routes.(s "v1" / s "transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/transfers/{id}/reversals" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "reversals" /? nil)

(** <p>Returns a Country Spec for a given Country code.</p>
    
    @param country country
    @see "openapi/spec3.json" /v1/country_specs/\{country\} *)
let getCountrySpecsCountry () =
  Routes.(s "v1" / s "country_specs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"country" ~op:"/v1/country_specs/{country}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"country" ~loc:`Path ~style:`Simple ~explode:false)
        ":country" /? nil)

(** <p>Refreshes the data associated with a Financial Connections <code>Account</code>.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/linked_accounts/\{account\}/refresh *)
let postLinkedAccountsAccountRefresh () =
  Routes.(s "v1" / s "linked_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/linked_accounts/{account}/refresh" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "refresh" /? nil)

(** <p>Updates the shipping status of the specified Issuing <code>Card</code> object to <code>returned</code>.</p>
    
    @param card card
    @see "openapi/spec3.json" /v1/test_helpers/issuing/cards/\{card\}/shipping/return *)
let postTestHelpersIssuingCardsCardShippingReturn () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"card" ~op:"/v1/test_helpers/issuing/cards/{card}/shipping/return" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"card" ~loc:`Path ~style:`Simple ~explode:false)
        ":card" / s "shipping" / s "return" /? nil)

(** <p>With <a href="/connect">Connect</a>, you can reject accounts that you have flagged as suspicious.</p>
    
    <p>Only accounts where your platform is liable for negative account balances, which includes Custom and Express accounts, can be rejected. Test-mode accounts can be rejected at any time. Live-mode accounts can only be rejected after all balances are zero.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/reject *)
let postAccountsAccountReject () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/reject" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "reject" /? nil)

(** <p>When retrieving an invoice, you’ll get a <strong>lines</strong> property containing the total count of line items and the first handful of those items. There is also a URL where you can retrieve the full (paginated) list of line items.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\}/lines *)
let getInvoicesInvoiceLines () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}/lines" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" / s "lines" /? nil)

(** <p>Retrieves the details of an existing Transaction.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/treasury/transactions/\{id\} *)
let getTreasuryTransactionsId () =
  Routes.(s "v1" / s "treasury" / s "transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/treasury/transactions/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Capture a test-mode authorization.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/test_helpers/issuing/authorizations/\{authorization\}/capture *)
let postTestHelpersIssuingAuthorizationsAuthorizationCapture () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/test_helpers/issuing/authorizations/{authorization}/capture" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" / s "capture" /? nil)

(** <p>Reverses a payout by debiting the destination bank account. At this time, you can only reverse payouts for connected accounts to US bank accounts. If the payout is manual and in the <code>pending</code> status, use <code>/v1/payouts/:id/cancel</code> instead.</p>
    
    <p>By requesting a reversal through <code>/v1/payouts/:id/reverse</code>, you confirm that the authorized signatory of the selected bank account authorizes the debit on the bank account and that no other authorization is required.</p>
    
    @param payout payout
    @see "openapi/spec3.json" /v1/payouts/\{payout\}/reverse *)
let postPayoutsPayoutReverse () =
  Routes.(s "v1" / s "payouts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payout" ~op:"/v1/payouts/{payout}/reverse" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payout" ~loc:`Path ~style:`Simple ~explode:false)
        ":payout" / s "reverse" /? nil)

(** <p>Removes the currently applied discount on a subscription.</p>
    
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/subscriptions/\{subscription_exposed_id\}/discount *)
let deleteSubscriptionsSubscriptionExposedIdDiscount () =
  Routes.(s "v1" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/subscriptions/{subscription_exposed_id}/discount" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" / s "discount" /? nil)

(** <p>Creates a new person.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/persons *)
let postAccountsAccountPersons () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/persons" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "persons" /? nil)

(** <p>Returns a list of people associated with the account’s legal entity. The people are returned sorted by creation date, with the most recent people appearing first.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/persons *)
let getAccountsAccountPersons () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/persons" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "persons" /? nil)

(** <p>Cancels the quote.</p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\}/cancel *)
let postQuotesQuoteCancel () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" / s "cancel" /? nil)

(** <p>Updates an existing tax rate.</p>
    
    @param tax_rate tax_rate
    @see "openapi/spec3.json" /v1/tax_rates/\{tax_rate\} *)
let postTaxRatesTaxRate () =
  Routes.(s "v1" / s "tax_rates"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"tax_rate" ~op:"/v1/tax_rates/{tax_rate}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"tax_rate" ~loc:`Path ~style:`Simple ~explode:false)
        ":tax_rate" /? nil)

(** <p>Retrieves a tax rate with the given ID</p>
    
    @param tax_rate tax_rate
    @see "openapi/spec3.json" /v1/tax_rates/\{tax_rate\} *)
let getTaxRatesTaxRate () =
  Routes.(s "v1" / s "tax_rates"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"tax_rate" ~op:"/v1/tax_rates/{tax_rate}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"tax_rate" ~loc:`Path ~style:`Simple ~explode:false)
        ":tax_rate" /? nil)

(** <p>Updates the specified source by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    <p>This request accepts the <code>metadata</code> and <code>owner</code> as arguments. It is also possible to update type specific information for selected payment methods. Please refer to our <a href="/docs/sources">payment method guides</a> for more detail.</p>
    
    @param source source
    @see "openapi/spec3.json" /v1/sources/\{source\} *)
let postSourcesSource () =
  Routes.(s "v1" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"source" ~op:"/v1/sources/{source}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"source" ~loc:`Path ~style:`Simple ~explode:false)
        ":source" /? nil)

(** <p>Retrieves an existing source object. Supply the unique source ID from a source creation request and Stripe will return the corresponding up-to-date source object information.</p>
    
    @param source source
    @see "openapi/spec3.json" /v1/sources/\{source\} *)
let getSourcesSource () =
  Routes.(s "v1" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"source" ~op:"/v1/sources/{source}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"source" ~loc:`Path ~style:`Simple ~explode:false)
        ":source" /? nil)

(** <p>Unsubscribes from periodic refreshes of data associated with a Financial Connections <code>Account</code>.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/financial_connections/accounts/\{account\}/unsubscribe *)
let postFinancialConnectionsAccountsAccountUnsubscribe () =
  Routes.(s "v1" / s "financial_connections" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/financial_connections/accounts/{account}/unsubscribe" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "unsubscribe" /? nil)

(** <p>Verifies microdeposits on a PaymentIntent object.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\}/verify_microdeposits *)
let postPaymentIntentsIntentVerifyMicrodeposits () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}/verify_microdeposits" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "verify_microdeposits" /? nil)

(** <p>An OutboundTransfer can be canceled if the funds have not yet been paid out.</p>
    
    @param outbound_transfer outbound_transfer
    @see "openapi/spec3.json" /v1/treasury/outbound_transfers/\{outbound_transfer\}/cancel *)
let postTreasuryOutboundTransfersOutboundTransferCancel () =
  Routes.(s "v1" / s "treasury" / s "outbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"outbound_transfer" ~op:"/v1/treasury/outbound_transfers/{outbound_transfer}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"outbound_transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":outbound_transfer" / s "cancel" /? nil)

(** <p>Transitions a test mode created OutboundPayment to the <code>posted</code> status. The OutboundPayment must already be in the <code>processing</code> state.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_payments/\{id\}/post *)
let postTestHelpersTreasuryOutboundPaymentsIdPost () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_payments"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/test_helpers/treasury/outbound_payments/{id}/post" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "post" /? nil)

(** <p>Creates a usage record for a specified subscription item and date, and fills it with a quantity.</p>
    
    <p>Usage records provide <code>quantity</code> information that Stripe uses to track how much a customer is using your service. With usage information and the pricing model set up by the <a href="https://stripe.com/docs/billing/subscriptions/metered-billing">metered billing</a> plan, Stripe helps you send accurate invoices to your customers.</p>
    
    <p>The default calculation for usage is to add up all the <code>quantity</code> values of the usage records within a billing period. You can change this default behavior with the billing plan’s <code>aggregate_usage</code> <a href="/docs/api/plans/create#create_plan-aggregate_usage">parameter</a>. When there is more than one usage record with the same timestamp, Stripe adds the <code>quantity</code> values together. In most cases, this is the desired resolution, however, you can change this behavior with the <code>action</code> parameter.</p>
    
    <p>The default pricing model for metered billing is <a href="/docs/api/plans/object#plan_object-billing_scheme">per-unit pricing</a>. For finer granularity, you can configure metered billing to have a <a href="https://stripe.com/docs/billing/subscriptions/tiers">tiered pricing</a> model.</p>
    
    @param subscription_item subscription_item
    @see "openapi/spec3.json" /v1/subscription_items/\{subscription_item\}/usage_records *)
let postSubscriptionItemsSubscriptionItemUsageRecords () =
  Routes.(s "v1" / s "subscription_items"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_item" ~op:"/v1/subscription_items/{subscription_item}/usage_records" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_item" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_item" / s "usage_records" /? nil)

(** <p>Retrieves an existing VerificationReport</p>
    
    @param report report
    @see "openapi/spec3.json" /v1/identity/verification_reports/\{report\} *)
let getIdentityVerificationReportsReport () =
  Routes.(s "v1" / s "identity" / s "verification_reports"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"report" ~op:"/v1/identity/verification_reports/{report}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"report" ~loc:`Path ~style:`Simple ~explode:false)
        ":report" /? nil)

(** <p>Some payment methods such as Apple Pay require additional steps to verify a domain. If the requirements weren’t satisfied when the domain was created, the payment method will be inactive on the domain.
    The payment method doesn’t appear in Elements for this domain until it is active.</p>
    
    <p>To activate a payment method on an existing payment method domain, complete the required validation steps specific to the payment method, and then validate the payment method domain with this endpoint.</p>
    
    <p>Related guides: <a href="/docs/payments/payment-methods/pmd-registration">Payment method domains</a>.</p>
    
    @param payment_method_domain payment_method_domain
    @see "openapi/spec3.json" /v1/payment_method_domains/\{payment_method_domain\}/validate *)
let postPaymentMethodDomainsPaymentMethodDomainValidate () =
  Routes.(s "v1" / s "payment_method_domains"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method_domain" ~op:"/v1/payment_method_domains/{payment_method_domain}/validate" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method_domain" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method_domain" / s "validate" /? nil)

(** <p>Cancels the current reader action.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\}/cancel_action *)
let postTerminalReadersReaderCancelAction () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}/cancel_action" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" / s "cancel_action" /? nil)

(** <p>Deletes a <code>ValueListItem</code> object, removing it from its parent value list.</p>
    
    @param item item
    @see "openapi/spec3.json" /v1/radar/value_list_items/\{item\} *)
let deleteRadarValueListItemsItem () =
  Routes.(s "v1" / s "radar" / s "value_list_items"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"item" ~op:"/v1/radar/value_list_items/{item}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"item" ~loc:`Path ~style:`Simple ~explode:false)
        ":item" /? nil)

(** <p>Retrieves a <code>ValueListItem</code> object.</p>
    
    @param item item
    @see "openapi/spec3.json" /v1/radar/value_list_items/\{item\} *)
let getRadarValueListItemsItem () =
  Routes.(s "v1" / s "radar" / s "value_list_items"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"item" ~op:"/v1/radar/value_list_items/{item}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"item" ~loc:`Path ~style:`Simple ~explode:false)
        ":item" /? nil)

(** @param id id
    @see "openapi/spec3.json" /v1/application_fees/\{id\}/refund *)
let postApplicationFeesIdRefund () =
  Routes.(s "v1" / s "application_fees"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/application_fees/{id}/refund" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "refund" /? nil)

(** <p>Updates a card personalization object.</p>
    
    @param personalization_design personalization_design
    @see "openapi/spec3.json" /v1/issuing/personalization_designs/\{personalization_design\} *)
let postIssuingPersonalizationDesignsPersonalizationDesign () =
  Routes.(s "v1" / s "issuing" / s "personalization_designs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"personalization_design" ~op:"/v1/issuing/personalization_designs/{personalization_design}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"personalization_design" ~loc:`Path ~style:`Simple ~explode:false)
        ":personalization_design" /? nil)

(** <p>Retrieves a personalization design object.</p>
    
    @param personalization_design personalization_design
    @see "openapi/spec3.json" /v1/issuing/personalization_designs/\{personalization_design\} *)
let getIssuingPersonalizationDesignsPersonalizationDesign () =
  Routes.(s "v1" / s "issuing" / s "personalization_designs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"personalization_design" ~op:"/v1/issuing/personalization_designs/{personalization_design}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"personalization_design" ~loc:`Path ~style:`Simple ~explode:false)
        ":personalization_design" /? nil)

(** <p>When you create a new credit card, you must specify a customer or recipient on which to create it.</p>
    
    <p>If the card’s owner has no default card, then the new card will become the default.
    However, if the owner already has a default, then it will not change.
    To change the default, you should <a href="/docs/api#update_customer">update the customer</a> to have a new <code>default_source</code>.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cards *)
let postCustomersCustomerCards () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cards" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cards" /? nil)

(** <p>You can see a list of the cards belonging to a customer.
    Note that the 10 most recent sources are always available on the <code>Customer</code> object.
    If you need more than those 10, you can use this API method and the <code>limit</code> and <code>starting_after</code> parameters to page through additional cards.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cards *)
let getCustomersCustomerCards () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cards" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cards" /? nil)

(** <p>Delete a product. Deleting a product is only possible if it has no prices associated with it. Additionally, deleting a product with <code>type=good</code> is only possible if it has no SKUs associated with it.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/products/\{id\} *)
let deleteProductsId () =
  Routes.(s "v1" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/products/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates the specific product by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/products/\{id\} *)
let postProductsId () =
  Routes.(s "v1" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/products/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves the details of an existing product. Supply the unique product ID from either a product creation request or the product list, and Stripe will return the corresponding product information.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/products/\{id\} *)
let getProductsId () =
  Routes.(s "v1" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/products/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates the <code>status</code> of the specified testmode personalization design object to <code>active</code>.</p>
    
    @param personalization_design personalization_design
    @see "openapi/spec3.json" /v1/test_helpers/issuing/personalization_designs/\{personalization_design\}/activate *)
let postTestHelpersIssuingPersonalizationDesignsPersonalizationDesignActivate () =
  Routes.(s "v1" / s "test_helpers" / s "issuing"
    / s "personalization_designs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"personalization_design" ~op:"/v1/test_helpers/issuing/personalization_designs/{personalization_design}/activate" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"personalization_design" ~loc:`Path ~style:`Simple ~explode:false)
        ":personalization_design" / s "activate" /? nil)

(** <p>You can cancel a SetupIntent object when it’s in one of these statuses: <code>requires_payment_method</code>, <code>requires_confirmation</code>, or <code>requires_action</code>. </p>
    
    <p>After you cancel it, setup is abandoned and any operations on the SetupIntent fail with an error.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/setup_intents/\{intent\}/cancel *)
let postSetupIntentsIntentCancel () =
  Routes.(s "v1" / s "setup_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/setup_intents/{intent}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "cancel" /? nil)

(** <p>You can delete coupons via the <a href="https://dashboard.stripe.com/coupons">coupon management</a> page of the Stripe dashboard. However, deleting a coupon does not affect any customers who have already applied the coupon; it means that new customers can’t redeem the coupon. You can also delete coupons via the API.</p>
    
    @param coupon coupon
    @see "openapi/spec3.json" /v1/coupons/\{coupon\} *)
let deleteCouponsCoupon () =
  Routes.(s "v1" / s "coupons"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"coupon" ~op:"/v1/coupons/{coupon}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"coupon" ~loc:`Path ~style:`Simple ~explode:false)
        ":coupon" /? nil)

(** <p>Updates the metadata of a coupon. Other coupon details (currency, duration, amount_off) are, by design, not editable.</p>
    
    @param coupon coupon
    @see "openapi/spec3.json" /v1/coupons/\{coupon\} *)
let postCouponsCoupon () =
  Routes.(s "v1" / s "coupons"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"coupon" ~op:"/v1/coupons/{coupon}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"coupon" ~loc:`Path ~style:`Simple ~explode:false)
        ":coupon" /? nil)

(** <p>Retrieves the coupon with the given ID.</p>
    
    @param coupon coupon
    @see "openapi/spec3.json" /v1/coupons/\{coupon\} *)
let getCouponsCoupon () =
  Routes.(s "v1" / s "coupons"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"coupon" ~op:"/v1/coupons/{coupon}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"coupon" ~loc:`Path ~style:`Simple ~explode:false)
        ":coupon" /? nil)

(** <p>Marking an invoice as uncollectible is useful for keeping track of bad debts that can be written off for accounting purposes.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\}/mark_uncollectible *)
let postInvoicesInvoiceMarkUncollectible () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}/mark_uncollectible" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" / s "mark_uncollectible" /? nil)

(** <p>Retrieves the details of an existing ReceivedDebit by passing the unique ReceivedDebit ID from the ReceivedDebit list</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/treasury/received_debits/\{id\} *)
let getTreasuryReceivedDebitsId () =
  Routes.(s "v1" / s "treasury" / s "received_debits"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/treasury/received_debits/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Sets reader display to show cart details.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\}/set_reader_display *)
let postTerminalReadersReaderSetReaderDisplay () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}/set_reader_display" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" / s "set_reader_display" /? nil)

(** <p>Redact a VerificationSession to remove all collected information from Stripe. This will redact
    the VerificationSession and all objects related to it, including VerificationReports, Events,
    request logs, etc.</p>
    
    <p>A VerificationSession object can be redacted when it is in <code>requires_input</code> or <code>verified</code>
    <a href="/docs/identity/how-sessions-work">status</a>. Redacting a VerificationSession in <code>requires_action</code>
    state will automatically cancel it.</p>
    
    <p>The redaction process may take up to four days. When the redaction process is in progress, the
    VerificationSession’s <code>redaction.status</code> field will be set to <code>processing</code>; when the process is
    finished, it will change to <code>redacted</code> and an <code>identity.verification_session.redacted</code> event
    will be emitted.</p>
    
    <p>Redaction is irreversible. Redacted objects are still accessible in the Stripe API, but all the
    fields that contain personal data will be replaced by the string <code>\[redacted\]</code> or a similar
    placeholder. The <code>metadata</code> field will also be erased. Redacted objects cannot be updated or
    used for any purpose.</p>
    
    <p><a href="/docs/identity/verification-sessions#redact">Learn more</a>.</p>
    
    @param session session
    @see "openapi/spec3.json" /v1/identity/verification_sessions/\{session\}/redact *)
let postIdentityVerificationSessionsSessionRedact () =
  Routes.(s "v1" / s "identity" / s "verification_sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/identity/verification_sessions/{session}/redact" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" / s "redact" /? nil)

(** <p>Updates the refund that you specify by setting the values of the passed parameters. Any parameters that you don’t provide remain unchanged.</p>
    
    <p>This request only accepts <code>metadata</code> as an argument.</p>
    
    @param refund refund
    @see "openapi/spec3.json" /v1/refunds/\{refund\} *)
let postRefundsRefund () =
  Routes.(s "v1" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"refund" ~op:"/v1/refunds/{refund}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"refund" ~loc:`Path ~style:`Simple ~explode:false)
        ":refund" /? nil)

(** <p>Retrieves the details of an existing refund.</p>
    
    @param refund refund
    @see "openapi/spec3.json" /v1/refunds/\{refund\} *)
let getRefundsRefund () =
  Routes.(s "v1" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"refund" ~op:"/v1/refunds/{refund}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"refund" ~loc:`Path ~style:`Simple ~explode:false)
        ":refund" /? nil)

(** <p>When retrieving a Checkout Session, there is an includable <strong>line_items</strong> property containing the first handful of those items. There is also a URL where you can retrieve the full (paginated) list of line items.</p>
    
    @param session session
    @see "openapi/spec3.json" /v1/checkout/sessions/\{session\}/line_items *)
let getCheckoutSessionsSessionLineItems () =
  Routes.(s "v1" / s "checkout" / s "sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/checkout/sessions/{session}/line_items" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" / s "line_items" /? nil)

(** <p>Transitions a test mode created OutboundTransfer to the <code>posted</code> status. The OutboundTransfer must already be in the <code>processing</code> state.</p>
    
    @param outbound_transfer outbound_transfer
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_transfers/\{outbound_transfer\}/post *)
let postTestHelpersTreasuryOutboundTransfersOutboundTransferPost () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"outbound_transfer" ~op:"/v1/test_helpers/treasury/outbound_transfers/{outbound_transfer}/post" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"outbound_transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":outbound_transfer" / s "post" /? nil)

(** <p>Releases the subscription schedule immediately, which will stop scheduling of its phases, but leave any existing subscription in place. A schedule can only be released if its status is <code>not_started</code> or <code>active</code>. If the subscription schedule is currently associated with a subscription, releasing it will remove its <code>subscription</code> property and set the subscription’s ID to the <code>released_subscription</code> property.</p>
    
    @param schedule schedule
    @see "openapi/spec3.json" /v1/subscription_schedules/\{schedule\}/release *)
let postSubscriptionSchedulesScheduleRelease () =
  Routes.(s "v1" / s "subscription_schedules"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"schedule" ~op:"/v1/subscription_schedules/{schedule}/release" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"schedule" ~loc:`Path ~style:`Simple ~explode:false)
        ":schedule" / s "release" /? nil)

(** <p>When retrieving a credit note, you’ll get a <strong>lines</strong> property containing the first handful of those items. There is also a URL where you can retrieve the full (paginated) list of line items.</p>
    
    @param credit_note credit_note
    @see "openapi/spec3.json" /v1/credit_notes/\{credit_note\}/lines *)
let getCreditNotesCreditNoteLines () =
  Routes.(s "v1" / s "credit_notes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"credit_note" ~op:"/v1/credit_notes/{credit_note}/lines" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"credit_note" ~loc:`Path ~style:`Simple ~explode:false)
        ":credit_note" / s "lines" /? nil)

(** <p>Detaches a PaymentMethod object from a Customer. After a PaymentMethod is detached, it can no longer be used for a payment or re-attached to a Customer.</p>
    
    @param payment_method payment_method
    @see "openapi/spec3.json" /v1/payment_methods/\{payment_method\}/detach *)
let postPaymentMethodsPaymentMethodDetach () =
  Routes.(s "v1" / s "payment_methods"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method" ~op:"/v1/payment_methods/{payment_method}/detach" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method" / s "detach" /? nil)

(** <p>Retrieves a DebitReversal object.</p>
    
    @param debit_reversal debit_reversal
    @see "openapi/spec3.json" /v1/treasury/debit_reversals/\{debit_reversal\} *)
let getTreasuryDebitReversalsDebitReversal () =
  Routes.(s "v1" / s "treasury" / s "debit_reversals"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"debit_reversal" ~op:"/v1/treasury/debit_reversals/{debit_reversal}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"debit_reversal" ~loc:`Path ~style:`Simple ~explode:false)
        ":debit_reversal" /? nil)

(** <p>Retrieves the details of an existing tax code. Supply the unique tax code ID and Stripe will return the corresponding tax code information.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/tax_codes/\{id\} *)
let getTaxCodesId () =
  Routes.(s "v1" / s "tax_codes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/tax_codes/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Disables your access to a Financial Connections <code>Account</code>. You will no longer be able to access data associated with the account (e.g. balances, transactions).</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/financial_connections/accounts/\{account\}/disconnect *)
let postFinancialConnectionsAccountsAccountDisconnect () =
  Routes.(s "v1" / s "financial_connections" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/financial_connections/accounts/{account}/disconnect" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "disconnect" /? nil)

(** <p>Updates the specified promotion code by setting the values of the parameters passed. Most fields are, by design, not editable.</p>
    
    @param promotion_code promotion_code
    @see "openapi/spec3.json" /v1/promotion_codes/\{promotion_code\} *)
let postPromotionCodesPromotionCode () =
  Routes.(s "v1" / s "promotion_codes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"promotion_code" ~op:"/v1/promotion_codes/{promotion_code}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"promotion_code" ~loc:`Path ~style:`Simple ~explode:false)
        ":promotion_code" /? nil)

(** <p>Retrieves the promotion code with the given ID. In order to retrieve a promotion code by the customer-facing <code>code</code> use <a href="/docs/api/promotion_codes/list">list</a> with the desired <code>code</code>.</p>
    
    @param promotion_code promotion_code
    @see "openapi/spec3.json" /v1/promotion_codes/\{promotion_code\} *)
let getPromotionCodesPromotionCode () =
  Routes.(s "v1" / s "promotion_codes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"promotion_code" ~op:"/v1/promotion_codes/{promotion_code}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"promotion_code" ~loc:`Path ~style:`Simple ~explode:false)
        ":promotion_code" /? nil)

(** <p>Updates an existing shipping rate object.</p>
    
    @param shipping_rate_token shipping_rate_token
    @see "openapi/spec3.json" /v1/shipping_rates/\{shipping_rate_token\} *)
let postShippingRatesShippingRateToken () =
  Routes.(s "v1" / s "shipping_rates"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"shipping_rate_token" ~op:"/v1/shipping_rates/{shipping_rate_token}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"shipping_rate_token" ~loc:`Path ~style:`Simple ~explode:false)
        ":shipping_rate_token" /? nil)

(** <p>Returns the shipping rate object with the given ID.</p>
    
    @param shipping_rate_token shipping_rate_token
    @see "openapi/spec3.json" /v1/shipping_rates/\{shipping_rate_token\} *)
let getShippingRatesShippingRateToken () =
  Routes.(s "v1" / s "shipping_rates"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"shipping_rate_token" ~op:"/v1/shipping_rates/{shipping_rate_token}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"shipping_rate_token" ~loc:`Path ~style:`Simple ~explode:false)
        ":shipping_rate_token" /? nil)

(** <p>Reactivates a billing meter</p>
    
    @param id Unique identifier for the object.
    @see "openapi/spec3.json" /v1/billing/meters/\{id\}/reactivate *)
let postBillingMetersIdReactivate () =
  Routes.(s "v1" / s "billing" / s "meters"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/billing/meters/{id}/reactivate" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "reactivate" /? nil)

(** <p>You can cancel a PaymentIntent object when it’s in one of these statuses: <code>requires_payment_method</code>, <code>requires_capture</code>, <code>requires_confirmation</code>, <code>requires_action</code> or, <a href="/docs/payments/intents">in rare cases</a>, <code>processing</code>. </p>
    
    <p>After it’s canceled, no additional charges are made by the PaymentIntent and any operations on the PaymentIntent fail with an error. For PaymentIntents with a <code>status</code> of <code>requires_capture</code>, the remaining <code>amount_capturable</code> is automatically refunded. </p>
    
    <p>You can’t cancel the PaymentIntent for a Checkout Session. <a href="/docs/api/checkout/sessions/expire">Expire the Checkout Session</a> instead.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\}/cancel *)
let postPaymentIntentsIntentCancel () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "cancel" /? nil)

(** <p>Retrieves the details of an existing OutboundPayment by passing the unique OutboundPayment ID from either the OutboundPayment creation request or OutboundPayment list.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/treasury/outbound_payments/\{id\} *)
let getTreasuryOutboundPaymentsId () =
  Routes.(s "v1" / s "treasury" / s "outbound_payments"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/treasury/outbound_payments/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves the details of a Financial Connections <code>Transaction</code></p>
    
    @param transaction transaction
    @see "openapi/spec3.json" /v1/financial_connections/transactions/\{transaction\} *)
let getFinancialConnectionsTransactionsTransaction () =
  Routes.(s "v1" / s "financial_connections" / s "transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/financial_connections/transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" /? nil)

(** <p>Download the PDF for a finalized quote. Explanation for special handling can be found <a href="https://docs.corp.stripe.com/quotes/overview#quote_pdf">here</a></p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\}/pdf *)
let getQuotesQuotePdf () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}/pdf" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" / s "pdf" /? nil)

(** @param charge charge
    @see "openapi/spec3.json" /v1/charges/\{charge\}/dispute *)
let postChargesChargeDispute () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/dispute" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "dispute" /? nil)

(** <p>Retrieve a dispute for a specified charge.</p>
    
    @param charge charge
    @see "openapi/spec3.json" /v1/charges/\{charge\}/dispute *)
let getChargesChargeDispute () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/dispute" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "dispute" /? nil)

(** <p>Marks the test mode InboundTransfer object as returned and links the InboundTransfer to a ReceivedDebit. The InboundTransfer must already be in the <code>succeeded</code> state.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/test_helpers/treasury/inbound_transfers/\{id\}/return *)
let postTestHelpersTreasuryInboundTransfersIdReturn () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "inbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/test_helpers/treasury/inbound_transfers/{id}/return" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "return" /? nil)

(** <p>Retrieves an existing ConfirmationToken object</p>
    
    @param confirmation_token confirmation_token
    @see "openapi/spec3.json" /v1/confirmation_tokens/\{confirmation_token\} *)
let getConfirmationTokensConfirmationToken () =
  Routes.(s "v1" / s "confirmation_tokens"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"confirmation_token" ~op:"/v1/confirmation_tokens/{confirmation_token}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"confirmation_token" ~loc:`Path ~style:`Simple ~explode:false)
        ":confirmation_token" /? nil)

(** <p>Verify a given source.</p>
    
    @param source source
    @see "openapi/spec3.json" /v1/sources/\{source\}/verify *)
let postSourcesSourceVerify () =
  Routes.(s "v1" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"source" ~op:"/v1/sources/{source}/verify" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"source" ~loc:`Path ~style:`Simple ~explode:false)
        ":source" / s "verify" /? nil)

(** <p>Cancels a top-up. Only pending top-ups can be canceled.</p>
    
    @param topup topup
    @see "openapi/spec3.json" /v1/topups/\{topup\}/cancel *)
let postTopupsTopupCancel () =
  Routes.(s "v1" / s "topups"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"topup" ~op:"/v1/topups/{topup}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"topup" ~loc:`Path ~style:`Simple ~explode:false)
        ":topup" / s "cancel" /? nil)

(** <p>Creates a new subscription on an existing customer.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/subscriptions *)
let postCustomersCustomerSubscriptions () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/subscriptions" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "subscriptions" /? nil)

(** <p>You can see a list of the customer’s active subscriptions. Note that the 10 most recent active subscriptions are always available by default on the customer object. If you need more than those 10, you can use the limit and starting_after parameters to page through additional subscriptions.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/subscriptions *)
let getCustomersCustomerSubscriptions () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/subscriptions" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "subscriptions" /? nil)

(** <p>Disables your access to a Financial Connections <code>Account</code>. You will no longer be able to access data associated with the account (e.g. balances, transactions).</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/linked_accounts/\{account\}/disconnect *)
let postLinkedAccountsAccountDisconnect () =
  Routes.(s "v1" / s "linked_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/linked_accounts/{account}/disconnect" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "disconnect" /? nil)

(** <p>Updates the shipping status of the specified Issuing <code>Card</code> object to <code>delivered</code>.</p>
    
    @param card card
    @see "openapi/spec3.json" /v1/test_helpers/issuing/cards/\{card\}/shipping/deliver *)
let postTestHelpersIssuingCardsCardShippingDeliver () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"card" ~op:"/v1/test_helpers/issuing/cards/{card}/shipping/deliver" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"card" ~loc:`Path ~style:`Simple ~explode:false)
        ":card" / s "shipping" / s "deliver" /? nil)

(** <p>Approves a <code>Review</code> object, closing it and removing it from the list of reviews.</p>
    
    @param review review
    @see "openapi/spec3.json" /v1/reviews/\{review\}/approve *)
let postReviewsReviewApprove () =
  Routes.(s "v1" / s "reviews"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"review" ~op:"/v1/reviews/{review}/approve" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"review" ~loc:`Path ~style:`Simple ~explode:false)
        ":review" / s "approve" /? nil)

(** <p>Delete an apple pay domain.</p>
    
    @param domain domain
    @see "openapi/spec3.json" /v1/apple_pay/domains/\{domain\} *)
let deleteApplePayDomainsDomain () =
  Routes.(s "v1" / s "apple_pay" / s "domains"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"domain" ~op:"/v1/apple_pay/domains/{domain}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"domain" ~loc:`Path ~style:`Simple ~explode:false)
        ":domain" /? nil)

(** <p>Retrieve an apple pay domain.</p>
    
    @param domain domain
    @see "openapi/spec3.json" /v1/apple_pay/domains/\{domain\} *)
let getApplePayDomainsDomain () =
  Routes.(s "v1" / s "apple_pay" / s "domains"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"domain" ~op:"/v1/apple_pay/domains/{domain}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"domain" ~loc:`Path ~style:`Simple ~explode:false)
        ":domain" /? nil)

(** <p>Permanently deletes a one-off invoice draft. This cannot be undone. Attempts to delete invoices that are no longer in a draft state will fail; once an invoice has been finalized or if an invoice is for a subscription, it must be <a href="#void_invoice">voided</a>.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\} *)
let deleteInvoicesInvoice () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" /? nil)

(** <p>Draft invoices are fully editable. Once an invoice is <a href="/docs/billing/invoices/workflow#finalized">finalized</a>,
    monetary values, as well as <code>collection_method</code>, become uneditable.</p>
    
    <p>If you would like to stop the Stripe Billing engine from automatically finalizing, reattempting payments on,
    sending reminders for, or <a href="/docs/billing/invoices/reconciliation">automatically reconciling</a> invoices, pass
    <code>auto_advance=false</code>.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\} *)
let postInvoicesInvoice () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" /? nil)

(** <p>Retrieves the invoice with the given ID.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\} *)
let getInvoicesInvoice () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" /? nil)

(** <p>Expire a test-mode Authorization.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/test_helpers/issuing/authorizations/\{authorization\}/expire *)
let postTestHelpersIssuingAuthorizationsAuthorizationExpire () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/test_helpers/issuing/authorizations/{authorization}/expire" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" / s "expire" /? nil)

(** <p>Retrieves the details of a Financial Connections <code>Session</code></p>
    
    @param session session
    @see "openapi/spec3.json" /v1/link_account_sessions/\{session\} *)
let getLinkAccountSessionsSession () =
  Routes.(s "v1" / s "link_account_sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/link_account_sessions/{session}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" /? nil)

(** <p>Retrieves the details of a Report Type. (Certain report types require a <a href="https://stripe.com/docs/keys#test-live-modes">live-mode API key</a>.)</p>
    
    @param report_type report_type
    @see "openapi/spec3.json" /v1/reporting/report_types/\{report_type\} *)
let getReportingReportTypesReportType () =
  Routes.(s "v1" / s "reporting" / s "report_types"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"report_type" ~op:"/v1/reporting/report_types/{report_type}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"report_type" ~loc:`Path ~style:`Simple ~explode:false)
        ":report_type" /? nil)

(** <p>Invalidates a short-lived API key for a given resource.</p>
    
    @param key key
    @see "openapi/spec3.json" /v1/ephemeral_keys/\{key\} *)
let deleteEphemeralKeysKey () =
  Routes.(s "v1" / s "ephemeral_keys"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"key" ~op:"/v1/ephemeral_keys/{key}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"key" ~loc:`Path ~style:`Simple ~explode:false)
        ":key" /? nil)

(** <p>Retrieves the token with the given ID.</p>
    
    @param token token
    @see "openapi/spec3.json" /v1/tokens/\{token\} *)
let getTokensToken () =
  Routes.(s "v1" / s "tokens"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"token" ~op:"/v1/tokens/{token}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"token" ~loc:`Path ~style:`Simple ~explode:false)
        ":token" /? nil)

(** <p>Initiates resumption of a paused subscription, optionally resetting the billing cycle anchor and creating prorations. If a resumption invoice is generated, it must be paid or marked uncollectible before the subscription will be unpaused. If payment succeeds the subscription will become <code>active</code>, and if payment fails the subscription will be <code>past_due</code>. The resumption invoice will void automatically if not paid by the expiration date.</p>
    
    @param subscription subscription
    @see "openapi/spec3.json" /v1/subscriptions/\{subscription\}/resume *)
let postSubscriptionsSubscriptionResume () =
  Routes.(s "v1" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription" ~op:"/v1/subscriptions/{subscription}/resume" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription" / s "resume" /? nil)

(** <p>Retrieves the details of an existing InboundTransfer.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/treasury/inbound_transfers/\{id\} *)
let getTreasuryInboundTransfersId () =
  Routes.(s "v1" / s "treasury" / s "inbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/treasury/inbound_transfers/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates the specified Issuing <code>Cardholder</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param cardholder cardholder
    @see "openapi/spec3.json" /v1/issuing/cardholders/\{cardholder\} *)
let postIssuingCardholdersCardholder () =
  Routes.(s "v1" / s "issuing" / s "cardholders"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"cardholder" ~op:"/v1/issuing/cardholders/{cardholder}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"cardholder" ~loc:`Path ~style:`Simple ~explode:false)
        ":cardholder" /? nil)

(** <p>Retrieves an Issuing <code>Cardholder</code> object.</p>
    
    @param cardholder cardholder
    @see "openapi/spec3.json" /v1/issuing/cardholders/\{cardholder\} *)
let getIssuingCardholdersCardholder () =
  Routes.(s "v1" / s "issuing" / s "cardholders"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"cardholder" ~op:"/v1/issuing/cardholders/{cardholder}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"cardholder" ~loc:`Path ~style:`Simple ~explode:false)
        ":cardholder" /? nil)

(** <p>When retrieving a quote, there is an includable <a href="https://stripe.com/docs/api/quotes/object#quote_object-computed-upfront-line_items"><strong>computed.upfront.line_items</strong></a> property containing the first handful of those items. There is also a URL where you can retrieve the full (paginated) list of upfront line items.</p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\}/computed_upfront_line_items *)
let getQuotesQuoteComputedUpfrontLineItems () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}/computed_upfront_line_items" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" / s "computed_upfront_line_items" /? nil)

(** <p>Retrieves the details of a Climate product with the given ID.</p>
    
    @param product product
    @see "openapi/spec3.json" /v1/climate/products/\{product\} *)
let getClimateProductsProduct () =
  Routes.(s "v1" / s "climate" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"product" ~op:"/v1/climate/products/{product}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"product" ~loc:`Path ~style:`Simple ~explode:false)
        ":product" /? nil)

(** <p>Deletes a test clock.</p>
    
    @param test_clock test_clock
    @see "openapi/spec3.json" /v1/test_helpers/test_clocks/\{test_clock\} *)
let deleteTestHelpersTestClocksTestClock () =
  Routes.(s "v1" / s "test_helpers" / s "test_clocks"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"test_clock" ~op:"/v1/test_helpers/test_clocks/{test_clock}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"test_clock" ~loc:`Path ~style:`Simple ~explode:false)
        ":test_clock" /? nil)

(** <p>Retrieves a test clock.</p>
    
    @param test_clock test_clock
    @see "openapi/spec3.json" /v1/test_helpers/test_clocks/\{test_clock\} *)
let getTestHelpersTestClocksTestClock () =
  Routes.(s "v1" / s "test_helpers" / s "test_clocks"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"test_clock" ~op:"/v1/test_helpers/test_clocks/{test_clock}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"test_clock" ~loc:`Path ~style:`Simple ~explode:false)
        ":test_clock" /? nil)

(** <p>When you create a new credit card, you must specify a customer or recipient on which to create it.</p>
    
    <p>If the card’s owner has no default card, then the new card will become the default.
    However, if the owner already has a default, then it will not change.
    To change the default, you should <a href="/docs/api#update_customer">update the customer</a> to have a new <code>default_source</code>.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/sources *)
let postCustomersCustomerSources () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/sources" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "sources" /? nil)

(** <p>List sources for a specified customer.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/sources *)
let getCustomersCustomerSources () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/sources" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "sources" /? nil)

(** <p>Initiates a payment flow on a Reader.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\}/process_payment_intent *)
let postTerminalReadersReaderProcessPaymentIntent () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}/process_payment_intent" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" / s "process_payment_intent" /? nil)

(** <p>Transitions a test mode created OutboundPayment to the <code>returned</code> status. The OutboundPayment must already be in the <code>processing</code> state.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_payments/\{id\}/return *)
let postTestHelpersTreasuryOutboundPaymentsIdReturn () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_payments"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/test_helpers/treasury/outbound_payments/{id}/return" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "return" /? nil)

(** <p>Creates a single-use login link for a connected account to access the Express Dashboard.</p>
    
    <p><strong>You can only create login links for accounts that use the <a href="/connect/express-dashboard">Express Dashboard</a> and are connected to your platform</strong>.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/login_links *)
let postAccountsAccountLoginLinks () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/login_links" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "login_links" /? nil)

(** <p>Retrieves a Tax <code>Transaction</code> object.</p>
    
    @param transaction transaction
    @see "openapi/spec3.json" /v1/tax/transactions/\{transaction\} *)
let getTaxTransactionsTransaction () =
  Routes.(s "v1" / s "tax" / s "transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/tax/transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" /? nil)

(** <p>Retrieves the details of an existing file object. After you supply a unique file ID, Stripe returns the corresponding file object. Learn how to <a href="/docs/file-upload#download-file-contents">access file contents</a>.</p>
    
    @param file file
    @see "openapi/spec3.json" /v1/files/\{file\} *)
let getFilesFile () =
  Routes.(s "v1" / s "files"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"file" ~op:"/v1/files/{file}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"file" ~loc:`Path ~style:`Simple ~explode:false)
        ":file" /? nil)

(** <p>Creates a product_feature, which represents a feature attachment to a product</p>
    
    @param product product
    @see "openapi/spec3.json" /v1/products/\{product\}/features *)
let postProductsProductFeatures () =
  Routes.(s "v1" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"product" ~op:"/v1/products/{product}/features" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"product" ~loc:`Path ~style:`Simple ~explode:false)
        ":product" / s "features" /? nil)

(** <p>Retrieve a list of features for a product</p>
    
    @param product product
    @see "openapi/spec3.json" /v1/products/\{product\}/features *)
let getProductsProductFeatures () =
  Routes.(s "v1" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"product" ~op:"/v1/products/{product}/features" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"product" ~loc:`Path ~style:`Simple ~explode:false)
        ":product" / s "features" /? nil)

(** <p>Updates the <code>status</code> of the specified testmode personalization design object to <code>inactive</code>.</p>
    
    @param personalization_design personalization_design
    @see "openapi/spec3.json" /v1/test_helpers/issuing/personalization_designs/\{personalization_design\}/deactivate *)
let postTestHelpersIssuingPersonalizationDesignsPersonalizationDesignDeactivate () =
  Routes.(s "v1" / s "test_helpers" / s "issuing"
    / s "personalization_designs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"personalization_design" ~op:"/v1/test_helpers/issuing/personalization_designs/{personalization_design}/deactivate" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"personalization_design" ~loc:`Path ~style:`Simple ~explode:false)
        ":personalization_design" / s "deactivate" /? nil)

(** <p>Confirm that your customer intends to set up the current or
    provided payment method. For example, you would confirm a SetupIntent
    when a customer hits the “Save” button on a payment method management
    page on your website.</p>
    
    <p>If the selected payment method does not require any additional
    steps from the customer, the SetupIntent will transition to the
    <code>succeeded</code> status.</p>
    
    <p>Otherwise, it will transition to the <code>requires_action</code> status and
    suggest additional actions via <code>next_action</code>. If setup fails,
    the SetupIntent will transition to the
    <code>requires_payment_method</code> status or the <code>canceled</code> status if the
    confirmation limit is reached.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/setup_intents/\{intent\}/confirm *)
let postSetupIntentsIntentConfirm () =
  Routes.(s "v1" / s "setup_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/setup_intents/{intent}/confirm" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "confirm" /? nil)

(** <p>Updates a billing meter</p>
    
    @param id Unique identifier for the object.
    @see "openapi/spec3.json" /v1/billing/meters/\{id\} *)
let postBillingMetersId () =
  Routes.(s "v1" / s "billing" / s "meters"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/billing/meters/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves a billing meter given an ID</p>
    
    @param id Unique identifier for the object.
    @see "openapi/spec3.json" /v1/billing/meters/\{id\} *)
let getBillingMetersId () =
  Routes.(s "v1" / s "billing" / s "meters"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/billing/meters/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Stripe automatically creates and then attempts to collect payment on invoices for customers on subscriptions according to your <a href="https://dashboard.stripe.com/account/billing/automatic">subscriptions settings</a>. However, if you’d like to attempt payment on an invoice out of the normal collection schedule or for some other reason, you can do so.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\}/pay *)
let postInvoicesInvoicePay () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}/pay" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" / s "pay" /? nil)

(** <p>Attempts to update the specified Issuing <code>Token</code> object to the status specified.</p>
    
    @param token token
    @see "openapi/spec3.json" /v1/issuing/tokens/\{token\} *)
let postIssuingTokensToken () =
  Routes.(s "v1" / s "issuing" / s "tokens"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"token" ~op:"/v1/issuing/tokens/{token}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"token" ~loc:`Path ~style:`Simple ~explode:false)
        ":token" /? nil)

(** <p>Retrieves an Issuing <code>Token</code> object.</p>
    
    @param token token
    @see "openapi/spec3.json" /v1/issuing/tokens/\{token\} *)
let getIssuingTokensToken () =
  Routes.(s "v1" / s "issuing" / s "tokens"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"token" ~op:"/v1/issuing/tokens/{token}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"token" ~loc:`Path ~style:`Simple ~explode:false)
        ":token" /? nil)

(** <p>Cancels a refund with a status of <code>requires_action</code>.</p>
    
    <p>You can’t cancel refunds in other states. Only refunds for payment methods that require customer action can enter the <code>requires_action</code> state.</p>
    
    @param refund refund
    @see "openapi/spec3.json" /v1/refunds/\{refund\}/cancel *)
let postRefundsRefundCancel () =
  Routes.(s "v1" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"refund" ~op:"/v1/refunds/{refund}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"refund" ~loc:`Path ~style:`Simple ~explode:false)
        ":refund" / s "cancel" /? nil)

(** <p>Updates an existing credit note.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/credit_notes/\{id\} *)
let postCreditNotesId () =
  Routes.(s "v1" / s "credit_notes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/credit_notes/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves the credit note object with the given identifier.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/credit_notes/\{id\} *)
let getCreditNotesId () =
  Routes.(s "v1" / s "credit_notes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/credit_notes/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Transitions a test mode created OutboundTransfer to the <code>returned</code> status. The OutboundTransfer must already be in the <code>processing</code> state.</p>
    
    @param outbound_transfer outbound_transfer
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_transfers/\{outbound_transfer\}/return *)
let postTestHelpersTreasuryOutboundTransfersOutboundTransferReturn () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"outbound_transfer" ~op:"/v1/test_helpers/treasury/outbound_transfers/{outbound_transfer}/return" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"outbound_transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":outbound_transfer" / s "return" /? nil)

(** <p>Retrieve an active entitlement</p>
    
    @param id The ID of the entitlement.
    @see "openapi/spec3.json" /v1/entitlements/active_entitlements/\{id\} *)
let getEntitlementsActiveEntitlementsId () =
  Routes.(s "v1" / s "entitlements" / s "active_entitlements"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/entitlements/active_entitlements/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Returns a list of capabilities associated with the account. The capabilities are returned sorted by creation date, with the most recent capability appearing first.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/capabilities *)
let getAccountsAccountCapabilities () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/capabilities" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "capabilities" /? nil)

(** <p>Lists all owners for a given <code>Account</code></p>
    
    @param account account
    @see "openapi/spec3.json" /v1/financial_connections/accounts/\{account\}/owners *)
let getFinancialConnectionsAccountsAccountOwners () =
  Routes.(s "v1" / s "financial_connections" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/financial_connections/accounts/{account}/owners" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "owners" /? nil)

(** <p>Capture the funds of an existing uncaptured PaymentIntent when its status is <code>requires_capture</code>.</p>
    
    <p>Uncaptured PaymentIntents are cancelled a set number of days (7 by default) after their creation.</p>
    
    <p>Learn more about <a href="/docs/payments/capture-later">separate authorization and capture</a>.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\}/capture *)
let postPaymentIntentsIntentCapture () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}/capture" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "capture" /? nil)

(** <p>Retrieves the balance transaction with the given ID.</p>
    
    <p>Note that this endpoint previously used the path <code>/v1/balance/history/:id</code>.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/balance_transactions/\{id\} *)
let getBalanceTransactionsId () =
  Routes.(s "v1" / s "balance_transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/balance_transactions/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Refund a test-mode Transaction.</p>
    
    @param transaction transaction
    @see "openapi/spec3.json" /v1/test_helpers/issuing/transactions/\{transaction\}/refund *)
let postTestHelpersIssuingTransactionsTransactionRefund () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/test_helpers/issuing/transactions/{transaction}/refund" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" / s "refund" /? nil)

(** <p>Removes the currently applied discount on a customer.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/discount *)
let deleteCustomersCustomerDiscount () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/discount" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "discount" /? nil)

(** @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/discount *)
let getCustomersCustomerDiscount () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/discount" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "discount" /? nil)

(** <p>Updates the specified Issuing <code>Authorization</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/issuing/authorizations/\{authorization\} *)
let postIssuingAuthorizationsAuthorization () =
  Routes.(s "v1" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/issuing/authorizations/{authorization}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" /? nil)

(** <p>Retrieves an Issuing <code>Authorization</code> object.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/issuing/authorizations/\{authorization\} *)
let getIssuingAuthorizationsAuthorization () =
  Routes.(s "v1" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/issuing/authorizations/{authorization}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" /? nil)

(** <p>Cancel an OutboundPayment.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/treasury/outbound_payments/\{id\}/cancel *)
let postTreasuryOutboundPaymentsIdCancel () =
  Routes.(s "v1" / s "treasury" / s "outbound_payments"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/treasury/outbound_payments/{id}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "cancel" /? nil)

(** <p>Updates the specified Issuing <code>Dispute</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged. Properties on the <code>evidence</code> object can be unset by passing in an empty string.</p>
    
    @param dispute dispute
    @see "openapi/spec3.json" /v1/issuing/disputes/\{dispute\} *)
let postIssuingDisputesDispute () =
  Routes.(s "v1" / s "issuing" / s "disputes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"dispute" ~op:"/v1/issuing/disputes/{dispute}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"dispute" ~loc:`Path ~style:`Simple ~explode:false)
        ":dispute" /? nil)

(** <p>Retrieves an Issuing <code>Dispute</code> object.</p>
    
    @param dispute dispute
    @see "openapi/spec3.json" /v1/issuing/disputes/\{dispute\} *)
let getIssuingDisputesDispute () =
  Routes.(s "v1" / s "issuing" / s "disputes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"dispute" ~op:"/v1/issuing/disputes/{dispute}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"dispute" ~loc:`Path ~style:`Simple ~explode:false)
        ":dispute" /? nil)

(** <p>Deletes a <code>Location</code> object.</p>
    
    @param location location
    @see "openapi/spec3.json" /v1/terminal/locations/\{location\} *)
let deleteTerminalLocationsLocation () =
  Routes.(s "v1" / s "terminal" / s "locations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"location" ~op:"/v1/terminal/locations/{location}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"location" ~loc:`Path ~style:`Simple ~explode:false)
        ":location" /? nil)

(** <p>Updates a <code>Location</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param location location
    @see "openapi/spec3.json" /v1/terminal/locations/\{location\} *)
let postTerminalLocationsLocation () =
  Routes.(s "v1" / s "terminal" / s "locations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"location" ~op:"/v1/terminal/locations/{location}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"location" ~loc:`Path ~style:`Simple ~explode:false)
        ":location" /? nil)

(** <p>Retrieves a <code>Location</code> object.</p>
    
    @param location location
    @see "openapi/spec3.json" /v1/terminal/locations/\{location\} *)
let getTerminalLocationsLocation () =
  Routes.(s "v1" / s "terminal" / s "locations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"location" ~op:"/v1/terminal/locations/{location}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"location" ~loc:`Path ~style:`Simple ~explode:false)
        ":location" /? nil)

(** <p>Transitions a test mode created InboundTransfer to the <code>succeeded</code> status. The InboundTransfer must already be in the <code>processing</code> state.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/test_helpers/treasury/inbound_transfers/\{id\}/succeed *)
let postTestHelpersTreasuryInboundTransfersIdSucceed () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "inbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/test_helpers/treasury/inbound_transfers/{id}/succeed" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "succeed" /? nil)

(** @param charge charge
    @see "openapi/spec3.json" /v1/charges/\{charge\}/dispute/close *)
let postChargesChargeDisputeClose () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/dispute/close" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "dispute" / s "close" /? nil)

(** <p>Update payment method configuration</p>
    
    @param configuration configuration
    @see "openapi/spec3.json" /v1/payment_method_configurations/\{configuration\} *)
let postPaymentMethodConfigurationsConfiguration () =
  Routes.(s "v1" / s "payment_method_configurations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"configuration" ~op:"/v1/payment_method_configurations/{configuration}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"configuration" ~loc:`Path ~style:`Simple ~explode:false)
        ":configuration" /? nil)

(** <p>Retrieve payment method configuration</p>
    
    @param configuration configuration
    @see "openapi/spec3.json" /v1/payment_method_configurations/\{configuration\} *)
let getPaymentMethodConfigurationsConfiguration () =
  Routes.(s "v1" / s "payment_method_configurations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"configuration" ~op:"/v1/payment_method_configurations/{configuration}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"configuration" ~loc:`Path ~style:`Simple ~explode:false)
        ":configuration" /? nil)

(** <p>Updates an existing Tax <code>Registration</code> object.</p>
    
    <p>A registration cannot be deleted after it has been created. If you wish to end a registration you may do so by setting <code>expires_at</code>.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/tax/registrations/\{id\} *)
let postTaxRegistrationsId () =
  Routes.(s "v1" / s "tax" / s "registrations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/tax/registrations/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Returns a Tax <code>Registration</code> object.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/tax/registrations/\{id\} *)
let getTaxRegistrationsId () =
  Routes.(s "v1" / s "tax" / s "registrations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/tax/registrations/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves the exchange rates from the given currency to every supported currency.</p>
    
    @param rate_id rate_id
    @see "openapi/spec3.json" /v1/exchange_rates/\{rate_id\} *)
let getExchangeRatesRateId () =
  Routes.(s "v1" / s "exchange_rates"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"rate_id" ~op:"/v1/exchange_rates/{rate_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"rate_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":rate_id" /? nil)

(** <p>Lists all owners for a given <code>Account</code></p>
    
    @param account account
    @see "openapi/spec3.json" /v1/linked_accounts/\{account\}/owners *)
let getLinkedAccountsAccountOwners () =
  Routes.(s "v1" / s "linked_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/linked_accounts/{account}/owners" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "owners" /? nil)

(** <p>Refunds an application fee that has previously been collected but not yet refunded.
    Funds will be refunded to the Stripe account from which the fee was originally collected.</p>
    
    <p>You can optionally refund only part of an application fee.
    You can do so multiple times, until the entire fee has been refunded.</p>
    
    <p>Once entirely refunded, an application fee can’t be refunded again.
    This method will raise an error when called on an already-refunded application fee,
    or when trying to refund more money than is left on an application fee.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/application_fees/\{id\}/refunds *)
let postApplicationFeesIdRefunds () =
  Routes.(s "v1" / s "application_fees"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/application_fees/{id}/refunds" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "refunds" /? nil)

(** <p>You can see a list of the refunds belonging to a specific application fee. Note that the 10 most recent refunds are always available by default on the application fee object. If you need more than those 10, you can use this API method and the <code>limit</code> and <code>starting_after</code> parameters to page through additional refunds.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/application_fees/\{id\}/refunds *)
let getApplicationFeesIdRefunds () =
  Routes.(s "v1" / s "application_fees"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/application_fees/{id}/refunds" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "refunds" /? nil)

(** <p>Updates the shipping status of the specified Issuing <code>Card</code> object to <code>failure</code>.</p>
    
    @param card card
    @see "openapi/spec3.json" /v1/test_helpers/issuing/cards/\{card\}/shipping/fail *)
let postTestHelpersIssuingCardsCardShippingFail () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"card" ~op:"/v1/test_helpers/issuing/cards/{card}/shipping/fail" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"card" ~loc:`Path ~style:`Simple ~explode:false)
        ":card" / s "shipping" / s "fail" /? nil)

(** <p>Updates the specified price by setting the values of the parameters passed. Any parameters not provided are left unchanged.</p>
    
    @param price price
    @see "openapi/spec3.json" /v1/prices/\{price\} *)
let postPricesPrice () =
  Routes.(s "v1" / s "prices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"price" ~op:"/v1/prices/{price}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"price" ~loc:`Path ~style:`Simple ~explode:false)
        ":price" /? nil)

(** <p>Retrieves the price with the given ID.</p>
    
    @param price price
    @see "openapi/spec3.json" /v1/prices/\{price\} *)
let getPricesPrice () =
  Routes.(s "v1" / s "prices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"price" ~op:"/v1/prices/{price}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"price" ~loc:`Path ~style:`Simple ~explode:false)
        ":price" /? nil)

(** <p>When you create a new credit card, you must specify a customer or recipient on which to create it.</p>
    
    <p>If the card’s owner has no default card, then the new card will become the default.
    However, if the owner already has a default, then it will not change.
    To change the default, you should <a href="/docs/api#update_customer">update the customer</a> to have a new <code>default_source</code>.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/bank_accounts *)
let postCustomersCustomerBankAccounts () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/bank_accounts" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "bank_accounts" /? nil)

(** <p>You can see a list of the bank accounts belonging to a Customer. Note that the 10 most recent sources are always available by default on the Customer. If you need more than those 10, you can use this API method and the <code>limit</code> and <code>starting_after</code> parameters to page through additional bank accounts.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/bank_accounts *)
let getCustomersCustomerBankAccounts () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/bank_accounts" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "bank_accounts" /? nil)

(** <p>Stripe automatically finalizes drafts before sending and attempting payment on invoices. However, if you’d like to finalize a draft invoice manually, you can do so using this method.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\}/finalize *)
let postInvoicesInvoiceFinalize () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}/finalize" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" / s "finalize" /? nil)

(** <p>Updates the specified Issuing <code>Transaction</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param transaction transaction
    @see "openapi/spec3.json" /v1/issuing/transactions/\{transaction\} *)
let postIssuingTransactionsTransaction () =
  Routes.(s "v1" / s "issuing" / s "transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/issuing/transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" /? nil)

(** <p>Retrieves an Issuing <code>Transaction</code> object.</p>
    
    @param transaction transaction
    @see "openapi/spec3.json" /v1/issuing/transactions/\{transaction\} *)
let getIssuingTransactionsTransaction () =
  Routes.(s "v1" / s "issuing" / s "transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/issuing/transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" /? nil)

(** <p>Retrieves the details of an existing Report Run.</p>
    
    @param report_run report_run
    @see "openapi/spec3.json" /v1/reporting/report_runs/\{report_run\} *)
let getReportingReportRunsReportRun () =
  Routes.(s "v1" / s "reporting" / s "report_runs"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"report_run" ~op:"/v1/reporting/report_runs/{report_run}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"report_run" ~loc:`Path ~style:`Simple ~explode:false)
        ":report_run" /? nil)

(** <p>Update a feature’s metadata or permanently deactivate it.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/entitlements/features/\{id\} *)
let postEntitlementsFeaturesId () =
  Routes.(s "v1" / s "entitlements" / s "features"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/entitlements/features/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves a feature</p>
    
    @param id The ID of the feature.
    @see "openapi/spec3.json" /v1/entitlements/features/\{id\} *)
let getEntitlementsFeaturesId () =
  Routes.(s "v1" / s "entitlements" / s "features"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/entitlements/features/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>You can cancel a previously created payout if its status is <code>pending</code>. Stripe refunds the funds to your available balance. You can’t cancel automatic Stripe payouts.</p>
    
    @param payout payout
    @see "openapi/spec3.json" /v1/payouts/\{payout\}/cancel *)
let postPayoutsPayoutCancel () =
  Routes.(s "v1" / s "payouts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payout" ~op:"/v1/payouts/{payout}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payout" ~loc:`Path ~style:`Simple ~explode:false)
        ":payout" / s "cancel" /? nil)

(** <p>Cancels a customer’s subscription immediately. The customer will not be charged again for the subscription.</p>
    
    <p>Note, however, that any pending invoice items that you’ve created will still be charged for at the end of the period, unless manually <a href="#delete_invoiceitem">deleted</a>. If you’ve set the subscription to cancel at the end of the period, any pending prorations will also be left in place and collected at the end of the period. But if the subscription is set to cancel immediately, pending prorations will be removed.</p>
    
    <p>By default, upon subscription cancellation, Stripe will stop automatic collection of all finalized invoices for the customer. This is intended to prevent unexpected payment attempts after the customer has canceled a subscription. However, you can resume automatic collection of the invoices manually after subscription cancellation to have us proceed. Or, you could check for unpaid invoices before allowing the customer to cancel the subscription at all.</p>
    
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/subscriptions/\{subscription_exposed_id\} *)
let deleteSubscriptionsSubscriptionExposedId () =
  Routes.(s "v1" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" /? nil)

(** <p>Updates an existing subscription to match the specified parameters.
    When changing prices or quantities, we optionally prorate the price we charge next month to make up for any price changes.
    To preview how the proration is calculated, use the <a href="/docs/api/invoices/upcoming">upcoming invoice</a> endpoint.</p>
    
    <p>By default, we prorate subscription changes. For example, if a customer signs up on May 1 for a <currency>100</currency> price, they’ll be billed <currency>100</currency> immediately. If on May 15 they switch to a <currency>200</currency> price, then on June 1 they’ll be billed <currency>250</currency> (<currency>200</currency> for a renewal of her subscription, plus a <currency>50</currency> prorating adjustment for half of the previous month’s <currency>100</currency> difference). Similarly, a downgrade generates a credit that is applied to the next invoice. We also prorate when you make quantity changes.</p>
    
    <p>Switching prices does not normally change the billing date or generate an immediate charge unless:</p>
    
    <ul>
    <li>The billing interval is changed (for example, from monthly to yearly).</li>
    <li>The subscription moves from free to paid, or paid to free.</li>
    <li>A trial starts or ends.</li>
    </ul>
    
    <p>In these cases, we apply a credit for the unused time on the previous price, immediately charge the customer using the new price, and reset the billing date.</p>
    
    <p>If you want to charge for an upgrade immediately, pass <code>proration_behavior</code> as <code>always_invoice</code> to create prorations, automatically invoice the customer for those proration adjustments, and attempt to collect payment. If you pass <code>create_prorations</code>, the prorations are created but not automatically invoiced. If you want to bill the customer for the prorations before the subscription’s renewal date, you need to manually <a href="/docs/api/invoices/create">invoice the customer</a>.</p>
    
    <p>If you don’t want to prorate, set the <code>proration_behavior</code> option to <code>none</code>. With this option, the customer is billed <currency>100</currency> on May 1 and <currency>200</currency> on June 1. Similarly, if you set <code>proration_behavior</code> to <code>none</code> when switching between different billing intervals (for example, from monthly to yearly), we don’t generate any credits for the old subscription’s unused time. We still reset the billing date and bill immediately for the new subscription.</p>
    
    <p>Updating the quantity on a subscription many times in an hour may result in <a href="/docs/rate-limits">rate limiting</a>. If you need to bill for a frequently changing quantity, consider integrating <a href="/docs/billing/subscriptions/usage-based">usage-based billing</a> instead.</p>
    
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/subscriptions/\{subscription_exposed_id\} *)
let postSubscriptionsSubscriptionExposedId () =
  Routes.(s "v1" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" /? nil)

(** <p>Retrieves the subscription with the given ID.</p>
    
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/subscriptions/\{subscription_exposed_id\} *)
let getSubscriptionsSubscriptionExposedId () =
  Routes.(s "v1" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" /? nil)

(** <p>Create an external account for a given account.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/external_accounts *)
let postAccountsAccountExternalAccounts () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/external_accounts" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "external_accounts" /? nil)

(** <p>List external accounts for an account.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/external_accounts *)
let getAccountsAccountExternalAccounts () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/external_accounts" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "external_accounts" /? nil)

(** <p>Updates the Features associated with a FinancialAccount.</p>
    
    @param financial_account financial_account
    @see "openapi/spec3.json" /v1/treasury/financial_accounts/\{financial_account\}/features *)
let postTreasuryFinancialAccountsFinancialAccountFeatures () =
  Routes.(s "v1" / s "treasury" / s "financial_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"financial_account" ~op:"/v1/treasury/financial_accounts/{financial_account}/features" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"financial_account" ~loc:`Path ~style:`Simple ~explode:false)
        ":financial_account" / s "features" /? nil)

(** <p>Retrieves Features information associated with the FinancialAccount.</p>
    
    @param financial_account financial_account
    @see "openapi/spec3.json" /v1/treasury/financial_accounts/\{financial_account\}/features *)
let getTreasuryFinancialAccountsFinancialAccountFeatures () =
  Routes.(s "v1" / s "treasury" / s "financial_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"financial_account" ~op:"/v1/treasury/financial_accounts/{financial_account}/features" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"financial_account" ~loc:`Path ~style:`Simple ~explode:false)
        ":financial_account" / s "features" /? nil)

(** <p>Subscribes to periodic refreshes of data associated with a Financial Connections <code>Account</code>.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/financial_connections/accounts/\{account\}/subscribe *)
let postFinancialConnectionsAccountsAccountSubscribe () =
  Routes.(s "v1" / s "financial_connections" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/financial_connections/accounts/{account}/subscribe" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "subscribe" /? nil)

(** <p>Perform an incremental authorization on an eligible
    <a href="/docs/api/payment_intents/object">PaymentIntent</a>. To be eligible, the
    PaymentIntent’s status must be <code>requires_capture</code> and
    <a href="/docs/api/charges/object#charge_object-payment_method_details-card_present-incremental_authorization_supported">incremental_authorization_supported</a>
    must be <code>true</code>.</p>
    
    <p>Incremental authorizations attempt to increase the authorized amount on
    your customer’s card to the new, higher <code>amount</code> provided. Similar to the
    initial authorization, incremental authorizations can be declined. A
    single PaymentIntent can call this endpoint multiple times to further
    increase the authorized amount.</p>
    
    <p>If the incremental authorization succeeds, the PaymentIntent object
    returns with the updated
    <a href="/docs/api/payment_intents/object#payment_intent_object-amount">amount</a>.
    If the incremental authorization fails, a
    <a href="/docs/error-codes#card-declined">card_declined</a> error returns, and no other
    fields on the PaymentIntent or Charge update. The PaymentIntent
    object remains capturable for the previously authorized amount.</p>
    
    <p>Each PaymentIntent can have a maximum of 10 incremental authorization attempts, including declines.
    After it’s captured, a PaymentIntent can no longer be incremented.</p>
    
    <p>Learn more about <a href="/docs/terminal/features/incremental-authorizations">incremental authorizations</a>.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\}/increment_authorization *)
let postPaymentIntentsIntentIncrementAuthorization () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}/increment_authorization" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "increment_authorization" /? nil)

(** <p>Cancels a Climate order. You can cancel an order within 30 days of creation. Stripe refunds the
    reservation <code>amount_subtotal</code>, but not the <code>amount_fees</code> for user-triggered cancellations. Frontier
    might cancel reservations if suppliers fail to deliver. If Frontier cancels the reservation, Stripe
    provides 90 days advance notice and refunds the <code>amount_total</code>.</p>
    
    @param order Unique identifier of the order.
    @see "openapi/spec3.json" /v1/climate/orders/\{order\}/cancel *)
let postClimateOrdersOrderCancel () =
  Routes.(s "v1" / s "climate" / s "orders"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"order" ~op:"/v1/climate/orders/{order}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"order" ~loc:`Path ~style:`Simple ~explode:false)
        ":order" / s "cancel" /? nil)

(** <p>Presents a payment method on a simulated reader. Can be used to simulate accepting a payment, saving a card or refunding a transaction.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/test_helpers/terminal/readers/\{reader\}/present_payment_method *)
let postTestHelpersTerminalReadersReaderPresentPaymentMethod () =
  Routes.(s "v1" / s "test_helpers" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/test_helpers/terminal/readers/{reader}/present_payment_method" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" / s "present_payment_method" /? nil)

(** <p>Accepts the specified quote.</p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\}/accept *)
let postQuotesQuoteAccept () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}/accept" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" / s "accept" /? nil)

(** <p>Returns a list of PaymentMethods for a given Customer</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/payment_methods *)
let getCustomersCustomerPaymentMethods () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/payment_methods" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "payment_methods" /? nil)

(** <p>\[Deprecated\] Declines a pending Issuing <code>Authorization</code> object. This request should be made within the timeout window of the <a href="/docs/issuing/controls/real-time-authorizations">real time authorization</a> flow.
    This method is deprecated. Instead, <a href="/docs/issuing/controls/real-time-authorizations#authorization-handling">respond directly to the webhook request to decline an authorization</a>.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/issuing/authorizations/\{authorization\}/decline *)
let postIssuingAuthorizationsAuthorizationDecline () =
  Routes.(s "v1" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/issuing/authorizations/{authorization}/decline" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" / s "decline" /? nil)

(** <p>Retrieves the details of an existing OutboundTransfer by passing the unique OutboundTransfer ID from either the OutboundTransfer creation request or OutboundTransfer list.</p>
    
    @param outbound_transfer outbound_transfer
    @see "openapi/spec3.json" /v1/treasury/outbound_transfers/\{outbound_transfer\} *)
let getTreasuryOutboundTransfersOutboundTransfer () =
  Routes.(s "v1" / s "treasury" / s "outbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"outbound_transfer" ~op:"/v1/treasury/outbound_transfers/{outbound_transfer}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"outbound_transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":outbound_transfer" /? nil)

(** <p>Deletes a <code>Reader</code> object.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\} *)
let deleteTerminalReadersReader () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" /? nil)

(** <p>Updates a <code>Reader</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\} *)
let postTerminalReadersReader () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" /? nil)

(** <p>Retrieves a <code>Reader</code> object.</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\} *)
let getTerminalReadersReader () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" /? nil)

(** <p>Creates a new <code>tax_id</code> object for a customer.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/tax_ids *)
let postCustomersCustomerTaxIds () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/tax_ids" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "tax_ids" /? nil)

(** <p>Returns a list of tax IDs for a customer.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/tax_ids *)
let getCustomersCustomerTaxIds () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/tax_ids" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "tax_ids" /? nil)

(** <p>Transitions a test mode created OutboundPayment to the <code>failed</code> status. The OutboundPayment must already be in the <code>processing</code> state.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_payments/\{id\}/fail *)
let postTestHelpersTreasuryOutboundPaymentsIdFail () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_payments"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/test_helpers/treasury/outbound_payments/{id}/fail" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "fail" /? nil)

(** <p>Updates an existing payment method domain.</p>
    
    @param payment_method_domain payment_method_domain
    @see "openapi/spec3.json" /v1/payment_method_domains/\{payment_method_domain\} *)
let postPaymentMethodDomainsPaymentMethodDomain () =
  Routes.(s "v1" / s "payment_method_domains"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method_domain" ~op:"/v1/payment_method_domains/{payment_method_domain}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method_domain" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method_domain" /? nil)

(** <p>Retrieves the details of an existing payment method domain.</p>
    
    @param payment_method_domain payment_method_domain
    @see "openapi/spec3.json" /v1/payment_method_domains/\{payment_method_domain\} *)
let getPaymentMethodDomainsPaymentMethodDomain () =
  Routes.(s "v1" / s "payment_method_domains"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method_domain" ~op:"/v1/payment_method_domains/{payment_method_domain}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method_domain" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method_domain" /? nil)

(** <p>For the specified subscription item, returns a list of summary objects. Each object in the list provides usage information that’s been summarized from multiple usage records and over a subscription billing period (e.g., 15 usage records in the month of September).</p>
    
    <p>The list is sorted in reverse-chronological order (newest first). The first list item represents the most current usage period that hasn’t ended yet. Since new usage records can still be added, the returned summary information for the subscription item’s ID should be seen as unstable until the subscription billing period ends.</p>
    
    @param subscription_item subscription_item
    @see "openapi/spec3.json" /v1/subscription_items/\{subscription_item\}/usage_record_summaries *)
let getSubscriptionItemsSubscriptionItemUsageRecordSummaries () =
  Routes.(s "v1" / s "subscription_items"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_item" ~op:"/v1/subscription_items/{subscription_item}/usage_record_summaries" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_item" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_item" / s "usage_record_summaries" /? nil)

(** <p>Create an external account for a given account.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/accounts/\{account\}/bank_accounts *)
let postAccountsAccountBankAccounts () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/bank_accounts" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "bank_accounts" /? nil)

(** <p>Updates the specified transfer by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    <p>This request accepts only metadata as an argument.</p>
    
    @param transfer transfer
    @see "openapi/spec3.json" /v1/transfers/\{transfer\} *)
let postTransfersTransfer () =
  Routes.(s "v1" / s "transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transfer" ~op:"/v1/transfers/{transfer}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":transfer" /? nil)

(** <p>Retrieves the details of an existing transfer. Supply the unique transfer ID from either a transfer creation request or the transfer list, and Stripe will return the corresponding transfer information.</p>
    
    @param transfer transfer
    @see "openapi/spec3.json" /v1/transfers/\{transfer\} *)
let getTransfersTransfer () =
  Routes.(s "v1" / s "transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transfer" ~op:"/v1/transfers/{transfer}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":transfer" /? nil)

(** <p>Retrieves a Mandate object.</p>
    
    @param mandate mandate
    @see "openapi/spec3.json" /v1/mandates/\{mandate\} *)
let getMandatesMandate () =
  Routes.(s "v1" / s "mandates"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"mandate" ~op:"/v1/mandates/{mandate}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"mandate" ~loc:`Path ~style:`Simple ~explode:false)
        ":mandate" /? nil)

(** <p>Updates the shipping status of the specified Issuing <code>Card</code> object to <code>shipped</code>.</p>
    
    @param card card
    @see "openapi/spec3.json" /v1/test_helpers/issuing/cards/\{card\}/shipping/ship *)
let postTestHelpersIssuingCardsCardShippingShip () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"card" ~op:"/v1/test_helpers/issuing/cards/{card}/shipping/ship" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"card" ~loc:`Path ~style:`Simple ~explode:false)
        ":card" / s "shipping" / s "ship" /? nil)

(** <p>Updates a SetupIntent object.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/setup_intents/\{intent\} *)
let postSetupIntentsIntent () =
  Routes.(s "v1" / s "setup_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/setup_intents/{intent}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" /? nil)

(** <p>Retrieves the details of a SetupIntent that has previously been created. </p>
    
    <p>Client-side retrieval using a publishable key is allowed when the <code>client_secret</code> is provided in the query string. </p>
    
    <p>When retrieved with a publishable key, only a subset of properties will be returned. Please refer to the <a href="#setup_intent_object">SetupIntent</a> object reference for more details.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/setup_intents/\{intent\} *)
let getSetupIntentsIntent () =
  Routes.(s "v1" / s "setup_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/setup_intents/{intent}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" /? nil)

(** <p>When you create a new refund, you must specify a Charge or a PaymentIntent object on which to create it.</p>
    
    <p>Creating a new refund will refund a charge that has previously been created but not yet refunded.
    Funds will be refunded to the credit or debit card that was originally charged.</p>
    
    <p>You can optionally refund only part of a charge.
    You can do so multiple times, until the entire charge has been refunded.</p>
    
    <p>Once entirely refunded, a charge can’t be refunded again.
    This method will raise an error when called on an already-refunded charge,
    or when trying to refund more money than is left on a charge.</p>
    
    @param charge The identifier of the charge to refund.
    @see "openapi/spec3.json" /v1/charges/\{charge\}/refunds *)
let postChargesChargeRefunds () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/refunds" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "refunds" /? nil)

(** <p>You can see a list of the refunds belonging to a specific charge. Note that the 10 most recent refunds are always available by default on the charge object. If you need more than those 10, you can use this API method and the <code>limit</code> and <code>starting_after</code> parameters to page through additional refunds.</p>
    
    @param charge charge
    @see "openapi/spec3.json" /v1/charges/\{charge\}/refunds *)
let getChargesChargeRefunds () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/refunds" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "refunds" /? nil)

(** <p>Updates an existing file link object. Expired links can no longer be updated.</p>
    
    @param link link
    @see "openapi/spec3.json" /v1/file_links/\{link\} *)
let postFileLinksLink () =
  Routes.(s "v1" / s "file_links"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"link" ~op:"/v1/file_links/{link}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"link" ~loc:`Path ~style:`Simple ~explode:false)
        ":link" /? nil)

(** <p>Retrieves the file link with the given ID.</p>
    
    @param link link
    @see "openapi/spec3.json" /v1/file_links/\{link\} *)
let getFileLinksLink () =
  Routes.(s "v1" / s "file_links"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"link" ~op:"/v1/file_links/{link}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"link" ~loc:`Path ~style:`Simple ~explode:false)
        ":link" /? nil)

(** <p>Initiates a refund on a Reader</p>
    
    @param reader reader
    @see "openapi/spec3.json" /v1/terminal/readers/\{reader\}/refund_payment *)
let postTerminalReadersReaderRefundPayment () =
  Routes.(s "v1" / s "terminal" / s "readers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"reader" ~op:"/v1/terminal/readers/{reader}/refund_payment" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"reader" ~loc:`Path ~style:`Simple ~explode:false)
        ":reader" / s "refund_payment" /? nil)

(** <p>Retrieves the details of an Financial Connections <code>Account</code>.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/financial_connections/accounts/\{account\} *)
let getFinancialConnectionsAccountsAccount () =
  Routes.(s "v1" / s "financial_connections" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/financial_connections/accounts/{account}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" /? nil)

(** <p>Manually reconcile the remaining amount for a <code>customer_balance</code> PaymentIntent.</p>
    
    @param intent intent
    @see "openapi/spec3.json" /v1/payment_intents/\{intent\}/apply_customer_balance *)
let postPaymentIntentsIntentApplyCustomerBalance () =
  Routes.(s "v1" / s "payment_intents"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"intent" ~op:"/v1/payment_intents/{intent}/apply_customer_balance" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"intent" ~loc:`Path ~style:`Simple ~explode:false)
        ":intent" / s "apply_customer_balance" /? nil)

(** <p>Capture the payment of an existing, uncaptured charge that was created with the <code>capture</code> option set to false.</p>
    
    <p>Uncaptured payments expire a set number of days after they are created (<a href="/docs/charges/placing-a-hold">7 by default</a>), after which they are marked as refunded and capture attempts will fail.</p>
    
    <p>Don’t use this method to capture a PaymentIntent-initiated charge. Use <a href="/docs/api/payment_intents/capture">Capture a PaymentIntent</a>.</p>
    
    @param charge charge
    @see "openapi/spec3.json" /v1/charges/\{charge\}/capture *)
let postChargesChargeCapture () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/capture" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "capture" /? nil)

(** <p>Transitions a test mode created OutboundTransfer to the <code>failed</code> status. The OutboundTransfer must already be in the <code>processing</code> state.</p>
    
    @param outbound_transfer outbound_transfer
    @see "openapi/spec3.json" /v1/test_helpers/treasury/outbound_transfers/\{outbound_transfer\}/fail *)
let postTestHelpersTreasuryOutboundTransfersOutboundTransferFail () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "outbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"outbound_transfer" ~op:"/v1/test_helpers/treasury/outbound_transfers/{outbound_transfer}/fail" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"outbound_transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":outbound_transfer" / s "fail" /? nil)

(** <p>Cancels a subscription schedule and its associated subscription immediately (if the subscription schedule has an active subscription). A subscription schedule can only be canceled if its status is <code>not_started</code> or <code>active</code>.</p>
    
    @param schedule schedule
    @see "openapi/spec3.json" /v1/subscription_schedules/\{schedule\}/cancel *)
let postSubscriptionSchedulesScheduleCancel () =
  Routes.(s "v1" / s "subscription_schedules"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"schedule" ~op:"/v1/subscription_schedules/{schedule}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"schedule" ~loc:`Path ~style:`Simple ~explode:false)
        ":schedule" / s "cancel" /? nil)

(** <p>Returns a list of transactions that modified the customer’s <a href="/docs/payments/customer-balance">cash balance</a>.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cash_balance_transactions *)
let getCustomersCustomerCashBalanceTransactions () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cash_balance_transactions" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cash_balance_transactions" /? nil)

(** <p>Mark a finalized invoice as void. This cannot be undone. Voiding an invoice is similar to <a href="#delete_invoice">deletion</a>, however it only applies to finalized invoices and maintains a papertrail where the invoice can still be found.</p>
    
    <p>Consult with local regulations to determine whether and how an invoice might be amended, canceled, or voided in the jurisdiction you’re doing business in. You might need to <a href="#create_invoice">issue another invoice</a> or <a href="#create_credit_note">credit note</a> instead. Stripe recommends that you consult with your legal counsel for advice specific to your business.</p>
    
    @param invoice invoice
    @see "openapi/spec3.json" /v1/invoices/\{invoice\}/void *)
let postInvoicesInvoiceVoid () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}/void" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" / s "void" /? nil)

(** <p>Closing the dispute for a charge indicates that you do not have any evidence to submit and are essentially dismissing the dispute, acknowledging it as lost.</p>
    
    <p>The status of the dispute will change from <code>needs_response</code> to <code>lost</code>. <em>Closing a dispute is irreversible</em>.</p>
    
    @param dispute dispute
    @see "openapi/spec3.json" /v1/disputes/\{dispute\}/close *)
let postDisputesDisputeClose () =
  Routes.(s "v1" / s "disputes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"dispute" ~op:"/v1/disputes/{dispute}/close" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"dispute" ~loc:`Path ~style:`Simple ~explode:false)
        ":dispute" / s "close" /? nil)

(** <p>Updates the specified Issuing <code>Settlement</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param settlement settlement
    @see "openapi/spec3.json" /v1/issuing/settlements/\{settlement\} *)
let postIssuingSettlementsSettlement () =
  Routes.(s "v1" / s "issuing" / s "settlements"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"settlement" ~op:"/v1/issuing/settlements/{settlement}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"settlement" ~loc:`Path ~style:`Simple ~explode:false)
        ":settlement" /? nil)

(** <p>Retrieves an Issuing <code>Settlement</code> object.</p>
    
    @param settlement settlement
    @see "openapi/spec3.json" /v1/issuing/settlements/\{settlement\} *)
let getIssuingSettlementsSettlement () =
  Routes.(s "v1" / s "issuing" / s "settlements"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"settlement" ~op:"/v1/issuing/settlements/{settlement}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"settlement" ~loc:`Path ~style:`Simple ~explode:false)
        ":settlement" /? nil)

(** <p>Retrieves the balance transaction with the given ID.</p>
    
    <p>Note that this endpoint previously used the path <code>/v1/balance/history/:id</code>.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/balance/history/\{id\} *)
let getBalanceHistoryId () =
  Routes.(s "v1" / s "balance" / s "history"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/balance/history/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Attaches a PaymentMethod object to a Customer.</p>
    
    <p>To attach a new PaymentMethod to a customer for future payments, we recommend you use a <a href="/docs/api/setup_intents">SetupIntent</a>
    or a PaymentIntent with <a href="/docs/api/payment_intents/create#create_payment_intent-setup_future_usage">setup_future_usage</a>.
    These approaches will perform any necessary steps to set up the PaymentMethod for future payments. Using the <code>/v1/payment_methods/:id/attach</code>
    endpoint without first using a SetupIntent or PaymentIntent with <code>setup_future_usage</code> does not optimize the PaymentMethod for
    future use, which makes later declines and payment friction more likely.
    See <a href="/docs/payments/payment-intents#future-usage">Optimizing cards for future payments</a> for more information about setting up
    future payments.</p>
    
    <p>To use this PaymentMethod as the default for invoice or subscription payments,
    set <a href="/docs/api/customers/update#update_customer-invoice_settings-default_payment_method"><code>invoice_settings.default_payment_method</code></a>,
    on the Customer to the PaymentMethod’s ID.</p>
    
    @param payment_method payment_method
    @see "openapi/spec3.json" /v1/payment_methods/\{payment_method\}/attach *)
let postPaymentMethodsPaymentMethodAttach () =
  Routes.(s "v1" / s "payment_methods"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method" ~op:"/v1/payment_methods/{payment_method}/attach" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method" / s "attach" /? nil)

(** <p>Retrieves a Climate supplier object.</p>
    
    @param supplier supplier
    @see "openapi/spec3.json" /v1/climate/suppliers/\{supplier\} *)
let getClimateSuppliersSupplier () =
  Routes.(s "v1" / s "climate" / s "suppliers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"supplier" ~op:"/v1/climate/suppliers/{supplier}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"supplier" ~loc:`Path ~style:`Simple ~explode:false)
        ":supplier" /? nil)

(** <p>A VerificationSession object can be canceled when it is in <code>requires_input</code> <a href="/docs/identity/how-sessions-work">status</a>.</p>
    
    <p>Once canceled, future submission attempts are disabled. This cannot be undone. <a href="/docs/identity/verification-sessions#cancel">Learn more</a>.</p>
    
    @param session session
    @see "openapi/spec3.json" /v1/identity/verification_sessions/\{session\}/cancel *)
let postIdentityVerificationSessionsSessionCancel () =
  Routes.(s "v1" / s "identity" / s "verification_sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/identity/verification_sessions/{session}/cancel" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" / s "cancel" /? nil)

(** <p>When retrieving a quote, there is an includable <strong>line_items</strong> property containing the first handful of those items. There is also a URL where you can retrieve the full (paginated) list of line items.</p>
    
    @param quote quote
    @see "openapi/spec3.json" /v1/quotes/\{quote\}/line_items *)
let getQuotesQuoteLineItems () =
  Routes.(s "v1" / s "quotes"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"quote" ~op:"/v1/quotes/{quote}/line_items" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"quote" ~loc:`Path ~style:`Simple ~explode:false)
        ":quote" / s "line_items" /? nil)

(** <p>Retrieves the details of an event. Supply the unique identifier of the event, which you might have received in a webhook.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/events/\{id\} *)
let getEventsId () =
  Routes.(s "v1" / s "events"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/events/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>When retrieving a payment link, there is an includable <strong>line_items</strong> property containing the first handful of those items. There is also a URL where you can retrieve the full (paginated) list of line items.</p>
    
    @param payment_link payment_link
    @see "openapi/spec3.json" /v1/payment_links/\{payment_link\}/line_items *)
let getPaymentLinksPaymentLinkLineItems () =
  Routes.(s "v1" / s "payment_links"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_link" ~op:"/v1/payment_links/{payment_link}/line_items" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_link" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_link" / s "line_items" /? nil)

(** <p>Transitions a test mode created InboundTransfer to the <code>failed</code> status. The InboundTransfer must already be in the <code>processing</code> state.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/test_helpers/treasury/inbound_transfers/\{id\}/fail *)
let postTestHelpersTreasuryInboundTransfersIdFail () =
  Routes.(s "v1" / s "test_helpers" / s "treasury" / s "inbound_transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/test_helpers/treasury/inbound_transfers/{id}/fail" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "fail" /? nil)

(** <p>Updates the specified Issuing <code>Card</code> object by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    @param card card
    @see "openapi/spec3.json" /v1/issuing/cards/\{card\} *)
let postIssuingCardsCard () =
  Routes.(s "v1" / s "issuing" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"card" ~op:"/v1/issuing/cards/{card}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"card" ~loc:`Path ~style:`Simple ~explode:false)
        ":card" /? nil)

(** <p>Retrieves an Issuing <code>Card</code> object.</p>
    
    @param card card
    @see "openapi/spec3.json" /v1/issuing/cards/\{card\} *)
let getIssuingCardsCard () =
  Routes.(s "v1" / s "issuing" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"card" ~op:"/v1/issuing/cards/{card}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"card" ~loc:`Path ~style:`Simple ~explode:false)
        ":card" /? nil)

(** <p>Retrieve a list of billing meter event summaries.</p>
    
    @param id Unique identifier for the object.
    @see "openapi/spec3.json" /v1/billing/meters/\{id\}/event_summaries *)
let getBillingMetersIdEventSummaries () =
  Routes.(s "v1" / s "billing" / s "meters"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/billing/meters/{id}/event_summaries" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "event_summaries" /? nil)

(** <p>Updates the metadata of a top-up. Other top-up details are not editable by design.</p>
    
    @param topup topup
    @see "openapi/spec3.json" /v1/topups/\{topup\} *)
let postTopupsTopup () =
  Routes.(s "v1" / s "topups"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"topup" ~op:"/v1/topups/{topup}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"topup" ~loc:`Path ~style:`Simple ~explode:false)
        ":topup" /? nil)

(** <p>Retrieves the details of a top-up that has previously been created. Supply the unique top-up ID that was returned from your previous request, and Stripe will return the corresponding top-up information.</p>
    
    @param topup topup
    @see "openapi/spec3.json" /v1/topups/\{topup\} *)
let getTopupsTopup () =
  Routes.(s "v1" / s "topups"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"topup" ~op:"/v1/topups/{topup}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"topup" ~loc:`Path ~style:`Simple ~explode:false)
        ":topup" /? nil)

(** <p>Creates an immutable transaction that updates the customer’s credit <a href="/docs/billing/customer/balance">balance</a>.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/balance_transactions *)
let postCustomersCustomerBalanceTransactions () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/balance_transactions" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "balance_transactions" /? nil)

(** <p>Returns a list of transactions that updated the customer’s <a href="/docs/billing/customer/balance">balances</a>.</p>
    
    @param customer customer
    @see "openapi/spec3.json" /v1/customers/\{customer\}/balance_transactions *)
let getCustomersCustomerBalanceTransactions () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/balance_transactions" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "balance_transactions" /? nil)

(** <p>Retrieves the line items of a persisted tax calculation as a collection.</p>
    
    @param calculation calculation
    @see "openapi/spec3.json" /v1/tax/calculations/\{calculation\}/line_items *)
let getTaxCalculationsCalculationLineItems () =
  Routes.(s "v1" / s "tax" / s "calculations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"calculation" ~op:"/v1/tax/calculations/{calculation}/line_items" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"calculation" ~loc:`Path ~style:`Simple ~explode:false)
        ":calculation" / s "line_items" /? nil)

(** <p>Retrieves the details of an application fee that your account has collected. The same information is returned when refunding the application fee.</p>
    
    @param id id
    @see "openapi/spec3.json" /v1/application_fees/\{id\} *)
let getApplicationFeesId () =
  Routes.(s "v1" / s "application_fees"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/application_fees/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Reverse a test-mode Authorization.</p>
    
    @param authorization authorization
    @see "openapi/spec3.json" /v1/test_helpers/issuing/authorizations/\{authorization\}/reverse *)
let postTestHelpersIssuingAuthorizationsAuthorizationReverse () =
  Routes.(s "v1" / s "test_helpers" / s "issuing" / s "authorizations"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"authorization" ~op:"/v1/test_helpers/issuing/authorizations/{authorization}/reverse" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"authorization" ~loc:`Path ~style:`Simple ~explode:false)
        ":authorization" / s "reverse" /? nil)

(** <p>Retrieves a <code>Review</code> object.</p>
    
    @param review review
    @see "openapi/spec3.json" /v1/reviews/\{review\} *)
let getReviewsReview () =
  Routes.(s "v1" / s "reviews"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"review" ~op:"/v1/reviews/{review}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"review" ~loc:`Path ~style:`Simple ~explode:false)
        ":review" /? nil)

(** <p>A Session can be expired when it is in one of these statuses: <code>open</code> </p>
    
    <p>After it expires, a customer can’t complete a Session and customers loading the Session see a message saying the Session is expired.</p>
    
    @param session session
    @see "openapi/spec3.json" /v1/checkout/sessions/\{session\}/expire *)
let postCheckoutSessionsSessionExpire () =
  Routes.(s "v1" / s "checkout" / s "sessions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"session" ~op:"/v1/checkout/sessions/{session}/expire" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"session" ~loc:`Path ~style:`Simple ~explode:false)
        ":session" / s "expire" /? nil)

(** <p>Retrieves the details of an Financial Connections <code>Account</code>.</p>
    
    @param account account
    @see "openapi/spec3.json" /v1/linked_accounts/\{account\} *)
let getLinkedAccountsAccount () =
  Routes.(s "v1" / s "linked_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/linked_accounts/{account}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" /? nil)

(** <p>Delete a specified source for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/sources/\{id\} *)
let deleteCustomersCustomerSourcesId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/sources/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/sources/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Update a specified source for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/sources/\{id\} *)
let postCustomersCustomerSourcesId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/sources/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/sources/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieve a specified source for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/sources/\{id\} *)
let getCustomersCustomerSourcesId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/sources/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/sources/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates the specified application fee refund by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    <p>This request only accepts metadata as an argument.</p>
    
    @param fee fee
    @param id id
    @see "openapi/spec3.json" /v1/application_fees/\{fee\}/refunds/\{id\} *)
let postApplicationFeesFeeRefundsId () =
  Routes.(s "v1" / s "application_fees"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"fee" ~op:"/v1/application_fees/{fee}/refunds/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"fee" ~loc:`Path ~style:`Simple ~explode:false)
        ":fee" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/application_fees/{fee}/refunds/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>By default, you can see the 10 most recent refunds stored directly on the application fee object, but you can also retrieve details about a specific refund stored on the application fee.</p>
    
    @param fee fee
    @param id id
    @see "openapi/spec3.json" /v1/application_fees/\{fee\}/refunds/\{id\} *)
let getApplicationFeesFeeRefundsId () =
  Routes.(s "v1" / s "application_fees"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"fee" ~op:"/v1/application_fees/{fee}/refunds/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"fee" ~loc:`Path ~style:`Simple ~explode:false)
        ":fee" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/application_fees/{fee}/refunds/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Deletes the feature attachment to a product</p>
    
    @param product product
    @param id id
    @see "openapi/spec3.json" /v1/products/\{product\}/features/\{id\} *)
let deleteProductsProductFeaturesId () =
  Routes.(s "v1" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"product" ~op:"/v1/products/{product}/features/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"product" ~loc:`Path ~style:`Simple ~explode:false)
        ":product" / s "features"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/products/{product}/features/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves a product_feature, which represents a feature attachment to a product</p>
    
    @param product The ID of the product.
    @param id The ID of the product_feature.
    @see "openapi/spec3.json" /v1/products/\{product\}/features/\{id\} *)
let getProductsProductFeaturesId () =
  Routes.(s "v1" / s "products"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"product" ~op:"/v1/products/{product}/features/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"product" ~loc:`Path ~style:`Simple ~explode:false)
        ":product" / s "features"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/products/{product}/features/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Removes the currently applied discount on a customer.</p>
    
    @param customer customer
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/subscriptions/\{subscription_exposed_id\}/discount *)
let deleteCustomersCustomerSubscriptionsSubscriptionExposedIdDiscount () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}/discount" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}/discount" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" / s "discount" /? nil)

(** @param customer customer
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/subscriptions/\{subscription_exposed_id\}/discount *)
let getCustomersCustomerSubscriptionsSubscriptionExposedIdDiscount () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}/discount" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}/discount" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" / s "discount" /? nil)

(** <p>Updates an existing Account Capability. Request or remove a capability by updating its <code>requested</code> parameter.</p>
    
    @param account account
    @param capability capability
    @see "openapi/spec3.json" /v1/accounts/\{account\}/capabilities/\{capability\} *)
let postAccountsAccountCapabilitiesCapability () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/capabilities/{capability}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "capabilities"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"capability" ~op:"/v1/accounts/{account}/capabilities/{capability}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"capability" ~loc:`Path ~style:`Simple ~explode:false)
        ":capability" /? nil)

(** <p>Retrieves information about the specified Account Capability.</p>
    
    @param account account
    @param capability capability
    @see "openapi/spec3.json" /v1/accounts/\{account\}/capabilities/\{capability\} *)
let getAccountsAccountCapabilitiesCapability () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/capabilities/{capability}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "capabilities"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"capability" ~op:"/v1/accounts/{account}/capabilities/{capability}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"capability" ~loc:`Path ~style:`Simple ~explode:false)
        ":capability" /? nil)

(** <p>Delete a specified source for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/bank_accounts/\{id\} *)
let deleteCustomersCustomerBankAccountsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "bank_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Update a specified source for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/bank_accounts/\{id\} *)
let postCustomersCustomerBankAccountsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "bank_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>By default, you can see the 10 most recent sources stored on a Customer directly on the object, but you can also retrieve details about a specific bank account stored on the Stripe account.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/bank_accounts/\{id\} *)
let getCustomersCustomerBankAccountsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "bank_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves a PaymentMethod object for a given Customer.</p>
    
    @param customer customer
    @param payment_method payment_method
    @see "openapi/spec3.json" /v1/customers/\{customer\}/payment_methods/\{payment_method\} *)
let getCustomersCustomerPaymentMethodsPaymentMethod () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/payment_methods/{payment_method}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "payment_methods"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"payment_method" ~op:"/v1/customers/{customer}/payment_methods/{payment_method}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"payment_method" ~loc:`Path ~style:`Simple ~explode:false)
        ":payment_method" /? nil)

(** <p>Deletes an existing <code>tax_id</code> object.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/tax_ids/\{id\} *)
let deleteCustomersCustomerTaxIdsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/tax_ids/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "tax_ids"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/tax_ids/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves the <code>tax_id</code> object with the given identifier.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/tax_ids/\{id\} *)
let getCustomersCustomerTaxIdsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/tax_ids/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "tax_ids"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/tax_ids/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Delete a specified external account for a given account.</p>
    
    @param account account
    @param id id
    @see "openapi/spec3.json" /v1/accounts/\{account\}/bank_accounts/\{id\} *)
let deleteAccountsAccountBankAccountsId () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "bank_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/accounts/{account}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates the metadata, account holder name, account holder type of a bank account belonging to
    a connected account and optionally sets it as the default for its currency. Other bank account
    details are not editable by design.</p>
    
    <p>You can only update bank accounts when <a href="/api/accounts/object#account_object-controller-requirement_collection">account.controller.requirement_collection</a> is <code>application</code>, which includes <a href="/connect/custom-accounts">Custom accounts</a>.</p>
    
    <p>You can re-enable a disabled bank account by performing an update call without providing any
    arguments or changes.</p>
    
    @param account account
    @param id id
    @see "openapi/spec3.json" /v1/accounts/\{account\}/bank_accounts/\{id\} *)
let postAccountsAccountBankAccountsId () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "bank_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/accounts/{account}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieve a specified external account for a given account.</p>
    
    @param account account
    @param id id
    @see "openapi/spec3.json" /v1/accounts/\{account\}/bank_accounts/\{id\} *)
let getAccountsAccountBankAccountsId () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "bank_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/accounts/{account}/bank_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates the specified reversal by setting the values of the parameters passed. Any parameters not provided will be left unchanged.</p>
    
    <p>This request only accepts metadata and description as arguments.</p>
    
    @param transfer transfer
    @param id id
    @see "openapi/spec3.json" /v1/transfers/\{transfer\}/reversals/\{id\} *)
let postTransfersTransferReversalsId () =
  Routes.(s "v1" / s "transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transfer" ~op:"/v1/transfers/{transfer}/reversals/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":transfer" / s "reversals"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/transfers/{transfer}/reversals/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>By default, you can see the 10 most recent reversals stored directly on the transfer object, but you can also retrieve details about a specific reversal stored on the transfer.</p>
    
    @param transfer transfer
    @param id id
    @see "openapi/spec3.json" /v1/transfers/\{transfer\}/reversals/\{id\} *)
let getTransfersTransferReversalsId () =
  Routes.(s "v1" / s "transfers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transfer" ~op:"/v1/transfers/{transfer}/reversals/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transfer" ~loc:`Path ~style:`Simple ~explode:false)
        ":transfer" / s "reversals"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/transfers/{transfer}/reversals/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Update a specified refund.</p>
    
    @param charge charge
    @param refund refund
    @see "openapi/spec3.json" /v1/charges/\{charge\}/refunds/\{refund\} *)
let postChargesChargeRefundsRefund () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/refunds/{refund}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"refund" ~op:"/v1/charges/{charge}/refunds/{refund}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"refund" ~loc:`Path ~style:`Simple ~explode:false)
        ":refund" /? nil)

(** <p>Retrieves the details of an existing refund.</p>
    
    @param charge charge
    @param refund refund
    @see "openapi/spec3.json" /v1/charges/\{charge\}/refunds/\{refund\} *)
let getChargesChargeRefundsRefund () =
  Routes.(s "v1" / s "charges"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"charge" ~op:"/v1/charges/{charge}/refunds/{refund}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"charge" ~loc:`Path ~style:`Simple ~explode:false)
        ":charge" / s "refunds"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"refund" ~op:"/v1/charges/{charge}/refunds/{refund}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"refund" ~loc:`Path ~style:`Simple ~explode:false)
        ":refund" /? nil)

(** <p>Delete a specified external account for a given account.</p>
    
    @param account account
    @param id id
    @see "openapi/spec3.json" /v1/accounts/\{account\}/external_accounts/\{id\} *)
let deleteAccountsAccountExternalAccountsId () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/external_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "external_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/accounts/{account}/external_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Updates the metadata, account holder name, account holder type of a bank account belonging to
    a connected account and optionally sets it as the default for its currency. Other bank account
    details are not editable by design.</p>
    
    <p>You can only update bank accounts when <a href="/api/accounts/object#account_object-controller-requirement_collection">account.controller.requirement_collection</a> is <code>application</code>, which includes <a href="/connect/custom-accounts">Custom accounts</a>.</p>
    
    <p>You can re-enable a disabled bank account by performing an update call without providing any
    arguments or changes.</p>
    
    @param account account
    @param id id
    @see "openapi/spec3.json" /v1/accounts/\{account\}/external_accounts/\{id\} *)
let postAccountsAccountExternalAccountsId () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/external_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "external_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/accounts/{account}/external_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieve a specified external account for a given account.</p>
    
    @param account account
    @param id id
    @see "openapi/spec3.json" /v1/accounts/\{account\}/external_accounts/\{id\} *)
let getAccountsAccountExternalAccountsId () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/external_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "external_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/accounts/{account}/external_accounts/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Retrieves a specific cash balance transaction, which updated the customer’s <a href="/docs/payments/customer-balance">cash balance</a>.</p>
    
    @param customer customer
    @param transaction transaction
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cash_balance_transactions/\{transaction\} *)
let getCustomersCustomerCashBalanceTransactionsTransaction () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cash_balance_transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cash_balance_transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/customers/{customer}/cash_balance_transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" /? nil)

(** <p>Most credit balance transaction fields are immutable, but you may update its <code>description</code> and <code>metadata</code>.</p>
    
    @param customer customer
    @param transaction transaction
    @see "openapi/spec3.json" /v1/customers/\{customer\}/balance_transactions/\{transaction\} *)
let postCustomersCustomerBalanceTransactionsTransaction () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/balance_transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "balance_transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/customers/{customer}/balance_transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" /? nil)

(** <p>Retrieves a specific customer balance transaction that updated the customer’s <a href="/docs/billing/customer/balance">balances</a>.</p>
    
    @param customer customer
    @param transaction transaction
    @see "openapi/spec3.json" /v1/customers/\{customer\}/balance_transactions/\{transaction\} *)
let getCustomersCustomerBalanceTransactionsTransaction () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/balance_transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "balance_transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"transaction" ~op:"/v1/customers/{customer}/balance_transactions/{transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":transaction" /? nil)

(** <p>Retrieves a new Source MandateNotification.</p>
    
    @param source source
    @param mandate_notification mandate_notification
    @see "openapi/spec3.json" /v1/sources/\{source\}/mandate_notifications/\{mandate_notification\} *)
let getSourcesSourceMandateNotificationsMandateNotification () =
  Routes.(s "v1" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"source" ~op:"/v1/sources/{source}/mandate_notifications/{mandate_notification}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"source" ~loc:`Path ~style:`Simple ~explode:false)
        ":source" / s "mandate_notifications"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"mandate_notification" ~op:"/v1/sources/{source}/mandate_notifications/{mandate_notification}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"mandate_notification" ~loc:`Path ~style:`Simple ~explode:false)
        ":mandate_notification" /? nil)

(** <p>Delete a specified source for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cards/\{id\} *)
let deleteCustomersCustomerCardsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cards/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/cards/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Update a specified source for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cards/\{id\} *)
let postCustomersCustomerCardsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cards/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/cards/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>You can always see the 10 most recent cards directly on a customer; this method lets you retrieve details about a specific card stored on the customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/cards/\{id\} *)
let getCustomersCustomerCardsId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/cards/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "cards"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/cards/{id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" /? nil)

(** <p>Cancels a customer’s subscription. If you set the <code>at_period_end</code> parameter to <code>true</code>, the subscription will remain active until the end of the period, at which point it will be canceled and not renewed. Otherwise, with the default <code>false</code> value, the subscription is terminated immediately. In either case, the customer will not be charged again for the subscription.</p>
    
    <p>Note, however, that any pending invoice items that you’ve created will still be charged for at the end of the period, unless manually <a href="#delete_invoiceitem">deleted</a>. If you’ve set the subscription to cancel at the end of the period, any pending prorations will also be left in place and collected at the end of the period. But if the subscription is set to cancel immediately, pending prorations will be removed.</p>
    
    <p>By default, upon subscription cancellation, Stripe will stop automatic collection of all finalized invoices for the customer. This is intended to prevent unexpected payment attempts after the customer has canceled a subscription. However, you can resume automatic collection of the invoices manually after subscription cancellation to have us proceed. Or, you could check for unpaid invoices before allowing the customer to cancel the subscription at all.</p>
    
    @param customer customer
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/subscriptions/\{subscription_exposed_id\} *)
let deleteCustomersCustomerSubscriptionsSubscriptionExposedId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" /? nil)

(** <p>Updates an existing subscription on a customer to match the specified parameters. When changing plans or quantities, we will optionally prorate the price we charge next month to make up for any price changes. To preview how the proration will be calculated, use the <a href="#upcoming_invoice">upcoming invoice</a> endpoint.</p>
    
    @param customer customer
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/subscriptions/\{subscription_exposed_id\} *)
let postCustomersCustomerSubscriptionsSubscriptionExposedId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" /? nil)

(** <p>Retrieves the subscription with the given ID.</p>
    
    @param customer customer
    @param subscription_exposed_id subscription_exposed_id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/subscriptions/\{subscription_exposed_id\} *)
let getCustomersCustomerSubscriptionsSubscriptionExposedId () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "subscriptions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"subscription_exposed_id" ~op:"/v1/customers/{customer}/subscriptions/{subscription_exposed_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"subscription_exposed_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":subscription_exposed_id" /? nil)

(** <p>Deletes an existing person’s relationship to the account’s legal entity. Any person with a relationship for an account can be deleted through the API, except if the person is the <code>account_opener</code>. If your integration is using the <code>executive</code> parameter, you cannot delete the only verified <code>executive</code> on file.</p>
    
    @param account account
    @param person person
    @see "openapi/spec3.json" /v1/accounts/\{account\}/persons/\{person\} *)
let deleteAccountsAccountPersonsPerson () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/persons/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "persons"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"person" ~op:"/v1/accounts/{account}/persons/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"person" ~loc:`Path ~style:`Simple ~explode:false)
        ":person" /? nil)

(** <p>Updates an existing person.</p>
    
    @param account account
    @param person person
    @see "openapi/spec3.json" /v1/accounts/\{account\}/persons/\{person\} *)
let postAccountsAccountPersonsPerson () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/persons/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "persons"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"person" ~op:"/v1/accounts/{account}/persons/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"person" ~loc:`Path ~style:`Simple ~explode:false)
        ":person" /? nil)

(** <p>Retrieves an existing person.</p>
    
    @param account account
    @param person person
    @see "openapi/spec3.json" /v1/accounts/\{account\}/persons/\{person\} *)
let getAccountsAccountPersonsPerson () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/persons/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "persons"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"person" ~op:"/v1/accounts/{account}/persons/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"person" ~loc:`Path ~style:`Simple ~explode:false)
        ":person" /? nil)

(** <p>Updates an invoice’s line item. Some fields, such as <code>tax_amounts</code>, only live on the invoice line item,
    so they can only be updated through this endpoint. Other fields, such as <code>amount</code>, live on both the invoice
    item and the invoice line item, so updates on this endpoint will propagate to the invoice item as well.
    Updating an invoice’s line item is only possible before the invoice is finalized.</p>
    
    @param invoice Invoice ID of line item
    @param line_item_id Invoice line item ID
    @see "openapi/spec3.json" /v1/invoices/\{invoice\}/lines/\{line_item_id\} *)
let postInvoicesInvoiceLinesLineItemId () =
  Routes.(s "v1" / s "invoices"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"invoice" ~op:"/v1/invoices/{invoice}/lines/{line_item_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"invoice" ~loc:`Path ~style:`Simple ~explode:false)
        ":invoice" / s "lines"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"line_item_id" ~op:"/v1/invoices/{invoice}/lines/{line_item_id}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"line_item_id" ~loc:`Path ~style:`Simple ~explode:false)
        ":line_item_id" /? nil)

(** <p>Verify a specified bank account for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/bank_accounts/\{id\}/verify *)
let postCustomersCustomerBankAccountsIdVerify () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/bank_accounts/{id}/verify" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "bank_accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/bank_accounts/{id}/verify" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "verify" /? nil)

(** <p>Deletes an existing person’s relationship to the account’s legal entity. Any person with a relationship for an account can be deleted through the API, except if the person is the <code>account_opener</code>. If your integration is using the <code>executive</code> parameter, you cannot delete the only verified <code>executive</code> on file.</p>
    
    @param account account
    @param person person
    @see "openapi/spec3.json" /v1/accounts/\{account\}/people/\{person\} *)
let deleteAccountsAccountPeoplePerson () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/people/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "people"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"person" ~op:"/v1/accounts/{account}/people/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"person" ~loc:`Path ~style:`Simple ~explode:false)
        ":person" /? nil)

(** <p>Updates an existing person.</p>
    
    @param account account
    @param person person
    @see "openapi/spec3.json" /v1/accounts/\{account\}/people/\{person\} *)
let postAccountsAccountPeoplePerson () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/people/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "people"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"person" ~op:"/v1/accounts/{account}/people/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"person" ~loc:`Path ~style:`Simple ~explode:false)
        ":person" /? nil)

(** <p>Retrieves an existing person.</p>
    
    @param account account
    @param person person
    @see "openapi/spec3.json" /v1/accounts/\{account\}/people/\{person\} *)
let getAccountsAccountPeoplePerson () =
  Routes.(s "v1" / s "accounts"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"account" ~op:"/v1/accounts/{account}/people/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"account" ~loc:`Path ~style:`Simple ~explode:false)
        ":account" / s "people"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"person" ~op:"/v1/accounts/{account}/people/{person}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"person" ~loc:`Path ~style:`Simple ~explode:false)
        ":person" /? nil)

(** <p>Verify a specified bank account for a given customer.</p>
    
    @param customer customer
    @param id id
    @see "openapi/spec3.json" /v1/customers/\{customer\}/sources/\{id\}/verify *)
let postCustomersCustomerSourcesIdVerify () =
  Routes.(s "v1" / s "customers"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"customer" ~op:"/v1/customers/{customer}/sources/{id}/verify" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"customer" ~loc:`Path ~style:`Simple ~explode:false)
        ":customer" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"id" ~op:"/v1/customers/{customer}/sources/{id}/verify" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"id" ~loc:`Path ~style:`Simple ~explode:false)
        ":id" / s "verify" /? nil)

(** <p>Retrieve an existing source transaction object. Supply the unique source ID from a source creation request and the source transaction ID and Stripe will return the corresponding up-to-date source object information.</p>
    
    @param source source
    @param source_transaction source_transaction
    @see "openapi/spec3.json" /v1/sources/\{source\}/source_transactions/\{source_transaction\} *)
let getSourcesSourceSourceTransactionsSourceTransaction () =
  Routes.(s "v1" / s "sources"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"source" ~op:"/v1/sources/{source}/source_transactions/{source_transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"source" ~loc:`Path ~style:`Simple ~explode:false)
        ":source" / s "source_transactions"
    / Routes.pattern
        (ParamSerDe'.string_of_p_String_ ~p:"source_transaction" ~op:"/v1/sources/{source}/source_transactions/{source_transaction}" ~loc:`Path ~style:`Simple ~explode:false)
        (ParamSerDe'.string_to_p_String_ ~p:"source_transaction" ~loc:`Path ~style:`Simple ~explode:false)
        ":source_transaction" /? nil)

