# dogoodr
dogooder.co API wrapper package for R

This package is under active development, which focuses on the DG API functionality found at https://[YOUR-DG-INSTANCE-PREFIX].good.do/developer/

### Setup

Set your DoGooder API token (including the "Token" text) to the env var DG_TOKEN
Set your DoGooder instance subdomain (ie. https://[THIS].good.do/developer/) to DG_SUBDOMAIN

You should be able to find your API token on your developer portal:
https://[YOUR-DG-INSTANCE-PREFIX].good.do/developer/

### Syntax

The package currently provides two higher level functions to interface with dogooder.

*dg_campaigns()*
Returns a tibble of your account's dogooder campaigns. Includes campaign name, ID, description, country (eg. AU), country_display (eg. Australia), state (eg. A). state_display (eg. Active), result (eg. U), result_display (eg. Undetermined). campaign_url, external_url, campaign_tag, featured_action, actions.

*dg_actions()*
Returns a tibble of actions matching your request arguments. DoGooders' API paginates responses in chunks of 100, in order to recursively request all possible actions, use process_pagination=TRUE.

Returned tibble will include id, email address, subscribed_to_campaign, test, created, promotion_name, subscribed_to_organisation, and three nested data.tables (API returns these as nested JSON). Nested data.tables are campaign, action, form_data.

If these aren't meeting your needs, the following lower level functions may help...

*dg_api()*
Make a request to the dogooder API, using env variables configured, specify an out_class which will be used to identify an appropriate parse_response to format the returned json into a list.
