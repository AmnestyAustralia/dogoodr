devtools::load_all(here::here())

Sys.setenv("DG_SUBDOMAIN"=rstudioapi::askForPassword("https://[THIS].good.do/developer/"))
browseURL(glue("https://{Sys.getenv('DG_SUBDOMAIN')}.good.do/developer/"))
Sys.setenv("DG_TOKEN"=rstudioapi::askForPassword("Token ******..."))

campaigns <- dg_api(endpoint="campaigns", out_class="campaigns", clean_response=TRUE)
View(campaigns)

most_recent_campaign_id <- campaigns[["id"]][1]
action_paginated <- dg_api(endpoint="action-log-feed", out_class="actionfeed",
                           query_param = list(campaign_ids=most_recent_campaign_id),
                           clean_response=TRUE, process_pagination=TRUE)
# More than 100 rows?
nrow(action_paginated)
# Shouldn't include a next attribute if it has downloaded everything
attributes(action_paginated)
actions <- purrr::map_dfr(action_paginated$form_data, ~.x)
View(actions)

actions_since_this_morning <- dg_actions()
