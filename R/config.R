
dogooder_token <- function(call = rlang::caller_env()) {
  tok <- Sys.getenv("DG_TOKEN")
  if (identical(tok, "")) {
    cli_abort(
      "Please set env var {.var DG_TOKEN} to your Do Gooder API token.",
      call = call
    )
  }
  tok
}

dogooder_subdomain <- function(call = rlang::caller_env()) {
  sdom <- Sys.getenv("DG_SUBDOMAIN")
  if (identical(sdom, "")) {
    cli_abort(
      c("Please set env var {.var DG_SUBDOMAIN} to your Do Gooder subdomain.",
        i = "e.g. {.val https://[THIS].good.do/developer/}"),
      call = call
    )
  }
  sdom
}
