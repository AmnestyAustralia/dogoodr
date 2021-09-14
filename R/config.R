
dogooder_token <- function() {
  tok <- Sys.getenv("DG_TOKEN")
  if (identical(tok, "")) {
    stop(
      "Please set env var DG_TOKEN to your dogooder API token",
      call. = FALSE
    )
  }
  tok
}

dogooder_subdomain <- function() {
  sdom <- Sys.getenv("DG_SUBDOMAIN")
  if (identical(sdom, "")) {
    stop(
      "Please set env var DG_SUBDOMAIN to your dogooder subdomain ",
      "eg. https://[THIS].good.do/developer/",
      call. = FALSE
    )
  }
  sdom
}
