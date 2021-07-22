dg_campaigns <- function(){
  dg_api(endpoint="campaigns", out_class="campaigns", clean_response=TRUE)
}
