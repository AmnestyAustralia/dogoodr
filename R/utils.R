polite_message <- function(msg, silent=TRUE, env=parent.frame()){
  if(!silent){
    return(message(glue(msg, .envir=env)))
  }
}
