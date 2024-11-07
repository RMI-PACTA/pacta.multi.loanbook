ask_for_permission <- function(msg) {
  if (!yesno::yesno(cli::cli_text(msg))) {
    cli::cli_abort(message = "The process cannot continue because you answered \"no\"")
  }
  
  invisible(TRUE)
}
