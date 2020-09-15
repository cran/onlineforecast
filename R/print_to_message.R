#' @title Simple function for capturing from the print function and send it in a message().
#' @param ... Passed to print which passed to message.

print_to_message <- function(...) {
    message(paste(utils::capture.output(print(...)), collapse="\n"))
}
