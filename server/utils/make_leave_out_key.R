# Path: "server/utils/make_leave_out_key.R"

make_leave_out_key <- function(a, b) {
  ab <- sort(c(as.character(a), as.character(b)))
  paste0(ab[1], "|", ab[2])
}