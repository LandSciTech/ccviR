# Declare global variables which we cannot import or reference with .data
.onLoad <- function(libname, pkgname) {
  if (getRversion() >= "2.15.1") {
    utils::globalVariables(c("."))
  }
}
