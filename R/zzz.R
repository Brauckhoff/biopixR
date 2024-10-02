.onAttach <- function(libname, pkgname) {
  # Check if X11 is available
  if (capabilities("X11")) {
    if (requireNamespace("tcltk", quietly = TRUE)) {
      packageStartupMessage("tcltk loaded successfully with X11 support.")
    } else {
      packageStartupMessage("tcltk is suggested but not installed.")
    }
  } else {
    packageStartupMessage("X11 not available, skipping tcltk.")
  }
}

