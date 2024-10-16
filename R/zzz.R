.onAttach <- function(libname, pkgname) {
 message <- paste0(
  pkgname, " package (v", utils::packageVersion(pkgname), ") includes significant updates to the codebase, aimed at reducing unexpected behavior and minimizing dependencies.\n",
  "If these changes cause issues with your existing code, you can access a previous version of the package from the archive.\n",
  "For more information, visit:\nhttps://github.com/hendr1km/dropout"
 )
 packageStartupMessage(message)
}



