.onAttach <- function(libname, pkgname) {
  ver <- utils::packageVersion(pkgname)
  banner <- paste0(
"
   ____   ____  _   _      _ __     ___
  / ___| / ___|| \\ | | ___| |\\ \\   / (_) _____      __
 | |  _ | |  _ |  \\| |/ _ \\ __\\ \\ / /| |/ _ \\ \\ /\\ / /
 | |_| || |_| || |\\  |  __/ |_ \\ V / | |  __/\\ V  V /
  \\____| \\____||_| \\_|\\___|\\__| \\_/  |_|\\___| \\_/\\_/

ggNetView: Reproducible and Deterministic Network Analysis and Visualization
Version: ", ver, "

  Authors:     Yue Liu, Chao Wang
  Maintainer:  Yue Liu <yueliu@iae.ac.cn>

  Manual:      https://jiawang1209.github.io/ggNetView-manual/
  GitHub:      https://github.com/Jiawang1209/ggNetView
  Bug Reports: https://github.com/Jiawang1209/ggNetView/issues

  Type citation('ggNetView') for how to cite this package.
"
  )
  packageStartupMessage(banner)
}
