## non-exported functions from miniCRAN

repoPrefix <- function(type, Rversion) {
  if (type == 'source') return('src/contrib')

  Rversion <- as_version(Rversion)
  Rversion <- as.numeric_version(paste(Rversion[[1,1:2]], collapse = '.'))
  v3.4 <- as.numeric_version("3.4")
  if (type == "mac.binary.el-capitan" && Rversion < v3.4) {
    warning("Type mac.binary.el-capitan only valid for R >= 3.4")
  } else if (type == "mac.binary.mavericks" && Rversion >= v3.4) {
    warning("Type mac.binary.mavericks only valid for R < 3.4")
  }
  switch(type,
    win.binary = sprintf("bin/windows/contrib/%s", Rversion),
    mac.binary = sprintf("bin/macosx/contrib/%s", Rversion),
    `mac.binary.el-capitan` = sprintf("bin/macosx/el-capitan/contrib/%s", Rversion),
    mac.binary.leopard = sprintf("bin/macosx/leopard/contrib/%s", Rversion),
    mac.binary.mavericks = sprintf("bin/macosx/mavericks/contrib/%s", Rversion),
    stop("Type ", type, " not recognised."))
}

as_version <- function(Rversion = R.version) {
  tryCatch(return(as.numeric_version(Rversion)), error = identity)
  if (inherits(Rversion, "list")) {
    s <- paste0(Rversion$major, '.', Rversion$minor)
  } else {
    s <- paste(Rversion[1:max(length(Rversion),3)], collapse = '.')
  }
  as.numeric_version(s)
}
