extdata_path <- \(x, mustWork = TRUE) {
  system.file(file.path('extdata/', x), package = 'microCRAN', mustWork = mustWork)
}

setup_local_registry <- function() {
  subtmp <- paste(sample(letters, 10), collapse = '')
  temp_root <- file.path(tempdir(), subtmp)
  #as_path <- \(x) test_path('../../', x)
  #as_path <- \(x) system.file(file.path('extdata/', x), package = 'microCRAN', mustWork = TRUE)

  fns <- c('microCRAN_0.1.0.tar.gz','microCRAN_0.1.0.zip')
  for (fn in fns) {
    addPackage(extdata_path(fn), type = r_package_type(fn),
               repo_dir = temp_root, is.new = FALSE)
  }

  writeBin(charToRaw('Hello world!\n'), file.path(temp_root, 'src/contrib', 'README'))
  #writeLines('Hello world', file.path(temp_root, 'src/contrib', 'README'))

  return(temp_root)
}
