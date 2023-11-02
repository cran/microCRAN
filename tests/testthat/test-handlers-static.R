test_that('Static paths are properly decomposed', {
  expect_equal(extract_cran_path_parts('/src/contrib'), c('src/contrib','/'))
  expect_equal(extract_cran_path_parts('/src/contrib/'), extract_cran_path_parts('/src/contrib'))
  expect_equal(extract_cran_path_parts('/bin/windows/contrib'), c('bin/windows/contrib','/'))
  expect_equal(extract_cran_path_parts('/bin/windows/contrib'), extract_cran_path_parts('/bin/windows/contrib/'))
  expect_equal(extract_cran_path_parts('/bin/macosx/contrib'), c('bin/macosx/contrib','/'))
  expect_equal(extract_cran_path_parts('/bin/macosx/contrib'), extract_cran_path_parts('/bin/macosx/contrib/'))

  expect_equal(extract_cran_path_parts('/src/contrib/PACKAGES'), c('src/contrib','PACKAGES'))
  expect_equal(extract_cran_path_parts('/src/contrib/PACKAGES.gz'), c('src/contrib','PACKAGES.gz'))
  expect_equal(extract_cran_path_parts('/src/contrib/PACKAGES.rds'), c('src/contrib','PACKAGES.rds'))
  expect_equal(extract_cran_path_parts('/src/contrib/microCRAN_1.1.0.tar.gz'), c('src/contrib','microCRAN_1.1.0.tar.gz'))

  expect_equal(extract_cran_path_parts('/bin/windows/contrib/4.3/'), c('bin/windows/contrib/4.3','/'))
  expect_equal(extract_cran_path_parts('/bin/windows/contrib/4.0/PACKAGES'), c('bin/windows/contrib/4.0','PACKAGES'))
  expect_equal(extract_cran_path_parts('/bin/windows/contrib/4.1/PACKAGES.gz'), c('bin/windows/contrib/4.1','PACKAGES.gz'))
  expect_equal(extract_cran_path_parts('/bin/windows/contrib/3.4/PACKAGES.rds'), c('bin/windows/contrib/3.4','PACKAGES.rds'))
})

test_that('Request to CRAN resources are safeguarded against misuse', {
  paths <- c(
    '/src/contrib', '/bin/windows/contrib', '/bin/macosx/contrib'
  )
  safe_components <- c(
    '', '/', 'PACKAGES', 'PACKAGES.gz', 'PACKAGES.rds','any_file',
    'microCRAN.2_1.0.0.tar.gz', 'microCRAN.2_1.0.0.zip',
    '4.2', '4.2/', '4.2/PACKAGES', '4.2/microCRAN.2_1.0.0.zip'
  )

  safe_paths <- file.path(
    paths,
    rep(safe_components, each = length(paths))
  )

  for (s in safe_paths) {
    expect_true(path_is_safe_for_cran(!!s))
  }

  # unsafe_paths <- c('', 'src', 'bin', 'bin/windows', 'bin/macosx', 'bin/nogo', 'bin/nogo/contrib')
  # unsafe_paths <- expand.grid(c('','/'), unsafe_paths, c('','/')) |>
  #   with(paste0(Var1, Var2, Var3))
  # for (s in unsafe_paths) {
  #   expect_false(path_is_safe_for_cran(!!s))
  # }

  unsafe_components <- c('..', '../..', '../../', '/../', '../', 'C:/', 'C:\\', 'not_valid_package..zip')
  unsafe_paths <- file.path(
    paths,
    rep(unsafe_components, each = length(paths))
  )

  for (s in unsafe_paths) {
    expect_false(path_is_safe_for_cran(!!s))
  }
})

test_that('simple formatting fixe sizes', {
  sizes <- c(0, 900, 1025, 1024**2, 5000*1024, 1024**3, 1024**4)
  expect_equal(format_file_size(sizes),
    c("0", "900", "1K", "1M", "5M", "1G", "1024G"))
})

test_that('Static path handler', {
  temp_root <- setup_local_registry()
  as_path = extdata_path

  handler <- \(req) {
    req$REQUEST_METHOD  <- 'GET'
    res <- plumber:::PlumberResponse$new()
    cran_static_path_handler(temp_root)(req, res)
    res
  }

  res <- handler(list(PATH_INFO = '/src'))
  expect_null(res$body)
  expect_equal(res$headers, list(), ignore_attr = TRUE)

  res <- handler(list(PATH_INFO = '/src/contrib'))
  expect_match(res$body, "<html>.*?<head>.*?<title>.*?Index of /src/contrib</title>")
  expect_equal(res$status, 200)

  res <- handler(list(PATH_INFO = '/src/contrib/'))
  expect_match(res$body, "<html>.*?<head>.*?<title>.*?Index of /src/contrib.*?</title>")
  expect_equal(res$status, 200)

  expect_condition(handler(list(PATH_INFO = '/src/contrib/pkg_1.0.0.tar.gz')),
    'Resource not found', fixed = TRUE, class = 'http_condition')

  res <- handler(list(PATH_INFO = '/src/contrib/README'))
  expect_equal(rawToChar(res$body), "Hello world!\n")
  expect_equal(res$status, 200)
  expect_equal(res$headers$`Content-Type`, "text/plain")

  res <- handler(list(PATH_INFO = '/src/contrib/microCRAN_0.1.0.tar.gz'))
  f <- readBin(as_path('microCRAN_0.1.0.tar.gz'), 'raw', n = 2000)
  expect_equal(res$body, f)
  expect_equal(res$headers$`Content-Length`, length(f))
  expect_equal(res$status, 200)

  res <- handler(list(PATH_INFO = '/bin/windows/contrib/4.3/microCRAN_0.1.0.zip'))
  f <- readBin(as_path('microCRAN_0.1.0.zip'), 'raw', n = 9000)
  expect_equal(res$body, f)
  expect_equal(res$headers$`Content-Length`, length(f))
  expect_equal(res$status, 200)
})
