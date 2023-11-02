test_that('File extension checkers works', {
  tar <- 'microCRAN_0.1.0.tar.gz'
  zip <- 'microCRAN_0.1.0.zip'
  tgz <- 'microCRAN_0.1.0.tgz'

  expect_true(is_tar_ext(tar))
  expect_false(is_zip_ext(tar))
  expect_false(is_tgz_ext(tar))

  expect_false(is_tar_ext(zip))
  expect_true(is_zip_ext(zip))
  expect_false(is_tgz_ext(zip))

  expect_false(is_tar_ext(tgz))
  expect_false(is_zip_ext(tgz))
  expect_true(is_tgz_ext(tgz))

  no_ext <- 'microCRAN_0.1.0'

  expect_false(is_tar_ext(no_ext))
  expect_false(is_zip_ext(no_ext))
  expect_false(is_tgz_ext(no_ext))

  no <- 'microCRAN_0.1.0.tar.gz .tgz .zip.exe'

  expect_false(is_tar_ext(no))
  expect_false(is_zip_ext(no))
  expect_false(is_tgz_ext(no))
})

test_that('Extracting Built-version from DESCRIPTION', {
  s <- 'Built: R 4.3.0; ; 2023-10-16 08:54:21 UTC; windows'
  expect_equal(extract_built(s), '4.3.0')

  expect_equal(extract_built('4.3.0'), character(0))
  expect_equal(extract_built('bad string'), character(0))
})

test_that('Detecting package type by extension', {
  tar <- 'microCRAN_0.1.0.tar.gz'
  zip <- 'microCRAN_0.1.0.zip'
  tgz <- 'microCRAN_0.1.0.tgz'

  expect_equal(r_package_type(tar), 'source')
  expect_equal(r_package_type(tgz), 'mac.binary')
  expect_equal(r_package_type(zip), 'win.binary')

  expect_condition(r_package_type('bad_name.tar.gz .zip .tgz .exe'),
    "Invalid file extension",
    class = 'http_condition')
})

test_that('Finding DESCRIPTION file in packaged listing', {
  s <- c("microCRAN/", "microCRAN/DESCRIPTION", "microCRAN/NAMESPACE",
         "microCRAN/R/", "microCRAN/R/api.R", "microCRAN/R/hello.R")
  expect_equal(find_description(s), s[2])
  s <- gsub('microCRAN/', 'DESCRIPTION/', s, fixed = TRUE)
  expect_equal(find_description(s), s[2])

  s <- c(s,s)
  expect_equal(find_description(s), s[2])

  s <- s[1]
  expect_equal(find_description(s), character(0))
})

test_that('Reading DESCRIPTION file from zip-file', {
  #fn <- test_path('../files/microCRAN_0.1.0.zip')
  fn <- extdata_path('microCRAN_0.1.0.zip')
  expect_true(file.exists(!!fn))
  d <- read_DESCRIPTION_zip(fn)
  expect_type(d, "list")
  expect_named(d, c(
    "Package", "Type", "Title", "Version", "Author", "Maintainer",
    "Description", "License", "Import", "Encoding", "NeedsCompilation",
    "Packaged", "Built"),
    ignore.order = TRUE)
  expect_equal(d$Package, 'microCRAN')

  fn <- sub('.zip', '.tar.gz', fn, fixed = TRUE)
  expect_true(file.exists(!!fn))
  expect_error(read_DESCRIPTION_zip(fn), 'Invalid R-package file', fixed = TRUE)
  expect_error(read_DESCRIPTION_zip('x'), 'Invalid R-package file', fixed = TRUE)
})

test_that('Reading DESCRIPTION file from tar-file', {
  #fn <- test_path('../files/microCRAN_0.1.0.tar.gz')
  fn <- extdata_path('microCRAN_0.1.0.tar.gz')
  expect_true(file.exists(!!fn))
  d <- read_DESCRIPTION_tar(fn)
  expect_type(d, "list")
  expect_named(d, c(
    "Package", "Type", "Title", "Version", "Author", "Maintainer",
    "Description", "License", "Import", "Encoding", "NeedsCompilation",
    "Packaged"),
    ignore.order = TRUE)
  expect_equal(d$Package, 'microCRAN')

  fn <- sub('.tar.gz', '.zip', fn, fixed = TRUE)
  expect_true(file.exists(!!fn))
  suppressWarnings(
    expect_error(read_DESCRIPTION_tar(fn), 'Invalid R-package file', fixed = TRUE)
  )
  expect_error(read_DESCRIPTION_tar('x'), 'File not found: x', fixed = TRUE)
})

