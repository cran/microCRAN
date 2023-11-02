test_that('adding package router', {
  subtmp <- paste(sample(letters, 10), collapse='')
  p <- file.path(tempdir(), subtmp)

  router <- pr() |>
    pr_add_package(path = '/bla', p)

  route <- router$routes[[1]]
  expect_setequal(route$verbs, c('PUT','POST'))
  expect_equal(route$path, "/bla")

  f <- list()
  package_fn <- 'microCRAN_0.1.0.tar.gz'
  #fn <- system.file('extdata/microCRAN_0.1.0.tar.gz', package='microCRAN', mustWork=TRUE)
  #  fn <- test_path('../files', package_fn)
  fn <- extdata_path(package_fn)
  f[[package_fn]] <- readBin(fn, 'raw', n = 2000)
  res <- plumber:::PlumberResponse$new()
  req <- list(REQUEST_METHOD = 'POST')
  route$getFunc()(req, res, f)

  dest_fn <- file.path(p, 'src/contrib', package_fn)
  expect_true(file.exists(dest_fn))
  dest_f <- readBin(dest_fn, 'raw', n = 2000)
  expect_equal(dest_f, f[[package_fn]])
})

test_that('error handling route handler http_conditions nicely', {
  e <- http_condition(404L)
  expect_equal(e$status_code, 404L)
  expect_equal(e$message, "Status code: 404")

  e <- http_condition(400, 'ouch')
  res <- plumber:::PlumberResponse$new()
  expect_equal(res$status, 200)
  expect_null(res$body)
  expect_length(res$headers, 0)

  cran_error_handler(NULL, res, e)
  expect_equal(res$status, 400)
  expect_equal(res$body, 'ouch')

  e <- simpleError("Some runtime error")
  expect_error(cran_error_handler(NULL, res, e), e$message, fixed = TRUE)
})


