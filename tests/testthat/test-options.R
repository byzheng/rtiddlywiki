test_that("test options", {
    options <- tw_options(host = "http://127.0.0.1:8081")
    expect_equal(as.character(options$host), "http://127.0.0.1:8081")

    tw_reset()
    options <- tw_options()

    expect_equal(as.character(options$host), "http://127.0.0.1:8080/")

})

