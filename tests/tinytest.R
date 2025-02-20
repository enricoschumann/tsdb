if (requireNamespace("tinytest", quietly = TRUE))
    tinytest.results <- tinytest::test_package("tsdb",
                                               color = interactive(),
                                               verbose = 1)
