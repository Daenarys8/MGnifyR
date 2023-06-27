context("MgnifyClient")
test_that("MgnifyClient", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    
    expect_error(MgnifyClient(username = 8))
    expect_error(MgnifyClient(username = 6.5))
    expect_error(MgnifyClient(password = 28))
    expect_error(MgnifyClient(password = 28.8))
    expect_error(MgnifyClient(username = TRUE))
    expect_error(MgnifyClient(password = TRUE))
    expect_error(MgnifyClient(url = 5))
    expect_error(MgnifyClient(url = 5.7))
    expect_error(MgnifyClient(url = FALSE))
    expect_error(MgnifyClient(useMemCache =  NULL))
    expect_error(MgnifyClient(cacheDir = TRUE))
    expect_error(MgnifyClient(cacheDir = 10))
    expect_error(MgnifyClient(cacheDir = 5.4))
    expect_error(MgnifyClient(useCache = 5))
    expect_error(MgnifyClient(useCache = 8.9))
    expect_error(MgnifyClient(useCache = NULL))
    
    
    # Test that slots are updated. Change arguments --> check that values
    # of slots correspond argument.
    mg <- MgnifyClient(
        useCache = TRUE,
        cacheDir = "test",
        warnings = FALSE,
        useMemCache = TRUE,
        url = "test"
    )
    expect_equal(mg@cacheDir, "test")
    expect_equal(mg@warnings, FALSE)
    expect_equal(mg@useMemCache, TRUE)
    expect_equal(mg@url, "test")
    mg <- MgnifyClient(
        useCache = FALSE,
        cacheDir = "test",
        warnings = TRUE,
        useMemCache = FALSE,
    )
    expect_true(is.na(mg@cacheDir))
    expect_equal(mg@warnings, TRUE)
    expect_equal(mg@useMemCache, FALSE)
    # Require internet access
    skip_if(httr::http_error("https://www.ebi.ac.uk/metagenomics/api/v1"))
    # Test that error occurs when wrong username/password is used in
    # authentication
    expect_error(MgnifyClient(username = "not_work",
                              password = "not_work"))
})
