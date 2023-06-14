context("MgnifyClient")
test_that("MgnifyClient", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    # Expect errors when input is wrong
    expect_error(
        MgnifyClient(
            username = "test",
            password = "study",
            useCache = TRUE,
            cacheDir = "test",
            warnings = TRUE,
            useMemCache = TRUE,
            url = "test",
        )
    )
    expect_error(
        MgnifyClient(
            username = "test",
            password = "study",
            useCache = TRUE,
            cacheDir = "study",
            warnings = TRUE,
            useMemCache = TRUE,
            url = "taxonomy-ssu",
        )
    )
    expect_error(
        MgnifyClient(
            username = "test",
            password = "study",
            useCache = TRUE,
            cacheDir = "taxonomy-ssu",
            warnings = TRUE,
            useMemCache = TRUE,
            url = "study",
        )
    )
    expect_error(
        MgnifyClient(
            username = "test",
            password = "study",
            useCache = TRUE,
            cacheDir = "TreeSE",
            warnings = TRUE,
            useMemCache = TRUE,
            url = "TreeSE",
        )
    )
    expect_error(
        MgnifyClient(
            username = "test",
            password = "study",
            useCache = TRUE,
            cacheDir = NULL,
            warnings = TRUE,
            useMemCache = TRUE,
            url = "test",
        )
    )
    expect_error(
        MgnifyClient(
            username = "test",
            password = "study",
            useCache = TRUE,
            cacheDir = "TreeSE",
            warnings = TRUE,
            useMemCache = TRUE,
            url = "taxonomy-ssu",
        )
    )
    
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
