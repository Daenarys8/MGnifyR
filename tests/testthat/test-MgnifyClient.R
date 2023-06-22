context("MgnifyClient")
test_that("MgnifyClient", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    
    expect_error(MgnifyClient(username = 8))
    expect_error(MgnifyClient(username = 6.5))
    expect_error(MgnifyClient(username = "test",
                              password = 28))
    expect_error(MgnifyClient(
        username = "test",
        password = "study",
        cacheDir = tree,
        
    ))
    expect_error(
        MgnifyClient(
            username = "test",
            password = "study",
            cacheDir = "TreeSE",
            useMemCache = tree,
            url = "taxonomy-ssu",
        )
    )
    expect_error(
        MgnifyClient(
            username = TRUE,
            password = "study",
            useCache = 87,
            cacheDir = "TreeSE",
            url = "taxonomy-ssu",
        )
    )
    expect_error(MgnifyClient(username = "test",
                              password = TRUE,))
    expect_error(MgnifyClient(username = "test",
                              password = tree,))
    expect_error(MgnifyClient(username = "tree",
                              password = "test",))
    expect_error(MgnifyClient(
        username = "test",
        password = "test",
        cacheDir = "TreeSE",
        url = 5,
    ))
    expect_error(
        MgnifyClient(
            username = "test",
            password = "test",
            cacheDir = "TreeSE",
            url = TreeSE,
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
