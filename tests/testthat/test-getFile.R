context("getFile")
test_that("getFile", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    # Expect errors when input is wrong
    expect_error(getFile(
        mg,
        url = "test",
        file = "studies",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(getFile(
        mg,
        url = "studies",
        file = "studies",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(getFile(
        mg,
        url = "TreeSE",
        file = "studies",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(
        getFile(
            mg,
            url = "taxonomy--ssu",
            file = "studies",
            read.func = NULL,
            use.cache = TRUE
        )
    )
    
    expect_error(getFile(
        mg,
        url = "test",
        file = "TreeSE",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(getFile(
        mg,
        url = "test",
        file = "taxonomy-ssu",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(getFile(
        mg,
        url = "test",
        file = NULL,
        read.func = NULL,
        use.cache = TRUE
    ))
    
    expect_error(getFile(
        mg,
        url = "studies",
        file = "TreeSE",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(getFile(
        mg,
        url = "studies",
        file = "taxonmy-ssu",
        read.func = NULL,
        use.cache = TRUE
    ))
    
    expect_error(getFile(
        mg,
        url = "TreeSE",
        file = "TreeSE",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(getFile(
        mg,
        url = "TreeSE",
        file = "taxonomy-ssu",
        read.func = NULL,
        use.cache = TRUE
    ))
    expect_error(getFile(
        mg,
        url = "TreeSE",
        file = NULL,
        read.func = NULL,
        use.cache = TRUE
    ))
    
    expect_error(
        getFile(
            mg,
            url = "taxonomy--ssu",
            file = "TreeSE",
            read.func = NULL,
            use.cache = TRUE
        )
    )
    expect_error(
        getFile(
            mg,
            url = "taxonomy--ssu",
            file = "taxonomy-ssu",
            read.func = NULL,
            use.cache = TRUE
        )
    )
    expect_error(getFile(
        mg,
        url = "taxonomy--ssu",
        file = NULL,
        read.func = NULL,
        use.cache = TRUE
    ))
    
    
    
    # Expect errors when input is wrong
    expect_error(searchFile(
        mg,
        accession = TRUE,
        type = "studies",
        use.cache = TRUE,
        verbose = TRUE
    ))
    expect_error(searchFile(
        mg,
        accession = FALSE,
        type = "studies",
        use.cache = TRUE,
        verbose = TRUE
    ))
    expect_error(searchFile(
        mg,
        accession = NULL,
        type = "samples",
        use.cache = TRUE,
        verbose = TRUE
    ))
    expect_error(searchFile(
        mg,
        accession = c("studies", "assembly"),
        type = "samples",
        use.cache = NULL,
        verbose = TRUE
    ))
    expect_error(searchFile(
        mg,
        accession = c("studies", "assembly"),
        type = "samples",
        use.cache = TRUE,
        verbose = NULL
    ))
    
    # Require internet access
    skip_if(httr::http_error("https://www.ebi.ac.uk/metagenomics/api/v1"))
    
    # Test that df is returned even if accession ID is not correct
    res <-
        searchFile(mg,
                   type = "assemblies",
                   accession = "random",
                   verbose = FALSE)
    expect_true(is.data.frame(res))
    
    # Test that file search is done correctly based on accession ID.
    # Use studies as type
    mg <- MgnifyClient()
    res <- searchFile(mg,
                      type = "studies",
                      accession = "MGYS00005292",
                      verbose = FALSE)
    expect_true(all(res$type == "studies"))
    expect_true(is.data.frame(res))
    expect_true(grepl("https", res$download_url[1]))
    
    # Test that correct file is fetched based on provided url.
    res <- getFile(mg, res$download_url[1])
    # Result is stored in a path which is returned
    expect_true(file.exists(res))
})
