context("getFile")
test_that("getFile", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    expect_error(getFile( mg, url = 10))
    expect_error(getFile( mg, url = "studies", file = 10))
    expect_error(getFile( mg, url = "TreeSE", read.func = 10))
    expect_error(getFile( mg, url = "taxonomy--ssu", use.cache = 10))
    expect_error(getFile( mg, url = "test", use.cache = test))
    
    expect_error(searchFile( mg, accession = TRUE ))
    expect_error(searchFile( mg, accession = FALSE))
    expect_error(searchFile( mg, accession = NULL ))
    expect_error(searchFile( mg, accession = "MGYS00005292", type = "samples", use.cache = NULL))
    expect_error(searchFile( mg, accession = "MGYS00005292", type = "samples", verbose = NULL))
    expect_error(searchFile( mg, accession = 12))
    expect_error(searchFile( mg, accession = 12.6))
    
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
    res <- searchFile(mg, type = "studies", accession = "MGYS00005292", verbose = FALSE)
    expect_true(all(res$type == "studies"))
    expect_true(is.data.frame(res))
    expect_true(grepl("https", res$download_url[1]))
    
    # Test that correct file is fetched based on provided url.
    res <- getFile(mg, res$download_url[1])
    # Result is stored in a path which is returned
    expect_true(file.exists(res))
})
