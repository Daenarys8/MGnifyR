context("searchAnalysis")
test_that("searchAnalysis", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    # Expect errors when input is wrong
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = "test",
            use.cache = "studies",
            verbose = TRUE
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = "studies",
            use.cache = "test",
            verbose = TRUE
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "samples",
            accession = c("studies", "assembly"),
            use.cache = "studies",
            verbose = TRUE
        )
    )
    
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = "TreeSE",
            use.cache = "studies",
            verbose = FALSE
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = c("TreeSE", "phyloseq"),
            use.cache = "test",
            verbose = FALSE
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "samples",
            accession = c("taxonomy-ssu", "go-slim"),
            use.cache = "studies",
            verbose = FALSE
        )
    )
    
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = TRUE,
            use.cache = TRUE,
            verbose = TRUE
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = FALSE,
            use.cache = TRUE,
            verbose = TRUE
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "samples",
            accession = TRUE,
            use.cache = TRUE,
            verbose = TRUE
        )
    )
    
    # Require internet access
    skip_if(httr::http_error("https://www.ebi.ac.uk/metagenomics/api/v1"))
    
    # Test that correct analysis IDs are found based on study ID.
    expect_warning(res <-
                       searchAnalysis(mg, "studies", "MGYA00097621", verbose = FALSE))
    expect_true(is.null(res))
    res <-
        searchAnalysis(mg, "studies", "MGYS00005058", verbose = FALSE)
    expect_true(length(res) > 0)
    expect_true("MGYA00377528" %in% res)
    # Test that correct analysis IDs are found based on sample ID.
    expect_warning(res <-
                       searchAnalysis(mg, "samples", "MGYA00097621", verbose = FALSE))
    expect_true(is.null(res))
    res <-
        searchAnalysis(mg, "samples", "ERS2161777", verbose = FALSE)
    expect_true(length(res) > 0)
    expect_true("MGYA00293854" %in% res)
})
