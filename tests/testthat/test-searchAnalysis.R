context("searchAnalysis")
test_that("searchAnalysis", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = "test",
            use.cache = "studies",
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = studies,
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "samples",
            accession = assembly,

        )
    )
    
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = 67,

        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = TRUE,

        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = 77,
            accession = c("taxonomy-ssu", "go-slim"),

        )
    )
    
    expect_error(
        searchAnalysis(
            mg,
            type = NULL,
            accession = "test",

        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "studies",
            accession = "test",
            use.cache = 80,
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "samples",
            accession = "test",
            use.cache = No,
        )
    )
    expect_error(
        searchAnalysis(
            mg,
            type = "samples",
            accession = "test",
            verbose = 20
        )
    )
    expect_error(searchAnalysis(
        mg,
        type = "samples",
        accession = "test",
        verbose = "Tree"
    ))
    
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
