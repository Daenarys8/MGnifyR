context("searchAnalysis")
test_that("searchAnalysis", {
    # Test that input check caches wrong arguments.
    # List of arguments with correct values
    mg <- MgnifyClient()
    var <- list(
        x = list(mg),
        type = list("studies"),
        accession = list("test", "studies", c("studies", "assembly"),
                         "TreeSE", c("TreeSE", "phyloseq"), "taxonomy-ssu",
                         c("taxonomy-ssu", "go-slim")),
        use.cache = list(TRUE, FALSE),
        verbose = list(TRUE, FALSE)
    )
    var <- .wrong_arguments(var)
    # Loop through rows, all variable pairs should end up to error
    for( i in seq_len(nrow(var)) ){
        expect_error(
            searchAnalysis(
                x = var[i, 1][[1]],
                type = var[i, 2][[1]],
                accession = var[i, 3][[1]],
                use.cache = var[i, 4][[1]],
                verbose = var[i, 5][[1]]
            )
        )
    }
    # Require internet access
    skip_if(httr::http_error("https://www.ebi.ac.uk/metagenomics/api/v1"))

    # Test that correct analysis IDs are found based on study ID.
    expect_warning(
    res <- searchAnalysis(mg, "studies", "MGYA00097621", verbose = FALSE)
    )
    expect_true(is.null(res))
    res <- searchAnalysis(mg, "studies", "MGYS00005058", verbose = FALSE)
    expect_true(length(res) > 0)
    expect_true("MGYA00377528" %in% res)
    # Test that correct analysis IDs are found based on sample ID.
    expect_warning(
        res <- searchAnalysis(mg, "samples", "MGYA00097621", verbose = FALSE)
    )
    expect_true(is.null(res))
    res <- searchAnalysis(mg, "samples", "ERS2161777", verbose = FALSE)
    expect_true(length(res) > 0)
    expect_true("MGYA00293854" %in% res)
})
