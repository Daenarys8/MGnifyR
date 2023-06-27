context("getResult")
test_that("getResult", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    expect_error(getResult(mg, accession = "test", output = 10))
    expect_error(getResult(mg, accession = "studies", output = Tree ))
    expect_error(getResult(mg, accession = 3.9))
    expect_error(getResult(mg, accession = 52))
    expect_error(getResult(mg, accession = NULL))
    expect_error(getResult(mg, accession = "taxonomy-ssu", output = NULL))
    expect_error(getResult(mg, accession = c("taxonomy-ssu", "go-slim"), get.taxa = 7))
    expect_error(getResult(mg, accession = "test", get.taxa = NULL))
    expect_error(getResult(mg, accession = "studies", get.taxa = Test))
    expect_error(getResult(mg, accession = c("studies", "assembly"), get.taxa = "TreeSE"))
    expect_error(getResult(mg, accession = "TreeSE", get.func = 2))
    expect_error(getResult(mg, accession = c("TreeSE", "phyloseq"), get.func = NULL))
    expect_error(getResult(mg, accession = "taxonomy-ssu", use.cache = 8))
    expect_error(getResult(mg, accession = c("taxonomy-ssu", "go-slim"),use.cache = NULL))
    expect_error(getResult(mg, accession = "test", get.func = "test"))
    expect_error(getResult(mg, accession = "studies", use.cache = "test"))
    expect_error(getResult(mg, accession = c("studies", "assembly"), verbose = test))
    expect_error(getResult(mg, accession = "TreeSE", verbose = 9))
    expect_error(getResult(mg, accession = c("TreeSE", "phyloseq"), verbose = NULL))
    expect_error(getResult(mg, accession = "taxonomy-ssu", verbose = "TreeSE"))
    
    # Require internet access
    skip_if(httr::http_error("https://www.ebi.ac.uk/metagenomics/api/v1"))
    
    # Test that only functional data is fetched based on certain accession ID.
    # Get data as list of data.frames
    res <- getResult(
        mg,
        "MGYA00097621",
        get.taxa = FALSE,
        output = "list",
        get.func = TRUE,
        verbose = FALSE
    )
    expect_true(is.list(res))
    expect_true("go-terms" %in% names(res))
    expect_true(
        is.character(res$`interpro-identifiers`$accession) &&
            is.character(res$`interpro-identifiers`$description) &&
            is.numeric(res$`interpro-identifiers`$MGYA00097621)
    )
    
    # Test that microbial profiling data and functional data is fetched. Get
    # data as MAE. Fetch also trees. Check that all data is is in correct place
    # and is correct.
    skip <- TRUE
    if (!skip) {
        res <- getResult(
            mg,
            "MGYA00097621",
            get.tree = TRUE,
            get.func = TRUE,
            verbose = FALSE
        )
        expect_true(class(res) == "MultiAssayExperiment")
        expect_true(class(res[[1]]) == "TreeSummarizedExperiment")
        expect_true(!is.null(rowTree(res[["microbiota"]])))
        expect_true(is.matrix(assay(res[[1]])))
        expect_true("microbiota" %in% names(res) &&
                        "go-terms" %in% names(res))
        expect_true(is.matrix(assay(res[[2]])))
        expect_true(is.matrix(assay(res[[3]])))
        expect_equal(assay(res[["go-slim"]])["GO:1990204", 1][[1]], 929)
        expect_equal(colnames(res[[1]]), colnames(res[[2]]))
        expect_equal(colnames(res[[3]]), colnames(res[[2]]))
    }
})
