context("doQuery")
test_that("doQuery", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    expect_error(doQuery(mg,
                         type = "studiess",
                         accession = "test"))
    
    expect_error(doQuery(
        mg,
        type = "studies",
        accession = c("studies", "assembly"),
        as.df = NULL
    ))
    expect_error(doQuery(
        mg,
        type = c("studies", "assembly"),
        accession = "studies"
    ))
    expect_error(doQuery(mg,
                         type = "studies",
                         accession = 0, ))
    
    expect_error(doQuery(mg,
                         type = 0,
                         accession = "test",))
    
    expect_error(doQuery(
        mg,
        type = "studies",
        accession = c(TreeSE, "phyloseq"),
    ))
    expect_error(
        doQuery(
            mg,
            type = "studies",
            accession = "TreeSE",
            as.df = FALSE,
            max.hits = 10.5,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studies",
            accession = c("TreeSE", "phyloseq"),
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = 10
        )
    )
    expect_error(doQuery(mg,
                         type = "studies",
                         accession = T))
    expect_error(doQuery(mg,
                         type = "studies",
                         accession = 8.5))
    expect_error(doQuery(mg,
                         type = TRUE,
                         accession = "test",))
    expect_error(doQuery(mg,
                         type = 4.9,
                         accession = "test",))
    expect_error(doQuery(mg,
                         type = "studies",
                         accession = 3,))
    
    # Require internet access
    skip_if(httr::http_error("https://www.ebi.ac.uk/metagenomics/api/v1"))
    # Test that studies are searched based on certain accession ID, get result
    # as list, choose max hits
    query <-
        doQuery(mg,
                "studies",
                "MGYS00005292",
                max.hits = 1,
                as.df = FALSE)
    expect_true(is.list(query))
    expect_true(names(query) %in% "MGYS00005292")
    expect_true(query$MGYS00005292$type == "studies")
    # Test that runs are searched, get result as df, choose max hits
    query2 <- doQuery(mg, "studies", "MGYS00005292", max.hits = 1)
    expect_true(is.data.frame(query2))
    expect_equal(query2$bioproject,
                 query$MGYS00005292$attributes$bioproject)
})
