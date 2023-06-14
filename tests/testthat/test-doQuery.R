context("doQuery")
test_that("doQuery", {
    # Test that input check caches wrong arguments.
    mg <- MgnifyClient()
    
    # Expect errors when input is wrong
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "studies",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("studies", "assembly"),
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "TreeSE",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("TreeSE", "phyloseq"),
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "taxonomy-ssu",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("taxonomy-ssu", "go-slim"),
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = NULL,
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = 0,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = 1,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = 16,
            use.cache = TRUE
        )
    )
    
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "studies",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("studies", "assembly"),
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "TreeSE",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("TreeSE", "phyloseq"),
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "taxonomy-ssu",
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("taxonomy-ssu", "go-slim"),
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = NULL,
            as.df = TRUE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = 0,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = 1,
            use.cache = FALSE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = TRUE,
            max.hits = 16,
            use.cache = FALSE
        )
    )
    
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "studies",
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("studies", "assembly"),
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "TreeSE",
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("TreeSE", "phyloseq"),
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "taxonomy-ssu",
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = c("taxonomy-ssu", "go-slim"),
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = NULL,
            as.df = FALSE,
            max.hits = FALSE,
            use.cache = TRUE
        )
    )
    
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = FALSE,
            max.hits = 0,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = FALSE,
            max.hits = 1,
            use.cache = TRUE
        )
    )
    expect_error(
        doQuery(
            mg,
            type = "studiess",
            accession = "test",
            as.df = FALSE,
            max.hits = 16,
            use.cache = TRUE
        )
    )
    
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
