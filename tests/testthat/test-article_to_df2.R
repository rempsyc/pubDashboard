test_that("fetch_openalex_pubs works", {

  testthat::expect_no_warning(
   fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
  )

})
