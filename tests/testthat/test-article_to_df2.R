test_that("save_process_pubmed_batch works", {

  testthat::expect_no_warning(
   fetch_openalex_pubs(journal_name = "Collabra", pages = 1)
  )

})
