context("testing crmatch")

test_that("oadoi_fetch returns", {
  a <- crmatch::cr_match(refs = c("Kleinbölting N, Huep G, Weisshaar B. Enhancing the GABI-Kat Arabidopsis thaliana T-DNA insertion mutant database by incorporating Araport11 annotation. Plant and Cell Physiology. 2017;58(1): e7.", "Tamir, Jonathan I. et al. T-2 Shuffling: Sharp, Multicontrast, Volumetric Fast Spin-Echo Imaging. Magnetic Resonance in Medicine 77.1 (2017): 180–195."))
  # correct classes
  expect_is(a, "tbl_df")
  expect_equal(nrow(a), 2)
  expect_equal(ncol(a), 5)
})
