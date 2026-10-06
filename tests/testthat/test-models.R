mk <- function(x, sp = "a") {
  data.frame(informant = paste0("i", seq_len(nrow(x))), sp_name = sp, x)
}

test_that("ethno_beta matches the closed-form Beta posterior", {
  d <- mk(data.frame(U1 = c(rep(1, 6), rep(0, 14))))
  r <- ethno_beta(d, level = 0.9)
  expect_equal(r$k, 6)
  expect_equal(r$n, 20)
  expect_equal(r$mean, 7 / 22)
  expect_equal(c(r$lower, r$upper), qbeta(c(0.05, 0.95), 7, 15))
})

test_that("ethno_beta gives a non-degenerate interval for 0 citations", {
  r <- ethno_beta(mk(data.frame(U1 = rep(0, 20))))
  expect_gt(r$upper, 0)
})

test_that("ethno_beta counts each informant once and rejects bad input", {
  d <- rbind(mk(data.frame(U1 = c(1, 0))), mk(data.frame(U1 = c(3, 0))))
  expect_equal(ethno_beta(d)$n, 2)
  expect_equal(ethno_beta(d)$k, 1)
  expect_error(ethno_beta(data.frame(U1 = 1)), "informant")
  d$U1[1] <- NA
  expect_error(ethno_beta(d), "NA")
})

test_that("ethno_boot warns on no variation and supports weights", {
  expect_warning(ethno_boot(rep(0, 10), mean), "identical")
  set.seed(1)
  x <- c(rep(1, 6), rep(0, 14))
  b <- ethno_boot(x, stats::weighted.mean, n1 = 2000, use_weights = TRUE)
  expect_length(b, 2000)
  expect_equal(mean(b), 0.3, tolerance = 0.03)
})

test_that("ethno_bayes_consensus counts 0 responses as evidence", {
  d <- mk(data.frame(U1 = c(1, rep(0, 19)), U2 = rep(1, 20)))
  r <- ethno_bayes_consensus(d, answers = 2, prior_for_answers = 0.9)
  expect_equal(rownames(r), c("0", "1"))
  expect_gt(r["0", "U1"], 0.99)
  expect_gt(r["1", "U2"], 0.99)
  expect_equal(colSums(r), c(U1 = 1, U2 = 1))
})

test_that("ethno_bayes_consensus validates input", {
  d <- mk(data.frame(U1 = c(0, 1, 2, 1)))
  expect_error(ethno_bayes_consensus(d, 2, 0.9), "0 to answers")
  d$U1 <- c(0, 1, 1, 0)
  expect_error(ethno_bayes_consensus(d, 2), "required")
  expect_error(ethno_bayes_consensus(d, 2, 1.5), "between 0 and 1")
  expect_error(ethno_bayes_consensus(d, 2, c(0.5, 0.5)), "length")
})

test_that("ethno_consensus recovers a simulated answer key", {
  set.seed(10)
  nq <- 40; ni <- 25
  key <- rbinom(nq, 1, 0.5)
  D <- c(rep(0.9, 15), rep(0.2, 10))
  resp <- t(sapply(D, function(d) {
    know <- rbinom(nq, 1, d)
    ifelse(know == 1, key, rbinom(nq, 1, 0.5))
  }))
  x <- as.data.frame(resp)
  names(x) <- paste0("U", seq_len(nq))
  d <- mk(x)
  r <- ethno_consensus(d)
  expect_true(r$converged)
  expect_gt(mean((r$truth$p_used > 0.5) == key), 0.9)
  expect_gt(mean(r$competence$D[1:15]), mean(r$competence$D[16:25]))
})

test_that("ethno_consensus needs enough data", {
  expect_error(ethno_consensus(mk(data.frame(U1 = c(1, 0)))), "at least")
})

test_that("ethno_saturation is non-decreasing and ends at the total", {
  set.seed(2)
  d <- mk(data.frame(U1 = rbinom(15, 1, 0.3), U2 = rbinom(15, 1, 0.3)))
  s <- ethno_saturation(d, n_perm = 50)
  expect_equal(nrow(s), 15)
  expect_false(is.unsorted(s$mean))
  expect_equal(s$lower[15], s$upper[15])
})

test_that("homegardens reproduces the published totals", {
  d <- ethnobotanyR::homegardens
  uses <- setdiff(names(d), c("informant", "sp_name"))
  expect_equal(dim(d), c(2870, 16))
  expect_equal(nlevels(d$informant), 102)
  expect_equal(nlevels(d$sp_name), 225)
  expect_equal(sum(d[uses]), 3961)
  expect_false(anyNA(d))
  expect_true(all(unlist(d[uses]) %in% 0:1))
  expect_false(any(duplicated(d[c("informant", "sp_name")])))
  # Whitney et al. 2018 Table 1 (food and medicine differ by 4 from the printed table)
  expect_equal(colSums(d["sale"]), c(sale = 604))
  expect_equal(colSums(d["technical"]), c(technical = 267))
  expect_equal(unname(URs(d)$URs[URs(d)$sp_name == "Musa (AAA-EAHB Group)"]), 169)
})

test_that("homegardens works with the models", {
  b <- ethno_beta(ethnobotanyR::homegardens[ethnobotanyR::homegardens$sp_name == "Coffea canephora", ])
  expect_equal(unique(b$n), 93)
  expect_gt(b$mean[b$use == "sale"], 0.9)
})

test_that("homegardens covariate tables join to homegardens", {
  hg <- ethnobotanyR::homegardens
  gi <- ethnobotanyR::homegardens_info
  gsp <- ethnobotanyR::homegardens_species
  expect_equal(nrow(gi), 102)
  expect_equal(nrow(gsp), 225)
  expect_setequal(as.character(gi$informant), as.character(hg$informant))
  expect_setequal(as.character(gsp$sp_name), as.character(hg$sp_name))
  expect_false(anyDuplicated(gi$informant) > 0)
  expect_false(anyDuplicated(gsp$sp_name) > 0)
  expect_equal(nrow(merge(merge(hg, gsp, by = "sp_name"), gi, by = "informant")), nrow(hg))
  expect_false(anyNA(gi))
  expect_false("lat" %in% tolower(names(gi)))
})
