library(testthat)

library(networkanalyticspackage)
test_that("sna plot", {
   p1 <- sna_plot(week02_network, week02_matrix)
   expect_that(p1, is_a("matrix"))
})

test_that("node measure", {
  node1 <- node_measure(week02_network)
  expect_that(node1, is_a("list"))
})

test_that("network measure", {
  net1 <- network_measure(week02_network)
  expect_that(net1, is_a("list"))
})
