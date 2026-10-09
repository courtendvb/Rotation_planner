test_that("基本ポジションは反則なし", {
  expect_equal(nrow(check_overlap(default_placement(), FALSE)), 0)
})

test_that("後衛が前衛より前にいると前後の反則", {
  pl <- default_placement()
  pl$y[6] <- 1
  v <- check_overlap(pl, FALSE)
  expect_equal(nrow(v), 1)
  expect_equal(c(v$kind, v$a, v$b), c("front-back", "3", "6"))
})

test_that("隣を左右で追い越すと左右の反則", {
  pl <- default_placement()
  pl$x[4] <- 5
  v <- check_overlap(pl, FALSE)
  expect_equal(nrow(v), 1)
  expect_equal(c(v$kind, v$a, v$b), c("left-right", "4", "3"))
})

test_that("サーブ側の P1 は判定対象外", {
  pl <- default_placement()
  pl[pl$pos == 1, c("x", "y")] <- c(0, 0)
  expect_equal(nrow(check_overlap(pl, TRUE)), 0)
  expect_gt(nrow(check_overlap(pl, FALSE)), 0)
})
