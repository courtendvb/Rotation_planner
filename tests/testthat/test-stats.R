# ラリーの勝者の列から、実際の対面とスコアを再現する(検証用)
play_set <- function(us_start, them_start, first, winners) {
  a <- us_start; b <- them_start; srv <- first
  log <- data.frame(us = integer(), them = integer(), server = character())
  for (w in winners) {
    log[nrow(log) + 1, ] <- list(a %% 6, b %% 6, srv)
    if (w != srv) {
      if (w == "us") a <- a + 1 else b <- b + 1
      srv <- w
    }
  }
  log
}

test_that("実際に起きる対面は pairings_for_set の12通りと一致する", {
  set.seed(1)
  for (first in SIDES) {
    log <- play_set(2, 5, first, sample(SIDES, 600, replace = TRUE))
    seen <- unique(paste(log$us, log$them, log$server))
    expected <- with(pairings_for_set(2, 5, first), paste(us, them, server))
    expect_setequal(seen, expected)
    expect_length(expected, 12)
  }
})

make_stats <- function(...) {
  ms <- empty_matchup_stats()
  for (x in list(...)) {
    i <- which(ms$us == x$us & ms$them == x$them & ms$server == x$server)
    ms$n[i] <- x$n
    ms$won[i] <- x$won
  }
  ms
}

test_that("実績がないときは一般的な値(レシーブ側のサイドアウト率)になる", {
  p <- rally_prob_from_stats()
  expect_true(all(abs(p[, , "them"] - DEFAULT_SIDE_OUT) < 1e-12))
  expect_true(all(abs(p[, , "us"] - (1 - DEFAULT_SIDE_OUT)) < 1e-12))
})

test_that("対面の実績が多いほど、その対面の率に近づく", {
  few <- rally_prob_from_stats(make_stats(list(us = 0, them = 0, server = "them", n = 4, won = 4)))
  many <- rally_prob_from_stats(make_stats(list(us = 0, them = 0, server = "them", n = 200, won = 200)))
  expect_gt(few[1, 1, "them"], DEFAULT_SIDE_OUT)
  expect_gt(many[1, 1, "them"], few[1, 1, "them"])
  expect_gt(many[1, 1, "them"], 0.95)
})

test_that("実績のない対面は、ローテーション単位の実績に寄る", {
  # 自チームのローテ0のレシーブは、相手のどのローテに対しても強い
  ms <- make_stats(list(us = 0, them = 1, server = "them", n = 50, won = 45),
                   list(us = 0, them = 2, server = "them", n = 50, won = 45))
  p <- rally_prob_from_stats(ms)
  expect_gt(p[1, 4, "them"], p[2, 4, "them"])   # 自ローテ0 vs 相手ローテ3(実績なし)
  expect_equal(p[2, 4, "us"], p[3, 5, "us"])    # サーブ時には影響しない(どちらも全体値)
})

test_that("ローテーション単位の集計", {
  ms <- make_stats(list(us = 0, them = 1, server = "them", n = 10, won = 6),
                   list(us = 0, them = 2, server = "us", n = 8, won = 3))
  u <- rotation_summary(ms, "us")
  expect_equal(unlist(u[1, -1]), c(receive = 10, side_out = 6, serve = 8, break_point = 3))
  t <- rotation_summary(ms, "them")
  expect_equal(unlist(t[t$rot == 1, -1]), c(receive = 0, side_out = 0, serve = 10, break_point = 4))
  expect_equal(unlist(t[t$rot == 2, -1]), c(receive = 8, side_out = 5, serve = 0, break_point = 0))
})
