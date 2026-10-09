const_p <- function(us_serve, them_serve) {
  p <- array(0, dim = c(6, 6, 2), dimnames = list(us = 0:5, them = 0:5, server = SIDES))
  p[, , 1] <- us_serve
  p[, , 2] <- them_serve
  p
}

# 検証用の素朴な再帰(メモ化のみ。小さい target で使う)
brute_force <- function(p, a, b, s, target) {
  memo <- new.env()
  rec <- function(a, b, s, us, them) {
    w <- set_winner(us, them, target)
    if (!is.na(w)) return(as.numeric(w == "us"))
    if (us + them > target * 2 + 40) return(0.5)
    key <- paste(a %% 6, b %% 6, s, us, them)
    if (!is.null(memo[[key]])) return(memo[[key]])
    pr <- p[a %% 6 + 1, b %% 6 + 1, s]
    win <- if (s == 1) rec(a, b, 1, us + 1, them) else rec(a + 1, b, 1, us + 1, them)
    lose <- if (s == 2) rec(a, b, 2, us, them + 1) else rec(a, b + 1, 2, us, them + 1)
    memo[[key]] <- pr * win + (1 - pr) * lose
  }
  rec(a, b, s, 0, 0)
}

test_that("互角なら約50%", {
  v <- set_win_probability(const_p(0.4, 0.6), 0, 0, "us")
  expect_gt(v, 0.45)
  expect_lt(v, 0.55)
})

test_that("ラリーを取りやすいほどセットを取りやすい", {
  strong <- set_win_probability(const_p(0.55, 0.55), 0, 0, "us")
  weak <- set_win_probability(const_p(0.45, 0.45), 0, 0, "us")
  expect_gt(strong, 0.7)
  expect_equal(weak, 1 - strong, tolerance = 1e-9)
  expect_equal(set_win_probability(const_p(1, 1), 0, 0, "them"), 1)
})

test_that("ローテーションごとに確率が違っても素朴な再帰と一致する(デュース含む)", {
  set.seed(42)
  p <- array(runif(72, 0.3, 0.7), dim = c(6, 6, 2))
  for (s in 1:2) {
    exact <- set_win_all_states(p, target = 5)[state_index(2, 4, s)]
    expect_equal(exact, brute_force(p, 2, 4, s, target = 5), tolerance = 1e-6)
  }
})

test_that("rank_starts は期待値の高い順", {
  m <- matrix(rep(c(0.4, 0.6, 0.5, 0.45, 0.55, 0.52), 6), 6)
  expect_equal(rank_starts(m)$us_start, c(1, 4, 5, 2, 3, 0))
  m2 <- diag(0.8, 6) + 0.1
  r <- rank_starts(m2, c(0, 0, 1, 0, 0, 0))
  expect_equal(r$us_start[1], 2)
  expect_equal(r$worst[1], 0.9)
})

test_that("能力値モデル:スタートの組み合わせで勝率が変わる", {
  t <- sample_teams()
  p <- rally_prob_array(t$us, t$them)
  expect_true(all(p > 0 & p < 1))
  m <- start_matrix(p, "us")
  expect_gt(max(m) - min(m), 0)
  ev <- evaluate_pairing(t$us, t$them, 0, 0, "them")
  expect_equal(sign(ev$p_us - 0.6), sign(ev$edge))
})
