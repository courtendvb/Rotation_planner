teams <- sample_teams()

test_that("既定のレセプションフォーメーションはどのローテーションでも順序を守る", {
  for (team in teams) for (k in 0:5) {
    pl <- default_reception(team, k)
    expect_true(keeps_rotation_order(pl, FALSE), info = paste(team$id, rotation_label(team, k)))
    expect_true(all(pl$x >= 0 & pl$x <= 9 & pl$y >= 0 & pl$y <= 9))
  }
})

test_that("OH 2人とリベロがレセプションの列に並ぶ", {
  us <- teams$us
  for (k in 0:5) {
    cp <- court_players(us, k, serving = FALSE)
    pl <- default_reception(us, k)
    passers <- cp$pos[cp$role %in% c("OH", "L")]
    others <- cp$pos[!cp$role %in% c("OH", "L", "S")]
    expect_length(passers, 3)
    # レセプション陣は横に広がり、ネット際の選手より後ろにいる
    expect_gt(diff(range(pl$x[passers])), 4)
    expect_true(all(pl$y[passers] >= 5))
    front_others <- others[is_front_row(others)]
    expect_true(all(pl$y[front_others] < 2))
  }
})

test_that("後衛のセッターは前の選手のすぐ後ろに隠れる", {
  us <- teams$us
  # S2(k = 5):セッター P2 は前衛、S6(k = 1):セッター P6 は後衛で P3 の後ろ
  pl <- default_reception(us, 1)
  expect_lt(pl$y[6], 3)
  expect_gt(pl$y[6], pl$y[3])
})

test_that("並びを入れ替えても順序を守る", {
  set.seed(3)
  us <- teams$us
  for (i in 1:50) {
    t <- us
    t$lineup <- sample(us$lineup)
    for (k in 0:5) expect_true(keeps_rotation_order(default_reception(t, k), FALSE))
  }
})

test_that("サーブ時はサーバーがエンドラインの外", {
  pl <- default_formation(teams$us, 0, serving = TRUE)
  expect_gt(pl$y[pl$pos == 1], 9)
  expect_true(keeps_rotation_order(pl, TRUE))
})

test_that("保存した立ち位置が優先される", {
  pl <- default_placement()
  saved <- setNames(list(pl), placement_key(teams$us, 2, FALSE))
  expect_identical(placement_for(teams$us, 2, FALSE, saved), pl)
  expect_identical(placement_for(teams$us, 3, FALSE, saved), default_reception(teams$us, 3))
})

test_that("標準の並びでは選手の丸が重ならない", {
  for (team in teams) for (k in 0:5) {
    pl <- default_reception(team, k)
    d <- as.matrix(dist(pl[, c("x", "y")]))
    diag(d) <- Inf
    expect_gt(min(d), 1.7, label = paste(team$id, rotation_label(team, k), "の最小距離"))
  }
})

test_that("レシーブ後の攻撃位置:前衛3人とセッター、後衛の OH/OP が動き、リベロは動かない", {
  us <- teams$us
  # S1(k = 0):前衛 P4 OP, P3 MB, P2 OH、後衛 P5 OH, P6 L, P1 S
  ap <- attack_positions(us, 0)
  expect_setequal(ap$pos, c(4, 3, 2, 5, 1))
  expect_equal(ap$x[ap$pos == 2], ATTACK_SPOTS$front_OH[1])  # 前衛 OH はレフトへ
  expect_equal(ap$x[ap$pos == 4], ATTACK_SPOTS$front_OP[1])  # OP はライトへ
  expect_equal(ap$y[ap$pos == 1], ATTACK_SPOTS$setter[2])    # セッターはネットへ
  expect_false(6 %in% ap$pos)                                # リベロ
})

test_that("矢印はレセプション位置から攻撃位置へ向かう", {
  us <- teams$us
  pl <- default_reception(us, 0)
  ar <- transition_arrows(us, 0, pl)
  s <- ar[ar$role == "S", ]
  expect_equal(nrow(s), 1)
  expect_equal(c(s$x1, s$y1), unlist(pl[pl$pos == 1, c("x", "y")]), ignore_attr = TRUE)
  expect_equal(c(s$x2, s$y2), ATTACK_SPOTS$setter)
  expect_true(all(sqrt((ar$x2 - ar$x1)^2 + (ar$y2 - ar$y1)^2) >= 0.8))
})
