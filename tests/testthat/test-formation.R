teams <- sample_teams()

test_that("既定のレセプションフォーメーションはどのローテーションでも順序を守る", {
  for (team in teams) for (k in 0:5) {
    pl <- default_reception(team, k)
    expect_true(keeps_rotation_order(pl, FALSE), info = paste(team$id, rotation_label(team, k)))
    expect_true(all(pl$x >= 0 & pl$x <= 9 & pl$y >= 0 & pl$y <= 9))
  }
})

test_that("5-1 の並びではレセプションの型(レクチャー)の位置になる", {
  us <- teams$us
  for (k in 0:5) {
    rt <- reception_template(us, k)
    expect_false(is.null(rt))
    pl <- default_reception(us, k)
    sp <- as.character(setter_position(us, k))
    for (slot in names(rt$slots)) {
      expect_equal(unlist(pl[pl$pos == rt$slots[[slot]], c("x", "y")]), RECEPTION_TEMPLATES[[sp]]$pos[[slot]],
                   ignore_attr = TRUE, info = paste(rotation_label(us, k), slot))
    }
  }
})

test_that("型の枠:リベロがいなければ後衛の MB が L の枠に入る。5-1 でなければ型を使わない", {
  t <- teams$us
  t$libero <- NA_character_
  expect_false(is.null(reception_slots(t, 0)))
  t2 <- teams$us
  t2$players$role[t2$players$role == "OP"] <- "OH"   # OH が3人
  expect_null(reception_slots(t2, 0))
  expect_identical(default_reception(t2, 0), generic_reception(t2, 0))
  t3 <- teams$us
  t3$lineup <- t3$lineup[c(1, 3, 2, 4, 6, 5)]         # S → MB → OH の並び(型の前提と違う)
  expect_false(is_standard_5_1(t3))
  expect_null(reception_template(t3, 0))
})

test_that("型が使えない並びの自動の隊形:後衛のセッターは前の選手の後ろに隠れる", {
  pl <- generic_reception(teams$us, 1)   # S6:セッター P6 は後衛で P3 の後ろ
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
    expect_gt(min(d), 1.24, label = paste(team$id, rotation_label(team, k), "の最小距離"))
  }
})

test_that("矢印:型の経路を、選手の今の立ち位置から描く(セッター・リベロはなし)", {
  us <- teams$us
  for (k in 0:5) {
    pl <- default_reception(us, k)
    ar <- transition_arrows(us, k, pl)
    roles <- vapply(ar, `[[`, "", "role")
    expect_setequal(roles, c("OP", "MB", "OH", "OH"))
    for (a in ar) {
      expect_gte(length(a$x), 2)
      # 終点は、前衛ならネット際、後衛ならアタックラインの後ろ
      expect_true(tail(a$y, 1) %in% c(ATTACK_FRONT_Y, ATTACK_BACK_Y))
    }
  }
  # 立ち位置を動かすと、矢印の始点も動く
  pl <- default_reception(us, 0)
  rt <- reception_template(us, 0)
  pl[pl$pos == rt$slots[["MB"]], c("x", "y")] <- c(3, 4)
  mb <- Filter(function(a) a$role == "MB", transition_arrows(us, 0, pl))[[1]]
  expect_equal(c(mb$x[1], mb$y[1]), c(3, 4))
})

test_that("型が使えない並びでは、役割から決めた攻撃位置へまっすぐ向かう", {
  t <- teams$us
  t$players$role[t$players$role == "OP"] <- "OH"
  ar <- transition_arrows(t, 0, default_reception(t, 0))
  expect_true(all(vapply(ar, function(a) length(a$x) == 2, logical(1))))
  expect_false(any(vapply(ar, `[[`, "", "role") %in% c("S", "L")))
})
