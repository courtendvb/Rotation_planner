us <- sample_teams()$us

test_that("時計回りに回る(P2 の選手が P1 へ)", {
  l <- letters[1:6]
  expect_equal(rotate_lineup(l, 1), c("b", "c", "d", "e", "f", "a"))
  expect_equal(rotate_lineup(l, 6), l)
  expect_equal(rotate_lineup(l, -1), c("f", "a", "b", "c", "d", "e"))
})

test_that("ローテーション名はセッターの位置で S1 → S6 → S5 ...", {
  expect_equal(vapply(0:5, function(k) rotation_label(us, k), ""), c("S1", "S6", "S5", "S4", "S3", "S2"))
})

test_that("スイッチ後は OH=レフト、MB=センター、S/OP=ライト", {
  f <- front_row_after_switch(us, 0)  # P4=OP, P3=MB, P2=OH
  expect_equal(f$left$role, "OH")
  expect_equal(f$middle$role, "MB")
  expect_equal(f$right$role, "OP")
})

test_that("セッター前衛ならアタッカー2枚", {
  expect_equal(front_attacker_count(us, 0), 3)  # S1
  expect_equal(front_attacker_count(us, 3), 2)  # S4
})

test_that("後衛の MB はリベロと入れ替わる", {
  cp <- court_players(us, 0, serving = FALSE)  # P6 = ミドル2
  expect_equal(player_at(us, 0, 6)$role, "MB")
  expect_equal(cp$role[6], "L")
  expect_equal(cp$role[3], "MB")
})

test_that("サーブを打つ MB はリベロと入れ替わらない", {
  # k = 5 で ミドル2 が P1
  expect_equal(court_players(us, 5, serving = TRUE)$role[1], "MB")
  expect_equal(court_players(us, 5, serving = FALSE)$role[1], "L")
})

test_that("並びの検証", {
  expect_length(validate_lineup(us), 0)
  bad <- us
  bad$lineup <- c("us-1", "us-1", "us-3", "us-4", "us-5", "us-7")
  errs <- validate_lineup(bad)
  expect_true(any(grepl("重複", errs)))
  expect_true(any(grepl("リベロ", errs)))
})
