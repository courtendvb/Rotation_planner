play <- function(record, winners) Reduce(add_rally, winners, record)

test_that("サイドアウトしたチームだけが回る", {
  s <- replay_set(play(new_set(0, 0, "us"), c("us", "them", "us")))
  expect_equal(c(s$us_score, s$them_score, s$us_rot, s$them_rot), c(2, 1, 1, 1))
  expect_equal(s$server, "us")
})

test_that("25点・2点差で終了し、それ以降は記録しない", {
  r <- play(new_set(0, 0, "us"), rep("us", 30))
  expect_length(r$rallies, 25)
  expect_equal(replay_set(r)$winner, "us")
})

test_that("デュースは2点差まで続く", {
  w <- c(rep("us", 24), rep("them", 24), "us", "them", "us")
  r <- play(new_set(0, 0, "us"), w)
  expect_true(is.na(replay_set(r)$winner))
  expect_equal(replay_set(add_rally(r, "us"))$winner, "us")
})

test_that("Undo で直前のラリーを取り消す", {
  r <- play(new_set(0, 0, "them"), c("us", "us"))
  s <- replay_set(undo_rally(r))
  expect_equal(c(s$us_score, s$us_rot), c(1, 1))
  expect_equal(s$server, "us")
})

test_that("実際の試合で起きる対面は pairings_for_set の12通りと一致する", {
  set.seed(1)
  for (first in c("us", "them")) {
    r <- new_set(2, 5, first, target = 1000)
    r$rallies <- sample(c("us", "them"), 600, replace = TRUE)
    log <- replay_set(r)$log
    seen <- unique(paste(log$us, log$them, log$server))
    expected <- with(pairings_for_set(2, 5, first), paste(us, them, server))
    expect_setequal(seen, expected)
    expect_length(expected, 12)
  }
})

test_that("サイドアウトとブレイクを分けて集計する", {
  s <- replay_set(play(new_set(0, 0, "them"), c("us", "us", "them")))
  st <- rotation_stats(s$log)
  expect_equal(unlist(st[1, -1]), c(receive = 1, side_out = 1, serve = 0, break_point = 0))
  expect_equal(unlist(st[2, -1]), c(receive = 0, side_out = 0, serve = 2, break_point = 1))
})

test_that("相手側の集計もできる", {
  s <- replay_set(play(new_set(0, 0, "them"), c("us", "us", "them")))
  st <- rotation_stats(s$log, "them")
  # 相手はローテ0でサーブ2本(1本ブレイク... 1本目は失点、3本目は...)
  expect_equal(sum(st$serve) + sum(st$receive), 3)
  expect_equal(st$serve[1], 1)       # 1本目:相手サーブ、自チームがサイドアウト
  expect_equal(st$receive[1], 2)     # 2・3本目:自チームサーブを相手がレシーブ
  expect_equal(st$side_out[1], 1)
})
