# datavolley::read_dv() の plays と同じ列名の data.frame を、テスト用の .dvw から作る
fake_dv_plays <- function(match_id, home_id = "H1", visiting_id = "V1", date = as.Date("2025-10-25")) {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  p <- d$plays
  out <- data.frame(
    match_id = match_id, code = p$code, set_number = p$set,
    home_setter_position = p$home_setter, visiting_setter_position = p$visiting_setter,
    home_team = d$teams[["home"]], visiting_team = d$teams[["visiting"]],
    home_team_id = home_id, visiting_team_id = visiting_id,
    player_name = NA_character_, date = date
  )
  for (side in DVW_SIDES) for (i in 1:6) out[[paste0(side, "_p", i)]] <- p[[paste0(side, "_p", i)]]
  out
}

test_that("datavolley の plays から、read_dvw() と同じラリーとチームを作る", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  m <- dv_match_from_plays(fake_dv_plays(1))
  expect_equal(m$rallies, d$rallies)
  expect_equal(m$team_ids, c(home = "H1", visiting = "V1"))

  team <- pre_match_team(list(m), "H1", "us")
  expect_equal(team$lineup, paste0("us-", c(6, 15, 4, 12, 10, 9)))
  # セッターは記録の宣言(*P04)から
  expect_equal(team$players$role[team$players$number == 4], "S")
})

test_that("試合の途中でセッターが替わっても、並びのセッターはそのセットのセッター位置の選手", {
  p <- fake_dv_plays(1)
  # 別の選手(#15)をセッターとして多く宣言しておく
  extra <- p[rep(which(p$code == "*P04>LUp"), 3), ]
  extra$code <- "*P15"
  team <- pre_match_team(list(dv_match_from_plays(rbind(p, extra))), "H1", "us")
  role <- function(n) team$players$role[team$players$number == n]
  expect_equal(role(4), "S")      # セッター位置 3 = P3 の #4
  expect_equal(role(9), "OP")     # #4 の対角(P6)
  expect_equal(rotation_label(team, 0), "S3")
})

test_that("P1〜P6 に入らずにプレーした選手はリベロにする", {
  p <- fake_dv_plays(1)
  extra <- p[p$code == "a13RM#", ]
  extra$code <- "*01DM+"
  m <- dv_match_from_plays(rbind(p, extra))
  expect_true(m$players$home$libero[m$players$home$number == 1])
  expect_false(any(m$players$home$libero[m$players$home$number != 1]))
  expect_equal(pre_match_team(list(m), "H1", "us")$libero, "us-1")
})

test_that("対戦だけを数える対面別の実績と、全試合のローテーション別の実績", {
  plays <- rbind(
    fake_dv_plays(1, date = as.Date("2025-10-25")),
    fake_dv_plays(2, date = as.Date("2025-10-26")),
    fake_dv_plays(3, visiting_id = "X9", date = as.Date("2025-11-01"))
  )
  matches <- dv_matches_from_plays(plays)
  expect_equal(unname(vapply(matches, function(m) m$match_id, 0)), c(1, 2, 3))
  idx <- pre_match_team_index(matches)
  expect_equal(idx$team_id[1], "H1")
  expect_equal(idx$matches[idx$team_id == "H1"], 3L)

  us <- pre_match_team(matches, "H1", "us")
  them <- pre_match_team(matches, "V1", "them")
  h2h <- head_to_head_stats(matches, "H1", "V1", us, them)
  expect_equal(attr(h2h, "matches"), 2L)
  expect_equal(sum(h2h$n), 6)   # 1試合 3 ラリー × 2 試合(X9 との試合は入らない)

  rec <- team_rotation_records(matches, "H1", us)
  expect_equal(sum(rec$receive + rec$serve), 9)   # 全3試合
  expect_equal(sum(rec$side_out + rec$break_point), 6)

  starts <- start_rotation_records(matches, "H1", us)
  expect_equal(sum(starts$sets), 3)
  expect_equal(starts$sets[starts$rot == 0], 3)
})

test_that("ローテーション単位の実績を外から渡すと、それを見込みに使う", {
  ms <- empty_matchup_stats()
  expect_equal(rally_prob_from_stats(ms, 10), rally_prob_from_stats(ms, 10, rotations = NULL))
  strong <- data.frame(rot = 0:5, receive = 100, side_out = 80, serve = 100, break_point = 50)
  weak <- data.frame(rot = 0:5, receive = 100, side_out = 50, serve = 100, break_point = 20)
  p <- rally_prob_from_stats(ms, 10, rotations = list(us = strong, them = weak))
  # 自チームのレセプション時:自 SO 80% と、相手 BP 20% の裏(80%)の平均
  expect_equal(unname(p[1, 1, "them"]), 0.8, tolerance = 0.01)
  # 自チームのサーブ時:自 BP 50% と、相手 SO 50% の裏(50%)の平均
  expect_equal(unname(p[1, 1, "us"]), 0.5, tolerance = 0.01)
})

test_that("試合前のモジュール:相手と自チームを選ぶとチームと実績が入る", {
  plays <- rbind(fake_dv_plays(1), fake_dv_plays(2))
  testServer(pre_match_server, args = list(plays = reactive(plays), opponent = reactive("V1"), our_team = "H1"), {
    session$flushReact()
    expect_true(valid())
    expect_equal(teams()$us$name, "山田クラブ")
    expect_equal(teams()$them$name, "Visitors")
    expect_true(has_data())
    expect_equal(attr(stats(), "matches"), 2L)
    expect_equal(sum(stats()$n), 6)
    # 相手と同じチームを自チームにすると、無効として扱う
    session$setInputs(our_team = "V1")
    expect_false(valid())
  })
})

test_that("試合前の組み込み用 UI は CSS / JavaScript の読み込みを含む", {
  for (ui in list(pre_match_board_ui("rotation"), pre_match_matchup_ui("rotation"))) {
    rp <- Filter(function(d) d$name == "rotationplanner", htmltools::findDependencies(ui))
    expect_length(rp, 1)
  }
})
