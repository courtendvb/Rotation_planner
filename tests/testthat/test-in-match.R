test_that("試合中のモジュール:.dvw からチーム・スタート・実績を作る", {
  path <- make_dvw(tempfile(fileext = ".dvw"))
  testServer(in_match_server, args = list(dvw_path = path, poll_ms = 100), {
    session$setInputs(our_side = "home", set = "1")
    session$flushReact()
    expect_equal(teams()$us$name, "山田クラブ")
    expect_equal(teams()$them$name, "Visitors")
    expect_equal(teams()$us$lineup, paste0("us-", c(6, 15, 4, 12, 10, 9)))
    # 第1セットの最初のサーブはホーム(= 自チーム)
    expect_equal(start()$first, "us")
    expect_equal(sum(stats()$n), 3)
    expect_true(has_data())
    # 自チームをビジターにすると入れ替わる
    session$setInputs(our_side = "visiting")
    session$flushReact()
    expect_equal(teams()$us$name, "Visitors")
    expect_equal(start()$first, "them")
  })
})

test_that("試合中のモジュール:各ローテの率をボードと表に出し、見込みは出さない", {
  path <- make_dvw(tempfile(fileext = ".dvw"))
  testServer(in_match_server, args = list(dvw_path = path, poll_ms = 100), {
    session$setInputs(our_side = "home", set = "1",
                      `board-us_start` = "0", `board-them_start` = "0", `board-first_server` = "us")
    session$flushReact()
    # ホーム:サーブ 2 本でブレイク 1、レセプション 1 本でサイドアウト 1
    us <- rates()$us
    expect_equal(c(sum(us$serve), sum(us$break_point), sum(us$receive), sum(us$side_out)), c(2, 1, 1, 1))
    them <- rates()$them
    expect_equal(c(sum(them$serve), sum(them$break_point), sum(them$receive), sum(them$side_out)), c(1, 0, 2, 1))

    board <- as.character(output$`board-board`$html)
    expect_match(board, "自BP 50% \\(1/2\\)")
    expect_match(board, "相SO")
    expect_false(grepl("この対面の実績", board))
    expect_false(grepl("セット取得率", as.character(output$`board-summary`$html)))
    expect_match(as.character(output$rotations_us$html), "サイドアウト")
  })
})

test_that("試合中のモジュール:ファイルがなくても落ちない", {
  testServer(in_match_server, args = list(dvw_path = tempfile(fileext = ".dvw"), poll_ms = 100), {
    session$flushReact()
    expect_false(has_data())
  })
})

test_that("組み込み用の UI は CSS / JavaScript の読み込みを含む", {
  for (ui in list(in_match_board_ui("rotation"), in_match_matchup_ui("rotation"))) {
    deps <- htmltools::findDependencies(ui)
    rp <- Filter(function(d) d$name == "rotationplanner", deps)
    expect_length(rp, 1)
    expect_equal(rp[[1]]$script, "rotationplanner.js")
    expect_equal(rp[[1]]$stylesheet, "rotationplanner.css")
    expect_true(file.exists(file.path(rp[[1]]$src$file, "rotationplanner.js")))
  }
})
