test_that("遠近法:手前のエンドラインは幅そのまま、奥ほど狭く、奥行きは縮む", {
  pr <- court_projection(18)
  near_l <- project_point(pr, 0, 18); near_r <- project_point(pr, 9, 18)
  far_l <- project_point(pr, 0, 0); far_r <- project_point(pr, 9, 0)
  expect_equal(near_r$x - near_l$x, 9)
  expect_equal((far_r$x - far_l$x) / 9, PERSPECTIVE_RATIO)
  expect_equal(near_l$y - far_l$y, 18 * PERSPECTIVE_DEPTH)
  # 奥ほど 1m あたりの見かけの長さが短い
  expect_lt(project_point(pr, 0, 1)$y - project_point(pr, 0, 0)$y,
            project_point(pr, 0, 18)$y - project_point(pr, 0, 17)$y)
})

test_that("遠近法:画面座標から元の位置に戻せる(ドラッグで使う)", {
  for (H in c(9, 18)) {
    pr <- court_projection(H)
    pts <- expand.grid(x = c(-0.5, 2, 4.5, 9.5), y = c(-1, 0, 4, H, H + 1.5))
    p <- project_point(pr, pts$x, pts$y)
    back <- unproject_point(pr, p$x, p$y)
    expect_equal(back$x, pts$x, tolerance = 1e-9)
    expect_equal(back$y, pts$y, tolerance = 1e-9)
  }
})

test_that("コート図は台形の床と全員のマーカーを描く", {
  t <- sample_teams()
  svg <- as.character(court_svg(team_markers(t$us, 0, FALSE), opponent = team_markers(t$them, 0, TRUE)))
  expect_match(svg, '<polygon class="court-floor"')
  expect_equal(lengths(regmatches(svg, gregexpr('class="marker ', svg))), 12)
  expect_match(svg, 'data-persp=')
})
