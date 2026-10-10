test_that(".dvw からチーム名・選手・並び・リベロを読み込む", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  expect_equal(unname(d$teams), c("山田クラブ", "Visitors"))
  home <- dvw_team(d, "home")
  expect_equal(home$name, "山田クラブ")
  expect_equal(home$players$name[home$players$number == 1], "田中 花子")
  expect_equal(home$lineup, paste0("us-", c(6, 15, 4, 12, 10, 9)))
  expect_equal(home$libero, "us-1")
  expect_equal(home$players$role[match(c(4, 6, 9, 10), home$players$number)], c("S", "OH", "OP", "MB"))
  expect_length(validate_lineup(home), 0)
})

test_that("役割コードがなくても、セッター宣言と並びの決まりから役割を推定する", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  v <- dvw_team(d, "visiting")
  role_of <- function(n) v$players$role[v$players$number == n]
  expect_equal(role_of(10), "S")   # *P / aP の宣言
  expect_equal(role_of(16), "OP")  # セッターの対角
  expect_equal(role_of(15), "MB")  # クイック
  expect_equal(role_of(3), "MB")   # MB の対角
  expect_equal(c(role_of(11), role_of(13)), c("OH", "OH"))
  expect_equal(role_of(6), "L")
})

test_that("Shift_JIS のファイルも読める", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw"), encoding = "CP932"))
  expect_equal(d$teams[["home"]], "山田クラブ")
})

test_that("ラリーを組み立て、対面別の実績にする", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  expect_equal(nrow(d$rallies), 3)
  expect_equal(d$rallies$winner, c("home", "visiting", "home"))
  home <- dvw_team(d, "home", team_id = "us")
  away <- dvw_team(d, "visiting", team_id = "them")
  ms <- matchup_stats(d$rallies, "home", home, away)
  expect_equal(sum(ms$n), 3)
  # ホームのサーブ2本(ホーム S3・ビジター S3):1本ブレイク
  cell <- ms[ms$server == "us" & ms$us == setter_pos_to_rot(home, 3) & ms$them == setter_pos_to_rot(away, 3), ]
  expect_equal(c(cell$n, cell$won), c(2, 1))
  # ビジターのサーブ1本(ビジター S2):ホームがサイドアウト
  cell <- ms[ms$server == "them" & ms$them == setter_pos_to_rot(away, 2), ]
  expect_equal(c(sum(cell$n), sum(cell$won)), c(1, 1))
  # 自チームをビジターにすると、取った数が入れ替わる
  ms2 <- matchup_stats(d$rallies, "visiting", away, home)
  expect_equal(sum(ms2$won), 1)
})

test_that("セッター位置とアプリのローテーションの対応(S1 = セッターが P1)", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  home <- dvw_team(d, "home")   # 並びの P3 がセッター → ローテ0 = S3
  expect_equal(rotation_label(home, 0), "S3")
  expect_equal(setter_pos_to_rot(home, 3), 0)
  for (k in 0:5) expect_equal(rotation_label(home, setter_pos_to_rot(home, setter_position(home, k))), rotation_label(home, k))
})
