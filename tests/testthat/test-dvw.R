# 小さな .dvw を作って読み込む(実際の DataVolley の書式に合わせたもの)
make_dvw <- function(path, encoding = "UTF-8") {
  scout_line <- function(code, set, hs, vs, hp, vp) {
    paste0(code, ";;;;;;;", "10.00.00;", set, ";", hs, ";", vs, ";;;;",
           paste(hp, collapse = ";"), ";", paste(vp, collapse = ";"), ";")
  }
  hp <- c(6, 15, 4, 12, 10, 9); vp <- c(11, 15, 10, 13, 3, 16)
  lines <- c(
    "[3DATAVOLLEYSCOUT]", "FILEFORMAT: 2.0",
    "[3TEAMS]", "H1;山田クラブ;0;;;", "V1;Visitors;0;;;",
    "[3PLAYERS-H]",
    "0;1;1;*;;;;;H1;田中;花子;;L;1;False;;;",
    "0;4;2;3;;;;;H4;佐藤;一;;;5;False;;;",
    "0;6;3;1;;;;;H6;鈴木;二;;;2;False;;;",
    "0;9;4;6;;;;;H9;高橋;三;;;3;False;;;",
    "0;10;5;5;;;;;H10;伊藤;四;;;4;False;;;",
    "0;12;6;4;;;;;H12;渡辺;五;;;2;False;;;",
    "0;15;7;2;;;;;H15;中村;六;;;4;False;;;",
    "[3PLAYERS-V]",
    "1;3;1;5;;;;;V3;A;B;;;;False;;;",
    "1;6;2;*;;;;;V6;C;D;;L;;False;;;",
    "1;10;3;3;;;;;V10;E;F;;;;False;;;",
    "1;11;4;1;;;;;V11;G;H;;;;False;;;",
    "1;13;5;4;;;;;V13;I;J;;;;False;;;",
    "1;15;6;2;;;;;V15;K;L;;;;False;;;",
    "1;16;7;6;;;;;V16;M;N;;;;False;;;",
    "[3SCOUT]",
    scout_line("*P04>LUp", 1, 3, 3, hp, vp),
    scout_line("aP10>LUp", 1, 3, 3, hp, vp),
    scout_line("*06SM+", 1, 3, 3, hp, vp),         # ホームのサーブ → ホームの得点(ブレイク)
    scout_line("a11RM-", 1, 3, 3, hp, vp),
    scout_line("*p01:00", 1, 3, 3, hp, vp),
    scout_line("*06SM-", 1, 3, 3, hp, vp),         # ホームのサーブ → ビジターの得点(サイドアウト)
    scout_line("a13RM#", 1, 3, 3, hp, vp),
    scout_line("a10EQ#", 1, 3, 3, hp, vp),
    scout_line("a15AQ#X1~36", 1, 3, 3, hp, vp),
    scout_line("ap01:01", 1, 3, 2, hp, vp),
    scout_line("a15SM=", 1, 3, 2, hp, vp),         # ビジターのサーブミス → ホームのサイドアウト
    scout_line("*p02:01", 1, 3, 2, hp, vp)
  )
  con <- file(path, open = "w", encoding = encoding)
  writeLines(lines, con)
  close(con)
  path
}

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

test_that("ラリーとセッター位置別のサイドアウト・ブレイク", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  expect_equal(nrow(d$rallies), 3)
  expect_equal(d$rallies$winner, c("home", "visiting", "home"))
  h <- dvw_setter_stats(d, "home")
  expect_equal(h$serve[h$setter_pos == 3], 2)
  expect_equal(h$break_point[h$setter_pos == 3], 1)
  expect_equal(h$receive[h$setter_pos == 3], 1)
  expect_equal(h$side_out[h$setter_pos == 3], 1)
  v <- dvw_setter_stats(d, "visiting")
  expect_equal(v$side_out[v$setter_pos == 3], 1)
  expect_equal(v$serve[v$setter_pos == 2], 1)
})

test_that("セッター位置別の実績をアプリのローテーションに対応させる", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  home <- dvw_team(d, "home")   # 並びの P3 がセッター → ローテ0 = S3
  st <- setter_stats_to_rotation(dvw_setter_stats(d, "home"), home)
  expect_equal(rotation_label(home, 0), "S3")
  expect_equal(st$serve[st$rot == 0], 2)
  expect_equal(sum(st$serve) + sum(st$receive), 3)
})

test_that("既存の能力値は背番号で引き継ぐ", {
  d <- read_dvw(make_dvw(tempfile(fileext = ".dvw")))
  old <- sample_teams()$us
  old$players$number[old$players$role == "S"] <- 4L
  old$players$attack[old$players$role == "S"] <- 9
  home <- dvw_team(d, "home", existing = old)
  expect_equal(home$players$attack[home$players$number == 4], 9)
  expect_equal(home$players$attack[home$players$number == 15], 5)
})
