# 試合中の記録 ----------------------------------------------------------------
#
# 1セットの記録は「ラリーの勝者の列」だけを持ち、状態は毎回再計算する。
# そのため取り消し(Undo)は末尾を消すだけでよい。

#' 新しいセットの記録
new_set <- function(us_start, them_start, first_server, target = 25) {
  list(us_start = us_start, them_start = them_start, first_server = first_server,
       target = target, rallies = character())
}

#' セット終了の判定。終わっていれば勝者("us" / "them")、続いていれば NA
set_winner <- function(us, them, target) {
  if (us >= target && us - them >= 2) return("us")
  if (them >= target && them - us >= 2) return("them")
  NA_character_
}

#' 記録から現在の状態を再計算する。
#' log は各ラリーの対面と勝者 data.frame(us, them, server, winner)
replay_set <- function(record) {
  st <- list(us_score = 0L, them_score = 0L,
             us_rot = record$us_start %% 6, them_rot = record$them_start %% 6,
             server = record$first_server, winner = NA_character_)
  n <- length(record$rallies)
  log <- data.frame(us = integer(n), them = integer(n), server = character(n), winner = character(n))
  played <- 0
  for (w in record$rallies) {
    if (!is.na(st$winner)) break
    played <- played + 1
    log[played, ] <- list(st$us_rot, st$them_rot, st$server, w)
    if (w == "us") st$us_score <- st$us_score + 1L else st$them_score <- st$them_score + 1L
    if (w != st$server) {
      # サイドアウト:サーブ権を得たチームが回る
      if (w == "us") st$us_rot <- (st$us_rot + 1) %% 6 else st$them_rot <- (st$them_rot + 1) %% 6
      st$server <- w
    }
    st$winner <- set_winner(st$us_score, st$them_score, record$target)
  }
  st$log <- log[seq_len(played), , drop = FALSE]
  st
}

#' ラリーの結果を追加する(セット終了後は何もしない)
add_rally <- function(record, winner) {
  stopifnot(winner %in% SIDES)
  if (!is.na(replay_set(record)$winner)) return(record)
  record$rallies <- c(record$rallies, winner)
  record
}

#' 直前のラリーを取り消す
undo_rally <- function(record) {
  record$rallies <- utils::head(record$rallies, -1)
  record
}

#' 自チームのローテーション別のサイドアウト・ブレイクの集計
#'   receive / side_out    : レシーブしたラリー数 / そのうち取った数
#'   serve   / break_point : サーブしたラリー数 / そのうち取った数
rotation_stats <- function(log) {
  rot <- 0:5
  recv <- log[log$server == "them", , drop = FALSE]
  srv <- log[log$server == "us", , drop = FALSE]
  data.frame(
    rot = rot,
    receive = vapply(rot, function(r) sum(recv$us == r), numeric(1)),
    side_out = vapply(rot, function(r) sum(recv$us == r & recv$winner == "us"), numeric(1)),
    serve = vapply(rot, function(r) sum(srv$us == r), numeric(1)),
    break_point = vapply(rot, function(r) sum(srv$us == r & srv$winner == "us"), numeric(1))
  )
}
