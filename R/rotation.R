# ローテーションの基本ロジック ------------------------------------------------
#
# チームは list で表す:
#   id, name   : 文字列
#   players    : data.frame(id, number, name, role, attack, block, serve, receive)
#                role は "S" "OH" "MB" "OP" "L"、能力値は 1〜10
#   lineup     : 長さ6の選手 id。1番目 = P1, 2番目 = P2, ... 6番目 = P6
#   libero     : リベロの id(いなければ NA)
#
# ローテーション k は、登録した並びから何回回ったか(0〜5)。
# 表示上はセッターの位置で S1〜S6 と呼ぶ(rotation_label)。

ROLES <- c("S", "OH", "MB", "OP", "L")
FRONT_ROW <- c(4L, 3L, 2L)
BACK_ROW <- c(5L, 6L, 1L)

#' 前衛かどうか
is_front_row <- function(pos) pos %in% FRONT_ROW

#' k 回ローテーションした後の並び。時計回りなので、回った後の P1 には元の P2 が入る
rotate_lineup <- function(lineup, k) {
  lineup[(0:5 + k) %% 6 + 1]
}

empty_player <- function(team) team$players[0, , drop = FALSE]

#' 選手 id から1行の data.frame を返す(見つからなければ 0 行)
find_player <- function(team, id) {
  if (length(id) != 1 || is.na(id) || !nzchar(id)) return(empty_player(team))
  team$players[team$players$id == id, , drop = FALSE][1, , drop = FALSE] |> drop_na_rows()
}

drop_na_rows <- function(df) df[!is.na(df$id), , drop = FALSE]

#' ローテーション k でポジション pos(1〜6)にいる選手
player_at <- function(team, k, pos) {
  find_player(team, rotate_lineup(team$lineup, k)[pos])
}

#' 選手のポジション(いなければ NA)
position_of <- function(team, k, id) {
  idx <- match(id, rotate_lineup(team$lineup, k))
  as.integer(idx)
}

#' 並びの中で最初のセッターの位置
setter_position <- function(team, k) {
  roles <- team$players$role[match(team$lineup, team$players$id)]
  setter <- team$lineup[which(roles == "S")[1]]
  if (is.na(setter)) return(NA_integer_)
  position_of(team, k, setter)
}

#' 表示用のローテーション名(S1〜S6)
rotation_label <- function(team, k) {
  pos <- setter_position(team, k)
  if (is.na(pos)) paste0("R", k %% 6 + 1) else paste0("S", pos)
}

#' コートに立っている6人(リベロの入れ替わりを反映)。
#' リベロは後衛の MB と入れ替わる。ただしサーブを打つ P1 の MB はそのまま。
#' 戻り値は pos 列を持つ6行の data.frame(選手がいない位置は NA)
court_players <- function(team, k, serving) {
  ids <- rotate_lineup(team$lineup, k)
  libero_ok <- !is.null(team$libero) && !is.na(team$libero) && team$libero %in% team$players$id
  roles <- team$players$role[match(ids, team$players$id)]
  if (libero_ok) {
    replace <- !is.na(roles) & roles == "MB" & !is_front_row(1:6) & !(serving & 1:6 == 1)
    ids[replace] <- team$libero
  }
  out <- team$players[match(ids, team$players$id), , drop = FALSE]
  out$pos <- 1:6
  rownames(out) <- NULL
  out
}

#' サーブを打つ選手(P1)
server_of <- function(team, k) player_at(team, k, 1)

#' サーブ後にスイッチした前衛3人。OH はレフト、MB はセンター、S/OP はライトへ。
#' 役割が重なる場合は元のポジション(P4=レフト, P3=センター, P2=ライト)に残る。
#' 戻り値は list(left, middle, right)、各要素は1行の data.frame(空なら 0 行)
front_row_after_switch <- function(team, k) {
  slots <- list(left = NULL, middle = NULL, right = NULL)
  positional <- c(`4` = "left", `3` = "middle", `2` = "right")
  preferred <- c(OH = "left", MB = "middle", S = "right", OP = "right")
  pending <- list()

  for (pos in FRONT_ROW) {
    p <- player_at(team, k, pos)
    if (nrow(p) == 0) next
    slot <- unname(preferred[p$role])
    if (!is.na(slot) && is.null(slots[[slot]])) {
      slots[[slot]] <- p
    } else {
      pending[[length(pending) + 1]] <- list(player = p, pos = pos)
    }
  }
  for (item in pending) {
    own <- unname(positional[as.character(item$pos)])
    free <- names(slots)[vapply(slots, is.null, logical(1))]
    slot <- if (own %in% free) own else free[1]
    if (!is.na(slot)) slots[[slot]] <- item$player
  }
  lapply(slots, function(s) if (is.null(s)) empty_player(team) else s)
}

#' 前衛のアタッカー数(セッターが前衛なら2枚)
front_attacker_count <- function(team, k) {
  sum(vapply(FRONT_ROW, function(pos) {
    p <- player_at(team, k, pos)
    nrow(p) == 1 && p$role != "S"
  }, logical(1)))
}

#' 並びの妥当性チェック。問題がなければ長さ0の文字列ベクトル
validate_lineup <- function(team) {
  errors <- character()
  ids <- team$lineup
  if (length(ids) != 6) return("スタートの並びは6人必要です")
  if (anyDuplicated(ids[!is.na(ids) & nzchar(ids)]))
    errors <- c(errors, "スタートの6人に同じ選手が重複しています")
  for (i in 1:6) {
    p <- find_player(team, ids[i])
    if (nrow(p) == 0) {
      errors <- c(errors, sprintf("P%d の選手が未設定です", i))
    } else if (p$role == "L") {
      errors <- c(errors, sprintf("リベロ(#%d)はスタートの6人に入れられません", p$number))
    }
  }
  if (!is.null(team$libero) && !is.na(team$libero) && team$libero %in% ids)
    errors <- c(errors, "リベロがスタートの6人に含まれています")
  unique(errors)
}
