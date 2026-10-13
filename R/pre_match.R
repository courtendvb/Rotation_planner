# 試合前の検討:複数の試合の記録から、対面別の実績とローテーション別の実績を作る ----------------
#
# 入力は datavolley::read_dv() の plays を試合ごとに縦につないだ data.frame
# (既存のスカウティングアプリの data0() / data1() のように列が足されていてもよい)。使う列は
#   match_id, code, set_number, home_setter_position, visiting_setter_position,
#   home_p1〜home_p6, visiting_p1〜visiting_p6, home_team, visiting_team,
#   home_team_id, visiting_team_id, player_name(なくてもよい), date(なくてもよい)
# 試合ごとに read_dvw() と同じ形(teams, players, plays, rallies)に直してから、既存の関数で集計する。

PRE_MATCH_COLUMNS <- c("match_id", "code", "set_number", "home_setter_position", "visiting_setter_position",
                       paste0("home_p", 1:6), paste0("visiting_p", 1:6),
                       "home_team", "visiting_team", "home_team_id", "visiting_team_id")

#' 1試合分の plays(datavolley の形)を read_dvw() と同じ形にする
dv_match_from_plays <- function(df) {
  df <- df[!is.na(df$code), , drop = FALSE]
  num <- function(x) suppressWarnings(as.integer(x))
  plays <- parse_scout_codes(as.character(df$code))
  plays$set <- num(df$set_number)
  plays$home_setter <- num(df$home_setter_position)
  plays$visiting_setter <- num(df$visiting_setter_position)
  for (side in DVW_SIDES) for (i in 1:6) {
    col <- paste0(side, "_p", i)
    plays[[col]] <- num(df[[col]])
  }
  names_col <- if ("player_name" %in% names(df)) as.character(df$player_name) else rep(NA_character_, nrow(df))

  players <- lapply(stats::setNames(DVW_SIDES, DVW_SIDES), function(side) {
    rows <- which(plays$team %in% side & !is.na(plays$number))
    on_court <- unique(unlist(plays[, paste0(side, "_p", 1:6)]))
    on_court <- on_court[!is.na(on_court)]
    # プレーの記録がない選手も、コートに入っていれば選手にする
    numbers <- sort(unique(c(plays$number[rows], on_court)))
    name <- vapply(numbers, function(n) {
      x <- names_col[rows][plays$number[rows] == n]
      x <- x[!is.na(x)]
      if (length(x)) x[1] else ""
    }, "")
    # リベロは P1〜P6 に入らない(記録上は後衛の選手と入れ替わらずに出てくる)
    data.frame(side = rep(side, length(numbers)), number = numbers, last = name,
               first = rep("", length(numbers)), libero = !numbers %in% on_court,
               role = rep(NA_character_, length(numbers)))
  })

  dates <- if ("date" %in% names(df)) df$date[!is.na(df$date)] else NULL
  list(
    match_id = df$match_id[1],
    date = if (length(dates)) max(dates) else NA,
    teams = stats::setNames(as.character(c(df$home_team[1], df$visiting_team[1])), DVW_SIDES),
    team_ids = stats::setNames(as.character(c(df$home_team_id[1], df$visiting_team_id[1])), DVW_SIDES),
    players = players,
    plays = plays,
    rallies = dvw_rallies(plays)
  )
}

#' 複数の試合の plays を、試合ごとの list にする(日付、match_id の順)
dv_matches_from_plays <- function(plays) {
  if (is.null(plays) || nrow(plays) == 0) return(list())
  missing <- setdiff(PRE_MATCH_COLUMNS, names(plays))
  if (length(missing)) stop("plays に必要な列がありません: ", paste(missing, collapse = ", "))
  matches <- lapply(split(plays, plays$match_id), dv_match_from_plays)
  key <- order(vapply(matches, function(m) as.numeric(m$date), 0), vapply(matches, function(m) as.numeric(m$match_id), 0))
  matches[key]
}

#' その試合でのチームの側("home" / "visiting")。出ていなければ NA
match_side <- function(match, team_id) {
  s <- DVW_SIDES[match$team_ids == team_id]
  if (length(s)) s[1] else NA_character_
}

matches_of <- function(matches, team_id) Filter(function(m) !is.na(match_side(m, team_id)), matches)

#' 試合に出ているチームの一覧(試合数の多い順)
pre_match_team_index <- function(matches) {
  if (length(matches) == 0) return(data.frame(team_id = character(), team = character(), matches = integer()))
  rows <- do.call(rbind, lapply(matches, function(m) data.frame(team_id = unname(m$team_ids), team = unname(m$teams))))
  n <- table(rows$team_id)
  out <- unique(rows[!duplicated(rows$team_id), ])
  out$matches <- as.integer(n[out$team_id])
  out[order(-out$matches, out$team_id), , drop = FALSE] |> `rownames<-`(NULL)
}

#' アプリのチームを作る。並びは、そのチームの一番新しい試合の最初のセットのスタート
pre_match_team <- function(matches, team_id, key = "us") {
  ms <- matches_of(matches, team_id)
  if (length(ms) == 0) return(NULL)
  m <- ms[[length(ms)]]
  side <- match_side(m, team_id)
  first_set <- if (nrow(m$rallies) > 0) min(m$rallies$set, na.rm = TRUE) else 1
  set_setter_from_position(dvw_team(m, side, team_id = key, set = first_set), m, side, first_set)
}

#' 並びのセッターを、そのセットの記録のセッター位置にいる選手にする。
#' 試合の中でセッターが替わったチームは、宣言の多い方のセッターが選ばれて並びにセッターがいなくなり、
#' ローテーションの対応がずれるため(OP / MB / OH は、そのセッターを基準に 5-1 の決まりで補正する)
set_setter_from_position <- function(team, match, side, set) {
  p <- match$plays
  rows <- !is.na(p$set) & p$set == set & !is.na(p[[paste0(side, "_setter")]])
  if (!any(rows)) return(team)
  pos <- p[[paste0(side, "_setter")]][rows][1]
  setter <- team$lineup[pos]
  if (is.na(setter) || !nzchar(setter)) return(team)
  roles <- team$players$role
  i <- match(setter, team$players$id)
  if (roles[i] == "S") return(team)
  # 並びの中の他のセッターは、5-1 の決まりで役割を付け直す
  in_lineup <- team$players$id %in% team$lineup
  roles[in_lineup & roles == "S"] <- NA
  roles[i] <- "S"
  pl <- match$players[[side]]
  cnt <- dvw_player_counts(pl, match$plays, side)
  evidence <- (cnt$middle_att - cnt$recv)[match(team$players$number, pl$number)]
  team$players$role <- roles
  fixed <- apply_lineup_structure(team$players, team$lineup, inferred = in_lineup & roles %in% c(NA, "OH", "OP", "MB"),
                                  mb_evidence = ifelse(is.na(evidence), 0, evidence))
  team$players$role <- ifelse(is.na(fixed), "OH", fixed)
  team
}

#' ローテーション別の実績(全試合、相手を問わない)。rotation_summary() と同じ形で、
#' そのチームが取った数を数える(receive / side_out、serve / break_point)
team_rotation_records <- function(matches, team_id, team) {
  out <- data.frame(rot = 0:5, receive = 0, side_out = 0, serve = 0, break_point = 0)
  for (m in matches_of(matches, team_id)) {
    side <- match_side(m, team_id)
    r <- m$rallies
    setter <- r[[paste0(side, "_setter")]]
    r <- r[!is.na(setter), , drop = FALSE]
    if (nrow(r) == 0) next
    rot <- factor(setter_pos_to_rot(team, setter[!is.na(setter)]), levels = 0:5)
    receiving <- r$server != side
    won <- r$winner == side
    count <- function(sel) as.numeric(table(rot[sel]))
    out$receive <- out$receive + count(receiving)
    out$side_out <- out$side_out + count(receiving & won)
    out$serve <- out$serve + count(!receiving)
    out$break_point <- out$break_point + count(!receiving & won)
  }
  out
}

#' 2チームの対戦だけから作る対面別の実績(matchup_stats() の形)。attr "matches" に対戦数
head_to_head_stats <- function(matches, us_id, them_id, us_team, them_team) {
  out <- empty_matchup_stats()
  n <- 0L
  for (m in matches_of(matches, us_id)) {
    our_side <- match_side(m, us_id)
    if (is.na(match_side(m, them_id)) || match_side(m, them_id) == our_side) next
    s <- matchup_stats(m$rallies, our_side, us_team, them_team)
    out$n <- out$n + s$n
    out$won <- out$won + s$won
    n <- n + 1L
  }
  attr(out, "matches") <- n
  out
}

#' スタートローテーション別のセット数と、取ったセット数
start_rotation_records <- function(matches, team_id, team) {
  out <- data.frame(rot = 0:5, sets = 0, won = 0)
  for (m in matches_of(matches, team_id)) {
    side <- match_side(m, team_id)
    r <- m$rallies
    for (s in sort(unique(r$set[!is.na(r$set)]))) {
      rs <- r[!is.na(r$set) & r$set == s, , drop = FALSE]
      setter <- rs[[paste0(side, "_setter")]][1]
      if (is.na(setter)) next
      k <- setter_pos_to_rot(team, setter) + 1
      out$sets[k] <- out$sets[k] + 1
      out$won[k] <- out$won[k] + (rs$winner[nrow(rs)] == side)
    }
  }
  out
}
