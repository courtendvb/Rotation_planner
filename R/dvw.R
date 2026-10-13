# DataVolley(.dvw)の読み込み ------------------------------------------------------
#
# 外部パッケージに頼らない軽量な読み込み。使うのは次の部分だけ。
#   [3TEAMS]     チーム名(1行目 = ホーム、2行目 = ビジター)
#   [3PLAYERS-H] [3PLAYERS-V]  選手(背番号、姓、名、リベロ印、役割コード)
#   [3SCOUT]     プレーの記録。各行に、その時点のセット番号・両チームのセッター位置・P1〜P6 が入る
#
# スカウトの行の先頭は "*" = ホーム、"a" = ビジター。
#   *p12:10  ホームの得点(スコア 12:10)
#   *06SM#   ホームの #6 のサーブ(スキル S、タイプ M、評価 #)
#   a13AH+V5~46  ビジターの #13 のアタック(位置 7-8 = コンビネーション、10 = 開始ゾーン)

DVW_SIDES <- c("home", "visiting")
# 役割コード → アプリの役割
DVW_ROLE <- c(`1` = "L", `2` = "OH", `3` = "OP", `4` = "MB", `5` = "S")

#' 文字コードを判定して読み込む(UTF-8 → Shift_JIS(CP932) → Windows-1252 の順に試す)
read_dvw_lines <- function(path) {
  raw <- readLines(path, warn = FALSE)
  if (all(validUTF8(raw))) {
    Encoding(raw) <- "UTF-8"
    return(raw)
  }
  for (enc in c("CP932", "CP1252")) {
    conv <- iconv(raw, from = enc, to = "UTF-8")
    if (!anyNA(conv)) return(conv)
  }
  iconv(raw, from = "latin1", to = "UTF-8")
}

dvw_section <- function(lines, name) {
  starts <- grep("^\\[3", lines)
  i <- match(paste0("[3", name, "]"), trimws(lines[starts]))
  if (is.na(i)) return(character())
  from <- starts[i] + 1
  to <- if (i < length(starts)) starts[i + 1] - 1 else length(lines)
  if (from > to) character() else lines[from:to]
}

split_fields <- function(lines, n) {
  parts <- strsplit(lines, ";", fixed = TRUE)
  t(vapply(parts, function(p) { length(p) <- n; p[is.na(p)] <- ""; p }, character(n)))
}

dvw_players <- function(lines, side) {
  if (length(lines) == 0) {
    return(data.frame(side = character(), number = integer(), last = character(), first = character(),
                      libero = logical(), role = character()))
  }
  f <- split_fields(lines, 15)
  data.frame(
    side = side,
    number = suppressWarnings(as.integer(f[, 2])),
    last = trimws(f[, 10]),
    first = trimws(f[, 11]),
    libero = grepl("L", f[, 13], fixed = TRUE),
    role = unname(DVW_ROLE[trimws(f[, 14])])
  )[!is.na(suppressWarnings(as.integer(f[, 2]))), ]
}

#' スカウトのコードを分解する(チーム、得点の行か、背番号、スキル、タイプ、コンビ、開始ゾーン)
parse_scout_codes <- function(code) {
  num <- function(x) suppressWarnings(as.integer(x))
  team <- ifelse(substr(code, 1, 1) == "*", "home", ifelse(substr(code, 1, 1) == "a", "visiting", NA))
  is_skill <- grepl("^[*a][0-9]{2}[SREABDF]", code)
  data.frame(
    code = code,
    team = team,
    point = grepl("^[*a]p", code),
    number = ifelse(is_skill, num(substr(code, 2, 3)), NA_integer_),
    skill = ifelse(is_skill, substr(code, 4, 4), NA_character_),
    type = ifelse(is_skill, substr(code, 5, 5), NA_character_),
    combo = ifelse(is_skill, substr(code, 7, 8), NA_character_),
    start_zone = ifelse(is_skill, num(substr(code, 10, 10)), NA_integer_)
  )
}

#' スカウトの行を表にする
dvw_plays <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) return(NULL)
  f <- split_fields(lines, 26)
  num <- function(x) suppressWarnings(as.integer(x))
  out <- parse_scout_codes(f[, 1])
  out$set <- num(f[, 9])
  out$home_setter <- num(f[, 10])
  out$visiting_setter <- num(f[, 11])
  for (i in 1:6) {
    out[[paste0("home_p", i)]] <- num(f[, 14 + i])
    out[[paste0("visiting_p", i)]] <- num(f[, 20 + i])
  }
  out
}

#' ラリーごとの結果(セット、サーブ側、得点側、サーブ時の両チームのセッター位置)。
#' サーブの記録がない得点(反則による得点など)は除く
dvw_rallies <- function(plays) {
  empty <- data.frame(set = integer(), server = character(), winner = character(),
                      home_setter = integer(), visiting_setter = integer())
  if (is.null(plays) || nrow(plays) == 0) return(empty)
  serve <- NA_integer_
  rows <- list()
  for (i in seq_len(nrow(plays))) {
    if (!is.na(plays$skill[i]) && plays$skill[i] == "S") {
      serve <- i
    } else if (plays$point[i] && !is.na(plays$team[i])) {
      if (!is.na(serve)) {
        rows[[length(rows) + 1]] <- data.frame(
          set = plays$set[serve], server = plays$team[serve], winner = plays$team[i],
          home_setter = plays$home_setter[serve], visiting_setter = plays$visiting_setter[serve]
        )
      }
      serve <- NA_integer_
    }
  }
  if (length(rows) == 0) empty else do.call(rbind, rows)
}

#' .dvw を読み込む。戻り値 list(teams, players, plays, rallies, file, mtime, size)
read_dvw <- function(path) {
  lines <- read_dvw_lines(path)
  teams <- split_fields(dvw_section(lines, "TEAMS"), 2)
  team_names <- if (nrow(teams) >= 2) trimws(teams[1:2, 2]) else c("ホーム", "ビジター")
  plays <- dvw_plays(dvw_section(lines, "SCOUT"))
  list(
    teams = setNames(team_names, DVW_SIDES),
    players = list(home = dvw_players(dvw_section(lines, "PLAYERS-H"), "home"),
                   visiting = dvw_players(dvw_section(lines, "PLAYERS-V"), "visiting")),
    plays = plays,
    rallies = dvw_rallies(plays),
    file = path,
    mtime = file.mtime(path),
    size = file.size(path)
  )
}

#' 選手ごとのプレー数(トス、レセプション、アタック、センターからのアタック)
dvw_player_counts <- function(players, plays, side) {
  p <- if (is.null(plays)) NULL else plays[!is.na(plays$team) & plays$team == side & !is.na(plays$number), ]
  count <- function(sel) {
    if (is.null(p)) return(numeric(nrow(players)))
    vapply(players$number, function(n) sum(sel & p$number == n, na.rm = TRUE), numeric(1))
  }
  data.frame(
    sets = count(p$skill == "E"),
    recv = count(p$skill == "R"),
    att = count(p$skill == "A"),
    middle_att = count(p$skill == "A" & (p$type == "Q" | p$start_zone %in% 3))
  )
}

#' 役割コードが空の選手の役割を、プレーの内容から推定する。
#'   リベロ印 → L / トス(E)が最も多い → S / クイック・センターからの攻撃が多い → MB /
#'   レセプション(R)が多い → OH / それ以外で攻撃する → OP
infer_dvw_roles <- function(players, plays, side) {
  role <- players$role
  role[is.na(role) & players$libero] <- "L"
  if (is.null(plays) || !anyNA(role)) return(ifelse(is.na(role), "OH", role))

  # セッターの宣言(*P04 = ホームのセッターは #4)
  prefix <- if (side == "home") "*" else "a"
  declared <- sub("^.P([0-9]{2}).*$", "\\1", plays$code[startsWith(plays$code, paste0(prefix, "P"))])
  declared <- suppressWarnings(as.integer(declared))
  declared <- declared[!is.na(declared) & declared > 0]
  if (!"S" %in% role && length(declared)) {
    s <- match(as.integer(names(which.max(table(declared)))), players$number)
    if (!is.na(s) && is.na(role[s])) role[s] <- "S"
  }

  cnt <- dvw_player_counts(players, plays, side)
  sets <- cnt$sets; recv <- cnt$recv; att <- cnt$att; middle_att <- cnt$middle_att

  unknown <- is.na(role)
  if (!"S" %in% role && any(unknown & sets > 0)) {
    s <- which(unknown)[which.max(sets[unknown])]
    role[s] <- "S"
    unknown[s] <- FALSE
  }
  mb <- unknown & att >= 3 & middle_att / pmax(att, 1) >= 0.4
  role[mb] <- "MB"
  unknown <- is.na(role)
  oh <- unknown & recv >= 5
  role[oh] <- "OH"
  unknown <- is.na(role)
  role[unknown & att >= 3] <- "OP"
  ifelse(is.na(role), "OH", role)
}

#' .dvw の一方のチームをアプリのチームにする。
#' スタートの並びは、指定したセットの最初の記録の P1〜P6
dvw_team <- function(dvw, side, team_id = if (side == "home") "us" else "them", set = 1) {
  pl <- dvw$players[[side]]
  role <- infer_dvw_roles(pl, dvw$plays, side)
  players <- data.frame(
    id = paste0(team_id, "-", pl$number),
    number = pl$number,
    name = trimws(paste(pl$last, pl$first)),
    role = role
  )

  lineup <- rep("", 6)
  plays <- dvw$plays
  if (!is.null(plays)) {
    cols <- paste0(side, "_p", 1:6)
    rows <- plays[!is.na(plays$set) & plays$set == set & stats::complete.cases(plays[, cols]), , drop = FALSE]
    if (nrow(rows) > 0) {
      nums <- unlist(rows[1, cols])
      lineup <- ifelse(nums %in% players$number, paste0(team_id, "-", nums), "")
    }
  }
  cnt <- dvw_player_counts(pl, dvw$plays, side)
  players$role <- apply_lineup_structure(players, lineup, inferred = is.na(pl$role) & !pl$libero,
                                         mb_evidence = cnt$middle_att - cnt$recv)
  liberos <- players$id[players$role == "L" & !players$id %in% lineup]
  list(id = team_id, name = dvw$teams[[side]], players = players, lineup = lineup,
       libero = if (length(liberos)) liberos[1] else NA_character_)
}

#' 5-1 の並びの決まり(セッターの対角 = OP、MB の対角 = MB、残りの対角 = OH)で、
#' 推定した役割を補正する(役割コードが入っていた選手は変えない)。
#' どちらの対角が MB かは、推定した役割 → mb_evidence(クイック数 - レセプション数)の順で決める
apply_lineup_structure <- function(players, lineup, inferred, mb_evidence = numeric(nrow(players))) {
  role <- players$role
  idx <- match(lineup, players$id)
  if (anyNA(idx) || length(idx) != 6) return(role)
  lr <- role[idx]
  s <- which(lr == "S")
  if (length(s) != 1) return(role)
  opp <- function(i) (i + 2) %% 6 + 1   # 対角(3つ先)のポジション
  set_role <- function(i, r) if (inferred[idx[i]]) role[idx[i]] <<- r
  set_role(opp(s), "OP")
  pairs <- Filter(function(p) !s %in% p, list(c(1, 4), c(2, 5), c(3, 6)))
  if (length(pairs) != 2) return(role)
  mb_score <- vapply(pairs, function(p) sum(lr[p] == "MB") * 1000 + sum(mb_evidence[idx[p]]), numeric(1))
  mb_pair <- pairs[[which.max(mb_score)]]
  oh_pair <- pairs[[3 - which.max(mb_score)]]
  for (i in mb_pair) set_role(i, "MB")
  for (i in oh_pair) set_role(i, "OH")
  role
}
