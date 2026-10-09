# 対面別の実績と、そこからの見込み ------------------------------------------------
#
# 実績は「自チームのローテーション × 相手のローテーション × サーブ権」の 72 通りの対面ごとに
# ラリー数と、自チームが取った数を数える(matchup_stats)。
# 見込み(自チームがそのラリーを取る確率)は、実績が少ない対面ほど、より大きな単位の実績に寄せる。
#   対面の実績 → ローテーション単位(自チームのそのローテ・相手のそのローテ)→ チーム全体 → 一般的な値

# 実績がまったくないときの値(レシーブ側のサイドアウト率)
DEFAULT_SIDE_OUT <- 0.6

#' 空の対面別実績(72 行)
empty_matchup_stats <- function() {
  g <- expand.grid(us = 0:5, them = 0:5, server = SIDES, stringsAsFactors = FALSE)
  g$n <- 0
  g$won <- 0
  g
}

#' セッター位置(1〜6)→ アプリのチームのローテーション(0〜5)。
#' S1 = セッターが P1 のローテーション、のように対応させる
setter_pos_to_rot <- function(team, setter_pos) {
  p0 <- setter_position(team, 0)
  if (is.na(p0)) p0 <- 1
  (p0 - setter_pos) %% 6
}

#' ラリーの記録(dvw_rallies() の形)から対面別の実績を作る。
#'   our_side: 自チームが "home" か "visiting" か
matchup_stats <- function(rallies, our_side, us_team, them_team) {
  out <- empty_matchup_stats()
  if (is.null(rallies) || nrow(rallies) == 0) return(out)
  their_side <- setdiff(DVW_SIDES, our_side)
  r <- rallies[!is.na(rallies[[paste0(our_side, "_setter")]]) & !is.na(rallies[[paste0(their_side, "_setter")]]), ]
  if (nrow(r) == 0) return(out)
  us <- setter_pos_to_rot(us_team, r[[paste0(our_side, "_setter")]])
  them <- setter_pos_to_rot(them_team, r[[paste0(their_side, "_setter")]])
  server <- ifelse(r$server == our_side, "us", "them")
  won <- r$winner == our_side
  key <- paste(us, them, server)
  okey <- paste(out$us, out$them, out$server)
  out$n <- as.numeric(table(factor(key, levels = okey)))
  out$won <- as.numeric(tapply(won, factor(key, levels = okey), sum, default = 0))
  out
}

#' ローテーション単位の実績(side = "us" なら自チーム、"them" なら相手のローテーション別)。
#'   receive / side_out    : レシーブしたラリー数 / そのうち取った数
#'   serve   / break_point : サーブしたラリー数 / そのうち取った数
rotation_summary <- function(ms, side = "us") {
  other <- setdiff(SIDES, side)
  rows <- lapply(0:5, function(k) {
    m <- ms[ms[[side]] == k, ]
    recv <- m[m$server == other, ]
    srv <- m[m$server == side, ]
    # won は自チームが取った数なので、相手側は n - won
    taken <- function(x) if (side == "us") sum(x$won) else sum(x$n - x$won)
    data.frame(rot = k, receive = sum(recv$n), side_out = taken(recv),
               serve = sum(srv$n), break_point = taken(srv))
  })
  do.call(rbind, rows)
}

#' 対面別の見込み:自チームがラリーを取る確率。
#' 戻り値は array [自チームのローテ + 1, 相手のローテ + 1, サーブ権(1 = us, 2 = them)]。
#'   prior_n: 実績が何本たまったら、より大きな単位の値と半々にするか
rally_prob_from_stats <- function(ms = empty_matchup_stats(), prior_n = 10) {
  shrink <- function(won, n, prior) (won + prior_n * prior) / (n + prior_n)
  us_rot <- rotation_summary(ms, "us")
  them_rot <- rotation_summary(ms, "them")
  p <- array(NA_real_, dim = c(6, 6, 2), dimnames = list(us = 0:5, them = 0:5, server = SIDES))

  for (srv in SIDES) {
    s <- match(srv, SIDES)
    m <- ms[ms$server == srv, ]
    # チーム全体(この局面の自チームの取得率)
    default <- if (srv == "them") DEFAULT_SIDE_OUT else 1 - DEFAULT_SIDE_OUT
    overall <- shrink(sum(m$won), sum(m$n), default)
    for (a in 0:5) for (b in 0:5) {
      u <- us_rot[us_rot$rot == a, ]
      t <- them_rot[them_rot$rot == b, ]
      # ローテーション単位:自チームのそのローテの取得率と、相手のそのローテで取られた率の平均
      rot_est <- if (srv == "them") {
        mean(c(shrink(u$side_out, u$receive, overall), 1 - shrink(t$break_point, t$serve, 1 - overall)))
      } else {
        mean(c(shrink(u$break_point, u$serve, overall), 1 - shrink(t$side_out, t$receive, 1 - overall)))
      }
      cell <- m[m$us == a & m$them == b, ]
      p[a + 1, b + 1, s] <- shrink(cell$won, cell$n, rot_est)
    }
  }
  p
}
