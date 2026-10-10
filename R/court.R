# コート図(SVG) ----------------------------------------------------------------
#
# 1単位 = 1m。自チームは下側(ネットに向かって左→右が x)、相手は上側に 180° 回して描く。
#
# 自チームのエンドラインの後ろ上方から見下ろした遠近法で描く(奥ほど狭い台形)。
# 「表示座標」(長方形のコート上の位置)を court_projection() で画面に写す。
# 直線は直線のまま写るので、コートのラインもそのまま描ける。マーカーの大きさは変えない。

# 奥の幅 / 手前の幅
PERSPECTIVE_RATIO <- 0.72
# 奥行きの縮み(画面上のコートの高さ / 実際の長さ)
PERSPECTIVE_DEPTH <- 0.82

#' 遠近法の写し方。H は表示するコートの長さ(手前のエンドラインが y = H)
court_projection <- function(H, ratio = PERSPECTIVE_RATIO, depth = PERSPECTIVE_DEPTH) {
  list(cx = 4.5, H = H, z0 = ratio * H / (1 - ratio), yh = H * (1 - depth - ratio) / (1 - ratio))
}

#' 表示座標 (x, y) → 画面座標。w は奥行きによる縮み(手前のエンドラインで 1)
project_point <- function(pr, x, y) {
  w <- pr$z0 / (pr$z0 + (pr$H - y))
  list(x = pr$cx + (x - pr$cx) * w, y = pr$yh + (pr$H - pr$yh) * w, w = w)
}



#' チームのローテーションをコート図のマーカー(data.frame)にする
team_markers <- function(team, k, serving, placement = default_placement()) {
  cp <- court_players(team, k, serving)
  pl <- placement[match(1:6, placement$pos), ]
  data.frame(
    pos = 1:6,
    x = pl$x,
    y = pl$y,
    label = ifelse(is.na(cp$number), "?", as.character(cp$number)),
    sub = ifelse(is.na(cp$role), paste0("P", 1:6), paste0("P", 1:6, " ", cp$role)),
    role = ifelse(is.na(cp$role), "none", cp$role)
  )
}

#' 対面の表示用:レシーブ側はレセプションフォーメーション、サーブ側はサーブ時の配置
match_markers <- function(team, k, serving, placements = list()) {
  team_markers(team, k, serving, placement_for(team, k, serving, placements))
}

#' 画面座標 → 表示座標(project_point の逆。ドラッグの JavaScript と同じ式)
unproject_point <- function(pr, X, Y) {
  w <- (Y - pr$yh) / (pr$H - pr$yh)
  list(x = pr$cx + (X - pr$cx) / w, y = pr$H - (pr$z0 / w - pr$z0))
}

svg_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# チームの自コート座標 → 表示座標(相手は 180° 回す)
to_display <- function(x, y, mine, offset) {
  list(x = if (mine) x else 9 - x, y = if (mine) offset + y else 9 - y)
}

marker_svg <- function(m, mine, offset, pr, draggable, attrs = "", compact = FALSE) {
  d <- to_display(m$x, m$y, mine, offset)
  p <- project_point(pr, d$x, d$y)
  cls <- paste0("marker role-", m$role, if (draggable) " draggable")
  sub <- if (compact) "" else sprintf('<text class="marker-sub" y="0.98">%s</text>', svg_escape(m$sub))
  sprintf(
    '<g class="%s" data-pos="%d" data-side="%s"%s transform="translate(%.2f %.2f)"><circle r="%.2f"/><text class="marker-label" y="0.05">%s</text>%s</g>',
    cls, m$pos, if (mine) "us" else "them", attrs, p$x, p$y, if (compact) 0.7 else 0.62, svg_escape(m$label), sub
  )
}

# 矢じり
arrow_defs <- function() {
  paste0('<marker id="rp-arrow" class="arrowhead" viewBox="0 0 10 10" refX="7" refY="5" ',
         'markerWidth="4.5" markerHeight="4.5" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z"/></marker>')
}

# 点の並びを通るなめらかな曲線(Catmull-Rom を3次ベジェにしたもの)の SVG パス
smooth_path <- function(x, y) {
  n <- length(x)
  if (n == 2) return(sprintf("M%.2f,%.2f L%.2f,%.2f", x[1], y[1], x[2], y[2]))
  px <- c(x[1], x, x[n]); py <- c(y[1], y, y[n])
  seg <- vapply(seq_len(n - 1), function(i) {
    j <- i + 1  # px/py での点 i の位置
    sprintf("C%.2f,%.2f %.2f,%.2f %.2f,%.2f",
            px[j] + (px[j + 1] - px[j - 1]) / 6, py[j] + (py[j + 1] - py[j - 1]) / 6,
            px[j + 1] - (px[j + 2] - px[j]) / 6, py[j + 1] - (py[j + 2] - py[j]) / 6,
            px[j + 1], py[j + 1])
  }, "")
  paste(sprintf("M%.2f,%.2f", x[1], y[1]), paste(seg, collapse = " "))
}

# 矢印(arrows は transition_arrows() の結果。座標はチームの自コート視点)
arrows_svg <- function(arrows, mine, offset, pr) {
  if (length(arrows) == 0) return(character())
  vapply(arrows, function(a) {
    d <- to_display(a$x, a$y, mine, offset)
    p <- project_point(pr, d$x, d$y)
    sprintf('<path class="move-arrow role-%s" d="%s" marker-end="url(#rp-arrow)"/>', a$role, smooth_path(p$x, p$y))
  }, "")
}

# 表示座標の線分を写して描く
line_svg <- function(pr, cls, x1, y1, x2, y2) {
  a <- project_point(pr, x1, y1)
  b <- project_point(pr, x2, y2)
  sprintf('<line class="%s" x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f"/>', cls, a$x, a$y, b$x, b$y)
}

# 表示座標の多角形を写して描く
polygon_svg <- function(pr, cls, xs, ys, extra = "") {
  p <- project_point(pr, xs, ys)
  sprintf('<polygon class="%s" points="%s"%s/>', cls, paste(sprintf("%.2f,%.2f", p$x, p$y), collapse = " "), extra)
}

# ネット(白帯のある、高さのある幕として描く)
net_svg <- function(pr, y) {
  l <- project_point(pr, -0.6, y)
  r <- project_point(pr, 9.6, y)
  h <- NET_HEIGHT * l$w
  c(
    sprintf('<polygon class="court-net-mesh" points="%.2f,%.2f %.2f,%.2f %.2f,%.2f %.2f,%.2f"/>',
            l$x, l$y, r$x, r$y, r$x, r$y - h, l$x, l$y - h),
    sprintf('<line class="court-net-band" x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f"/>', l$x, l$y - h, r$x, r$y - h),
    sprintf('<line class="court-net" x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f"/>', l$x, l$y, r$x, r$y),
    sprintf('<line class="court-net-post" x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f"/>', l$x, l$y, l$x, l$y - h * 1.15),
    sprintf('<line class="court-net-post" x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f"/>', r$x, r$y, r$x, r$y - h * 1.15)
  )
}

# ネットの見かけの高さ(m)
NET_HEIGHT <- 1.1

#' ドラッグ時に送る情報(data 属性)
drag_attrs <- function(rot, serving) {
  sprintf(' data-rot="%d" data-phase="%s"', as.integer(rot), if (serving) "serve" else "receive")
}

#' コート図を作る。
#'   markers   : 自チームのマーカー
#'   opponent  : 相手のマーカー(NULL なら自コートのみ)
#'   drag_input: ドラッグ結果を受け取る Shiny の input id(NULL ならドラッグ不可)
#'   us_attrs / them_attrs: ドラッグ可能にするチームのマーカーに付ける data 属性(drag_attrs())。
#'               NULL のチームはドラッグできない(自コートのみの図では自チームは常にドラッグ可)
#'   court_key : 複数のコートを並べるときの識別子(ドラッグ結果の court に入る)
#'   compact   : 小さく並べる用(ポジション名を省き、番号だけ表示)
#'   us_arrows / them_arrows: 移動の矢印(transition_arrows() の結果)
court_svg <- function(markers, opponent = NULL, drag_input = NULL, max_width = 440, court_key = NULL,
                      us_attrs = NULL, them_attrs = NULL, compact = FALSE,
                      us_arrows = NULL, them_arrows = NULL) {
  full <- !is.null(opponent)
  offset <- if (full) 9 else 0
  height <- if (full) 18 else 9
  pr <- court_projection(height)
  side_margin <- if (compact) 0.8 else 1.2
  top <- if (full) 1.5 else 1.2  # 相手のサーバーの表示ぶん
  bottom <- 1.9                  # エンドラインの外に立つサーバーの表示ぶん

  # 画面上の表示範囲:奥(上)はネットの高さとマーカーのぶん、手前(下)はサーバーのぶん
  top_y <- project_point(pr, 0, -top)$y - 0.8
  bottom_y <- project_point(pr, 0, height + bottom)$y
  net_y <- if (full) 9 else 0
  top_y <- min(top_y, project_point(pr, 0, net_y)$y - NET_HEIGHT - 0.3)

  lines <- c(
    if (full) c(line_svg(pr, "court-line", 0, 6, 9, 6), line_svg(pr, "court-line", 0, 12, 9, 12))
    else line_svg(pr, "court-line", 0, 3, 9, 3)
  )
  has_input <- !is.null(drag_input)
  us_drag <- has_input && (!full || !is.null(us_attrs))
  them_drag <- has_input && full && !is.null(them_attrs)
  draw <- function(ms, mine, drag, attrs) {
    vapply(seq_len(nrow(ms)), function(i) {
      marker_svg(ms[i, ], mine, offset, pr, drag, if (drag) attrs %||% "" else "", compact)
    }, "")
  }
  # 奥 → 手前の順に描く(手前のものが上に重なる)
  body <- c(
    sprintf('<defs><linearGradient id="rp-floor-shade" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="#000" stop-opacity="0.10"/><stop offset="1" stop-color="#000" stop-opacity="0"/></linearGradient>%s</defs>',
            if (!is.null(us_arrows) || !is.null(them_arrows)) arrow_defs() else ""),
    polygon_svg(pr, "court-floor", c(0, 9, 9, 0), c(0, 0, height, height)),
    polygon_svg(pr, "court-shade", c(0, 9, 9, 0), c(0, 0, height, height), ' fill="url(#rp-floor-shade)"'),
    lines,
    if (full) c(arrows_svg(them_arrows, FALSE, offset, pr), draw(opponent, FALSE, them_drag, them_attrs)),
    net_svg(pr, net_y),
    arrows_svg(us_arrows, TRUE, offset, pr),
    draw(markers, TRUE, us_drag, us_attrs)
  )
  HTML(sprintf(
    '<svg class="rp-court%s" viewBox="%.2f %.2f %.2f %.2f" style="max-width:%dpx" data-offset="%d" data-persp="%.5f,%.5f,%.5f,%.5f"%s>%s</svg>',
    if (compact) " compact" else "",
    -side_margin, top_y, 9 + 2 * side_margin, bottom_y - top_y, max_width, offset,
    pr$cx, pr$H, pr$z0, pr$yh,
    paste0(if (has_input) sprintf(' data-input="%s"', drag_input) else "",
           if (!is.null(court_key)) sprintf(' data-court="%s"', court_key) else ""),
    paste(body, collapse = "")
  ))
}
