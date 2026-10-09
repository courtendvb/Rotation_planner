# コート図(SVG) ----------------------------------------------------------------
#
# 1単位 = 1m。自チームは下側(ネットに向かって左→右が x)、相手は上側に 180° 回して描く。

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

svg_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

marker_svg <- function(m, mine, offset, draggable, attrs = "", compact = FALSE) {
  x <- if (mine) m$x else 9 - m$x
  y <- if (mine) offset + m$y else 9 - m$y
  cls <- paste0("marker role-", m$role, if (draggable) " draggable")
  sub <- if (compact) "" else sprintf('<text class="marker-sub" y="0.98">%s</text>', svg_escape(m$sub))
  sprintf(
    '<g class="%s" data-pos="%d" data-side="%s"%s transform="translate(%.2f %.2f)"><circle r="%.2f"/><text class="marker-label" y="0.05">%s</text>%s</g>',
    cls, m$pos, if (mine) "us" else "them", attrs, x, y, if (compact) 0.7 else 0.62, svg_escape(m$label), sub
  )
}

ARROW_ROLES <- c("S", "OH", "MB", "OP", "L", "none")

# 矢じり(役割の色ごと)
arrow_defs <- function() {
  paste0("<defs>", paste(sprintf(
    '<marker id="rp-arrow-%s" class="arrowhead role-%s" viewBox="0 0 10 10" refX="8" refY="5" markerWidth="4" markerHeight="4" orient="auto-start-reverse"><path d="M0,0 L10,5 L0,10 z"/></marker>',
    ARROW_ROLES, ARROW_ROLES), collapse = ""), "</defs>")
}

# 矢印(arrows はチームの自コート座標の data.frame(role, x1, y1, x2, y2))
arrows_svg <- function(arrows, mine, offset) {
  if (is.null(arrows) || nrow(arrows) == 0) return(character())
  tx <- function(x) if (mine) x else 9 - x
  ty <- function(y) if (mine) offset + y else 9 - y
  role <- ifelse(arrows$role %in% ARROW_ROLES, arrows$role, "none")
  sprintf(
    '<line class="move-arrow role-%s" x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" marker-end="url(#rp-arrow-%s)"/>',
    role, tx(arrows$x1), ty(arrows$y1), tx(arrows$x2), ty(arrows$y2), role
  )
}

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
  side_margin <- if (compact) 0.8 else 1.2
  top <- if (full) 1.5 else 1.2  # 相手のサーバーの表示ぶん
  bottom <- 1.9                  # エンドラインの外に立つサーバーの表示ぶん
  lines <- if (full) {
    c('<line class="court-line" x1="0" x2="9" y1="6" y2="6"/>',
      '<line class="court-line" x1="0" x2="9" y1="12" y2="12"/>',
      '<line class="court-net" x1="-0.6" x2="9.6" y1="9" y2="9"/>')
  } else {
    c('<line class="court-line" x1="0" x2="9" y1="3" y2="3"/>',
      '<line class="court-net" x1="-0.6" x2="9.6" y1="0" y2="0"/>')
  }
  has_input <- !is.null(drag_input)
  us_drag <- has_input && (!full || !is.null(us_attrs))
  them_drag <- has_input && full && !is.null(them_attrs)
  draw <- function(ms, mine, drag, attrs) {
    vapply(seq_len(nrow(ms)), function(i) {
      marker_svg(ms[i, ], mine, offset, drag, if (drag) attrs %||% "" else "", compact)
    }, "")
  }
  has_arrows <- !is.null(us_arrows) || !is.null(them_arrows)
  body <- c(
    if (has_arrows) arrow_defs(),
    sprintf('<rect class="court-floor" x="0" y="0" width="9" height="%d"/>', height),
    lines,
    arrows_svg(them_arrows, FALSE, offset),
    arrows_svg(us_arrows, TRUE, offset),
    if (full) draw(opponent, FALSE, them_drag, them_attrs),
    draw(markers, TRUE, us_drag, us_attrs)
  )
  HTML(sprintf(
    '<svg class="rp-court%s" viewBox="%.1f %.1f %.1f %.1f" style="max-width:%dpx" data-offset="%d"%s>%s</svg>',
    if (compact) " compact" else "",
    -side_margin, -top, 9 + 2 * side_margin, height + top + bottom, max_width, offset,
    paste0(if (has_input) sprintf(' data-input="%s"', drag_input) else "",
           if (!is.null(court_key)) sprintf(' data-court="%s"', court_key) else ""),
    paste(body, collapse = "")
  ))
}
