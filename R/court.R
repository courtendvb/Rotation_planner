# コート図(SVG) ----------------------------------------------------------------
#
# 1単位 = 1m。自チームは下側(ネットに向かって左→右が x)、相手は上側に 180° 回して描く。

#' チームのローテーションをコート図のマーカー(data.frame)にする
team_markers <- function(team, k, serving, placement = default_placement(), warn = integer()) {
  cp <- court_players(team, k, serving)
  pl <- placement[match(1:6, placement$pos), ]
  data.frame(
    pos = 1:6,
    x = pl$x,
    y = pl$y,
    label = ifelse(is.na(cp$number), "?", as.character(cp$number)),
    sub = ifelse(is.na(cp$role), paste0("P", 1:6), paste0("P", 1:6, " ", cp$role)),
    role = ifelse(is.na(cp$role), "none", cp$role),
    warn = 1:6 %in% warn
  )
}

svg_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

marker_svg <- function(m, mine, offset, draggable) {
  x <- if (mine) m$x else 9 - m$x
  y <- if (mine) offset + m$y else 9 - m$y
  cls <- paste0("marker role-", m$role, if (m$warn) " warn", if (draggable) " draggable")
  sprintf(
    '<g class="%s" data-pos="%d" transform="translate(%.2f %.2f)"><circle r="0.62"/><text class="marker-label" y="0.05">%s</text><text class="marker-sub" y="0.98">%s</text></g>',
    cls, m$pos, x, y, svg_escape(m$label), svg_escape(m$sub)
  )
}

#' コート図を作る。
#'   markers   : 自チームのマーカー
#'   opponent  : 相手のマーカー(NULL なら自コートのみ)
#'   drag_input: ドラッグ結果を受け取る Shiny の input id(NULL ならドラッグ不可)
court_svg <- function(markers, opponent = NULL, drag_input = NULL, max_width = 440) {
  full <- !is.null(opponent)
  offset <- if (full) 9 else 0
  height <- if (full) 18 else 9
  margin <- 1.2
  lines <- if (full) {
    c('<line class="court-line" x1="0" x2="9" y1="6" y2="6"/>',
      '<line class="court-line" x1="0" x2="9" y1="12" y2="12"/>',
      '<line class="court-net" x1="-0.6" x2="9.6" y1="9" y2="9"/>')
  } else {
    c('<line class="court-line" x1="0" x2="9" y1="3" y2="3"/>',
      '<line class="court-net" x1="-0.6" x2="9.6" y1="0" y2="0"/>')
  }
  draggable <- !is.null(drag_input)
  body <- c(
    sprintf('<rect class="court-floor" x="0" y="0" width="9" height="%d"/>', height),
    lines,
    if (full) vapply(seq_len(nrow(opponent)), function(i) marker_svg(opponent[i, ], FALSE, offset, FALSE), ""),
    vapply(seq_len(nrow(markers)), function(i) marker_svg(markers[i, ], TRUE, offset, draggable), "")
  )
  HTML(sprintf(
    '<svg class="rp-court" viewBox="%.1f %.1f %.1f %.1f" style="max-width:%dpx" data-offset="%d"%s>%s</svg>',
    -margin, -margin, 9 + 2 * margin, height + 2 * margin, max_width, offset,
    if (draggable) sprintf(' data-input="%s"', drag_input) else "",
    paste(body, collapse = "")
  ))
}
