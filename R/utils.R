# 小さな共通関数 ----------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

pct <- function(v, digits = 1) sprintf(paste0("%.", digits, "f%%"), v * 100)

#' 確率を背景色にする(50% = 白、高いほど緑、低いほど赤)
prob_color <- function(v) {
  d <- max(-1, min(1, (v - 0.5) / 0.15))
  if (d >= 0) {
    grDevices::rgb(1 - 0.55 * d, 1 - 0.2 * d, 1 - 0.5 * d)
  } else {
    grDevices::rgb(1, 1 + 0.45 * d, 1 + 0.45 * d)
  }
}
