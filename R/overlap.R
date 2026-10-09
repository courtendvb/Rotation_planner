# ポジショナルフォルト(オーバーラップ)判定 ---------------------------------
#
# 立ち位置は data.frame(pos = 1:6, x, y) で表す(単位メートル、自コート視点)。
#   x: 0 = 左サイドライン 〜 9 = 右サイドライン(ネットに向かって)
#   y: 0 = センターライン 〜 9 = エンドライン

#' 基本ポジション(標準的な立ち位置)
default_placement <- function() {
  data.frame(
    pos = 1:6,
    x = c(7.5, 7.5, 4.5, 1.5, 1.5, 4.5),
    y = c(6.5, 1.5, 1.5, 1.5, 6.5, 6.5)
  )
}

# 前衛 → その後ろの後衛
FRONT_BACK_PAIRS <- list(c(4L, 5L), c(3L, 6L), c(2L, 1L))
# 左 → 右 の隣り合う組
LEFT_RIGHT_PAIRS <- list(c(4L, 3L), c(3L, 2L), c(5L, 6L), c(6L, 1L))

#' サーブ時のポジショナルフォルトを判定する。
#' 選手は1点で表すので、同じ座標(同列)はフォルトとみなす。
#' サーブを打つチームの P1(サーバー)は判定の対象外。
#' 戻り値は data.frame(kind, a, b, message)。反則がなければ 0 行
check_overlap <- function(placement, serving) {
  pl <- placement[order(placement$pos), ]
  exempt <- function(p) serving && p == 1L
  rows <- list()

  for (pair in FRONT_BACK_PAIRS) {
    front <- pair[1]; back <- pair[2]
    if (exempt(front) || exempt(back)) next
    if (!(pl$y[front] < pl$y[back])) {
      rows[[length(rows) + 1]] <- data.frame(
        kind = "front-back", a = front, b = back,
        message = sprintf("P%d が P%d より前にいます", back, front)
      )
    }
  }
  for (pair in LEFT_RIGHT_PAIRS) {
    left <- pair[1]; right <- pair[2]
    if (exempt(left) || exempt(right)) next
    if (!(pl$x[left] < pl$x[right])) {
      rows[[length(rows) + 1]] <- data.frame(
        kind = "left-right", a = left, b = right,
        message = sprintf("P%d が P%d より右にいます", left, right)
      )
    }
  }
  if (length(rows) == 0) {
    return(data.frame(kind = character(), a = integer(), b = integer(), message = character()))
  }
  do.call(rbind, rows)
}
