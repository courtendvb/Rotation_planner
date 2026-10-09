# 既定のフォーメーションがローテーション順(前後・左右の関係)を守っているかの確認用
keeps_rotation_order <- function(pl, serving) {
  pl <- pl[order(pl$pos), ]
  exempt <- function(p) serving && p == 1
  ok <- TRUE
  for (pr in list(c(4, 5), c(3, 6), c(2, 1))) {
    if (!exempt(pr[2])) ok <- ok && pl$y[pr[1]] < pl$y[pr[2]]
  }
  for (pr in list(c(4, 3), c(3, 2), c(5, 6), c(6, 1))) {
    if (!exempt(pr[1]) && !exempt(pr[2])) ok <- ok && pl$x[pr[1]] < pl$x[pr[2]]
  }
  ok
}
