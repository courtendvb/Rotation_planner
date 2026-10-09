# セット勝率(マルコフ連鎖) -------------------------------------------------
#
# 状態 = (自チームのローテーション a, 相手のローテーション b, サーブ権) の 72 通り。
# 各ラリーで自チームが勝つ確率は p[a, b, server] で与える。
# サーブ側が勝てばそのまま、レシーブ側が勝てばそのチームが1つ回ってサーブ権を得る。

# 状態 (a, b, s) の通し番号(1〜72)。a, b は 0〜5、s は 1 = us, 2 = them
state_index <- function(a, b, s) a %% 6 + 6 * (b %% 6) + 36 * (s - 1) + 1

state_grid <- function() expand.grid(a = 0:5, b = 0:5, s = 1:2)

# 各状態から、自チームが勝った/負けた後の状態の通し番号
transitions <- function() {
  g <- state_grid()
  list(
    us_win = ifelse(g$s == 1, state_index(g$a, g$b, 1), state_index(g$a + 1, g$b, 1)),
    them_win = ifelse(g$s == 2, state_index(g$a, g$b, 2), state_index(g$a, g$b + 1, 2))
  )
}

#' 全スタート状態からのセット取得確率(長さ72のベクトル)。
#' 後ろ向きに計算し、デュースは連立方程式で厳密に解く
set_win_all_states <- function(p, target = 25) {
  p <- as.vector(p)
  q <- 1 - p
  tr <- transitions()
  iu <- tr$us_win
  it <- tr$them_win
  n <- 72

  # デュース(target-1 オール以降の同点)の値 T を解く。
  #   A(自チーム1点リード) = p + q * T[it]
  #   B(相手1点リード)    = p * T[iu]
  #   T                   = p * A[iu] + q * B[it]
  Pu <- diag(n)[iu, ]
  Pt <- diag(n)[it, ]
  M <- diag(p) %*% Pu %*% diag(q) %*% Pt + diag(q) %*% Pt %*% diag(p) %*% Pu
  tie <- as.vector(solve(diag(n) - M, p * p[iu]))
  tie <- pmin(pmax(tie, 0), 1)

  # V[[u + 1]][[t + 1]] = 得点 (u, t) での値。u, t は 0〜target-1
  last <- target - 1
  V <- vector("list", target)
  for (u in last:0) {
    V[[u + 1]] <- vector("list", target)
    for (t in last:0) {
      if (u == last && t == last) {
        V[[u + 1]][[t + 1]] <- tie
        next
      }
      next_u <- if (u == last) rep(1, n) else V[[u + 2]][[t + 1]]
      next_t <- if (t == last) rep(0, n) else V[[u + 1]][[t + 2]]
      V[[u + 1]][[t + 1]] <- p * next_u[iu] + q * next_t[it]
    }
  }
  V[[1]][[1]]
}

#' 1つのスタートの組み合わせのセット取得確率
set_win_probability <- function(p, us_start, them_start, first_server, target = 25) {
  s <- match(first_server, SIDES)
  set_win_all_states(p, target)[state_index(us_start, them_start, s)]
}

#' [自チームのスタート, 相手のスタート] → セット取得確率 の 6 x 6 行列
start_matrix <- function(p, first_server, target = 25) {
  v <- set_win_all_states(p, target)
  s <- match(first_server, SIDES)
  m <- outer(0:5, 0:5, function(a, b) v[state_index(a, b, s)])
  dimnames(m) <- list(us = 0:5, them = 0:5)
  m
}

#' 自チームのスタート候補を期待値の高い順に並べる。
#' them_weights は相手の各スタートの予想の重み(省略時は一様、0 は「来ない」)
rank_starts <- function(m, them_weights = rep(1, 6)) {
  w <- pmax(them_weights, 0)
  if (sum(w) <= 0) w <- rep(1, 6)
  w <- w / sum(w)
  res <- data.frame(
    us_start = 0:5,
    expected = as.vector(m %*% w),
    worst = apply(m[, w > 0, drop = FALSE], 1, min)
  )
  res[order(-res$expected, -res$worst), ] |> `rownames<-`(NULL)
}
