# 既定のフォーメーション ------------------------------------------------------------
#
# レシーブ時は、役割からサーブレシーブ隊形を自動で作る(3人レセプション)。
#   - レセプション:OH 2人とリベロ(足りなければ後衛の S 以外で補う)が横一列に並ぶ
#   - 後衛のセッター:前衛の対応する選手のすぐ後ろに隠れ、すぐ前に出られる位置
#   - 前衛のセッター・レセプションに入らない前衛:ネット際
#   - レセプションに入らない後衛:エンドライン寄りに隠れる
# 最後にポジショナルフォルトにならないよう位置を補正するので、どんな並びでも反則にならない。
#
# サーブ時は基本ポジションで、サーバー(P1)はエンドラインの外に立つ。

# ポジションの列(1 = 左, 2 = 中, 3 = 右)
POSITION_COLUMN <- c(`1` = 3, `2` = 3, `3` = 2, `4` = 1, `5` = 1, `6` = 2)
COLUMN_X <- c(1.5, 4.5, 7.5)
# 後衛のセッターが前の選手からずれる距離(丸が重ならないように)
SETTER_OFFSET <- 1.3

FORMATION_GAP <- 0.5
COURT_MIN <- 0.4
COURT_MAX <- 8.6

#' 既定のフォーメーション(placement の data.frame)
default_formation <- function(team, k, serving) {
  if (serving) default_serve_placement() else default_reception(team, k)
}

default_serve_placement <- function() {
  pl <- default_placement()
  pl[pl$pos == 1, c("x", "y")] <- c(7.5, 9.6)
  pl
}

#' レセプションに入る選手のポジション
reception_passers <- function(cp) {
  role <- cp$role
  pos <- cp$pos
  front <- is_front_row(pos)
  # 優先順:リベロ → 後衛の OH → 前衛の OH
  rank <- ifelse(role %in% "L", 1, ifelse(role %in% "OH" & !front, 2, ifelse(role %in% "OH", 3, NA)))
  passers <- pos[!is.na(rank)][order(rank[!is.na(rank)])]
  if (length(passers) < 3) {
    extra <- pos[!front & !(role %in% c("S", "OH", "L")) & !is.na(role)]
    passers <- c(passers, extra)
  }
  utils::head(passers, 3)
}

#' レシーブ時の既定のフォーメーション
default_reception <- function(team, k) {
  cp <- court_players(team, k, serving = FALSE)
  pl <- default_placement()
  col <- POSITION_COLUMN[as.character(pl$pos)]
  front <- is_front_row(pl$pos)
  role <- cp$role[match(pl$pos, cp$pos)]

  passers <- reception_passers(cp)
  setter <- pl$pos[!is.na(role) & role == "S"][1]

  for (i in seq_len(nrow(pl))) {
    p <- pl$pos[i]
    if (p %in% passers) next
    if (!is.na(setter) && p == setter && !front[i]) {
      next  # 後衛のセッターは、ほかの選手の位置が決まってから置く
    } else if (front[i]) {
      pl$x[i] <- COLUMN_X[col[i]]
      pl$y[i] <- 0.9
    } else {
      pl$x[i] <- COLUMN_X[col[i]]
      pl$y[i] <- 8.3
    }
  }

  # レセプションの列:左の列から順に、同じ列なら前衛を先に
  if (length(passers) > 0) {
    idx <- match(passers, pl$pos)
    ord <- idx[order(col[idx], !front[idx])]
    slots <- seq(1.6, 7.4, length.out = max(length(ord), 2))
    if (length(ord) == 1) slots <- 4.5
    pl$x[ord] <- slots[seq_along(ord)]
    pl$y[ord] <- ifelse(front[ord], 5.2, 6.2)
  }

  # 後衛のセッター:前の選手の斜め後ろ(セッターが上がるライト寄り)に隠れる
  if (!is.na(setter) && !is_front_row(setter)) {
    f <- FRONT_BACK_PAIRS[[which(vapply(FRONT_BACK_PAIRS, `[`, 0, 2) == setter)]][1]
    pl$x[setter] <- min(pl$x[f] + SETTER_OFFSET, COURT_MAX)
    pl$y[setter] <- pl$y[f] + SETTER_OFFSET
  }

  legalize_placement(pl)
}

#' ポジショナルフォルトにならないよう、必要最小限だけ位置をずらす
legalize_placement <- function(pl, gap = FORMATION_GAP) {
  pl <- pl[order(pl$pos), ]
  # 左右:前衛 P4 < P3 < P2、後衛 P5 < P6 < P1
  for (row in list(c(4, 3, 2), c(5, 6, 1))) {
    x <- pl$x[row]
    for (j in 2:3) x[j] <- max(x[j], x[j - 1] + gap)
    x[3] <- min(x[3], COURT_MAX)
    for (j in 2:1) x[j] <- min(x[j], x[j + 1] - gap)
    pl$x[row] <- x
  }
  # 前後:後衛は対応する前衛より後ろ
  for (pair in FRONT_BACK_PAIRS) {
    f <- pair[1]; b <- pair[2]
    pl$y[b] <- max(pl$y[b], pl$y[f] + gap)
    if (pl$y[b] > COURT_MAX) {
      pl$y[b] <- COURT_MAX
      pl$y[f] <- min(pl$y[f], COURT_MAX - gap)
    }
  }
  pl
}

#' 立ち位置の保存キー
placement_key <- function(team, k, serving) {
  sprintf("%s:%d:%s", team$id, k %% 6, if (serving) "serve" else "receive")
}

#' 保存した立ち位置があればそれを、なければ既定のフォーメーションを返す
placement_for <- function(team, k, serving, placements = list()) {
  placements[[placement_key(team, k, serving)]] %||% default_formation(team, k, serving)
}
