# 既定のフォーメーション ------------------------------------------------------------
#
# レシーブ時は、5-1 の並びならレセプションの型(reception.R、指導者のレクチャー)を使う。
# それ以外の並びでは、役割からサーブレシーブ隊形を自動で作る(3人レセプション)。
#   - レセプション:OH 2人とリベロ(足りなければ後衛の S 以外で補う)が横一列に並ぶ
#   - 後衛のセッター:前衛の対応する選手のすぐ後ろに隠れ、すぐ前に出られる位置
#   - 前衛のセッター・レセプションに入らない前衛:ネット際
#   - レセプションに入らない後衛:エンドライン寄りに隠れる
# 最後にローテーション順(前後・左右の関係)を保つよう位置を補正する。
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
  rt <- reception_template(team, k)
  if (!is.null(rt)) template_placement(rt) else generic_reception(team, k)
}

#' 役割から自動で作るレセプションの隊形(型が使えない並び用)
generic_reception <- function(team, k) {
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

#' ローテーション順(前後・左右の関係)を保つよう、必要最小限だけ位置をずらす
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

#' 立ち位置の保存キー。レセプションで別パターンを選んでいるときは、パターンごとに分けて保存する
placement_key <- function(team, k, serving) {
  key <- sprintf("%s:%d:%s", team$id, k %% 6, if (serving) "serve" else "receive")
  if (!serving) {
    sp <- setter_position(team, k)
    v <- if (is.na(sp)) "standard" else reception_variant(team, sp)
    if (v != "standard") key <- paste0(key, ":", v)
  }
  key
}

#' 保存した立ち位置があればそれを、なければ既定のフォーメーションを返す
placement_for <- function(team, k, serving, placements = list()) {
  placements[[placement_key(team, k, serving)]] %||% default_formation(team, k, serving)
}

# レシーブ後の攻撃に入る動き --------------------------------------------------------
#
# 5-1 の並びでは、レセプションの型(reception.R)の経路を使う。
# それ以外の並びでは、役割から決めた攻撃位置へまっすぐ向かう。
#   前衛 OH:レフト、前衛 MB:センター、前衛 OP:ライト(いずれもネット際)
#   後衛 OH:パイプ、後衛 OP:バックライト(いずれもアタックラインの後ろ)
#   セッター・リベロ・後衛 MB:矢印なし
# 攻撃に入る位置の深さ:前衛はネット際、後衛はアタックラインのすぐ後ろ
ATTACK_FRONT_Y <- 0.5
ATTACK_BACK_Y <- 3.2

ATTACK_SPOTS <- list(
  front_OH = c(0.4, ATTACK_FRONT_Y),
  front_MB = c(4.0, ATTACK_FRONT_Y),
  front_OP = c(8.6, ATTACK_FRONT_Y),
  back_OH = c(4.5, ATTACK_BACK_Y),
  back_OP = c(8.4, ATTACK_BACK_Y)
)

#' 型が使えない並びでの攻撃位置。data.frame(pos, role, x, y)(動かない選手は含まない)
attack_positions <- function(team, k) {
  cp <- court_players(team, k, serving = FALSE)
  rows <- lapply(seq_len(nrow(cp)), function(i) {
    role <- cp$role[i]
    if (is.na(role)) return(NULL)
    spot <- ATTACK_SPOTS[[paste0(if (is_front_row(cp$pos[i])) "front_" else "back_", role)]]
    if (is.null(spot)) return(NULL)
    data.frame(pos = cp$pos[i], role = role, x = spot[1], y = spot[2])
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) data.frame(pos = integer(), role = character(), x = numeric(), y = numeric()) else out
}

#' レセプション位置から攻撃に入る動きの矢印。
#' 戻り値は矢印のリスト。各要素は list(role, x, y)(x, y は始点から終点までの点の並び)。
#' 始点は今の立ち位置(placement)なので、選手をドラッグすると矢印の始点も動く
transition_arrows <- function(team, k, placement, min_length = 0.8) {
  start <- function(pos) unlist(placement[placement$pos == pos, c("x", "y")])
  rt <- reception_template(team, k)
  arrows <- if (!is.null(rt)) {
    lapply(names(rt$template$path), function(slot) {
      pos <- rt$slots[[slot]]
      path <- rt$template$path[[slot]]
      s <- start(pos)
      list(role = court_players(team, k, FALSE)$role[pos], x = c(s[[1]], path$x), y = c(s[[2]], path$y))
    })
  } else {
    ap <- attack_positions(team, k)
    lapply(seq_len(nrow(ap)), function(i) {
      s <- start(ap$pos[i])
      list(role = ap$role[i], x = c(s[[1]], ap$x[i]), y = c(s[[2]], ap$y[i]))
    })
  }
  Filter(function(a) sum(sqrt(diff(a$x)^2 + diff(a$y)^2)) >= min_length, arrows)
}
