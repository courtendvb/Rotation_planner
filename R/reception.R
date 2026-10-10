# レセプションの型(5-1)と、攻撃位置への動き ------------------------------------------
#
# 指導者のレクチャー(ローテーションごとのレセプション位置と、攻撃に入る動きの図)を
# セッターの位置(S1〜S6)ごとに持つ。座標は各チームの自コート視点(m)。
#   x: 0 = 左サイドライン 〜 9 = 右サイドライン、y: 0 = ネット 〜 9 = エンドライン
#
# 役割の枠:S = セッター、OP = オポジット、MB = 前衛のミドル、
#           OHF = 前衛のアウトサイド、OHB = 後衛のアウトサイド、L = リベロ(いなければ後衛のミドル)
#
# path は矢印の通過点と終点(始点はその選手の立ち位置)。セッターとリベロは矢印なし。
# 終点は、前衛アタッカーはネット際(ATTACK_FRONT_Y)、後衛アタッカーはアタックラインの後ろ
# (ATTACK_BACK_Y)にそろえる(どちらも formation.R)。

RECEPTION_TEMPLATES <- list(
  `1` = list(
    pos = list(S = c(8.1, 8.3), OP = c(1.4, 2.0), MB = c(2.1, 3.3), OHF = c(2.4, 6.0), OHB = c(4.8, 6.3), L = c(7.0, 6.4)),
    path = list(OP = list(x = c(5.0, 8.5, 8.8, 8.7), y = c(2.0, 3.0, 4.4, ATTACK_FRONT_Y)),
                MB = list(x = c(3.7, 4.1), y = c(3.4, ATTACK_FRONT_Y)),
                OHF = list(x = c(0.5, 0.4), y = c(3.5, ATTACK_FRONT_Y)),
                OHB = list(x = 4.8, y = ATTACK_BACK_Y))
  ),
  `6` = list(
    pos = list(S = c(5.0, 3.2), OP = c(5.8, 2.2), MB = c(7.4, 2.6), OHF = c(1.5, 6.0), OHB = c(7.4, 6.5), L = c(4.4, 6.9)),
    path = list(MB = list(x = c(3.8, 3.9), y = c(2.5, ATTACK_FRONT_Y)),
                OP = list(x = c(8.1, 9.0, 8.7), y = c(3.7, 4.4, ATTACK_FRONT_Y)),
                OHF = list(x = c(0.4, 0.6), y = c(3.4, ATTACK_FRONT_Y)),
                OHB = list(x = c(4.3, 4.2), y = c(6.5, ATTACK_BACK_Y)))
  ),
  `5` = list(
    pos = list(S = c(2.5, 3.8), OP = c(7.9, 2.2), MB = c(1.6, 2.6), OHF = c(1.8, 5.9), OHB = c(4.5, 6.5), L = c(7.2, 6.9)),
    path = list(OHF = list(x = c(0.3, 0.4), y = c(3.0, ATTACK_FRONT_Y)),
                MB = list(x = c(3.6, 3.7), y = c(2.3, ATTACK_FRONT_Y)),
                OHB = list(x = 4.5, y = ATTACK_BACK_Y),
                OP = list(x = c(8.7, 8.7), y = c(4.1, ATTACK_FRONT_Y)))
  ),
  `4` = list(
    pos = list(S = c(0.8, 1.4), OP = c(8.4, 8.0), MB = c(1.5, 2.8), OHF = c(2.1, 5.8), OHB = c(4.7, 6.4), L = c(7.2, 6.3)),
    path = list(OHF = list(x = c(0.3, 0.3), y = c(3.5, ATTACK_FRONT_Y)),
                MB = list(x = c(4.4, 4.6), y = c(3.0, ATTACK_FRONT_Y)),
                OHB = list(x = 4.7, y = ATTACK_BACK_Y),
                OP = list(x = 8.5, y = ATTACK_BACK_Y))
  ),
  `3` = list(
    pos = list(S = c(5.3, 1.0), OP = c(5.6, 8.0), MB = c(6.8, 2.6), OHF = c(2.0, 5.8), OHB = c(7.6, 5.6), L = c(4.6, 6.1)),
    path = list(OHF = list(x = c(0.3, 0.4), y = c(3.5, ATTACK_FRONT_Y)),
                MB = list(x = c(3.5, 3.3), y = c(3.1, ATTACK_FRONT_Y)),
                OHB = list(x = c(4.7, 4.5), y = c(4.9, ATTACK_BACK_Y)),
                OP = list(x = c(8.6, 8.7), y = c(7.4, ATTACK_BACK_Y)))
  ),
  `2` = list(
    pos = list(S = c(6.5, 1.3), OP = c(2.6, 8.0), MB = c(1.0, 2.3), OHF = c(1.7, 5.8), OHB = c(4.3, 6.4), L = c(7.0, 6.7)),
    path = list(OHF = list(x = c(0.2, 0.3), y = c(3.5, ATTACK_FRONT_Y)),
                MB = list(x = c(4.1, 4.2), y = c(2.9, ATTACK_FRONT_Y)),
                OHB = list(x = 4.2, y = ATTACK_BACK_Y),
                OP = list(x = c(8.3, 9.0, 8.6), y = c(8.9, 5.7, ATTACK_BACK_Y)))
  )
)

#' レセプションの型の役割の枠に、コート上の選手のポジションを当てはめる。
#' 5-1 の形(S・OP・前衛 MB・前衛 OH・後衛 OH・リベロ(または後衛 MB)が1人ずつ)でなければ NULL
reception_slots <- function(team, k) {
  cp <- court_players(team, k, serving = FALSE)
  front <- is_front_row(cp$pos)
  role <- cp$role
  one <- function(sel) if (sum(sel, na.rm = TRUE) == 1) cp$pos[which(sel)] else NA_integer_
  slots <- c(
    S = one(role %in% "S"),
    OP = one(role %in% "OP"),
    MB = one(role %in% "MB" & front),
    OHF = one(role %in% "OH" & front),
    OHB = one(role %in% "OH" & !front),
    L = one(role %in% "L" | (role %in% "MB" & !front))
  )
  if (anyNA(slots) || anyDuplicated(slots)) NULL else slots
}

# 型が前提とする並び(セッターから時計回りと逆に P1 → P2 → ... の順)
STANDARD_5_1 <- c("S", "OH", "MB", "OP", "OH", "MB")

#' 並びが型の前提(S → OH → MB → OP → OH → MB)どおりか
is_standard_5_1 <- function(team) {
  roles <- team$players$role[match(team$lineup, team$players$id)]
  s <- which(roles %in% "S")
  length(s) == 1 && identical(roles[(s - 1 + 0:5) %% 6 + 1], STANDARD_5_1)
}

#' その並び・ローテーションで使うレセプションの型(使えなければ NULL)
reception_template <- function(team, k) {
  if (!is_standard_5_1(team)) return(NULL)
  sp <- setter_position(team, k)
  slots <- reception_slots(team, k)
  if (is.na(sp) || is.null(slots)) return(NULL)
  list(slots = slots, template = RECEPTION_TEMPLATES[[as.character(sp)]])
}

#' 型からレセプションの立ち位置を作る
template_placement <- function(rt) {
  pl <- default_placement()
  for (slot in names(rt$slots)) {
    xy <- rt$template$pos[[slot]]
    pl[pl$pos == rt$slots[[slot]], c("x", "y")] <- xy
  }
  pl
}
