# マッチアップ(対面)の計算と評価 --------------------------------------------

SIDES <- c("us", "them")

#' 1セットの間に実際に起きる対面(12通り)。
#' サイドアウトのたびにサーブ権を得たチームだけが回るので、両チームの回転数の差は常に 0 か 1。
#' したがって対面はスタートローテーションと最初のサーブ権だけで決まる。
#' 戻り値は data.frame(us, them, server)。us/them はローテーション(0〜5)
pairings_for_set <- function(us_start, them_start, first_server) {
  a <- 0:5  # 自チームの回転数
  b_us_serve <- if (first_server == "us") a else a - 1
  b_them_serve <- if (first_server == "us") a + 1 else a
  us <- (us_start + a) %% 6
  rbind(
    data.frame(us = us, them = (them_start + b_us_serve) %% 6, server = "us"),
    data.frame(us = us, them = (them_start + b_them_serve) %% 6, server = "them")
  )[order(rep(0:5, 2), rep(1:2, each = 6)), ] |> `rownames<-`(NULL)
}

#' 評価の設定
#'   base_side_out : 互角の対面でのレシーブ側のサイドアウト率
#'   net_weight    : 前衛の攻撃 vs ブロックの重み
#'   serve_weight  : サーブ vs レセプションの重み
#'   scale         : 評価点 → 確率の変換係数(ロジスティック)
eval_settings <- function(base_side_out = 0.6, net_weight = 1, serve_weight = 1, scale = 0.06) {
  list(base_side_out = base_side_out, net_weight = net_weight, serve_weight = serve_weight, scale = scale)
}

DEFAULT_RATING <- 5

rating_of <- function(player, key) {
  if (nrow(player) == 0 || is.na(player[[key]])) DEFAULT_RATING else player[[key]]
}

#' レセプションに入る選手(OH とリベロ)の平均。いなければ全員の平均
receive_strength <- function(team) {
  pl <- team$players
  passers <- pl[pl$role %in% c("OH", "L"), , drop = FALSE]
  pool <- if (nrow(passers) > 0) passers else pl
  if (nrow(pool) == 0) DEFAULT_RATING else mean(pool$receive)
}

#' 前衛の対面。攻撃側のレフトは守備側のライトのブロックとぶつかる。
#' スイッチ後の配置(OH=レフト, MB=センター, S/OP=ライト)で比較する
net_duels <- function(attackers, a_rot, blockers, b_rot) {
  atk <- front_row_after_switch(attackers, a_rot)
  blk <- front_row_after_switch(blockers, b_rot)
  pairs <- list(
    list(zone = "レフト", a = atk$left, b = blk$right),
    list(zone = "センター", a = atk$middle, b = blk$middle),
    list(zone = "ライト", a = atk$right, b = blk$left)
  )
  do.call(rbind, lapply(pairs, function(p) {
    data.frame(
      zone = p$zone,
      attacker = if (nrow(p$a) == 1) p$a$number else NA_integer_,
      blocker = if (nrow(p$b) == 1) p$b$number else NA_integer_,
      diff = rating_of(p$a, "attack") - rating_of(p$b, "block")
    )
  }))
}

logit <- function(p) log(p / (1 - p))
sigmoid <- function(x) 1 / (1 + exp(-x))

#' 1つの対面を能力値で評価する。
#' 戻り値 list(edge = 自チーム目線の評価点, p_us = 自チームがラリーを取る確率,
#'             our_attack, their_attack, serve_diff)
evaluate_pairing <- function(us, them, us_rot, them_rot, server, settings = eval_settings()) {
  our_attack <- net_duels(us, us_rot, them, them_rot)
  their_attack <- net_duels(them, them_rot, us, us_rot)
  net <- sum(our_attack$diff) - sum(their_attack$diff)

  serve_diff <- if (server == "us") {
    rating_of(server_of(us, us_rot), "serve") - receive_strength(them)
  } else {
    -(rating_of(server_of(them, them_rot), "serve") - receive_strength(us))
  }

  edge <- settings$net_weight * net + settings$serve_weight * serve_diff
  # サーブ側は不利(サイドアウト率が基準)。そこから評価点でずらす
  base <- if (server == "us") 1 - settings$base_side_out else settings$base_side_out
  p_us <- sigmoid(logit(base) + settings$scale * edge)

  list(edge = edge, p_us = p_us, our_attack = our_attack, their_attack = their_attack, serve_diff = serve_diff)
}

#' 全対面(6 x 6 x サーブ権2)の「自チームがラリーを取る確率」。
#' 戻り値は array [us_rot + 1, them_rot + 1, server(1 = us, 2 = them)]
rally_prob_array <- function(us, them, settings = eval_settings()) {
  p <- array(NA_real_, dim = c(6, 6, 2), dimnames = list(us = 0:5, them = 0:5, server = SIDES))
  for (a in 0:5) for (b in 0:5) for (s in 1:2) {
    p[a + 1, b + 1, s] <- evaluate_pairing(us, them, a, b, SIDES[s], settings)$p_us
  }
  p
}

#' 実績(サイドアウト率・ブレイク率)で能力値モデルを置き換える。
#' stats は rotation_stats() の結果。試行数 min_n 未満のローテーションは元の値のまま
blend_with_stats <- function(p, stats, min_n = 5) {
  for (a in 0:5) {
    s <- stats[stats$rot == a, ]
    if (nrow(s) == 0) next
    if (s$receive >= min_n) p[a + 1, , "them"] <- s$side_out / s$receive
    if (s$serve >= min_n) p[a + 1, , "us"] <- s$break_point / s$serve
  }
  p
}
