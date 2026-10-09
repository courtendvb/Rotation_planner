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
