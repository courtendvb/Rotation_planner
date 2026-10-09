# サンプルデータ(初回起動時に使う 5-1 システムの2チーム) ----------------------

make_players <- function(prefix, numbers, names, roles, ratings) {
  r <- do.call(rbind, ratings)
  data.frame(
    id = paste0(prefix, "-", numbers),
    number = as.integer(numbers),
    name = names,
    role = roles,
    attack = r[, 1], block = r[, 2], serve = r[, 3], receive = r[, 4]
  )
}

sample_teams <- function() {
  names <- c("セッター", "レフト1", "ミドル1", "オポジット", "レフト2", "ミドル2", "リベロ")
  roles <- c("S", "OH", "MB", "OP", "OH", "MB", "L")
  us <- list(
    id = "us", name = "自チーム",
    players = make_players("us", 1:7, names, roles, list(
      c(3, 4, 6, 4), c(8, 5, 6, 7), c(7, 8, 5, 3), c(8, 6, 7, 4),
      c(6, 5, 5, 8), c(6, 7, 6, 3), c(1, 1, 1, 9)
    )),
    lineup = paste0("us-", 1:6), libero = "us-7"
  )
  them <- list(
    id = "them", name = "相手チーム",
    players = make_players("them", 10:16, names, roles, list(
      c(3, 5, 5, 4), c(7, 5, 7, 6), c(6, 9, 5, 3), c(9, 6, 6, 4),
      c(6, 6, 5, 7), c(7, 6, 6, 3), c(1, 1, 1, 8)
    )),
    lineup = paste0("them-", 10:15), libero = "them-16"
  )
  list(us = us, them = them)
}
