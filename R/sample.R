# サンプルデータ(初回起動時に使う 5-1 システムの2チーム) ----------------------

make_players <- function(prefix, numbers, names, roles) {
  data.frame(id = paste0(prefix, "-", numbers), number = as.integer(numbers), name = names, role = roles)
}

sample_teams <- function() {
  names <- c("セッター", "レフト1", "ミドル1", "オポジット", "レフト2", "ミドル2", "リベロ")
  roles <- c("S", "OH", "MB", "OP", "OH", "MB", "L")
  list(
    us = list(id = "us", name = "自チーム", players = make_players("us", 1:7, names, roles),
              lineup = paste0("us-", 1:6), libero = "us-7"),
    them = list(id = "them", name = "相手チーム", players = make_players("them", 10:16, names, roles),
                lineup = paste0("them-", 10:15), libero = "them-16")
  )
}

#' 保存されていたチームを今の形にそろえる(以前の版の能力値の列などを落とす)
normalize_team <- function(team, fallback) {
  if (!is.list(team) || is.null(team$players) || !all(c("id", "number", "name", "role") %in% names(team$players))) {
    return(fallback)
  }
  team$players <- team$players[, c("id", "number", "name", "role"), drop = FALSE]
  if (is.null(team$libero)) team$libero <- NA_character_
  team
}
