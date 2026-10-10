# 小さな .dvw を作って読み込む(実際の DataVolley の書式に合わせたもの)
make_dvw <- function(path, encoding = "UTF-8") {
  scout_line <- function(code, set, hs, vs, hp, vp) {
    paste0(code, ";;;;;;;", "10.00.00;", set, ";", hs, ";", vs, ";;;;",
           paste(hp, collapse = ";"), ";", paste(vp, collapse = ";"), ";")
  }
  hp <- c(6, 15, 4, 12, 10, 9); vp <- c(11, 15, 10, 13, 3, 16)
  lines <- c(
    "[3DATAVOLLEYSCOUT]", "FILEFORMAT: 2.0",
    "[3TEAMS]", "H1;山田クラブ;0;;;", "V1;Visitors;0;;;",
    "[3PLAYERS-H]",
    "0;1;1;*;;;;;H1;田中;花子;;L;1;False;;;",
    "0;4;2;3;;;;;H4;佐藤;一;;;5;False;;;",
    "0;6;3;1;;;;;H6;鈴木;二;;;2;False;;;",
    "0;9;4;6;;;;;H9;高橋;三;;;3;False;;;",
    "0;10;5;5;;;;;H10;伊藤;四;;;4;False;;;",
    "0;12;6;4;;;;;H12;渡辺;五;;;2;False;;;",
    "0;15;7;2;;;;;H15;中村;六;;;4;False;;;",
    "[3PLAYERS-V]",
    "1;3;1;5;;;;;V3;A;B;;;;False;;;",
    "1;6;2;*;;;;;V6;C;D;;L;;False;;;",
    "1;10;3;3;;;;;V10;E;F;;;;False;;;",
    "1;11;4;1;;;;;V11;G;H;;;;False;;;",
    "1;13;5;4;;;;;V13;I;J;;;;False;;;",
    "1;15;6;2;;;;;V15;K;L;;;;False;;;",
    "1;16;7;6;;;;;V16;M;N;;;;False;;;",
    "[3SCOUT]",
    scout_line("*P04>LUp", 1, 3, 3, hp, vp),
    scout_line("aP10>LUp", 1, 3, 3, hp, vp),
    scout_line("*06SM+", 1, 3, 3, hp, vp),         # ホームのサーブ → ホームの得点(ブレイク)
    scout_line("a11RM-", 1, 3, 3, hp, vp),
    scout_line("*p01:00", 1, 3, 3, hp, vp),
    scout_line("*06SM-", 1, 3, 3, hp, vp),         # ホームのサーブ → ビジターの得点(サイドアウト)
    scout_line("a13RM#", 1, 3, 3, hp, vp),
    scout_line("a10EQ#", 1, 3, 3, hp, vp),
    scout_line("a15AQ#X1~36", 1, 3, 3, hp, vp),
    scout_line("ap01:01", 1, 3, 2, hp, vp),
    scout_line("a15SM=", 1, 3, 2, hp, vp),         # ビジターのサーブミス → ホームのサイドアウト
    scout_line("*p02:01", 1, 3, 2, hp, vp)
  )
  con <- file(path, open = "w", encoding = encoding)
  writeLines(lines, con)
  close(con)
  path
}

