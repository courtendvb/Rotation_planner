# 試合中のアプリへの組み込み(既存の Shiny アプリにタブとして足す) -----------------------
#
# 既存のアプリ(page_navbar など)に、次の2つのタブを足して使う。
#   UI     : in_match_board_ui("rotation")    … 12面ボード(このセットの対面)
#            in_match_matchup_ui("rotation")  … 対面別の実績と、スタートの検討
#   server : in_match_server("rotation", dvw_path = fname)
#
# .dvw は自分で読み直す(更新時刻とサイズを見て、変わったときだけ)。
# 選んだセットのスタートの並びと最初のサーブは .dvw から自動で入る。
# 画面の状態(ドラッグした立ち位置、S1 のパターンなど)はファイルに保存せず、
# 見ている人(ブラウザ)ごとにメモリ上で持つので、ベンチの複数の PC で操作しても干渉しない。

#' コート図の CSS と JavaScript(どのページに置いても読み込まれるようにする)
rp_dependency <- function() {
  htmltools::htmlDependency(
    name = "rotationplanner", version = "0.1.0", src = c(file = app_www_dir()),
    script = "rotationplanner.js", stylesheet = "rotationplanner.css"
  )
}

#' 12面ボードのタブ
in_match_board_ui <- function(id) {
  ns <- NS(id)
  tagList(
    rp_dependency(),
    div(
      class = "rp-in-match",
      mod_rotation_ui(ns("board"), extra_controls = tagList(
        uiOutput(ns("our_side"), inline = TRUE), uiOutput(ns("set_choice"), inline = TRUE),
        uiOutput(ns("status"), inline = TRUE)
      ))
    )
  )
}

#' 対面別の実績と、スタートの検討のタブ
in_match_matchup_ui <- function(id) {
  ns <- NS(id)
  tagList(rp_dependency(), mod_matchup_ui(ns("matchup")))
}

#' 試合中のアプリ用の server。
#'   dvw_path: .dvw のパス(文字列、または文字列を返す reactive)
#'   poll_ms : ファイルの更新を確かめる間隔(ミリ秒)
#'   prior_n : 実績の少ない対面の補正(何本で半分信頼するか)
in_match_server <- function(id, dvw_path, poll_ms = 3000, prior_n = 10) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    path <- if (is.reactive(dvw_path)) dvw_path else reactive(dvw_path)

    dvw <- reactivePoll(
      poll_ms, session,
      checkFunc = function() {
        p <- path()
        if (is.null(p) || !file.exists(p)) return(NULL)
        c(file.mtime(p), file.size(p))
      },
      valueFunc = function() {
        p <- path()
        if (is.null(p) || !file.exists(p)) return(NULL)
        tryCatch(read_dvw(p), error = function(e) NULL)
      }
    )
    # 値が変わったときだけ更新する(ファイルが更新されるたびに選択欄を描き直さないように)
    stable <- function(expr_fn, initial) {
      rv <- reactiveVal(initial)
      observe({
        v <- expr_fn()
        if (!identical(v, isolate(rv()))) rv(v)
      })
      rv
    }
    sets <- stable(function() {
      d <- dvw()
      if (is.null(d) || nrow(d$rallies) == 0) 1L else as.integer(sort(unique(d$rallies$set)))
    }, 1L)
    our_side <- reactive(input$our_side %||% "home")

    output$our_side <- renderUI({
      d <- dvw()
      names <- if (is.null(d)) c(home = "ホーム", visiting = "ビジター") else d$teams
      radioButtons(ns("our_side"), "自チーム", setNames(DVW_SIDES, names[DVW_SIDES]),
                   selected = isolate(input$our_side) %||% "home", inline = TRUE)
    })

    # セットは最新を表示する(新しいセットが始まったら自動で移る。過去のセットを選んでいる間はそのまま)
    latest_seen <- reactiveVal(NA_integer_)
    output$set_choice <- renderUI({
      s <- sets()
      latest <- max(s)
      current <- isolate(input$set)
      follow <- is.null(current) || is.na(isolate(latest_seen())) || as.integer(current) == isolate(latest_seen())
      latest_seen(latest)
      selectInput(ns("set"), "セット", setNames(s, paste0("第", s, "セット")),
                  selected = if (follow) latest else current, width = "140px")
    })
    set <- reactive(as.integer(input$set %||% max(sets())))

    output$status <- renderUI({
      d <- dvw()
      if (is.null(d)) return(div(class = "rp-errors-text", "DataVolley のファイルを読み込めません"))
      div(class = "rp-hint", sprintf("ラリー %d / 最終更新 %s", nrow(d$rallies), format(d$mtime, "%H:%M:%S")))
    })

    # チーム:選んだセットのスタートの並び(ローテ 0 = そのセットのスタート)
    teams <- reactiveVal(sample_teams())
    observe({
      d <- dvw()
      req(d)
      side <- our_side()
      s <- set()
      cur <- isolate(teams())
      us <- dvw_team(d, side, team_id = "us", set = s)
      them <- dvw_team(d, setdiff(DVW_SIDES, side), team_id = "them", set = s)
      us$reception_variants <- cur$us$reception_variants
      them$reception_variants <- cur$them$reception_variants
      new <- list(us = us, them = them)
      if (!identical(new, cur)) teams(new)
    })

    # このセットの最初のサーブ(まだサーブがなければ自チーム)
    start <- stable(function() {
      d <- dvw()
      first <- "us"
      if (!is.null(d)) {
        r <- d$rallies[d$rallies$set == set(), ]
        if (nrow(r) > 0) first <- if (r$server[1] == our_side()) "us" else "them"
      }
      list(us_start = 0L, them_start = 0L, first = first)
    }, list(us_start = 0L, them_start = 0L, first = "us"))

    placements <- reactiveVal(list())
    stats <- reactive({
      d <- dvw()
      if (is.null(d)) return(empty_matchup_stats())
      matchup_stats(d$rallies, our_side(), teams()$us, teams()$them)
    })
    has_data <- reactive(sum(stats()$n) > 0)
    prior <- reactive(prior_n)

    mod_rotation_server("board", teams, placements, stats, has_data, prior, start = start)
    mod_matchup_server("matchup", teams, placements, stats, has_data, prior)
  })
}
