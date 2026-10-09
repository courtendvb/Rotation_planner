# ローテーション・ビューア(立ち位置の編集とポジショナルフォルト判定) -----------

mod_rotation_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      7,
      div(
        class = "rp-panel",
        fluidRow(
          column(6, radioButtons(ns("team"), NULL, c("自チーム" = "us", "相手チーム" = "them"), inline = TRUE)),
          column(6, radioButtons(ns("serving"), NULL, c("レシーブ" = "receive", "サーブ" = "serve"), inline = TRUE))
        ),
        uiOutput(ns("rot_buttons")),
        uiOutput(ns("court")),
        p(class = "rp-hint", "選手をドラッグして立ち位置を編集できます(自動保存)。")
      )
    ),
    column(5, div(class = "rp-panel", uiOutput(ns("side"))))
  )
}

#' teams: list(us, them) の reactiveVal。placements: 立ち位置を保存する reactiveVal
mod_rotation_server <- function(id, teams, placements) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    team <- reactive(teams()[[input$team]])
    k <- reactiveVal(0)
    serving <- reactive(identical(input$serving, "serve"))

    output$rot_buttons <- renderUI({
      t <- team()
      choices <- setNames(0:5, vapply(0:5, function(i) rotation_label(t, i), ""))
      radioButtons(ns("rot"), "ローテーション", choices, selected = isolate(k()), inline = TRUE)
    })
    observeEvent(input$rot, k(as.integer(input$rot)))

    key <- reactive(sprintf("%s:%d:%s", team()$id, k(), input$serving))
    placement <- reactive(placements()[[key()]] %||% default_placement())
    violations <- reactive(check_overlap(placement(), serving()))

    observeEvent(input$move, {
      m <- input$move
      pl <- placement()
      pl[pl$pos == m$pos, c("x", "y")] <- c(m$x, m$y)
      all <- placements()
      all[[key()]] <- pl
      placements(all)
    })
    observeEvent(input$reset, {
      all <- placements()
      all[[key()]] <- NULL
      placements(all)
    })

    output$court <- renderUI({
      v <- violations()
      markers <- team_markers(team(), k(), serving(), placement(), warn = unique(c(v$a, v$b)))
      court_svg(markers, drag_input = ns("move"), max_width = 480)
    })

    output$side <- renderUI({
      t <- team()
      fmt <- function(p) if (nrow(p) == 1) sprintf("#%d %s", p$number, p$role) else "—"
      front <- front_row_after_switch(t, k())
      v <- violations()
      tagList(
        h3(rotation_label(t, k())),
        tags$dl(
          class = "rp-facts",
          tags$dt("前衛アタッカー"), tags$dd(sprintf("%d 枚", front_attacker_count(t, k()))),
          tags$dt("サーバー"), tags$dd(fmt(server_of(t, k()))),
          tags$dt("スイッチ後(レフト / センター / ライト)"),
          tags$dd(paste(fmt(front$left), fmt(front$middle), fmt(front$right), sep = " / "))
        ),
        h4("ポジショナルフォルト"),
        if (nrow(v) == 0) p(class = "rp-ok", "反則なし")
        else tags$ul(class = "rp-errors", lapply(v$message, tags$li)),
        if (!is.null(placements()[[key()]])) actionButton(ns("reset"), "基本ポジションに戻す")
      )
    })
  })
}
