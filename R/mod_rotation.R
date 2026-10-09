# ローテーション・ビューア(6ローテーションを一覧で確認・編集) -------------------

mod_rotation_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "rp-panel",
    fluidRow(
      column(4, radioButtons(ns("team"), NULL, c("自チーム" = "us", "相手チーム" = "them"), inline = TRUE)),
      column(4, radioButtons(ns("serving"), NULL,
                             c("レシーブ(レセプション)" = "receive", "サーブ" = "serve"), inline = TRUE)),
      column(4, uiOutput(ns("summary")))
    ),
    uiOutput(ns("grid")),
    p(class = "rp-hint",
      "初期表示は役割から自動で作った標準のフォーメーションです。",
      "選手をドラッグすると立ち位置を変更でき、自動で保存されます。赤枠はポジショナルフォルトです。")
  )
}

#' teams: list(us, them) の reactiveVal。placements: 立ち位置を保存する reactiveVal
mod_rotation_server <- function(id, teams, placements) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    team <- reactive(teams()[[input$team %||% "us"]])
    serving <- reactive(identical(input$serving, "serve"))

    # ローテーションごとの立ち位置と反則
    courts <- reactive({
      t <- team()
      lapply(0:5, function(k) {
        pl <- placement_for(t, k, serving(), placements())
        list(k = k, placement = pl, violations = check_overlap(pl, serving()),
             custom = !is.null(placements()[[placement_key(t, k, serving())]]))
      })
    })

    observeEvent(input$move, {
      m <- input$move
      k <- as.integer(m$court)
      t <- team()
      pl <- placement_for(t, k, serving(), placements())
      pl[pl$pos == m$pos, c("x", "y")] <- c(m$x, m$y)
      all <- placements()
      all[[placement_key(t, k, serving())]] <- pl
      placements(all)
    })

    lapply(0:5, function(k) {
      observeEvent(input[[paste0("reset_", k)]], {
        all <- placements()
        all[[placement_key(team(), k, serving())]] <- NULL
        placements(all)
      })
    })
    observeEvent(input$reset_all, {
      all <- placements()
      for (k in 0:5) all[[placement_key(team(), k, serving())]] <- NULL
      placements(all)
    })

    output$summary <- renderUI({
      cs <- courts()
      n_bad <- sum(vapply(cs, function(c) nrow(c$violations) > 0, logical(1)))
      n_custom <- sum(vapply(cs, function(c) c$custom, logical(1)))
      tagList(
        if (n_bad == 0) span(class = "rp-ok", "6ローテーションすべて反則なし")
        else span(class = "rp-errors", sprintf("%d つのローテーションに反則があります", n_bad)),
        if (n_custom > 0) actionLink(ns("reset_all"), "すべて標準のフォーメーションに戻す", style = "margin-left:12px")
      )
    })

    output$grid <- renderUI({
      t <- team()
      cards <- lapply(courts(), function(c) rotation_card(ns, t, c, serving()))
      tagList(fluidRow(cards[1:3]), fluidRow(cards[4:6]))
    })
  })
}

rotation_card <- function(ns, team, court, serving) {
  k <- court$k
  v <- court$violations
  front <- front_row_after_switch(team, k)
  fmt <- function(p) if (nrow(p) == 1) sprintf("#%d", p$number) else "—"
  srv <- server_of(team, k)
  markers <- team_markers(team, k, serving, court$placement, warn = unique(c(v$a, v$b)))

  column(
    4,
    div(
      class = paste("rp-rot-card", if (nrow(v) > 0) "bad"),
      div(
        class = "rp-rot-head",
        span(class = "rp-rot-label", rotation_label(team, k)),
        span(class = "rp-rot-meta",
             sprintf("前衛 %d 枚 / サーバー %s", front_attacker_count(team, k), fmt(srv))),
        if (court$custom) span(class = "rp-rot-custom", "編集済み")
      ),
      court_svg(markers, drag_input = ns("move"), court_key = k, max_width = 340),
      div(
        class = "rp-rot-foot",
        if (nrow(v) == 0) span(class = "rp-ok", "反則なし")
        else tags$ul(class = "rp-errors", lapply(v$message, tags$li)),
        div(class = "rp-hint",
            sprintf("スイッチ後 L / C / R:%s / %s / %s", fmt(front$left), fmt(front$middle), fmt(front$right))),
        if (court$custom) actionLink(ns(paste0("reset_", k)), "標準に戻す")
      )
    )
  )
}
