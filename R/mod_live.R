# 試合中モード ------------------------------------------------------------------
#
# 操作は「自チーム得点」「相手得点」「取り消し」の3つだけ。
# キーボードでも A / L / Z で入力できる。記録は自動保存され、再起動しても続きから使える。

mod_live_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("body"))
}

#' teams: list(us, them) の reactiveVal。
#' 戻り値: これまでの全ラリー(終了したセット + 進行中のセット)の data.frame を返す reactive
mod_live_server <- function(id, teams) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    us <- reactive(teams()$us)
    them <- reactive(teams()$them)
    current <- stored_reactive_val("live_set", NULL)
    history <- stored_reactive_val("history", list())
    state <- reactive({
      req(current())
      replay_set(current())
    })

    rot_choices <- function(team) setNames(0:5, vapply(0:5, function(i) rotation_label(team, i), ""))

    # 画面の切り替えはセットの開始・終了のときだけ(ラリーごとに全体を描き直さない)
    live_mode <- reactiveVal(FALSE)
    observe(live_mode(!is.null(current())))
    output$body <- renderUI({
      if (live_mode()) live_ui() else setup_ui()
    })

    setup_ui <- function() {
      set_no <- length(isolate(history())) + 1
      div(
        class = "rp-panel",
        h3(sprintf("第%dセットの開始", set_no)),
        fluidRow(
          column(3, selectInput(ns("us_start"), "自チームのスタート", rot_choices(isolate(us())))),
          column(3, selectInput(ns("them_start"), "相手のスタート", rot_choices(isolate(them())))),
          column(3, radioButtons(ns("first_server"), "最初のサーブ", c("自チーム" = "us", "相手" = "them"), inline = TRUE)),
          column(3, radioButtons(ns("target"), "セット", c("25点" = 25, "15点" = 15),
                                 selected = if (set_no == 5) 15 else 25, inline = TRUE))
        ),
        actionButton(ns("start"), "セット開始", class = "btn-primary btn-lg"),
        if (length(isolate(history())) > 0) tagList(
          hr(),
          actionButton(ns("reset_match"), "試合の記録をすべて消す", class = "btn-link")
        )
      )
    }

    live_ui <- function() {
      fluidRow(
        div(class = "rp-live-keys", `data-input` = ns("key")),
        column(
          6,
          div(
            class = "rp-panel",
            uiOutput(ns("score")),
            div(
              class = "rp-live-buttons",
              actionButton(ns("point_us"), tagList("自チーム得点", span(class = "rp-kbd", "A")), class = "btn-us"),
              actionButton(ns("point_them"), tagList("相手得点", span(class = "rp-kbd", "L")), class = "btn-them")
            ),
            actionButton(ns("undo"), tagList("取り消し", span(class = "rp-kbd", "Z"))),
            actionButton(ns("end_set"), "セットを終了して保存", class = "pull-right"),
            uiOutput(ns("now"))
          )
        ),
        column(
          6,
          div(class = "rp-panel", uiOutput(ns("court")), h4("ローテーション別(この試合)"), tableOutput(ns("stats")))
        )
      )
    }

    observeEvent(input$start, {
      current(new_set(as.integer(input$us_start), as.integer(input$them_start),
                      input$first_server, as.integer(input$target)))
    })

    rally <- function(winner) {
      req(current())
      current(add_rally(current(), winner))
    }
    observeEvent(input$point_us, rally("us"))
    observeEvent(input$point_them, rally("them"))
    observeEvent(input$undo, current(undo_rally(req(current()))))
    observeEvent(input$key, {
      switch(input$key,
             us = rally("us"),
             them = rally("them"),
             undo = current(undo_rally(req(current()))))
    })

    observeEvent(input$end_set, {
      showModal(modalDialog(
        "このセットの記録を保存して、次のセットの設定に戻ります。",
        footer = tagList(modalButton("キャンセル"), actionButton(ns("end_set_ok"), "保存して終了", class = "btn-primary"))
      ))
    })
    observeEvent(input$end_set_ok, {
      removeModal()
      history(c(history(), list(current())))
      current(NULL)
    })
    observeEvent(input$reset_match, {
      showModal(modalDialog(
        "これまでのセットの記録をすべて消します。元に戻せません。",
        footer = tagList(modalButton("キャンセル"), actionButton(ns("reset_ok"), "消す", class = "btn-danger"))
      ))
    })
    observeEvent(input$reset_ok, {
      removeModal()
      history(list())
    })

    output$score <- renderUI({
      s <- state()
      side <- function(name, score, serving) {
        div(div(class = paste("team", if (serving) "serving"), name), div(score))
      }
      tagList(
        div(class = "rp-score",
            side(us()$name, s$us_score, s$server == "us"),
            div("-"),
            side(them()$name, s$them_score, s$server == "them")),
        if (!is.na(s$winner)) p(class = "rp-ok", style = "text-align:center",
                                sprintf("セット終了(%s の勝ち)", if (s$winner == "us") us()$name else them()$name))
      )
    })

    output$now <- renderUI({
      s <- state()
      ev <- evaluate_pairing(us(), them(), s$us_rot, s$them_rot, s$server)
      # 次にサイドアウトが起きたときの対面
      next_us <- if (s$server == "us") s$us_rot else (s$us_rot + 1) %% 6
      next_them <- if (s$server == "them") s$them_rot else (s$them_rot + 1) %% 6
      next_server <- if (s$server == "us") "them" else "us"
      tags$dl(
        class = "rp-facts", style = "margin-top:16px",
        tags$dt("いまの対面"),
        tags$dd(sprintf("%s vs %s(%sサーブ)", rotation_label(us(), s$us_rot), rotation_label(them(), s$them_rot),
                        if (s$server == "us") "自チーム" else "相手")),
        tags$dt("このラリーを取る見込み(能力値)"),
        tags$dd(pct(ev$p_us)),
        tags$dt("サイドアウトされた後の対面"),
        tags$dd(sprintf("%s vs %s(%sサーブ)", rotation_label(us(), next_us), rotation_label(them(), next_them),
                        if (next_server == "us") "自チーム" else "相手"))
      )
    })

    output$court <- renderUI({
      s <- state()
      court_svg(team_markers(us(), s$us_rot, s$server == "us"),
                opponent = team_markers(them(), s$them_rot, s$server == "them"), max_width = 360)
    })

    all_logs <- reactive({
      sets <- c(history(), if (!is.null(current())) list(current()))
      logs <- lapply(sets, function(r) replay_set(r)$log)
      if (length(logs) == 0) return(replay_set(new_set(0, 0, "us"))$log)
      do.call(rbind, logs)
    })

    output$stats <- renderTable({
      st <- rotation_stats(all_logs())
      rate <- function(won, n) ifelse(n == 0, "—", sprintf("%s (%d/%d)", pct(won / n, 0), won, n))
      data.frame(
        `ローテ` = vapply(st$rot, function(i) rotation_label(us(), i), ""),
        `サイドアウト率` = rate(st$side_out, st$receive),
        `ブレイク率` = rate(st$break_point, st$serve),
        check.names = FALSE
      )
    }, striped = TRUE)

    all_logs
  })
}
