# マッチアップ分析(スタートローテーションの最適化) -----------------------------

mod_matchup_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      3,
      div(
        class = "rp-panel",
        radioButtons(ns("first_server"), "最初のサーブ", c("自チーム" = "us", "相手" = "them"), inline = TRUE),
        radioButtons(ns("target"), "セット", c("25点" = 25, "15点(第5セット)" = 15), inline = TRUE),
        radioButtons(ns("model"), "ラリー取得率の根拠",
                     c("能力値" = "rating", "能力値 + 試合の実績" = "stats")),
        uiOutput(ns("them_starts")),
        sliderInput(ns("base_so"), "基準サイドアウト率", min = 0.4, max = 0.8, value = 0.6, step = 0.01),
        p(class = "rp-hint",
          "対面はスタートの組み合わせと最初のサーブで決まり、セット中は変わりません。",
          "相手が来そうなスタートだけにチェックすると、その中での期待値で順位をつけます。")
      )
    ),
    column(
      9,
      div(
        class = "rp-panel",
        h4("おすすめのスタート"),
        tableOutput(ns("ranking")),
        h4("セット取得確率(行 = 自チームのスタート、列 = 相手のスタート)"),
        uiOutput(ns("matrix")),
        hr(),
        h4("対面の詳細"),
        fluidRow(
          column(4, uiOutput(ns("detail_us"))),
          column(4, uiOutput(ns("detail_them")))
        ),
        fluidRow(
          column(7, DT::DTOutput(ns("pairings"))),
          column(5, uiOutput(ns("court")), uiOutput(ns("duels")))
        )
      )
    )
  )
}

#' teams: list(us, them) の reactiveVal。logs: 試合の記録(ラリーの data.frame)を返す reactive
mod_matchup_server <- function(id, teams, logs, placements = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    us <- reactive(teams()$us)
    them <- reactive(teams()$them)
    settings <- reactive(eval_settings(base_side_out = input$base_so))
    target <- reactive(as.integer(input$target))

    rot_choices <- function(team) setNames(0:5, vapply(0:5, function(i) rotation_label(team, i), ""))

    output$them_starts <- renderUI({
      checkboxGroupInput(ns("them_likely"), "相手が来そうなスタート", rot_choices(them()),
                         selected = isolate(input$them_likely) %||% 0:5, inline = TRUE)
    })
    weights <- reactive({
      sel <- as.integer(input$them_likely %||% 0:5)
      as.numeric(0:5 %in% sel)
    })

    prob <- reactive({
      p <- rally_prob_array(us(), them(), settings())
      if (identical(input$model, "stats")) p <- blend_with_stats(p, rotation_stats(logs()))
      p
    })
    mat <- reactive(start_matrix(prob(), input$first_server, target()))
    ranking <- reactive(rank_starts(mat(), weights()))

    output$ranking <- renderTable({
      r <- ranking()
      data.frame(
        `順位` = seq_len(nrow(r)),
        `自チームのスタート` = vapply(r$us_start, function(i) rotation_label(us(), i), ""),
        `期待セット取得率` = pct(r$expected),
        `最悪の場合` = pct(r$worst),
        check.names = FALSE
      )
    }, striped = TRUE)

    output$matrix <- renderUI({
      m <- mat()
      best <- ranking()$us_start[1]
      w <- weights()
      header <- tags$tr(tags$th(""), lapply(0:5, function(j) {
        tags$th(rotation_label(them(), j), style = if (w[j + 1] == 0) "opacity:0.4")
      }))
      rows <- lapply(0:5, function(i) {
        tags$tr(
          tags$th(rotation_label(us(), i)),
          lapply(0:5, function(j) {
            v <- m[i + 1, j + 1]
            tags$td(pct(v), class = if (i == best) "best",
                    style = sprintf("background:%s;%s", prob_color(v), if (w[j + 1] == 0) "opacity:0.4" else ""))
          })
        )
      })
      tags$table(class = "rp-matrix", header, rows)
    })

    output$detail_us <- renderUI({
      selectInput(ns("d_us"), "自チームのスタート", rot_choices(us()), selected = ranking()$us_start[1])
    })
    output$detail_them <- renderUI({
      selectInput(ns("d_them"), "相手のスタート", rot_choices(them()),
                  selected = isolate(input$d_them) %||% (which.max(weights()) - 1))
    })

    pairings <- reactive({
      req(input$d_us, input$d_them)
      pr <- pairings_for_set(as.integer(input$d_us), as.integer(input$d_them), input$first_server)
      p <- prob()
      pr$p_us <- p[cbind(pr$us + 1, pr$them + 1, match(pr$server, SIDES))]
      pr$edge <- mapply(function(a, b, s) evaluate_pairing(us(), them(), a, b, s, settings())$edge,
                        pr$us, pr$them, pr$server)
      pr
    })

    output$pairings <- DT::renderDT({
      pr <- pairings()
      shown <- data.frame(
        `自チーム` = vapply(pr$us, function(i) rotation_label(us(), i), ""),
        `相手` = vapply(pr$them, function(i) rotation_label(them(), i), ""),
        `サーブ` = ifelse(pr$server == "us", "自チーム", "相手"),
        `前衛(自/相手)` = sprintf("%d / %d", vapply(pr$us, function(i) front_attacker_count(us(), i), 0),
                                  vapply(pr$them, function(i) front_attacker_count(them(), i), 0)),
        `評価点` = round(pr$edge, 1),
        `ラリー取得率` = pct(pr$p_us),
        check.names = FALSE
      )
      colors <- vapply(pr$p_us, prob_color, "")
      keep <- !duplicated(shown[["ラリー取得率"]])
      DT::datatable(shown, rownames = FALSE, selection = list(mode = "single", selected = 1),
                    options = list(dom = "t", paging = FALSE, ordering = FALSE)) |>
        DT::formatStyle("ラリー取得率", target = "cell",
                        backgroundColor = DT::styleEqual(shown[["ラリー取得率"]][keep], colors[keep]))
    })

    selected <- reactive({
      pr <- pairings()
      i <- input$pairings_rows_selected %||% 1
      pr[min(i, nrow(pr)), ]
    })

    output$court <- renderUI({
      s <- selected()
      court_svg(match_markers(us(), s$us, s$server == "us", placements()),
                opponent = match_markers(them(), s$them, s$server == "them", placements()), max_width = 300)
    })

    output$duels <- renderUI({
      s <- selected()
      ev <- evaluate_pairing(us(), them(), s$us, s$them, s$server, settings())
      duel_rows <- function(d, label) {
        lapply(seq_len(nrow(d)), function(i) {
          tags$tr(tags$td(label), tags$td(d$zone[i]),
                  tags$td(sprintf("#%s vs #%s", d$attacker[i], d$blocker[i])),
                  tags$td(sprintf("%+.0f", d$diff[i])))
        })
      }
      tagList(
        tags$table(
          class = "table table-condensed",
          tags$tr(tags$th(""), tags$th("ゾーン"), tags$th("攻撃 vs ブロック"), tags$th("差")),
          duel_rows(ev$our_attack, "自チーム攻撃"),
          duel_rows(ev$their_attack, "相手攻撃")
        ),
        p(sprintf("サーブ vs レセプション: %+.1f(自チーム目線)", ev$serve_diff))
      )
    })
  })
}
