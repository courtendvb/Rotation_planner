# マッチアップ分析(対面別の実績と、スタートローテーションの検討) -------------------------

mod_matchup_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      3,
      div(
        class = "rp-panel",
        radioButtons(ns("first_server"), "最初のサーブ", c("自チーム" = "us", "相手" = "them"), inline = TRUE),
        radioButtons(ns("target"), "セット", c("25点" = 25, "15点(第5セット)" = 15), inline = TRUE),
        uiOutput(ns("them_starts")),
        p(class = "rp-hint",
          "見込みはデータタブで読み込んだ .dvw の実績(対面ごと)から計算します。",
          "実績の少ない対面は、ローテーション単位・チーム全体の実績に寄せています。"),
        p(class = "rp-hint",
          "対面はスタートの組み合わせと最初のサーブで決まり、セット中は変わりません。",
          "相手が来そうなスタートだけにチェックすると、その中での期待値で順位をつけます。")
      )
    ),
    column(
      9,
      div(
        class = "rp-panel",
        uiOutput(ns("no_data")),
        h4("対面別の実績(行 = 自チームのローテーション、列 = 相手のローテーション)"),
        fluidRow(
          column(6, h5("自チームレセプション時のサイドアウト"), uiOutput(ns("record_receive"))),
          column(6, h5("自チームサーブ時のブレイク"), uiOutput(ns("record_serve")))
        ),
        hr(),
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
          column(5, uiOutput(ns("court")))
        )
      )
    )
  )
}

#' teams: list(us, them) の reactiveVal。stats / has_data / prior_n: 対面別の実績(データタブ)
#' prob : 見込みを外から渡す場合の reactive(rally_prob_from_stats() の形)。NULL なら stats から作る
mod_matchup_server <- function(id, teams, placements = reactive(list()), stats = reactive(empty_matchup_stats()),
                               has_data = reactive(FALSE), prior_n = reactive(10), prob = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    us <- reactive(teams()$us)
    them <- reactive(teams()$them)
    target <- reactive(as.integer(input$target))

    rot_choices <- function(team) setNames(0:5, vapply(0:5, function(i) rotation_label(team, i), ""))

    output$no_data <- renderUI({
      if (!has_data()) div(class = "alert alert-info", "データタブで .dvw を読み込むと、実績と見込みを表示します。")
    })

    # 対面別の実績表:セルは「取った数/ラリー数」、色は実績の率
    record_table <- function(server) {
      ms <- stats()
      m <- ms[ms$server == server, ]
      baseline <- if (server == "us") 1 - DEFAULT_SIDE_OUT else DEFAULT_SIDE_OUT
      header <- tags$tr(tags$th(""), lapply(0:5, function(j) tags$th(rotation_label(them(), j))), tags$th("計"))
      rows <- lapply(0:5, function(i) {
        cells <- lapply(0:5, function(j) {
          cell <- m[m$us == i & m$them == j, ]
          if (cell$n == 0) return(tags$td(class = "rp-empty", "—"))
          tags$td(style = sprintf("background:%s", prob_color(cell$won / cell$n - baseline + 0.5)),
                  sprintf("%d/%d", as.integer(cell$won), as.integer(cell$n)))
        })
        row <- m[m$us == i, ]
        total <- if (sum(row$n) == 0) "—" else sprintf("%s", pct(sum(row$won) / sum(row$n), 0))
        tags$tr(tags$th(rotation_label(us(), i)), cells, tags$td(class = "rp-total-cell", total))
      })
      tags$table(class = "rp-matrix rp-record", header, rows)
    }
    output$record_receive <- renderUI(record_table("them"))
    output$record_serve <- renderUI(record_table("us"))

    output$them_starts <- renderUI({
      checkboxGroupInput(ns("them_likely"), "相手が来そうなスタート", rot_choices(them()),
                         selected = isolate(input$them_likely) %||% 0:5, inline = TRUE)
    })
    weights <- reactive({
      sel <- as.integer(input$them_likely %||% 0:5)
      as.numeric(0:5 %in% sel)
    })

    if (is.null(prob)) prob <- reactive(rally_prob_from_stats(stats(), prior_n()))
    mat <- reactive(start_matrix(prob(), input$first_server, target()))
    ranking <- reactive(rank_starts(mat(), weights()))

    output$ranking <- renderTable({
      req(has_data())
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
      req(has_data())
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
      best <- if (has_data()) ranking()$us_start[1] else 0
      selectInput(ns("d_us"), "自チームのスタート", rot_choices(us()), selected = best)
    })
    output$detail_them <- renderUI({
      selectInput(ns("d_them"), "相手のスタート", rot_choices(them()),
                  selected = isolate(input$d_them) %||% (which.max(weights()) - 1))
    })

    pairings <- reactive({
      req(input$d_us, input$d_them)
      pr <- pairings_for_set(as.integer(input$d_us), as.integer(input$d_them), input$first_server)
      p <- prob()
      ms <- stats()
      pr$p_us <- p[cbind(pr$us + 1, pr$them + 1, match(pr$server, SIDES))]
      rec <- ms[match(paste(pr$us, pr$them, pr$server), paste(ms$us, ms$them, ms$server)), ]
      pr$n <- rec$n
      pr$won <- rec$won
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
        `実績(自チームが取った数)` = ifelse(pr$n == 0, "—", sprintf("%d/%d", as.integer(pr$won), as.integer(pr$n))),
        `見込み` = if (has_data()) pct(pr$p_us) else "—",
        check.names = FALSE
      )
      DT::datatable(shown, rownames = FALSE, selection = list(mode = "single", selected = 1),
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
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
  })
}
