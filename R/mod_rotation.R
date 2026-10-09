# ローテーション対面ボード ---------------------------------------------------------
#
# スタートの組み合わせと最初のサーブを1回設定すると、そのセットで起きる12通りの対面を
#   上段:自チームサーブ時(ブレイクフェイズ)の6ローテーション
#   下段:自チームレセプション時(サイドアウトフェイズ)の6ローテーション
# として1画面に並べる。列は自チームのローテーションでそろえる。

ROLE_LEGEND <- c(S = "セッター", OH = "アウトサイド", MB = "ミドル", OP = "オポジット", L = "リベロ")

mod_rotation_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "rp-panel",
    div(
      class = "rp-board-controls",
      uiOutput(ns("controls"), inline = TRUE),
      uiOutput(ns("summary"), inline = TRUE)
    ),
    uiOutput(ns("board")),
    div(
      class = "rp-hint",
      span(class = "rp-legend", lapply(names(ROLE_LEGEND), function(r) {
        span(span(class = paste0("rp-dot role-", r)), ROLE_LEGEND[[r]])
      })),
      "下が自チーム、上が相手。初期表示は役割から作った標準のフォーメーションで、",
      "選手をドラッグすると立ち位置を変更・保存できます(両チームとも)。赤枠はポジショナルフォルトです。"
    )
  )
}

#' teams: list(us, them) の reactiveVal。placements: 立ち位置を保存する reactiveVal
mod_rotation_server <- function(id, teams, placements) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    us <- reactive(teams()$us)
    them <- reactive(teams()$them)

    rot_choices <- function(team) setNames(0:5, vapply(0:5, function(i) rotation_label(team, i), ""))

    output$controls <- renderUI({
      div(
        class = "rp-inline-inputs",
        selectInput(ns("us_start"), "自チームのスタート", rot_choices(us()),
                    selected = isolate(input$us_start) %||% 0, width = "150px"),
        selectInput(ns("them_start"), "相手のスタート", rot_choices(them()),
                    selected = isolate(input$them_start) %||% 0, width = "150px"),
        radioButtons(ns("first_server"), "最初のサーブ", c("自チーム" = "us", "相手" = "them"),
                     selected = isolate(input$first_server) %||% "us", inline = TRUE)
      )
    })

    setup <- reactive({
      req(input$us_start, input$them_start, input$first_server)
      list(us_start = as.integer(input$us_start), them_start = as.integer(input$them_start),
           first = input$first_server)
    })
    prob <- reactive(rally_prob_array(us(), them()))

    # 12 の対面と、それぞれの立ち位置・反則・ラリー取得率
    boards <- reactive({
      st <- setup()
      pr <- pairings_for_set(st$us_start, st$them_start, st$first)
      p <- prob()
      pr$a <- rep(0:5, each = 2)
      lapply(seq_len(nrow(pr)), function(i) {
        row <- pr[i, ]
        us_srv <- row$server == "us"
        us_pl <- placement_for(us(), row$us, us_srv, placements())
        them_pl <- placement_for(them(), row$them, !us_srv, placements())
        list(
          a = row$a, us = row$us, them = row$them, us_serving = us_srv,
          p_us = p[row$us + 1, row$them + 1, match(row$server, SIDES)],
          us_pl = us_pl, them_pl = them_pl,
          us_v = check_overlap(us_pl, us_srv), them_v = check_overlap(them_pl, !us_srv),
          custom = !is.null(placements()[[placement_key(us(), row$us, us_srv)]]) ||
            !is.null(placements()[[placement_key(them(), row$them, !us_srv)]])
        )
      })
    })

    observeEvent(input$move, {
      m <- input$move
      req(m$side %in% SIDES, !is.null(m$rot), m$phase %in% c("serve", "receive"))
      team <- teams()[[m$side]]
      serving <- m$phase == "serve"
      pl <- placement_for(team, m$rot, serving, placements())
      pl[pl$pos == m$pos, c("x", "y")] <- c(m$x, m$y)
      all <- placements()
      all[[placement_key(team, m$rot, serving)]] <- pl
      placements(all)
    })

    reset_board <- function(b) {
      all <- placements()
      all[[placement_key(us(), b$us, b$us_serving)]] <- NULL
      all[[placement_key(them(), b$them, !b$us_serving)]] <- NULL
      placements(all)
    }
    for (phase in c("serve", "receive")) for (a in 0:5) local({
      ph <- phase; aa <- a
      observeEvent(input[[sprintf("reset_%s_%d", ph, aa)]], {
        b <- Filter(function(b) b$a == aa && b$us_serving == (ph == "serve"), boards())[[1]]
        reset_board(b)
      })
    })
    observeEvent(input$reset_all, {
      all <- placements()
      for (b in boards()) {
        all[[placement_key(us(), b$us, b$us_serving)]] <- NULL
        all[[placement_key(them(), b$them, !b$us_serving)]] <- NULL
      }
      placements(all)
    })

    output$summary <- renderUI({
      st <- setup()
      bs <- boards()
      win <- set_win_probability(prob(), st$us_start, st$them_start, st$first)
      n_bad <- sum(vapply(bs, function(b) nrow(b$us_v) + nrow(b$them_v) > 0, logical(1)))
      div(
        class = "rp-board-summary",
        span("このスタートでのセット取得率(能力値)", strong(pct(win))),
        if (n_bad == 0) span(class = "rp-ok", "12面すべて反則なし")
        else span(class = "rp-errors-inline", sprintf("%d 面に反則があります", n_bad)),
        if (any(vapply(bs, `[[`, logical(1), "custom")))
          actionLink(ns("reset_all"), "すべて標準のフォーメーションに戻す")
      )
    })

    output$board <- renderUI({
      bs <- boards()
      serve <- Filter(function(b) b$us_serving, bs)
      receive <- Filter(function(b) !b$us_serving, bs)
      avg <- function(xs) mean(vapply(xs, `[[`, 0, "p_us"))
      tagList(
        board_row(ns, us(), them(), serve, "serve",
                  "自チームサーブ(ブレイクフェイズ)", sprintf("ブレイク見込み 平均 %s", pct(avg(serve)))),
        board_row(ns, us(), them(), receive, "receive",
                  "自チームレセプション(サイドアウトフェイズ)", sprintf("サイドアウト見込み 平均 %s", pct(avg(receive))))
      )
    })
  })
}

board_row <- function(ns, us, them, boards, phase, title, note) {
  div(
    class = "rp-board-row",
    div(class = "rp-board-title", strong(title), span(class = "rp-hint", note)),
    div(class = "rp-board", lapply(boards, function(b) board_card(ns, us, them, b, phase)))
  )
}

board_card <- function(ns, us, them, b, phase) {
  bad <- nrow(b$us_v) + nrow(b$them_v) > 0
  us_m <- team_markers(us, b$us, b$us_serving, b$us_pl, warn = unique(c(b$us_v$a, b$us_v$b)))
  them_m <- team_markers(them, b$them, !b$us_serving, b$them_pl, warn = unique(c(b$them_v$a, b$them_v$b)))
  violations <- c(if (nrow(b$us_v)) paste("自:", b$us_v$message), if (nrow(b$them_v)) paste("相:", b$them_v$message))

  div(
    class = paste("rp-board-card", if (bad) "bad"),
    div(
      class = "rp-board-head",
      span(class = "rp-board-match", strong(rotation_label(us, b$us)), " vs ", rotation_label(them, b$them)),
      span(class = "rp-pill", style = sprintf("background:%s", prob_color(b$p_us)), pct(b$p_us, 0))
    ),
    div(class = "rp-board-meta",
        sprintf("前衛 自 %d 枚 / 相手 %d 枚", front_attacker_count(us, b$us), front_attacker_count(them, b$them))),
    court_svg(us_m, opponent = them_m, drag_input = ns("move"), compact = TRUE, max_width = 2000,
              us_attrs = drag_attrs(b$us, b$us_serving), them_attrs = drag_attrs(b$them, !b$us_serving)),
    if (length(violations)) tags$ul(class = "rp-errors rp-board-errors", lapply(violations, tags$li)),
    if (b$custom) actionLink(ns(sprintf("reset_%s_%d", phase, b$a)), "標準に戻す", class = "rp-board-reset")
  )
}
