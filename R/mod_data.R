# データ(DataVolley の読み込みと、計算に使う実績) ------------------------------------
#
# .dvw はファイルのパスを指定すると、更新されるたびに自動で読み直す(試合中に DataVolley が
# 保存したファイルをそのまま使える)。ファイルを選んで読み込むこともできる(この場合は1回だけ)。

DVW_POLL_MS <- 3000

mod_data_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(
      6,
      div(
        class = "rp-panel",
        h4("DataVolley(.dvw)"),
        textInput(ns("path"), "ファイルのパス(試合中に自動で読み直す)", width = "100%",
                  placeholder = "例: C:/DataVolley/2026-10-10 山田 vs 鈴木.dvw"),
        div(
          class = "rp-inline-inputs",
          actionButton(ns("load_path"), "読み込む", class = "btn-primary"),
          checkboxInput(ns("watch"), sprintf("更新されたら自動で読み直す(%d秒ごとに確認)", DVW_POLL_MS / 1000), TRUE)
        ),
        fileInput(ns("upload"), "またはファイルを選ぶ(1回だけ読み込み)", accept = ".dvw"),
        uiOutput(ns("status")),
        hr(),
        radioButtons(ns("our_side"), "自チームは", c("ホーム" = "home", "ビジター" = "visiting"), inline = TRUE),
        uiOutput(ns("set_choice")),
        actionButton(ns("import"), "メンバーを読み込む(両チーム)"),
        p(class = "rp-hint",
          "チーム名、選手(背番号・名前・役割)、スタートの並び、リベロを読み込みます。",
          "役割が入っていない選手は、セッターの宣言・プレー内容・並びから推定します。",
          "能力値は、背番号が同じ選手の値を引き継ぎます。")
      )
    ),
    column(
      6,
      div(
        class = "rp-panel",
        h4("サイドアウト率(実績)"),
        radioButtons(ns("source"), "マッチアップの計算に使う実績",
                     c("DataVolley(.dvw)" = "dvw", "試合タブの記録" = "live", "使わない(能力値のみ)" = "none")),
        sliderInput(ns("prior_n"), "能力値の重み(実績が何本たまったら半々にするか)",
                    min = 2, max = 40, value = 10, step = 1),
        uiOutput(ns("stats"))
      )
    )
  )
}

#' teams: list(us, them) の reactiveVal。logs: 試合タブの全ラリーを返す reactive。
#' 戻り値 list(actual = 実績 list(us, them) または NULL を返す reactive, prior_n = reactive)
mod_data_server <- function(id, teams, logs) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    settings <- stored_reactive_val("data_settings", list(path = "", our_side = "home", source = "dvw", prior_n = 10))
    dvw <- reactiveVal(NULL)
    watched <- reactiveVal(NULL)   # 自動で読み直すファイルのパス
    error <- reactiveVal(NULL)

    # 前回の設定を復元
    observe({
      s <- isolate(settings())
      updateTextInput(session, "path", value = s$path)
      updateRadioButtons(session, "our_side", selected = s$our_side)
      updateRadioButtons(session, "source", selected = s$source)
      updateSliderInput(session, "prior_n", value = s$prior_n)
      if (nzchar(s$path) && file.exists(s$path)) load_file(s$path, watch = TRUE)
    }) |> bindEvent(TRUE, once = TRUE)

    observe({
      s <- settings()
      new <- list(path = input$path %||% s$path, our_side = input$our_side %||% s$our_side,
                  source = input$source %||% s$source, prior_n = input$prior_n %||% s$prior_n)
      if (!identical(new, s)) settings(new)
    })

    load_file <- function(path, watch) {
      d <- tryCatch(read_dvw(path), error = function(e) e)
      if (inherits(d, "error")) {
        error(conditionMessage(d))
        return(invisible(FALSE))
      }
      error(NULL)
      dvw(d)
      watched(if (watch) path else NULL)
      invisible(TRUE)
    }

    observeEvent(input$load_path, {
      path <- trimws(gsub('^"|"$', "", input$path))
      if (!file.exists(path)) return(error(sprintf("ファイルが見つかりません: %s", path)))
      load_file(path, watch = TRUE)
    })
    observeEvent(input$upload, {
      load_file(input$upload$datapath, watch = FALSE)
      d <- dvw()
      if (!is.null(d)) d$file <- input$upload$name
      dvw(d)
    })

    # 試合中:ファイルが更新されていたら読み直す
    observe({
      invalidateLater(DVW_POLL_MS)
      path <- watched()
      d <- isolate(dvw())
      req(isTRUE(input$watch), !is.null(path), file.exists(path))
      if (is.null(d) || !identical(file.mtime(path), d$mtime)) load_file(path, watch = TRUE)
    })

    output$status <- renderUI({
      if (!is.null(error())) return(p(class = "rp-errors-text", error()))
      d <- dvw()
      if (is.null(d)) return(p(class = "rp-hint", "まだ読み込んでいません。"))
      sets <- sort(unique(d$rallies$set))
      tags$dl(
        class = "rp-facts",
        tags$dt("ファイル"), tags$dd(basename(d$file)),
        tags$dt("対戦"), tags$dd(sprintf("%s(ホーム) vs %s(ビジター)", d$teams[["home"]], d$teams[["visiting"]])),
        tags$dt("ラリー数"), tags$dd(sprintf("%d(%s)", nrow(d$rallies),
                                         paste(sprintf("第%dセット %d", sets, as.vector(table(d$rallies$set))[seq_along(sets)]), collapse = " / "))),
        tags$dt("最終更新"), tags$dd(format(d$mtime, "%H:%M:%S"),
                                   if (!is.null(watched()) && isTRUE(input$watch)) span(class = "rp-live-dot", "自動更新中"))
      )
    })

    output$set_choice <- renderUI({
      d <- dvw()
      sets <- if (is.null(d) || nrow(d$rallies) == 0) 1 else sort(unique(d$rallies$set))
      selectInput(ns("set"), "スタートの並びを取るセット", setNames(sets, paste0("第", sets, "セット")),
                  selected = isolate(input$set) %||% sets[1], width = "200px")
    })

    observeEvent(input$import, {
      d <- dvw()
      if (is.null(d)) return(showNotification("先に .dvw を読み込んでください", type = "warning"))
      ours <- input$our_side
      theirs <- setdiff(DVW_SIDES, ours)
      set <- as.integer(input$set %||% 1)
      cur <- teams()
      teams(list(us = dvw_team(d, ours, existing = cur$us, team_id = "us", set = set),
                 them = dvw_team(d, theirs, existing = cur$them, team_id = "them", set = set)))
      showNotification(sprintf("%s と %s のメンバーを読み込みました", d$teams[[ours]], d$teams[[theirs]]),
                       type = "message")
    })

    actual <- reactive({
      src <- input$source %||% "none"
      if (src == "dvw") {
        d <- dvw()
        if (is.null(d)) return(NULL)
        ours <- input$our_side %||% "home"
        list(us = setter_stats_to_rotation(dvw_setter_stats(d, ours), teams()$us),
             them = setter_stats_to_rotation(dvw_setter_stats(d, setdiff(DVW_SIDES, ours)), teams()$them))
      } else if (src == "live") {
        l <- logs()
        list(us = rotation_stats(l, "us"), them = rotation_stats(l, "them"))
      } else {
        NULL
      }
    })

    output$stats <- renderUI({
      a <- actual()
      if (is.null(a)) {
        return(p(class = "rp-hint", if (identical(input$source, "dvw")) "DataVolley のファイルを読み込むと表示します。"
                 else "能力値だけで計算しています。"))
      }
      rate <- function(won, n) ifelse(n == 0, "—", sprintf("%s (%d/%d)", pct(won / n, 0), won, n))
      tbl <- function(team, st) {
        tags$table(
          class = "table table-condensed rp-stats-table",
          tags$tr(tags$th(team$name), tags$th("サイドアウト率"), tags$th("ブレイク率")),
          lapply(seq_len(nrow(st)), function(i) tags$tr(
            tags$td(rotation_label(team, st$rot[i])),
            tags$td(rate(st$side_out[i], st$receive[i])),
            tags$td(rate(st$break_point[i], st$serve[i]))
          )),
          tags$tr(class = "rp-total", tags$td("合計"),
                  tags$td(rate(sum(st$side_out), sum(st$receive))),
                  tags$td(rate(sum(st$break_point), sum(st$serve))))
        )
      }
      tagList(fluidRow(column(6, tbl(teams()$us, a$us)), column(6, tbl(teams()$them, a$them))),
              p(class = "rp-hint", "ローテーションタブ・マッチアップタブ・試合タブの見込みは、この実績と能力値を混ぜて計算します。"))
    })

    list(actual = actual, prior_n = reactive(input$prior_n %||% 10))
  })
}
