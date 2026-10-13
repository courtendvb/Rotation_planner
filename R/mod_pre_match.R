# 試合前のアプリへの組み込み(既存のスカウティングアプリにタブとして足す) -----------------------
#
# 既存のアプリ(page_navbar など)に、次の2つのタブを足して使う。
#   UI     : pre_match_board_ui("rotation")    … 12面ボード(選んだスタートで起きる対面)
#            pre_match_matchup_ui("rotation")  … 対面別の実績、全試合のローテーション別の実績、スタートの検討
#   server : pre_match_server("rotation", plays = data1, opponent = reactive(input$master_ui_team_id))
#
# plays は datavolley::read_dv() の plays を試合ごとにつないだもの(reactive)。
# 相手チームはアプリ側(サイドバー)で選び、自チームはボードのタブで選ぶ。
# 対面別の実績は2チームの対戦だけから、ローテーション単位の実績は相手を問わない全試合から作り、
# 対戦の少ない対面ほど全試合の実績に寄せて見込みを出す。
# 画面の状態(ドラッグした立ち位置、S1 のパターン)はファイルに保存せず、見ている人ごとに持つ。

#' 12面ボードのタブ
pre_match_board_ui <- function(id) {
  ns <- NS(id)
  tagList(
    rp_dependency(),
    div(
      class = "rp-in-match",
      mod_rotation_ui(ns("board"), extra_controls = tagList(
        uiOutput(ns("our_team"), inline = TRUE), uiOutput(ns("status"), inline = TRUE)
      ))
    )
  )
}

#' 対面別の実績、全試合のローテーション別の実績、スタートの検討のタブ
pre_match_matchup_ui <- function(id) {
  ns <- NS(id)
  tagList(
    rp_dependency(),
    div(
      class = "rp-panel",
      uiOutput(ns("summary")),
      h4("全試合のローテーション別の実績(相手を問わない)"),
      fluidRow(column(6, uiOutput(ns("rotations_us"))), column(6, uiOutput(ns("rotations_them")))),
      h4("これまでのスタートローテーション"),
      fluidRow(column(6, uiOutput(ns("starts_us"))), column(6, uiOutput(ns("starts_them"))))
    ),
    mod_matchup_ui(ns("matchup"))
  )
}

#' 試合前のアプリ用の server。
#'   plays   : datavolley の plays を試合ごとにつないだ data.frame を返す reactive
#'   opponent: 相手チームの team_id(文字列、または文字列を返す reactive)
#'   our_team: 自チームの team_id の初期値(データにない場合は、相手以外で試合数の最も多いチーム)
#'   prior_n : 対戦の少ない対面の補正(何本で半分信頼するか)
pre_match_server <- function(id, plays, opponent, our_team = NULL, prior_n = 10) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    opponent_id <- if (is.reactive(opponent)) opponent else reactive(opponent)

    matches <- reactive(dv_matches_from_plays(plays()))
    index <- reactive(pre_match_team_index(matches()))

    default_us <- reactive({
      ids <- index()$team_id
      if (!is.null(our_team) && our_team %in% ids) return(our_team)
      others <- setdiff(ids, opponent_id())
      if (length(others)) others[1] else NA_character_
    })
    output$our_team <- renderUI({
      idx <- index()
      req(nrow(idx) > 0)
      current <- isolate(input$our_team)
      selectInput(ns("our_team"), "自チーム", stats::setNames(idx$team_id, paste0(idx$team, " (", idx$team_id, ")")),
                  selected = if (!is.null(current) && current %in% idx$team_id) current else default_us(),
                  width = "220px")
    })
    us_id <- reactive(input$our_team %||% default_us())
    them_id <- reactive(opponent_id() %||% NA_character_)
    valid <- reactive({
      ids <- index()$team_id
      !is.na(us_id()) && !is.na(them_id()) && us_id() %in% ids && them_id() %in% ids && us_id() != them_id()
    })

    # チーム:それぞれの一番新しい試合の最初のセットのスタート(ローテ 0)
    teams <- reactiveVal(sample_teams())
    observe({
      req(valid())
      cur <- isolate(teams())
      us <- pre_match_team(matches(), us_id(), "us")
      them <- pre_match_team(matches(), them_id(), "them")
      req(us, them)
      us$reception_variants <- cur$us$reception_variants
      them$reception_variants <- cur$them$reception_variants
      new <- list(us = us, them = them)
      if (!identical(new, cur)) teams(new)
    })

    empty_rotations <- data.frame(rot = 0:5, receive = 0, side_out = 0, serve = 0, break_point = 0)
    rotations <- reactive({
      if (!valid()) return(list(us = empty_rotations, them = empty_rotations))
      list(us = team_rotation_records(matches(), us_id(), teams()$us),
           them = team_rotation_records(matches(), them_id(), teams()$them))
    })
    stats <- reactive({
      if (!valid()) return(empty_matchup_stats())
      head_to_head_stats(matches(), us_id(), them_id(), teams()$us, teams()$them)
    })
    has_data <- reactive({
      r <- rotations()
      sum(r$us$receive + r$us$serve) > 0 || sum(r$them$receive + r$them$serve) > 0
    })
    prior <- reactive(prior_n)
    prob <- reactive(rally_prob_from_stats(stats(), prior_n, rotations = rotations()))

    count_matches <- function(id) length(matches_of(matches(), id))
    status_text <- reactive({
      if (length(matches()) == 0) return("試合のデータがありません")
      if (is.na(them_id()) || !them_id() %in% index()$team_id) return("相手チームのデータがありません")
      if (!valid()) return("自チームと相手チームに別のチームを選んでください")
      sprintf("対戦 %d 試合 / %s %d 試合 / %s %d 試合", attr(stats(), "matches") %||% 0L,
              teams()$us$name, count_matches(us_id()), teams()$them$name, count_matches(them_id()))
    })
    output$status <- renderUI(div(class = if (valid()) "rp-hint" else "rp-errors-text", status_text()))
    output$summary <- renderUI({
      div(
        p(strong(if (valid()) sprintf("%s vs %s", teams()$us$name, teams()$them$name) else ""), " ", status_text()),
        p(class = "rp-hint",
          "対面別の実績はこの2チームの対戦だけから数えます。見込みは、対戦の少ない対面ほど、",
          "相手を問わない全試合のローテーション別の実績に寄せて計算します。",
          "自チームはボードのタブで、相手チームはサイドバーで選びます。")
      )
    })

    rotation_table <- function(team, rec) {
      rate <- function(won, n, baseline) {
        if (n == 0) return(tags$td(class = "rp-empty", "—"))
        tags$td(style = sprintf("background:%s", prob_color(won / n - baseline + 0.5)),
                sprintf("%s (%d/%d)", pct(won / n, 0), as.integer(won), as.integer(n)))
      }
      tags$table(
        class = "rp-matrix rp-record",
        tags$tr(tags$th(team$name), tags$th("サイドアウト"), tags$th("ブレイク")),
        lapply(0:5, function(k) {
          r <- rec[rec$rot == k, ]
          tags$tr(tags$th(rotation_label(team, k)),
                  rate(r$side_out, r$receive, DEFAULT_SIDE_OUT),
                  rate(r$break_point, r$serve, 1 - DEFAULT_SIDE_OUT))
        }),
        tags$tr(tags$th("計"), rate(sum(rec$side_out), sum(rec$receive), DEFAULT_SIDE_OUT),
                rate(sum(rec$break_point), sum(rec$serve), 1 - DEFAULT_SIDE_OUT))
      )
    }
    output$rotations_us <- renderUI({ req(valid()); rotation_table(teams()$us, rotations()$us) })
    output$rotations_them <- renderUI({ req(valid()); rotation_table(teams()$them, rotations()$them) })

    start_table <- function(team, rec) {
      tags$table(
        class = "rp-matrix rp-record",
        tags$tr(tags$th(team$name), tags$th("スタートしたセット"), tags$th("取ったセット")),
        lapply(0:5, function(k) {
          r <- rec[rec$rot == k, ]
          tags$tr(tags$th(rotation_label(team, k)),
                  tags$td(if (r$sets == 0) "—" else as.integer(r$sets)),
                  tags$td(if (r$sets == 0) "—" else sprintf("%d (%s)", as.integer(r$won), pct(r$won / r$sets, 0))))
        })
      )
    }
    output$starts_us <- renderUI({
      req(valid())
      start_table(teams()$us, start_rotation_records(matches(), us_id(), teams()$us))
    })
    output$starts_them <- renderUI({
      req(valid())
      start_table(teams()$them, start_rotation_records(matches(), them_id(), teams()$them))
    })

    placements <- reactiveVal(list())
    mod_rotation_server("board", teams, placements, stats, has_data, prior, prob = prob)
    mod_matchup_server("matchup", teams, placements, stats, has_data, prior, prob = prob)
  })
}
