# 試合前のアプリへの組み込み(既存のスカウティングアプリにタブとして足す) -----------------------
#
# 既存のアプリ(page_navbar など)に、次の2つのタブを足して使う。
#   UI     : pre_match_board_ui("rotation")    … 12面ボード(選んだスタートで起きる対面)
#            pre_match_matchup_ui("rotation")  … 対面別の実績、全試合のローテーション別の実績、スタートの検討
#   server : pre_match_server("rotation", plays = data1, our_team = "HTH",
#                             opponent = reactive(input$master_ui_team_id))
#
# plays は datavolley::read_dv() の plays を試合ごとにつないだもの(reactive)。
# 自チームは our_team で固定し、相手チームはボードのタブで選ぶ(opponent は初期値。変わればそれに合わせる)。
# 両チームの選手(並び、リベロ、役割など)はボードのタブで変更できる。
# 対面別の実績は2チームの対戦だけから、ローテーション単位の実績は相手を問わない全試合から作り、
# 対戦の少ない対面ほど全試合の実績に寄せて見込みを出す。
# 画面の状態(ドラッグした立ち位置、S1 のパターン、変更した選手)はファイルに保存せず、見ている人ごとに持つ。

#' 12面ボードのタブ
pre_match_board_ui <- function(id) {
  ns <- NS(id)
  tagList(
    rp_dependency(),
    div(
      class = "rp-in-match",
      mod_rotation_ui(ns("board"), extra_controls = tagList(
        uiOutput(ns("opponent"), inline = TRUE),
        checkboxInput(ns("edit_players"), "選手を変更する", value = FALSE),
        uiOutput(ns("status"), inline = TRUE)
      )),
      conditionalPanel(
        "input.edit_players", ns = ns,
        div(
          class = "rp-panel",
          p(class = "rp-hint", "変更は、相手チームを選び直すまでこの画面だけに残ります(データは変わりません)。",
            actionLink(ns("reset_players"), "データの並びに戻す")),
          fluidRow(column(6, h4("自チーム"), mod_roster_ui(ns("roster_us"))),
                   column(6, h4("相手チーム"), mod_roster_ui(ns("roster_them"))))
        )
      )
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
#'   our_team: 自チームの team_id(固定)
#'   opponent: 相手チームの初期値の team_id(文字列、または文字列を返す reactive、NULL)。
#'             自チームと同じかデータにないときは、自チーム以外で試合数の最も多いチーム
#'   prior_n : 対戦の少ない対面の補正(何本で半分信頼するか)
pre_match_server <- function(id, plays, our_team, opponent = NULL, prior_n = 10) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    opponent_hint <- if (is.reactive(opponent)) opponent else reactive(opponent)

    matches <- reactive(dv_matches_from_plays(plays()))
    index <- reactive(pre_match_team_index(matches()))
    us_id <- reactive(our_team)

    opponent_choices <- reactive(index()[index()$team_id != our_team, , drop = FALSE])
    default_them <- reactive({
      ids <- opponent_choices()$team_id
      hint <- opponent_hint()
      if (length(hint) == 1 && !is.na(hint) && hint %in% ids) hint else if (length(ids)) ids[1] else NA_character_
    })
    output$opponent <- renderUI({
      idx <- opponent_choices()
      req(nrow(idx) > 0)
      current <- isolate(input$opponent)
      selectInput(ns("opponent"), "相手チーム", stats::setNames(idx$team_id, paste0(idx$team, " (", idx$team_id, ")")),
                  selected = if (!is.null(current) && current %in% idx$team_id) current else default_them(),
                  width = "240px")
    })
    # アプリ側(サイドバーなど)で相手が変わったら、ボードの選択も合わせる
    observeEvent(opponent_hint(), {
      hint <- opponent_hint()
      if (length(hint) == 1 && !is.na(hint) && hint %in% opponent_choices()$team_id &&
          !identical(hint, input$opponent)) {
        updateSelectInput(session, "opponent", selected = hint)
      }
    }, ignoreInit = TRUE)
    them_id <- reactive({
      sel <- input$opponent
      if (!is.null(sel) && sel %in% opponent_choices()$team_id) sel else default_them()
    })
    valid <- reactive({
      ids <- index()$team_id
      !is.na(them_id()) && us_id() %in% ids && them_id() %in% ids && us_id() != them_id()
    })

    # チーム:それぞれの一番新しい試合の最初のセットのスタート(ローテ 0)。
    # ボードで変更した選手は、相手チームやデータが変わるまで残す
    teams <- reactiveVal(sample_teams())
    from_data <- reactive({
      req(valid())
      us <- pre_match_team(matches(), us_id(), "us")
      them <- pre_match_team(matches(), them_id(), "them")
      req(us, them)
      list(us = us, them = them)
    })
    load_teams <- function(new) {
      cur <- isolate(teams())
      new$us$reception_variants <- cur$us$reception_variants
      new$them$reception_variants <- cur$them$reception_variants
      if (!identical(new, cur)) teams(new)
    }
    observe(load_teams(from_data()))
    observeEvent(input$reset_players, load_teams(from_data()))
    mod_roster_server("roster_us", teams, "us")
    mod_roster_server("roster_them", teams, "them")

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
      if (!our_team %in% index()$team_id) return(sprintf("自チーム(%s)の試合がデータにありません", our_team))
      if (!valid()) return("相手チームのデータがありません")
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
          "相手チームと、両チームの選手はボードのタブで選び・変更します。")
      )
    })

    output$rotations_us <- renderUI({ req(valid()); rotation_record_table(teams()$us, rotations()$us) })
    output$rotations_them <- renderUI({ req(valid()); rotation_record_table(teams()$them, rotations()$them) })

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
