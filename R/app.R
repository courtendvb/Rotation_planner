# アプリ本体 --------------------------------------------------------------------

app_www_dir <- function() {
  installed <- system.file("app", "www", package = "rotationplanner")
  if (nzchar(installed)) installed else file.path("inst", "app", "www")
}

app_ui <- function() {
  addResourcePath("rp", app_www_dir())
  navbarPage(
    "Rotation Planner",
    header = tags$head(
      tags$link(rel = "stylesheet", href = "rp/rotationplanner.css"),
      tags$script(src = "rp/rotationplanner.js")
    ),
    tabPanel("チーム", fluidRow(column(6, mod_roster_ui("us")), column(6, mod_roster_ui("them")))),
    tabPanel("ローテーション", mod_rotation_ui("rotation")),
    tabPanel("マッチアップ", mod_matchup_ui("matchup")),
    tabPanel("データ", mod_data_ui("data"))
  )
}

app_server <- function(input, output, session) {
  defaults <- sample_teams()
  teams <- stored_reactive_val("teams", defaults)
  # 以前の版で保存したチーム(能力値の列など)を今の形にそろえる
  isolate(teams(list(us = normalize_team(teams()$us, defaults$us), them = normalize_team(teams()$them, defaults$them))))
  placements <- stored_reactive_val("placements", list())

  mod_roster_server("us", teams, "us")
  mod_roster_server("them", teams, "them")
  data <- mod_data_server("data", teams, placements)
  mod_rotation_server("rotation", teams, placements, data$stats, data$has_data, data$prior_n)
  mod_matchup_server("matchup", teams, placements, data$stats, data$has_data, data$prior_n)
}

#' アプリを起動する。
#' 例: run_app() / run_app(data_dir = "~/rotation-data")
run_app <- function(data_dir = NULL, ...) {
  if (!is.null(data_dir)) options(rotationplanner.data_dir = data_dir)
  shinyApp(app_ui(), app_server, options = list(...))
}
