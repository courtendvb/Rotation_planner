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
    tabPanel("試合", mod_live_ui("live"))
  )
}

app_server <- function(input, output, session) {
  teams <- stored_reactive_val("teams", sample_teams())
  placements <- stored_reactive_val("placements", list())

  mod_roster_server("us", teams, "us")
  mod_roster_server("them", teams, "them")
  mod_rotation_server("rotation", teams, placements)
  logs <- mod_live_server("live", teams, placements)
  mod_matchup_server("matchup", teams, logs, placements)
}

#' アプリを起動する。
#' 例: run_app() / run_app(data_dir = "~/rotation-data")
run_app <- function(data_dir = NULL, ...) {
  if (!is.null(data_dir)) options(rotationplanner.data_dir = data_dir)
  shinyApp(app_ui(), app_server, options = list(...))
}
