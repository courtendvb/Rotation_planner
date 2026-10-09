# チーム(選手と並び)の編集 -----------------------------------------------------

ROSTER_COLUMNS <- c(id = "id", number = "#", name = "名前", role = "役割",
                    attack = "攻撃", block = "ブロック", serve = "サーブ", receive = "レセプション")

mod_roster_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "rp-panel",
    textInput(ns("name"), NULL, width = "100%"),
    DT::DTOutput(ns("players")),
    div(
      style = "margin: 8px 0",
      actionButton(ns("add"), "+ 選手を追加"),
      actionButton(ns("remove"), "選択した選手を削除")
    ),
    p(class = "rp-hint", "セルをダブルクリックで編集。役割は S / OH / MB / OP / L、能力値は 1〜10。"),
    h4("スタートの並び"),
    uiOutput(ns("lineup")),
    uiOutput(ns("errors"))
  )
}

#' teams: list(us, them) を持つ reactiveVal。key: "us" または "them"
mod_roster_server <- function(id, teams, key) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    team <- reactive(teams()[[key]])
    set_team <- function(t) {
      all <- teams()
      all[[key]] <- t
      teams(all)
    }

    observe(updateTextInput(session, "name", value = isolate(team()$name)))
    observeEvent(input$name, {
      if (nzchar(input$name) && input$name != team()$name) set_team(modifyList(team(), list(name = input$name)))
    }, ignoreInit = TRUE)

    sorted_players <- reactive({
      pl <- team()$players
      pl[order(pl$number), , drop = FALSE]
    })

    output$players <- DT::renderDT({
      DT::datatable(
        isolate(sorted_players()),
        colnames = unname(ROSTER_COLUMNS),
        rownames = FALSE,
        editable = list(target = "cell", disable = list(columns = 0)),
        selection = "multiple",
        options = list(dom = "t", paging = FALSE, ordering = FALSE,
                       columnDefs = list(list(visible = FALSE, targets = 0)))
      )
    })
    proxy <- DT::dataTableProxy("players")
    observeEvent(sorted_players(), {
      DT::replaceData(proxy, sorted_players(), resetPaging = FALSE, rownames = FALSE)
    })

    observeEvent(input$players_cell_edit, {
      info <- input$players_cell_edit
      shown <- sorted_players()
      col <- names(shown)[info$col + 1]
      id <- shown$id[info$row]
      value <- info$value
      pl <- team()$players
      i <- which(pl$id == id)
      if (col == "role") {
        value <- toupper(trimws(value))
        if (!value %in% ROLES) {
          showNotification("役割は S / OH / MB / OP / L のどれかです", type = "warning")
          DT::replaceData(proxy, shown, resetPaging = FALSE, rownames = FALSE)
          return()
        }
      } else if (col == "number") {
        value <- suppressWarnings(as.integer(value))
        if (is.na(value)) return(DT::replaceData(proxy, shown, resetPaging = FALSE, rownames = FALSE))
      } else if (col %in% c("attack", "block", "serve", "receive")) {
        value <- suppressWarnings(as.numeric(value))
        if (is.na(value)) return(DT::replaceData(proxy, shown, resetPaging = FALSE, rownames = FALSE))
        value <- min(10, max(1, value))
      }
      pl[i, col] <- value
      set_team(modifyList(team(), list(players = pl)))
    })

    observeEvent(input$add, {
      t <- team()
      number <- max(c(0L, t$players$number)) + 1L
      new <- data.frame(id = paste0(t$id, "-", as.integer(Sys.time()), "-", number), number = number,
                        name = "", role = "OH", attack = 5, block = 5, serve = 5, receive = 5)
      t$players <- rbind(t$players, new)
      set_team(t)
    })

    observeEvent(input$remove, {
      rows <- input$players_rows_selected
      if (length(rows) == 0) return(showNotification("削除する選手を表で選択してください", type = "message"))
      ids <- sorted_players()$id[rows]
      t <- team()
      t$players <- t$players[!t$players$id %in% ids, , drop = FALSE]
      t$lineup[t$lineup %in% ids] <- ""
      if (!is.na(t$libero) && t$libero %in% ids) t$libero <- NA_character_
      set_team(t)
    })

    # 並びはコートの配置どおりに 前衛 P4 P3 P2 / 後衛 P5 P6 P1 で並べる
    output$lineup <- renderUI({
      t <- team()
      pl <- sorted_players()
      field <- pl[pl$role != "L", , drop = FALSE]
      choices <- c("—" = "", setNames(field$id, sprintf("#%d %s (%s)", field$number, field$name, field$role)))
      liberos <- pl[pl$role == "L", , drop = FALSE]
      libero_choices <- c("なし" = "", setNames(liberos$id, sprintf("#%d %s", liberos$number, liberos$name)))
      sel <- function(pos) {
        column(4, selectInput(ns(paste0("p", pos)), paste0("P", pos), choices, selected = t$lineup[pos]))
      }
      tagList(
        fluidRow(sel(4), sel(3), sel(2)),
        fluidRow(sel(5), sel(6), sel(1)),
        fluidRow(column(4, selectInput(ns("libero"), "リベロ", libero_choices,
                                       selected = if (is.na(t$libero)) "" else t$libero)))
      )
    })

    lapply(1:6, function(pos) {
      observeEvent(input[[paste0("p", pos)]], {
        t <- team()
        value <- input[[paste0("p", pos)]]
        if (!identical(t$lineup[pos], value)) {
          t$lineup[pos] <- value
          set_team(t)
        }
      }, ignoreInit = TRUE)
    })
    observeEvent(input$libero, {
      t <- team()
      value <- if (nzchar(input$libero)) input$libero else NA_character_
      if (!identical(t$libero, value)) {
        t$libero <- value
        set_team(t)
      }
    }, ignoreInit = TRUE)

    output$errors <- renderUI({
      errs <- validate_lineup(team())
      if (length(errs) == 0) return(NULL)
      tags$ul(class = "rp-errors", lapply(errs, tags$li))
    })
  })
}
