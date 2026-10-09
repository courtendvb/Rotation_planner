# データの保存(ローカルの RDS ファイル) -------------------------------------
#
# 保存先は options(rotationplanner.data_dir = "...") で変更できる。
# 既定は R の標準のユーザーデータ用フォルダ。

data_dir <- function() {
  getOption("rotationplanner.data_dir", tools::R_user_dir("rotationplanner", "data"))
}

load_value <- function(name, default) {
  path <- file.path(data_dir(), paste0(name, ".rds"))
  if (!file.exists(path)) return(default)
  tryCatch(readRDS(path), error = function(e) {
    warning(sprintf("%s を読み込めませんでした: %s", path, conditionMessage(e)))
    default
  })
}

save_value <- function(name, value) {
  dir.create(data_dir(), recursive = TRUE, showWarnings = FALSE)
  path <- file.path(data_dir(), paste0(name, ".rds"))
  tmp <- paste0(path, ".tmp")
  saveRDS(value, tmp)
  file.rename(tmp, path)
  invisible(path)
}

#' 値が変わるたびに自動保存される reactiveVal
stored_reactive_val <- function(name, default) {
  rv <- reactiveVal(load_value(name, default))
  observeEvent(rv(), save_value(name, rv()), ignoreInit = TRUE)
  rv
}
