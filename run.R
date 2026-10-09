# インストールせずに起動する: R でこのフォルダを作業ディレクトリにして source("run.R")
library(shiny)
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")
runApp(run_app(), launch.browser = TRUE)
