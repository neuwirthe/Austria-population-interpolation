## ----setup, include=FALSE---------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(here)
  library(lubridate)
  library(readxl)
})


## ---------------------------------------------------------------------------------------------------
tmpdir <- tempdir()
download.file("https://www.statistik.at/verzeichnis/reglisten/gemliste_knz.xls",
              path(tmpdir,"gemliste_knz.xls"),
              quiet=TRUE)
read_excel(path(tmpdir,"gemliste_knz.xls"),skip=3) |>
  select(1:2) |>
  drop_na() |>
  rename(GKZ=1,
         Name=2) |>
  mutate_at(vars(GKZ),as.integer) ->
  namen_gemeinden


## ---------------------------------------------------------------------------------------------------
download.file("https://www.statistik.at/verzeichnis/reglisten/polbezirke.xls",
              path(tmpdir,"polbezirke.xls"),
              quiet=TRUE)
read_excel(path(tmpdir,"polbezirke.xls"),skip=3) |>
  select(1:4) |>
  drop_na() |>
  rename(BundeslandID=1,
         Bundesland=2,
         BKZ=3,
         Name=4)|>
  mutate_at(vars(BundeslandID,BKZ),as.integer) |>
  mutate(Name=str_replace(Name,fixed("(")," (")) ->
  namen_bezirke

namen_bezirke |>
  select(1:2) |>
  distinct() |>
  bind_rows(
    tibble(
      BundeslandID=10,
      Bundesland="Österreich"
    )
  ) ->
  namen_bl

