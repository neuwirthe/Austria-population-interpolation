## ----setup, include=FALSE---------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(here)
  library(lubridate)
  library(readxl)
})


## ---------------------------------------------------------------------------------------
save_path <- path(here(),"r_data")


## ---------------------------------------------------------------------------------------
source(path(here(),"utils","purl_and_source.R"))
purl_and_source(path(here(),"utils","get_region_names.Rmd"))


## ---------------------------------------------------------------------------------------
2002:2023 |>
  map(
  \(i)suppressMessages(
    read_csv2(
      paste0(
      "https://data.statistik.gv.at/data/OGD_bevstandjbab2002_BevStand_",
      i,
      ".csv")))
  ) |> 
  reduce(bind_rows) |>
  mutate(Jahr=str_sub(`C-A10-0`,5,8) |> as.integer()) |>
  mutate(Geschlecht=str_sub(`C-C11-0`,5,5)) |>
  mutate(Geschlecht=ifelse(Geschlecht==1,"m","w")) |>
  rename(Alter_prov=`C-GALTEJ112-0`) |>
  mutate(
    Alter=ifelse(str_detect(Alter_prov,"GALT5"),
                 str_sub(Alter_prov,11,13) |>
                   as.integer() |>
                   (\(x)(x-1)*5)(),
                 str_sub(Alter_prov,11,13) |>
                   as.integer() |>
                   (\(x)x-1)())) |>
    mutate(GKZ=str_sub(`C-GRGEMAKT-0`,10,14) |> as.integer()) |>
  mutate(Geschlecht=str_sub(`C-C11-0`,5,5)) |>
  mutate(Geschlecht=ifelse(Geschlecht==1,"m","w")) |>
  mutate(GKZ=str_sub(`C-GRGEMAKT-0`,10,14) |> as.integer()) |>
  mutate(pop=`F-ISIS-1` |> as.integer()) |>
  select(Jahr,GKZ,Alter,Geschlecht,pop) ->
  pop_GKZ_age_1y_gender_yearly


## ---------------------------------------------------------------------------------------
expand_grid(
  Jahr = pop_GKZ_age_1y_gender_yearly |> pull(Jahr) |> unique(),
  GKZ = pop_GKZ_age_1y_gender_yearly |> pull(GKZ) |> unique(),
  Alter = 0:100 |> as.integer(),
  Geschlecht = pop_GKZ_age_1y_gender_yearly |> 
    pull(Geschlecht) |> unique()) |>
  left_join(pop_GKZ_age_1y_gender_yearly) |>
  replace_na(list(pop=0)) |>
  left_join(namen_gemeinden) ->
  pop_GKZ_age_1y_gender_yearly    


## ---------------------------------------------------------------------------------------
save(pop_GKZ_age_1y_gender_yearly,
     file=path(save_path,
               "pop_GKZ_age_1y_gender_yearly.RData"))

