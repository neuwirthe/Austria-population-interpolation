knitr::opts_chunk$set(echo = TRUE)

setwd("~/Sites/Austria-population-interpolation")

## ---------------------------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(fs)
  library(here)
  library(lubridate)
  library(readxl)
  library(furrr)
  library(purrr)
  library(numbers)
  library(tictoc)
})


## ---------------------------------------------------------------------------------------------------------
source(path("utils","purl_and_source.R"))
source(path("utils","date_utils.R"))


## ---------------------------------------------------------------------------------------------------------
#purl_and_source(path("utils","date_utils.Rmd"))


## ---------------------------------------------------------------------------------------------------------
#save_path <- path(here(),"Austria-population-interpolation","r_data")


## ---------------------------------------------------------------------------------------------------------
#purl_and_source(path(here(),"utils","get_region_names.Rmd"))


## ---------------------------------------------------------------------------------------------------------
load(path("~","Sites","Austria-population-interpolation","r_data", "pop_GKZ_1y_gender_yearly.RData"))


## ---------------------------------------------------------------------------------------------------------
pop_GKZ_1y_gender_yearly |> str()


## ---------------------------------------------------------------------------------------------------------
#if(is.null(GKZ_to_use)){
  data_to_use <- pop_GKZ_1y_gender_yearly
#} else {
#  data_to_use <- pop_GKZ_1y_gender_yearly |>
#    filter(GKZ %in% GKZ_to_use)
#}
rm(pop_GKZ_1y_gender_yearly)


## ---------------------------------------------------------------------------------------------------------
print("creating incomplete data")


## ---------------------------------------------------------------------------------------------------------
tic()
expand_grid(
  datum_jahr_woche() |> select(-Quartal,-Wochen_im_Quartal),
  GKZ = data_to_use |>
    pull(GKZ) |> 
    unique() |>
    as.integer() ,
  Alter= 0:100 |> as.integer(), 
  Geschlecht=c("m","w")) |> 
  left_join(data_to_use  |>
              select(-Name) |>
              mutate(Woche=1L)) |>
  mutate(pop = pop |> as.numeric()) ->
  data_incomplete
rm(data_to_use)
gc() |> invisible()
gc() |> invisible()
toc()
data_incomplete |>
  str()


## ---------------------------------------------------------------------------------------------------------
print("saving incomplete data")


## ---------------------------------------------------------------------------------------------------------
tic()
save(data_incomplete, 
     file="data_incomplete.RData")
toc()
gc() |> invisible()


## ---------------------------------------------------------------------------------------------------------
print("start interpolation multisession")


## ---------------------------------------------------------------------------------------------------------
plan(multisession, workers = 8)

bl_num <- 1:9
pop_GKZ_1y_gender_weekly <-
  tibble()

load(path("data_incomplete.RData"))

#bl_num <- 1
tic()
for (bl in bl_num) {
  data_incomplete |>
    filter((floor(GKZ / 10000) |> as.integer()) == bl) ->
    filtered_df
  if(nrow(filtered_df) == 0) next
    filtered_df |>  
    split(~ GKZ + Alter + Geschlecht) -> xxx

#  rm(data_incomplete)
  gc() |> invisible()
  gc() |> invisible()

  xxx |>
    future_map_dfr(
      \(x)
        mutate(x,
          pop_new = approxfun(
            x |>
              filter(!is.na(pop)) |>
              pull(Jahr_Woche),
            x |> filter(!is.na(pop)) |>
              pull(pop),
            rule = 2
          )(Jahr_Woche)
        ) |>
          select(-pop) |>
          rename(pop = pop_new),
        .progress=TRUE
    ) ->
  yyy
  rm(filtered_df)
  gc() |> invisible()
  gc() |> invisible()
  bind_rows(
    pop_GKZ_1y_gender_weekly,
    yyy
  ) |>
  select(-Jahr_Woche) ->
  pop_GKZ_1y_gender_weekly
  rm(yyy)

  gc() |> invisible()
  gc() |> invisible()
  
  paste("Bundesland", bl, "finished") |> print()

}
toc()
beepr::beep(8)


## ---------------------------------------------------------------------------------------------------------
pop_GKZ_1y_gender_weekly |> str()


## ---------------------------------------------------------------------------------------------------------
print("saving interpolated data")


## ---------------------------------------------------------------------------------------------------------
tic()
#if(is_null(GKZ_to_use)){
#  save(pop_GKZ_1y_gender_weekly,
#       file=path(save_path,"pop_GKZ_1y_gender_weekly.RData"))
#} else {
#  df_name <- paste(c("pop_GKZ_1y_gender_weekly",GKZ_to_use),collapse="_")
#  assign(df_name,pop_GKZ_1y_gender_weekly,
#         envir=.GlobalEnv)
#  save(list=df_name,
#       file=path(save_path,
#                 paste0(df_name,".RData")))
#}
toc()  


## ---------------------------------------------------------------------------------------------------------
rm(pop_GKZ_1y_gender_weekly)
rm(xxx)
gc() |> invisible()
gc() |> invisible()

## ---------------------------------------------------------------------------------------------------------
print("start interpolation multicore")


## ---------------------------------------------------------------------------------------------------------
plan(multicore, workers = 8)

bl_num <- 1:9
pop_GKZ_1y_gender_weekly <-
  tibble()

load(path("data_incomplete.RData"))

#bl_num <- 1
tic()
for (bl in bl_num) {
  data_incomplete |>
    filter((floor(GKZ / 10000) |> as.integer()) == bl) ->
    filtered_df
  if(nrow(filtered_df) == 0) next
    filtered_df |>  
    split(~ GKZ + Alter + Geschlecht) -> xxx

#  rm(data_incomplete)
  gc() |> invisible()
  gc() |> invisible()

  xxx |>
    future_map_dfr(
      \(x)
        mutate(x,
          pop_new = approxfun(
            x |>
              filter(!is.na(pop)) |>
              pull(Jahr_Woche),
            x |> filter(!is.na(pop)) |>
              pull(pop),
            rule = 2
          )(Jahr_Woche)
        ) |>
          select(-pop) |>
          rename(pop = pop_new),
        .progress=TRUE
    ) ->
  yyy
  rm(filtered_df)
  gc() |> invisible()
  gc() |> invisible()
  bind_rows(
    pop_GKZ_1y_gender_weekly,
    yyy
  ) |>
  select(-Jahr_Woche) ->
  pop_GKZ_1y_gender_weekly
  rm(yyy)

  gc() |> invisible()
  gc() |> invisible()
  
  paste("Bundesland", bl, "finished") |> print()

}
toc()
beepr::beep(8)


## ---------------------------------------------------------------------------------------------------------
rm(pop_GKZ_1y_gender_weekly)
rm(xxx)
gc() |> invisible()
gc() |> invisible()


## ---------------------------------------------------------------------------------------------------------
print("start interpolation sequential")

## ---------------------------------------------------------------------------------------------------------
plan(sequential)


## ---------------------------------------------------------------------------------------------------------
#plan(multisession, workers = 8)

bl_num <- 1:9
pop_GKZ_1y_gender_weekly <-
  tibble()


#bl_num <- 1
load(path( "data_incomplete.RData"))
tic()
for (bl in bl_num) {
  data_incomplete |>
    filter((floor(GKZ / 10000) |> as.integer()) == bl) ->
    filtered_df
  if(nrow(filtered_df) == 0) next
    filtered_df |>  
    split(~ GKZ + Alter + Geschlecht) -> xxx

#  rm(data_incomplete)
  gc() |> invisible()
  gc() |> invisible()

  xxx |>
    map_dfr(
      \(x)
        mutate(x,
          pop_new = approxfun(
            x |>
              filter(!is.na(pop)) |>
              pull(Jahr_Woche),
            x |> filter(!is.na(pop)) |>
              pull(pop),
            rule = 2
          )(Jahr_Woche)
        ) |>
          select(-pop) |>
          rename(pop = pop_new),
        .progress=TRUE
    ) ->
  yyy
  rm(filtered_df)
  gc() |> invisible()
  gc() |> invisible()
  bind_rows(
    pop_GKZ_1y_gender_weekly,
    yyy
  ) ->
  pop_GKZ_1y_gender_weekly
  rm(yyy)
  gc() |> invisible()
  gc() |> invisible()
  paste("Bundesland", bl, "finished") |> print()
}
toc()
beepr::beep(8)


## ---------------------------------------------------------------------------------------------------------
rm(pop_GKZ_1y_gender_weekly)
rm(xxx)
gc() |> invisible()
gc() |> invisible()


## ---------------------------------------------------------------------------------------------------------
# unlink(path(save_path,"data_incomplete.RData"))

