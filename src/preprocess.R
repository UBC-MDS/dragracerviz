# author: Shaun Hutchinson
# date: 2023-02-28

"Preprocess the drag data and combines the cities dataset for longitude and latitude

Usage: src/preprocess.R --data=<data> --out_dir=<out_dir>

Options:
--data=<data>          Path (including filename) to cities data (which is saved in data directory)
--out_dir=<out_dir>    Path to directory where the plots should be saved
" -> doc

library(dragracer)
library(tidyverse)
library(docopt)

opt <- docopt(doc)
main <- function(data, out_dir) {

  # Read in cities data
  cities <-read_csv("data/uscities.csv") |>
    rename(state = state_name)

  #Merge drag tables and fix hometown/outcomes
  drag <- merge(x=dragracer::rpdr_contep, y=dragracer::rpdr_contestants,
                by=c("contestant", "season"), all.x = TRUE) |>
    mutate(hometown = case_when(hometown == "London, UK/ Boston, Massachusetts" ~ "Boston, Massachusetts",
                                hometown == "Toronto, Ontario, Canada" ~ "Toronto, Ontario",
                                hometown == "Fort Lauderdale,Florida" ~ "Fort Lauderdale, Florida",
                                TRUE ~ hometown)) |>
    separate(hometown, c("city", "state"), sep = ", ") |>
    mutate(outcome = case_when(outcome == "LOST2ND ROUND" ~ "HIGH",
                               outcome == "RUNNING" ~ NA_character_,
                               outcome == "Guest" ~ NA_character_,
                               outcome == "Eliminated" ~ "BTM",
                               outcome == "STAY" ~ "SAFE",
                               outcome == "RTRN" ~ NA_character_,
                               outcome == "LOST1ST ROUND" ~ "BTM",
                               outcome == "SAVE" ~ "BTM",
                               outcome == "LOST3RD ROUND" ~ "HIGH",
                               outcome == "TOP2" ~ "HIGH",
                               outcome == "LOSS" ~ "BTM",
                               outcome == "SAFE+DEPT" ~ "SAFE",
                               outcome == "TOP 4" ~ NA_character_,
                               outcome == "OUT" ~ NA_character_,
                               outcome == "WDR" ~ NA_character_,
                               outcome == "Miss C" ~ NA_character_,
                               outcome == "Runner-up" ~ "HIGH",
                               outcome == "MISSCON" ~ NA_character_,
                               outcome == "DISQ" ~ NA_character_,
                               outcome == "WIN+RTRN" ~ "WIN",
                               outcome == "Winner" ~ "WIN",
                               TRUE ~ outcome))
  # Add in lat/lng and extra columns
  drag_df <- merge(x=drag, y=cities[, c("city", "state", "lat", "lng")], by= c("city", "state"), all.x = TRUE) |>
    mutate(lat = case_when(city == "Back Swamp" ~ 34.5847,
                           city == "Catano" ~ 18.4375,
                           city == "Cherry Hill" ~ 39.9268,
                           city == "Harlem" ~ 40.8116,
                           city == "Mira Loma" ~ 33.9845,
                           city == "Saint Petersburg" ~ 27.7676,
                           city == "The Bronx" ~ 40.8448,
                           city == "Toronto" ~ 43.6532,
                           city == "Van Nuys" ~ 34.1899,
                           TRUE ~ lat),
           lng = case_when(city == "Back Swamp" ~ 79.1210,
                           city == "Catano" ~ -66.1440,
                           city == "Cherry Hill" ~ 75.0246,
                           city == "Harlem" ~ 73.9465,
                           city == "Mira Loma" ~ 117.5159,
                           city == "Saint Petersburg" ~ 82.6403,
                           city == "The Bronx" ~ 73.8648,
                           city == "Toronto" ~ 79.3832,
                           city == "Van Nuys" ~ 118.4514,
                           TRUE ~ lng),
           finalist = case_when((finale == 1 & contestant == 1) ~ 1),
           winner = case_when(rank == 1 ~ 1),
           first_eliminated = case_when((episode == 1 & eliminated == 1) ~ 1,
                                        contestant == "Jaymes Mansfield" ~ 1,
                                        contestant == "Dahlia Sin" ~ 1,
                                        contestant == "Kahmora Hall" ~ 1,
                                        contestant == "Venus D-Lite" ~ 1))
  #Download data to data folder
  write.csv(drag_df, paste0(out_dir, "/drag.csv"))

}

main(opt$data, opt$out_dir)

