library(tidyverse)
library(duckdb)
library(DBI)

# this script takes the top 200 charts from MGD+, aggregates for each country and year the top songs per country and exports it as a single csv


# read all data and cocatenate

# list of all files to read
path = "data/raw/MGDplus/charts/regional/"
vec_of_files = list.files(path)
list_of_charts = list()

for (file in vec_of_files){
  # loop through all files put them in a list
  df = read_tsv(paste0(path, file))

  # select relevant columns
  df = df %>%
    select(Track, Artist, Streams, Date, market) %>%
    mutate(Streams = as.integer(Streams),
           Date = as.Date(Date))

  list_of_charts[[file]] = df
}

df_charts = bind_rows(list_of_charts)


# add year
df_charts = df_charts %>%
  mutate(Year = format(Date, "%Y")) %>%
  select(!Date)

# group by year, artist and Track to remove duplets within a year
df_charts = df_charts %>%
  group_by(Year, Artist, Track, market) %>%
  summarize(Streams = sum(Streams)) %>%
  arrange(Year, -Streams) %>%
  ungroup()


# remove global
df_charts = df_charts %>%
  filter(market != "global")

# Make market upper case to correspond to 2 letter isocodes and rename column
df_charts = df_charts %>%
  mutate(market = toupper(market)) %>%
  rename("country_code" = "market",
         "year" = "Year",
         "artist" = "Artist",
         "title" = "Track",
         "streams" = "Streams") %>%
  relocate(country_code)

# write
write_csv(df_charts, "data/processed/country_year_artist_title_streams.csv")

# get unique artist track combination, i.e., that which needs to be labeled
df_artist_title = df_charts %>%
  distinct(artist, title)


write_csv(df_artist_title, "data/processed/artist_title.csv")

