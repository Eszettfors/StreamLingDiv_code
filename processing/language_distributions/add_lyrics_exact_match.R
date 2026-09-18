library(duckdb)
library(DBI)
library(tidyverse)

# this script takes the 9GB song_lyrics csv scraped from genius (https://www.kaggle.com/datasets/carlosgdcj/genius-song-lyrics-with-language-information/data), subsets
# to the relevant artist, song combinations and exports

# read relevant song artist information
df_songs = read_csv("data/processed/artist_title.csv")
nrow(df_songs) # 110753 unique songs

# define a connection
con = dbConnect(duckdb())

# do a query on the database to get the relevant artist and track ionformation

# define a writing table
dbWriteTable(con, "df_songs", df_songs)

# find matches
matched = dbGetQuery(
  conn = con,
  "SELECT
      s.title AS title,
      s.artist AS artist,
      l.title AS genius_title,
      l.artist AS genius_artist,
      l.lyrics
  FROM  df_songs s
  LEFT JOIN read_csv_auto('data/raw/song_lyrics.csv', ignore_errors = true) l
    ON lower(trim(s.title)) = lower(trim(l.title))
   AND lower(trim(s.artist)) = lower(trim(l.artist))
  "
)

df_matched = as_tibble(matched)
head(df_matched)


# check for na
colSums(is.na(df_matched)) # 74795 were not matched

df_not_matched = df_matched %>%
  filter(is.na(lyrics))

df_matched = df_matched %>%
  filter(!is.na(lyrics))


# format and write
df_matched = df_matched %>%
  select(title, artist, lyrics)

write_csv(df_matched, "data/processed/artist_title_lyrics_exact_match.csv")


df_non_matched = df_not_matched %>%
  select(title, artist)

write_csv(df_non_matched, "data/processed/artist_title_no_exact_match.csv")
