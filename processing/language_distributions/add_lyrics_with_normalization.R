library(duckdb)
library(DBI)
library(tidyverse)

### all cases where the artist has å, ä or ö does not get recognized because they have been
# removed from the artist column in the lyrics dataset; not in the title however


# this script takes the data which had no exact match and tries normalize both the MGD+ and kaggle data

# read records with no exact match
df_songs = read_csv("data/processed/artist_title_no_exact_match.csv")
nrow(df_songs) # 74795 unique songs

# define a connection
con = dbConnect(duckdb())

# macro for normalizing the text that can be applied to the sql join
dbExecute(con, "
CREATE OR REPLACE MACRO normalize_text(t) AS
  trim(
    regexp_replace(
      regexp_replace(
        regexp_replace(
          strip_accents(lower(t)),
          '[!,:;@()\\[\\]{}#$/&%*=+<>|~`''^\".\\-_…—].*$', '', 'g'
        ),
        '\\s*[-–—]?\\s*(remaster(ed)?|radio edit|single version|live|acoustic|remix|feat|featured|featuring|Ao Vivo|with|con|edit|Acústico).*$', '', 'g'
      ),
      '\\s+', ' ', 'g'
    )
  );
")

dbExecute(con, "
CREATE OR REPLACE MACRO primary_artist(t) AS
  trim(
    split_part(
      split_part(
        split_part(
          split_part(
            split_part(t, ',', 1), '&', 1),
          ';', 1),
        '(', 1),
      '-', 1)
  );")


# normalize both no match data and lyrics data and do an inner join

dbWriteTable(con, "df_songs", df_songs, overwrite = TRUE)

matched = dbGetQuery(con, "
WITH songs AS (
  SELECT
    title,
    artist,
    normalize_text(title)  AS title_norm,
    normalize_text(primary_artist(artist)) AS artist_norm
  FROM df_songs
),
lyrics AS (
  SELECT
    title  AS genius_title,
    artist AS genius_artist,
    lyrics,
    normalize_text(title)  AS title_norm,
    normalize_text(primary_artist(artist)) AS artist_norm
  FROM read_csv_auto('data/raw/song_lyrics.csv', ignore_errors = true)
)
SELECT
  s.title,
  s.artist,
  l.genius_title,
  l.genius_artist,
  s.title_norm,
  s.artist_norm,
  l.lyrics
FROM songs s
INNER JOIN lyrics l
  ON s.title_norm = l.title_norm
 AND s.artist_norm = l.artist_norm
")

df_matched = as_tibble(matched) 
nrow(df_matched) # 36796 matched records

# look at possible duplets from over matching after normalizing

dublets = df_matched %>%
  group_by(title, artist) %>%
  summarize(n = n()) %>%
  filter(n > 1)

# deduplicate by selecting the first one
df_matched = df_matched %>%
  group_by(title, artist) %>%
  summarize(lyrics = first(lyrics)) %>%
  ungroup()


nrow(df_matched) # 29956 records salvaged


# read and join the exact matches
df_exact_match = read_csv("data/processed/artist_title_lyrics_exact_match.csv")


# merge
df_final = rbind(df_exact_match, df_matched)

# again, deduplicate
df_final = df_final %>%
  group_by(title, artist) %>%
  summarize(lyrics = first(lyrics)) %>%
  ungroup()

nrow(df_final) # lyrics for 65914 songs

write_csv(df_final, "data/processed/artist_title_lyrics_all_matches.csv")

#####

# Get unmatched songs
unmatched <- dbGetQuery(con, "
  WITH songs AS (SELECT title, artist, normalize_text(title) AS title_norm, normalize_text(primary_artist(artist)) AS artist_norm FROM df_songs)
  SELECT title, artist, title_norm, artist_norm
  FROM songs
  WHERE NOT EXISTS (
    SELECT 1 FROM read_csv_auto('data/raw/song_lyrics.csv', ignore_errors = TRUE)
    WHERE normalize_text(title) = songs.title_norm
      AND normalize_text(primary_artist(artist)) = songs.artist_norm
  )
  LIMIT 1000
")

