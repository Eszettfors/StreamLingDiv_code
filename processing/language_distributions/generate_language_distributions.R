library(tidyverse)
library(DescTools)
library(rnaturalearthdata)
library(rnaturalearth)
library(plotly)

# this script takes the language tagged songs and adds them to the timeseries stream data
df_songs_langs = read_csv("data/processed/artist_title_lyrics_lang.csv")
df_streams = read_csv("data/processed/country_year_artist_title_streams.csv")
df_countries = ne_countries(scale = "medium")


colSums(is.na(df_songs_langs))
# two empty lyrics

df_songs_langs = df_songs_langs %>%
  filter(!is.na(lyrics))

# 65,912 songs that we have lyrics for
Desc(df_songs_langs$ft_lyrics_conf) # almost all labels are high confidence
Desc(df_songs_langs$glot_lyrics_conf) # somewhat longer tail



# cases where the LIDs agree
df_songs_langs = df_songs_langs %>%
  mutate(agreement = ifelse(ft_lyrics_lang == glot_lyrics_lang, TRUE, FALSE))

table(df_songs_langs$agreement) / nrow(df_songs_langs) # 70.6% of cases they agree


# use cases where Fasttext is very certain, i.e., FT > 0.75; i.e., we are here quite certain what the language is. However, FT is not good at detecting smaller varieties. So if FT hestites but GlotLID is more certain, we use glotLID
df_songs_langs %>%
  filter(ft_lyrics_conf > 0.75) %>%
  filter(glot_lyrics_conf > ft_lyrics_conf & agreement == FALSE) # 577 cases where FT is certain but GlotLID is more certain 

# glotlid has problem with reading hindi script --> gives undefined; needs to be kept as exception; "jam" and "pcm" clear oversensitive to AAV
df_songs_langs_filtered = df_songs_langs %>%
  filter(ft_lyrics_conf > 0.75) %>%
  mutate(glot_lyrics_lang = case_when(glot_lyrics_lang %in% c("jam", "pcm", "und") ~ ft_lyrics_lang,
                                                                                  TRUE ~ glot_lyrics_lang)) %>%
  mutate(ISO6393 = ifelse(glot_lyrics_conf > ft_lyrics_conf, glot_lyrics_lang, ft_lyrics_lang))

df_songs_langs_filtered %>%
  distinct(ISO6393) # 102 different languages

### add to the streams data
df_streams = df_streams %>%
  left_join(df_songs_langs_filtered %>%
                      select(title, artist, ISO6393), join_by("title" == "title", "artist" == "artist"))

tot_streams = sum(df_streams$streams) # 728,585,852,208
streams_with_lang = df_streams %>%
  filter(!is.na(ISO6393)) %>%
  summarize(tot_streams = sum(streams)) %>%
  pull(tot_streams)

streams_with_lang # 562,263,224,324

streams_with_lang / tot_streams # We can identify the language of 77 % of streams 

### adjust macro lang for streams
df_streams = df_streams %>%
  mutate(ISO6393 = ifelse(ISO6393 == "est", "ekk", ISO6393))

# write streams
write_csv(df_streams, "data/processed/country_year_artist_title_streams_langs.csv")


### are any countries or years especially impacted by lack of coverage?
# year
df_streams %>%
  mutate(lang_na = ifelse(is.na(ISO6393), TRUE, FALSE)) %>%
  group_by(year, lang_na) %>%
  summarize(streams = sum(streams)) %>%
  group_by(year) %>%
  mutate(streams_with_lang_perc = streams / sum(streams) * 100) %>%
  filter(lang_na == FALSE) %>%
  ggplot(aes(y = streams_with_lang_perc, x = year)) + 
    geom_point() + 
    geom_line() + 
  ylim(c(0,100)) +
  labs(title = "Percent of streams with language labels",
       y = "percent")
  
# country
df_streams %>%
  mutate(lang_na = ifelse(is.na(ISO6393), TRUE, FALSE)) %>%
  group_by(country_code, lang_na) %>%
  summarize(streams = sum(streams)) %>%
  group_by(country_code) %>%
  mutate(streams_with_lang_perc = streams / sum(streams) * 100) %>%
  filter(lang_na == FALSE) %>%
  right_join(df_countries, join_by("country_code" == "iso_a2_eh")) %>%
  ggplot(aes(geometry = geometry, fill = streams_with_lang_perc)) + 
  geom_sf() + 
  labs(title = "percent of streams with lyrics")

# western industrialized countries tends to have lyrics or because text processing is designed
# for latin scripts


# year country outliers
country_year_coverage = df_streams %>%
  mutate(lang_na = ifelse(is.na(ISO6393), TRUE, FALSE)) %>%
  group_by(country_code, year, lang_na) %>%
  summarize(streams = sum(streams)) %>%
  group_by(country_code, year) %>%
  mutate(streams_with_lang_perc = streams / sum(streams) * 100) %>%
  filter(lang_na == FALSE) %>%
  ggplot(aes(y = streams_with_lang_perc, x = year, fill = country_code)) + 
    geom_point() + 
    geom_line() + 
  ylim(c(0,100)) +
  labs(title = "Percent of streams with language labels",
       y = "percent")

ggplotly(country_year_coverage)


# replace NA with unknown
df_streams = df_streams %>%
  mutate(ISO6393 = ifelse(is.na(ISO6393), "unknown", ISO6393))

# group by country year and language tag
country_year_lang = df_streams %>%
  group_by(country_code, year, ISO6393) %>%
  summarize(streams = sum(streams))

# write final df
write_csv(country_year_lang, "data/final_dataset/country_year_lang_streams.csv")
