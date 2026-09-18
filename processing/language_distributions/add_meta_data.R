library(tidyverse)
library(rnaturalearth)

# this script takes the country year lang stream data, country data and language data and generates to dataframes with metadata

df_stream = read_csv("data/final_dataset/country_year_lang_streams.csv")
df_glotto = read_csv("data/processed/glottolog.csv")
df_countries = ne_countries(scale = "medium")

df_stream %>%
  distinct(ISO6393) # 100 language (101 because of unknown)

#### countries? ####
df_stream %>%
  distinct(country_code) # 67 countries

# country codes not in natural earth
mismatch = df_stream %>%
  distinct(country_code) %>%
  filter(!country_code %in% df_countries$iso_a2_eh)

# add country_names and ISO a3
df_stream = df_stream %>%
  left_join(df_countries %>%
              select(iso_a2_eh, iso_a3_eh, name), join_by("country_code" == "iso_a2_eh"))

# countrycodes occuring multiple times
df_stream %>%
  distinct(country_code, name) %>%
  count(country_code) %>%
  filter(n > 1) # AUSTRALIA

df_stream = df_stream %>%
  as_tibble() %>%
  group_by(country_code, iso_a3_eh, year, ISO6393, streams) %>%
  summarize(name = first(name)) %>% ungroup()


# change variable names
df_stream = df_stream %>%
  rename("country_code_a3" = "iso_a3_eh",
         "country_name" = "name") %>%
  relocate(country_code, country_code_a3, country_name) %>%
  ungroup()

# check distinct codes
df_stream %>%
  distinct(country_code, country_code_a3, country_name) %>%
  count(country_code) %>%
  filter(n > 1) # no country code or names occure multiple times

# add country data
df_countries = df_countries %>%
  as_tibble() %>%
  select(name, continent, region_wb, income_grp) %>%
  rename("country_name" = name)


df_stream = df_stream %>%
  left_join(df_countries, join_by(country_name))

# check for NAs
colSums(is.na(df_stream))

df_stream %>%
  distinct(country_code) # 67

df_stream %>%
  distinct(country_code_a3) # 67

df_stream %>%
  distinct(country_name) # 67

# store country data as separate df
country_data = df_stream %>%
  distinct(country_code, country_code_a3, country_name, continent, region_wb, income_grp)

##### languages? ####
df_stream %>%
  distinct(ISO6393) # 100

df_stream %>%
  distinct(ISO6393) %>%
  filter(!ISO6393 %in% df_glotto$ISO6393)


# join with glottolog
df_glotto = df_glotto %>%
  select(language, macroarea, glottocode, ISO6393, latitude, longitude, aes, classification, family) %>%
  rename("language_name" = language)


df_stream = df_stream %>%
  left_join(df_glotto, join_by(ISO6393))

colSums(is.na(df_stream)) 

# isolates
df_stream %>%
  filter(is.na(family)) %>%
  distinct(ISO6393) # good


# macroarea
df_stream %>%
  filter(is.na(macroarea)) %>%
  distinct(ISO6393) # good

df_stream %>%
  distinct(macroarea)

df_stream %>%
  filter(macroarea == "Eurasia;Papunesia")

# check language
df_stream %>%
  filter(macroarea %in% c("Africa;Eurasia;South America", "Eurasia;Papunesia")) %>%
  distinct(ISO6393) # english and ace

# resolve macroarea
df_stream = df_stream %>%
  mutate(macroarea = case_when(language_name == "Acehnese" ~ "Papunesia",
                               language_name == "English" ~ "Eurasia",
                               TRUE ~ macroarea))

# coords
df_stream %>%
  filter(is.na(latitude)) %>%
  distinct(ISO6393, language_name) # good

# aes
df_stream %>%
  filter(is.na(aes)) %>%
  distinct(ISO6393, language_name) #norwegian official languages --> 1

df_stream = df_stream %>%
  mutate(aes = case_when(ISO6393 %in% c("nob", "nno") ~ 1,
                         TRUE ~ aes))

# adjust some know errors with aes
df_stream = df_stream %>%
  mutate(aes = case_when(ISO6393 == "ayr" ~ 1,
                         ISO6393 == "crm" ~ 2,
                         ISO6393 == "dip" ~ 1,
                         ISO6393 == "egl" ~ 3,
                         ISO6393 %in% c("bos", "hrv", "pol", "srp", "ckb", "khk", "nno", "als", "zzj", "uzn") ~ 1,
                         ISO6393 == "koi" ~ 2,
                         TRUE ~ aes))


colSums(is.na(df_stream)) # no NAs left except unknown

# store away language data in a separate df
language_data = df_stream %>%
  filter(ISO6393 != "unknown") %>%
  distinct(ISO6393, language_name, glottocode, macroarea, latitude, longitude, aes, classification, family)

# check country data
country_data %>%
  distinct(continent)

# check that all data frames can be joined
country_year_lang_streams = df_stream %>%
  select(country_code, year, ISO6393, streams)

country_year_lang_streams %>%
  left_join(country_data) %>%
  left_join(language_data)


# write all data
write_csv(country_data, "data/final_dataset/country_data.csv")
write_csv(language_data, "data/final_dataset/language_data.csv")

