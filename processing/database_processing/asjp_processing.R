library(tidyverse)

# this script processes the ASJP database to make it ready for analysis. It joins the necessary column and pivots
# it wide with all lexical concepts as columns with one row per data entry.

asjp = readRDS("data/raw/asjp_database.rds")


# extract relevant tables
tabs = asjp$tables

form_tab = tabs[1][[1]]
lang_tab = tabs[2][[1]]
param_tab = tabs[3][[1]]

# subset to relevant language columns
langs = lang_tab %>% 
  select(ID, ISO639P3code, Glottolog_Name, Macroarea, Family) 


# subset to relevant value columns
values = form_tab %>%
  select(ID, Language_ID, Parameter_ID, Value)

params = param_tab %>%
  select(ID, Concepticon_Gloss)


df = values %>% 
  left_join(params, join_by("Parameter_ID" == "ID")) %>%
  left_join(langs, join_by("Language_ID" == "ID")) %>% 
  select(-ID, -Parameter_ID)


# filter to only have the 40 word list
swadesh_40 = c(
  "I",
  "THOU",
  "WE",
  "ONE",
  "TWO",
  "PERSON",
  "FISH",
  "DOG",
  "LOUSE",
  "TREE",
  "LEAF",
  "SKIN",
  "BLOOD",
  "BONE",
  "HORN (ANATOMY)",
  "EAR",
  "EYE",
  "NOSE",
  "TOOTH",
  "TONGUE",
  "KNEE",
  "HAND",
  "BREAST",
  "LIVER",
  "DRINK",
  "SEE",
  "HEAR",
  "DIE",
  "COME",
  "SUN",
  "STAR",
  "WATER",
  "STONE",
  "FIRE",
  "PATH",
  "MOUNTAIN",
  "NIGHT",
  "FULL",
  "NEW",
  "NAME"
)

df = df %>%
  filter(Concepticon_Gloss %in% swadesh_40)

completeness = df %>% 
  group_by(Language_ID) %>%
  distinct(Concepticon_Gloss, ISO639P3code) %>%
  summarize(words = n()) %>% mutate(
    completeness = words / 40 * 100
  )


# restructure to one ID per row, with concepts as columns

# take into account: One ISO might be represented by multiple variants: 
# one concept might have multiple entries e.g. swedish en/ett

# if multiple values, take the first one
df_wide = df %>%
  pivot_wider(names_from = Concepticon_Gloss, values_from = Value, values_fn = ~first(.x))

head(df_wide)

df_wide = df_wide %>% left_join(
  completeness, join_by(Language_ID == Language_ID)) %>%
  relocate(Language_ID, ISO639P3code, words, completeness)

df_wide = df_wide %>% 
  rename("lang_id" = Language_ID, "ISO6393" = ISO639P3code, "language" = Glottolog_Name, "macroarea" = Macroarea, "family" = Family)


# discovered error with ASJP wrongly giving Jamtska SWE as iso-code
df_wide = df_wide %>%
  mutate(ISO6393 = case_when(
    lang_id == "JAMTLANDIC" ~ "jmk",
    TRUE ~ ISO6393,
  )) %>%
    mutate(language = case_when(
      lang_id == "JAMTLANDIC" ~ "Jamtlandic",
      TRUE ~ language
    ))


####### read the languages from stream data

df_stream = read_csv("data/final_dataset/country_year_lang_streams.csv")

df_stream_lang = df_stream %>%
  distinct(ISO6393)

# check what languages are not in ASJP
df_stream_lang %>%
  filter(!ISO6393 %in% df_wide$ISO6393)

# cnr = montenegrin  -> can use hbs
# ckm = Chakavski -> can use hrv
# nob = Bokmål --> adjust asjp
# est = estonian --> change iso to ekk
# vec = venetian --> use dalmatian
# nno = new norwegian --> adjust asjp
# fil = filipino -> use tagalog tlg

# add cnr
cnr = df_wide %>%
  filter(ISO6393 == "hbs") %>%
  mutate(ISO6393 = "cnr",
         language = "Montenegrin")

# add ckm
ckm = df_wide %>%
  filter(ISO6393 == "hrv") %>%
  mutate(ISO6393 = "ckm",
         language = "Chakavian")

# add veneto
vec = df_wide %>%
  filter(ISO6393 == "dlm") %>%
  mutate(ISO6393 = "vec",
         language = "Venetian")

# add fil
fil = df_wide %>%
  filter(ISO6393 == "tgl" & lang_id == "TAGALOG") %>%
  mutate(ISO6393 = "fil",
         language = "Filipino")

df_wide = rbind(df_wide, cnr, ckm, vec, fil)


# adjust norwegian
df_wide = df_wide %>%
  mutate(ISO6393 = case_when(lang_id == "NORWEGIAN_BOKMAAL" ~ "nob",
                             lang_id == "NORWEGIAN_NYNORSK_TOTEN" ~ "nno",
                             TRUE ~ ISO6393),
         language = case_when(lang_id == "NORWEGIAN_BOKMAAL" ~ "Norwegian Bokmål",
                              lang_id == "NORWEGIAN_NYNORSK_TOTEN" ~ "Norwegian nynorsk",
                              TRUE ~ ISO6393))



# subset to languages in stream data
df_wide = df_wide %>%
  filter(ISO6393 %in% df_stream$ISO6393)

# some languages have more than one entry. Some cases the main variant won't be selected by going by completeness: mandarin, arabic, dutch, and turkish -> manual selection
turkish_variants_wrong = df_wide %>%
  filter(ISO6393 == "tur") %>%
  filter(!lang_id == "TURKISH") %>%
  pull(lang_id)

chinese_variants_wrong = df_wide %>%
  filter(ISO6393 == "cmn") %>%
  filter(!lang_id == "MANDARIN") %>%
  pull(lang_id)

arabic_variants_wrong = df_wide %>%
  filter(ISO6393 == "arb") %>%
  filter(!lang_id == "STANDARD_ARABIC_2") %>%
  pull(lang_id)


dutch_variant_wrong = df_wide %>%
  filter(ISO6393 == "nld") %>%
  filter(!lang_id == "DUTCH") %>%
  pull(lang_id)

variants_to_filter = c(turkish_variants_wrong, chinese_variants_wrong, arabic_variants_wrong, dutch_variant_wrong)

df_wide = df_wide %>%
  filter(!lang_id %in% variants_to_filter)

# if multiple ISO_codes per language_ID -> keep the one with the most completeness and if there are multiple
# pick the first one
df_wide = df_wide %>%
  group_by(ISO6393) %>% slice_max(completeness, n = 1) %>%
  group_by(ISO6393) %>% slice_head(n = 1) 


# handle special symbols
param_cols = df_wide %>%
  ungroup() %>%
  select(!c(lang_id, ISO6393, words, completeness, language, macroarea, family)) %>% colnames()

# select 
df_wide = df_wide %>%
  mutate(across(param_cols, ~ gsub("\\*", "", .x))) %>%
  mutate(across(param_cols, ~ gsub('\\"', "", .x))) %>%
  mutate(across(param_cols, ~ gsub("..\\$", "", .x))) %>%
  mutate(across(param_cols, ~ gsub(".\\~", "", .x)))


# write 
write_csv(df_wide, "data/processed/asjp_wide.csv")


