library(tidyverse)
source("processing/diversity_measures/asjp_get_similarity.R")

##### this script generates a similarity matrix with lexical similarity between all languages in the CGLU data

# read data
df_language = read_csv("data/final_dataset/country_year_lang_streams.csv")

# extract ISO codes in ASJP
langs = df_language %>%
  filter(ISO6393 %in% df_asjp$ISO6393) %>%
  distinct(ISO6393) %>%
  pull()

# generate similarity matrix
sim_m = get_ldn_sim_matrix(langs)

# write matrix as rds
write_rds(sim_m, "data/processed/lexical_similarity_matrix.rds")
