library(tidyverse)
source("processing/diversity_measures/asjp_get_similarity.R")

# this script takes the streaming data and generates diversity measures for each country and year

#read spotify data
df_stream = read_csv("data/final_dataset/country_year_lang_streams.csv")


# calculate streams covered for each country and year

df_stream_coverage = df_stream %>%
  group_by(country_code, year) %>%
  mutate(total_streams = sum(streams)) %>%
  filter(ISO6393 != "unknown") %>%
  summarize(streams_with_language = sum(streams), total_streams = unique(total_streams)) %>%
  mutate(stream_coverage_percent = streams_with_language / total_streams * 100) %>%
  select(country_code, year, stream_coverage_percent)


# drop unknown
df_stream = df_stream %>%
  filter(ISO6393 != "unknown")

# generate similarity measures ----------------
# pull langs from data
langs = df_stream %>%
  distinct(ISO6393) %>%
  pull()

# read similarity matrix
sim_m = read_rds("data/processed/lexical_similarity_matrix.rds")

# generate diversity
# for each year, generate q = 0, q = 1 and q = 2 for each country both naive and similarity aware
test_vec = df_stream %>%
  filter(year == 2017, country_code == "CH") %>%
  pull(streams)

get_prop_vec = function(streams){
  #takes a vector with streams and turns it into a proportion vector
  prop_vec = streams / sum(streams)
  return(prop_vec)
}

sum(get_prop_vec(test_vec))

get_richness = function(streams){
  # takes a vector with streams per language and calculates the richness
  streams = streams[streams != 0]
  richness = length(streams)
  return(richness)
}

get_richness(test_vec)


get_exp_shannon = function(streams){
  # takes a vector of proportions and calculates the exponent shannon entropy
  
  prop_vec = get_prop_vec(streams)
  prop_vec = prop_vec[prop_vec != 0]
  log_vec = log(prop_vec)
  entropy = -sum(prop_vec*log_vec)
  return(exp(entropy))
}

get_exp_shannon(test_vec)


get_inv_simp = function(prop_vec){
  # takes a vector of proportions and calculates the inverse simpson
  
  prop_vec = get_prop_vec(prop_vec)
  prop_vec = prop_vec[prop_vec != 0]
  squared_prop = prop_vec*prop_vec
  inv_simp = 1/sum(squared_prop)
  
  return(inv_simp)
}

get_inv_simp(test_vec)


test_iso = df_stream %>%
  filter(year == 2017, country_code == "CH") %>%
  pull(ISO6393)

subset_and_reorder = function(matrix, labels){
  # this function takes a matrix and labels and input and subsets the matrix to match the values in the label.
  
  matrix = matrix[labels, labels]
  
  return(matrix)
}

subset_langs = function(sim_m, langs, streams){
  
  # subset to langs found in the similarity matrix
  sim_langs = rownames(sim_m)
  
  # save the indices of the langs that exist in the similarity matrix
  valid_idx = langs %in% sim_langs
  
  # Subset langs and streams according to the indices
  langs = langs[valid_idx]
  streams = streams[valid_idx]
  return(list(langs, streams))
  
}


get_shannon_diversity = function(langs, streams, sim_m){
  # calculates diversity for q = 1 ergo shannon given a vector with proportions
  
  # subset the languages and their streams to match that of the sim vector in case not all languages are covered 
  langs_and_streams = subset_langs(sim_m, langs, streams)
  langs = langs_and_streams[[1]]
  streams = langs_and_streams[[2]]
  
  
  
  # subset sim matrix to the languages
  sim_m = subset_and_reorder(sim_m, langs)
  
  
  prop_vec = get_prop_vec(streams)
  
  # for each proportion, get the expected similarity to all other proportions
  expected = log(sim_m %*% prop_vec)
  
  # for each proportion, multiply by expected similarity to all other proportions
  # and derive entropy
  E = -1 * sum(prop_vec * expected)
  
  # exponentiate entropy to get diversity
  D = exp(E)
  
  return(D)
}
langs2 = c("swe", "eng", "deu", "nor")
test_vec2 = c(5, 5, 5, 5)

I = matrix(data = 0, nrow = length(test_vec2),
           ncol = length(test_vec2))

diag(I) = 1
colnames(I) = langs2
rownames(I) = langs2
get_shannon_diversity(langs2, test_vec2, sim_m)
get_shannon_diversity(langs2, test_vec2, I)


get_diversity_q = function(langs, streams, sim_m, q = 0){
  # a general function to implement diversity for any q
  
  # to avoid division with zero, implement shannon diversity as a special case
  if (q == 1){
    return(get_shannon_diversity(langs, streams, sim_m))
  }
  
  # subset the languages and their streams to match that of the sim vector in case not all languages are covered 
  langs_and_streams = subset_langs(sim_m, langs, streams)
  langs = langs_and_streams[[1]]
  streams = langs_and_streams[[2]]
  
  # subset sim matrix to the languages
  sim_m = subset_and_reorder(sim_m, langs)
  
  # proportion vector
  prop_vec = get_prop_vec(streams)
  
  
  # get expected similarity to all other prop for each proportion
  expected = sim_m %*% prop_vec
  
  # raise the expected similarity to the power of q-1
  expected_order = expected^(q-1)
  
  # multiply the expected similarity with each proportion and take the reciprocal
  D = (sum(prop_vec * expected_order))^(1/(1-q))
  
  return(D)
}
length(test_iso)


get_diversity_q(test_iso, test_vec, sim_m, q = 0)
get_diversity_q(test_iso, test_vec, sim_m, q = 1)
get_diversity_q(test_iso, test_vec, sim_m, q = 2)


get_naive_diversity_q = function(langs, streams, q = 0){
  # a general function to implement naive diversity for any q
  
  prop_vec = get_prop_vec(streams)
  
  I = diag(length(prop_vec))
  colnames(I) = langs
  rownames(I) = langs
  
  # get diversity
  D = get_diversity_q(langs, prop_vec, I, q)
  
  return(D)
}

get_diversity_q(test_iso, test_vec, sim_m, q = 2)
get_naive_diversity_q(test_iso, test_vec, q = 2)


div_measures = df_stream %>%
  group_by(country_code, year) %>%
  summarize(tot_streams = sum(streams),
            richness = get_richness(streams),
            exp_shannon = get_exp_shannon(streams),
            inv_simpson = get_inv_simp(streams),
            lex_div_q_0 = get_diversity_q(ISO6393, streams, sim_m = sim_m, q = 0),
            lex_div_q_1 = get_diversity_q(ISO6393, streams, sim_m = sim_m, q = 1),
            lex_div_q_2 = get_diversity_q(ISO6393, streams, sim_m = sim_m, q = 2))

# join with stream coverage
div_measures = div_measures %>%
  left_join(df_stream_coverage) %>%
  relocate(country_code, year, tot_streams, stream_coverage_percent)


# export
write_csv(div_measures, file = "data/final_dataset/diversity_measures.csv")

