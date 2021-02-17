library(tidyverse)
library(data.table)
library(babynames)
library(here)
library(uwot)
library(janitor)
library(tidytext)

N_NAMES_PER_GENDER <- 2000
N_TRAITS <- 100

MODELPATH <- here("raw_data/wiki.en.vec")
ADJ_LIST <- here("raw_data/traitlist.txt")
VALENCE_PATH <- here("raw_data/Ratings_Warriner_et_al.csv")
DIST_OUTFILE <- here("shiny_app/data/name_trait_distances_2000.csv")
COORDS_OUTFILE <- here("shiny_app/data/coords_2000.csv")

valence <- read_csv(VALENCE_PATH) %>%
  clean_names() %>%
  select(word, v_mean_sum) %>%
  rename(valence = v_mean_sum)

adjs <- read_tsv(ADJ_LIST, col_names = "person_trait")

target_names <- babynames %>%
  filter(year == 2017) %>%
  group_by(sex) %>%
  arrange(-n) %>%
  slice(1:N_NAMES_PER_GENDER) %>% # take N_NAMES_PER_GENDER most frequent names 
  mutate(name = tolower(name))

# read in model
model <- fread(
  MODELPATH,
  header = FALSE,
  skip = 1,
  quote = "",
  encoding = "UTF-8",
  data.table = TRUE,
  col.names = c("word",
                unlist(lapply(2:301, function(x) paste0("V", x))))) 

target_vectors <- model %>%
  filter(word %in% c(target_names$name, adjs$person_trait)) %>%
  left_join(target_names %>% select(sex, name), by = c("word" = "name")) %>%
  mutate(type = case_when(sex == "F"  ~ "f_name",
                          sex == "M" ~ "m_name",
                           TRUE ~ "trait"),
         word = case_when(word %in% target_names$name ~
                            str_to_title(word),
                          TRUE ~ word)) %>%
  select(word, type, contains("V")) 

vecs <- target_vectors %>%
  filter(type != "trait")

# get bitmap coordinates
umap_coordinates_large <- umap(
  vecs %>% select(-word, -type),
  n_components = 2, #30
  n_neighbors = 5,
  learning_rate = .5,
  local_connectivity = 2,
  bandwidth = 1,
  repulsion_strength = 2,
  init = "random") %>%
  as_tibble() %>%
  mutate(word = vecs$word,
         type = vecs$type) %>%
  distinct(word, type, .keep_all = T) 

umap_coordinates_large_tidy <- umap_coordinates_large %>%
  mutate(type = fct_recode(type, "male" = "m_name", "female" = "f_name"))

write_csv(umap_coordinates_large_tidy, COORDS_OUTFILE)
 
get_pairwise_dist_between_words <- function(wordvecs, words){
  
  word_word_dists <- coop::cosine(t(as.matrix(wordvecs))) # this is fast
  
  wide_word_word_dists <- word_word_dists %>%
    as.data.frame(row.names = words) %>%
    setNames(., words)  %>%
    rownames_to_column(var = "w1")
  
  long_word_word_dists <- gather(wide_word_word_dists, "w2", "cos_dist", -w1)
  
  long_word_word_dists
}

vecs_unique <-  target_vectors %>% 
  distinct(word, .keep_all = T)

pairwise_word_dists <- get_pairwise_dist_between_words(vecs_unique  %>% select(-word, -type), 
                                    vecs_unique$word) 

target_pairwise_word_dists <- pairwise_word_dists %>%
  left_join(target_vectors %>% select(word, type), by = c("w1" = "word")) %>%
  rename(type1 = type) %>%
  left_join(target_vectors %>% select(word, type), by = c("w2" = "word")) %>%
  rename(type2 = type) %>%
  filter(type1 != type2,
         type2 == "trait") %>%
  left_join(valence, by = c("w2" = "word")) %>%
  distinct(w1, w2, .keep_all = T) %>%
  rename(name = "w1",
         trait = "w2") %>%
  select(name, trait, cos_dist, valence) %>%
  as.data.table()

target_trait_distances <- target_pairwise_word_dists %>%
  group_by(name) %>%
  arrange(-cos_dist) %>%
  mutate(cos_dist = round(cos_dist, 2)) %>%
  slice(1:N_TRAITS)

write_csv(target_trait_distances, DIST_OUTFILE)
  