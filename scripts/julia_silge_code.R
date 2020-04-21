library(tidyverse)

rankings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv")

rankings

rankings %>%
  ggplot(aes(year, points, color = gender)) +
  geom_jitter(alpha = 0.7) +
  scale_y_log10() +
  labs(
    y = "Critic rating",
    x = NULL,
    color = NULL
  )

library(spotifyr)

Sys.setenv(SPOTIFY_CLIENT_ID = spotify_client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)

access_token <- get_spotify_access_token()

playlist_features <- get_playlist_audio_features("tmock1923", "7esD007S7kzeSwVtcH9GFe")

access_token

playlist_features

pull_id <- function(query) {
  search_spotify(query, "track") %>%
    arrange(-popularity) %>%
    filter(row_number() == 1) %>%
    pull(id)
}

ranking_ids <- rankings %>%
  mutate(
    search_query = paste(title, artist),
    search_query = str_to_lower(search_query),
    search_query = str_remove(search_query, "ft.*$")
  ) %>%
  mutate(id = map_chr(search_query, possibly(pull_id, NA_character_)))

ranking_ids %>%
  select(title, artist, id)

ranking_features <- ranking_ids %>%
  mutate(id_group = row_number() %/% 80) %>%
  select(id_group, id) %>%
  nest(data = c(id)) %>%
  mutate(audio_features = map(data, ~ get_track_audio_features(.$id)))

ranking_features


ranking_df <- ranking_ids %>%
  bind_cols(ranking_features %>%
              select(audio_features) %>%
              unnest(audio_features)) %>%
  select(title, artist, points, year, danceability:tempo) %>%
  na.omit()

ranking_df

library(corrr)

ranking_df %>%
  select(year:tempo) %>%
  correlate() %>%
  rearrange() %>%
  shave() %>%
  rplot(shape = 15, colours = c("darkorange", "white", "darkcyan"))

ranking_lm <- ranking_df %>%
  select(-title, -artist) %>%
  lm(log(points) ~ ., data = .)

summary(ranking_lm)

library(tidymodels)

ranking_rec <- recipe(points ~ ., data = ranking_df) %>%
  update_role(title, artist, new_role = "id") %>%
  step_log(points) %>%
  step_normalize(all_predictors()) %>%
  step_pca(all_predictors())

ranking_prep <- prep(ranking_rec)

ranking_prep

tidied_pca <- tidy(ranking_prep, 3)

tidied_pca %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component) +
  labs(y = NULL)

library(tidytext)

tidied_pca %>%
  filter(component %in% c("PC1", "PC2", "PC3", "PC4")) %>%
  group_by(component) %>%
  top_n(6, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )

juice(ranking_prep) %>%
  ggplot(aes(PC1, PC2, label = title)) +
  geom_point(alpha = 0.2) +
  geom_text(check_overlap = TRUE, family = "IBMPlexSans")

sdev <- ranking_prep$steps[[3]]$res$sdev

percent_variation <- sdev^2 / sum(sdev^2)

tibble(
  component = unique(tidied_pca$component),
  percent_var = percent_variation ## use cumsum() to find cumulative, if you prefer
) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(component, percent_variation)) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = NULL, y = "Percent variance explained by each PCA component")

