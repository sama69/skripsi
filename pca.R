library(tidyverse)
library(tidytext)
library(broom)
library(randomForest)
data <- read_csv("korupsi.csv")
#data_clean <- data %>%
#filter(choose_one != "Can't Decide") %>%
#mutate(id = `_unit_id`,
#disaster = choose_one == "Relevant",
#text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>%
#select(id, disaster, text)

data_counts <- map_df(1:2,
                      ~ unnest_tokens(data, word, text,
                                      token = "ngrams", n = .x)) %>%
  anti_join(stop_words, by = "word")
top150 <- data_counts %>%
  count(word, sort = TRUE) %>%
  slice(1:150) %>%
  select(word)

unnested_words <- data_counts %>%
  count(id, word, sort = TRUE) %>%
  inner_join(top150, by = "word")

sparse_word_matrix <- unnested_words %>%
  cast_sparse(id, word, n)

dim(sparse_word_matrix)

word_scaled <- scale(sparse_word_matrix)
word_pca <- irlba::prcomp_irlba(word_scaled, n = 64)

meta <- tibble(id = as.numeric(dimnames(sparse_word_matrix)[[1]])) %>%
  left_join(data[!duplicated(data$id), ], by = "id")

class_df <- data.frame(word_pca$x) %>%
  mutate(response = factor(meta$label),
         split = sample(0:1, NROW(meta), replace = TRUE, prob = c(0.2, 0.8)))

model <- glm(response ~ .,
             data = filter(class_df, split == 1),
             family = binomial)
y_pred <- predict(model,
                  type = "response",
                  newdata = filter(class_df, split == 0) %>% select(-response))

y_pred_logical <- if_else(y_pred > 0.5, 1, 0)
(con <- table(y_pred, filter(class_df, split == 0) %>% pull(response)))



model <- randomForest(response ~ .,
                      data = filter(class_df, split == 1),sampsize=c(20, 30))
model

model <- randomForest(response ~ .,
                      data = filter(class_df, split == 1),maxnodes=4, ntree=30)
model

y_pred <- predict(model,
                  type = "class",
                  newdata = filter(class_df, split == 0) %>% select(-response))

(con <- table(y_pred, filter(class_df, split == 0) %>% pull(response)))
sum(diag(con)) / sum(con)
tidied_pca <- bind_cols(Tag = colnames(word_scaled),
                        tidy(word_pca$rotation)) %>%
  gather(PC, Contribution, PC1:PC4)

tidied_pca %>%
  filter(PC %in% paste0("PC", 1:4)) %>%
  ggplot(aes(Tag, Contribution, fill = Tag)) +
  geom_col(show.legend = FALSE) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "Words",
       y = "Relative importance in each principal component") +
  facet_wrap(~ PC, ncol = 2)

map_df(c(-1, 1) * 20,
       ~ tidied_pca %>%
         filter(PC == "PC1") %>%
         top_n(.x, Contribution)) %>%
  mutate(Tag = reorder(Tag, Contribution)) %>%
  ggplot(aes(Tag, Contribution, fill = Tag)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(x = "Words",
       y = "Relative importance in principle component",
       title = "PC1")

map_df(c(-1, 1) * 20,
       ~ tidied_pca %>%
         filter(PC == "PC4") %>%
         top_n(.x, Contribution)) %>%
  mutate(Tag = reorder(Tag, Contribution)) %>%
  ggplot(aes(Tag, Contribution, fill = Tag)) +
  geom_col(show.legend = FALSE, alpha = 0.8) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.ticks.x = element_blank()) +
  labs(x = "Words",
       y = "Relative importance in principle component",
       title = "PC4")

