library(dplyr)
library(stringi)

source("methods.R")

data <- read.csv("dumy.csv") %>%
  select("song_id", "position", "Text")

# preprocessing
text.cl <- data$Text %>%
  stri_replace_all_regex("\\s+", " ") %>%         # normalize spaces
  stri_trans_tolower() %>%                        # lowerase
  stri_replace_all_regex(                         # remove punctuation and accents
    "[-,–:!?.;—‒‘’“”»«()…\u0301\u0302]", "") %>%
  stri_replace_all_regex("[0-9]", "") %>%         # remove digits
  stri_trans_char("áéýó", "аеуо") %>%             # remove the precombined accents
  stri_trim_both()                                # remove leading and trailing spaces

# calculate the n-gram frequency vectors
# Looking at the bigram frequency list, we might need as many as 800
# relevant bigrams. The amount of common letter combinations seems to be
# larger than in Finnish and Estonian combined.
vec <- vectorize(text.cl, n = 2, dim = 800)

# normalize the vectors to length 1
vec <- vec / sqrt(apply(vec^2, 1, sum))

# compute the dot product (= cosine similarity)
sims <- vec %*% t(vec)

# apply a similarity threshold
sims[sims < 0.75] <- 0

# clustering
data$clust_id <- chinwhisp(sims)

# show top-10 clusters
#lapply(
#  as.integer(names(head(sort(table(data$clust_id), decreasing=T), n = 10))),
#  function(x) data[data$clust_id == x,]$Text)

### CLUSTER CO-OCCURRENCE CALCULATION ###

window <- c(-2,-1,1,2)          # the current and 2 next and 2 previous verses
data <- data %>%
  mutate(key = paste(song_id, position, sep='@'))
cooc <- data.frame(
  nro = data %>% pull(song_id) %>% rep(each=length(window)),
  pos1 = data %>% pull(position) %>% rep(each=length(window))) %>%
  mutate(pos2 = pos1 + window,
         key1 = paste(nro, pos1, sep='@'),
         key2 = paste(nro, pos2, sep='@')) %>%
  inner_join(data, by=c(key1 = 'key')) %>%
  inner_join(data, by=c(key2 = 'key'))

clust_data <- data %>%
  group_by(clust_id) %>%
  summarize(freq = n(), text = first(Text))
total = nrow(cooc)

# count the co-occurrences and calculate the significance measures
cooc.stats <- cooc %>%
  group_by(clust_id.x, clust_id.y) %>%
  summarize(n = n()) %>%
  inner_join(clust_data, by=c(clust_id.x = 'clust_id')) %>%
  inner_join(clust_data, by=c(clust_id.y = 'clust_id')) %>%
  mutate(lmi = n*log(n*total / (freq.x * freq.y)))

### VIEWING THE RESULTS

# in Rstudio:
View(cooc.stats)

# writing
write.table(cooc.stats, "cooc_dumy.csv", row.names=FALSE, sep=";")
