library(dplyr)

source("methods.R")

data <- read.csv("dumy.csv") %>%
  select("song_id", "position", "Text")

# TODO preprocessing
# - remove punctuation
# - lowercase
# - normalize whitespaces

# calculate the n-gram frequency vectors
vec <- vectorize(data$Text)

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
