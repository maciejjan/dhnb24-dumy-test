# This file contains functions for the n-gram cosine similarity
# and Chinese Whispers clustering.

# Converts a string to a vector of n-grams.
ngrams <- function(string, n) {
  sapply(1:(nchar(string)-n+1), function(i) substr(string, i, i+n-1))
}

# Converts strings into a matrix of n-gram frequencies.
# `n` - the size of the n-gram (default: n = 2, i.e. bigrams)
# `dim` - the number of dimensions of the vectors, i.e. the top-`dim` n-grams
#         are considered.
# The size of the resulting matrix is: length(strings) x dim.
#
vectorize <- function(strings, n = 2, dim = 300) {
  # calculate the n-grams for each line
  line.ngrams <- lapply(strings, function(x) ngrams(x, n))
  # determine the most frequent `dim` n-grams that will be taken into account
  relevant.ngrams <- names(sort(table(unlist(line.ngrams)), decreasing=T)[1:dim])
  # count the relevant n-grams in each line
  m <- do.call('rbind',
    lapply(line.ngrams, function(x) {
      ngram.counts <- table(na.omit(match(x, relevant.ngrams)))
      y <- rep(0, dim)
      y[as.integer(names(ngram.counts))] <- ngram.counts
      y
    })
  )
  rownames(m) <- strings
  colnames(m) <- relevant.ngrams
  m
}

# Computes the Chinese Whispers clustering from a similarity matrix.
# `m` - a similarity matrix (dense, square),
# Returns: a vector of cluster IDs per each row of the matrix `m`.
#
chinwhisp <- function(m) {
  changed <- TRUE
  clust <- 1:(nrow(m))
  iter <- 1
  # convert the matrix to list of neighbors per line for optimization purposes
  sim_idx <- lapply(
    1:nrow(sims),
    function(i) {                # for each line
      y <- which(sims[i,] > 0)   # get a list of neighbors
      y <- y[y != i]             # discard the similarity of the line to itself
      names(y) <- NULL           # discard the neighbors' names
      y
  })
  # the clustering algorithm
  while (changed) {
    message('iteration ', iter, ' ', format(Sys.time(), "%a %b %d %X %Y"))
    changed <- FALSE
    for (i in sample(1:(nrow(m)))) {   # go through the lines in random order
      # calculate the scores for each proposed new cluster
      # based on the neighbors' clusters
      v <- ave(m[i,sim_idx[[i]] ], clust[ sim_idx[[i]] ], FUN=sum)
      # if there are any proposals -> choose the best one
      if (any(v > 0)) {
        newcl <- clust[ sim_idx[[i]] ][which.max(v)]
        changed <- changed | (clust[i] != newcl)
        if (clust[i] != newcl) {
          clust[i] <- newcl
        }
      }
    }
    iter <- iter + 1
  }
  clust
}

