##############################
## Pattern detection in fish length measurements
##
## Karl Cottenie
##
## 2020-02-19
##
##############################

library(tidyverse)
library(viridis)
library(ngram)
#library(shiny)
#library(DT)
#library(colorspace)
# + scale_color/fill_viridis(discrete = T/F)
theme_set(theme_light())

# Startup ends here

## Comment codes ------
# Coding explanations (#, often after the code, but not exclusively)
# Code organization (## XXXXX -----)
# Justification for a section of code ## XXX
# Dead end analyses because it did not work, or not pursuing this line of inquiry (but leave it in as a trace of it, to potentially solve this issue, or avoid making the same mistake in the future # (>_<) 
# Solutions/results/interpretations (#==> XXX)
# Reference to manuscript pieces, figures, results, tables, ... # (*_*)
# TODO items #TODO
# names for data frames (dfName), for lists (lsName), for vectors (vcName) (Thanks Jacqueline May)

findDuplicates <- function(somevector, min_length, ignoreAllEqual=FALSE, sep=" ") {
  # `somevector`: vector of values that needs to be tested for repeat sequences
  #  `min_length`: the minimum sequence length
  #  `sep`: in case this is a vector of strings, some separator that is not present in the data itself
  #  `ignoreAllEquall`: ignore n-grams that are repeats of one single value (trying to ignore defaults/censored data/etc)
  
  # paste all values into one string for n-gram calculation
  values_as_string <- paste(somevector, collapse= sep)
  
  # input data has to be coverted to character as well
  charactervector <- as.character(somevector)
  output <- data.frame(value=somevector, seqID = NA)
  done <- FALSE
  i <- min_length
  while (!done) {
    duplicates <- get.phrasetable(ngram(values_as_string, i, sep)) %>% 
      dplyr::filter(freq>1) %>%
      select(ngrams) %>%
      mutate(ngrams = strsplit(ngrams, " ")) # ngram() makes " " separated ngrams.
    
    if (ignoreAllEqual & nrow(duplicates)>0) duplicates <- dplyr::filter(duplicates, rle(ngrams[[1]])$lengths[[1]]<i)
    if (nrow(duplicates)>0) {
      for(k in 1:nrow(duplicates)) {
        ng <- duplicates$ngrams[[k]]
        idx <- which(charactervector == ng[1])
        seqStart <- idx[sapply(idx, function(j) all(charactervector[j:(j+(length(ng)-1))] == ng))]
        output$seqID[rep(seqStart, each = i) + 0:(i-1)] <-paste0("n",i,"Seq",k)
      }
    }  else done=TRUE
    i<-i+1
  }
  output$seqID
}

#fish_lenghts = read.csv("fish_lengths problem example for Karl.csv")[1:1500,-c(8:11)]
fish_lenghts = read.csv("fish_lengths problem example for Karl.csv")[,-c(8:11)]

fish_lenghts_dupl = fish_lenghts %>% group_by(STATION, DATE) %>% 
  dplyr::filter(n() > 10) %>% 
  mutate(ngramID = findDuplicates(LENGTH, 4, ignoreAllEqual = T))

fish_lenghts_dupl %>% View()

