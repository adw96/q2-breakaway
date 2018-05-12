#!/usr/bin/env Rscript

###################################################
# This R script ...
#
# Ex: Rscript run_new_richness.R ...
####################################################

####################################################
#             DESCRIPTION OF ARGUMENTS             #
####################################################
# NOTE: ALL ARGUMENTS ARE POSITIONAL!
#
### FILE SYSTEM ARGUMENTS ###
#
# 1) File path to input csv file. 
#    Ex: path/to/input_file.csv
#

cat(R.version$version.string, "\n")
args <- commandArgs(TRUE)

otu.file <- args[[1]] # "/Users/amy/qiime2-workshop/test_otus.csv"
my.metric <- args[[2]] #  "richness"
out.file <- args[[3]] # "/Users/amy/qiime2-workshop/tmp.tsv"

# otu.file <- "/Users/amy/qiime2-workshop/test_otus.csv"
# my.metric <- "shannon"

errQuit <- function(mesg, status=1) {
  message("Error: ", mesg)
  q(status=status)
}

### VALIDATE ARGUMENTS ###

# Input directory is expected to contain .fastq.gz file(s)
# that have not yet been filtered and globally trimmed
# to the same length.
if(!file.exists(otu.file)) {
  errQuit("Input file does not exist.")
} else {
  # QIIME has OTUs as rows
  otu_table <- read.table(file = otu.file, 
                          skip = 0, 
                          header = F,
                          #col.names = T
                          row.names = NULL
  )[, -1]
  colnames(otu_table) <- colnames(read.csv(otu.file, nrows=1, skip=1, sep = "\t"))[-1]
  # otu_table <- t(otu_table)
  # need check to figure out if samples are rows or columns
}


### LOAD LIBRARIES ###
# install.packages("devtools", repos='http://cran.us.r-project.org')
# devtools::install_github("adw96/breakaway")
library(breakaway)
# suppressWarnings(library(breakaway))
cat("breakaway R package version:", as.character(packageVersion("breakaway")), "\n")


### ESTIMATE DIVERSITY ###
cat("1) Estimate diversity\n")

cc <- function(x) sum(x>0)
if (my.metric == "richness") {
  ccs <- apply(otu_table, 2, cc)
  se <-  runif(length(apply(otu_table, 2, cc)))
} else if (my.metric == "shannon") {
  ccs <- runif(length(apply(otu_table, 2, cc)))
  se <-  runif(length(apply(otu_table, 2, cc)))
}

### PRINT DIVERSITY ###
cat("2) Write diversity estimates\n")
ccs2 <- data.frame(colnames(otu_table), ccs, se)
rownames(ccs2) <- NULL

write.table(ccs2, out.file, sep = "\t", 
            row.names = F, 
            col.names = c("ID", my.metric, "Se"),
            quote = F)

q(status=0)
