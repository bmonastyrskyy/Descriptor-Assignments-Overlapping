#! /usr/bin/Rscript

# Author : Bohdan Monastyrskyy
# Date : 09-26-2016
# Description : 
#   the script visulizes distributions of statistics 
#   regrading the pair-wise overlapping of assignments

# load libraries
require("ggplot2")
require("manipulate")
require("dplyr")
library("grid")
library("gridExtra")
source("Utils.R")

# read args
args = commandArgs(trailingOnly=TRUE)
if (length(args) == 0){
 target <- "d1dwna_"
} else {
  target <- args[1]
}

# read data
df <- read.table(paste0(target, ".stat.txt"), header=FALSE, stringsAsFactors = FALSE, comment.char = '%')

# get familiar with data
summary(df)
# assign header to columns
names(df) <- c("assign1", "assign2", "no_segm", "no_res", "no_res_p", "rmsd", "hmm")

# define my plot function
my.pplot<-function(df, score){
  if (score == 'no_res'){
    p1 <- ggplot(df, aes_string(x=score)) + geom_bar() + theme(plot.margin=unit(c(0.1, 0.2, 0.1, 0.5), 'cm'))
  } else {
    p1 <- ggplot(df, aes_string(x=score)) + geom_histogram() + theme(plot.margin=unit(c(0.1, 0.2, 0.1, 1), 'cm'))
  }
  p2 <- ggplot(df, aes_string(x=score)) + geom_density() + theme(plot.margin=unit(c(0.1, 0.2, 0.1, 0.75), 'cm'))
  p3 <- ggplot(df, aes_string(y=score,  x='1')) + geom_boxplot() + coord_flip() + 
    theme(axis.text.y=element_blank(), axis.title.y=element_blank(), plot.margin=unit(c(0.1, 0.2, 0.1, 1.75), 'cm')) 
  
  print(multiplot(p1, p2, p3, cols=1))
}

# define oanother my plot function
# use no_segm parameter as a factor 
my.plot2<-function(df, score, 
                   no_res_from = 0, no_res_to = 1000, 
                   perc_res_from = 0, perc_res_to = 100, 
                   rmsd_from = 0.0, rmsd_to = 25.0){
  tmp <- df[df$no_res >= no_res_from & 
              df$no_res <= no_res_to & 
              df$no_res_p >= perc_res_from & 
              df$no_res_p <= perc_res_to &
              df$rmsd >= rmsd_from &
              df$rmsd <= rmsd_to
            ,]
  tmp$no_segm <- as.factor(tmp$no_segm)
  if (score == 'no_res'){
    p1 <- ggplot(tmp, aes_string(x=score, fill='no_segm')) + geom_bar() 
  } else {
    p1 <- ggplot(tmp, aes_string(x=score, fill='no_segm')) + geom_histogram() 
  }
  p2 <- ggplot(tmp, aes_string(x=score, fill='no_segm')) + geom_density() 
  p3 <- ggplot(tmp, aes_string(y=score,  x='1', fill='no_segm')) + geom_boxplot() + coord_flip() + 
    theme(axis.text.y=element_blank(), axis.title.y=element_blank()) 
  
  print(multiplot(p1, p2, p3, cols=1))
}

# use manipulate as a user-friendly interface
manipulate(my.plot2(df, score, no_res_from, no_res_to, 
                    perc_res_from, perc_res_to, 
                    rmsd_from, rmsd_to), 
           score=picker("no_res",  "no_res_p", "rmsd", "hmm"),
           no_res_from = slider(min=3, max=50, step = 1, initial = 3 ), 
           no_res_to = slider(min=3, max=50, step = 1, initial = 50 ), 
           perc_res_from = slider(min=0, max=100, step = 5, initial = 0 ), 
           perc_res_to = slider(min=0, max=100, step = 5, initial = 100 ), 
           rmsd_from = slider(min=0, max=25, step = 0.1, initial = 0 ), 
           rmsd_to = slider(min=0, max=25, step = 0.1, initial = 25 )
           )


# generate png files
for (score in c("no_res", "no_segm", "no_res_p", "rmsd", "hmm")){
  png(paste0(target,'.', score, '.png'), width=720, height=720);
  my.pplot(df, score)
  dev.off()
}
