library(sp)
library(ggeffects)
library(raster)
library(SearchTrees)
library(splitstackshape)
library(cowplot)
library(nnet)
library(DescTools)
library(miceadds)
library(readxl)
library(extrafont)
library(stargazer)
library(jtools)
library(htmlwidgets)
library(ggiraph)
library(scales)
library(rgdal)
library(Matching)
library(kevostools)
library(rgeos)
library(RSQLite)
library(tidyverse)
library(data.table)

db <- dbConnect(SQLite(), "D:/national_file.db")

save <- c("db", "cleanup", "theme_bc", "save", "weighted.ttest.ci")


cleanup <- function(...){
  save2 <- c(save, ...)
  rm(list=ls(envir = .GlobalEnv)[! ls(envir = .GlobalEnv) %in% save2], envir = .GlobalEnv)
}