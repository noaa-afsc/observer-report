###################################################################################
## HELPER FILE FOR THE DESCRIPTIVE CHAPTER OF THE OBSERVER PROGRAM ANNUAL REPORT ##
###################################################################################

#############
# Libraries #
#############

if(!require("RODBC"))   install.packages("RODBC")
if(!require("ROracle"))   install.packages("ROracle")
if(!require("ggplot2"))   install.packages("ggplot2")
if(!require("RColorBrewer"))   install.packages("RColorBrewer")
#if(!require("plyr"))   install.packages("plyr")
if(!require("dplyr"))   install.packages("dplyr")
if(!require("mosaic"))   install.packages("mosaic")
if(!require("tidyr"))   install.packages("tidyr")
if(!require("forcats"))   install.packages("forcats")
if(!require("DT"))   install.packages("DT")
if(!require("data.table"))   install.packages("data.table")
if(!require("htmlwidgets"))   install.packages("htmlwidgets")
if(!require("knitr"))   install.packages("knitr")
if(!require("htmlTable"))   install.packages("htmlTable")
if(!require("flextable"))   install.packages("flextable")
if(!require("officer"))   install.packages("officer")
if(!require("toOrdinal"))   install.packages("toOrdinal")
if(!require("pander"))   install.packages("pander")
if(!require("stringr"))   install.packages("stringr")

#################
# Figure themes #
#################

fig.theme <- 
  theme_bw() +
  theme(text = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        strip.text.y = element_text(angle = 360, size = 12, face = "bold"),
        strip.text.x = element_text(angle = 360, size = 12, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.line = element_line(size = 0.5, color = "black"),
        legend.position = "bottom") 

