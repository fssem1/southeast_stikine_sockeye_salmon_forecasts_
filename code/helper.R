# helper file for stikine inseason management

# load ----
library(tidyverse)
library(readxl)
library(broom)
library(scales)
library(extrafont)
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12, base_family='Times New Roman') +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()))


