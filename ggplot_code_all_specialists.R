## A code for modeling the ecolutionary dynamics of müllerian mimicry species 
# when all species are habitat specialists

#using the data.frame generate in "code_all_specialists.R"

#For Graphs in ggplot 

library(reshape2)
library(ggplot2)
library(tidyr)

fenotipo.df = data.frame(fenotipo[1:100,])
names(fenotipo.df) <- paste(rep(LETTERS[1:10], each = 5), 1:5, sep = "_")
m_fen = melt(fenotipo.df)
m_fen$gen = rep(1:100, 5*10)
m_fen = separate(m_fen, variable, c("habitat", "species"))
names(m_fen)[3] <- "z"

teta.df = data.frame(teta = unique(teta.mod),
                     habitat = LETTERS[1:10])

ggplot(m_fen, aes(gen, z, 
                  group = interaction(habitat, species), 
                  color = habitat)) + 
  geom_line() +
  geom_hline(data = teta.df, aes(yintercept = teta, color = habitat), linetype = 2)+
  theme_classic() +
  theme(axis.text = element_text(size = 12)) 