## A code for modeling the ecolutionary dynamics of müllerian mimicry species 
# when there are supergeneralists (SG) species 

#using the data.frame generate in "code_sg_species.R"

#For Graphs in ggplot 

library(reshape2)
library(ggplot2)
library(tidyr)

gen <- cbind(z.sg.10, teta.sp)
fenotipo.df = data.frame(fenotipo[1:100,])
names(fenotipo.df) <- paste(rep(LETTERS[1:10], each = 5), 1:5, sep = "_")
m_fen = melt(fenotipo.df)
m_fen$gen = rep(1:100, 5*10)
m_fen$species <- rep(seq(from=1, to=50), each=100)
m_fen$teta <- rep(teta.sp, each=100)
m_fen = separate(m_fen, variable, c("habitat", "species_habitat"))
for(a in 1:5){
  m_fen$habitat[which(m_fen$species==sg[a])] <- "SG"
}

teta.df <- data.frame(teta=unique(teta.sp))
teta.df$habitat <- c(LETTERS[1:10], "SG")


ggplot(m_fen, aes(gen, value, 
                  group = interaction(habitat, species), 
                  color = habitat)) + 
  geom_line() + theme_classic() + 
  geom_hline(data = teta.df, aes(yintercept = teta, color = habitat), linetype = 2)+
  theme(axis.text = element_text(size = 12)) 