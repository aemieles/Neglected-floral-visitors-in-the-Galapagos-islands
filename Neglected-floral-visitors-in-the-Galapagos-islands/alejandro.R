install.packages("bipartite")
library(bipartite)
setwd("~/IMEDEA/DEPICT/Galapagos/DATOS/Ants/Neglected-floral-visitors-in-the-Galapagos-islands")

santiago = read.csv("santiago.csv", row.names=1)
fernandina = read.csv("fernandina.csv", row.names=1)
pinta = read.csv("pinta.csv", row.names=1)
santa_cruz = read.csv("santa_cruz.csv", row.names=1)
san_cristobal = read.csv("san_cristobal.csv", row.names=1)

# Specialization H2'

networklevel(santiago, index="H2")
networklevel(fernandina, index="H2")
networklevel(pinta, index="H2")
networklevel(santa_cruz, index="H2")
networklevel(san_cristobal, index="H2")

# Species Strength

specieslevel(santiago, index="species strength")
specieslevel(fernandina, index="species strength")
specieslevel(pinta, index="species strength")
specieslevel(santa_cruz, index="species strength")
specieslevel(san_cristobal, index="species strength")


# Robustness

networklevel(santiago, index="robustness")
networklevel(fernandina, index="robustness")
networklevel(pinta, index="robustness")
networklevel(santa_cruz, index="robustness")
networklevel(san_cristobal, index="robustness")


# Modularity (por red)
mod <- computeModules(web=san_cristobal, steps=1E6)
mod
nulls <- nullmodel(san_cristobal, N=500, method=4)
modules.nulls <- sapply(nulls, computeModules)
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
mean(like.nulls)
sd(like.nulls)
mod@likelihood
praw <- sum(like.nulls>(mod@likelihood)) / length(like.nulls)
ifelse(praw > 0.5, 1-praw, praw)             
(z <- (mod@likelihood - mean(like.nulls))/sd(like.nulls))  


# Weighted Nestedness (por red)
obs <- unlist(networklevel(pinta, index="weighted NODF"))
obs
nulls <- nullmodel(santiago, N=500, method=4)
null <- unlist(sapply(nulls, networklevel, index="weighted NODF"))
mean(null)
sd(null)
obs
praw <- sum(null>obs) / length(null)
ifelse(praw > 0.5, 1-praw, praw)    
(z <- (obs - mean(null))/sd(null))  



# Plot

plotweb(sortweb(santiago, sort.order="dec"), arrow="up", text.rot=90, col.high="darkorange2",col.low="darkolivegreen3", adj.high=c(0,0), y.lim=c(0,4), col.interaction="gray80", bor.col.interaction = NA, method="normal")

plotweb(sortweb(fernandina, sort.order="dec"), arrow="up", text.rot=90, col.high="darkorange2",col.low="darkolivegreen3", adj.high=c(0,0), y.lim=c(0,4), col.interaction="gray80", bor.col.interaction = NA, method="normal")


plotweb(sortweb(pinta, sort.order="dec"), arrow="up", text.rot=90, col.high="darkorange2",col.low="darkolivegreen3", adj.high=c(0,0), y.lim=c(0,4), col.interaction="gray80", bor.col.interaction = NA, method="normal")


plotweb(sortweb(santa_cruz, sort.order="dec"), arrow="up", text.rot=90, col.high="darkorange2",col.low="darkolivegreen3", adj.high=c(0,0), y.lim=c(0,4), col.interaction="gray80", bor.col.interaction = NA, method="normal")


plotweb(sortweb(san_cristobal, sort.order="dec"), arrow="up", text.rot=90, col.high="darkorange2",col.low="darkolivegreen3", adj.high=c(0,0), y.lim=c(0,4), col.interaction="gray80", bor.col.interaction = NA, method="normal")




###############################


# Species strength (Endemic versus NOn-endemic)
plant_species = read.csv("plant_species.csv", header=T)
ant_species = read.csv("ant_species.csv", header=T)
attach(plant_species)
attach(ant_species)


t.test(Plant_mean ~ Plant_status)
t.test(Ant_mean ~ Ant_status)

par(mfrow = c(1, 2))

cores <- c("darkblue", "brown")
boxplot(Plant_mean ~ Plant_status, xlab="Status", ylab="Species strength (Plants)", col=cores, outline=FALSE)

boxplot(Ant_mean ~ Ant_status, xlab="Status", ylab="Species strength (Ants)", col=cores, outline=FALSE)

networklevel(santa_cruz)
networklevel(santiago)
networklevel(san_cristobal)
networklevel(fernandina)
networklevel(pinta)


