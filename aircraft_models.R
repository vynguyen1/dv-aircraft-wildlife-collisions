machines <- unique(birds[,c("atype", "num_engs", "ac_mass")])
sapply(machines, function(x) sum(is.na(x)))

unique(unique(machines)[,1]) #one machine with different uniques

which(is.na(unique(birds[,c("atype", "num_engs", "ac_mass")]))) #19 machines with na

machines[is.na(machines$num_engs) | is.na(machines$ac_mass),]

na_machines <- machines[is.na(machines$num_engs) | is.na(machines$ac_mass),1]
na_machines$atype

birds[birds$atype %in% na_machines$atype,]


#check unknown as type
birds[birds$atype=="UNKNOWN",]
birds[birds$atype=="HELICOPTER",]
birds[birds$atype=="MILITARY",]
birds[birds$atype=="HOMEBUILT",]

birds[birds$operator=="PRIVATELY OWNED",]
birds[birds$operator=="GOVERNMENT",]

operator <- unique(birds[,"operator"])
grep(".*CHARTER.*", operator$operator)
operator[grep(".*CHARTER.*", operator$operator),]$operator
birds[birds$operator %in% operator[grep(".*CHARTER.*", operator$operator),]$operator, ]
birds[grep(".*EXPRESS.*", birds$operator),]
plot(machines$ac_mass, machines$num_engs )

#create aircraft statistics
install.packages("dplyr")
install.packages("openintro")
require(dplyr)
require(openintro)
data("birds")

df <- birds[!is.na(birds$height) & !is.na(birds$speed),]
machines <- df %>%
  group_by(atype) %>%
  summarise(max_speed = max(speed),
            max_height = max(height),
            num_engs = median(as.integer(num_engs)),
            ac_mass = median(as.integer(ac_mass)))

machines <- as.data.frame(machines)

machines$num_engs <- factor(machines$num_engs, levels=1:4)
machines$ac_mass <- factor(machines$ac_mass, levels=1:5)

machines$ac_mass2 <- ifelse(as.integer(machines$ac_mass)<3,1,ifelse(as.integer(machines$ac_mass)==3,2,3))
machines$ac_mass2 <- factor(machines$ac_mass2, levels=1:3)  
  ## ac_mass2 level 1: 0-5700, 2: 5701-27000 kg, 3:  more than 27001

table(machines$ac_mass2)
table(machines$ac_mass)

ggplot(machines[!is.na(machines$ac_mass),]) +
  aes(x=max_speed, y=max_height, color = ac_mass) +
  geom_point()

ggplot(machines[!is.na(machines$num_engs),]) +
  aes(x=max_speed, y=max_height, color = num_engs) +
  geom_point()

ggplot(machines[!is.na(machines$ac_mass),]) +
  aes(x=max_height, y=max_speed, color = ac_mass) +
  geom_point()

ggplot(machines[!is.na(machines$ac_mass),]) +
  aes(x=max_height, y=max_speed, color = ac_mass2) +
  geom_point() +
  #geom_smooth(method = lm, se = FALSE)
  geom_smooth()

ggplot(machines[!is.na(machines$ac_mass2),]) +
 aes(x = max_speed, group=ac_mass2,fill= ac_mass2) +
 geom_density(adjust=1.5,alpha=.8)

ggplot(machines[!is.na(machines$ac_mass2),]) +
  aes(x = max_height, group=ac_mass2,fill= ac_mass2) +
  geom_density(adjust=1.5,alpha=.8)


#3D plot or multipple 2D

xlim <- range(machines$max_speed)
ylim <- range(machines$max_height)
machines_c <- machines[complete.cases(machines),]

par(mfrow=c(2,2))
plot(machines_c[machines_c$num_engs==1,"max_speed"], machines_c[machines_c$num_engs==1,"max_height"], xlim = xlim, ylim=ylim)
plot(machines_c[machines_c$num_engs==2,"max_speed"], machines_c[machines_c$num_engs==2,"max_height"], xlim = xlim, ylim=ylim)
plot(machines_c[machines_c$num_engs==3,"max_speed"], machines_c[machines_c$num_engs==3,"max_height"], xlim = xlim, ylim=ylim)
plot(machines_c[machines_c$num_engs==4,"max_speed"], machines_c[machines_c$num_engs==4,"max_height"], xlim = xlim, ylim=ylim)
par(mfrow=c(1,1))

#multiple 2d ggplot
p <- ggplot(machines_c) +
  aes(x=max_speed, y=max_height, color = ac_mass) +
  geom_point()

p + facet_grid(. ~ num_engs)

#3d plot
require(rgl)
plot3d(machines_c[,2:4], size =5, col=machines_c[,5])


#pairs
?birds
pairs(machines[,2:5])

library(GGally)
ggpairs(machines, legend=2, mapping=ggplot2::aes(color=machines$ac_mass2), columns = 2:5) +
  theme(legend.position = "right")

#Species
#top 5 species
sort(table(birds$species))

species <- birds[birds$species %in% c("SPARROWS","GULLS", "HAWKS", "BLACKBIRDS", "DUCKS"),]
species$species <- factor(species$species) 
table(species$species)
spineplot(species$height, species$species)

df <- species[!is.na(species$height),]
plot(density(df$height))

ggplot(species[!is.na(species$height),]) +
  aes(x = height, group=species,fill=species) +
  geom_density(adjust=1.5,alpha=.8)


