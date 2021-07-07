library(ggplot2)
library(openintro)
data("birds")

######################################
## Change levels
############
factor_phase_flt<-c("Parked", "Taxi", "Take-off run", "Climb", "En Route", "Descent", "Approach", "Landing Roll")
birds$phase_of_flt<-factor(birds$phase_of_flt, levels=factor_phase_flt)

factor_struck<-c("0", "1", "2-10", "11-100", "Over 100")
birds$birds_struck<-factor(birds$birds_struck, levels=factor_struck)


######################################
## Missing values
############
table(birds$birds_struck)
sum(is.na(birds$birds_struck)) # amount of na values for birds struck: 39
sum(birds$birds_struck=="0", na.rm=T)  # 33

#get rows where birds struck is missing
df <- birds[is.na(birds$birds_struck), c("remarks", "birds_seen", "effect", "birds_struck")]

no_strike <- grepl("NOT A STRIKE|NO STRIKE|NOT A STRKE", df$remarks)
sum(no_strike)  # 19

df[no_strike,]
# set birds_struck to 0 for "no strike"
birds[is.na(birds$birds_struck),][no_strike,"birds_struck"] <- "0"

sum(is.na(birds$birds_struck))  # now 20
birds[is.na(birds$birds_struck),] 
sum(birds$birds_struck=="0", na.rm=T)  # now 52


######################################
## State - Wildlife relationship
############

# 20 most frequent species
ggplot(birds[birds$species %in% names(sort(table(birds$species), decreasing = TRUE)[1:20]), ], aes(..count.., species)) + geom_bar(aes(fill = state)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

table(birds$species, birds$state)

# Number of Gulls collisions by state
gulls_collisions <- birds[birds$species == 'GULLS',]
ggplot(gulls_collisions, aes(..count.., state)) + geom_bar(aes(fill = state)) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Species in California collisions
california_collisions<-birds[birds$state == 'CA',]
ggplot(california_collisions, aes(..count.., species)) + 
  geom_bar(aes(fill = species))

# Number of collisions per state
ggplot(data = birds[birds$birds_struck != "0" | !is.na(birds$birds_struck),], aes(..count.., state, fill = ..count..)) + 
  geom_bar() +
  scale_fill_continuous(low="grey", high="red")


# Number of collisions per state in a map
library(maps)
library(mapdata)

usa <- map_data('usa')
us.state <- map_data("state")
df.state <- as.data.frame(cbind(state.abb, state.name)); df.state
df.state$state.name <- tolower(df.state$state.name)

birds$struck_num<-unclass(birds$birds_struck)

birds.with.statenames<-merge(x = birds, y = df.state, by.x = "state", by.y = "state.abb")
filtered.birds<-birds.with.statenames[!is.na(birds.with.statenames$birds_struck),]
num.collisions.per.state<-aggregate(filtered.birds$struck_num, by=list(region=filtered.birds$state.name), FUN=sum)
dfs.merged<-merge(us.state, num.collisions.per.state, by = "region")

ggplot(data=dfs.merged, aes(x=long, y=lat, fill=x, group=group)) + 
  geom_polygon(color = "white") + 
  scale_fill_continuous(low="grey", high="red")
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)

  
ggplot(data=us.state, aes(x=long, y=lat, fill=region, group=group)) + 
  geom_polygon(color = "white") + 
  guides(fill=FALSE) + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  ggtitle('U.S. Map with States') + 
  coord_fixed(1.3)


# Kategorisierung Bird/Non-Bird ?  (Grep wo species bird beinhaltet & dann nach height; alles kleiner als z.B. 10 ist nicht Vogel)
# Look at different species (what's closer to equator)
# Eine Spezies raussuchen: mappen