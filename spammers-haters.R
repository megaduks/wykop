library(ggplot2)
library(grid)
library(reshape2)

#
# draw the distribution of haters
#

data <- read.csv('haters.csv',header=TRUE)

# remove unused empty columns
data <- data[,c(1:7)]
ratio <- data$ratio
df <- data.frame(ratio)

plot <- ggplot(df, aes(x=ratio)) 
plot <- plot + geom_density()
plot <- plot + theme(legend.position="none")+xlab("ratio")+ylab("density")
plot <- plot + scale_fill_grey()

ggsave("haters.png")




#
# draw the distribution of spammers
#

data <- read.csv('spamers.csv',header=TRUE)

# remove unused empty columns
data <- data[,c(1:7)]
ratio <- data$ratio
df <- data.frame(ratio)

plot <- ggplot(df, aes(x=ratio)) 
plot <- plot + geom_density()
plot <- plot + theme(legend.position="none")+xlab("ratio")+ylab("density")
plot <- plot + 


ggsave("spammers.png")

#
# draw the boxplots for haters and spammers
#

data1 <- read.csv('haters.csv',header=TRUE)
data2 <- read.csv('spamers.csv',header=TRUE)

# remove unused empty columns
dfh <- data.frame(haters=data1$ratio)
dfs <- data.frame(spammers=data2$ratio)

ploth <- ggplot(dfh, aes(haters,y=haters))+geom_boxplot()
plots <- ggplot(dfs, aes(spammers,y=spammers))+geom_boxplot()

pushViewport(viewport(layout = grid.layout(1, 2)))
print(ploth, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plots, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

ggsave("boxplots.png")

#
# draw the sensitivity parameter distribution
#

data <- read.csv('sensitivity.csv', header=TRUE)
df <- data.frame(data$gamma, data$positive, data$negative)
names(df) <- c('gamma','positive','negative')
dfm <- melt(df,id.vars='gamma')

plot <- ggplot(dfm,aes(gamma,value,colour=variable))+geom_line() +  ylab("count") + xlab("sensitivity") + scale_colour_discrete(name="Sensitivity", breaks=c("positive","negative"), labels=c("positive","negative")) 
plot <- plot + scale_fill_grey()

ggsave("sensitivity.png")

