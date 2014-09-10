#
# Research on the textual influence between groups of users of the popular link aggregator Wykop.pl
#

# read data on random similarity between comments
# the file contains the similarity between a few selected comments and all other comments in the dataset
# this allows to compute the average expected similarity between any pair of comments
options(java.parameters = "-Xmx2000m")
library(xlsx)
random.comments.stats <- array(0, dim = c(4,1,6))

# second sheet contains an average similarity to a comment 'lol, robi się co raz zabawniej w tym prawodawstwie'
# this is an example of a specific comment, not too generic and not too detailed
random.comments.data <- read.xlsx2(file = "example-text-similarity.xlsx", sheetIndex = 2, startRow = 3, colIndex = c(1), colClasses = c("numeric"))
random.comments.stats[1,,] <- as.matrix(summary(random.comments.data$sim))

# third sheet contains an average similarity to a comment 'Ciekawe jak wielką gwiazdą byłby teraz Ritchie Valens'
# this is an example of a very specific comment mentioning a proper name
random.comments.data <- read.xlsx2(file = "example-text-similarity.xlsx", sheetIndex = 3, startRow = 3, colIndex = c(1), colClasses = c("numeric"))
random.comments.stats[2,,] <- as.matrix(summary(random.comments.data$sim))

# third sheet contains an average similarity to a comment 'DUPLIKAT: http://www.wykop.pl/link/1027277/manipulacja-polskich-mediow/ - główna'
# this is an example of a very generic comment with an URL embedded
random.comments.data <- read.xlsx2(file = "example-text-similarity.xlsx", sheetIndex = 4, startRow = 3, colIndex = c(1), colClasses = c("numeric"))
random.comments.stats[3,,] <- as.matrix(summary(random.comments.data$sim))

# third sheet contains an average similarity to a comment 'Coś chyba Ci nie wyszło.|Gaz gaz gaz gaz gaz gaz, gaz na ulicach|Zakop zakop zakop zakop zakop zakop, karabinem'
# this is an example of a very specific long comment without proper names
random.comments.data <- read.xlsx2(file = "example-text-similarity.xlsx", sheetIndex = 5, startRow = 5, colIndex = c(1), colClasses = c("numeric"))
random.comments.stats[4,,] <- as.matrix(summary(random.comments.data$sim))

# a summary of statistics pertaining to comment similarity
# Min | 1st Qu.| Median | Mean | 3rd Qu. | Max
random.comments.stats[,1,]

# compute means over all four comment statistics
aggregated.random.comments.stats <- colMeans(random.comments.stats[,1,])
names(aggregated.random.comments.stats) <- c("Min", "1st Q", "Median", "Mean", "3rd Q", "Max")
aggregated.random.comments.stats

# plot an empirical cumulative density function of the similarity for one of the comments
library(ggplot2)

png("ecdf.comments.png")
plot <- ggplot(random.comments.data, aes(x = sim)) + stat_ecdf(aes(colour = "red")) + 
  ggtitle("Empirical cumulative density function") + 
  labs(x = "similarity") + 
  labs(y = "probability") + 
  theme(legend.position = "none")
plot
dev.off()

################################ data analysis #######################################

# read input data
data <- read.csv(file = "wykop_comments_1-7days-no_noise.csv", header = TRUE, sep = ";")
attach(data)

# compute forward similarities across categories
aggregated.columns <- cbind(avg_sim_after1,avg_sim_after2,avg_sim_after3,avg_sim_after4,avg_sim_after5,avg_sim_after6,avg_sim_after7)
similarity.forward.by.category <- aggregate(aggregated.columns~category, data, mean)

# compute backward similarities across categories
aggregated.columns <- cbind(avg_sim_before1,avg_sim_before2,avg_sim_before3,avg_sim_before4,avg_sim_before5,avg_sim_before6,avg_sim_before7)
similarity.backward.by.category <- aggregate(aggregated.columns~category, data, mean)

# how many comments are there on average in each category
# the plot shows the histogram and the density of the number of comments by categories,
# with the vertical red line showing the mean number of comments (16144)
count.comments.by.category <- aggregate(comment_count~category, data, sum)

png("count.comments.png")
plot <- ggplot(count.comments.by.category, aes(x = comment_count)) +
  geom_density(color = "black") +
  geom_histogram(aes(y = ..density..), alpha = 0.3, fill = "blue", position = "identity") +
  ggtitle("distribution of comments across categories") +
  labs(x = "number of comments") +
  labs(y = "density") +
  geom_vline(xintercept = mean(count.comments.by.category$comment_count), colour = "red")
plot
dev.off()

# how does the similarity change between days from the publication date
# the question is: across all categories, are comments becoming less similar as the time goes by?

# backward similarity
backward.between.day.similarity <- c(1:6)
for (i in backward.between.day.similarity) {
  backward.between.day.similarity[i] <- mean(similarity.backward.by.category[,i+1]) - mean(similarity.backward.by.category[,i+2]) 
}

#forward similarity
forward.between.day.similarity <- c(1:6)
for (i in forward.between.day.similarity) {
  forward.between.day.similarity[i] <- mean(similarity.forward.by.category[,i+1]) - mean(similarity.forward.by.category[,i+2]) 
}

df <- data.frame(cbind(id = rep(c(1:6),2), melt(list(bbds = backward.between.day.similarity, fbds = forward.between.day.similarity))))

png("back.forw.similarity.change.png")
plot <- ggplot(data = df, aes(x = id, y = value, colour = L1)) + 
  geom_line() +
  scale_x_continuous(breaks = 1:6) +
  scale_color_discrete(breaks = c("bbds", "fbds"), labels = c("backward", "forward")) +
  guides(color = guide_legend(title = NULL)) +
  ggtitle("average similarity between days") + 
  labs(x = "day") +
  labs(y = "similarity difference")
plot
dev.off()

# what is the expected similarity when looking n days forward/backward? i.e. how significant is 5% change in similarity when looking
# forward and backward in time, depending on the number of days?

df1 <- data.frame(diff = abs(avg_sim_after1-avg_sim_before1)/max(avg_sim_after1,avg_sim_before1), day = "1 day")
df2 <- data.frame(diff = abs(avg_sim_after2-avg_sim_before2)/max(avg_sim_after2,avg_sim_before2), day = "2 days")
df3 <- data.frame(diff = abs(avg_sim_after3-avg_sim_before3)/max(avg_sim_after3,avg_sim_before3), day = "3 days")
df4 <- data.frame(diff = abs(avg_sim_after4-avg_sim_before4)/max(avg_sim_after4,avg_sim_before4), day = "4 days")
df5 <- data.frame(diff = abs(avg_sim_after5-avg_sim_before5)/max(avg_sim_after5,avg_sim_before5), day = "5 days")
df6 <- data.frame(diff = abs(avg_sim_after6-avg_sim_before6)/max(avg_sim_after6,avg_sim_before6), day = "6 days")
df7 <- data.frame(diff = abs(avg_sim_after7-avg_sim_before7)/max(avg_sim_after7,avg_sim_before7), day = "7 days")

library(plyr)
df.list <- list(df1, df2, df3, df4, df5, df6, df7)
df <- ldply(df.list, data.frame)

png("ecdf.similarity.png")
plot <- ggplot(df, aes(x = diff, group = day, color = day)) + stat_ecdf() + 
  ggtitle("Forward/backward similarity of comments by time span") + 
  labs(x = "similarity") + 
  labs(y = "cummulative probability")
plot
dev.off()

# the plot shows that for all time intervals seeing a change of 5% or more is very significant
# as close to 95% of all documents have the forward/backward similarity variability below 5%


### how many documents are submitted by each author depending on her/his rank

author.stats <- data.frame(table(data[which(authors_rank > 0),]$authors_rank))
author.stats <- transform(author.stats, Var1 = as.numeric(Var1))
names(author.stats) <- c("rank", "count")

png("count.documents.by.author.png")
plot <- ggplot(author.stats, aes(x = rank, y = count, group = 1)) +
  stat_smooth() +
  ggtitle("number of documents by author's rank") + 
  labs(x = "author's rank") + 
  labs(y = "count") + 
  scale_x_continuous(breaks = seq(0,5000,200))
  theme(axis.text.x = element_blank()) +
  theme(legend.position = "none")
plot
dev.off()

### let's aggregate data by author and by published/not published, and for each author let's compute its 
# dominant similarity (forward or backward)
# we will only consider ranked authors, i.e. we will ommit the authors_rank=0 items

library(scales)

# remove data on anonymous authors
clean.data <- data[authors_rank > 0, ]

# create a dataset with comments, votes, and 1-day similarities
model.data <- aggregate(cbind(clean.data$inc1,clean.data$comment_count,clean.data$votes)~authors_rank+published, clean.data, sum)
names(model.data) <- c("rank", "published", "sim.1.day", "comments", "votes")

# melt data frame according to 1-day similarity
melted.model.data <- melt(model.data, measure.vars = 3)

png("model.1.day.similarity.png")
plot <- ggplot(melted.model.data, aes(x = published, y = value), fill = variable) + 
  geom_boxplot() + 
  facet_grid(.~variable) +
  scale_y_log10() +
  ggtitle("comparison of 1-day similarity w.r.t. document visibility") + 
  labs(x = "the resource was featured on home page") + 
  labs(y = "similarity") 
plot
dev.off()

# melt data frame according to the number of comments
melted.model.data <- melt(model.data, measure.vars = 4)

png("model.comments.png")
plot <- ggplot(melted.model.data, aes(x = published, y = value), fill = variable) + 
  geom_boxplot() + 
  facet_grid(.~variable) +
  scale_y_log10() +
  ggtitle("comparison of number of comments w.r.t. document visibility") + 
  labs(x = "the resource was featured on home page") + 
  labs(y = "number of comments") 
plot
dev.off()

# melt data frame according to the number of votes
melted.model.data <- melt(model.data, measure.vars = 5)

png("model.votes.png")
plot <- ggplot(melted.model.data, aes(x = published, y = value), fill = variable) + 
  geom_boxplot() + 
  facet_grid(.~variable) +
  scale_y_log10() +
  ggtitle("comparison of number of votes w.r.t. document visibility") + 
  labs(x = "the resource was featured on home page") + 
  labs(y = "number of votes") 
plot
dev.off()
