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

