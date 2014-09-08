#
# Research on the textual influence between groups of users of the popular link aggregator Wykop.pl
#

# read input data
data <- read.csv(file = "wykop_comments_1-7days-no_noise.csv", header = TRUE, sep = ";")
attach(data)

# compute forward similarities across categories
aggregated.columns <- cbind(avg_sim_after1,avg_sim_after2,avg_sim_after3,avg_sim_after4,avg_sim_after5,avg_sim_after6,avg_sim_after7)
similarity.forward.by.category <- aggregate(aggregated.columns~category, data, mean)

# compute backward similarities across categories
aggregated.columns <- cbind(avg_sim_before1,avg_sim_before2,avg_sim_before3,avg_sim_before4,avg_sim_before5,avg_sim_before6,avg_sim_before7)
similarity.backward.by.category <- aggregate(aggregated.columns~category, data, mean)
