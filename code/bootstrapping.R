plot_data <- read.csv("../Data/output/filtered_output/normalized_metrics_1_size_500.csv")

plot(as.numeric(plot_data[1,32:40]),as.numeric(plot_data[1,53:61]))

vect <- c(12:22)
vect2 <- c(1:10)

v <- sample(vect2)
v

vect[v]
 