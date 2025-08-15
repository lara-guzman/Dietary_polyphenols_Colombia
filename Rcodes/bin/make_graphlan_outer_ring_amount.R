# Read in the food otu table
library(tidyverse)
setwd("/Users/ssojlara/Documents/R/FoodTree/Rcodes")


food_otu <- read.csv(file = "output/MiSalud.food.amount.otu.txt", sep = "\t")
food_otu <- food_otu %>%
  mutate(graphlan = taxonomy) %>%
  select(graphlan, everything())

# get the correct naming for the last level 
food_otu$graphlan <- gsub(".*?;", "", food_otu$graphlan)

# select just the columns we want
food_otu <- food_otu %>%
  select(graphlan, everything(), -taxonomy, -X.FOODID)

# if using g weight food otu table:
# drop water because it's problematic when plotting
# water <- c("Carbonated_water_unsweetened", "Water_tap", "Water_bottled_unsweetened", "Water_baby_bottled_unsweetened", "Coffee_made_from_ground_regular")
# food_otu <- food_otu %>% filter(!(graphlan %in% water))

# move graphlan to rownames
food_otu <- column_to_rownames(food_otu, var = "graphlan")

# make relative abundances
food_otu <- sweep(food_otu, 2, colSums(food_otu), "/")

# find the average for each row
ring_height <- round(rowMeans(food_otu),5) *100
food_otu <- as.data.frame(ring_height)

# move rownames back to a column
food_otu <- rownames_to_column(food_otu, var = "index")

# make a column that just says "ring_height"
food_otu$label <- "ring_height"
food_otu$ring <- 1

# reorder
food_otu <- food_otu %>% select(index, label, ring, ring_height)


# write table to txt file
write.table(food_otu, file = "/Users/ssojlara/Documents/R/FoodTree/Graphlan/annotation_amount.txt", sep= "\t", quote = F, row.names = F, col.names = F)