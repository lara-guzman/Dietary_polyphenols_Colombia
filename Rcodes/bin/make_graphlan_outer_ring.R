# Read in the food otu table
library(tidyverse)
setwd("C:/INFORMACION D/R/FoodTree_All/3.FoodTree_AWS_MISREP_Huang_2000")

#Para calcular medianas y SD explorar estos comandos 
#rowMedians
#rowSds
# del pquete library(matrixStats)


  food_otu <- read.csv(file = "Rcodes/output/TPC_Folin.otu.txt", sep = "\t") #Replace by your food component of interest  (fiber, phenols, vitamins, food amount etc)### PILAS CON EL NOMBRE DEL ARCHIVO AL FINAL
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
#food_otu <- sweep(food_otu, 10, colSums(food_otu), "/")
#food_otu <- sweep(food_otu, 2, colSums(food_otu), "/") #OPCIONAL DE ABUNDANCIA RELATIVA

# find the average for each row
ring_height <- round(rowMeans(food_otu)/20,5)  #(Factor final para expresar los datos. En este caso fue para la visualizacion en el graphlan. En otros situaciones s puede usar para expresar el valor fina ej. *100 gramos.)
food_otu <- as.data.frame(ring_height)

# move rownames back to a column
food_otu <- rownames_to_column(food_otu, var = "index")

# make a column that just says "ring_height"
food_otu$label <- "ring_height"
food_otu$ring <- 1

# reorder
food_otu <- food_otu %>% select(index, label, ring, ring_height)


# write table to txt file
write.table(food_otu, file = "Graphlan/Annotations/annotation_TPC_Folin.txt", sep= "\t", quote = F, row.names = F, col.names = F)