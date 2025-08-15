# Goal is to create a dataframe that contains the following for each L2:
# [L2 name] annotation  *:
# [L2 name] annotation_background_color [color matching L1]
# [L2 name] annotation_font_size  6
library(dplyr)

setwd("~/R/FoodTree_All/3.FoodTree_AWS/Rcodes")
setwd("C:/INFORMACION D/R/FoodTree_All/3.FoodTree_AWS_MISREP_Huang_2000")

#Load taxonomy data
tax <- read.table(file = "Rcodes/output/taxonomy.txt", sep = "\t", header = T)


# Split levels
tax_new <- tax %>% separate(taxonomy, into = paste("L", 1:6, sep = ""), sep = ";")

# subset to keep L1 and L2
names <- tax_new %>% select(L1, L2)

# deduplicate
names <- names[!duplicated(names),]

# load colors based on L1 naming
colors <- read.table(file = "Graphlan/Annotations/L1_colors.txt", 
                     sep = "\t", 
                     header = T, 
                     comment = "",
                     stringsAsFactors = F)
col_levels <- as.vector(colors$L1)
colors$L1 <- factor(colors$L1, levels=c(col_levels), ordered = TRUE)

# ensure names$L1 is also ordered factor
names$L1 <- factor(names$L1, levels = c(col_levels), ordered = TRUE)

# merge names with colors
L2_col <- inner_join(names, colors)


# make annotation naming
L2_annot <- names
L2_annot$annotation <- "annotation"
L2_annot$var <- gsub("L2_", "", L2_annot$L2)
L2_annot$var <- gsub("_", " ", L2_annot$var)
L2_annot$var <- paste0("*:", L2_annot$var, "")

# make font variable
L2_font <- names
L2_font$annotation <- "annotation_font_size"
L2_font$var <- "6"

# make one file
L2_annotations <- rbind(L2_col, L2_annot, L2_font)

# order by L1 order
# get order
tax_new$L2 <- as.factor(tax_new$L2)
order <- tax_new %>% count(L2)

# add count to L2_annotations
L2_annotations <- inner_join(L2_annotations, order)

# order by n, then by L1
L2_annotations <- L2_annotations[order(L2_annotations$n, decreasing = TRUE),]
L2_annotations <- L2_annotations[order(L2_annotations$L1),]

# drop unused columns
L2_annotations <- L2_annotations %>% select(-c(L1, n))

# write table to annot_2
write.table(L2_annotations, "Graphlan/Annotations/annot_2.txt", sep = "\t", row.names = F, col.names = F, quote = F)


# all of this ordering is for nothing, the order of the tree seems to dictate the way it's plotted. ARGH!
# Solution for now is to turn off ordering when plotting
# Down the line, we could potentially re-order the xml file after annotation to fix the issue with letter order