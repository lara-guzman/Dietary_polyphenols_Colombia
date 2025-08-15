# REF: Es el mismo archivo editado de Jhonson 2019 "make.mct.tree"  10.1016/j.chom.2019.05.005
        #setwd("/Users/abby/Documents/Projects/Food_Tree/R")
        #https://github.com/knights-lab/Food_Tree/tree/master/R


#Set FT_FOLDER to your absolute local path to /Food_Tree/R
   setwd("~/R/FoodTree_All/3.FoodTree_AWS/Rcodes")
   setwd("C:/INFORMACION D/R/FoodTree_All/3.FoodTree_AWS_MISREP_Huang_2000/Rcodes")
   
#Folder lib  
  source("lib/format.foods.r")
  source("lib/check.db.r")
  source("lib/filter.db.by.diet.records.r")
  source("lib/newick.tree.r")
     
   
  source("lib/make.food.tree.r")
  source("lib/make.fiber.otu.r")
  source("lib/make.food.otu.r")
  source("lib/make.dhydrt.otu.r")
     
   
      #Polyphenols Methodology
   
   source("lib/make.Chromatography.otu.r") # Solo
   source("lib/make.Chromatography_AH.otu.r") # Chromatography after hydrolysis
   source("lib/make.TPC_Chr.otu.r") # total polyphenols content by Folin
   source("lib/make.TPC_Folin.otu.r") # total polyphenols content by Folin
   
      #Polyphenols totals by group
   
           #Total flavonoids
         source("lib/make.T.Flavonoids.otu.r") # total polyphenols content by Folin
   
        #Total phenolic acids
         source("lib/make.T.Phenolic.A.otu.r") # total polyphenols content by Folin

   
        #Other_polyphenols_chr
          source("lib/make.Other_polyphenols_chr.otu.r") # otros polifenoles por cromatografia
        #Other_polyphenols_chr
          source("lib/make.Other_polyphenols_chrah.otu.r") # otros polifenoles por cromatografia despues de hidrolisis
   
   
        #Lignans_chr
         source("lib/make.Lignans_chr.otu.r") # Lignanos por cromatogafia
        #Lignans_chrah
         source("lib/make.Lignans_chrah.otu.r") # Lignanos por cromatogafia despues de hidrolisis
   
   
        #Stilbenes_chr (no hay estilbenos por cromatografia despues de hidrolisis)
         source("lib/make.Stilbenes_chr.otu.r") 
   
   
   
                     #Polyphenols top TEN subgroups (cormatografia y croma. despues de hidrolisis- deben sumarse)
   
                                   #S.Hydroxycinnamic_acids
                                   source("lib/Subgroups/make.S.Hydroxycinnamic_acids_chr.otu.r") 
                                   source("lib/Subgroups/make.S.Hydroxycinnamic_acids_chrah.otu.r") 
                                   
                                   #S.Flavanols_chr 
                                   source("lib/Subgroups/make.S.Flavanols_chr.otu.r") 

                                   #S.Flavonols_chr
                                   source("lib/Subgroups/make.S.Flavonols_chr.otu.r") 
                                   source("lib/Subgroups/make.S.Flavonols_chrah.otu.r") 
                                   
                                   #S.Anthocyanins_chr
                                   source("lib/Subgroups/make.S.Anthocyanins_chr.otu.r") 

                                   #Flavanones 
                                   source("lib/Subgroups/make.S.Flavanones_chr.otu.r") 
                                   source("lib/Subgroups/make.S.Flavanones_chrah.otu.r") 
                                   
                                   #S.Lignans_chr 
                                   source("lib/Subgroups/make.S.Lignans_chr.otu.r") 
                                   source("lib/Subgroups/make.S.Lignans_chrah.otu.r") 
                                   
                                   
                                   #S.Hydroxybenzoic acids_chr 
                                   source("lib/Subgroups/make.S.Hydroxybenzoic.acids_chr.otu.r") 
                                   source("lib/Subgroups/make.S.Hydroxybenzoic.acids_chrah.otu.r") 
                                   
                                   #S.Flavones_chr
                                   source("lib/Subgroups/make.S.Flavones_chr.otu.r") 
                                   source("lib/Subgroups/make.S.Flavones_chrah.otu.r") 
                                   
                                   #S.Alkylphenols_chr
                                   source("lib/Subgroups/make.S.Alkylphenols_chr.otu.r") 

   
                                   #Curcuminoids_chr 
                                   source("lib/Subgroups/make.S.Curcuminoids_chr.otu.r") 

   
      
#FORMAT
# FoodID variable generation (Foodcode+ModCode) and verification of codes
   setwd("C:/INFORMACION D/R/FoodTree_All/3.FoodTree_AWS_MISREP_Huang_2000")
format.foods(input_fn="raw_data/all.food.desc.final.txt", output_fn="Rcodes/data/FNDDS_Foods_and_Beverages.txt")
format.foods(input_fn="raw_data/Items_to_use_UTF8.txt", output_fn="Rcodes/data/dietrecords.txt", dedupe=F)

writeLines(iconv(readLines("raw_data/Items_to_use.txt", encoding = "latin1"), 
                 from = "latin1", to = "UTF-8"), 
           "raw_data/Items_to_use_UTF8.txt")



#Check 
   check.db(food_database_fn = "Rcodes/data/FNDDS_Foods_and_Beverages.txt", food_records_fn="Rcodes/data/dietrecords.txt", output_fn="Rcodes/data/missing.txt")


#filter
#limit to just the foods reported in this study
  filter.db.by.diet.records(food_database_fn = "Rcodes/data/FNDDS_Foods_and_Beverages.txt", 
                            food_records_fn = "Rcodes/data/dietrecords.txt",
                            output_fn = "Rcodes/data/FoodTree.txt")
  

#Tree and taxonomy files generation
  #make reduced food tree
  make.food.tree(nodes_fn="Rcodes/data/NodeLabels.txt", # NodelLabels (Original file from 10.1016/j.chom.2019.05.005)
                 food_database_fn="Rcodes/data/FoodTree.txt", 
                 output_tree_fn="Rcodes/output/tree.xml", 
                 output_taxonomy_fn = "Rcodes/output/taxonomy.txt")



#FIBER
# this makes the food otu table with data in grams of fiber per food

    make.fiber.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                   output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/fiber.otu.txt")


#Food Amount
# this makes the standard food otu table with data in gram weights of food
    make.food.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                         output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/food_amount.otu.txt")
       
        
#total polyphenols content by Folin (TPC_F)
      # this makes the food otu table with data in milligrams of phenol per food
    
    
    #Por metodlog?a
    
          #Solo Cromatograf?a 
          make.Chromatography.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                             output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Chromatography.otu.txt")
    
          
          #Cromatografia despues de hidrolisis 
          make.Chromatography_AH.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                             output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Chromatography_AH.otu.txt")
          
    
          #Total Cromatografia + Cromatografia despues de hidrolisis 
          make.TPC_Chr.otu(food_records_fn="Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="Rcodes/output/taxonomy.txt", 
                           output_fn = "Rcodes/output/TPC_Chr.otu.txt")
          
       
          #FOLIN
          make.TPC_Folin.otu(food_records_fn="Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="Rcodes/output/taxonomy.txt", 
                            output_fn = "Rcodes/output/TPC_Folin.otu.txt")
             
      
    #Total grupos
         
             
        #Total Flavonoids  
           make.T.Flavonoids.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                           output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/T.Flavonoids.otu.txt")
          
        #Total Phenolic Acids
           make.T.Phenolic.A.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/T.Phenolic.A.otu.txt")
          
        #Other_polyphenols_chr
           
           #Other_polyphenols_chr
           make.Other_polyphenols_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                 output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Other_polyphenols_chr.otu.txt")
           
           #Other_polyphenols_chrah
           make.Other_polyphenols_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                 output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Other_polyphenols_chrah.otu.txt")
           
        #Lignanos
           #Lignans_chr 
           make.Lignans_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                 output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Lignans_chr.otu.txt")
           
           #Lignans_chrah
           make.Lignans_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                 output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Lignans_chrah.otu.txt")
           
           
        #Estilbenos
           make.Stilbenes_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                 output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Stilbenes_chr.otu.txt")
           
           
             
    #Total subgrupos
           
              
                   #S.Hydroxycinnamic_acids 
                   make.S.Hydroxycinnamic_acids_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Hydroxycinnamic_acids_chr.otu.txt")
                   make.S.Hydroxycinnamic_acids_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Hydroxycinnamic_acids_chrah.otu.txt")
                   
                   #S.Flavanols
                   make.S.Flavanols_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Flavanols_chr.otu.txt")
                   

                   #S.Flavonols 
                   make.S.Flavonols_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Flavonols_chr.otu.txt")
                   make.S.Flavonols_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Flavonols_chrah.otu.txt")

                   #S.Anthocyanins
                   make.S.Anthocyanins_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Anthocyanins_chr.otu.txt")
                   
                   #Flavanones
                   make.S.Flavanones_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Flavanones_chr.otu.txt")
                   make.S.Flavanones_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Flavanones_chrah.otu.txt")
                   
                
                  #S.Lignans 
                   make.S.Lignans_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Lignans_chr.otu.txt")
                   make.S.Lignans_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Lignans_chrah.otu.txt")
                   
                   #Hydroxybenzoic acids 
                   make.S.Hydroxybenzoic.acids_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Hydroxybenzoic_acids__chr.otu.txt")
                   make.S.Hydroxybenzoic.acids_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Hydroxybenzoic_acids_chrah.otu.txt")
                   
                   
                   #Flavones 
                   make.S.Flavones_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Flavones_chr.otu.txt")
                   make.S.Flavones_chrah.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Flavones_chrah.otu.txt")
                   
                   
                   
                   #Alkylphenols 
                   make.S.Alkylphenols_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Alkylphenols_chr.otu.txt")

                   
                   #Curcuminoids
                   make.S.Curcuminoids_chr.otu(food_records_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/data/dietrecords.txt", food_record_id = "X.SampleID", food_taxonomy_fn="~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/taxonomy.txt", 
                                    output_fn = "~/R/FoodTree_All/3.FoodTree_AWS/Rcodes/output/Subgroups/S.Curcuminoids_chr.txt")
                   
                   
