#PACKAGE AND LIBRARIES (run twice)----
  library(factoextra) 
  library(utils) 
  library(stats) 
  library(FactoMineR) 
  library(ggfortify) 
  library(ggplot2) 
  library("corrplot") 
  library(cluster) 
  library(ggpubr) 
  library(magrittr) 
  library("NbClust") 
  library(REdaS) 
  library(DiscriMiner)
  library(corrr)
  library(tidyverse)
  library(ggraph)
  library(devtools)
  library(igraph)
  library(dplyr)
  library(missMDA)
  library(normtest)
  library(ggpubr)
  library(qvalue)
  library(Anova)
  library(FSA) #Dunn's Test; https://www.statology.org/dunns-test-in-r/
  library(car)
  library(qvalue)
  library(mice)
   library(openxlsx)

# Complete database (DB) ---- 

setwd("~/R/FoodTree_All/3.FoodTree_AWS/Database_Paper")
 attach(DB_integrated_polypenols_intake_all_MiSalud)
  DB_Polyphenols <- DB_integrated_polypenols_intake_all_MiSalud

  ###MLR 311
  
  
  
  DB_Polyphenols <- (DB_integrated_polypenols_intake_all_MiSalud_311)
  TPC_intake<- DB_integrated_polypenols_intake_all_MiSalud_311
              TPC_intake_numeric <- TPC_intake[, sapply(TPC_intake, is.numeric)]
              colnames(DB_Polyphenols)
              DB_Polyphenols$TPC_Folin_Tertil
              
              #///MLR 

              md.pattern(DB_Polyphenols,rotate.names = TRUE)
              TPC_intake_Miss<-mice(TPC_intake,m=5,maxit=50,method="pmm", seed =23109,print=FALSE)%>%complete()
              #integrate the imputated values (LDL and oxLDL)
              TPC_intake$oxLDL <- TPC_intake_Miss$oxLDL
              TPC_intake$LDL <- TPC_intake_Miss$LDL
              
              
              
              colnames(TPC_intake)
              # Paquete necesario
              library(car)
              library(openxlsx)
              
              # Base de datos
              # Asegúrate de que TPC_intake esté cargada en el entorno
              
              # Variables de interés
              
              
              # Paquete necesario
              library(car)
              library(openxlsx)
              
              # Variables de interés
              variables <- c(
                             "TPC_Folin","E_DII",
                             "Weight","BMI","Waist_Circumference","RFM",
                             "Leptina_ng_mL","Adiponectin_ug_ml","Lep_Adip",
                             "ApoB_mg_dL","HDL_mg_dL","VLDL_mg_dL","LDL_mg_dL","T.Cholesterol_mg_dL_","TGs_mg_dL",
                             "Creatinine_mg_dL_",
                             "hsCRP_mg_L",
                             "Glucose_mmol_L","Insulin_uU_mL","HbA1c","beta_cell_function","Insulin_Sensibility","HOMA_IR",
                             "Systolic_BP","Diastolic_BP",
                             "FIBER",
                             "Ca","P","Fe_total","Na","K","Mg","Zn","Cu","Mn",
                             "Vit_A_ER","B1","B2","B3","Ac_pantoenico","B6","Folic","B12","Vit_C",
                             "Prevotella","Lachnospiraceae","Pathogen","Akkermansia-Bacteroidales","Ruminococcaceae")
              
              
              # Variables que requieren suma de 0.0001
              variables_necesitan_ajuste <- c("Prevotella","Lachnospiraceae","Pathogen","Akkermansia-Bacteroidales","Ruminococcaceae")
              
              # Lista para almacenar resultados
              results_list <- list()
              
              # Proceso para cada variable
              for (var in variables) {
                # Manejo de valores no válidos
                var_data <- TPC_intake[[var]]
                
                # Sumar 0.0001 si la variable está en la lista correspondiente
                if (var %in% variables_necesitan_ajuste) {
                  var_data <- var_data + 0.0001
                }
                
                # Remover valores no válidos
                if (any(var_data <= 0, na.rm = TRUE)) {
                  cat("Advertencia: Valores no válidos en la variable", var, "- filas con valores <= 0 serán eliminadas.\n")
                  valid_rows <- which(var_data > 0 & !is.na(var_data))
                  TPC_intake <- TPC_intake[valid_rows, ]
                  var_data <- var_data[valid_rows]
                }
                
                # Transformación logarítmica
                transformed_var <- log(var_data, 2)
                
                # Crear nombre para la variable transformada
                transformed_var_name <- paste0("log_", var)
                TPC_intake[[transformed_var_name]] <- transformed_var
                
                # Modelo de regresión
                model <- lm(TPC_intake[[transformed_var_name]] ~ 
                              TPC_Folin_Tertil ) #+ City + Sex + Age_range + Actividad_fisica+ Alcohol+, Tabaquismo_st, data = TPC_intake
               

                # Análisis de varianza
                anova_result <- Anova(model)
                
                # Guardar los resultados en una lista
                results_list[[var]] <- list(
                  Model = summary(model),
                  ANOVA = anova_result
                )
              }
              
              # Exportar resultados a Excel
              wb <- createWorkbook()
              
              for (var in names(results_list)) {
                # Crear hoja para la variable
                addWorksheet(wb, var)
                
                # Obtener resumen del modelo y ANOVA
                model_summary <- results_list[[var]]$Model
                anova_summary <- results_list[[var]]$ANOVA
                
                # Escribir resultados en la hoja
                writeData(wb, var, as.data.frame(coef(model_summary)), startRow = 1, startCol = 1, colNames = TRUE)
                writeData(wb, var, anova_summary, startRow = nrow(as.data.frame(coef(model_summary))) + 3, startCol = 1, colNames = TRUE)
              }
              
              # Guardar archivo Excel
              saveWorkbook(wb, "Resultados_Regresion_TPC_intake_sin ajuste.xlsx", overwrite = TRUE)
              
              # Mensaje de finalización
              cat("Los resultados se han exportado exitosamente a 'Resultados_Regresion_TPC_intake_sin ajuste.xlsx'.\n")
              
              
              BMI_log<- log(TPC_intake$BMI,2) #transformo la variable
              
              TPC_intake_BMI_log <-lm(BMI_log~TPC_intake$TPC_Folin_Tertil )  #+ TPC_intake$City+TPC_intake$Sex+ TPC_intake$Age_range  + TPC_intake$Status.tabaquismo
              
              Anova_TPC_intake_BMI_log_lm_log<- Anova(TPC_intake_BMI_log) # y la anova
              
              

              
              #///FIN MLR score cardiometabolico
              
              
              
              
              
              
              
  
  
  
  
  
  

               # Multiple linear regression (MLR- BMI) ---- 
                             # Log transformation and adjust by sex, rage of age, and city (Origin)
                             
                             colnames(DB_Polyphenols)
                             

  #Log transformation 
  
  #Log_sex <- log(DB_Polyphenols$Sex,2)
              
  Log_Age <- log(DB_Polyphenols$Age,2)
  Log_TPC_Folin <- log(DB_Polyphenols$TPC_Folin,2)
  Log_E_DII_score <- log(1.5+DB_Polyphenols$E_DII ,2) # Se le sumo para no tener valores negativos ( el rango del score apox es de 1.8 a -2.2)
  
  Log_Weight <- log(DB_Polyphenols$Weight ,2)
  Log_BMI <- log(DB_Polyphenols$BMI ,2)
  Log_Waist_Circumference <- log(DB_Polyphenols$Waist_Circumference,2)
  Log_RFM <- log(DB_Polyphenols$RFM,2)
  
  Log_Leptina_ng_mL <- log(DB_Polyphenols$Leptina_ng_mL,2)
  Log_Adiponectin_ug_ml <- log(DB_Polyphenols$Adiponectin_ug_ml,2)
  Log_Lep_Adip <- log(DB_Polyphenols$Lep_Adip,2)
  
  Log_ApoB_mg_dL <- log(DB_Polyphenols$ApoB_mg_dL,2)
  Log_HDL_mg_dL <- log(DB_Polyphenols$HDL_mg_dL,2)
  Log_VLDL_mg_dL <- log(DB_Polyphenols$VLDL_mg_dL,2)
  Log_LDL_mg_dL <- log(DB_Polyphenols$LDL_mg_dL,2)
  Log_T.Cholesterol_mg_dL_ <- log(DB_Polyphenols$T.Cholesterol_mg_dL_,2)
  Log_TGs_mg_dL <- log(DB_Polyphenols$TGs_mg_dL,2)
  Log_Atherogenic_index <- log(DB_Polyphenols$Atherogenic_index,2)
  
  
  Log_Creatinine_mg_dL_ <- log(DB_Polyphenols$Creatinine_mg_dL_,2)
  Log_hsCRP_mg_L <- log(DB_Polyphenols$hsCRP_mg_L,2)
  
  Log_Glucose_mmol_L <- log(DB_Polyphenols$Glucose_mmol_,2)
  Log_Insulin_uU_mL <- log(DB_Polyphenols$Insulin_uU_mL ,2)
  Log_HbA1c <- log(DB_Polyphenols$HbA1c,2)
  Log_beta_cell_function <- log(DB_Polyphenols$beta_cell_function,2)
  Log_Insulin_Sensibility <- log(DB_Polyphenols$Insulin_Sensibility,2)
  Log_HOMA_IR <- log(DB_Polyphenols$HOMA_IR,2)
  
  Log_Systolic_BP <- log(DB_Polyphenols$Systolic_BP,2)
  Log_Diastolic_BP <- log(DB_Polyphenols$Diastolic_BP,2)
  
  Log_FIBER <- log(DB_Polyphenols$FIBER,2)
  
  Log_Ca <- log(DB_Polyphenols$Ca ,2)
  Log_P <- log(DB_Polyphenols$P,2)
  Log_Fe_total <- log(DB_Polyphenols$Fe_total,2)
  Log_Na <- log(DB_Polyphenols$Na,2)
  Log_K <- log(DB_Polyphenols$K, 2)
  Log_Mg <- log(DB_Polyphenols$Mg,2)
  Log_Zn <- log(DB_Polyphenols$Zn,2)
  Log_Cu <- log(DB_Polyphenols$Cu ,2)
  Log_Mn <- log(DB_Polyphenols$Mn,2)
  
  Log_Vit_A_ER <- log(DB_Polyphenols$Vit_A_ER,2)
  Log_B1 <- log(DB_Polyphenols$B1,2)
  Log_B2 <- log(DB_Polyphenols$B2,2)
  Log_B3 <- log(DB_Polyphenols$B3,2)
  Log_Ac_pantoenico <- log(DB_Polyphenols$Ac_pantoenico,2)
  Log_B6 <- log(DB_Polyphenols$B6,2)
  Log_Folic <- log(DB_Polyphenols$Folic,2)
  Log_B12 <- log(DB_Polyphenols$B12,2)
  Log_Vit_C <- log(DB_Polyphenols$Vit_C,2)
  
  Log_Prevotella <- log(0.0001+DB_Polyphenols$Prevotella,2)
  Log_Lachnospiraceae <- log(DB_Polyphenols$Lachnospiraceae,2)
  Log_Pathogen <- log(DB_Polyphenols$Pathogen,2)
  Log_Akkermansia.Bacteroidales <- log(DB_Polyphenols$Akkermansia.Bacteroidales,2)
  Log_Ruminococcaceae <- log(0.0001+DB_Polyphenols$Ruminococcaceae,2)
  
  Log_TyG_index <- log(DB_Polyphenols$TyG_index,2)
  Log_TyG_BMI <- log(DB_Polyphenols$TyG_BMI,2)
  Log_TyG_WC <- log(DB_Polyphenols$TyG_WC,2)
  Log_TyG_WHtR <- log(DB_Polyphenols$TyG_WHtR,2)
  Log_ABSI <- log(DB_Polyphenols$ABSI,2)

  
  
  ##########
  ###########
  #######lm Modelo 0 (Log_Age~DB_Polyphenols$TPC_Folin_Tertil)
  
  colnames(DB_Polyphenols)
  
  lm_log_Model0_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_Tertil)
  
  
  lm_log_Model0_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_Tertil)
  
  lm_log_Model0_Log_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Log_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Log_TyG_WCn <- lm(Log_TyG_WC~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Log_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$TPC_Folin_Tertil)
  lm_log_Model0_Log_ABSI <- lm(Log_ABSI~DB_Polyphenols$TPC_Folin_Tertil)
  
  
  
  

    
  
  
  #ANOVA-Model 0
  
  
  anova_lm_log_Model0_Age_Age <- Anova(lm_log_Model0_Age)
  anova_lm_log_Model0_TPC_Folin <- Anova(lm_log_Model0_TPC_Folin)
  anova_lm_log_lm_log_Model0_E_DII_score <- Anova(lm_log_Model0_E_DII_score)
  
  anova_lm_log_Model0_Weight <- Anova(lm_log_Model0_Weight)
  anova_lm_log_Model0_BMI <- Anova(lm_log_Model0_BMI)
  anova_lm_log_Model0_Waist_Circumference <- Anova(lm_log_Model0_Waist_Circumference)
  anova_lm_log_Model0_RFM <- Anova(lm_log_Model0_RFM)
  
  anova_lm_log_Model0_Leptina_ng_mL <- Anova(lm_log_Model0_Leptina_ng_mL)
  anova_lm_log_Model0_Adiponectin_ug_ml <- Anova(lm_log_Model0_Adiponectin_ug_ml)
  anova_lm_log_Model0_Lep_Adip <- Anova(lm_log_Model0_Lep_Adip)
  
  
  anova_lm_log_Model0_ApoB_mg_dL <- Anova(lm_log_Model0_ApoB_mg_dL)
  anova_lm_log_Model0_HDL_mg_dL <- Anova(lm_log_Model0_HDL_mg_dL)
  anova_lm_log_Model0_VLDL_mg_dL <- Anova(lm_log_Model0_VLDL_mg_dL)
  anova_lm_log_Model0_LDL_mg_dL <- Anova(lm_log_Model0_LDL_mg_dL)
  anova_lm_log_Model0_T.Cholesterol_mg_dL_ <- Anova(lm_log_Model0_T.Cholesterol_mg_dL_)
  anova_lm_log_Model0_TGs_mg_dL <- Anova(lm_log_Model0_TGs_mg_dL)
  anova_lm_log_Model0_Atherogenic_index <- Anova(lm_log_Model0_Atherogenic_index)
  
  anova_lm_log_Model0_Creatinine_mg_dL_ <- Anova(lm_log_Model0_Creatinine_mg_dL_)
  anova_lm_log_Model0_hsCRP_mg_L <- Anova(lm_log_Model0_hsCRP_mg_L)
  
  anova_lm_log_Model0_Glucose_mmol_L <- Anova(lm_log_Model0_Glucose_mmol_L)
  anova_lm_log_Model0_Insulin_uU_mL <- Anova(lm_log_Model0_Insulin_uU_mL)
  
  anova_lm_log_Model0_HbA1c <- Anova(lm_log_Model0_HbA1c)
  anova_lm_log_Model0_beta_cell_function <- Anova(lm_log_Model0_beta_cell_function)
  anova_lm_log_Model0_HOMA_IR <- Anova(lm_log_Model0_HOMA_IR)
  
  anova_lm_log_Model0_Systolic_BP <- Anova(lm_log_Model0_Systolic_BP)
  anova_lm_log_Model0_Diastolic_BP <- Anova(lm_log_Model0_Diastolic_BP)
  
  anova_lm_log_Model0_FIBERP <- Anova(lm_log_Model0_FIBER)
  anova_lm_log_Model0_Ca <- Anova(lm_log_Model0_Ca)
  anova_lm_log_Model0_P<- Anova(lm_log_Model0_P)
  anova_lm_log_Model0_Fe_total <- Anova(lm_log_Model0_Fe_total)
  anova_lm_log_Model0_NaP <- Anova(lm_log_Model0_Na)
  anova_lm_log_Model0_K <- Anova(lm_log_Model0_K)
  anova_lm_log_Model0_Mg <- Anova(lm_log_Model0_Mg)
  anova_lm_log_Model0_Zn <- Anova(lm_log_Model0_Zn)
  anova_lm_log_Model0_Cu <- Anova(lm_log_Model0_Cu)
  anova_lm_log_Model0_Mn <- Anova(lm_log_Model0_Mn)
  
  
  anova_lm_log_Model0_Vit_A_ER <- Anova(lm_log_Model0_Vit_A_ER)
  anova_lm_log_Model0_B1 <- Anova(lm_log_Model0_B1)
  anova_lm_log_Model0_B2 <- Anova(lm_log_Model0_B2)
  anova_lm_log_Model0_B3 <- Anova(lm_log_Model0_B3)
  anova_lm_log_Model0_Ac_pantoenico <- Anova(lm_log_Model0_Ac_pantoenico)
  anova_lm_log_Model0_B6 <- Anova(lm_log_Model0_B6)
  anova_lm_log_Model0_Folic <- Anova(lm_log_Model0_Folic)
  anova_lm_log_Model0_B12 <- Anova(lm_log_Model0_B12)
  anova_lm_log_Model0_Vit_C <- Anova(lm_log_Model0_Vit_C)
  
  
  anova_lm_log_Model0_Prevotella <- Anova(lm_log_Model0_Prevotella)
  anova_lm_log_Model0_Lachnospiraceae <- Anova(lm_log_Model0_Lachnospiraceae)
  anova_lm_log_Model0_Pathogen <- Anova(lm_log_Model0_Pathogen)
  anova_lm_log_Model0_Akkermansia.Bacteroidales <- Anova(lm_log_Model0_Akkermansia.Bacteroidales)
  anova_lm_log_Model0_Ruminococcaceae<- Anova(lm_log_Model0_Ruminococcaceae)
  
  anova_lm_log_Model0_Log_TyG_index <- Anova(lm_log_Model0_Log_TyG_index)
  anova_lm_log_Model0_Log_TyG_BMI <- Anova(lm_log_Model0_Log_TyG_BMI)
  anova_lm_log_Model0_Log_TyG_WCn <- Anova(lm_log_Model0_Log_TyG_WCn)
  anova_lm_log_Model0_Log_TyG_WHtR <- Anova(lm_log_Model0_Log_TyG_WHtR)
  anova_lm_log_Model0_Log_ABSI<- Anova(lm_log_Model0_Log_ABSI)
  
  
  
  

  
  
  
  # Cargar librerías
  library(car)
  library(writexl)
  
  # Lista de modelos ANOVA
  anova_Model0 <- list(
    Age_Model0 = anova_lm_log_Model0_Age_Age,
    TPC_Folin_Model0 = anova_lm_log_Model0_TPC_Folin,
    E_DII_score_Model0 = anova_lm_log_lm_log_Model0_E_DII_score,
    Weight_Model0 = anova_lm_log_Model0_Weight,
    BMI_Model0 = anova_lm_log_Model0_BMI,
    Waist_Circumference_Model0 = anova_lm_log_Model0_Waist_Circumference,
    RFM_Model0 = anova_lm_log_Model0_RFM,
    Leptina_Model0 = anova_lm_log_Model0_Leptina_ng_mL,
    Adiponectin_Model0 = anova_lm_log_Model0_Adiponectin_ug_ml,
    Lep_Adip_Model0 = anova_lm_log_Model0_Lep_Adip,
    ApoB_Model0 = anova_lm_log_Model0_ApoB_mg_dL,
    HDL_Model0 = anova_lm_log_Model0_HDL_mg_dL,
    VLDL_Model0 = anova_lm_log_Model0_VLDL_mg_dL,
    LDL_Model0 = anova_lm_log_Model0_LDL_mg_dL,
    T_Cholesterol_Model0 = anova_lm_log_Model0_T.Cholesterol_mg_dL_,
    TGs_Model0 = anova_lm_log_Model0_TGs_mg_dL,
    Atherogenic_index_Model0 = anova_lm_log_Model0_Atherogenic_index,
    Creatinine_Model0 = anova_lm_log_Model0_Creatinine_mg_dL_,
    hsCRP_Model0 = anova_lm_log_Model0_hsCRP_mg_L,
    Glucose_Model0 = anova_lm_log_Model0_Glucose_mmol_L,
    Insulin_Model0 = anova_lm_log_Model0_Insulin_uU_mL,
    HbA1c_Model0 = anova_lm_log_Model0_HbA1c,
    Beta_cell_function_Model0 = anova_lm_log_Model0_beta_cell_function,
    HOMA_IR_Model0 = anova_lm_log_Model0_HOMA_IR,
    Systolic_BP_Model0 = anova_lm_log_Model0_Systolic_BP,
    Diastolic_BP_Model0 = anova_lm_log_Model0_Diastolic_BP,
    Fiber_Model0 = anova_lm_log_Model0_FIBERP,
    Ca_Model0 = anova_lm_log_Model0_Ca,
    P_Model0 = anova_lm_log_Model0_P,
    Fe_total_Model0 = anova_lm_log_Model0_Fe_total,
    Na_Model0 = anova_lm_log_Model0_NaP,
    K_Model0 = anova_lm_log_Model0_K,
    Mg_Model0 = anova_lm_log_Model0_Mg,
    Zn_Model0 = anova_lm_log_Model0_Zn,
    Cu_Model0 = anova_lm_log_Model0_Cu,
    Mn_Model0 = anova_lm_log_Model0_Mn,
    Vit_A_ER_Model0 = anova_lm_log_Model0_Vit_A_ER,
    B1_Model0 = anova_lm_log_Model0_B1,
    B2_Model0 = anova_lm_log_Model0_B2,
    B3_Model0 = anova_lm_log_Model0_B3,
    Ac_pantoenico_Model0 = anova_lm_log_Model0_Ac_pantoenico,
    B6_Model0 = anova_lm_log_Model0_B6,
    Folic_Model0 = anova_lm_log_Model0_Folic,
    B12_Model0 = anova_lm_log_Model0_B12,
    Vit_C_Model0 = anova_lm_log_Model0_Vit_C,
    Prevotella_Model0 = anova_lm_log_Model0_Prevotella,
    Lachnospiraceae_Model0 = anova_lm_log_Model0_Lachnospiraceae,
    Pathogen_Model0 = anova_lm_log_Model0_Pathogen,
    Akkermansia_Bacteroidales_Model0 = anova_lm_log_Model0_Akkermansia.Bacteroidales,
    Ruminococcaceae_Model0 = anova_lm_log_Model0_Ruminococcaceae,
    TyG_index_Model0 = anova_lm_log_Model0_Log_TyG_index,
    TyG_BMI_Model0= anova_lm_log_Model0_Log_TyG_BMI,
    TyG_WCn_Model0= anova_lm_log_Model0_Log_TyG_WCn,
    TyG_WHtRModel0 = anova_lm_log_Model0_Log_TyG_WHtR,
    ABSI_Model0= anova_lm_log_Model0_Log_ABSI
  )
  
  

  
  
  
  # Crear un dataframe vacío para almacenar los resultados
  anova_results_Model0 <- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
  
  # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
  for (var in names(anova_Model0)) {
    model <- anova_Model0[[var]]
    
    # Verificar si el modelo tiene la columna "Pr(>F)"
    if ("Pr(>F)" %in% colnames(model)) {
      p_values <- model$`Pr(>F)`
      
      # Agregar los resultados al dataframe
      anova_results_Model0 <- rbind(anova_results_Model0, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
    }
  }
  
  # Guardar el archivo en Excel
  
  write_xlsx(anova_results_Model0, "anova_results_Model0.xlsx")
  
  # Mensaje de confirmación
  cat("✅ Archivo 'anova_results_Model10xlsx' guardado con éxito.\n")
  
  #Qvalue
  
  
  
  # Cargar librerías
  library(openxlsx)
  library(writexl)
  
  # Función para calcular q-values (ajuste FDR)
  calculate_qvalues <- function(input_file, output_file) {
    
    # Cargar los resultados desde Excel
    anova_results_Model0 <- read.xlsx(input_file)
    
    # Verificar que la columna Pr_F existe
    if (!"Pr_F" %in% colnames(anova_results_Model1)) {
      stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
    }
    
    # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
    anova_results_Model0$Q_Value <- p.adjust(anova_results_Model0$Pr_F, method = "fdr")
    
    # Guardar los resultados en un nuevo archivo Excel
    write_xlsx(anova_results_Model0, output_file)
    
    # Mensaje de confirmación
    cat("✅ Archivo con q-values guardado en:", output_file, "\n")
  }
  
  # Uso de la función
  calculate_qvalues("anova_results_model0.xlsx", "anova_results_with_qvalues_model0.xlsx")
  
  
  

  ##########
  ###########
  ########lm Modelo 1 (Log_Age~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  colnames(DB_Polyphenols)
  
  lm_log_Model1_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  
  lm_log_Model1_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
  lm_log_Model1_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)

  lm_log_Model1_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)

  lm_log_Model1_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_Tertil+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)

  lm_log_Model1_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  

  lm_log_Model1_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  
  lm_log_Model1_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  
  lm_log_Model1_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_Tertil+   DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)

  lm_log_Model1_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
  lm_log_Model1_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
  
  lm_log_Model1_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_Tertil+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  lm_log_Model1_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
  lm_log_Model1_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
  
  lm_log_Model1_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  
  lm_log_Model1_Log_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Log_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Log_TyG_WCn <- lm(Log_TyG_WC~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Log_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  lm_log_Model1_Log_ABSI <- lm(Log_ABSI~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
  
  
  #ANOVA-Model 1
  
  
 anova_lm_log_Model1_Age_Age <- Anova(lm_log_Model1_Age)
 anova_lm_log_Model1_TPC_Folin <- Anova(lm_log_Model1_TPC_Folin)
 anova_lm_log_lm_log_Model1_E_DII_score <- Anova(lm_log_Model1_E_DII_score)

 anova_lm_log_Model1_Weight <- Anova(lm_log_Model1_Weight)
 anova_lm_log_Model1_BMI <- Anova(lm_log_Model1_BMI)
 anova_lm_log_Model1_Waist_Circumference <- Anova(lm_log_Model1_Waist_Circumference)
 anova_lm_log_Model1_RFM <- Anova(lm_log_Model1_RFM)
 
 anova_lm_log_Model1_Leptina_ng_mL <- Anova(lm_log_Model1_Leptina_ng_mL)
 anova_lm_log_Model1_Adiponectin_ug_ml <- Anova(lm_log_Model1_Adiponectin_ug_ml)
 anova_lm_log_Model1_Lep_Adip <- Anova(lm_log_Model1_Lep_Adip)
 
 
 anova_lm_log_Model1_ApoB_mg_dL <- Anova(lm_log_Model1_ApoB_mg_dL)
 anova_lm_log_Model1_HDL_mg_dL <- Anova(lm_log_Model1_HDL_mg_dL)
 anova_lm_log_Model1_VLDL_mg_dL <- Anova(lm_log_Model1_VLDL_mg_dL)
 anova_lm_log_Model1_LDL_mg_dL <- Anova(lm_log_Model1_LDL_mg_dL)
 anova_lm_log_Model1_T.Cholesterol_mg_dL_ <- Anova(lm_log_Model1_T.Cholesterol_mg_dL_)
 anova_lm_log_Model1_TGs_mg_dL <- Anova(lm_log_Model1_TGs_mg_dL)
 anova_lm_log_Model1_Atherogenic_index <- Anova(lm_log_Model1_Atherogenic_index)
 
 anova_lm_log_Model1_Creatinine_mg_dL_ <- Anova(lm_log_Model1_Creatinine_mg_dL_)
 anova_lm_log_Model1_hsCRP_mg_L <- Anova(lm_log_Model1_hsCRP_mg_L)
 
 anova_lm_log_Model1_Glucose_mmol_L <- Anova(lm_log_Model1_Glucose_mmol_L)
 anova_lm_log_Model1_Insulin_uU_mL <- Anova(lm_log_Model1_Insulin_uU_mL)
 
 anova_lm_log_Model1_HbA1c <- Anova(lm_log_Model1_HbA1c)
 anova_lm_log_Model1_beta_cell_function <- Anova(lm_log_Model1_beta_cell_function)
 anova_lm_log_Model1_HOMA_IR <- Anova(lm_log_Model1_HOMA_IR)
 
 anova_lm_log_Model1_Systolic_BP <- Anova(lm_log_Model1_Systolic_BP)
 anova_lm_log_Model1_Diastolic_BP <- Anova(lm_log_Model1_Diastolic_BP)
 
 anova_lm_log_Model1_FIBERP <- Anova(lm_log_Model1_FIBER)
 anova_lm_log_Model1_Ca <- Anova(lm_log_Model1_Ca)
 anova_lm_log_Model1_P<- Anova(lm_log_Model1_P)
 anova_lm_log_Model1_Fe_total <- Anova(lm_log_Model1_Fe_total)
 anova_lm_log_Model1_NaP <- Anova(lm_log_Model1_Na)
 anova_lm_log_Model1_K <- Anova(lm_log_Model1_K)
 anova_lm_log_Model1_Mg <- Anova(lm_log_Model1_Mg)
 anova_lm_log_Model1_Zn <- Anova(lm_log_Model1_Zn)
 anova_lm_log_Model1_Cu <- Anova(lm_log_Model1_Cu)
 anova_lm_log_Model1_Mn <- Anova(lm_log_Model1_Mn)
 
 
 anova_lm_log_Model1_Vit_A_ER <- Anova(lm_log_Model1_Vit_A_ER)
 anova_lm_log_Model1_B1 <- Anova(lm_log_Model1_B1)
 anova_lm_log_Model1_B2 <- Anova(lm_log_Model1_B2)
 anova_lm_log_Model1_B3 <- Anova(lm_log_Model1_B3)
 anova_lm_log_Model1_Ac_pantoenico <- Anova(lm_log_Model1_Ac_pantoenico)
 anova_lm_log_Model1_B6 <- Anova(lm_log_Model1_B6)
 anova_lm_log_Model1_Folic <- Anova(lm_log_Model1_Folic)
 anova_lm_log_Model1_B12 <- Anova(lm_log_Model1_B12)
 anova_lm_log_Model1_Vit_C <- Anova(lm_log_Model1_Vit_C)
 

 anova_lm_log_Model1_Prevotella <- Anova(lm_log_Model1_Prevotella)
 anova_lm_log_Model1_Lachnospiraceae <- Anova(lm_log_Model1_Lachnospiraceae)
 anova_lm_log_Model1_Pathogen <- Anova(lm_log_Model1_Pathogen)
 anova_lm_log_Model1_Akkermansia.Bacteroidales <- Anova(lm_log_Model1_Akkermansia.Bacteroidales)
 anova_lm_log_Model1_Ruminococcaceae<- Anova(lm_log_Model1_Ruminococcaceae)
 
 anova_lm_log_Model1_Log_TyG_index <- Anova(lm_log_Model1_Log_TyG_index)
 anova_lm_log_Model1_Log_TyG_BMI <- Anova(lm_log_Model1_Log_TyG_BMI)
 anova_lm_log_Model1_Log_TyG_WCn <- Anova(lm_log_Model1_Log_TyG_WCn)
 anova_lm_log_Model1_Log_TyG_WHtR <- Anova(lm_log_Model1_Log_TyG_WHtR)
 anova_lm_log_Model1_Log_ABSI<- Anova(lm_log_Model1_Log_ABSI)
 
 

 


 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_Model1<- list(
   Age_Model1= anova_lm_log_Model1_Age_Age,
   TPC_Folin_Model1= anova_lm_log_Model1_TPC_Folin,
   E_DII_score_Model1= anova_lm_log_lm_log_Model1_E_DII_score,
   Weight_Model1= anova_lm_log_Model1_Weight,
   BMI_Model1= anova_lm_log_Model1_BMI,
   Waist_Circumference_Model1= anova_lm_log_Model1_Waist_Circumference,
   RFM_Model1= anova_lm_log_Model1_RFM,
   Leptina_Model1= anova_lm_log_Model1_Leptina_ng_mL,
   Adiponectin_Model1= anova_lm_log_Model1_Adiponectin_ug_ml,
   Lep_Adip_Model1= anova_lm_log_Model1_Lep_Adip,
   ApoB_Model1= anova_lm_log_Model1_ApoB_mg_dL,
   HDL_Model1= anova_lm_log_Model1_HDL_mg_dL,
   VLDL_Model1= anova_lm_log_Model1_VLDL_mg_dL,
   LDL_Model1= anova_lm_log_Model1_LDL_mg_dL,
   T_Cholesterol_Model1= anova_lm_log_Model1_T.Cholesterol_mg_dL_,
   TGs_Model1= anova_lm_log_Model1_TGs_mg_dL,
   Atherogenic_index_Model1= anova_lm_log_Model1_Atherogenic_index,
   Creatinine_Model1= anova_lm_log_Model1_Creatinine_mg_dL_,
   hsCRP_Model1= anova_lm_log_Model1_hsCRP_mg_L,
   Glucose_Model1= anova_lm_log_Model1_Glucose_mmol_L,
   Insulin_Model1= anova_lm_log_Model1_Insulin_uU_mL,
   HbA1c_Model1= anova_lm_log_Model1_HbA1c,
   Beta_cell_function_Model1= anova_lm_log_Model1_beta_cell_function,
   HOMA_IR_Model1= anova_lm_log_Model1_HOMA_IR,
   Systolic_BP_Model1= anova_lm_log_Model1_Systolic_BP,
   Diastolic_BP_Model1= anova_lm_log_Model1_Diastolic_BP,
   Fiber_Model1= anova_lm_log_Model1_FIBERP,
   Ca_Model1= anova_lm_log_Model1_Ca,
   P_Model1= anova_lm_log_Model1_P,
   Fe_total_Model1= anova_lm_log_Model1_Fe_total,
   Na_Model1= anova_lm_log_Model1_NaP,
   K_Model1= anova_lm_log_Model1_K,
   Mg_Model1= anova_lm_log_Model1_Mg,
   Zn_Model1= anova_lm_log_Model1_Zn,
   Cu_Model1= anova_lm_log_Model1_Cu,
   Mn_Model1= anova_lm_log_Model1_Mn,
   Vit_A_ER_Model1= anova_lm_log_Model1_Vit_A_ER,
   B1_Model1= anova_lm_log_Model1_B1,
   B2_Model1= anova_lm_log_Model1_B2,
   B3_Model1= anova_lm_log_Model1_B3,
   Ac_pantoenico_Model1= anova_lm_log_Model1_Ac_pantoenico,
   B6_Model1= anova_lm_log_Model1_B6,
   Folic_Model1= anova_lm_log_Model1_Folic,
   B12_Model1= anova_lm_log_Model1_B12,
   Vit_C_Model1= anova_lm_log_Model1_Vit_C,
   Prevotella_Model1= anova_lm_log_Model1_Prevotella,
   Lachnospiraceae_Model1= anova_lm_log_Model1_Lachnospiraceae,
   Pathogen_Model1= anova_lm_log_Model1_Pathogen,
   Akkermansia_Bacteroidales_Model1= anova_lm_log_Model1_Akkermansia.Bacteroidales,
   Ruminococcaceae_Model1= anova_lm_log_Model1_Ruminococcaceae,
   TyG_index_Model1 = anova_lm_log_Model1_Log_TyG_index,
   TyG_BMI_Model1= anova_lm_log_Model1_Log_TyG_BMI,
   TyG_WCn_Model1= anova_lm_log_Model1_Log_TyG_WCn,
   TyG_WHtRModel1 = anova_lm_log_Model1_Log_TyG_WHtR,
   ABSI_Model1= anova_lm_log_Model1_Log_ABSI
 )
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_Model1<- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_Model1)) {
   model <- anova_Model1[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_Model1<- rbind(anova_results_Model1, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_Model1, "anova_results_Model1.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results_Model1.xlsx' guardado con éxito.\n")
 
 #Qvalue
 


 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_Model1 <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_Model1)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_Model1$Q_Value <- p.adjust(anova_results_Model1$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_Model1, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_model1.xlsx", "anova_results_with_qvalues_model1.xlsx")
 

 

 
 
 
 
 ##########
 ###########
 ########lm Modelo 2 (Log_Age~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 colnames(DB_Polyphenols)
 
 lm_log_Model2_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin )
 lm_log_Model2_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_Tertil+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 lm_log_Model2_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_Tertil+   DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_Tertil+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_Tertil+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_Tertil+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_Log_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Log_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Log_TyG_WCn <- lm(Log_TyG_WC~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Log_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_Log_ABSI <- lm(Log_ABSI~DB_Polyphenols$TPC_Folin_Tertil + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 
 #ANOVA-Model 2
 
 
 anova_lm_log_Model2_Age_Age <- Anova(lm_log_Model2_Age)
 anova_lm_log_Model2_TPC_Folin <- Anova(lm_log_Model2_TPC_Folin)
 anova_lm_log_lm_log_Model2_E_DII_score <- Anova(lm_log_Model2_E_DII_score)
 
 anova_lm_log_Model2_Weight <- Anova(lm_log_Model2_Weight)
 anova_lm_log_Model2_BMI <- Anova(lm_log_Model2_BMI)
 anova_lm_log_Model2_Waist_Circumference <- Anova(lm_log_Model2_Waist_Circumference)
 anova_lm_log_Model2_RFM <- Anova(lm_log_Model2_RFM)
 
 anova_lm_log_Model2_Leptina_ng_mL <- Anova(lm_log_Model2_Leptina_ng_mL)
 anova_lm_log_Model2_Adiponectin_ug_ml <- Anova(lm_log_Model2_Adiponectin_ug_ml)
 anova_lm_log_Model2_Lep_Adip <- Anova(lm_log_Model2_Lep_Adip)
 
 
 anova_lm_log_Model2_ApoB_mg_dL <- Anova(lm_log_Model2_ApoB_mg_dL)
 anova_lm_log_Model2_HDL_mg_dL <- Anova(lm_log_Model2_HDL_mg_dL)
 anova_lm_log_Model2_VLDL_mg_dL <- Anova(lm_log_Model2_VLDL_mg_dL)
 anova_lm_log_Model2_LDL_mg_dL <- Anova(lm_log_Model2_LDL_mg_dL)
 anova_lm_log_Model2_T.Cholesterol_mg_dL_ <- Anova(lm_log_Model2_T.Cholesterol_mg_dL_)
 anova_lm_log_Model2_TGs_mg_dL <- Anova(lm_log_Model2_TGs_mg_dL)
 anova_lm_log_Model2_Atherogenic_index <- Anova(lm_log_Model2_Atherogenic_index)
 
 anova_lm_log_Model2_Creatinine_mg_dL_ <- Anova(lm_log_Model2_Creatinine_mg_dL_)
 anova_lm_log_Model2_hsCRP_mg_L <- Anova(lm_log_Model2_hsCRP_mg_L)
 
 anova_lm_log_Model2_Glucose_mmol_L <- Anova(lm_log_Model2_Glucose_mmol_L)
 anova_lm_log_Model2_Insulin_uU_mL <- Anova(lm_log_Model2_Insulin_uU_mL)
 
 anova_lm_log_Model2_HbA1c <- Anova(lm_log_Model2_HbA1c)
 anova_lm_log_Model2_beta_cell_function <- Anova(lm_log_Model2_beta_cell_function)
 anova_lm_log_Model2_HOMA_IR <- Anova(lm_log_Model2_HOMA_IR)
 
 anova_lm_log_Model2_Systolic_BP <- Anova(lm_log_Model2_Systolic_BP)
 anova_lm_log_Model2_Diastolic_BP <- Anova(lm_log_Model2_Diastolic_BP)
 
 anova_lm_log_Model2_FIBERP <- Anova(lm_log_Model2_FIBER)
 anova_lm_log_Model2_Ca <- Anova(lm_log_Model2_Ca)
 anova_lm_log_Model2_P<- Anova(lm_log_Model2_P)
 anova_lm_log_Model2_Fe_total <- Anova(lm_log_Model2_Fe_total)
 anova_lm_log_Model2_NaP <- Anova(lm_log_Model2_Na)
 anova_lm_log_Model2_K <- Anova(lm_log_Model2_K)
 anova_lm_log_Model2_Mg <- Anova(lm_log_Model2_Mg)
 anova_lm_log_Model2_Zn <- Anova(lm_log_Model2_Zn)
 anova_lm_log_Model2_Cu <- Anova(lm_log_Model2_Cu)
 anova_lm_log_Model2_Mn <- Anova(lm_log_Model2_Mn)
 
 
 anova_lm_log_Model2_Vit_A_ER <- Anova(lm_log_Model2_Vit_A_ER)
 anova_lm_log_Model2_B1 <- Anova(lm_log_Model2_B1)
 anova_lm_log_Model2_B2 <- Anova(lm_log_Model2_B2)
 anova_lm_log_Model2_B3 <- Anova(lm_log_Model2_B3)
 anova_lm_log_Model2_Ac_pantoenico <- Anova(lm_log_Model2_Ac_pantoenico)
 anova_lm_log_Model2_B6 <- Anova(lm_log_Model2_B6)
 anova_lm_log_Model2_Folic <- Anova(lm_log_Model2_Folic)
 anova_lm_log_Model2_B12 <- Anova(lm_log_Model2_B12)
 anova_lm_log_Model2_Vit_C <- Anova(lm_log_Model2_Vit_C)
 
 
 anova_lm_log_Model2_Prevotella <- Anova(lm_log_Model2_Prevotella)
 anova_lm_log_Model2_Lachnospiraceae <- Anova(lm_log_Model2_Lachnospiraceae)
 anova_lm_log_Model2_Pathogen <- Anova(lm_log_Model2_Pathogen)
 anova_lm_log_Model2_Akkermansia.Bacteroidales <- Anova(lm_log_Model2_Akkermansia.Bacteroidales)
 anova_lm_log_Model2_Ruminococcaceae<- Anova(lm_log_Model2_Ruminococcaceae)
 
 anova_lm_log_Model2_Log_TyG_index <- Anova(lm_log_Model2_Log_TyG_index)
 anova_lm_log_Model2_Log_TyG_BMI <- Anova(lm_log_Model2_Log_TyG_BMI)
 anova_lm_log_Model2_Log_TyG_WCn <- Anova(lm_log_Model2_Log_TyG_WCn)
 anova_lm_log_Model2_Log_TyG_WHtR <- Anova(lm_log_Model2_Log_TyG_WHtR)
 anova_lm_log_Model2_Log_ABSI <- Anova(lm_log_Model2_Log_ABSI)
 
 
 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_Model2<- list(
   Age_Model2= anova_lm_log_Model2_Age_Age,
   TPC_Folin_Model2= anova_lm_log_Model2_TPC_Folin,
   E_DII_score_Model2= anova_lm_log_lm_log_Model2_E_DII_score,
   Weight_Model2= anova_lm_log_Model2_Weight,
   BMI_Model2= anova_lm_log_Model2_BMI,
   Waist_Circumference_Model2= anova_lm_log_Model2_Waist_Circumference,
   RFM_Model2= anova_lm_log_Model2_RFM,
   Leptina_Model2= anova_lm_log_Model2_Leptina_ng_mL,
   Adiponectin_Model2= anova_lm_log_Model2_Adiponectin_ug_ml,
   Lep_Adip_Model2= anova_lm_log_Model2_Lep_Adip,
   ApoB_Model2= anova_lm_log_Model2_ApoB_mg_dL,
   HDL_Model2= anova_lm_log_Model2_HDL_mg_dL,
   VLDL_Model2= anova_lm_log_Model2_VLDL_mg_dL,
   LDL_Model2= anova_lm_log_Model2_LDL_mg_dL,
   T_Cholesterol_Model2= anova_lm_log_Model2_T.Cholesterol_mg_dL_,
   TGs_Model2= anova_lm_log_Model2_TGs_mg_dL,
   Atherogenic_index_Model2= anova_lm_log_Model2_Atherogenic_index,
   Creatinine_Model2= anova_lm_log_Model2_Creatinine_mg_dL_,
   hsCRP_Model2= anova_lm_log_Model2_hsCRP_mg_L,
   Glucose_Model2= anova_lm_log_Model2_Glucose_mmol_L,
   Insulin_Model2= anova_lm_log_Model2_Insulin_uU_mL,
   HbA1c_Model2= anova_lm_log_Model2_HbA1c,
   Beta_cell_function_Model2= anova_lm_log_Model2_beta_cell_function,
   HOMA_IR_Model2= anova_lm_log_Model2_HOMA_IR,
   Systolic_BP_Model2= anova_lm_log_Model2_Systolic_BP,
   Diastolic_BP_Model2= anova_lm_log_Model2_Diastolic_BP,
   Fiber_Model2= anova_lm_log_Model2_FIBERP,
   Ca_Model2= anova_lm_log_Model2_Ca,
   P_Model2= anova_lm_log_Model2_P,
   Fe_total_Model2= anova_lm_log_Model2_Fe_total,
   Na_Model2= anova_lm_log_Model2_NaP,
   K_Model2= anova_lm_log_Model2_K,
   Mg_Model2= anova_lm_log_Model2_Mg,
   Zn_Model2= anova_lm_log_Model2_Zn,
   Cu_Model2= anova_lm_log_Model2_Cu,
   Mn_Model2= anova_lm_log_Model2_Mn,
   Vit_A_ER_Model2= anova_lm_log_Model2_Vit_A_ER,
   B1_Model2= anova_lm_log_Model2_B1,
   B2_Model2= anova_lm_log_Model2_B2,
   B3_Model2= anova_lm_log_Model2_B3,
   Ac_pantoenico_Model2= anova_lm_log_Model2_Ac_pantoenico,
   B6_Model2= anova_lm_log_Model2_B6,
   Folic_Model2= anova_lm_log_Model2_Folic,
   B12_Model2= anova_lm_log_Model2_B12,
   Vit_C_Model2= anova_lm_log_Model2_Vit_C,
   Prevotella_Model2= anova_lm_log_Model2_Prevotella,
   Lachnospiraceae_Model2= anova_lm_log_Model2_Lachnospiraceae,
   Pathogen_Model2= anova_lm_log_Model2_Pathogen,
   Akkermansia_Bacteroidales_Model2= anova_lm_log_Model2_Akkermansia.Bacteroidales,
   Ruminococcaceae_Model2= anova_lm_log_Model2_Ruminococcaceae,
   TyG_index_Model2 = anova_lm_log_Model2_Log_TyG_index,
   TyG_BMI_Model2= anova_lm_log_Model2_Log_TyG_BMI,
   TyG_WCn_Model2= anova_lm_log_Model2_Log_TyG_WCn,
   TyG_WHtRModel2 = anova_lm_log_Model2_Log_TyG_WHtR,
   ABSI_Model2= anova_lm_log_Model2_Log_ABSI
 )
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_Model2<- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_Model2)) {
   model <- anova_Model2[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_Model2<- rbind(anova_results_Model2, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_Model2, "anova_results_Model2.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results_Model2.xlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_Model2 <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_Model2)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_Model2$Q_Value <- p.adjust(anova_results_Model2$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_Model2, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_Model2.xlsx", "anova_results_with_qvalues_model2.xlsx")
 
 
 
 
 ########
 ################
 ################
 ################
 ########################    MLR E-DII
 
 #######lm Modelo 0 (Log_Age~DB_Polyphenols$E_DII_Tertiles)
 
 colnames(DB_Polyphenols)
 
 lm_log_EDIIModel0_Age <-     lm(Log_Age~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_Weight <- lm(Log_Weight~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_BMI <- lm(Log_BMI~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_RFM <- lm(Log_RFM~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$E_DII_Tertiles)
 
 
 lm_log_EDIIModel0_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_FIBER <-lm(Log_FIBER~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Ca <- lm(Log_Ca~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_P <- lm(Log_P~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Na <- lm(Log_Na~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_K <- lm(Log_K~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Mg <- lm(Log_Mg~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Zn <- lm(Log_Zn~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Cu <- lm(Log_Cu~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Mn <- lm(Log_Mn~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_B1 <- lm(Log_B1~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_B2 <- lm(Log_B2~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_B3 <- lm(Log_B3~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_B6 <- lm(Log_B6~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Folic <- lm(Log_Folic~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_B12 <- lm(Log_B12~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$E_DII_Tertiles)
 
 lm_log_EDIIModel0_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_TyG_WC <- lm(Log_TyG_WC~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$E_DII_Tertiles)
 lm_log_EDIIModel0_ABSI <- lm(Log_ABSI~DB_Polyphenols$E_DII_Tertiles)
 

 
 
 #ANOVA-Model 0
 
 
 anova_lm_log_EDIIModel0_Age_Age <- Anova(lm_log_EDIIModel0_Age)
 anova_lm_log_EDIIModel0_TPC_Folin <- Anova(lm_log_EDIIModel0_TPC_Folin)
 anova_lm_log_lm_log_EDIIModel0_E_DII_score <- Anova(lm_log_EDIIModel0_E_DII_score)
 
 anova_lm_log_EDIIModel0_Weight <- Anova(lm_log_EDIIModel0_Weight)
 anova_lm_log_EDIIModel0_BMI <- Anova(lm_log_EDIIModel0_BMI)
 anova_lm_log_EDIIModel0_Waist_Circumference <- Anova(lm_log_EDIIModel0_Waist_Circumference)
 anova_lm_log_EDIIModel0_RFM <- Anova(lm_log_EDIIModel0_RFM)
 
 anova_lm_log_EDIIModel0_Leptina_ng_mL <- Anova(lm_log_EDIIModel0_Leptina_ng_mL)
 anova_lm_log_EDIIModel0_Adiponectin_ug_ml <- Anova(lm_log_EDIIModel0_Adiponectin_ug_ml)
 anova_lm_log_EDIIModel0_Lep_Adip <- Anova(lm_log_EDIIModel0_Lep_Adip)
 
 
 anova_lm_log_EDIIModel0_ApoB_mg_dL <- Anova(lm_log_EDIIModel0_ApoB_mg_dL)
 anova_lm_log_EDIIModel0_HDL_mg_dL <- Anova(lm_log_EDIIModel0_HDL_mg_dL)
 anova_lm_log_EDIIModel0_VLDL_mg_dL <- Anova(lm_log_EDIIModel0_VLDL_mg_dL)
 anova_lm_log_EDIIModel0_LDL_mg_dL <- Anova(lm_log_EDIIModel0_LDL_mg_dL)
 anova_lm_log_EDIIModel0_T.Cholesterol_mg_dL_ <- Anova(lm_log_EDIIModel0_T.Cholesterol_mg_dL_)
 anova_lm_log_EDIIModel0_TGs_mg_dL <- Anova(lm_log_EDIIModel0_TGs_mg_dL)
 anova_lm_log_EDIIModel0_Atherogenic_index <- Anova(lm_log_EDIIModel0_Atherogenic_index)
 
 anova_lm_log_EDIIModel0_Creatinine_mg_dL_ <- Anova(lm_log_EDIIModel0_Creatinine_mg_dL_)
 anova_lm_log_EDIIModel0_hsCRP_mg_L <- Anova(lm_log_EDIIModel0_hsCRP_mg_L)
 
 anova_lm_log_EDIIModel0_Glucose_mmol_L <- Anova(lm_log_EDIIModel0_Glucose_mmol_L)
 anova_lm_log_EDIIModel0_Insulin_uU_mL <- Anova(lm_log_EDIIModel0_Insulin_uU_mL)
 
 anova_lm_log_EDIIModel0_HbA1c <- Anova(lm_log_EDIIModel0_HbA1c)
 anova_lm_log_EDIIModel0_beta_cell_function <- Anova(lm_log_EDIIModel0_beta_cell_function)
 anova_lm_log_EDIIModel0_HOMA_IR <- Anova(lm_log_EDIIModel0_HOMA_IR)
 
 anova_lm_log_EDIIModel0_Systolic_BP <- Anova(lm_log_EDIIModel0_Systolic_BP)
 anova_lm_log_EDIIModel0_Diastolic_BP <- Anova(lm_log_EDIIModel0_Diastolic_BP)
 
 anova_lm_log_EDIIModel0_FIBERP <- Anova(lm_log_EDIIModel0_FIBER)
 anova_lm_log_EDIIModel0_Ca <- Anova(lm_log_EDIIModel0_Ca)
 anova_lm_log_EDIIModel0_P<- Anova(lm_log_EDIIModel0_P)
 anova_lm_log_EDIIModel0_Fe_total <- Anova(lm_log_EDIIModel0_Fe_total)
 anova_lm_log_EDIIModel0_NaP <- Anova(lm_log_EDIIModel0_Na)
 anova_lm_log_EDIIModel0_K <- Anova(lm_log_EDIIModel0_K)
 anova_lm_log_EDIIModel0_Mg <- Anova(lm_log_EDIIModel0_Mg)
 anova_lm_log_EDIIModel0_Zn <- Anova(lm_log_EDIIModel0_Zn)
 anova_lm_log_EDIIModel0_Cu <- Anova(lm_log_EDIIModel0_Cu)
 anova_lm_log_EDIIModel0_Mn <- Anova(lm_log_EDIIModel0_Mn)
 
 
 anova_lm_log_EDIIModel0_Vit_A_ER <- Anova(lm_log_EDIIModel0_Vit_A_ER)
 anova_lm_log_EDIIModel0_B1 <- Anova(lm_log_EDIIModel0_B1)
 anova_lm_log_EDIIModel0_B2 <- Anova(lm_log_EDIIModel0_B2)
 anova_lm_log_EDIIModel0_B3 <- Anova(lm_log_EDIIModel0_B3)
 anova_lm_log_EDIIModel0_Ac_pantoenico <- Anova(lm_log_EDIIModel0_Ac_pantoenico)
 anova_lm_log_EDIIModel0_B6 <- Anova(lm_log_EDIIModel0_B6)
 anova_lm_log_EDIIModel0_Folic <- Anova(lm_log_EDIIModel0_Folic)
 anova_lm_log_EDIIModel0_B12 <- Anova(lm_log_EDIIModel0_B12)
 anova_lm_log_EDIIModel0_Vit_C <- Anova(lm_log_EDIIModel0_Vit_C)
 
 
 anova_lm_log_EDIIModel0_Prevotella <- Anova(lm_log_EDIIModel0_Prevotella)
 anova_lm_log_EDIIModel0_Lachnospiraceae <- Anova(lm_log_EDIIModel0_Lachnospiraceae)
 anova_lm_log_EDIIModel0_Pathogen <- Anova(lm_log_EDIIModel0_Pathogen)
 anova_lm_log_EDIIModel0_Akkermansia.Bacteroidales <- Anova(lm_log_EDIIModel0_Akkermansia.Bacteroidales)
 anova_lm_log_EDIIModel0_Ruminococcaceae<- Anova(lm_log_EDIIModel0_Ruminococcaceae)
 
 anova_lm_log_EDIIModel0_TyG_index <- Anova(lm_log_EDIIModel0_TyG_index)
 anova_lm_log_EDIIModel0_TyG_BMI <- Anova(lm_log_EDIIModel0_TyG_BMI)
 anova_lm_log_EDIIModel0_TyG_WC <- Anova(lm_log_EDIIModel0_TyG_WC)
 anova_lm_log_EDIIModel0_TyG_WHtR <- Anova(lm_log_EDIIModel0_TyG_WHtR)
 anova_lm_log_EDIIModel0_ABSI <- Anova(lm_log_EDIIModel0_ABSI)
 

 
 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_EDIIModel0 <- list(
   Age_EDIIModel0 = anova_lm_log_EDIIModel0_Age_Age,
   TPC_Folin_EDIIModel0 = anova_lm_log_EDIIModel0_TPC_Folin,
   E_DII_score_EDIIModel0 = anova_lm_log_lm_log_EDIIModel0_E_DII_score,
   Weight_EDIIModel0 = anova_lm_log_EDIIModel0_Weight,
   BMI_EDIIModel0 = anova_lm_log_EDIIModel0_BMI,
   Waist_Circumference_EDIIModel0 = anova_lm_log_EDIIModel0_Waist_Circumference,
   RFM_EDIIModel0 = anova_lm_log_EDIIModel0_RFM,
   Leptina_EDIIModel0 = anova_lm_log_EDIIModel0_Leptina_ng_mL,
   Adiponectin_EDIIModel0 = anova_lm_log_EDIIModel0_Adiponectin_ug_ml,
   Lep_Adip_EDIIModel0 = anova_lm_log_EDIIModel0_Lep_Adip,
   ApoB_EDIIModel0 = anova_lm_log_EDIIModel0_ApoB_mg_dL,
   HDL_EDIIModel0 = anova_lm_log_EDIIModel0_HDL_mg_dL,
   VLDL_EDIIModel0 = anova_lm_log_EDIIModel0_VLDL_mg_dL,
   LDL_EDIIModel0 = anova_lm_log_EDIIModel0_LDL_mg_dL,
   T_Cholesterol_EDIIModel0 = anova_lm_log_EDIIModel0_T.Cholesterol_mg_dL_,
   TGs_EDIIModel0 = anova_lm_log_EDIIModel0_TGs_mg_dL,
   Atherogenic_index_EDIIModel0 = anova_lm_log_EDIIModel0_Atherogenic_index,
   Creatinine_EDIIModel0 = anova_lm_log_EDIIModel0_Creatinine_mg_dL_,
   hsCRP_EDIIModel0 = anova_lm_log_EDIIModel0_hsCRP_mg_L,
   Glucose_EDIIModel0 = anova_lm_log_EDIIModel0_Glucose_mmol_L,
   Insulin_EDIIModel0 = anova_lm_log_EDIIModel0_Insulin_uU_mL,
   HbA1c_EDIIModel0 = anova_lm_log_EDIIModel0_HbA1c,
   Beta_cell_function_EDIIModel0 = anova_lm_log_EDIIModel0_beta_cell_function,
   HOMA_IR_EDIIModel0 = anova_lm_log_EDIIModel0_HOMA_IR,
   Systolic_BP_EDIIModel0 = anova_lm_log_EDIIModel0_Systolic_BP,
   Diastolic_BP_EDIIModel0 = anova_lm_log_EDIIModel0_Diastolic_BP,
   Fiber_EDIIModel0 = anova_lm_log_EDIIModel0_FIBERP,
   Ca_EDIIModel0 = anova_lm_log_EDIIModel0_Ca,
   P_EDIIModel0 = anova_lm_log_EDIIModel0_P,
   Fe_total_EDIIModel0 = anova_lm_log_EDIIModel0_Fe_total,
   Na_EDIIModel0 = anova_lm_log_EDIIModel0_NaP,
   K_EDIIModel0 = anova_lm_log_EDIIModel0_K,
   Mg_EDIIModel0 = anova_lm_log_EDIIModel0_Mg,
   Zn_EDIIModel0 = anova_lm_log_EDIIModel0_Zn,
   Cu_EDIIModel0 = anova_lm_log_EDIIModel0_Cu,
   Mn_EDIIModel0 = anova_lm_log_EDIIModel0_Mn,
   Vit_A_ER_EDIIModel0 = anova_lm_log_EDIIModel0_Vit_A_ER,
   B1_EDIIModel0 = anova_lm_log_EDIIModel0_B1,
   B2_EDIIModel0 = anova_lm_log_EDIIModel0_B2,
   B3_EDIIModel0 = anova_lm_log_EDIIModel0_B3,
   Ac_pantoenico_EDIIModel0 = anova_lm_log_EDIIModel0_Ac_pantoenico,
   B6_EDIIModel0 = anova_lm_log_EDIIModel0_B6,
   Folic_EDIIModel0 = anova_lm_log_EDIIModel0_Folic,
   B12_EDIIModel0 = anova_lm_log_EDIIModel0_B12,
   Vit_C_EDIIModel0 = anova_lm_log_EDIIModel0_Vit_C,
   Prevotella_EDIIModel0 = anova_lm_log_EDIIModel0_Prevotella,
   Lachnospiraceae_EDIIModel0 = anova_lm_log_EDIIModel0_Lachnospiraceae,
   Pathogen_EDIIModel0 = anova_lm_log_EDIIModel0_Pathogen,
   Akkermansia_Bacteroidales_EDIIModel0 = anova_lm_log_EDIIModel0_Akkermansia.Bacteroidales,
   Ruminococcaceae_EDIIModel0 = anova_lm_log_EDIIModel0_Ruminococcaceae,
   TyG_index_Model0 = anova_lm_log_EDIIModel0_TyG_index,
   TyG_BMI_Model0 = anova_lm_log_EDIIModel0_TyG_BMI,
   TyG_WC_Model0 = anova_lm_log_EDIIModel0_TyG_WC,
   TyG_WHtR_Model0 = anova_lm_log_EDIIModel0_TyG_WHtR,
   ABSI_Model = anova_lm_log_EDIIModel0_ABSI
 )
 
 

 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_EDIIModel0 <- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_EDIIModel0)) {
   model <- anova_EDIIModel0[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_EDIIModel0 <- rbind(anova_results_EDIIModel0, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_EDIIModel0, "anova_results_EDIIModel0.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results_Model10xlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_EDIIModel0 <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_Model1)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_EDIIModel0$Q_Value <- p.adjust(anova_results_EDIIModel0$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_EDIIModel0, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_EDIIModel0.xlsx", "anova_results_with_qvalues_EDIIModel0.xlsx")
 
 
 
 
 ##########
 ###########
 ########lm Modelo 1 (Log_Age~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 colnames(DB_Polyphenols)
 
 lm_log_EDIIModel1_Age <-     lm(Log_Age~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_Weight <- lm(Log_Weight~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_BMI <- lm(Log_BMI~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_RFM <- lm(Log_RFM~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$E_DII_Tertiles+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 
 
 lm_log_EDIIModel1_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$E_DII_Tertiles+   DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_FIBER <-lm(Log_FIBER~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Ca <- lm(Log_Ca~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_P <- lm(Log_P~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Na <- lm(Log_Na~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_K <- lm(Log_K~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Mg <- lm(Log_Mg~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Zn <- lm(Log_Zn~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Cu <- lm(Log_Cu~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Mn <- lm(Log_Mn~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$E_DII_Tertiles+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_B1 <- lm(Log_B1~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_B2 <- lm(Log_B2~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_B3 <- lm(Log_B3~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_B6 <- lm(Log_B6~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Folic <- lm(Log_Folic~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_B12 <- lm(Log_B12~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 
 lm_log_EDIIModel1_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_TyG_WC <- lm(Log_TyG_WC~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 lm_log_EDIIModel1_ABSI <- lm(Log_ABSI~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range)
 
 
 #ANOVA-Model 1
 
 
 anova_lm_log_EDIIModel1_Age <- Anova(lm_log_EDIIModel1_Age)
 anova_lm_log_EDIIModel1_TPC_Folin <- Anova(lm_log_EDIIModel1_TPC_Folin)
 anova_lm_log_lm_log_EDIIModel1_E_DII_score <- Anova(lm_log_EDIIModel1_E_DII_score)
 
 anova_lm_log_EDIIModel1_Weight <- Anova(lm_log_EDIIModel1_Weight)
 anova_lm_log_EDIIModel1_BMI <- Anova(lm_log_EDIIModel1_BMI)
 anova_lm_log_EDIIModel1_Waist_Circumference <- Anova(lm_log_EDIIModel1_Waist_Circumference)
 anova_lm_log_EDIIModel1_RFM <- Anova(lm_log_EDIIModel1_RFM)
 
 anova_lm_log_EDIIModel1_Leptina_ng_mL <- Anova(lm_log_EDIIModel1_Leptina_ng_mL)
 anova_lm_log_EDIIModel1_Adiponectin_ug_ml <- Anova(lm_log_EDIIModel1_Adiponectin_ug_ml)
 anova_lm_log_EDIIModel1_Lep_Adip <- Anova(lm_log_EDIIModel1_Lep_Adip)
 
 
 anova_lm_log_EDIIModel1_ApoB_mg_dL <- Anova(lm_log_EDIIModel1_ApoB_mg_dL)
 anova_lm_log_EDIIModel1_HDL_mg_dL <- Anova(lm_log_EDIIModel1_HDL_mg_dL)
 anova_lm_log_EDIIModel1_VLDL_mg_dL <- Anova(lm_log_EDIIModel1_VLDL_mg_dL)
 anova_lm_log_EDIIModel1_LDL_mg_dL <- Anova(lm_log_EDIIModel1_LDL_mg_dL)
 anova_lm_log_EDIIModel1_T.Cholesterol_mg_dL_ <- Anova(lm_log_EDIIModel1_T.Cholesterol_mg_dL_)
 anova_lm_log_EDIIModel1_TGs_mg_dL <- Anova(lm_log_EDIIModel1_TGs_mg_dL)
 anova_lm_log_EDIIModel1_Atherogenic_index <- Anova(lm_log_EDIIModel1_Atherogenic_index)
 
 anova_lm_log_EDIIModel1_Creatinine_mg_dL_ <- Anova(lm_log_EDIIModel1_Creatinine_mg_dL_)
 anova_lm_log_EDIIModel1_hsCRP_mg_L <- Anova(lm_log_EDIIModel1_hsCRP_mg_L)
 
 anova_lm_log_EDIIModel1_Glucose_mmol_L <- Anova(lm_log_EDIIModel1_Glucose_mmol_L)
 anova_lm_log_EDIIModel1_Insulin_uU_mL <- Anova(lm_log_EDIIModel1_Insulin_uU_mL)
 
 anova_lm_log_EDIIModel1_HbA1c <- Anova(lm_log_EDIIModel1_HbA1c)
 anova_lm_log_EDIIModel1_beta_cell_function <- Anova(lm_log_EDIIModel1_beta_cell_function)
 anova_lm_log_EDIIModel1_HOMA_IR <- Anova(lm_log_EDIIModel1_HOMA_IR)
 
 anova_lm_log_EDIIModel1_Systolic_BP <- Anova(lm_log_EDIIModel1_Systolic_BP)
 anova_lm_log_EDIIModel1_Diastolic_BP <- Anova(lm_log_EDIIModel1_Diastolic_BP)
 
 anova_lm_log_EDIIModel1_FIBERP <- Anova(lm_log_EDIIModel1_FIBER)
 anova_lm_log_EDIIModel1_Ca <- Anova(lm_log_EDIIModel1_Ca)
 anova_lm_log_EDIIModel1_P<- Anova(lm_log_EDIIModel1_P)
 anova_lm_log_EDIIModel1_Fe_total <- Anova(lm_log_EDIIModel1_Fe_total)
 anova_lm_log_EDIIModel1_NaP <- Anova(lm_log_EDIIModel1_Na)
 anova_lm_log_EDIIModel1_K <- Anova(lm_log_EDIIModel1_K)
 anova_lm_log_EDIIModel1_Mg <- Anova(lm_log_EDIIModel1_Mg)
 anova_lm_log_EDIIModel1_Zn <- Anova(lm_log_EDIIModel1_Zn)
 anova_lm_log_EDIIModel1_Cu <- Anova(lm_log_EDIIModel1_Cu)
 anova_lm_log_EDIIModel1_Mn <- Anova(lm_log_EDIIModel1_Mn)
 
 
 anova_lm_log_EDIIModel1_Vit_A_ER <- Anova(lm_log_EDIIModel1_Vit_A_ER)
 anova_lm_log_EDIIModel1_B1 <- Anova(lm_log_EDIIModel1_B1)
 anova_lm_log_EDIIModel1_B2 <- Anova(lm_log_EDIIModel1_B2)
 anova_lm_log_EDIIModel1_B3 <- Anova(lm_log_EDIIModel1_B3)
 anova_lm_log_EDIIModel1_Ac_pantoenico <- Anova(lm_log_EDIIModel1_Ac_pantoenico)
 anova_lm_log_EDIIModel1_B6 <- Anova(lm_log_EDIIModel1_B6)
 anova_lm_log_EDIIModel1_Folic <- Anova(lm_log_EDIIModel1_Folic)
 anova_lm_log_EDIIModel1_B12 <- Anova(lm_log_EDIIModel1_B12)
 anova_lm_log_EDIIModel1_Vit_C <- Anova(lm_log_EDIIModel1_Vit_C)
 
 
 anova_lm_log_EDIIModel1_Prevotella <- Anova(lm_log_EDIIModel1_Prevotella)
 anova_lm_log_EDIIModel1_Lachnospiraceae <- Anova(lm_log_EDIIModel1_Lachnospiraceae)
 anova_lm_log_EDIIModel1_Pathogen <- Anova(lm_log_EDIIModel1_Pathogen)
 anova_lm_log_EDIIModel1_Akkermansia.Bacteroidales <- Anova(lm_log_EDIIModel1_Akkermansia.Bacteroidales)
 anova_lm_log_EDIIModel1_Ruminococcaceae<- Anova(lm_log_EDIIModel1_Ruminococcaceae)
 
 
 anova_lm_log_EDIIModel1_TyG_index <- Anova(lm_log_EDIIModel1_TyG_index)
 anova_lm_log_EDIIModel1_TyG_BMI <- Anova(lm_log_EDIIModel1_TyG_BMI)
 anova_lm_log_EDIIModel1_TyG_WC <- Anova(lm_log_EDIIModel1_TyG_WC)
 anova_lm_log_EDIIModel1_TyG_WHtR <- Anova(lm_log_EDIIModel1_TyG_WHtR)
 anova_lm_log_EDIIModel1_ABSI<- Anova(lm_log_EDIIModel1_ABSI)
 
 
 

 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_EDIIModel1<- list(
   Age_EDIIModel1= anova_lm_log_EDIIModel1_Age,
   TPC_Folin_EDIIModel1= anova_lm_log_EDIIModel1_TPC_Folin,
   E_DII_score_EDIIModel1= anova_lm_log_lm_log_EDIIModel1_E_DII_score,
   Weight_EDIIModel1= anova_lm_log_EDIIModel1_Weight,
   BMI_EDIIModel1= anova_lm_log_EDIIModel1_BMI,
   Waist_Circumference_EDIIModel1= anova_lm_log_EDIIModel1_Waist_Circumference,
   RFM_EDIIModel1= anova_lm_log_EDIIModel1_RFM,
   Leptina_EDIIModel1= anova_lm_log_EDIIModel1_Leptina_ng_mL,
   Adiponectin_EDIIModel1= anova_lm_log_EDIIModel1_Adiponectin_ug_ml,
   Lep_Adip_EDIIModel1= anova_lm_log_EDIIModel1_Lep_Adip,
   ApoB_EDIIModel1= anova_lm_log_EDIIModel1_ApoB_mg_dL,
   HDL_EDIIModel1= anova_lm_log_EDIIModel1_HDL_mg_dL,
   VLDL_EDIIModel1= anova_lm_log_EDIIModel1_VLDL_mg_dL,
   LDL_EDIIModel1= anova_lm_log_EDIIModel1_LDL_mg_dL,
   T_Cholesterol_EDIIModel1= anova_lm_log_EDIIModel1_T.Cholesterol_mg_dL_,
   TGs_EDIIModel1= anova_lm_log_EDIIModel1_TGs_mg_dL,
   Atherogenic_index_EDIIModel1= anova_lm_log_EDIIModel1_Atherogenic_index,
   Creatinine_EDIIModel1= anova_lm_log_EDIIModel1_Creatinine_mg_dL_,
   hsCRP_EDIIModel1= anova_lm_log_EDIIModel1_hsCRP_mg_L,
   Glucose_EDIIModel1= anova_lm_log_EDIIModel1_Glucose_mmol_L,
   Insulin_EDIIModel1= anova_lm_log_EDIIModel1_Insulin_uU_mL,
   HbA1c_EDIIModel1= anova_lm_log_EDIIModel1_HbA1c,
   Beta_cell_function_EDIIModel1= anova_lm_log_EDIIModel1_beta_cell_function,
   HOMA_IR_EDIIModel1= anova_lm_log_EDIIModel1_HOMA_IR,
   Systolic_BP_EDIIModel1= anova_lm_log_EDIIModel1_Systolic_BP,
   Diastolic_BP_EDIIModel1= anova_lm_log_EDIIModel1_Diastolic_BP,
   Fiber_EDIIModel1= anova_lm_log_EDIIModel1_FIBERP,
   Ca_EDIIModel1= anova_lm_log_EDIIModel1_Ca,
   P_EDIIModel1= anova_lm_log_EDIIModel1_P,
   Fe_total_EDIIModel1= anova_lm_log_EDIIModel1_Fe_total,
   Na_EDIIModel1= anova_lm_log_EDIIModel1_NaP,
   K_EDIIModel1= anova_lm_log_EDIIModel1_K,
   Mg_EDIIModel1= anova_lm_log_EDIIModel1_Mg,
   Zn_EDIIModel1= anova_lm_log_EDIIModel1_Zn,
   Cu_EDIIModel1= anova_lm_log_EDIIModel1_Cu,
   Mn_EDIIModel1= anova_lm_log_EDIIModel1_Mn,
   Vit_A_ER_EDIIModel1= anova_lm_log_EDIIModel1_Vit_A_ER,
   B1_EDIIModel1= anova_lm_log_EDIIModel1_B1,
   B2_EDIIModel1= anova_lm_log_EDIIModel1_B2,
   B3_EDIIModel1= anova_lm_log_EDIIModel1_B3,
   Ac_pantoenico_EDIIModel1= anova_lm_log_EDIIModel1_Ac_pantoenico,
   B6_EDIIModel1= anova_lm_log_EDIIModel1_B6,
   Folic_EDIIModel1= anova_lm_log_EDIIModel1_Folic,
   B12_EDIIModel1= anova_lm_log_EDIIModel1_B12,
   Vit_C_EDIIModel1= anova_lm_log_EDIIModel1_Vit_C,
   Prevotella_EDIIModel1= anova_lm_log_EDIIModel1_Prevotella,
   Lachnospiraceae_EDIIModel1= anova_lm_log_EDIIModel1_Lachnospiraceae,
   Pathogen_EDIIModel1= anova_lm_log_EDIIModel1_Pathogen,
   Akkermansia_Bacteroidales_EDIIModel1= anova_lm_log_EDIIModel1_Akkermansia.Bacteroidales,
   Ruminococcaceae_EDIIModel1= anova_lm_log_EDIIModel1_Ruminococcaceae,
   TyG_index_Model1 = anova_lm_log_EDIIModel1_TyG_index,
   TyG_BMI_Model1 = anova_lm_log_EDIIModel1_TyG_BMI,
   TyG_WC_Model1 = anova_lm_log_EDIIModel1_TyG_WC,
   TyG_WHtR_Model1 = anova_lm_log_EDIIModel1_TyG_WHtR,
   ABSI_Model1 = anova_lm_log_EDIIModel1_ABSI
 )
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_EDIIModel1<- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_EDIIModel1)) {
   model <- anova_EDIIModel1[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_EDIIModel1<- rbind(anova_results_EDIIModel1, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_EDIIModel1, "anova_results_EDIIModel1.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results_EDIIModel1.xlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_EDIIModel1 <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_EDIIModel1)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_EDIIModel1$Q_Value <- p.adjust(anova_results_EDIIModel1$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_EDIIModel1, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_EDIIModel1.xlsx", "anova_results_with_qvalues_EDIIModel1.xlsx")
 
 
 
 
 
 
 
 
 ##########
 ###########
 ########lm Modelo 2 (Log_Age~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 colnames(DB_Polyphenols)
 
 lm_log_EDIIModel2_Age <-     lm(Log_Age~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin )
 lm_log_EDIIModel2_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_Weight <- lm(Log_Weight~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_BMI <- lm(Log_BMI~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_RFM <- lm(Log_RFM~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$E_DII_Tertiles+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 lm_log_EDIIModel2_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$E_DII_Tertiles+   DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_FIBER <-lm(Log_FIBER~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Ca <- lm(Log_Ca~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_P <- lm(Log_P~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Na <- lm(Log_Na~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_K <- lm(Log_K~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Mg <- lm(Log_Mg~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Zn <- lm(Log_Zn~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Cu <- lm(Log_Cu~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Mn <- lm(Log_Mn~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$E_DII_Tertiles+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_B1 <- lm(Log_B1~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_B2 <- lm(Log_B2~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_B3 <- lm(Log_B3~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_B6 <- lm(Log_B6~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Folic <- lm(Log_Folic~DB_Polyphenols$E_DII_Tertiles+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_B12 <- lm(Log_B12~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_EDIIModel2_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_TyG_WC <- lm(Log_TyG_WC~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_EDIIModel2_ABSI <- lm(Log_ABSI~DB_Polyphenols$E_DII_Tertiles+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 #ANOVA-Model 2
 
 
 anova_lm_log_EDIIModel2_Age <- Anova(lm_log_EDIIModel2_Age)
 anova_lm_log_EDIIModel2_TPC_Folin <- Anova(lm_log_EDIIModel2_TPC_Folin)
 anova_lm_log_lm_log_EDIIModel2_E_DII_score <- Anova(lm_log_EDIIModel2_E_DII_score)
 
 anova_lm_log_EDIIModel2_Weight <- Anova(lm_log_EDIIModel2_Weight)
 anova_lm_log_EDIIModel2_BMI <- Anova(lm_log_EDIIModel2_BMI)
 anova_lm_log_EDIIModel2_Waist_Circumference <- Anova(lm_log_EDIIModel2_Waist_Circumference)
 anova_lm_log_EDIIModel2_RFM <- Anova(lm_log_EDIIModel2_RFM)
 
 anova_lm_log_EDIIModel2_Leptina_ng_mL <- Anova(lm_log_EDIIModel2_Leptina_ng_mL)
 anova_lm_log_EDIIModel2_Adiponectin_ug_ml <- Anova(lm_log_EDIIModel2_Adiponectin_ug_ml)
 anova_lm_log_EDIIModel2_Lep_Adip <- Anova(lm_log_EDIIModel2_Lep_Adip)
 
 
 anova_lm_log_EDIIModel2_ApoB_mg_dL <- Anova(lm_log_EDIIModel2_ApoB_mg_dL)
 anova_lm_log_EDIIModel2_HDL_mg_dL <- Anova(lm_log_EDIIModel2_HDL_mg_dL)
 anova_lm_log_EDIIModel2_VLDL_mg_dL <- Anova(lm_log_EDIIModel2_VLDL_mg_dL)
 anova_lm_log_EDIIModel2_LDL_mg_dL <- Anova(lm_log_EDIIModel2_LDL_mg_dL)
 anova_lm_log_EDIIModel2_T.Cholesterol_mg_dL_ <- Anova(lm_log_EDIIModel2_T.Cholesterol_mg_dL_)
 anova_lm_log_EDIIModel2_TGs_mg_dL <- Anova(lm_log_EDIIModel2_TGs_mg_dL)
 anova_lm_log_EDIIModel2_Atherogenic_index <- Anova(lm_log_EDIIModel2_Atherogenic_index)
 
 anova_lm_log_EDIIModel2_Creatinine_mg_dL_ <- Anova(lm_log_EDIIModel2_Creatinine_mg_dL_)
 anova_lm_log_EDIIModel2_hsCRP_mg_L <- Anova(lm_log_EDIIModel2_hsCRP_mg_L)
 
 anova_lm_log_EDIIModel2_Glucose_mmol_L <- Anova(lm_log_EDIIModel2_Glucose_mmol_L)
 anova_lm_log_EDIIModel2_Insulin_uU_mL <- Anova(lm_log_EDIIModel2_Insulin_uU_mL)
 
 anova_lm_log_EDIIModel2_HbA1c <- Anova(lm_log_EDIIModel2_HbA1c)
 anova_lm_log_EDIIModel2_beta_cell_function <- Anova(lm_log_EDIIModel2_beta_cell_function)
 anova_lm_log_EDIIModel2_HOMA_IR <- Anova(lm_log_EDIIModel2_HOMA_IR)
 
 anova_lm_log_EDIIModel2_Systolic_BP <- Anova(lm_log_EDIIModel2_Systolic_BP)
 anova_lm_log_EDIIModel2_Diastolic_BP <- Anova(lm_log_EDIIModel2_Diastolic_BP)
 
 anova_lm_log_EDIIModel2_FIBERP <- Anova(lm_log_EDIIModel2_FIBER)
 anova_lm_log_EDIIModel2_Ca <- Anova(lm_log_EDIIModel2_Ca)
 anova_lm_log_EDIIModel2_P<- Anova(lm_log_EDIIModel2_P)
 anova_lm_log_EDIIModel2_Fe_total <- Anova(lm_log_EDIIModel2_Fe_total)
 anova_lm_log_EDIIModel2_NaP <- Anova(lm_log_EDIIModel2_Na)
 anova_lm_log_EDIIModel2_K <- Anova(lm_log_EDIIModel2_K)
 anova_lm_log_EDIIModel2_Mg <- Anova(lm_log_EDIIModel2_Mg)
 anova_lm_log_EDIIModel2_Zn <- Anova(lm_log_EDIIModel2_Zn)
 anova_lm_log_EDIIModel2_Cu <- Anova(lm_log_EDIIModel2_Cu)
 anova_lm_log_EDIIModel2_Mn <- Anova(lm_log_EDIIModel2_Mn)
 
 
 anova_lm_log_EDIIModel2_Vit_A_ER <- Anova(lm_log_EDIIModel2_Vit_A_ER)
 anova_lm_log_EDIIModel2_B1 <- Anova(lm_log_EDIIModel2_B1)
 anova_lm_log_EDIIModel2_B2 <- Anova(lm_log_EDIIModel2_B2)
 anova_lm_log_EDIIModel2_B3 <- Anova(lm_log_EDIIModel2_B3)
 anova_lm_log_EDIIModel2_Ac_pantoenico <- Anova(lm_log_EDIIModel2_Ac_pantoenico)
 anova_lm_log_EDIIModel2_B6 <- Anova(lm_log_EDIIModel2_B6)
 anova_lm_log_EDIIModel2_Folic <- Anova(lm_log_EDIIModel2_Folic)
 anova_lm_log_EDIIModel2_B12 <- Anova(lm_log_EDIIModel2_B12)
 anova_lm_log_EDIIModel2_Vit_C <- Anova(lm_log_EDIIModel2_Vit_C)
 
 
 anova_lm_log_EDIIModel2_Prevotella <- Anova(lm_log_EDIIModel2_Prevotella)
 anova_lm_log_EDIIModel2_Lachnospiraceae <- Anova(lm_log_EDIIModel2_Lachnospiraceae)
 anova_lm_log_EDIIModel2_Pathogen <- Anova(lm_log_EDIIModel2_Pathogen)
 anova_lm_log_EDIIModel2_Akkermansia.Bacteroidales <- Anova(lm_log_EDIIModel2_Akkermansia.Bacteroidales)
 anova_lm_log_EDIIModel2_Ruminococcaceae<- Anova(lm_log_EDIIModel2_Ruminococcaceae)
 
 anova_lm_log_EDIIModel2_TyG_index <- Anova(lm_log_EDIIModel2_TyG_index)
 anova_lm_log_EDIIModel2_TyG_BMI <- Anova(lm_log_EDIIModel2_TyG_BMI)
 anova_lm_log_EDIIModel2_TyG_WC <- Anova(lm_log_EDIIModel2_TyG_WC)
 anova_lm_log_EDIIModel2_TyG_WHtR <- Anova(lm_log_EDIIModel2_TyG_WHtR)
 anova_lm_log_EDIIModel2_ABSI<- Anova(lm_log_EDIIModel2_ABSI)
 
 
 

 
 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_EDIIModel2<- list(
   Age_EDIIModel2= anova_lm_log_EDIIModel2_Age,
   TPC_Folin_EDIIModel2= anova_lm_log_EDIIModel2_TPC_Folin,
   E_DII_score_EDIIModel2= anova_lm_log_lm_log_EDIIModel2_E_DII_score,
   Weight_EDIIModel2= anova_lm_log_EDIIModel2_Weight,
   BMI_EDIIModel2= anova_lm_log_EDIIModel2_BMI,
   Waist_Circumference_EDIIModel2= anova_lm_log_EDIIModel2_Waist_Circumference,
   RFM_EDIIModel2= anova_lm_log_EDIIModel2_RFM,
   Leptina_EDIIModel2= anova_lm_log_EDIIModel2_Leptina_ng_mL,
   Adiponectin_EDIIModel2= anova_lm_log_EDIIModel2_Adiponectin_ug_ml,
   Lep_Adip_EDIIModel2= anova_lm_log_EDIIModel2_Lep_Adip,
   ApoB_EDIIModel2= anova_lm_log_EDIIModel2_ApoB_mg_dL,
   HDL_EDIIModel2= anova_lm_log_EDIIModel2_HDL_mg_dL,
   VLDL_EDIIModel2= anova_lm_log_EDIIModel2_VLDL_mg_dL,
   LDL_EDIIModel2= anova_lm_log_EDIIModel2_LDL_mg_dL,
   T_Cholesterol_EDIIModel2= anova_lm_log_EDIIModel2_T.Cholesterol_mg_dL_,
   TGs_EDIIModel2= anova_lm_log_EDIIModel2_TGs_mg_dL,
   Atherogenic_index_EDIIModel2= anova_lm_log_EDIIModel2_Atherogenic_index,
   Creatinine_EDIIModel2= anova_lm_log_EDIIModel2_Creatinine_mg_dL_,
   hsCRP_EDIIModel2= anova_lm_log_EDIIModel2_hsCRP_mg_L,
   Glucose_EDIIModel2= anova_lm_log_EDIIModel2_Glucose_mmol_L,
   Insulin_EDIIModel2= anova_lm_log_EDIIModel2_Insulin_uU_mL,
   HbA1c_EDIIModel2= anova_lm_log_EDIIModel2_HbA1c,
   Beta_cell_function_EDIIModel2= anova_lm_log_EDIIModel2_beta_cell_function,
   HOMA_IR_EDIIModel2= anova_lm_log_EDIIModel2_HOMA_IR,
   Systolic_BP_EDIIModel2= anova_lm_log_EDIIModel2_Systolic_BP,
   Diastolic_BP_EDIIModel2= anova_lm_log_EDIIModel2_Diastolic_BP,
   Fiber_EDIIModel2= anova_lm_log_EDIIModel2_FIBERP,
   Ca_EDIIModel2= anova_lm_log_EDIIModel2_Ca,
   P_EDIIModel2= anova_lm_log_EDIIModel2_P,
   Fe_total_EDIIModel2= anova_lm_log_EDIIModel2_Fe_total,
   Na_EDIIModel2= anova_lm_log_EDIIModel2_NaP,
   K_EDIIModel2= anova_lm_log_EDIIModel2_K,
   Mg_EDIIModel2= anova_lm_log_EDIIModel2_Mg,
   Zn_EDIIModel2= anova_lm_log_EDIIModel2_Zn,
   Cu_EDIIModel2= anova_lm_log_EDIIModel2_Cu,
   Mn_EDIIModel2= anova_lm_log_EDIIModel2_Mn,
   Vit_A_ER_EDIIModel2= anova_lm_log_EDIIModel2_Vit_A_ER,
   B1_EDIIModel2= anova_lm_log_EDIIModel2_B1,
   B2_EDIIModel2= anova_lm_log_EDIIModel2_B2,
   B3_EDIIModel2= anova_lm_log_EDIIModel2_B3,
   Ac_pantoenico_EDIIModel2= anova_lm_log_EDIIModel2_Ac_pantoenico,
   B6_EDIIModel2= anova_lm_log_EDIIModel2_B6,
   Folic_EDIIModel2= anova_lm_log_EDIIModel2_Folic,
   B12_EDIIModel2= anova_lm_log_EDIIModel2_B12,
   Vit_C_EDIIModel2= anova_lm_log_EDIIModel2_Vit_C,
   Prevotella_EDIIModel2= anova_lm_log_EDIIModel2_Prevotella,
   Lachnospiraceae_EDIIModel2= anova_lm_log_EDIIModel2_Lachnospiraceae,
   Pathogen_EDIIModel2= anova_lm_log_EDIIModel2_Pathogen,
   Akkermansia_Bacteroidales_EDIIModel2= anova_lm_log_EDIIModel2_Akkermansia.Bacteroidales,
   Ruminococcaceae_EDIIModel2= anova_lm_log_EDIIModel2_Ruminococcaceae,
   TyG_index_Model2 = anova_lm_log_EDIIModel2_TyG_index,
   TyG_BMI_Model2 = anova_lm_log_EDIIModel2_TyG_BMI,
   TyG_WC_Model2 = anova_lm_log_EDIIModel2_TyG_WC,
   TyG_WHtR_Model2 = anova_lm_log_EDIIModel2_TyG_WHtR,
   ABSI_Model2 = anova_lm_log_EDIIModel2_ABSI
 )
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_EDIIModel2<- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_EDIIModel2)) {
   model <- anova_EDIIModel2[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_EDIIModel2<- rbind(anova_results_EDIIModel2, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_EDIIModel2, "anova_results_EDIIModel2.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results_EDIIModel2.xlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_EDIIModel2 <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_EDIIModel2)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_EDIIModel2$Q_Value <- p.adjust(anova_results_EDIIModel2$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_EDIIModel2, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_EDIIModel2.xlsx", "anova_results_with_qvalues_EDIIModel2.xlsx")
 
 
 
 
 
 ######
 ########
 #########
 
 #####MODELO CUTOFF 1000
 
 
 ##########
 ###########
 #######lm Modelo 0 (Log_Age~DB_Polyphenols$TPC_Folin_1000)
 
 colnames(DB_Polyphenols)
 
 lm_log_Model0_1K_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_1000)
 
 
 lm_log_Model0_1K_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_1000)
 
 lm_log_Model0_1K_Log_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Log_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Log_TyG_WCn <- lm(Log_TyG_WC~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Log_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$TPC_Folin_1000)
 lm_log_Model0_1K_Log_ABSI <- lm(Log_ABSI~DB_Polyphenols$TPC_Folin_1000)
 
 
 
 
 
 
 
 
 #ANOVA-Model 0
 
 
 anova_lm_log_Model0_1K_Age_Age <- Anova(lm_log_Model0_1K_Age)
 anova_lm_log_Model0_1K_TPC_Folin <- Anova(lm_log_Model0_1K_TPC_Folin)
 anova_lm_log_lm_log_Model0_1K_E_DII_score <- Anova(lm_log_Model0_1K_E_DII_score)
 
 anova_lm_log_Model0_1K_Weight <- Anova(lm_log_Model0_1K_Weight)
 anova_lm_log_Model0_1K_BMI <- Anova(lm_log_Model0_1K_BMI)
 anova_lm_log_Model0_1K_Waist_Circumference <- Anova(lm_log_Model0_1K_Waist_Circumference)
 anova_lm_log_Model0_1K_RFM <- Anova(lm_log_Model0_1K_RFM)
 
 anova_lm_log_Model0_1K_Leptina_ng_mL <- Anova(lm_log_Model0_1K_Leptina_ng_mL)
 anova_lm_log_Model0_1K_Adiponectin_ug_ml <- Anova(lm_log_Model0_1K_Adiponectin_ug_ml)
 anova_lm_log_Model0_1K_Lep_Adip <- Anova(lm_log_Model0_1K_Lep_Adip)
 
 
 anova_lm_log_Model0_1K_ApoB_mg_dL <- Anova(lm_log_Model0_1K_ApoB_mg_dL)
 anova_lm_log_Model0_1K_HDL_mg_dL <- Anova(lm_log_Model0_1K_HDL_mg_dL)
 anova_lm_log_Model0_1K_VLDL_mg_dL <- Anova(lm_log_Model0_1K_VLDL_mg_dL)
 anova_lm_log_Model0_1K_LDL_mg_dL <- Anova(lm_log_Model0_1K_LDL_mg_dL)
 anova_lm_log_Model0_1K_T.Cholesterol_mg_dL_ <- Anova(lm_log_Model0_1K_T.Cholesterol_mg_dL_)
 anova_lm_log_Model0_1K_TGs_mg_dL <- Anova(lm_log_Model0_1K_TGs_mg_dL)
 anova_lm_log_Model0_1K_Atherogenic_index <- Anova(lm_log_Model0_1K_Atherogenic_index)
 
 anova_lm_log_Model0_1K_Creatinine_mg_dL_ <- Anova(lm_log_Model0_1K_Creatinine_mg_dL_)
 anova_lm_log_Model0_1K_hsCRP_mg_L <- Anova(lm_log_Model0_1K_hsCRP_mg_L)
 
 anova_lm_log_Model0_1K_Glucose_mmol_L <- Anova(lm_log_Model0_1K_Glucose_mmol_L)
 anova_lm_log_Model0_1K_Insulin_uU_mL <- Anova(lm_log_Model0_1K_Insulin_uU_mL)
 
 anova_lm_log_Model0_1K_HbA1c <- Anova(lm_log_Model0_1K_HbA1c)
 anova_lm_log_Model0_1K_beta_cell_function <- Anova(lm_log_Model0_1K_beta_cell_function)
 anova_lm_log_Model0_1K_HOMA_IR <- Anova(lm_log_Model0_1K_HOMA_IR)
 
 anova_lm_log_Model0_1K_Systolic_BP <- Anova(lm_log_Model0_1K_Systolic_BP)
 anova_lm_log_Model0_1K_Diastolic_BP <- Anova(lm_log_Model0_1K_Diastolic_BP)
 
 anova_lm_log_Model0_1K_FIBERP <- Anova(lm_log_Model0_1K_FIBER)
 anova_lm_log_Model0_1K_Ca <- Anova(lm_log_Model0_1K_Ca)
 anova_lm_log_Model0_1K_P<- Anova(lm_log_Model0_1K_P)
 anova_lm_log_Model0_1K_Fe_total <- Anova(lm_log_Model0_1K_Fe_total)
 anova_lm_log_Model0_1K_NaP <- Anova(lm_log_Model0_1K_Na)
 anova_lm_log_Model0_1K_K <- Anova(lm_log_Model0_1K_K)
 anova_lm_log_Model0_1K_Mg <- Anova(lm_log_Model0_1K_Mg)
 anova_lm_log_Model0_1K_Zn <- Anova(lm_log_Model0_1K_Zn)
 anova_lm_log_Model0_1K_Cu <- Anova(lm_log_Model0_1K_Cu)
 anova_lm_log_Model0_1K_Mn <- Anova(lm_log_Model0_1K_Mn)
 
 
 anova_lm_log_Model0_1K_Vit_A_ER <- Anova(lm_log_Model0_1K_Vit_A_ER)
 anova_lm_log_Model0_1K_B1 <- Anova(lm_log_Model0_1K_B1)
 anova_lm_log_Model0_1K_B2 <- Anova(lm_log_Model0_1K_B2)
 anova_lm_log_Model0_1K_B3 <- Anova(lm_log_Model0_1K_B3)
 anova_lm_log_Model0_1K_Ac_pantoenico <- Anova(lm_log_Model0_1K_Ac_pantoenico)
 anova_lm_log_Model0_1K_B6 <- Anova(lm_log_Model0_1K_B6)
 anova_lm_log_Model0_1K_Folic <- Anova(lm_log_Model0_1K_Folic)
 anova_lm_log_Model0_1K_B12 <- Anova(lm_log_Model0_1K_B12)
 anova_lm_log_Model0_1K_Vit_C <- Anova(lm_log_Model0_1K_Vit_C)
 
 
 anova_lm_log_Model0_1K_Prevotella <- Anova(lm_log_Model0_1K_Prevotella)
 anova_lm_log_Model0_1K_Lachnospiraceae <- Anova(lm_log_Model0_1K_Lachnospiraceae)
 anova_lm_log_Model0_1K_Pathogen <- Anova(lm_log_Model0_1K_Pathogen)
 anova_lm_log_Model0_1K_Akkermansia.Bacteroidales <- Anova(lm_log_Model0_1K_Akkermansia.Bacteroidales)
 anova_lm_log_Model0_1K_Ruminococcaceae<- Anova(lm_log_Model0_1K_Ruminococcaceae)
 
 anova_lm_log_Model0_1K_Log_TyG_index <- Anova(lm_log_Model0_1K_Log_TyG_index)
 anova_lm_log_Model0_1K_Log_TyG_BMI <- Anova(lm_log_Model0_1K_Log_TyG_BMI)
 anova_lm_log_Model0_1K_Log_TyG_WCn <- Anova(lm_log_Model0_1K_Log_TyG_WCn)
 anova_lm_log_Model0_1K_Log_TyG_WHtR <- Anova(lm_log_Model0_1K_Log_TyG_WHtR)
 anova_lm_log_Model0_1K_Log_ABSI<- Anova(lm_log_Model0_1K_Log_ABSI)
 
 
 
 
 
 
 
 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_Model0_1K <- list(
   Age_Model0_1K = anova_lm_log_Model0_1K_Age_Age,
   TPC_Folin_Model0_1K = anova_lm_log_Model0_1K_TPC_Folin,
   E_DII_score_Model0_1K = anova_lm_log_lm_log_Model0_1K_E_DII_score,
   Weight_Model0_1K = anova_lm_log_Model0_1K_Weight,
   BMI_Model0_1K = anova_lm_log_Model0_1K_BMI,
   Waist_Circumference_Model0_1K = anova_lm_log_Model0_1K_Waist_Circumference,
   RFM_Model0_1K = anova_lm_log_Model0_1K_RFM,
   Leptina_Model0_1K = anova_lm_log_Model0_1K_Leptina_ng_mL,
   Adiponectin_Model0_1K = anova_lm_log_Model0_1K_Adiponectin_ug_ml,
   Lep_Adip_Model0_1K = anova_lm_log_Model0_1K_Lep_Adip,
   ApoB_Model0_1K = anova_lm_log_Model0_1K_ApoB_mg_dL,
   HDL_Model0_1K = anova_lm_log_Model0_1K_HDL_mg_dL,
   VLDL_Model0_1K = anova_lm_log_Model0_1K_VLDL_mg_dL,
   LDL_Model0_1K = anova_lm_log_Model0_1K_LDL_mg_dL,
   T_Cholesterol_Model0_1K = anova_lm_log_Model0_1K_T.Cholesterol_mg_dL_,
   TGs_Model0_1K = anova_lm_log_Model0_1K_TGs_mg_dL,
   Atherogenic_index_Model0_1K = anova_lm_log_Model0_1K_Atherogenic_index,
   Creatinine_Model0_1K = anova_lm_log_Model0_1K_Creatinine_mg_dL_,
   hsCRP_Model0_1K = anova_lm_log_Model0_1K_hsCRP_mg_L,
   Glucose_Model0_1K = anova_lm_log_Model0_1K_Glucose_mmol_L,
   Insulin_Model0_1K = anova_lm_log_Model0_1K_Insulin_uU_mL,
   HbA1c_Model0_1K = anova_lm_log_Model0_1K_HbA1c,
   Beta_cell_function_Model0_1K = anova_lm_log_Model0_1K_beta_cell_function,
   HOMA_IR_Model0_1K = anova_lm_log_Model0_1K_HOMA_IR,
   Systolic_BP_Model0_1K = anova_lm_log_Model0_1K_Systolic_BP,
   Diastolic_BP_Model0_1K = anova_lm_log_Model0_1K_Diastolic_BP,
   Fiber_Model0_1K = anova_lm_log_Model0_1K_FIBERP,
   Ca_Model0_1K = anova_lm_log_Model0_1K_Ca,
   P_Model0_1K = anova_lm_log_Model0_1K_P,
   Fe_total_Model0_1K = anova_lm_log_Model0_1K_Fe_total,
   Na_Model0_1K = anova_lm_log_Model0_1K_NaP,
   K_Model0_1K = anova_lm_log_Model0_1K_K,
   Mg_Model0_1K = anova_lm_log_Model0_1K_Mg,
   Zn_Model0_1K = anova_lm_log_Model0_1K_Zn,
   Cu_Model0_1K = anova_lm_log_Model0_1K_Cu,
   Mn_Model0_1K = anova_lm_log_Model0_1K_Mn,
   Vit_A_ER_Model0_1K = anova_lm_log_Model0_1K_Vit_A_ER,
   B1_Model0_1K = anova_lm_log_Model0_1K_B1,
   B2_Model0_1K = anova_lm_log_Model0_1K_B2,
   B3_Model0_1K = anova_lm_log_Model0_1K_B3,
   Ac_pantoenico_Model0_1K = anova_lm_log_Model0_1K_Ac_pantoenico,
   B6_Model0_1K = anova_lm_log_Model0_1K_B6,
   Folic_Model0_1K = anova_lm_log_Model0_1K_Folic,
   B12_Model0_1K = anova_lm_log_Model0_1K_B12,
   Vit_C_Model0_1K = anova_lm_log_Model0_1K_Vit_C,
   Prevotella_Model0_1K = anova_lm_log_Model0_1K_Prevotella,
   Lachnospiraceae_Model0_1K = anova_lm_log_Model0_1K_Lachnospiraceae,
   Pathogen_Model0_1K = anova_lm_log_Model0_1K_Pathogen,
   Akkermansia_Bacteroidales_Model0_1K = anova_lm_log_Model0_1K_Akkermansia.Bacteroidales,
   Ruminococcaceae_Model0_1K = anova_lm_log_Model0_1K_Ruminococcaceae,
   TyG_index_Model0_1K = anova_lm_log_Model0_1K_Log_TyG_index,
   TyG_BMI_Model0_1K= anova_lm_log_Model0_1K_Log_TyG_BMI,
   TyG_WCn_Model0_1K= anova_lm_log_Model0_1K_Log_TyG_WCn,
   TyG_WHtRModel0 = anova_lm_log_Model0_1K_Log_TyG_WHtR,
   ABSI_Model0_1K= anova_lm_log_Model0_1K_Log_ABSI
 )
 
 
 
 
 
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_Model0_1K <- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_Model0_1K)) {
   model <- anova_Model0_1K[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_Model0_1K <- rbind(anova_results_Model0_1K, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_Model0_1K, "anova_results_Model0_1K.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results_Model10xlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_Model0_1K <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_Model1)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_Model0_1K$Q_Value <- p.adjust(anova_results_Model0_1K$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_Model0_1K, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_Model0_1K.xlsx", "anova_results_with_qvalues_Model0_1K.xlsx")
 
 #####
 
 
 
 ####
 ######
 ###########
 
 ##########
 ###########
 ########lm Modelo 2 (Log_Age~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 colnames(DB_Polyphenols)
 
 lm_log_Model2_1K_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin )
 lm_log_Model2_1K_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_1000+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 lm_log_Model2_1K_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_1000+   DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_1000+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model2_1K_Log_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Log_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Log_TyG_WCn <- lm(Log_TyG_WC~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Log_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model2_1K_Log_ABSI <- lm(Log_ABSI~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range +DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 
 #ANOVA-Model 2
 
 
 anova_lm_log_Model2_1K_Age <- Anova(lm_log_Model2_1K_Age)
 anova_lm_log_Model2_1K_TPC_Folin <- Anova(lm_log_Model2_1K_TPC_Folin)
 anova_lm_log_lm_log_Model2_1K_E_DII_score <- Anova(lm_log_Model2_1K_E_DII_score)
 
 anova_lm_log_Model2_1K_Weight <- Anova(lm_log_Model2_1K_Weight)
 anova_lm_log_Model2_1K_BMI <- Anova(lm_log_Model2_1K_BMI)
 anova_lm_log_Model2_1K_Waist_Circumference <- Anova(lm_log_Model2_1K_Waist_Circumference)
 anova_lm_log_Model2_1K_RFM <- Anova(lm_log_Model2_1K_RFM)
 
 anova_lm_log_Model2_1K_Leptina_ng_mL <- Anova(lm_log_Model2_1K_Leptina_ng_mL)
 anova_lm_log_Model2_1K_Adiponectin_ug_ml <- Anova(lm_log_Model2_1K_Adiponectin_ug_ml)
 anova_lm_log_Model2_1K_Lep_Adip <- Anova(lm_log_Model2_1K_Lep_Adip)
 
 
 anova_lm_log_Model2_1K_ApoB_mg_dL <- Anova(lm_log_Model2_1K_ApoB_mg_dL)
 anova_lm_log_Model2_1K_HDL_mg_dL <- Anova(lm_log_Model2_1K_HDL_mg_dL)
 anova_lm_log_Model2_1K_VLDL_mg_dL <- Anova(lm_log_Model2_1K_VLDL_mg_dL)
 anova_lm_log_Model2_1K_LDL_mg_dL <- Anova(lm_log_Model2_1K_LDL_mg_dL)
 anova_lm_log_Model2_1K_T.Cholesterol_mg_dL_ <- Anova(lm_log_Model2_1K_T.Cholesterol_mg_dL_)
 anova_lm_log_Model2_1K_TGs_mg_dL <- Anova(lm_log_Model2_1K_TGs_mg_dL)
 anova_lm_log_Model2_1K_Atherogenic_index <- Anova(lm_log_Model2_1K_Atherogenic_index)
 
 anova_lm_log_Model2_1K_Creatinine_mg_dL_ <- Anova(lm_log_Model2_1K_Creatinine_mg_dL_)
 anova_lm_log_Model2_1K_hsCRP_mg_L <- Anova(lm_log_Model2_1K_hsCRP_mg_L)
 
 anova_lm_log_Model2_1K_Glucose_mmol_L <- Anova(lm_log_Model2_1K_Glucose_mmol_L)
 anova_lm_log_Model2_1K_Insulin_uU_mL <- Anova(lm_log_Model2_1K_Insulin_uU_mL)
 
 anova_lm_log_Model2_1K_HbA1c <- Anova(lm_log_Model2_1K_HbA1c)
 anova_lm_log_Model2_1K_beta_cell_function <- Anova(lm_log_Model2_1K_beta_cell_function)
 anova_lm_log_Model2_1K_HOMA_IR <- Anova(lm_log_Model2_1K_HOMA_IR)
 
 anova_lm_log_Model2_1K_Systolic_BP <- Anova(lm_log_Model2_1K_Systolic_BP)
 anova_lm_log_Model2_1K_Diastolic_BP <- Anova(lm_log_Model2_1K_Diastolic_BP)
 
 anova_lm_log_Model2_1K_FIBERP <- Anova(lm_log_Model2_1K_FIBER)
 anova_lm_log_Model2_1K_Ca <- Anova(lm_log_Model2_1K_Ca)
 anova_lm_log_Model2_1K_P<- Anova(lm_log_Model2_1K_P)
 anova_lm_log_Model2_1K_Fe_total <- Anova(lm_log_Model2_1K_Fe_total)
 anova_lm_log_Model2_1K_NaP <- Anova(lm_log_Model2_1K_Na)
 anova_lm_log_Model2_1K_K <- Anova(lm_log_Model2_1K_K)
 anova_lm_log_Model2_1K_Mg <- Anova(lm_log_Model2_1K_Mg)
 anova_lm_log_Model2_1K_Zn <- Anova(lm_log_Model2_1K_Zn)
 anova_lm_log_Model2_1K_Cu <- Anova(lm_log_Model2_1K_Cu)
 anova_lm_log_Model2_1K_Mn <- Anova(lm_log_Model2_1K_Mn)
 
 
 anova_lm_log_Model2_1K_Vit_A_ER <- Anova(lm_log_Model2_1K_Vit_A_ER)
 anova_lm_log_Model2_1K_B1 <- Anova(lm_log_Model2_1K_B1)
 anova_lm_log_Model2_1K_B2 <- Anova(lm_log_Model2_1K_B2)
 anova_lm_log_Model2_1K_B3 <- Anova(lm_log_Model2_1K_B3)
 anova_lm_log_Model2_1K_Ac_pantoenico <- Anova(lm_log_Model2_1K_Ac_pantoenico)
 anova_lm_log_Model2_1K_B6 <- Anova(lm_log_Model2_1K_B6)
 anova_lm_log_Model2_1K_Folic <- Anova(lm_log_Model2_1K_Folic)
 anova_lm_log_Model2_1K_B12 <- Anova(lm_log_Model2_1K_B12)
 anova_lm_log_Model2_1K_Vit_C <- Anova(lm_log_Model2_1K_Vit_C)
 
 
 anova_lm_log_Model2_1K_Prevotella <- Anova(lm_log_Model2_1K_Prevotella)
 anova_lm_log_Model2_1K_Lachnospiraceae <- Anova(lm_log_Model2_1K_Lachnospiraceae)
 anova_lm_log_Model2_1K_Pathogen <- Anova(lm_log_Model2_1K_Pathogen)
 anova_lm_log_Model2_1K_Akkermansia.Bacteroidales <- Anova(lm_log_Model2_1K_Akkermansia.Bacteroidales)
 anova_lm_log_Model2_1K_Ruminococcaceae<- Anova(lm_log_Model2_1K_Ruminococcaceae)
 
 anova_lm_log_Model2_1K_Log_TyG_index <- Anova(lm_log_Model2_1K_Log_TyG_index)
 anova_lm_log_Model2_1K_Log_TyG_BMI <- Anova(lm_log_Model2_1K_Log_TyG_BMI)
 anova_lm_log_Model2_1K_Log_TyG_WCn <- Anova(lm_log_Model2_1K_Log_TyG_WCn)
 anova_lm_log_Model2_1K_Log_TyG_WHtR <- Anova(lm_log_Model2_1K_Log_TyG_WHtR)
 anova_lm_log_Model2_1K_Log_ABSI <- Anova(lm_log_Model2_1K_Log_ABSI)
 
 
 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_Model2_1K<- list(
   Age_Model2_1K= anova_lm_log_Model2_1K_Age,
   TPC_Folin_Model2_1K= anova_lm_log_Model2_1K_TPC_Folin,
   E_DII_score_Model2_1K= anova_lm_log_lm_log_Model2_1K_E_DII_score,
   Weight_Model2_1K= anova_lm_log_Model2_1K_Weight,
   BMI_Model2_1K= anova_lm_log_Model2_1K_BMI,
   Waist_Circumference_Model2_1K= anova_lm_log_Model2_1K_Waist_Circumference,
   RFM_Model2_1K= anova_lm_log_Model2_1K_RFM,
   Leptina_Model2_1K= anova_lm_log_Model2_1K_Leptina_ng_mL,
   Adiponectin_Model2_1K= anova_lm_log_Model2_1K_Adiponectin_ug_ml,
   Lep_Adip_Model2_1K= anova_lm_log_Model2_1K_Lep_Adip,
   ApoB_Model2_1K= anova_lm_log_Model2_1K_ApoB_mg_dL,
   HDL_Model2_1K= anova_lm_log_Model2_1K_HDL_mg_dL,
   VLDL_Model2_1K= anova_lm_log_Model2_1K_VLDL_mg_dL,
   LDL_Model2_1K= anova_lm_log_Model2_1K_LDL_mg_dL,
   T_Cholesterol_Model2_1K= anova_lm_log_Model2_1K_T.Cholesterol_mg_dL_,
   TGs_Model2_1K= anova_lm_log_Model2_1K_TGs_mg_dL,
   Atherogenic_index_Model2_1K= anova_lm_log_Model2_1K_Atherogenic_index,
   Creatinine_Model2_1K= anova_lm_log_Model2_1K_Creatinine_mg_dL_,
   hsCRP_Model2_1K= anova_lm_log_Model2_1K_hsCRP_mg_L,
   Glucose_Model2_1K= anova_lm_log_Model2_1K_Glucose_mmol_L,
   Insulin_Model2_1K= anova_lm_log_Model2_1K_Insulin_uU_mL,
   HbA1c_Model2_1K= anova_lm_log_Model2_1K_HbA1c,
   Beta_cell_function_Model2_1K= anova_lm_log_Model2_1K_beta_cell_function,
   HOMA_IR_Model2_1K= anova_lm_log_Model2_1K_HOMA_IR,
   Systolic_BP_Model2_1K= anova_lm_log_Model2_1K_Systolic_BP,
   Diastolic_BP_Model2_1K= anova_lm_log_Model2_1K_Diastolic_BP,
   Fiber_Model2_1K= anova_lm_log_Model2_1K_FIBERP,
   Ca_Model2_1K= anova_lm_log_Model2_1K_Ca,
   P_Model2_1K= anova_lm_log_Model2_1K_P,
   Fe_total_Model2_1K= anova_lm_log_Model2_1K_Fe_total,
   Na_Model2_1K= anova_lm_log_Model2_1K_NaP,
   K_Model2_1K= anova_lm_log_Model2_1K_K,
   Mg_Model2_1K= anova_lm_log_Model2_1K_Mg,
   Zn_Model2_1K= anova_lm_log_Model2_1K_Zn,
   Cu_Model2_1K= anova_lm_log_Model2_1K_Cu,
   Mn_Model2_1K= anova_lm_log_Model2_1K_Mn,
   Vit_A_ER_Model2_1K= anova_lm_log_Model2_1K_Vit_A_ER,
   B1_Model2_1K= anova_lm_log_Model2_1K_B1,
   B2_Model2_1K= anova_lm_log_Model2_1K_B2,
   B3_Model2_1K= anova_lm_log_Model2_1K_B3,
   Ac_pantoenico_Model2_1K= anova_lm_log_Model2_1K_Ac_pantoenico,
   B6_Model2_1K= anova_lm_log_Model2_1K_B6,
   Folic_Model2_1K= anova_lm_log_Model2_1K_Folic,
   B12_Model2_1K= anova_lm_log_Model2_1K_B12,
   Vit_C_Model2_1K= anova_lm_log_Model2_1K_Vit_C,
   Prevotella_Model2_1K= anova_lm_log_Model2_1K_Prevotella,
   Lachnospiraceae_Model2_1K= anova_lm_log_Model2_1K_Lachnospiraceae,
   Pathogen_Model2_1K= anova_lm_log_Model2_1K_Pathogen,
   Akkermansia_Bacteroidales_Model2_1K= anova_lm_log_Model2_1K_Akkermansia.Bacteroidales,
   Ruminococcaceae_Model2_1K= anova_lm_log_Model2_1K_Ruminococcaceae,
   TyG_index_Model2_1K = anova_lm_log_Model2_1K_Log_TyG_index,
   TyG_BMI_Model2_1K= anova_lm_log_Model2_1K_Log_TyG_BMI,
   TyG_WCn_Model2_1K= anova_lm_log_Model2_1K_Log_TyG_WCn,
   TyG_WHtRModel2 = anova_lm_log_Model2_1K_Log_TyG_WHtR,
   ABSI_Model2_1K= anova_lm_log_Model2_1K_Log_ABSI
 )
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_Model2_1K<- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_Model2_1K)) {
   model <- anova_Model2_1K[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_Model2_1K<- rbind(anova_results_Model2_1K, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_Model2_1K, "anova_results_Model2_1K.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results__Model2_1Kxlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_Model2_1K <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_Model2_1K)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_Model2_1K$Q_Value <- p.adjust(anova_results_Model2_1K$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_Model2_1K, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_Model2_1K.xlsx", "anova_results_with_qvalues_Model2_1K.xlsx")
 
 
 
 
 ########lm Modelo 3 (Log_Age~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+DB_Polyphenols$Actividad_fisica+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 colnames(DB_Polyphenols)
 
 lm_log_Model3_1K_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin )
 lm_log_Model3_1K_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_1000+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 lm_log_Model3_1K_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_1000+   DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_1000+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 lm_log_Model3_1K_Log_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Log_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Log_TyG_WCn <- lm(Log_TyG_WC~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Log_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 lm_log_Model3_1K_Log_ABSI <- lm(Log_ABSI~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_bin)
 
 
 
 #ANOVA-Model 2
 
 
 anova_lm_log_Model3_1K_Age <- Anova(lm_log_Model3_1K_Age)
 anova_lm_log_Model3_1K_TPC_Folin <- Anova(lm_log_Model3_1K_TPC_Folin)
 anova_lm_log_lm_log_Model3_1K_E_DII_score <- Anova(lm_log_Model3_1K_E_DII_score)
 
 anova_lm_log_Model3_1K_Weight <- Anova(lm_log_Model3_1K_Weight)
 anova_lm_log_Model3_1K_BMI <- Anova(lm_log_Model3_1K_BMI)
 anova_lm_log_Model3_1K_Waist_Circumference <- Anova(lm_log_Model3_1K_Waist_Circumference)
 anova_lm_log_Model3_1K_RFM <- Anova(lm_log_Model3_1K_RFM)
 
 anova_lm_log_Model3_1K_Leptina_ng_mL <- Anova(lm_log_Model3_1K_Leptina_ng_mL)
 anova_lm_log_Model3_1K_Adiponectin_ug_ml <- Anova(lm_log_Model3_1K_Adiponectin_ug_ml)
 anova_lm_log_Model3_1K_Lep_Adip <- Anova(lm_log_Model3_1K_Lep_Adip)
 
 
 anova_lm_log_Model3_1K_ApoB_mg_dL <- Anova(lm_log_Model3_1K_ApoB_mg_dL)
 anova_lm_log_Model3_1K_HDL_mg_dL <- Anova(lm_log_Model3_1K_HDL_mg_dL)
 anova_lm_log_Model3_1K_VLDL_mg_dL <- Anova(lm_log_Model3_1K_VLDL_mg_dL)
 anova_lm_log_Model3_1K_LDL_mg_dL <- Anova(lm_log_Model3_1K_LDL_mg_dL)
 anova_lm_log_Model3_1K_T.Cholesterol_mg_dL_ <- Anova(lm_log_Model3_1K_T.Cholesterol_mg_dL_)
 anova_lm_log_Model3_1K_TGs_mg_dL <- Anova(lm_log_Model3_1K_TGs_mg_dL)
 anova_lm_log_Model3_1K_Atherogenic_index <- Anova(lm_log_Model3_1K_Atherogenic_index)
 
 anova_lm_log_Model3_1K_Creatinine_mg_dL_ <- Anova(lm_log_Model3_1K_Creatinine_mg_dL_)
 anova_lm_log_Model3_1K_hsCRP_mg_L <- Anova(lm_log_Model3_1K_hsCRP_mg_L)
 
 anova_lm_log_Model3_1K_Glucose_mmol_L <- Anova(lm_log_Model3_1K_Glucose_mmol_L)
 anova_lm_log_Model3_1K_Insulin_uU_mL <- Anova(lm_log_Model3_1K_Insulin_uU_mL)
 
 anova_lm_log_Model3_1K_HbA1c <- Anova(lm_log_Model3_1K_HbA1c)
 anova_lm_log_Model3_1K_beta_cell_function <- Anova(lm_log_Model3_1K_beta_cell_function)
 anova_lm_log_Model3_1K_HOMA_IR <- Anova(lm_log_Model3_1K_HOMA_IR)
 
 anova_lm_log_Model3_1K_Systolic_BP <- Anova(lm_log_Model3_1K_Systolic_BP)
 anova_lm_log_Model3_1K_Diastolic_BP <- Anova(lm_log_Model3_1K_Diastolic_BP)
 
 anova_lm_log_Model3_1K_FIBERP <- Anova(lm_log_Model3_1K_FIBER)
 anova_lm_log_Model3_1K_Ca <- Anova(lm_log_Model3_1K_Ca)
 anova_lm_log_Model3_1K_P<- Anova(lm_log_Model3_1K_P)
 anova_lm_log_Model3_1K_Fe_total <- Anova(lm_log_Model3_1K_Fe_total)
 anova_lm_log_Model3_1K_NaP <- Anova(lm_log_Model3_1K_Na)
 anova_lm_log_Model3_1K_K <- Anova(lm_log_Model3_1K_K)
 anova_lm_log_Model3_1K_Mg <- Anova(lm_log_Model3_1K_Mg)
 anova_lm_log_Model3_1K_Zn <- Anova(lm_log_Model3_1K_Zn)
 anova_lm_log_Model3_1K_Cu <- Anova(lm_log_Model3_1K_Cu)
 anova_lm_log_Model3_1K_Mn <- Anova(lm_log_Model3_1K_Mn)
 
 
 anova_lm_log_Model3_1K_Vit_A_ER <- Anova(lm_log_Model3_1K_Vit_A_ER)
 anova_lm_log_Model3_1K_B1 <- Anova(lm_log_Model3_1K_B1)
 anova_lm_log_Model3_1K_B2 <- Anova(lm_log_Model3_1K_B2)
 anova_lm_log_Model3_1K_B3 <- Anova(lm_log_Model3_1K_B3)
 anova_lm_log_Model3_1K_Ac_pantoenico <- Anova(lm_log_Model3_1K_Ac_pantoenico)
 anova_lm_log_Model3_1K_B6 <- Anova(lm_log_Model3_1K_B6)
 anova_lm_log_Model3_1K_Folic <- Anova(lm_log_Model3_1K_Folic)
 anova_lm_log_Model3_1K_B12 <- Anova(lm_log_Model3_1K_B12)
 anova_lm_log_Model3_1K_Vit_C <- Anova(lm_log_Model3_1K_Vit_C)
 
 
 anova_lm_log_Model3_1K_Prevotella <- Anova(lm_log_Model3_1K_Prevotella)
 anova_lm_log_Model3_1K_Lachnospiraceae <- Anova(lm_log_Model3_1K_Lachnospiraceae)
 anova_lm_log_Model3_1K_Pathogen <- Anova(lm_log_Model3_1K_Pathogen)
 anova_lm_log_Model3_1K_Akkermansia.Bacteroidales <- Anova(lm_log_Model3_1K_Akkermansia.Bacteroidales)
 anova_lm_log_Model3_1K_Ruminococcaceae<- Anova(lm_log_Model3_1K_Ruminococcaceae)
 
 anova_lm_log_Model3_1K_Log_TyG_index <- Anova(lm_log_Model3_1K_Log_TyG_index)
 anova_lm_log_Model3_1K_Log_TyG_BMI <- Anova(lm_log_Model3_1K_Log_TyG_BMI)
 anova_lm_log_Model3_1K_Log_TyG_WCn <- Anova(lm_log_Model3_1K_Log_TyG_WCn)
 anova_lm_log_Model3_1K_Log_TyG_WHtR <- Anova(lm_log_Model3_1K_Log_TyG_WHtR)
 anova_lm_log_Model3_1K_Log_ABSI <- Anova(lm_log_Model3_1K_Log_ABSI)
 
 
 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_Model3_1K<- list(
   Age_Model3_1K= anova_lm_log_Model3_1K_Age,
   TPC_Folin_Model3_1K= anova_lm_log_Model3_1K_TPC_Folin,
   E_DII_score_Model3_1K= anova_lm_log_lm_log_Model3_1K_E_DII_score,
   Weight_Model3_1K= anova_lm_log_Model3_1K_Weight,
   BMI_Model3_1K= anova_lm_log_Model3_1K_BMI,
   Waist_Circumference_Model3_1K= anova_lm_log_Model3_1K_Waist_Circumference,
   RFM_Model3_1K= anova_lm_log_Model3_1K_RFM,
   Leptina_Model3_1K= anova_lm_log_Model3_1K_Leptina_ng_mL,
   Adiponectin_Model3_1K= anova_lm_log_Model3_1K_Adiponectin_ug_ml,
   Lep_Adip_Model3_1K= anova_lm_log_Model3_1K_Lep_Adip,
   ApoB_Model3_1K= anova_lm_log_Model3_1K_ApoB_mg_dL,
   HDL_Model3_1K= anova_lm_log_Model3_1K_HDL_mg_dL,
   VLDL_Model3_1K= anova_lm_log_Model3_1K_VLDL_mg_dL,
   LDL_Model3_1K= anova_lm_log_Model3_1K_LDL_mg_dL,
   T_Cholesterol_Model3_1K= anova_lm_log_Model3_1K_T.Cholesterol_mg_dL_,
   TGs_Model3_1K= anova_lm_log_Model3_1K_TGs_mg_dL,
   Atherogenic_index_Model3_1K= anova_lm_log_Model3_1K_Atherogenic_index,
   Creatinine_Model3_1K= anova_lm_log_Model3_1K_Creatinine_mg_dL_,
   hsCRP_Model3_1K= anova_lm_log_Model3_1K_hsCRP_mg_L,
   Glucose_Model3_1K= anova_lm_log_Model3_1K_Glucose_mmol_L,
   Insulin_Model3_1K= anova_lm_log_Model3_1K_Insulin_uU_mL,
   HbA1c_Model3_1K= anova_lm_log_Model3_1K_HbA1c,
   Beta_cell_function_Model3_1K= anova_lm_log_Model3_1K_beta_cell_function,
   HOMA_IR_Model3_1K= anova_lm_log_Model3_1K_HOMA_IR,
   Systolic_BP_Model3_1K= anova_lm_log_Model3_1K_Systolic_BP,
   Diastolic_BP_Model3_1K= anova_lm_log_Model3_1K_Diastolic_BP,
   Fiber_Model3_1K= anova_lm_log_Model3_1K_FIBERP,
   Ca_Model3_1K= anova_lm_log_Model3_1K_Ca,
   P_Model3_1K= anova_lm_log_Model3_1K_P,
   Fe_total_Model3_1K= anova_lm_log_Model3_1K_Fe_total,
   Na_Model3_1K= anova_lm_log_Model3_1K_NaP,
   K_Model3_1K= anova_lm_log_Model3_1K_K,
   Mg_Model3_1K= anova_lm_log_Model3_1K_Mg,
   Zn_Model3_1K= anova_lm_log_Model3_1K_Zn,
   Cu_Model3_1K= anova_lm_log_Model3_1K_Cu,
   Mn_Model3_1K= anova_lm_log_Model3_1K_Mn,
   Vit_A_ER_Model3_1K= anova_lm_log_Model3_1K_Vit_A_ER,
   B1_Model3_1K= anova_lm_log_Model3_1K_B1,
   B2_Model3_1K= anova_lm_log_Model3_1K_B2,
   B3_Model3_1K= anova_lm_log_Model3_1K_B3,
   Ac_pantoenico_Model3_1K= anova_lm_log_Model3_1K_Ac_pantoenico,
   B6_Model3_1K= anova_lm_log_Model3_1K_B6,
   Folic_Model3_1K= anova_lm_log_Model3_1K_Folic,
   B12_Model3_1K= anova_lm_log_Model3_1K_B12,
   Vit_C_Model3_1K= anova_lm_log_Model3_1K_Vit_C,
   Prevotella_Model3_1K= anova_lm_log_Model3_1K_Prevotella,
   Lachnospiraceae_Model3_1K= anova_lm_log_Model3_1K_Lachnospiraceae,
   Pathogen_Model3_1K= anova_lm_log_Model3_1K_Pathogen,
   Akkermansia_Bacteroidales_Model3_1K= anova_lm_log_Model3_1K_Akkermansia.Bacteroidales,
   Ruminococcaceae_Model3_1K= anova_lm_log_Model3_1K_Ruminococcaceae,
   TyG_index_Model3_1K = anova_lm_log_Model3_1K_Log_TyG_index,
   TyG_BMI_Model3_1K= anova_lm_log_Model3_1K_Log_TyG_BMI,
   TyG_WCn_Model3_1K= anova_lm_log_Model3_1K_Log_TyG_WCn,
   TyG_WHtRModel2 = anova_lm_log_Model3_1K_Log_TyG_WHtR,
   ABSI_Model3_1K= anova_lm_log_Model3_1K_Log_ABSI
 )
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_Model3_1K<- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_Model3_1K)) {
   model <- anova_Model3_1K[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_Model3_1K<- rbind(anova_results_Model3_1K, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_Model3_1K, "anova_results_Model3_1K.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results__Model3_1Kxlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_Model3_1K <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_Model3_1K)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_Model3_1K$Q_Value <- p.adjust(anova_results_Model3_1K$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_Model3_1K, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_Model3_1K.xlsx", "anova_results_with_qvalues_Model3_1K.xlsx")
 
 
 
 ######MODELO 4
 
 ########lm Modelo  (Log_Age~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 colnames(DB_Polyphenols)
 
 lm_log_Model4_1K_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st )
 lm_log_Model4_1K_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_1000+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 
 lm_log_Model4_1K_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_1000+   DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_1000+  DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_1000+DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+  DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_1000+ DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range+ DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 lm_log_Model4_1K_Log_TyG_index <- lm(Log_TyG_index~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Log_TyG_BMI <- lm(Log_TyG_BMI~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Log_TyG_WCn <- lm(Log_TyG_WC~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Log_TyG_WHtR <- lm(Log_TyG_WHtR~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 lm_log_Model4_1K_Log_ABSI <- lm(Log_ABSI~DB_Polyphenols$TPC_Folin_1000 + DB_Polyphenols$City+DB_Polyphenols$Sex+ DB_Polyphenols$Age_range + DB_Polyphenols$Alcohol+DB_Polyphenols$Tabaquismo_st)
 
 
 
 #ANOVA-Model 2
 
 
 anova_lm_log_Model4_1K_Age <- Anova(lm_log_Model4_1K_Age)
 anova_lm_log_Model4_1K_TPC_Folin <- Anova(lm_log_Model4_1K_TPC_Folin)
 anova_lm_log_lm_log_Model4_1K_E_DII_score <- Anova(lm_log_Model4_1K_E_DII_score)
 
 anova_lm_log_Model4_1K_Weight <- Anova(lm_log_Model4_1K_Weight)
 anova_lm_log_Model4_1K_BMI <- Anova(lm_log_Model4_1K_BMI)
 anova_lm_log_Model4_1K_Waist_Circumference <- Anova(lm_log_Model4_1K_Waist_Circumference)
 anova_lm_log_Model4_1K_RFM <- Anova(lm_log_Model4_1K_RFM)
 
 anova_lm_log_Model4_1K_Leptina_ng_mL <- Anova(lm_log_Model4_1K_Leptina_ng_mL)
 anova_lm_log_Model4_1K_Adiponectin_ug_ml <- Anova(lm_log_Model4_1K_Adiponectin_ug_ml)
 anova_lm_log_Model4_1K_Lep_Adip <- Anova(lm_log_Model4_1K_Lep_Adip)
 
 
 anova_lm_log_Model4_1K_ApoB_mg_dL <- Anova(lm_log_Model4_1K_ApoB_mg_dL)
 anova_lm_log_Model4_1K_HDL_mg_dL <- Anova(lm_log_Model4_1K_HDL_mg_dL)
 anova_lm_log_Model4_1K_VLDL_mg_dL <- Anova(lm_log_Model4_1K_VLDL_mg_dL)
 anova_lm_log_Model4_1K_LDL_mg_dL <- Anova(lm_log_Model4_1K_LDL_mg_dL)
 anova_lm_log_Model4_1K_T.Cholesterol_mg_dL_ <- Anova(lm_log_Model4_1K_T.Cholesterol_mg_dL_)
 anova_lm_log_Model4_1K_TGs_mg_dL <- Anova(lm_log_Model4_1K_TGs_mg_dL)
 anova_lm_log_Model4_1K_Atherogenic_index <- Anova(lm_log_Model4_1K_Atherogenic_index)
 
 anova_lm_log_Model4_1K_Creatinine_mg_dL_ <- Anova(lm_log_Model4_1K_Creatinine_mg_dL_)
 anova_lm_log_Model4_1K_hsCRP_mg_L <- Anova(lm_log_Model4_1K_hsCRP_mg_L)
 
 anova_lm_log_Model4_1K_Glucose_mmol_L <- Anova(lm_log_Model4_1K_Glucose_mmol_L)
 anova_lm_log_Model4_1K_Insulin_uU_mL <- Anova(lm_log_Model4_1K_Insulin_uU_mL)
 
 anova_lm_log_Model4_1K_HbA1c <- Anova(lm_log_Model4_1K_HbA1c)
 anova_lm_log_Model4_1K_beta_cell_function <- Anova(lm_log_Model4_1K_beta_cell_function)
 anova_lm_log_Model4_1K_HOMA_IR <- Anova(lm_log_Model4_1K_HOMA_IR)
 
 anova_lm_log_Model4_1K_Systolic_BP <- Anova(lm_log_Model4_1K_Systolic_BP)
 anova_lm_log_Model4_1K_Diastolic_BP <- Anova(lm_log_Model4_1K_Diastolic_BP)
 
 anova_lm_log_Model4_1K_FIBERP <- Anova(lm_log_Model4_1K_FIBER)
 anova_lm_log_Model4_1K_Ca <- Anova(lm_log_Model4_1K_Ca)
 anova_lm_log_Model4_1K_P<- Anova(lm_log_Model4_1K_P)
 anova_lm_log_Model4_1K_Fe_total <- Anova(lm_log_Model4_1K_Fe_total)
 anova_lm_log_Model4_1K_NaP <- Anova(lm_log_Model4_1K_Na)
 anova_lm_log_Model4_1K_K <- Anova(lm_log_Model4_1K_K)
 anova_lm_log_Model4_1K_Mg <- Anova(lm_log_Model4_1K_Mg)
 anova_lm_log_Model4_1K_Zn <- Anova(lm_log_Model4_1K_Zn)
 anova_lm_log_Model4_1K_Cu <- Anova(lm_log_Model4_1K_Cu)
 anova_lm_log_Model4_1K_Mn <- Anova(lm_log_Model4_1K_Mn)
 
 
 anova_lm_log_Model4_1K_Vit_A_ER <- Anova(lm_log_Model4_1K_Vit_A_ER)
 anova_lm_log_Model4_1K_B1 <- Anova(lm_log_Model4_1K_B1)
 anova_lm_log_Model4_1K_B2 <- Anova(lm_log_Model4_1K_B2)
 anova_lm_log_Model4_1K_B3 <- Anova(lm_log_Model4_1K_B3)
 anova_lm_log_Model4_1K_Ac_pantoenico <- Anova(lm_log_Model4_1K_Ac_pantoenico)
 anova_lm_log_Model4_1K_B6 <- Anova(lm_log_Model4_1K_B6)
 anova_lm_log_Model4_1K_Folic <- Anova(lm_log_Model4_1K_Folic)
 anova_lm_log_Model4_1K_B12 <- Anova(lm_log_Model4_1K_B12)
 anova_lm_log_Model4_1K_Vit_C <- Anova(lm_log_Model4_1K_Vit_C)
 
 
 anova_lm_log_Model4_1K_Prevotella <- Anova(lm_log_Model4_1K_Prevotella)
 anova_lm_log_Model4_1K_Lachnospiraceae <- Anova(lm_log_Model4_1K_Lachnospiraceae)
 anova_lm_log_Model4_1K_Pathogen <- Anova(lm_log_Model4_1K_Pathogen)
 anova_lm_log_Model4_1K_Akkermansia.Bacteroidales <- Anova(lm_log_Model4_1K_Akkermansia.Bacteroidales)
 anova_lm_log_Model4_1K_Ruminococcaceae<- Anova(lm_log_Model4_1K_Ruminococcaceae)
 
 anova_lm_log_Model4_1K_Log_TyG_index <- Anova(lm_log_Model4_1K_Log_TyG_index)
 anova_lm_log_Model4_1K_Log_TyG_BMI <- Anova(lm_log_Model4_1K_Log_TyG_BMI)
 anova_lm_log_Model4_1K_Log_TyG_WCn <- Anova(lm_log_Model4_1K_Log_TyG_WCn)
 anova_lm_log_Model4_1K_Log_TyG_WHtR <- Anova(lm_log_Model4_1K_Log_TyG_WHtR)
 anova_lm_log_Model4_1K_Log_ABSI <- Anova(lm_log_Model4_1K_Log_ABSI)
 
 
 
 # Cargar librerías
 library(car)
 library(writexl)
 
 # Lista de modelos ANOVA
 anova_Model4_1K<- list(
   Age_Model4_1K= anova_lm_log_Model4_1K_Age,
   TPC_Folin_Model4_1K= anova_lm_log_Model4_1K_TPC_Folin,
   E_DII_score_Model4_1K= anova_lm_log_lm_log_Model4_1K_E_DII_score,
   Weight_Model4_1K= anova_lm_log_Model4_1K_Weight,
   BMI_Model4_1K= anova_lm_log_Model4_1K_BMI,
   Waist_Circumference_Model4_1K= anova_lm_log_Model4_1K_Waist_Circumference,
   RFM_Model4_1K= anova_lm_log_Model4_1K_RFM,
   Leptina_Model4_1K= anova_lm_log_Model4_1K_Leptina_ng_mL,
   Adiponectin_Model4_1K= anova_lm_log_Model4_1K_Adiponectin_ug_ml,
   Lep_Adip_Model4_1K= anova_lm_log_Model4_1K_Lep_Adip,
   ApoB_Model4_1K= anova_lm_log_Model4_1K_ApoB_mg_dL,
   HDL_Model4_1K= anova_lm_log_Model4_1K_HDL_mg_dL,
   VLDL_Model4_1K= anova_lm_log_Model4_1K_VLDL_mg_dL,
   LDL_Model4_1K= anova_lm_log_Model4_1K_LDL_mg_dL,
   T_Cholesterol_Model4_1K= anova_lm_log_Model4_1K_T.Cholesterol_mg_dL_,
   TGs_Model4_1K= anova_lm_log_Model4_1K_TGs_mg_dL,
   Atherogenic_index_Model4_1K= anova_lm_log_Model4_1K_Atherogenic_index,
   Creatinine_Model4_1K= anova_lm_log_Model4_1K_Creatinine_mg_dL_,
   hsCRP_Model4_1K= anova_lm_log_Model4_1K_hsCRP_mg_L,
   Glucose_Model4_1K= anova_lm_log_Model4_1K_Glucose_mmol_L,
   Insulin_Model4_1K= anova_lm_log_Model4_1K_Insulin_uU_mL,
   HbA1c_Model4_1K= anova_lm_log_Model4_1K_HbA1c,
   Beta_cell_function_Model4_1K= anova_lm_log_Model4_1K_beta_cell_function,
   HOMA_IR_Model4_1K= anova_lm_log_Model4_1K_HOMA_IR,
   Systolic_BP_Model4_1K= anova_lm_log_Model4_1K_Systolic_BP,
   Diastolic_BP_Model4_1K= anova_lm_log_Model4_1K_Diastolic_BP,
   Fiber_Model4_1K= anova_lm_log_Model4_1K_FIBERP,
   Ca_Model4_1K= anova_lm_log_Model4_1K_Ca,
   P_Model4_1K= anova_lm_log_Model4_1K_P,
   Fe_total_Model4_1K= anova_lm_log_Model4_1K_Fe_total,
   Na_Model4_1K= anova_lm_log_Model4_1K_NaP,
   K_Model4_1K= anova_lm_log_Model4_1K_K,
   Mg_Model4_1K= anova_lm_log_Model4_1K_Mg,
   Zn_Model4_1K= anova_lm_log_Model4_1K_Zn,
   Cu_Model4_1K= anova_lm_log_Model4_1K_Cu,
   Mn_Model4_1K= anova_lm_log_Model4_1K_Mn,
   Vit_A_ER_Model4_1K= anova_lm_log_Model4_1K_Vit_A_ER,
   B1_Model4_1K= anova_lm_log_Model4_1K_B1,
   B2_Model4_1K= anova_lm_log_Model4_1K_B2,
   B3_Model4_1K= anova_lm_log_Model4_1K_B3,
   Ac_pantoenico_Model4_1K= anova_lm_log_Model4_1K_Ac_pantoenico,
   B6_Model4_1K= anova_lm_log_Model4_1K_B6,
   Folic_Model4_1K= anova_lm_log_Model4_1K_Folic,
   B12_Model4_1K= anova_lm_log_Model4_1K_B12,
   Vit_C_Model4_1K= anova_lm_log_Model4_1K_Vit_C,
   Prevotella_Model4_1K= anova_lm_log_Model4_1K_Prevotella,
   Lachnospiraceae_Model4_1K= anova_lm_log_Model4_1K_Lachnospiraceae,
   Pathogen_Model4_1K= anova_lm_log_Model4_1K_Pathogen,
   Akkermansia_Bacteroidales_Model4_1K= anova_lm_log_Model4_1K_Akkermansia.Bacteroidales,
   Ruminococcaceae_Model4_1K= anova_lm_log_Model4_1K_Ruminococcaceae,
   TyG_index_Model4_1K = anova_lm_log_Model4_1K_Log_TyG_index,
   TyG_BMI_Model4_1K= anova_lm_log_Model4_1K_Log_TyG_BMI,
   TyG_WCn_Model4_1K= anova_lm_log_Model4_1K_Log_TyG_WCn,
   TyG_WHtRModel2 = anova_lm_log_Model4_1K_Log_TyG_WHtR,
   ABSI_Model4_1K= anova_lm_log_Model4_1K_Log_ABSI
 )
 
 # Crear un dataframe vacío para almacenar los resultados
 anova_results_Model4_1K<- data.frame(Variable = character(), Pr_F = numeric(), stringsAsFactors = FALSE)
 
 # Extraer los valores de "Pr(>F)" de cada modelo ANOVA
 for (var in names(anova_Model4_1K)) {
   model <- anova_Model4_1K[[var]]
   
   # Verificar si el modelo tiene la columna "Pr(>F)"
   if ("Pr(>F)" %in% colnames(model)) {
     p_values <- model$`Pr(>F)`
     
     # Agregar los resultados al dataframe
     anova_results_Model4_1K<- rbind(anova_results_Model4_1K, data.frame(Variable = var, Pr_F = p_values[1])) # Solo primer término
   }
 }
 
 # Guardar el archivo en Excel
 
 write_xlsx(anova_results_Model4_1K, "anova_results_Model4_1K.xlsx")
 
 # Mensaje de confirmación
 cat("✅ Archivo 'anova_results__Model4_1Kxlsx' guardado con éxito.\n")
 
 #Qvalue
 
 
 
 # Cargar librerías
 library(openxlsx)
 library(writexl)
 
 # Función para calcular q-values (ajuste FDR)
 calculate_qvalues <- function(input_file, output_file) {
   
   # Cargar los resultados desde Excel
   anova_results_Model4_1K <- read.xlsx(input_file)
   
   # Verificar que la columna Pr_F existe
   if (!"Pr_F" %in% colnames(anova_results_Model4_1K)) {
     stop("Error: No se encontró la columna 'Pr_F' en el archivo.")
   }
   
   # Calcular los q-values (ajuste de p-values con método Benjamini-Hochberg)
   anova_results_Model4_1K$Q_Value <- p.adjust(anova_results_Model4_1K$Pr_F, method = "fdr")
   
   # Guardar los resultados en un nuevo archivo Excel
   write_xlsx(anova_results_Model4_1K, output_file)
   
   # Mensaje de confirmación
   cat("✅ Archivo con q-values guardado en:", output_file, "\n")
 }
 
 # Uso de la función
 calculate_qvalues("anova_results_Model4_1K.xlsx", "anova_results_with_qvalues_Model4_1K.xlsx")
 
 
 
 
 
 
 
 ###
 
 colnames(DB_Polyphenols)
 DB_Polyphe_Segmented <- DB_Polyphenols [c(30, 33, 78,18, 13, 20, 26,82,83,85)] 
 
 
 library(brms)
 library(RcppParallel) 
 library(rstantools) 
 library(loo)
 library(posterior)
 library(distributional)
 library(tidyverse)
 library(bayesplot)
 library(bridgesampling)
 library(coda)
 library(Brobdingnag)
 library(Matrix)
 
 
 # Cargar datos
 DB <- read.csv("DB_Polyphe_Segmented.csv")  # Ajusta si el formato es diferente
 
 library(installr)  # Cargar paquete
 updateR()
 
 library(brms)
 library(tidyverse)
 library(rstan)
 library(QuickJSR)
 library(inline)
 library(StanHeaders)
 
 # Crear una variable categórica basada en el umbral de 1000 mg/día
 DB_Polyphe_Segmented <- DB_Polyphe_Segmented %>%
   mutate(Grupo_Consumo = ifelse(TPC_Folin < 1000, "Bajo", "Alto"))
 
 # Ajustar el modelo Bayesiano con el grupo de consumo como predictor
 modelo_bayesiano <- brm(
   BMI ~ Grupo_Consumo * TPC_Folin,  # Modelo con interacción
   data = DB_Polyphe_Segmented,
   family = gaussian(),
   prior = c(
     prior(normal(25, 10), class = "Intercept"),
     prior(normal(0, 5), class = "b")
   ),
   chains = 4, iter = 4000, warmup = 1000, cores = 4
 )
 
 # Resumen del modelo
 summary(modelo_bayesiano)
 
 # Visualización
 DB_Polyphe_Segmented %>%
   ggplot(aes(x = TPC_Folin, y = BMI, color = Grupo_Consumo)) +
   geom_point(alpha = 0.5) +
   geom_smooth(method = "lm", se = FALSE) +
   theme_minimal()
 
 
 
 
 
 #summary ANOVA-FOLIN adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_FOLIN_TERTILS <- cbind (anova_lm_log_Folin_Age$`Pr(>F)`,
 anova_lm_log_Folin_ApoB_mg_dL$`Pr(>F)`,
 anova_lm_log_Folin_HDL_mg_dL$`Pr(>F)`,
 anova_lm_log_Folin_VLDL_mg_dL$`Pr(>F)`,
 anova_lm_log_Folin_LDL_mg_dL$`Pr(>F)`,
 anova_lm_log_Folin_T.Cholesterol_mg_dL_$`Pr(>F)`, 
 anova_lm_log_Folin_Atherogenic_index$`Pr(>F)`,
 anova_lm_log_Folin_Creatinine_mg_dL_$`Pr(>F)`,
 anova_lm_log_Folin_hsCRP_mg_L$`Pr(>F)`,
 anova_lm_log_Folin_Glucose_mg_dL$`Pr(>F)`,
 anova_lm_log_Folin_HbA1c$`Pr(>F)`,
 anova_lm_log_Folin_Leptina_ng_mL$`Pr(>F)`,
 anova_lm_log_Folin_Adiponectin_ug_ml$`Pr(>F)`,
 anova_lm_log_Folin_Lep_Adip$`Pr(>F)`,
 anova_lm_log_Folin_TGs_mg_dL$`Pr(>F)`,
 anova_lm_log_Folin_Insulin_uU_mL$`Pr(>F)`,
 anova_lm_log_Folin_Glucose_mmol_L$`Pr(>F)`,
 anova_lm_log_Folin_beta_cell_function$`Pr(>F)`,
 anova_lm_log_Folin_Insulin_Sensibility$`Pr(>F)`,
 anova_lm_log_Folin_HOMA_IR$`Pr(>F)`,
 anova_lm_log_Folin_Weight$`Pr(>F)`,
 anova_lm_log_Folin_BMI$`Pr(>F)`,
 anova_lm_log_Folin_Perc_fat$`Pr(>F)`,
 anova_lm_log_Folin_Waist_Circumference$`Pr(>F)`,
 anova_lm_log_Folin_Systolic_BP$`Pr(>F)`,
 anova_lm_log_Folin_Diastolic_BP$`Pr(>F)`,
 anova_lm_log_Folin_Calories$`Pr(>F)`,
 anova_lm_log_Folin_FIBER$`Pr(>F)`,
 anova_lm_log_Folin_Ca$`Pr(>F)`,
 anova_lm_log_Folin_P$`Pr(>F)`,
 anova_lm_log_Folin_Fe_total$`Pr(>F)`,
 anova_lm_log_Folin_Na$`Pr(>F)`,
 anova_lm_log_Folin_K$`Pr(>F)`,
 anova_lm_log_Folin_Mg$`Pr(>F)`,
 anova_lm_log_Folin_Zn$`Pr(>F)`,
 anova_lm_log_Folin_Cu$`Pr(>F)`,
 anova_lm_log_Folin_Mn$`Pr(>F)`,
 anova_lm_log_Folin_Vit_A_ER$`Pr(>F)`,
 anova_lm_log_Folin_B1$`Pr(>F)`,
 anova_lm_log_Folin_B2$`Pr(>F)`,
 anova_lm_log_Folin_B3$`Pr(>F)`,
 anova_lm_log_Folin_Ac_pantoenico$`Pr(>F)`,  
 anova_lm_log_Folin_B6$`Pr(>F)`,
 anova_lm_log_Folin_Folic$`Pr(>F)`,
 anova_lm_log_Folin_B12$`Pr(>F)`,
 anova_lm_log_Folin_Vit_C$`Pr(>F)`,
 anova_lm_log_Folin_Acetic_A_Feces$`Pr(>F)`,
 anova_lm_log_Folin_Propionic_A_Feces$`Pr(>F)`,
 anova_lm_log_Folin_Butyric_A_Feces$`Pr(>F)`, 
 anova_lm_log_Folin_Isobutyric_A_Feces$`Pr(>F)`, 
 anova_lm_log_Folin_Acetic_A_Plasma$`Pr(>F)`, 
 anova_lm_log_Folin_Propionic_A_Plasma$`Pr(>F)`, 
 anova_lm_log_Folin_Isobutyric_A_Plasmas$`Pr(>F)`, 
 anova_lm_log_Folin_Butyric_A_Plasma$`Pr(>F)`, 
 anova_lm_log_Folin_Valeric_A_Plasma$`Pr(>F)`, 
 anova_lm_log_Folin_DII_score$`Pr(>F)`,
 anova_lm_log_Folin_E_DII_score$`Pr(>F)`,
 anova_lm_log_Folin_RFM$`Pr(>F)`,
 anova_lm_log_Folin_Prevotella$`Pr(>F)`,
 anova_lm_log_Folin_Lachnospiraceae$`Pr(>F)`,
 anova_lm_log_Folin_Pathogen$`Pr(>F)`,
 anova_lm_log_Folin_Akkermansia.Bacteroidales$`Pr(>F)`, 
 anova_lm_log_Folin_Ruminococcaceae$`Pr(>F)`,
 anova_lm_log_Folin_TPC_Folin$`Pr(>F)`)
 
 
 ##Export and add manually the variables names in the list 
 write.table(MLR_Pvalue_FOLIN_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/1.MLR_Pvalue_FOLIN_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
 
 
 
 
 #lm (Adjust) FOLIN/ADJUST kcal (NO SE VA A INCLUIR EN EL PAPER POR EL MOMENTO- FALTARIA AGREGAR E_DII)
 
 
 lm_log_Folin_Kcal_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Calories <- lm(Log_Calories~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                           DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_P <- lm(Log_P~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                        DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Na <- lm(Log_Na~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_K <- lm(Log_K~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                        DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_DII_score <- lm(Log_DII_score~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Folin_Kcal_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_TPC_Folin_KCAL_Adj <- lm(Log_TPC_Folin_KCAL_Adj~DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 
 
 
 #ANOVA-FOLIN
 
 
 anova_lm_log_Folin_Kcal_Age <- Anova(lm_log_Folin_Kcal_Age)
 anova_lm_log_Folin_Kcal_ApoB_mg_dL <- Anova(lm_log_Folin_Kcal_ApoB_mg_dL)
 anova_lm_log_Folin_Kcal_HDL_mg_dL <- Anova(lm_log_Folin_Kcal_HDL_mg_dL)
 anova_lm_log_Folin_Kcal_VLDL_mg_dL <- Anova(lm_log_Folin_Kcal_VLDL_mg_dL)
 anova_lm_log_Folin_Kcal_LDL_mg_dL <- Anova(lm_log_Folin_Kcal_LDL_mg_dL)
 anova_lm_log_Folin_Kcal_T.Cholesterol_mg_dL_ <-Anova(lm_log_Folin_Kcal_T.Cholesterol_mg_dL_) 
 anova_lm_log_Folin_Kcal_Atherogenic_index <- Anova(lm_log_Folin_Kcal_Atherogenic_index)
 anova_lm_log_Folin_Kcal_Creatinine_mg_dL_ <- Anova(lm_log_Folin_Kcal_Creatinine_mg_dL_)
 anova_lm_log_Folin_Kcal_hsCRP_mg_L <- Anova(lm_log_Folin_Kcal_hsCRP_mg_L)
 anova_lm_log_Folin_Kcal_Glucose_mg_dL <- Anova(lm_log_Folin_Kcal_Glucose_mg_dL)
 anova_lm_log_Folin_Kcal_HbA1c <- Anova(lm_log_Folin_Kcal_HbA1c)
 anova_lm_log_Folin_Kcal_Leptina_ng_mL <- Anova(lm_log_Folin_Kcal_Leptina_ng_mL)
 anova_lm_log_Folin_Kcal_Adiponectin_ug_ml <- Anova(lm_log_Folin_Kcal_Adiponectin_ug_ml)
 anova_lm_log_Folin_Kcal_Lep_Adip <- Anova(lm_log_Folin_Kcal_Lep_Adip)
 anova_lm_log_Folin_Kcal_TGs_mg_dL <- Anova(lm_log_Folin_Kcal_TGs_mg_dL)
 anova_lm_log_Folin_Kcal_Insulin_uU_mL <- Anova(lm_log_Folin_Kcal_Insulin_uU_mL)
 anova_lm_log_Folin_Kcal_Glucose_mmol_L <- Anova(lm_log_Folin_Kcal_Glucose_mmol_L)
 anova_lm_log_Folin_Kcal_beta_cell_function <- Anova(lm_log_Folin_Kcal_beta_cell_function)
 anova_lm_log_Folin_Kcal_Insulin_Sensibility <- Anova(lm_log_Folin_Kcal_Insulin_Sensibility)
 anova_lm_log_Folin_Kcal_HOMA_IR <- Anova(lm_log_Folin_Kcal_HOMA_IR)
 anova_lm_log_Folin_Kcal_Weight <- Anova(lm_log_Folin_Kcal_Weight)
 anova_lm_log_Folin_Kcal_BMI <- Anova(lm_log_Folin_Kcal_BMI)
 anova_lm_log_Folin_Kcal_Perc_fat <- Anova(lm_log_Folin_Kcal_Perc_fat)
 anova_lm_log_Folin_Kcal_Waist_Circumference <- Anova(lm_log_Folin_Kcal_Waist_Circumference)
 anova_lm_log_Folin_Kcal_Systolic_BP <- Anova(lm_log_Folin_Kcal_Systolic_BP)
 anova_lm_log_Folin_Kcal_Diastolic_BP <- Anova(lm_log_Folin_Kcal_Diastolic_BP)
 anova_lm_log_Folin_Kcal_Calories <- Anova(lm_log_Folin_Kcal_Calories)
 anova_lm_log_Folin_Kcal_FIBER <- Anova(lm_log_Folin_Kcal_FIBER)
 anova_lm_log_Folin_Kcal_Ca <- Anova(lm_log_Folin_Kcal_Ca)
 anova_lm_log_Folin_Kcal_P <- Anova(lm_log_Folin_Kcal_P)
 anova_lm_log_Folin_Kcal_Fe_total <- Anova(lm_log_Folin_Kcal_Fe_total)
 anova_lm_log_Folin_Kcal_Na <- Anova(lm_log_Folin_Kcal_Na)
 anova_lm_log_Folin_Kcal_K <- Anova(lm_log_Folin_Kcal_K)
 anova_lm_log_Folin_Kcal_Mg <- Anova(lm_log_Folin_Kcal_Mg)
 anova_lm_log_Folin_Kcal_Zn <- Anova(lm_log_Folin_Kcal_Zn)
 anova_lm_log_Folin_Kcal_Cu <- Anova(lm_log_Folin_Kcal_Cu)
 anova_lm_log_Folin_Kcal_Mn <- Anova(lm_log_Folin_Kcal_Mn)
 anova_lm_log_Folin_Kcal_Vit_A_ER <- Anova(lm_log_Folin_Kcal_Vit_A_ER)
 anova_lm_log_Folin_Kcal_B1 <- Anova(lm_log_Folin_Kcal_B1)
 anova_lm_log_Folin_Kcal_B2 <- Anova(lm_log_Folin_Kcal_B2)
 anova_lm_log_Folin_Kcal_B3 <- Anova(lm_log_Folin_Kcal_B3)
 anova_lm_log_Folin_Kcal_Ac_pantoenico <- Anova(lm_log_Folin_Kcal_Ac_pantoenico) 
 anova_lm_log_Folin_Kcal_B6 <- Anova(lm_log_Folin_Kcal_B6)
 anova_lm_log_Folin_Kcal_Folic <- Anova(lm_log_Folin_Kcal_Folic)
 anova_lm_log_Folin_Kcal_B12 <- Anova(lm_log_Folin_Kcal_B12)
 anova_lm_log_Folin_Kcal_Vit_C <- Anova(lm_log_Folin_Kcal_Vit_C)
 anova_lm_log_Folin_Kcal_Acetic_A_Feces <- Anova(lm_log_Folin_Kcal_Acetic_A_Feces) 
 anova_lm_log_Folin_Kcal_Propionic_A_Feces <- Anova(lm_log_Folin_Kcal_Propionic_A_Feces)
 anova_lm_log_Folin_Kcal_Butyric_A_Feces <- Anova(lm_log_Folin_Kcal_Butyric_A_Feces)
 anova_lm_log_Folin_Kcal_Isobutyric_A_Feces <- Anova(lm_log_Folin_Kcal_Isobutyric_A_Feces)
 anova_lm_log_Folin_Kcal_Acetic_A_Plasma <- Anova(lm_log_Folin_Kcal_Acetic_A_Plasma)
 anova_lm_log_Folin_Kcal_Propionic_A_Plasma <- Anova(lm_log_Folin_Kcal_Propionic_A_Plasma)
 anova_lm_log_Folin_Kcal_Isobutyric_A_Plasmas <- Anova(lm_log_Folin_Kcal_Isobutyric_A_Plasmas)
 anova_lm_log_Folin_Kcal_Butyric_A_Plasma <- Anova(lm_log_Folin_Kcal_Butyric_A_Plasma)
 anova_lm_log_Folin_Kcal_Valeric_A_Plasma <- Anova(lm_log_Folin_Kcal_Valeric_A_Plasma)
 anova_lm_log_Folin_Kcal_DII_score <- Anova(lm_log_Folin_Kcal_DII_score)
 anova_lm_log_Folin_Kcal_RFM <- Anova(lm_log_Folin_Kcal_RFM)
 anova_lm_log_Folin_Kcal_Prevotella <- Anova(lm_log_Folin_Kcal_Prevotella)
 anova_lm_log_Folin_Kcal_Lachnospiraceae <- Anova(lm_log_Folin_Kcal_Lachnospiraceae)
 anova_lm_log_Folin_Kcal_Pathogen <- Anova(lm_log_Folin_Kcal_Pathogen)
 anova_lm_log_Folin_Kcal_Akkermansia.Bacteroidales <- Anova(lm_log_Folin_Kcal_Akkermansia.Bacteroidales) 
 anova_lm_log_Folin_Kcal_Ruminococcaceae <- Anova(lm_log_Folin_Kcal_Ruminococcaceae)
 anova_lm_log_TPC_Folin_KCAL_Adj <- Anova(lm_log_TPC_Folin_KCAL_Adj)
 
 
 #summary ANOVA-FOLIN adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_FOLIN_ADJUSTED_Kcal_TERTILS <- cbind (anova_lm_log_Folin_Kcal_Age$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_ApoB_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_HDL_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_VLDL_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_LDL_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Atherogenic_index$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Creatinine_mg_dL_$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_hsCRP_mg_L$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Glucose_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_HbA1c$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Leptina_ng_mL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Adiponectin_ug_ml$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Lep_Adip$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_TGs_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Insulin_uU_mL$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Glucose_mmol_L$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_beta_cell_function$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Insulin_Sensibility$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_HOMA_IR$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Weight$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_BMI$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Perc_fat$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Waist_Circumference$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Systolic_BP$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Diastolic_BP$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Calories$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_FIBER$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Ca$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_P$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Fe_total$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Na$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_K$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Mg$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Zn$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Cu$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Mn$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Vit_A_ER$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_B1$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_B2$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_B3$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Ac_pantoenico$`Pr(>F)`,  
                                    anova_lm_log_Folin_Kcal_B6$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Folic$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_B12$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Vit_C$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Acetic_A_Feces$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Propionic_A_Feces$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Butyric_A_Feces$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Isobutyric_A_Feces$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Acetic_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Propionic_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Butyric_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Valeric_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_DII_score$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_RFM$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Prevotella$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Lachnospiraceae$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Pathogen$`Pr(>F)`,
                                    anova_lm_log_Folin_Kcal_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                    anova_lm_log_Folin_Kcal_Ruminococcaceae$`Pr(>F)`,
                                    anova_lm_log_TPC_Folin_KCAL_Adj$`Pr(>F)`)
 
 
 ##Export and add manually the variables names in the list 
 write.table(MLR_Pvalue_FOLIN_ADJUSTED_Kcal_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/2.MLR_Pvalue_FOLIN_ADJUSTED_Kcal_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
                             

 
 
 
 #lm (Adjust) TPC by chromatography (NO SE VA A INCLUIR EN EL PAPER POR EL MOMENTO- FALTARIA AGREGAR E_DII)
 
 
 lm_log_Chrom_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Chr_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Chrom_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Chr_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Chr_Tertil+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Chr_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Chr_Tertil+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Chr_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Chrom_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Chr_Tertil+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Chr_Tertil+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Chr_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$TPC_Chr_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Chr_Tertil+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Chrom_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Chr_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Chr_Tertil+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Chr_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Chr_Tertil+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Chr_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Chr_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Chrom_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Chr_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Chr_Tertil+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Chrom_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Chr_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Chrom_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Chr_Tertil+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Chrom_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Chr_Tertil+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$TPC_Chr_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Chr_Tertil+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Chr_Tertil+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Chrom_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Chr_Tertil+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Chrom_Calories <- lm(Log_Calories~DB_Polyphenols$TPC_Chr_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Chr_Tertil+
                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                           DB_Polyphenols$Age_range)
 lm_log_Chrom_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_P <- lm(Log_P~DB_Polyphenols$TPC_Chr_Tertil+
                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Chr_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Na <- lm(Log_Na~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_K <- lm(Log_K~DB_Polyphenols$TPC_Chr_Tertil+
                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Chr_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Chr_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Chr_Tertil+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Chr_Tertil+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Chrom_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Chr_Tertil+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Chr_Tertil+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Chrom_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$TPC_Chr_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Chrom_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$TPC_Chr_Tertil+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$TPC_Chr_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Chrom_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$TPC_Chr_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$TPC_Chr_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Chrom_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$TPC_Chr_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$TPC_Chr_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Chrom_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$TPC_Chr_Tertil+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Chrom_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$TPC_Chr_Tertil+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Chrom_DII_score <- lm(Log_DII_score~DB_Polyphenols$TPC_Chr_Tertil+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Chr_Tertil+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Chr_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Chr_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Chrom_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Chr_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Chr_Tertil+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Chrom_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Chr_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_Log_TPC_Chr <- lm(Log_TPC_Chr~DB_Polyphenols$TPC_Chr_Tertil+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 
 
 
 #ANOVA-CHROMATOGRAPHY
 
 
 anova_lm_log_Chrom_Age <- Anova(lm_log_Chrom_Age)
 anova_lm_log_Chrom_ApoB_mg_dL <- Anova(lm_log_Chrom_ApoB_mg_dL)
 anova_lm_log_Chrom_HDL_mg_dL <- Anova(lm_log_Chrom_HDL_mg_dL)
 anova_lm_log_Chrom_VLDL_mg_dL <- Anova(lm_log_Chrom_VLDL_mg_dL)
 anova_lm_log_Chrom_LDL_mg_dL <- Anova(lm_log_Chrom_LDL_mg_dL)
 anova_lm_log_Chrom_T.Cholesterol_mg_dL_ <-Anova(lm_log_Chrom_T.Cholesterol_mg_dL_) 
 anova_lm_log_Chrom_Atherogenic_index <- Anova(lm_log_Chrom_Atherogenic_index)
 anova_lm_log_Chrom_Creatinine_mg_dL_ <- Anova(lm_log_Chrom_Creatinine_mg_dL_)
 anova_lm_log_Chrom_hsCRP_mg_L <- Anova(lm_log_Chrom_hsCRP_mg_L)
 anova_lm_log_Chrom_Glucose_mg_dL <- Anova(lm_log_Chrom_Glucose_mg_dL)
 anova_lm_log_Chrom_HbA1c <- Anova(lm_log_Chrom_HbA1c)
 anova_lm_log_Chrom_Leptina_ng_mL <- Anova(lm_log_Chrom_Leptina_ng_mL)
 anova_lm_log_Chrom_Adiponectin_ug_ml <- Anova(lm_log_Chrom_Adiponectin_ug_ml)
 anova_lm_log_Chrom_Lep_Adip <- Anova(lm_log_Chrom_Lep_Adip)
 anova_lm_log_Chrom_TGs_mg_dL <- Anova(lm_log_Chrom_TGs_mg_dL)
 anova_lm_log_Chrom_Insulin_uU_mL <- Anova(lm_log_Chrom_Insulin_uU_mL)
 anova_lm_log_Chrom_Glucose_mmol_L <- Anova(lm_log_Chrom_Glucose_mmol_L)
 anova_lm_log_Chrom_beta_cell_function <- Anova(lm_log_Chrom_beta_cell_function)
 anova_lm_log_Chrom_Insulin_Sensibility <- Anova(lm_log_Chrom_Insulin_Sensibility)
 anova_lm_log_Chrom_HOMA_IR <- Anova(lm_log_Chrom_HOMA_IR)
 anova_lm_log_Chrom_Weight <- Anova(lm_log_Chrom_Weight)
 anova_lm_log_Chrom_BMI <- Anova(lm_log_Chrom_BMI)
 anova_lm_log_Chrom_Perc_fat <- Anova(lm_log_Chrom_Perc_fat)
 anova_lm_log_Chrom_Waist_Circumference <- Anova(lm_log_Chrom_Waist_Circumference)
 anova_lm_log_Chrom_Systolic_BP <- Anova(lm_log_Chrom_Systolic_BP)
 anova_lm_log_Chrom_Diastolic_BP <- Anova(lm_log_Chrom_Diastolic_BP)
 anova_lm_log_Chrom_Calories <- Anova(lm_log_Chrom_Calories)
 anova_lm_log_Chrom_FIBER <- Anova(lm_log_Chrom_FIBER)
 anova_lm_log_Chrom_Ca <- Anova(lm_log_Chrom_Ca)
 anova_lm_log_Chrom_P <- Anova(lm_log_Chrom_P)
 anova_lm_log_Chrom_Fe_total <- Anova(lm_log_Chrom_Fe_total)
 anova_lm_log_Chrom_Na <- Anova(lm_log_Chrom_Na)
 anova_lm_log_Chrom_K <- Anova(lm_log_Chrom_K)
 anova_lm_log_Chrom_Mg <- Anova(lm_log_Chrom_Mg)
 anova_lm_log_Chrom_Zn <- Anova(lm_log_Chrom_Zn)
 anova_lm_log_Chrom_Cu <- Anova(lm_log_Chrom_Cu)
 anova_lm_log_Chrom_Mn <- Anova(lm_log_Chrom_Mn)
 anova_lm_log_Chrom_Vit_A_ER <- Anova(lm_log_Chrom_Vit_A_ER)
 anova_lm_log_Chrom_B1 <- Anova(lm_log_Chrom_B1)
 anova_lm_log_Chrom_B2 <- Anova(lm_log_Chrom_B2)
 anova_lm_log_Chrom_B3 <- Anova(lm_log_Chrom_B3)
 anova_lm_log_Chrom_Ac_pantoenico <- Anova(lm_log_Chrom_Ac_pantoenico) 
 anova_lm_log_Chrom_B6 <- Anova(lm_log_Chrom_B6)
 anova_lm_log_Chrom_Folic <- Anova(lm_log_Chrom_Folic)
 anova_lm_log_Chrom_B12 <- Anova(lm_log_Chrom_B12)
 anova_lm_log_Chrom_Vit_C <- Anova(lm_log_Chrom_Vit_C)
 anova_lm_log_Chrom_Acetic_A_Feces <- Anova(lm_log_Chrom_Acetic_A_Feces) 
 anova_lm_log_Chrom_Propionic_A_Feces <- Anova(lm_log_Chrom_Propionic_A_Feces)
 anova_lm_log_Chrom_Butyric_A_Feces <- Anova(lm_log_Chrom_Butyric_A_Feces)
 anova_lm_log_Chrom_Isobutyric_A_Feces <- Anova(lm_log_Chrom_Isobutyric_A_Feces)
 anova_lm_log_Chrom_Acetic_A_Plasma <- Anova(lm_log_Chrom_Acetic_A_Plasma)
 anova_lm_log_Chrom_Propionic_A_Plasma <- Anova(lm_log_Chrom_Propionic_A_Plasma)
 anova_lm_log_Chrom_Isobutyric_A_Plasmas <- Anova(lm_log_Chrom_Isobutyric_A_Plasmas)
 anova_lm_log_Chrom_Butyric_A_Plasma <- Anova(lm_log_Chrom_Butyric_A_Plasma)
 anova_lm_log_Chrom_Valeric_A_Plasma <- Anova(lm_log_Chrom_Valeric_A_Plasma)
 anova_lm_log_Chrom_DII_score <- Anova(lm_log_Chrom_DII_score)
 anova_lm_log_Chrom_RFM <- Anova(lm_log_Chrom_RFM)
 anova_lm_log_Chrom_Prevotella <- Anova(lm_log_Chrom_Prevotella)
 anova_lm_log_Chrom_Lachnospiraceae <- Anova(lm_log_Chrom_Lachnospiraceae)
 anova_lm_log_Chrom_Pathogen <- Anova(lm_log_Chrom_Pathogen)
 anova_lm_log_Chrom_Akkermansia.Bacteroidales <- Anova(lm_log_Chrom_Akkermansia.Bacteroidales) 
 anova_lm_log_Chrom_Ruminococcaceae <- Anova(lm_log_Chrom_Ruminococcaceae)
 anova_lm_Log_TPC_Chr <- Anova(lm_Log_TPC_Chr)
 
 
 #summary ANOVA-Chromatography adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_Chromatography_TERTILS <- cbind (anova_lm_log_Chrom_Age$`Pr(>F)`,
                                    anova_lm_log_Chrom_ApoB_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Chrom_HDL_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Chrom_VLDL_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Chrom_LDL_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Chrom_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Atherogenic_index$`Pr(>F)`,
                                    anova_lm_log_Chrom_Creatinine_mg_dL_$`Pr(>F)`,
                                    anova_lm_log_Chrom_hsCRP_mg_L$`Pr(>F)`,
                                    anova_lm_log_Chrom_Glucose_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Chrom_HbA1c$`Pr(>F)`,
                                    anova_lm_log_Chrom_Leptina_ng_mL$`Pr(>F)`,
                                    anova_lm_log_Chrom_Adiponectin_ug_ml$`Pr(>F)`,
                                    anova_lm_log_Chrom_Lep_Adip$`Pr(>F)`,
                                    anova_lm_log_Chrom_TGs_mg_dL$`Pr(>F)`,
                                    anova_lm_log_Chrom_Insulin_uU_mL$`Pr(>F)`,
                                    anova_lm_log_Chrom_Glucose_mmol_L$`Pr(>F)`,
                                    anova_lm_log_Chrom_beta_cell_function$`Pr(>F)`,
                                    anova_lm_log_Chrom_Insulin_Sensibility$`Pr(>F)`,
                                    anova_lm_log_Chrom_HOMA_IR$`Pr(>F)`,
                                    anova_lm_log_Chrom_Weight$`Pr(>F)`,
                                    anova_lm_log_Chrom_BMI$`Pr(>F)`,
                                    anova_lm_log_Chrom_Perc_fat$`Pr(>F)`,
                                    anova_lm_log_Chrom_Waist_Circumference$`Pr(>F)`,
                                    anova_lm_log_Chrom_Systolic_BP$`Pr(>F)`,
                                    anova_lm_log_Chrom_Diastolic_BP$`Pr(>F)`,
                                    anova_lm_log_Chrom_Calories$`Pr(>F)`,
                                    anova_lm_log_Chrom_FIBER$`Pr(>F)`,
                                    anova_lm_log_Chrom_Ca$`Pr(>F)`,
                                    anova_lm_log_Chrom_P$`Pr(>F)`,
                                    anova_lm_log_Chrom_Fe_total$`Pr(>F)`,
                                    anova_lm_log_Chrom_Na$`Pr(>F)`,
                                    anova_lm_log_Chrom_K$`Pr(>F)`,
                                    anova_lm_log_Chrom_Mg$`Pr(>F)`,
                                    anova_lm_log_Chrom_Zn$`Pr(>F)`,
                                    anova_lm_log_Chrom_Cu$`Pr(>F)`,
                                    anova_lm_log_Chrom_Mn$`Pr(>F)`,
                                    anova_lm_log_Chrom_Vit_A_ER$`Pr(>F)`,
                                    anova_lm_log_Chrom_B1$`Pr(>F)`,
                                    anova_lm_log_Chrom_B2$`Pr(>F)`,
                                    anova_lm_log_Chrom_B3$`Pr(>F)`,
                                    anova_lm_log_Chrom_Ac_pantoenico$`Pr(>F)`,  
                                    anova_lm_log_Chrom_B6$`Pr(>F)`,
                                    anova_lm_log_Chrom_Folic$`Pr(>F)`,
                                    anova_lm_log_Chrom_B12$`Pr(>F)`,
                                    anova_lm_log_Chrom_Vit_C$`Pr(>F)`,
                                    anova_lm_log_Chrom_Acetic_A_Feces$`Pr(>F)`,
                                    anova_lm_log_Chrom_Propionic_A_Feces$`Pr(>F)`,
                                    anova_lm_log_Chrom_Butyric_A_Feces$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Isobutyric_A_Feces$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Acetic_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Propionic_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Butyric_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Valeric_A_Plasma$`Pr(>F)`, 
                                    anova_lm_log_Chrom_DII_score$`Pr(>F)`,
                                    anova_lm_log_Chrom_RFM$`Pr(>F)`,
                                    anova_lm_log_Chrom_Prevotella$`Pr(>F)`,
                                    anova_lm_log_Chrom_Lachnospiraceae$`Pr(>F)`,
                                    anova_lm_log_Chrom_Pathogen$`Pr(>F)`,
                                    anova_lm_log_Chrom_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                    anova_lm_log_Chrom_Ruminococcaceae$`Pr(>F)`,
                                    anova_lm_Log_TPC_Chr$`Pr(>F)`)
 
 
 ##Export and add manually the variables names in the list 
 write.table(MLR_Pvalue_Chromatography_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/3.MLR_Pvalue_Chromatography_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
 
 
 
 #lm (Adjust) TPC by chromatography/Adjust by KCal (NO SE VA A INCLUIR EN EL PAPER POR EL MOMENTO- FALTARIA AGREGAR E_DII)
 
 
 lm_log_Chrom_Kcal_Age <-     lm(Log_Age~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Weight <- lm(Log_Weight~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_BMI <- lm(Log_BMI~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Calories <- lm(Log_Calories~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_FIBER <-lm(Log_FIBER~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                           DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Ca <- lm(Log_Ca~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_P <- lm(Log_P~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Na <- lm(Log_Na~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_K <- lm(Log_K~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Mg <- lm(Log_Mg~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Zn <- lm(Log_Zn~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Cu <- lm(Log_Cu~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Mn <- lm(Log_Mn~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_B1 <- lm(Log_B1~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_B2 <- lm(Log_B2~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_B3 <- lm(Log_B3~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_B6 <- lm(Log_B6~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Folic <- lm(Log_Folic~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_B12 <- lm(Log_B12~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                            DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_DII_score <- lm(Log_DII_score~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_RFM <- lm(Log_RFM~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                          DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Chrom_Kcal_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_Log_TPC_Chr_KCAL_Adj <- lm(Log_TPC_Chr_KCAL_Adj~DB_Polyphenols$TPC_Chr_KCAL_Adj+
                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                        DB_Polyphenols$Age_range)
 
 
 
 #ANOVA-CHROMATOGRAPHY/Kcal
 
 
 anova_lm_log_Chrom_Kcal_Age <- Anova(lm_log_Chrom_Kcal_Age)
 anova_lm_log_Chrom_Kcal_ApoB_mg_dL <- Anova(lm_log_Chrom_Kcal_ApoB_mg_dL)
 anova_lm_log_Chrom_Kcal_HDL_mg_dL <- Anova(lm_log_Chrom_Kcal_HDL_mg_dL)
 anova_lm_log_Chrom_Kcal_VLDL_mg_dL <- Anova(lm_log_Chrom_Kcal_VLDL_mg_dL)
 anova_lm_log_Chrom_Kcal_LDL_mg_dL <- Anova(lm_log_Chrom_Kcal_LDL_mg_dL)
 anova_lm_log_Chrom_Kcal_T.Cholesterol_mg_dL_ <-Anova(lm_log_Chrom_Kcal_T.Cholesterol_mg_dL_) 
 anova_lm_log_Chrom_Kcal_Atherogenic_index <- Anova(lm_log_Chrom_Kcal_Atherogenic_index)
 anova_lm_log_Chrom_Kcal_Creatinine_mg_dL_ <- Anova(lm_log_Chrom_Kcal_Creatinine_mg_dL_)
 anova_lm_log_Chrom_Kcal_hsCRP_mg_L <- Anova(lm_log_Chrom_Kcal_hsCRP_mg_L)
 anova_lm_log_Chrom_Kcal_Glucose_mg_dL <- Anova(lm_log_Chrom_Kcal_Glucose_mg_dL)
 anova_lm_log_Chrom_Kcal_HbA1c <- Anova(lm_log_Chrom_Kcal_HbA1c)
 anova_lm_log_Chrom_Kcal_Leptina_ng_mL <- Anova(lm_log_Chrom_Kcal_Leptina_ng_mL)
 anova_lm_log_Chrom_Kcal_Adiponectin_ug_ml <- Anova(lm_log_Chrom_Kcal_Adiponectin_ug_ml)
 anova_lm_log_Chrom_Kcal_Lep_Adip <- Anova(lm_log_Chrom_Kcal_Lep_Adip)
 anova_lm_log_Chrom_Kcal_TGs_mg_dL <- Anova(lm_log_Chrom_Kcal_TGs_mg_dL)
 anova_lm_log_Chrom_Kcal_Insulin_uU_mL <- Anova(lm_log_Chrom_Kcal_Insulin_uU_mL)
 anova_lm_log_Chrom_Kcal_Glucose_mmol_L <- Anova(lm_log_Chrom_Kcal_Glucose_mmol_L)
 anova_lm_log_Chrom_Kcal_beta_cell_function <- Anova(lm_log_Chrom_Kcal_beta_cell_function)
 anova_lm_log_Chrom_Kcal_Insulin_Sensibility <- Anova(lm_log_Chrom_Kcal_Insulin_Sensibility)
 anova_lm_log_Chrom_Kcal_HOMA_IR <- Anova(lm_log_Chrom_Kcal_HOMA_IR)
 anova_lm_log_Chrom_Kcal_Weight <- Anova(lm_log_Chrom_Kcal_Weight)
 anova_lm_log_Chrom_Kcal_BMI <- Anova(lm_log_Chrom_Kcal_BMI)
 anova_lm_log_Chrom_Kcal_Perc_fat <- Anova(lm_log_Chrom_Kcal_Perc_fat)
 anova_lm_log_Chrom_Kcal_Waist_Circumference <- Anova(lm_log_Chrom_Kcal_Waist_Circumference)
 anova_lm_log_Chrom_Kcal_Systolic_BP <- Anova(lm_log_Chrom_Kcal_Systolic_BP)
 anova_lm_log_Chrom_Kcal_Diastolic_BP <- Anova(lm_log_Chrom_Kcal_Diastolic_BP)
 anova_lm_log_Chrom_Kcal_Calories <- Anova(lm_log_Chrom_Kcal_Calories)
 anova_lm_log_Chrom_Kcal_FIBER <- Anova(lm_log_Chrom_Kcal_FIBER)
 anova_lm_log_Chrom_Kcal_Ca <- Anova(lm_log_Chrom_Kcal_Ca)
 anova_lm_log_Chrom_Kcal_P <- Anova(lm_log_Chrom_Kcal_P)
 anova_lm_log_Chrom_Kcal_Fe_total <- Anova(lm_log_Chrom_Kcal_Fe_total)
 anova_lm_log_Chrom_Kcal_Na <- Anova(lm_log_Chrom_Kcal_Na)
 anova_lm_log_Chrom_Kcal_K <- Anova(lm_log_Chrom_Kcal_K)
 anova_lm_log_Chrom_Kcal_Mg <- Anova(lm_log_Chrom_Kcal_Mg)
 anova_lm_log_Chrom_Kcal_Zn <- Anova(lm_log_Chrom_Kcal_Zn)
 anova_lm_log_Chrom_Kcal_Cu <- Anova(lm_log_Chrom_Kcal_Cu)
 anova_lm_log_Chrom_Kcal_Mn <- Anova(lm_log_Chrom_Kcal_Mn)
 anova_lm_log_Chrom_Kcal_Vit_A_ER <- Anova(lm_log_Chrom_Kcal_Vit_A_ER)
 anova_lm_log_Chrom_Kcal_B1 <- Anova(lm_log_Chrom_Kcal_B1)
 anova_lm_log_Chrom_Kcal_B2 <- Anova(lm_log_Chrom_Kcal_B2)
 anova_lm_log_Chrom_Kcal_B3 <- Anova(lm_log_Chrom_Kcal_B3)
 anova_lm_log_Chrom_Kcal_Ac_pantoenico <- Anova(lm_log_Chrom_Kcal_Ac_pantoenico) 
 anova_lm_log_Chrom_Kcal_B6 <- Anova(lm_log_Chrom_Kcal_B6)
 anova_lm_log_Chrom_Kcal_Folic <- Anova(lm_log_Chrom_Kcal_Folic)
 anova_lm_log_Chrom_Kcal_B12 <- Anova(lm_log_Chrom_Kcal_B12)
 anova_lm_log_Chrom_Kcal_Vit_C <- Anova(lm_log_Chrom_Kcal_Vit_C)
 anova_lm_log_Chrom_Kcal_Acetic_A_Feces <- Anova(lm_log_Chrom_Kcal_Acetic_A_Feces) 
 anova_lm_log_Chrom_Kcal_Propionic_A_Feces <- Anova(lm_log_Chrom_Kcal_Propionic_A_Feces)
 anova_lm_log_Chrom_Kcal_Butyric_A_Feces <- Anova(lm_log_Chrom_Kcal_Butyric_A_Feces)
 anova_lm_log_Chrom_Kcal_Isobutyric_A_Feces <- Anova(lm_log_Chrom_Kcal_Isobutyric_A_Feces)
 anova_lm_log_Chrom_Kcal_Acetic_A_Plasma <- Anova(lm_log_Chrom_Kcal_Acetic_A_Plasma)
 anova_lm_log_Chrom_Kcal_Propionic_A_Plasma <- Anova(lm_log_Chrom_Kcal_Propionic_A_Plasma)
 anova_lm_log_Chrom_Kcal_Isobutyric_A_Plasmas <- Anova(lm_log_Chrom_Kcal_Isobutyric_A_Plasmas)
 anova_lm_log_Chrom_Kcal_Butyric_A_Plasma <- Anova(lm_log_Chrom_Kcal_Butyric_A_Plasma)
 anova_lm_log_Chrom_Kcal_Valeric_A_Plasma <- Anova(lm_log_Chrom_Kcal_Valeric_A_Plasma)
 anova_lm_log_Chrom_Kcal_DII_score <- Anova(lm_log_Chrom_Kcal_DII_score)
 anova_lm_log_Chrom_Kcal_RFM <- Anova(lm_log_Chrom_Kcal_RFM)
 anova_lm_log_Chrom_Kcal_Prevotella <- Anova(lm_log_Chrom_Kcal_Prevotella)
 anova_lm_log_Chrom_Kcal_Lachnospiraceae <- Anova(lm_log_Chrom_Kcal_Lachnospiraceae)
 anova_lm_log_Chrom_Kcal_Pathogen <- Anova(lm_log_Chrom_Kcal_Pathogen)
 anova_lm_log_Chrom_Kcal_Akkermansia.Bacteroidales <- Anova(lm_log_Chrom_Kcal_Akkermansia.Bacteroidales) 
 anova_lm_log_Chrom_Kcal_Ruminococcaceae <- Anova(lm_log_Chrom_Kcal_Ruminococcaceae)
 anova_lm_Log_TPC_Chr_KCAL_Adj <- Anova(lm_Log_TPC_Chr_KCAL_Adj)
 
 
 #summary ANOVA-Chormatografy KCal adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_Chromatography_Kcal_TERTILS <- cbind (anova_lm_log_Chrom_Kcal_Age$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_ApoB_mg_dL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_HDL_mg_dL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_VLDL_mg_dL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_LDL_mg_dL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Atherogenic_index$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Creatinine_mg_dL_$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_hsCRP_mg_L$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Glucose_mg_dL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_HbA1c$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Leptina_ng_mL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Adiponectin_ug_ml$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Lep_Adip$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_TGs_mg_dL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Insulin_uU_mL$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Glucose_mmol_L$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_beta_cell_function$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Insulin_Sensibility$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_HOMA_IR$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Weight$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_BMI$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Perc_fat$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Waist_Circumference$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Systolic_BP$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Diastolic_BP$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Calories$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_FIBER$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Ca$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_P$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Fe_total$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Na$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_K$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Mg$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Zn$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Cu$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Mn$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Vit_A_ER$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_B1$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_B2$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_B3$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Ac_pantoenico$`Pr(>F)`,  
                                             anova_lm_log_Chrom_Kcal_B6$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Folic$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_B12$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Vit_C$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Acetic_A_Feces$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Propionic_A_Feces$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Butyric_A_Feces$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Isobutyric_A_Feces$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Acetic_A_Plasma$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Propionic_A_Plasma$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Butyric_A_Plasma$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Valeric_A_Plasma$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_DII_score$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_RFM$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Prevotella$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Lachnospiraceae$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Pathogen$`Pr(>F)`,
                                             anova_lm_log_Chrom_Kcal_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                             anova_lm_log_Chrom_Kcal_Ruminococcaceae$`Pr(>F)`,
                                             anova_lm_Log_TPC_Chr_KCAL_Adj$`Pr(>F)`)
 
 
 ##Export and add manually the variables names in the list 
 write.table(MLR_Pvalue_Chromatography_Kcal_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/4.MLR_Pvalue_Chromatography_Kcal_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
 
 
 
 #lm (Adjust) Flavonoids/Adjust  (NO SE VA A INCLUIR EN EL PAPER POR EL MOMENTO- FALTARIA AGREGAR E_DII)
 
 
 lm_log_Flavonoids_Age <-     lm(Log_Age~DB_Polyphenols$T.Flavonoids_Tertil+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Flavonoids_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$T.Flavonoids_Tertil+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$T.Flavonoids_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$T.Flavonoids_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$T.Flavonoids_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$T.Flavonoids_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$T.Flavonoids_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$T.Flavonoids_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$T.Flavonoids_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$T.Flavonoids_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$T.Flavonoids_Tertil+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Flavonoids_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$T.Flavonoids_Tertil+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$T.Flavonoids_Tertil+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$T.Flavonoids_Tertil+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Weight <- lm(Log_Weight~DB_Polyphenols$T.Flavonoids_Tertil+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Flavonoids_BMI <- lm(Log_BMI~DB_Polyphenols$T.Flavonoids_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$T.Flavonoids_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$T.Flavonoids_Tertil+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$T.Flavonoids_Tertil+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$T.Flavonoids_Tertil+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Calories <- lm(Log_Calories~DB_Polyphenols$T.Flavonoids_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_FIBER <-lm(Log_FIBER~DB_Polyphenols$T.Flavonoids_Tertil+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Ca <- lm(Log_Ca~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_P <- lm(Log_P~DB_Polyphenols$T.Flavonoids_Tertil+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$T.Flavonoids_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Na <- lm(Log_Na~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_K <- lm(Log_K~DB_Polyphenols$T.Flavonoids_Tertil+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Mg <- lm(Log_Mg~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Zn <- lm(Log_Zn~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Cu <- lm(Log_Cu~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Mn <- lm(Log_Mn~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$T.Flavonoids_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_B1 <- lm(Log_B1~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_B2 <- lm(Log_B2~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_B3 <- lm(Log_B3~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$T.Flavonoids_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_B6 <- lm(Log_B6~DB_Polyphenols$T.Flavonoids_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Folic <- lm(Log_Folic~DB_Polyphenols$T.Flavonoids_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Flavonoids_B12 <- lm(Log_B12~DB_Polyphenols$T.Flavonoids_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$T.Flavonoids_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$T.Flavonoids_Tertil+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil+
                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                            DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil+
                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                            DB_Polyphenols$Age_range)
 lm_log_Flavonoids_DII_score <- lm(Log_DII_score~DB_Polyphenols$T.Flavonoids_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_RFM <- lm(Log_RFM~DB_Polyphenols$T.Flavonoids_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$T.Flavonoids_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$T.Flavonoids_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$T.Flavonoids_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$T.Flavonoids_Tertil+
                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$T.Flavonoids_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_Log_T.Flavonoids <- lm(Log_T.Flavonoids~DB_Polyphenols$T.Flavonoids_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 
 
 
 #ANOVA-CHROMATOGRAPHY/Kcal
 
 
 anova_lm_log_Flavonoids_Age <- Anova(lm_log_Flavonoids_Age)
 anova_lm_log_Flavonoids_ApoB_mg_dL <- Anova(lm_log_Flavonoids_ApoB_mg_dL)
 anova_lm_log_Flavonoids_HDL_mg_dL <- Anova(lm_log_Flavonoids_HDL_mg_dL)
 anova_lm_log_Flavonoids_VLDL_mg_dL <- Anova(lm_log_Flavonoids_VLDL_mg_dL)
 anova_lm_log_Flavonoids_LDL_mg_dL <- Anova(lm_log_Flavonoids_LDL_mg_dL)
 anova_lm_log_Flavonoids_T.Cholesterol_mg_dL_ <-Anova(lm_log_Flavonoids_T.Cholesterol_mg_dL_) 
 anova_lm_log_Flavonoids_Atherogenic_index <- Anova(lm_log_Flavonoids_Atherogenic_index)
 anova_lm_log_Flavonoids_Creatinine_mg_dL_ <- Anova(lm_log_Flavonoids_Creatinine_mg_dL_)
 anova_lm_log_Flavonoids_hsCRP_mg_L <- Anova(lm_log_Flavonoids_hsCRP_mg_L)
 anova_lm_log_Flavonoids_Glucose_mg_dL <- Anova(lm_log_Flavonoids_Glucose_mg_dL)
 anova_lm_log_Flavonoids_HbA1c <- Anova(lm_log_Flavonoids_HbA1c)
 anova_lm_log_Flavonoids_Leptina_ng_mL <- Anova(lm_log_Flavonoids_Leptina_ng_mL)
 anova_lm_log_Flavonoids_Adiponectin_ug_ml <- Anova(lm_log_Flavonoids_Adiponectin_ug_ml)
 anova_lm_log_Flavonoids_Lep_Adip <- Anova(lm_log_Flavonoids_Lep_Adip)
 anova_lm_log_Flavonoids_TGs_mg_dL <- Anova(lm_log_Flavonoids_TGs_mg_dL)
 anova_lm_log_Flavonoids_Insulin_uU_mL <- Anova(lm_log_Flavonoids_Insulin_uU_mL)
 anova_lm_log_Flavonoids_Glucose_mmol_L <- Anova(lm_log_Flavonoids_Glucose_mmol_L)
 anova_lm_log_Flavonoids_beta_cell_function <- Anova(lm_log_Flavonoids_beta_cell_function)
 anova_lm_log_Flavonoids_Insulin_Sensibility <- Anova(lm_log_Flavonoids_Insulin_Sensibility)
 anova_lm_log_Flavonoids_HOMA_IR <- Anova(lm_log_Flavonoids_HOMA_IR)
 anova_lm_log_Flavonoids_Weight <- Anova(lm_log_Flavonoids_Weight)
 anova_lm_log_Flavonoids_BMI <- Anova(lm_log_Flavonoids_BMI)
 anova_lm_log_Flavonoids_Perc_fat <- Anova(lm_log_Flavonoids_Perc_fat)
 anova_lm_log_Flavonoids_Waist_Circumference <- Anova(lm_log_Flavonoids_Waist_Circumference)
 anova_lm_log_Flavonoids_Systolic_BP <- Anova(lm_log_Flavonoids_Systolic_BP)
 anova_lm_log_Flavonoids_Diastolic_BP <- Anova(lm_log_Flavonoids_Diastolic_BP)
 anova_lm_log_Flavonoids_Calories <- Anova(lm_log_Flavonoids_Calories)
 anova_lm_log_Flavonoids_FIBER <- Anova(lm_log_Flavonoids_FIBER)
 anova_lm_log_Flavonoids_Ca <- Anova(lm_log_Flavonoids_Ca)
 anova_lm_log_Flavonoids_P <- Anova(lm_log_Flavonoids_P)
 anova_lm_log_Flavonoids_Fe_total <- Anova(lm_log_Flavonoids_Fe_total)
 anova_lm_log_Flavonoids_Na <- Anova(lm_log_Flavonoids_Na)
 anova_lm_log_Flavonoids_K <- Anova(lm_log_Flavonoids_K)
 anova_lm_log_Flavonoids_Mg <- Anova(lm_log_Flavonoids_Mg)
 anova_lm_log_Flavonoids_Zn <- Anova(lm_log_Flavonoids_Zn)
 anova_lm_log_Flavonoids_Cu <- Anova(lm_log_Flavonoids_Cu)
 anova_lm_log_Flavonoids_Mn <- Anova(lm_log_Flavonoids_Mn)
 anova_lm_log_Flavonoids_Vit_A_ER <- Anova(lm_log_Flavonoids_Vit_A_ER)
 anova_lm_log_Flavonoids_B1 <- Anova(lm_log_Flavonoids_B1)
 anova_lm_log_Flavonoids_B2 <- Anova(lm_log_Flavonoids_B2)
 anova_lm_log_Flavonoids_B3 <- Anova(lm_log_Flavonoids_B3)
 anova_lm_log_Flavonoids_Ac_pantoenico <- Anova(lm_log_Flavonoids_Ac_pantoenico) 
 anova_lm_log_Flavonoids_B6 <- Anova(lm_log_Flavonoids_B6)
 anova_lm_log_Flavonoids_Folic <- Anova(lm_log_Flavonoids_Folic)
 anova_lm_log_Flavonoids_B12 <- Anova(lm_log_Flavonoids_B12)
 anova_lm_log_Flavonoids_Vit_C <- Anova(lm_log_Flavonoids_Vit_C)
 anova_lm_log_Flavonoids_Acetic_A_Feces <- Anova(lm_log_Flavonoids_Acetic_A_Feces) 
 anova_lm_log_Flavonoids_Propionic_A_Feces <- Anova(lm_log_Flavonoids_Propionic_A_Feces)
 anova_lm_log_Flavonoids_Butyric_A_Feces <- Anova(lm_log_Flavonoids_Butyric_A_Feces)
 anova_lm_log_Flavonoids_Isobutyric_A_Feces <- Anova(lm_log_Flavonoids_Isobutyric_A_Feces)
 anova_lm_log_Flavonoids_Acetic_A_Plasma <- Anova(lm_log_Flavonoids_Acetic_A_Plasma)
 anova_lm_log_Flavonoids_Propionic_A_Plasma <- Anova(lm_log_Flavonoids_Propionic_A_Plasma)
 anova_lm_log_Flavonoids_Isobutyric_A_Plasmas <- Anova(lm_log_Flavonoids_Isobutyric_A_Plasmas)
 anova_lm_log_Flavonoids_Butyric_A_Plasma <- Anova(lm_log_Flavonoids_Butyric_A_Plasma)
 anova_lm_log_Flavonoids_Valeric_A_Plasma <- Anova(lm_log_Flavonoids_Valeric_A_Plasma)
 anova_lm_log_Flavonoids_DII_score <- Anova(lm_log_Flavonoids_DII_score)
 anova_lm_log_Flavonoids_RFM <- Anova(lm_log_Flavonoids_RFM)
 anova_lm_log_Flavonoids_Prevotella <- Anova(lm_log_Flavonoids_Prevotella)
 anova_lm_log_Flavonoids_Lachnospiraceae <- Anova(lm_log_Flavonoids_Lachnospiraceae)
 anova_lm_log_Flavonoids_Pathogen <- Anova(lm_log_Flavonoids_Pathogen)
 anova_lm_log_Flavonoids_Akkermansia.Bacteroidales <- Anova(lm_log_Flavonoids_Akkermansia.Bacteroidales) 
 anova_lm_log_Flavonoids_Ruminococcaceae <- Anova(lm_log_Flavonoids_Ruminococcaceae)
 anova_lm_Log_T.Flavonoids <- Anova(lm_Log_T.Flavonoids)
 
 
 #summary ANOVA-Chormatografy KCal adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_T.Flavonoids_TERTILS <- cbind (anova_lm_log_Flavonoids_Age$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_ApoB_mg_dL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_HDL_mg_dL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_VLDL_mg_dL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_LDL_mg_dL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Atherogenic_index$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Creatinine_mg_dL_$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_hsCRP_mg_L$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Glucose_mg_dL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_HbA1c$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Leptina_ng_mL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Adiponectin_ug_ml$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Lep_Adip$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_TGs_mg_dL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Insulin_uU_mL$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Glucose_mmol_L$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_beta_cell_function$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Insulin_Sensibility$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_HOMA_IR$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Weight$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_BMI$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Perc_fat$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Waist_Circumference$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Systolic_BP$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Diastolic_BP$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Calories$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_FIBER$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Ca$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_P$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Fe_total$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Na$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_K$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Mg$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Zn$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Cu$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Mn$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Vit_A_ER$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_B1$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_B2$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_B3$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Ac_pantoenico$`Pr(>F)`,  
                                                  anova_lm_log_Flavonoids_B6$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Folic$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_B12$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Vit_C$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Acetic_A_Feces$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Propionic_A_Feces$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Butyric_A_Feces$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Isobutyric_A_Feces$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Acetic_A_Plasma$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Propionic_A_Plasma$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Butyric_A_Plasma$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Valeric_A_Plasma$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_DII_score$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_RFM$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Prevotella$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Lachnospiraceae$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Pathogen$`Pr(>F)`,
                                                  anova_lm_log_Flavonoids_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                                  anova_lm_log_Flavonoids_Ruminococcaceae$`Pr(>F)`,
                                                  anova_lm_Log_T.Flavonoids$`Pr(>F)`)
 
 
 ##Export and add manually the variables names in the list 
 write.table(MLR_Pvalue_T.Flavonoids_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/5.MLR_Pvalue_T.Flavonoids_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
 
 
 
 
 
 #lm (Adjust) Flavonoids/Adjust BY kCAL (NO SE VA A INCLUIR EN EL PAPER POR EL MOMENTO- FALTARIA AGREGAR E_DII)
 
 
 lm_log_Flavonoids_Kcal_Age <-     lm(Log_Age~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Weight <- lm(Log_Weight~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_BMI <- lm(Log_BMI~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Calories <- lm(Log_Calories~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_FIBER <-lm(Log_FIBER~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Ca <- lm(Log_Ca~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_P <- lm(Log_P~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Na <- lm(Log_Na~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_K <- lm(Log_K~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Mg <- lm(Log_Mg~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Zn <- lm(Log_Zn~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Cu <- lm(Log_Cu~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Mn <- lm(Log_Mn~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_B1 <- lm(Log_B1~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_B2 <- lm(Log_B2~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_B3 <- lm(Log_B3~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_B6 <- lm(Log_B6~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Folic <- lm(Log_Folic~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_B12 <- lm(Log_B12~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                            DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                            DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_DII_score <- lm(Log_DII_score~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_RFM <- lm(Log_RFM~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                     DB_Polyphenols$Age_range)
 lm_log_Flavonoids_Kcal_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_Log_T.Flavonoids_KCAL <- lm(Log_T.Flavonoids_KCAL_Adj~DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 
 
 
 #ANOVA-CHROMATOGRAPHY/Kcal
 
 
 anova_lm_log_Flavonoids_Kcal_Age <- Anova(lm_log_Flavonoids_Kcal_Age)
 anova_lm_log_Flavonoids_Kcal_ApoB_mg_dL <- Anova(lm_log_Flavonoids_Kcal_ApoB_mg_dL)
 anova_lm_log_Flavonoids_Kcal_HDL_mg_dL <- Anova(lm_log_Flavonoids_Kcal_HDL_mg_dL)
 anova_lm_log_Flavonoids_Kcal_VLDL_mg_dL <- Anova(lm_log_Flavonoids_Kcal_VLDL_mg_dL)
 anova_lm_log_Flavonoids_Kcal_LDL_mg_dL <- Anova(lm_log_Flavonoids_Kcal_LDL_mg_dL)
 anova_lm_log_Flavonoids_Kcal_T.Cholesterol_mg_dL_ <-Anova(lm_log_Flavonoids_Kcal_T.Cholesterol_mg_dL_) 
 anova_lm_log_Flavonoids_Kcal_Atherogenic_index <- Anova(lm_log_Flavonoids_Kcal_Atherogenic_index)
 anova_lm_log_Flavonoids_Kcal_Creatinine_mg_dL_ <- Anova(lm_log_Flavonoids_Kcal_Creatinine_mg_dL_)
 anova_lm_log_Flavonoids_Kcal_hsCRP_mg_L <- Anova(lm_log_Flavonoids_Kcal_hsCRP_mg_L)
 anova_lm_log_Flavonoids_Kcal_Glucose_mg_dL <- Anova(lm_log_Flavonoids_Kcal_Glucose_mg_dL)
 anova_lm_log_Flavonoids_Kcal_HbA1c <- Anova(lm_log_Flavonoids_Kcal_HbA1c)
 anova_lm_log_Flavonoids_Kcal_Leptina_ng_mL <- Anova(lm_log_Flavonoids_Kcal_Leptina_ng_mL)
 anova_lm_log_Flavonoids_Kcal_Adiponectin_ug_ml <- Anova(lm_log_Flavonoids_Kcal_Adiponectin_ug_ml)
 anova_lm_log_Flavonoids_Kcal_Lep_Adip <- Anova(lm_log_Flavonoids_Kcal_Lep_Adip)
 anova_lm_log_Flavonoids_Kcal_TGs_mg_dL <- Anova(lm_log_Flavonoids_Kcal_TGs_mg_dL)
 anova_lm_log_Flavonoids_Kcal_Insulin_uU_mL <- Anova(lm_log_Flavonoids_Kcal_Insulin_uU_mL)
 anova_lm_log_Flavonoids_Kcal_Glucose_mmol_L <- Anova(lm_log_Flavonoids_Kcal_Glucose_mmol_L)
 anova_lm_log_Flavonoids_Kcal_beta_cell_function <- Anova(lm_log_Flavonoids_Kcal_beta_cell_function)
 anova_lm_log_Flavonoids_Kcal_Insulin_Sensibility <- Anova(lm_log_Flavonoids_Kcal_Insulin_Sensibility)
 anova_lm_log_Flavonoids_Kcal_HOMA_IR <- Anova(lm_log_Flavonoids_Kcal_HOMA_IR)
 anova_lm_log_Flavonoids_Kcal_Weight <- Anova(lm_log_Flavonoids_Kcal_Weight)
 anova_lm_log_Flavonoids_Kcal_BMI <- Anova(lm_log_Flavonoids_Kcal_BMI)
 anova_lm_log_Flavonoids_Kcal_Perc_fat <- Anova(lm_log_Flavonoids_Kcal_Perc_fat)
 anova_lm_log_Flavonoids_Kcal_Waist_Circumference <- Anova(lm_log_Flavonoids_Kcal_Waist_Circumference)
 anova_lm_log_Flavonoids_Kcal_Systolic_BP <- Anova(lm_log_Flavonoids_Kcal_Systolic_BP)
 anova_lm_log_Flavonoids_Kcal_Diastolic_BP <- Anova(lm_log_Flavonoids_Kcal_Diastolic_BP)
 anova_lm_log_Flavonoids_Kcal_Calories <- Anova(lm_log_Flavonoids_Kcal_Calories)
 anova_lm_log_Flavonoids_Kcal_FIBER <- Anova(lm_log_Flavonoids_Kcal_FIBER)
 anova_lm_log_Flavonoids_Kcal_Ca <- Anova(lm_log_Flavonoids_Kcal_Ca)
 anova_lm_log_Flavonoids_Kcal_P <- Anova(lm_log_Flavonoids_Kcal_P)
 anova_lm_log_Flavonoids_Kcal_Fe_total <- Anova(lm_log_Flavonoids_Kcal_Fe_total)
 anova_lm_log_Flavonoids_Kcal_Na <- Anova(lm_log_Flavonoids_Kcal_Na)
 anova_lm_log_Flavonoids_Kcal_K <- Anova(lm_log_Flavonoids_Kcal_K)
 anova_lm_log_Flavonoids_Kcal_Mg <- Anova(lm_log_Flavonoids_Kcal_Mg)
 anova_lm_log_Flavonoids_Kcal_Zn <- Anova(lm_log_Flavonoids_Kcal_Zn)
 anova_lm_log_Flavonoids_Kcal_Cu <- Anova(lm_log_Flavonoids_Kcal_Cu)
 anova_lm_log_Flavonoids_Kcal_Mn <- Anova(lm_log_Flavonoids_Kcal_Mn)
 anova_lm_log_Flavonoids_Kcal_Vit_A_ER <- Anova(lm_log_Flavonoids_Kcal_Vit_A_ER)
 anova_lm_log_Flavonoids_Kcal_B1 <- Anova(lm_log_Flavonoids_Kcal_B1)
 anova_lm_log_Flavonoids_Kcal_B2 <- Anova(lm_log_Flavonoids_Kcal_B2)
 anova_lm_log_Flavonoids_Kcal_B3 <- Anova(lm_log_Flavonoids_Kcal_B3)
 anova_lm_log_Flavonoids_Kcal_Ac_pantoenico <- Anova(lm_log_Flavonoids_Kcal_Ac_pantoenico) 
 anova_lm_log_Flavonoids_Kcal_B6 <- Anova(lm_log_Flavonoids_Kcal_B6)
 anova_lm_log_Flavonoids_Kcal_Folic <- Anova(lm_log_Flavonoids_Kcal_Folic)
 anova_lm_log_Flavonoids_Kcal_B12 <- Anova(lm_log_Flavonoids_Kcal_B12)
 anova_lm_log_Flavonoids_Kcal_Vit_C <- Anova(lm_log_Flavonoids_Kcal_Vit_C)
 anova_lm_log_Flavonoids_Kcal_Acetic_A_Feces <- Anova(lm_log_Flavonoids_Kcal_Acetic_A_Feces) 
 anova_lm_log_Flavonoids_Kcal_Propionic_A_Feces <- Anova(lm_log_Flavonoids_Kcal_Propionic_A_Feces)
 anova_lm_log_Flavonoids_Kcal_Butyric_A_Feces <- Anova(lm_log_Flavonoids_Kcal_Butyric_A_Feces)
 anova_lm_log_Flavonoids_Kcal_Isobutyric_A_Feces <- Anova(lm_log_Flavonoids_Kcal_Isobutyric_A_Feces)
 anova_lm_log_Flavonoids_Kcal_Acetic_A_Plasma <- Anova(lm_log_Flavonoids_Kcal_Acetic_A_Plasma)
 anova_lm_log_Flavonoids_Kcal_Propionic_A_Plasma <- Anova(lm_log_Flavonoids_Kcal_Propionic_A_Plasma)
 anova_lm_log_Flavonoids_Kcal_Isobutyric_A_Plasmas <- Anova(lm_log_Flavonoids_Kcal_Isobutyric_A_Plasmas)
 anova_lm_log_Flavonoids_Kcal_Butyric_A_Plasma <- Anova(lm_log_Flavonoids_Kcal_Butyric_A_Plasma)
 anova_lm_log_Flavonoids_Kcal_Valeric_A_Plasma <- Anova(lm_log_Flavonoids_Kcal_Valeric_A_Plasma)
 anova_lm_log_Flavonoids_Kcal_DII_score <- Anova(lm_log_Flavonoids_Kcal_DII_score)
 anova_lm_log_Flavonoids_Kcal_RFM <- Anova(lm_log_Flavonoids_Kcal_RFM)
 anova_lm_log_Flavonoids_Kcal_Prevotella <- Anova(lm_log_Flavonoids_Kcal_Prevotella)
 anova_lm_log_Flavonoids_Kcal_Lachnospiraceae <- Anova(lm_log_Flavonoids_Kcal_Lachnospiraceae)
 anova_lm_log_Flavonoids_Kcal_Pathogen <- Anova(lm_log_Flavonoids_Kcal_Pathogen)
 anova_lm_log_Flavonoids_Kcal_Akkermansia.Bacteroidales <- Anova(lm_log_Flavonoids_Kcal_Akkermansia.Bacteroidales) 
 anova_lm_log_Flavonoids_Kcal_Ruminococcaceae <- Anova(lm_log_Flavonoids_Kcal_Ruminococcaceae)
 anova_lm_Log_T.Flavonoids_KCAL <- Anova(lm_Log_T.Flavonoids_KCAL)
 
 
 #summary ANOVA-Chormatografy KCal adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_T.Flavonoids_KCAL_TERTILS <- cbind (anova_lm_log_Flavonoids_Kcal_Age$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_ApoB_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_HDL_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_VLDL_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_LDL_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Atherogenic_index$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Creatinine_mg_dL_$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_hsCRP_mg_L$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Glucose_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_HbA1c$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Leptina_ng_mL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Adiponectin_ug_ml$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Lep_Adip$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_TGs_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Insulin_uU_mL$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Glucose_mmol_L$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_beta_cell_function$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Insulin_Sensibility$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_HOMA_IR$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Weight$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_BMI$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Perc_fat$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Waist_Circumference$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Systolic_BP$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Diastolic_BP$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Calories$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_FIBER$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Ca$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_P$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Fe_total$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Na$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_K$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Mg$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Zn$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Cu$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Mn$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Vit_A_ER$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_B1$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_B2$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_B3$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Ac_pantoenico$`Pr(>F)`,  
                                           anova_lm_log_Flavonoids_Kcal_B6$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Folic$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_B12$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Vit_C$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Acetic_A_Feces$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Propionic_A_Feces$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Butyric_A_Feces$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Isobutyric_A_Feces$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Acetic_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Propionic_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Butyric_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Valeric_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_DII_score$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_RFM$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Prevotella$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Lachnospiraceae$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Pathogen$`Pr(>F)`,
                                           anova_lm_log_Flavonoids_Kcal_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                           anova_lm_log_Flavonoids_Kcal_Ruminococcaceae$`Pr(>F)`,
                                           anova_lm_Log_T.Flavonoids_KCAL$`Pr(>F)`)
 
 
 ##Export and add manually the variables names in the list 
 write.table(MLR_Pvalue_T.Flavonoids_KCAL_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/6.MLR_Pvalue_T.Flavonoids_KCAL_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
 
 
 
 
 #lm (Adjust) Phenlic acid (NO SE VA A INCLUIR EN EL PAPER POR EL MOMENTO- FALTARIA AGREGAR E_DII)
 
 
 lm_log_Phenolic.A_Age <-     lm(Log_Age~DB_Polyphenols$T.Phenolic.A_Tertil+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$T.Phenolic.A_Tertil+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$T.Phenolic.A_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$T.Phenolic.A_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$T.Phenolic.A_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$T.Phenolic.A_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$T.Phenolic.A_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$T.Phenolic.A_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$T.Phenolic.A_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$T.Phenolic.A_Tertil+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$T.Phenolic.A_Tertil+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$T.Phenolic.A_Tertil+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$T.Phenolic.A_Tertil+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Weight <- lm(Log_Weight~DB_Polyphenols$T.Phenolic.A_Tertil+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_BMI <- lm(Log_BMI~DB_Polyphenols$T.Phenolic.A_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$T.Phenolic.A_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$T.Phenolic.A_Tertil+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$T.Phenolic.A_Tertil+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$T.Phenolic.A_Tertil+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Calories <- lm(Log_Calories~DB_Polyphenols$T.Phenolic.A_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_FIBER <-lm(Log_FIBER~DB_Polyphenols$T.Phenolic.A_Tertil+
                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Ca <- lm(Log_Ca~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_P <- lm(Log_P~DB_Polyphenols$T.Phenolic.A_Tertil+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$T.Phenolic.A_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Na <- lm(Log_Na~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_K <- lm(Log_K~DB_Polyphenols$T.Phenolic.A_Tertil+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Mg <- lm(Log_Mg~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Zn <- lm(Log_Zn~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Cu <- lm(Log_Cu~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Mn <- lm(Log_Mn~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$T.Phenolic.A_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_B1 <- lm(Log_B1~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_B2 <- lm(Log_B2~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_B3 <- lm(Log_B3~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$T.Phenolic.A_Tertil+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_B6 <- lm(Log_B6~DB_Polyphenols$T.Phenolic.A_Tertil+
                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Folic <- lm(Log_Folic~DB_Polyphenols$T.Phenolic.A_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_B12 <- lm(Log_B12~DB_Polyphenols$T.Phenolic.A_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$T.Phenolic.A_Tertil+
                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                 DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$T.Phenolic.A_Tertil+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil+
                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                            DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil+
                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                            DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_DII_score <- lm(Log_DII_score~DB_Polyphenols$T.Phenolic.A_Tertil+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_RFM <- lm(Log_RFM~DB_Polyphenols$T.Phenolic.A_Tertil+
                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                               DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$T.Phenolic.A_Tertil+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$T.Phenolic.A_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$T.Phenolic.A_Tertil+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$T.Phenolic.A_Tertil+
                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$T.Phenolic.A_Tertil+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_Log_T.Phenolic.A <- lm(Log_T.Phenolic.A~DB_Polyphenols$T.Phenolic.A_Tertil+
                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                             DB_Polyphenols$Age_range)
 
 
 
 #ANOVA-CHROMATOGRAPHY/Kcal
 
 
 anova_lm_log_Phenolic.A_Age <- Anova(lm_log_Phenolic.A_Age)
 anova_lm_log_Phenolic.A_ApoB_mg_dL <- Anova(lm_log_Phenolic.A_ApoB_mg_dL)
 anova_lm_log_Phenolic.A_HDL_mg_dL <- Anova(lm_log_Phenolic.A_HDL_mg_dL)
 anova_lm_log_Phenolic.A_VLDL_mg_dL <- Anova(lm_log_Phenolic.A_VLDL_mg_dL)
 anova_lm_log_Phenolic.A_LDL_mg_dL <- Anova(lm_log_Phenolic.A_LDL_mg_dL)
 anova_lm_log_Phenolic.A_T.Cholesterol_mg_dL_ <-Anova(lm_log_Phenolic.A_T.Cholesterol_mg_dL_) 
 anova_lm_log_Phenolic.A_Atherogenic_index <- Anova(lm_log_Phenolic.A_Atherogenic_index)
 anova_lm_log_Phenolic.A_Creatinine_mg_dL_ <- Anova(lm_log_Phenolic.A_Creatinine_mg_dL_)
 anova_lm_log_Phenolic.A_hsCRP_mg_L <- Anova(lm_log_Phenolic.A_hsCRP_mg_L)
 anova_lm_log_Phenolic.A_Glucose_mg_dL <- Anova(lm_log_Phenolic.A_Glucose_mg_dL)
 anova_lm_log_Phenolic.A_HbA1c <- Anova(lm_log_Phenolic.A_HbA1c)
 anova_lm_log_Phenolic.A_Leptina_ng_mL <- Anova(lm_log_Phenolic.A_Leptina_ng_mL)
 anova_lm_log_Phenolic.A_Adiponectin_ug_ml <- Anova(lm_log_Phenolic.A_Adiponectin_ug_ml)
 anova_lm_log_Phenolic.A_Lep_Adip <- Anova(lm_log_Phenolic.A_Lep_Adip)
 anova_lm_log_Phenolic.A_TGs_mg_dL <- Anova(lm_log_Phenolic.A_TGs_mg_dL)
 anova_lm_log_Phenolic.A_Insulin_uU_mL <- Anova(lm_log_Phenolic.A_Insulin_uU_mL)
 anova_lm_log_Phenolic.A_Glucose_mmol_L <- Anova(lm_log_Phenolic.A_Glucose_mmol_L)
 anova_lm_log_Phenolic.A_beta_cell_function <- Anova(lm_log_Phenolic.A_beta_cell_function)
 anova_lm_log_Phenolic.A_Insulin_Sensibility <- Anova(lm_log_Phenolic.A_Insulin_Sensibility)
 anova_lm_log_Phenolic.A_HOMA_IR <- Anova(lm_log_Phenolic.A_HOMA_IR)
 anova_lm_log_Phenolic.A_Weight <- Anova(lm_log_Phenolic.A_Weight)
 anova_lm_log_Phenolic.A_BMI <- Anova(lm_log_Phenolic.A_BMI)
 anova_lm_log_Phenolic.A_Perc_fat <- Anova(lm_log_Phenolic.A_Perc_fat)
 anova_lm_log_Phenolic.A_Waist_Circumference <- Anova(lm_log_Phenolic.A_Waist_Circumference)
 anova_lm_log_Phenolic.A_Systolic_BP <- Anova(lm_log_Phenolic.A_Systolic_BP)
 anova_lm_log_Phenolic.A_Diastolic_BP <- Anova(lm_log_Phenolic.A_Diastolic_BP)
 anova_lm_log_Phenolic.A_Calories <- Anova(lm_log_Phenolic.A_Calories)
 anova_lm_log_Phenolic.A_FIBER <- Anova(lm_log_Phenolic.A_FIBER)
 anova_lm_log_Phenolic.A_Ca <- Anova(lm_log_Phenolic.A_Ca)
 anova_lm_log_Phenolic.A_P <- Anova(lm_log_Phenolic.A_P)
 anova_lm_log_Phenolic.A_Fe_total <- Anova(lm_log_Phenolic.A_Fe_total)
 anova_lm_log_Phenolic.A_Na <- Anova(lm_log_Phenolic.A_Na)
 anova_lm_log_Phenolic.A_K <- Anova(lm_log_Phenolic.A_K)
 anova_lm_log_Phenolic.A_Mg <- Anova(lm_log_Phenolic.A_Mg)
 anova_lm_log_Phenolic.A_Zn <- Anova(lm_log_Phenolic.A_Zn)
 anova_lm_log_Phenolic.A_Cu <- Anova(lm_log_Phenolic.A_Cu)
 anova_lm_log_Phenolic.A_Mn <- Anova(lm_log_Phenolic.A_Mn)
 anova_lm_log_Phenolic.A_Vit_A_ER <- Anova(lm_log_Phenolic.A_Vit_A_ER)
 anova_lm_log_Phenolic.A_B1 <- Anova(lm_log_Phenolic.A_B1)
 anova_lm_log_Phenolic.A_B2 <- Anova(lm_log_Phenolic.A_B2)
 anova_lm_log_Phenolic.A_B3 <- Anova(lm_log_Phenolic.A_B3)
 anova_lm_log_Phenolic.A_Ac_pantoenico <- Anova(lm_log_Phenolic.A_Ac_pantoenico) 
 anova_lm_log_Phenolic.A_B6 <- Anova(lm_log_Phenolic.A_B6)
 anova_lm_log_Phenolic.A_Folic <- Anova(lm_log_Phenolic.A_Folic)
 anova_lm_log_Phenolic.A_B12 <- Anova(lm_log_Phenolic.A_B12)
 anova_lm_log_Phenolic.A_Vit_C <- Anova(lm_log_Phenolic.A_Vit_C)
 anova_lm_log_Phenolic.A_Acetic_A_Feces <- Anova(lm_log_Phenolic.A_Acetic_A_Feces) 
 anova_lm_log_Phenolic.A_Propionic_A_Feces <- Anova(lm_log_Phenolic.A_Propionic_A_Feces)
 anova_lm_log_Phenolic.A_Butyric_A_Feces <- Anova(lm_log_Phenolic.A_Butyric_A_Feces)
 anova_lm_log_Phenolic.A_Isobutyric_A_Feces <- Anova(lm_log_Phenolic.A_Isobutyric_A_Feces)
 anova_lm_log_Phenolic.A_Acetic_A_Plasma <- Anova(lm_log_Phenolic.A_Acetic_A_Plasma)
 anova_lm_log_Phenolic.A_Propionic_A_Plasma <- Anova(lm_log_Phenolic.A_Propionic_A_Plasma)
 anova_lm_log_Phenolic.A_Isobutyric_A_Plasmas <- Anova(lm_log_Phenolic.A_Isobutyric_A_Plasmas)
 anova_lm_log_Phenolic.A_Butyric_A_Plasma <- Anova(lm_log_Phenolic.A_Butyric_A_Plasma)
 anova_lm_log_Phenolic.A_Valeric_A_Plasma <- Anova(lm_log_Phenolic.A_Valeric_A_Plasma)
 anova_lm_log_Phenolic.A_DII_score <- Anova(lm_log_Phenolic.A_DII_score)
 anova_lm_log_Phenolic.A_RFM <- Anova(lm_log_Phenolic.A_RFM)
 anova_lm_log_Phenolic.A_Prevotella <- Anova(lm_log_Phenolic.A_Prevotella)
 anova_lm_log_Phenolic.A_Lachnospiraceae <- Anova(lm_log_Phenolic.A_Lachnospiraceae)
 anova_lm_log_Phenolic.A_Pathogen <- Anova(lm_log_Phenolic.A_Pathogen)
 anova_lm_log_Phenolic.A_Akkermansia.Bacteroidales <- Anova(lm_log_Phenolic.A_Akkermansia.Bacteroidales) 
 anova_lm_log_Phenolic.A_Ruminococcaceae <- Anova(lm_log_Phenolic.A_Ruminococcaceae)
 anova_lm_Log_T.Phenolic.A <- Anova(lm_Log_T.Phenolic.A)
 
 
 #summary ANOVA-Chormatografy KCal adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_T.Phenolic.A_TERTILS <- cbind (anova_lm_log_Phenolic.A_Age$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_ApoB_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_HDL_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_VLDL_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_LDL_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Atherogenic_index$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Creatinine_mg_dL_$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_hsCRP_mg_L$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Glucose_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_HbA1c$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Leptina_ng_mL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Adiponectin_ug_ml$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Lep_Adip$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_TGs_mg_dL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Insulin_uU_mL$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Glucose_mmol_L$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_beta_cell_function$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Insulin_Sensibility$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_HOMA_IR$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Weight$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_BMI$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Perc_fat$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Waist_Circumference$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Systolic_BP$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Diastolic_BP$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Calories$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_FIBER$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Ca$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_P$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Fe_total$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Na$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_K$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Mg$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Zn$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Cu$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Mn$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Vit_A_ER$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_B1$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_B2$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_B3$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Ac_pantoenico$`Pr(>F)`,  
                                           anova_lm_log_Phenolic.A_B6$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Folic$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_B12$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Vit_C$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Acetic_A_Feces$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Propionic_A_Feces$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Butyric_A_Feces$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Isobutyric_A_Feces$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Acetic_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Propionic_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Butyric_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Valeric_A_Plasma$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_DII_score$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_RFM$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Prevotella$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Lachnospiraceae$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Pathogen$`Pr(>F)`,
                                           anova_lm_log_Phenolic.A_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                           anova_lm_log_Phenolic.A_Ruminococcaceae$`Pr(>F)`,
                                           anova_lm_Log_T.Phenolic.A$`Pr(>F)`)
 
 
 ##Export and add manually the variables names in the list 
 write.table(MLR_Pvalue_T.Phenolic.A_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/7.MLR_Pvalue_T.Phenolic.A_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
 
 
 
 
 
 #lm (Adjust) Phenolic.A/Adjust BY kCAL (NO SE VA A INCLUIR EN EL PAPER POR EL MOMENTO- FALTARIA AGREGAR E_DII) 
 
 
 lm_log_Phenolic.A_Kcal_Age <-     lm(Log_Age~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                  DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                  DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                  DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                        DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Weight <- lm(Log_Weight~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                       DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_BMI <- lm(Log_BMI~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                            DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                             DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Calories <- lm(Log_Calories~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_FIBER <-lm(Log_FIBER~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Ca <- lm(Log_Ca~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_P <- lm(Log_P~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Na <- lm(Log_Na~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_K <- lm(Log_K~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Mg <- lm(Log_Mg~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Zn <- lm(Log_Zn~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Cu <- lm(Log_Cu~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Mn <- lm(Log_Mn~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_B1 <- lm(Log_B1~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_B2 <- lm(Log_B2~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_B3 <- lm(Log_B3~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                              DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_B6 <- lm(Log_B6~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Folic <- lm(Log_Folic~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_B12 <- lm(Log_B12~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                      DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                               DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                  DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                     DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                 DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                 DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_DII_score <- lm(Log_DII_score~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                          DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_RFM <- lm(Log_RFM~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                    DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                           DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                         DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                          DB_Polyphenols$Age_range)
 lm_log_Phenolic.A_Kcal_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                DB_Polyphenols$Age_range)
 lm_Log_T.Phenolic.A_KCAL <- lm(Log_T.Phenolic.A_KCAL_Adj~DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj+
                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                  DB_Polyphenols$Age_range)
 
 
 
 #ANOVA-CHROMATOGRAPHY/Kcal
 
 
 anova_lm_log_Phenolic.A_Kcal_Age <- Anova(lm_log_Phenolic.A_Kcal_Age)
 anova_lm_log_Phenolic.A_Kcal_ApoB_mg_dL <- Anova(lm_log_Phenolic.A_Kcal_ApoB_mg_dL)
 anova_lm_log_Phenolic.A_Kcal_HDL_mg_dL <- Anova(lm_log_Phenolic.A_Kcal_HDL_mg_dL)
 anova_lm_log_Phenolic.A_Kcal_VLDL_mg_dL <- Anova(lm_log_Phenolic.A_Kcal_VLDL_mg_dL)
 anova_lm_log_Phenolic.A_Kcal_LDL_mg_dL <- Anova(lm_log_Phenolic.A_Kcal_LDL_mg_dL)
 anova_lm_log_Phenolic.A_Kcal_T.Cholesterol_mg_dL_ <-Anova(lm_log_Phenolic.A_Kcal_T.Cholesterol_mg_dL_) 
 anova_lm_log_Phenolic.A_Kcal_Atherogenic_index <- Anova(lm_log_Phenolic.A_Kcal_Atherogenic_index)
 anova_lm_log_Phenolic.A_Kcal_Creatinine_mg_dL_ <- Anova(lm_log_Phenolic.A_Kcal_Creatinine_mg_dL_)
 anova_lm_log_Phenolic.A_Kcal_hsCRP_mg_L <- Anova(lm_log_Phenolic.A_Kcal_hsCRP_mg_L)
 anova_lm_log_Phenolic.A_Kcal_Glucose_mg_dL <- Anova(lm_log_Phenolic.A_Kcal_Glucose_mg_dL)
 anova_lm_log_Phenolic.A_Kcal_HbA1c <- Anova(lm_log_Phenolic.A_Kcal_HbA1c)
 anova_lm_log_Phenolic.A_Kcal_Leptina_ng_mL <- Anova(lm_log_Phenolic.A_Kcal_Leptina_ng_mL)
 anova_lm_log_Phenolic.A_Kcal_Adiponectin_ug_ml <- Anova(lm_log_Phenolic.A_Kcal_Adiponectin_ug_ml)
 anova_lm_log_Phenolic.A_Kcal_Lep_Adip <- Anova(lm_log_Phenolic.A_Kcal_Lep_Adip)
 anova_lm_log_Phenolic.A_Kcal_TGs_mg_dL <- Anova(lm_log_Phenolic.A_Kcal_TGs_mg_dL)
 anova_lm_log_Phenolic.A_Kcal_Insulin_uU_mL <- Anova(lm_log_Phenolic.A_Kcal_Insulin_uU_mL)
 anova_lm_log_Phenolic.A_Kcal_Glucose_mmol_L <- Anova(lm_log_Phenolic.A_Kcal_Glucose_mmol_L)
 anova_lm_log_Phenolic.A_Kcal_beta_cell_function <- Anova(lm_log_Phenolic.A_Kcal_beta_cell_function)
 anova_lm_log_Phenolic.A_Kcal_Insulin_Sensibility <- Anova(lm_log_Phenolic.A_Kcal_Insulin_Sensibility)
 anova_lm_log_Phenolic.A_Kcal_HOMA_IR <- Anova(lm_log_Phenolic.A_Kcal_HOMA_IR)
 anova_lm_log_Phenolic.A_Kcal_Weight <- Anova(lm_log_Phenolic.A_Kcal_Weight)
 anova_lm_log_Phenolic.A_Kcal_BMI <- Anova(lm_log_Phenolic.A_Kcal_BMI)
 anova_lm_log_Phenolic.A_Kcal_Perc_fat <- Anova(lm_log_Phenolic.A_Kcal_Perc_fat)
 anova_lm_log_Phenolic.A_Kcal_Waist_Circumference <- Anova(lm_log_Phenolic.A_Kcal_Waist_Circumference)
 anova_lm_log_Phenolic.A_Kcal_Systolic_BP <- Anova(lm_log_Phenolic.A_Kcal_Systolic_BP)
 anova_lm_log_Phenolic.A_Kcal_Diastolic_BP <- Anova(lm_log_Phenolic.A_Kcal_Diastolic_BP)
 anova_lm_log_Phenolic.A_Kcal_Calories <- Anova(lm_log_Phenolic.A_Kcal_Calories)
 anova_lm_log_Phenolic.A_Kcal_FIBER <- Anova(lm_log_Phenolic.A_Kcal_FIBER)
 anova_lm_log_Phenolic.A_Kcal_Ca <- Anova(lm_log_Phenolic.A_Kcal_Ca)
 anova_lm_log_Phenolic.A_Kcal_P <- Anova(lm_log_Phenolic.A_Kcal_P)
 anova_lm_log_Phenolic.A_Kcal_Fe_total <- Anova(lm_log_Phenolic.A_Kcal_Fe_total)
 anova_lm_log_Phenolic.A_Kcal_Na <- Anova(lm_log_Phenolic.A_Kcal_Na)
 anova_lm_log_Phenolic.A_Kcal_K <- Anova(lm_log_Phenolic.A_Kcal_K)
 anova_lm_log_Phenolic.A_Kcal_Mg <- Anova(lm_log_Phenolic.A_Kcal_Mg)
 anova_lm_log_Phenolic.A_Kcal_Zn <- Anova(lm_log_Phenolic.A_Kcal_Zn)
 anova_lm_log_Phenolic.A_Kcal_Cu <- Anova(lm_log_Phenolic.A_Kcal_Cu)
 anova_lm_log_Phenolic.A_Kcal_Mn <- Anova(lm_log_Phenolic.A_Kcal_Mn)
 anova_lm_log_Phenolic.A_Kcal_Vit_A_ER <- Anova(lm_log_Phenolic.A_Kcal_Vit_A_ER)
 anova_lm_log_Phenolic.A_Kcal_B1 <- Anova(lm_log_Phenolic.A_Kcal_B1)
 anova_lm_log_Phenolic.A_Kcal_B2 <- Anova(lm_log_Phenolic.A_Kcal_B2)
 anova_lm_log_Phenolic.A_Kcal_B3 <- Anova(lm_log_Phenolic.A_Kcal_B3)
 anova_lm_log_Phenolic.A_Kcal_Ac_pantoenico <- Anova(lm_log_Phenolic.A_Kcal_Ac_pantoenico) 
 anova_lm_log_Phenolic.A_Kcal_B6 <- Anova(lm_log_Phenolic.A_Kcal_B6)
 anova_lm_log_Phenolic.A_Kcal_Folic <- Anova(lm_log_Phenolic.A_Kcal_Folic)
 anova_lm_log_Phenolic.A_Kcal_B12 <- Anova(lm_log_Phenolic.A_Kcal_B12)
 anova_lm_log_Phenolic.A_Kcal_Vit_C <- Anova(lm_log_Phenolic.A_Kcal_Vit_C)
 anova_lm_log_Phenolic.A_Kcal_Acetic_A_Feces <- Anova(lm_log_Phenolic.A_Kcal_Acetic_A_Feces) 
 anova_lm_log_Phenolic.A_Kcal_Propionic_A_Feces <- Anova(lm_log_Phenolic.A_Kcal_Propionic_A_Feces)
 anova_lm_log_Phenolic.A_Kcal_Butyric_A_Feces <- Anova(lm_log_Phenolic.A_Kcal_Butyric_A_Feces)
 anova_lm_log_Phenolic.A_Kcal_Isobutyric_A_Feces <- Anova(lm_log_Phenolic.A_Kcal_Isobutyric_A_Feces)
 anova_lm_log_Phenolic.A_Kcal_Acetic_A_Plasma <- Anova(lm_log_Phenolic.A_Kcal_Acetic_A_Plasma)
 anova_lm_log_Phenolic.A_Kcal_Propionic_A_Plasma <- Anova(lm_log_Phenolic.A_Kcal_Propionic_A_Plasma)
 anova_lm_log_Phenolic.A_Kcal_Isobutyric_A_Plasmas <- Anova(lm_log_Phenolic.A_Kcal_Isobutyric_A_Plasmas)
 anova_lm_log_Phenolic.A_Kcal_Butyric_A_Plasma <- Anova(lm_log_Phenolic.A_Kcal_Butyric_A_Plasma)
 anova_lm_log_Phenolic.A_Kcal_Valeric_A_Plasma <- Anova(lm_log_Phenolic.A_Kcal_Valeric_A_Plasma)
 anova_lm_log_Phenolic.A_Kcal_DII_score <- Anova(lm_log_Phenolic.A_Kcal_DII_score)
 anova_lm_log_Phenolic.A_Kcal_RFM <- Anova(lm_log_Phenolic.A_Kcal_RFM)
 anova_lm_log_Phenolic.A_Kcal_Prevotella <- Anova(lm_log_Phenolic.A_Kcal_Prevotella)
 anova_lm_log_Phenolic.A_Kcal_Lachnospiraceae <- Anova(lm_log_Phenolic.A_Kcal_Lachnospiraceae)
 anova_lm_log_Phenolic.A_Kcal_Pathogen <- Anova(lm_log_Phenolic.A_Kcal_Pathogen)
 anova_lm_log_Phenolic.A_Kcal_Akkermansia.Bacteroidales <- Anova(lm_log_Phenolic.A_Kcal_Akkermansia.Bacteroidales) 
 anova_lm_log_Phenolic.A_Kcal_Ruminococcaceae <- Anova(lm_log_Phenolic.A_Kcal_Ruminococcaceae)
 anova_lm_Log_T.Phenolic.A_KCAL <- Anova(lm_Log_T.Phenolic.A_KCAL)
 
 
 #summary ANOVA-Chormatografy KCal adjusted (BMI,sex, AGE_RANGE)
 
 MLR_Pvalue_T.Phenolic.A_KCAL_TERTILS <- cbind (anova_lm_log_Phenolic.A_Kcal_Age$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_ApoB_mg_dL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_HDL_mg_dL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_VLDL_mg_dL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_LDL_mg_dL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Atherogenic_index$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Creatinine_mg_dL_$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_hsCRP_mg_L$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Glucose_mg_dL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_HbA1c$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Leptina_ng_mL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Adiponectin_ug_ml$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Lep_Adip$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_TGs_mg_dL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Insulin_uU_mL$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Glucose_mmol_L$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_beta_cell_function$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Insulin_Sensibility$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_HOMA_IR$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Weight$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_BMI$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Perc_fat$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Waist_Circumference$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Systolic_BP$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Diastolic_BP$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Calories$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_FIBER$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Ca$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_P$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Fe_total$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Na$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_K$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Mg$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Zn$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Cu$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Mn$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Vit_A_ER$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_B1$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_B2$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_B3$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Ac_pantoenico$`Pr(>F)`,  
                                                anova_lm_log_Phenolic.A_Kcal_B6$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Folic$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_B12$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Vit_C$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Acetic_A_Feces$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Propionic_A_Feces$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Butyric_A_Feces$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Isobutyric_A_Feces$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Acetic_A_Plasma$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Propionic_A_Plasma$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Butyric_A_Plasma$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Valeric_A_Plasma$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_DII_score$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_RFM$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Prevotella$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Lachnospiraceae$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Pathogen$`Pr(>F)`,
                                                anova_lm_log_Phenolic.A_Kcal_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                                anova_lm_log_Phenolic.A_Kcal_Ruminococcaceae$`Pr(>F)`,
                                                anova_lm_Log_T.Phenolic.A_KCAL$`Pr(>F)`)
 
 
                             ##Export and add manually the variables names in the list 
                             write.table(MLR_Pvalue_T.Phenolic.A_KCAL_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/8.MLR_Pvalue_T.Phenolic.A_KCAL_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
 
 
    #lm (Adjust) DII (SE VA A AMLIZAR EL E_DII; POR LO TANTO ESTE DII NO IRA EN EL PAPER)
                             
                             
                             lm_log_DII_Age <-     lm(Log_Age~DB_Polyphenols$DII_TERTIL+
                                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                    DB_Polyphenols$Age_range)
                             lm_log_DII_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$DII_TERTIL+
                                                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                       DB_Polyphenols$Age_range)
                             lm_log_DII_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$DII_TERTIL+
                                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                      DB_Polyphenols$Age_range)
                             lm_log_DII_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$DII_TERTIL+
                                                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                       DB_Polyphenols$Age_range)
                             lm_log_DII_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$DII_TERTIL+
                                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                      DB_Polyphenols$Age_range)
                             lm_log_DII_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$DII_TERTIL+
                                                                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                                 DB_Polyphenols$Age_range)
                             lm_log_DII_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$DII_TERTIL+
                                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                              DB_Polyphenols$Age_range)
                             lm_log_DII_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$DII_TERTIL+
                                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                              DB_Polyphenols$Age_range)
                             lm_log_DII_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$DII_TERTIL+
                                                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                       DB_Polyphenols$Age_range)
                             lm_log_DII_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$DII_TERTIL+
                                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                          DB_Polyphenols$Age_range)
                             lm_log_DII_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$DII_TERTIL+
                                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                  DB_Polyphenols$Age_range)
                             lm_log_DII_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$DII_TERTIL+
                                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                          DB_Polyphenols$Age_range)
                             lm_log_DII_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$DII_TERTIL+
                                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                              DB_Polyphenols$Age_range)
                             lm_log_DII_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_DII_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$DII_TERTIL+
                                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                      DB_Polyphenols$Age_range)
                             lm_log_DII_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$DII_TERTIL+
                                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                          DB_Polyphenols$Age_range)
                             lm_log_DII_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$DII_TERTIL+
                                                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                           DB_Polyphenols$Age_range)
                             lm_log_DII_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$DII_TERTIL+
                                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$DII_TERTIL+
                                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                                DB_Polyphenols$Age_range)
                             lm_log_DII_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$DII_TERTIL+
                                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                    DB_Polyphenols$Age_range)
                             lm_log_DII_Weight <- lm(Log_Weight~DB_Polyphenols$DII_TERTIL+
                                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                   DB_Polyphenols$Age_range)
                             lm_log_DII_BMI <- lm(Log_BMI~DB_Polyphenols$DII_TERTIL+
                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                DB_Polyphenols$Age_range)
                             lm_log_DII_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_DII_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$DII_TERTIL+
                                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                                DB_Polyphenols$Age_range)
                             lm_log_DII_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$DII_TERTIL+
                                                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                        DB_Polyphenols$Age_range)
                             lm_log_DII_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$DII_TERTIL+
                                                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                         DB_Polyphenols$Age_range)
                             lm_log_DII_Calories <- lm(Log_Calories~DB_Polyphenols$DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_DII_FIBER <-lm(Log_FIBER~DB_Polyphenols$DII_TERTIL+
                                                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                 DB_Polyphenols$Age_range)
                             lm_log_DII_Ca <- lm(Log_Ca~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_P <- lm(Log_P~DB_Polyphenols$DII_TERTIL+
                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                              DB_Polyphenols$Age_range)
                             lm_log_DII_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_DII_Na <- lm(Log_Na~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_K <- lm(Log_K~DB_Polyphenols$DII_TERTIL+
                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                              DB_Polyphenols$Age_range)
                             lm_log_DII_Mg <- lm(Log_Mg~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Zn <- lm(Log_Zn~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Cu <- lm(Log_Cu~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Mn <- lm(Log_Mn~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_DII_B1 <- lm(Log_B1~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_B2 <- lm(Log_B2~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_B3 <- lm(Log_B3~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$DII_TERTIL+
                                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                          DB_Polyphenols$Age_range)
                             lm_log_DII_B6 <- lm(Log_B6~DB_Polyphenols$DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Folic <- lm(Log_Folic~DB_Polyphenols$DII_TERTIL+
                                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                  DB_Polyphenols$Age_range)
                             lm_log_DII_B12 <- lm(Log_B12~DB_Polyphenols$DII_TERTIL+
                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                DB_Polyphenols$Age_range)
                             lm_log_DII_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$DII_TERTIL+
                                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                  DB_Polyphenols$Age_range)
                             lm_log_DII_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$DII_TERTIL+
                                                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                           DB_Polyphenols$Age_range)
                             lm_log_DII_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$DII_TERTIL+
                                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                              DB_Polyphenols$Age_range)
                             lm_log_DII_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$DII_TERTIL+
                                                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                            DB_Polyphenols$Age_range)
                             lm_log_DII_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$DII_TERTIL+
                                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$DII_TERTIL+
                                                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                            DB_Polyphenols$Age_range)
                             lm_log_DII_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$DII_TERTIL+
                                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                               DB_Polyphenols$Age_range)
                             lm_log_DII_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$DII_TERTIL+
                                                                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                                 DB_Polyphenols$Age_range)
                             lm_log_DII_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$DII_TERTIL+
                                                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                             DB_Polyphenols$Age_range)
                             lm_log_DII_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$DII_TERTIL+
                                                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                             DB_Polyphenols$Age_range)
                             lm_log_DII_DII_score <- lm(Log_DII_score~DB_Polyphenols$DII_TERTIL+
                                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                      DB_Polyphenols$Age_range)
                             lm_log_DII_RFM <- lm(Log_RFM~DB_Polyphenols$DII_TERTIL+
                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                DB_Polyphenols$Age_range)
                             lm_log_DII_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$DII_TERTIL+
                                                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                       DB_Polyphenols$Age_range)
                             lm_log_DII_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$DII_TERTIL+
                                                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                            DB_Polyphenols$Age_range)
                             lm_log_DII_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_DII_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$DII_TERTIL+
                                                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                                      DB_Polyphenols$Age_range)
                             lm_log_DII_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$DII_TERTIL+
                                                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                            DB_Polyphenols$Age_range)
                             lm_Log_DII_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$DII_TERTIL+
                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                              DB_Polyphenols$Age_range)
                             
                             
                             
                             #ANOVA-CHROMATOGRAPHY/Kcal
                             
                             
                             anova_lm_log_DII_Age <- Anova(lm_log_DII_Age)
                             anova_lm_log_DII_ApoB_mg_dL <- Anova(lm_log_DII_ApoB_mg_dL)
                             anova_lm_log_DII_HDL_mg_dL <- Anova(lm_log_DII_HDL_mg_dL)
                             anova_lm_log_DII_VLDL_mg_dL <- Anova(lm_log_DII_VLDL_mg_dL)
                             anova_lm_log_DII_LDL_mg_dL <- Anova(lm_log_DII_LDL_mg_dL)
                             anova_lm_log_DII_T.Cholesterol_mg_dL_ <-Anova(lm_log_DII_T.Cholesterol_mg_dL_) 
                             anova_lm_log_DII_Atherogenic_index <- Anova(lm_log_DII_Atherogenic_index)
                             anova_lm_log_DII_Creatinine_mg_dL_ <- Anova(lm_log_DII_Creatinine_mg_dL_)
                             anova_lm_log_DII_hsCRP_mg_L <- Anova(lm_log_DII_hsCRP_mg_L)
                             anova_lm_log_DII_Glucose_mg_dL <- Anova(lm_log_DII_Glucose_mg_dL)
                             anova_lm_log_DII_HbA1c <- Anova(lm_log_DII_HbA1c)
                             anova_lm_log_DII_Leptina_ng_mL <- Anova(lm_log_DII_Leptina_ng_mL)
                             anova_lm_log_DII_Adiponectin_ug_ml <- Anova(lm_log_DII_Adiponectin_ug_ml)
                             anova_lm_log_DII_Lep_Adip <- Anova(lm_log_DII_Lep_Adip)
                             anova_lm_log_DII_TGs_mg_dL <- Anova(lm_log_DII_TGs_mg_dL)
                             anova_lm_log_DII_Insulin_uU_mL <- Anova(lm_log_DII_Insulin_uU_mL)
                             anova_lm_log_DII_Glucose_mmol_L <- Anova(lm_log_DII_Glucose_mmol_L)
                             anova_lm_log_DII_beta_cell_function <- Anova(lm_log_DII_beta_cell_function)
                             anova_lm_log_DII_Insulin_Sensibility <- Anova(lm_log_DII_Insulin_Sensibility)
                             anova_lm_log_DII_HOMA_IR <- Anova(lm_log_DII_HOMA_IR)
                             anova_lm_log_DII_Weight <- Anova(lm_log_DII_Weight)
                             anova_lm_log_DII_BMI <- Anova(lm_log_DII_BMI)
                             anova_lm_log_DII_Perc_fat <- Anova(lm_log_DII_Perc_fat)
                             anova_lm_log_DII_Waist_Circumference <- Anova(lm_log_DII_Waist_Circumference)
                             anova_lm_log_DII_Systolic_BP <- Anova(lm_log_DII_Systolic_BP)
                             anova_lm_log_DII_Diastolic_BP <- Anova(lm_log_DII_Diastolic_BP)
                             anova_lm_log_DII_Calories <- Anova(lm_log_DII_Calories)
                             anova_lm_log_DII_FIBER <- Anova(lm_log_DII_FIBER)
                             anova_lm_log_DII_Ca <- Anova(lm_log_DII_Ca)
                             anova_lm_log_DII_P <- Anova(lm_log_DII_P)
                             anova_lm_log_DII_Fe_total <- Anova(lm_log_DII_Fe_total)
                             anova_lm_log_DII_Na <- Anova(lm_log_DII_Na)
                             anova_lm_log_DII_K <- Anova(lm_log_DII_K)
                             anova_lm_log_DII_Mg <- Anova(lm_log_DII_Mg)
                             anova_lm_log_DII_Zn <- Anova(lm_log_DII_Zn)
                             anova_lm_log_DII_Cu <- Anova(lm_log_DII_Cu)
                             anova_lm_log_DII_Mn <- Anova(lm_log_DII_Mn)
                             anova_lm_log_DII_Vit_A_ER <- Anova(lm_log_DII_Vit_A_ER)
                             anova_lm_log_DII_B1 <- Anova(lm_log_DII_B1)
                             anova_lm_log_DII_B2 <- Anova(lm_log_DII_B2)
                             anova_lm_log_DII_B3 <- Anova(lm_log_DII_B3)
                             anova_lm_log_DII_Ac_pantoenico <- Anova(lm_log_DII_Ac_pantoenico) 
                             anova_lm_log_DII_B6 <- Anova(lm_log_DII_B6)
                             anova_lm_log_DII_Folic <- Anova(lm_log_DII_Folic)
                             anova_lm_log_DII_B12 <- Anova(lm_log_DII_B12)
                             anova_lm_log_DII_Vit_C <- Anova(lm_log_DII_Vit_C)
                             anova_lm_log_DII_Acetic_A_Feces <- Anova(lm_log_DII_Acetic_A_Feces) 
                             anova_lm_log_DII_Propionic_A_Feces <- Anova(lm_log_DII_Propionic_A_Feces)
                             anova_lm_log_DII_Butyric_A_Feces <- Anova(lm_log_DII_Butyric_A_Feces)
                             anova_lm_log_DII_Isobutyric_A_Feces <- Anova(lm_log_DII_Isobutyric_A_Feces)
                             anova_lm_log_DII_Acetic_A_Plasma <- Anova(lm_log_DII_Acetic_A_Plasma)
                             anova_lm_log_DII_Propionic_A_Plasma <- Anova(lm_log_DII_Propionic_A_Plasma)
                             anova_lm_log_DII_Isobutyric_A_Plasmas <- Anova(lm_log_DII_Isobutyric_A_Plasmas)
                             anova_lm_log_DII_Butyric_A_Plasma <- Anova(lm_log_DII_Butyric_A_Plasma)
                             anova_lm_log_DII_Valeric_A_Plasma <- Anova(lm_log_DII_Valeric_A_Plasma)
                             anova_lm_log_DII_DII_score <- Anova(lm_log_DII_DII_score)
                             anova_lm_log_DII_RFM <- Anova(lm_log_DII_RFM)
                             anova_lm_log_DII_Prevotella <- Anova(lm_log_DII_Prevotella)
                             anova_lm_log_DII_Lachnospiraceae <- Anova(lm_log_DII_Lachnospiraceae)
                             anova_lm_log_DII_Pathogen <- Anova(lm_log_DII_Pathogen)
                             anova_lm_log_DII_Akkermansia.Bacteroidales <- Anova(lm_log_DII_Akkermansia.Bacteroidales) 
                             anova_lm_log_DII_Ruminococcaceae <- Anova(lm_log_DII_Ruminococcaceae)
                             anova_lm_Log_DII_TPC_Folin <- Anova(lm_Log_DII_TPC_Folin)
                             
                             
                             #summary ANOVA-Chormatografy KCal adjusted (BMI,sex, AGE_RANGE)
                             
                             MLR_Pvalue_T.DII_TERTILS <- cbind (anova_lm_log_DII_Age$`Pr(>F)`,
                                                                            anova_lm_log_DII_ApoB_mg_dL$`Pr(>F)`,
                                                                            anova_lm_log_DII_HDL_mg_dL$`Pr(>F)`,
                                                                            anova_lm_log_DII_VLDL_mg_dL$`Pr(>F)`,
                                                                            anova_lm_log_DII_LDL_mg_dL$`Pr(>F)`,
                                                                            anova_lm_log_DII_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Atherogenic_index$`Pr(>F)`,
                                                                            anova_lm_log_DII_Creatinine_mg_dL_$`Pr(>F)`,
                                                                            anova_lm_log_DII_hsCRP_mg_L$`Pr(>F)`,
                                                                            anova_lm_log_DII_Glucose_mg_dL$`Pr(>F)`,
                                                                            anova_lm_log_DII_HbA1c$`Pr(>F)`,
                                                                            anova_lm_log_DII_Leptina_ng_mL$`Pr(>F)`,
                                                                            anova_lm_log_DII_Adiponectin_ug_ml$`Pr(>F)`,
                                                                            anova_lm_log_DII_Lep_Adip$`Pr(>F)`,
                                                                            anova_lm_log_DII_TGs_mg_dL$`Pr(>F)`,
                                                                            anova_lm_log_DII_Insulin_uU_mL$`Pr(>F)`,
                                                                            anova_lm_log_DII_Glucose_mmol_L$`Pr(>F)`,
                                                                            anova_lm_log_DII_beta_cell_function$`Pr(>F)`,
                                                                            anova_lm_log_DII_Insulin_Sensibility$`Pr(>F)`,
                                                                            anova_lm_log_DII_HOMA_IR$`Pr(>F)`,
                                                                            anova_lm_log_DII_Weight$`Pr(>F)`,
                                                                            anova_lm_log_DII_BMI$`Pr(>F)`,
                                                                            anova_lm_log_DII_Perc_fat$`Pr(>F)`,
                                                                            anova_lm_log_DII_Waist_Circumference$`Pr(>F)`,
                                                                            anova_lm_log_DII_Systolic_BP$`Pr(>F)`,
                                                                            anova_lm_log_DII_Diastolic_BP$`Pr(>F)`,
                                                                            anova_lm_log_DII_Calories$`Pr(>F)`,
                                                                            anova_lm_log_DII_FIBER$`Pr(>F)`,
                                                                            anova_lm_log_DII_Ca$`Pr(>F)`,
                                                                            anova_lm_log_DII_P$`Pr(>F)`,
                                                                            anova_lm_log_DII_Fe_total$`Pr(>F)`,
                                                                            anova_lm_log_DII_Na$`Pr(>F)`,
                                                                            anova_lm_log_DII_K$`Pr(>F)`,
                                                                            anova_lm_log_DII_Mg$`Pr(>F)`,
                                                                            anova_lm_log_DII_Zn$`Pr(>F)`,
                                                                            anova_lm_log_DII_Cu$`Pr(>F)`,
                                                                            anova_lm_log_DII_Mn$`Pr(>F)`,
                                                                            anova_lm_log_DII_Vit_A_ER$`Pr(>F)`,
                                                                            anova_lm_log_DII_B1$`Pr(>F)`,
                                                                            anova_lm_log_DII_B2$`Pr(>F)`,
                                                                            anova_lm_log_DII_B3$`Pr(>F)`,
                                                                            anova_lm_log_DII_Ac_pantoenico$`Pr(>F)`,  
                                                                            anova_lm_log_DII_B6$`Pr(>F)`,
                                                                            anova_lm_log_DII_Folic$`Pr(>F)`,
                                                                            anova_lm_log_DII_B12$`Pr(>F)`,
                                                                            anova_lm_log_DII_Vit_C$`Pr(>F)`,
                                                                            anova_lm_log_DII_Acetic_A_Feces$`Pr(>F)`,
                                                                            anova_lm_log_DII_Propionic_A_Feces$`Pr(>F)`,
                                                                            anova_lm_log_DII_Butyric_A_Feces$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Isobutyric_A_Feces$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Acetic_A_Plasma$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Propionic_A_Plasma$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Butyric_A_Plasma$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Valeric_A_Plasma$`Pr(>F)`, 
                                                                            anova_lm_log_DII_DII_score$`Pr(>F)`,
                                                                            anova_lm_log_DII_RFM$`Pr(>F)`,
                                                                            anova_lm_log_DII_Prevotella$`Pr(>F)`,
                                                                            anova_lm_log_DII_Lachnospiraceae$`Pr(>F)`,
                                                                            anova_lm_log_DII_Pathogen$`Pr(>F)`,
                                                                            anova_lm_log_DII_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                                                            anova_lm_log_DII_Ruminococcaceae$`Pr(>F)`,
                                                                            anova_lm_Log_DII_TPC_Folin$`Pr(>F)`)
                             
                             
                             ##Export and add manually the variables names in the list 
                             write.table(MLR_Pvalue_T.DII_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/9.MLR_Pvalue_T.DII_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                             
                             
  #lm (Adjust) E_DII 
                             
                             
                             lm_log_E_DII_Age <-     lm(Log_Age~DB_Polyphenols$E_DII_TERTIL+
                                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                        DB_Polyphenols$Age_range)
                             lm_log_E_DII_ApoB_mg_dL <- lm(Log_ApoB_mg_dL~DB_Polyphenols$E_DII_TERTIL+
                                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                           DB_Polyphenols$Age_range)
                             lm_log_E_DII_HDL_mg_dL <- lm(Log_HDL_mg_dL~DB_Polyphenols$E_DII_TERTIL+
                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                          DB_Polyphenols$Age_range)
                             lm_log_E_DII_VLDL_mg_dL <- lm(Log_VLDL_mg_dL~DB_Polyphenols$E_DII_TERTIL+
                                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                           DB_Polyphenols$Age_range)
                             lm_log_E_DII_LDL_mg_dL <- lm(Log_LDL_mg_dL~DB_Polyphenols$E_DII_TERTIL+
                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                          DB_Polyphenols$Age_range)
                             lm_log_E_DII_T.Cholesterol_mg_dL_ <- lm(Log_T.Cholesterol_mg_dL_~DB_Polyphenols$E_DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_E_DII_Atherogenic_index <- lm(Log_Atherogenic_index~DB_Polyphenols$E_DII_TERTIL+
                                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                  DB_Polyphenols$Age_range)
                             lm_log_E_DII_Creatinine_mg_dL_ <- lm(Log_Creatinine_mg_dL_~DB_Polyphenols$E_DII_TERTIL+
                                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                  DB_Polyphenols$Age_range)
                             lm_log_E_DII_hsCRP_mg_L <- lm(Log_hsCRP_mg_L~DB_Polyphenols$E_DII_TERTIL+
                                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                           DB_Polyphenols$Age_range)
                             lm_log_E_DII_Glucose_mg_dL <- lm(Log_Glucose_mg_dL~DB_Polyphenols$E_DII_TERTIL+
                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                              DB_Polyphenols$Age_range)
                             lm_log_E_DII_HbA1c <- lm(Log_HbA1c~DB_Polyphenols$E_DII_TERTIL+
                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                      DB_Polyphenols$Age_range)
                             lm_log_E_DII_Leptina_ng_mL <-lm( Log_Leptina_ng_mL~DB_Polyphenols$E_DII_TERTIL+
                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                              DB_Polyphenols$Age_range)
                             lm_log_E_DII_Adiponectin_ug_ml <- lm(Log_Adiponectin_ug_ml~DB_Polyphenols$E_DII_TERTIL+
                                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                  DB_Polyphenols$Age_range)
                             lm_log_E_DII_Lep_Adip <- lm(Log_Lep_Adip~DB_Polyphenols$E_DII_TERTIL+
                                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                         DB_Polyphenols$Age_range)
                             lm_log_E_DII_TGs_mg_dL <- lm(Log_TGs_mg_dL~DB_Polyphenols$E_DII_TERTIL+
                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                          DB_Polyphenols$Age_range)
                             lm_log_E_DII_Insulin_uU_mL <- lm(Log_Insulin_uU_mL~DB_Polyphenols$E_DII_TERTIL+
                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                              DB_Polyphenols$Age_range)
                             lm_log_E_DII_Glucose_mmol_L <- lm(Log_Glucose_mmol_L~DB_Polyphenols$E_DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_E_DII_beta_cell_function <- lm(Log_beta_cell_function~DB_Polyphenols$E_DII_TERTIL+
                                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Insulin_Sensibility <- lm(Log_Insulin_Sensibility~DB_Polyphenols$E_DII_TERTIL+
                                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                    DB_Polyphenols$Age_range)
                             lm_log_E_DII_HOMA_IR <- lm(Log_HOMA_IR~DB_Polyphenols$E_DII_TERTIL+
                                                        DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                        DB_Polyphenols$Age_range)
                             lm_log_E_DII_Weight <- lm(Log_Weight~DB_Polyphenols$E_DII_TERTIL+
                                                       DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                       DB_Polyphenols$Age_range)
                             lm_log_E_DII_BMI <- lm(Log_BMI~DB_Polyphenols$E_DII_TERTIL+
                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                    DB_Polyphenols$Age_range)
                             lm_log_E_DII_Perc_fat <- lm(Log_Perc_fat~DB_Polyphenols$E_DII_TERTIL+
                                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                         DB_Polyphenols$Age_range)
                             lm_log_E_DII_Waist_Circumference <- lm(Log_Waist_Circumference~DB_Polyphenols$E_DII_TERTIL+
                                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                    DB_Polyphenols$Age_range)
                             lm_log_E_DII_Systolic_BP <- lm(Log_Systolic_BP~DB_Polyphenols$E_DII_TERTIL+
                                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                            DB_Polyphenols$Age_range)
                             lm_log_E_DII_Diastolic_BP <- lm(Log_Diastolic_BP~DB_Polyphenols$E_DII_TERTIL+
                                                             DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                             DB_Polyphenols$Age_range)
                             lm_log_E_DII_Calories <- lm(Log_Calories~DB_Polyphenols$E_DII_TERTIL+
                                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                         DB_Polyphenols$Age_range)
                             lm_log_E_DII_FIBER <-lm(Log_FIBER~DB_Polyphenols$E_DII_TERTIL+
                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                     DB_Polyphenols$Age_range)
                             lm_log_E_DII_Ca <- lm(Log_Ca~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_P <- lm(Log_P~DB_Polyphenols$E_DII_TERTIL+
                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                  DB_Polyphenols$Age_range)
                             lm_log_E_DII_Fe_total <- lm(Log_Fe_total~DB_Polyphenols$E_DII_TERTIL+
                                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                         DB_Polyphenols$Age_range)
                             lm_log_E_DII_Na <- lm(Log_Na~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_K <- lm(Log_K~DB_Polyphenols$E_DII_TERTIL+
                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                  DB_Polyphenols$Age_range)
                             lm_log_E_DII_Mg <- lm(Log_Mg~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Zn <- lm(Log_Zn~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Cu <- lm(Log_Cu~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Mn <- lm(Log_Mn~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Vit_A_ER <- lm(Log_Vit_A_ER~DB_Polyphenols$E_DII_TERTIL+
                                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                         DB_Polyphenols$Age_range)
                             lm_log_E_DII_B1 <- lm(Log_B1~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_B2 <- lm(Log_B2~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_B3 <- lm(Log_B3~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Ac_pantoenico <- lm(Log_Ac_pantoenico~DB_Polyphenols$E_DII_TERTIL+
                                                              DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                              DB_Polyphenols$Age_range)
                             lm_log_E_DII_B6 <- lm(Log_B6~DB_Polyphenols$E_DII_TERTIL+
                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Folic <- lm(Log_Folic~DB_Polyphenols$E_DII_TERTIL+
                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                      DB_Polyphenols$Age_range)
                             lm_log_E_DII_B12 <- lm(Log_B12~DB_Polyphenols$E_DII_TERTIL+
                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                    DB_Polyphenols$Age_range)
                             lm_log_E_DII_Vit_C <- lm(Log_Vit_C~DB_Polyphenols$E_DII_TERTIL+
                                                      DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                      DB_Polyphenols$Age_range)
                             lm_log_E_DII_Acetic_A_Feces <- lm(Log_Acetic_A_Feces~DB_Polyphenols$E_DII_TERTIL+
                                                               DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                               DB_Polyphenols$Age_range)
                             lm_log_E_DII_Propionic_A_Feces <- lm(Log_Propionic_A_Feces~DB_Polyphenols$E_DII_TERTIL+
                                                                  DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                  DB_Polyphenols$Age_range)
                             lm_log_E_DII_Butyric_A_Feces <- lm(Log_Butyric_A_Feces~DB_Polyphenols$E_DII_TERTIL+
                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                DB_Polyphenols$Age_range)
                             lm_log_E_DII_Isobutyric_A_Feces <- lm(Log_Isobutyric_A_Feces~DB_Polyphenols$E_DII_TERTIL+
                                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Acetic_A_Plasma <- lm(Log_Acetic_A_Plasma~DB_Polyphenols$E_DII_TERTIL+
                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                DB_Polyphenols$Age_range)
                             lm_log_E_DII_Propionic_A_Plasma <- lm(Log_Propionic_A_Plasma~DB_Polyphenols$E_DII_TERTIL+
                                                                   DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                   DB_Polyphenols$Age_range)
                             lm_log_E_DII_Isobutyric_A_Plasmas <- lm(Log_Isobutyric_A_Plasmas~DB_Polyphenols$E_DII_TERTIL+
                                                                     DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                     DB_Polyphenols$Age_range)
                             lm_log_E_DII_Butyric_A_Plasma <- lm(Log_Butyric_A_Plasma~DB_Polyphenols$E_DII_TERTIL+
                                                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                 DB_Polyphenols$Age_range)
                             lm_log_E_DII_Valeric_A_Plasma <- lm(Log_Valeric_A_Plasma~DB_Polyphenols$E_DII_TERTIL+
                                                                 DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                 DB_Polyphenols$Age_range)
                             lm_log_E_DII_DII_score <- lm(Log_DII_score~DB_Polyphenols$E_DII_TERTIL+
                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                          DB_Polyphenols$Age_range)
                             lm_log_E_DII_E_DII_score <- lm(Log_E_DII_score~DB_Polyphenols$E_DII_TERTIL+
                                                            DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                            DB_Polyphenols$Age_range)
                             lm_log_E_DII_RFM <- lm(Log_RFM~DB_Polyphenols$E_DII_TERTIL+
                                                    DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                    DB_Polyphenols$Age_range)
                             lm_log_E_DII_Prevotella <- lm(Log_Prevotella~DB_Polyphenols$E_DII_TERTIL+
                                                           DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                           DB_Polyphenols$Age_range)
                             lm_log_E_DII_Lachnospiraceae <- lm(Log_Lachnospiraceae~DB_Polyphenols$E_DII_TERTIL+
                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                DB_Polyphenols$Age_range)
                             lm_log_E_DII_Pathogen <- lm(Log_Pathogen~DB_Polyphenols$E_DII_TERTIL+
                                                         DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                         DB_Polyphenols$Age_range)
                             lm_log_E_DII_Akkermansia.Bacteroidales <- lm(Log_Akkermansia.Bacteroidales~DB_Polyphenols$E_DII_TERTIL+
                                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                          DB_Polyphenols$Age_range)
                             lm_log_E_DII_Ruminococcaceae <- lm(Log_Ruminococcaceae~DB_Polyphenols$E_DII_TERTIL+
                                                                DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                                DB_Polyphenols$Age_range)
                             lm_log_E_DII_TPC_Folin <- lm(Log_TPC_Folin~DB_Polyphenols$E_DII_TERTIL+
                                                          DB_Polyphenols$City+DB_Polyphenols$Sex+
                                                          DB_Polyphenols$Age_range)
                             
                             
                             
                             #ANOVA-CHROMATOGRAPHY/Kcal
                             
                             
                             anova_lm_log_E_DII_Age <- Anova(lm_log_E_DII_Age)
                             anova_lm_log_E_DII_ApoB_mg_dL <- Anova(lm_log_E_DII_ApoB_mg_dL)
                             anova_lm_log_E_DII_HDL_mg_dL <- Anova(lm_log_E_DII_HDL_mg_dL)
                             anova_lm_log_E_DII_VLDL_mg_dL <- Anova(lm_log_E_DII_VLDL_mg_dL)
                             anova_lm_log_E_DII_LDL_mg_dL <- Anova(lm_log_E_DII_LDL_mg_dL)
                             anova_lm_log_E_DII_T.Cholesterol_mg_dL_ <-Anova(lm_log_E_DII_T.Cholesterol_mg_dL_) 
                             anova_lm_log_E_DII_Atherogenic_index <- Anova(lm_log_E_DII_Atherogenic_index)
                             anova_lm_log_E_DII_Creatinine_mg_dL_ <- Anova(lm_log_E_DII_Creatinine_mg_dL_)
                             anova_lm_log_E_DII_hsCRP_mg_L <- Anova(lm_log_E_DII_hsCRP_mg_L)
                             anova_lm_log_E_DII_Glucose_mg_dL <- Anova(lm_log_E_DII_Glucose_mg_dL)
                             anova_lm_log_E_DII_HbA1c <- Anova(lm_log_E_DII_HbA1c)
                             anova_lm_log_E_DII_Leptina_ng_mL <- Anova(lm_log_E_DII_Leptina_ng_mL)
                             anova_lm_log_E_DII_Adiponectin_ug_ml <- Anova(lm_log_E_DII_Adiponectin_ug_ml)
                             anova_lm_log_E_DII_Lep_Adip <- Anova(lm_log_E_DII_Lep_Adip)
                             anova_lm_log_E_DII_TGs_mg_dL <- Anova(lm_log_E_DII_TGs_mg_dL)
                             anova_lm_log_E_DII_Insulin_uU_mL <- Anova(lm_log_E_DII_Insulin_uU_mL)
                             anova_lm_log_E_DII_Glucose_mmol_L <- Anova(lm_log_E_DII_Glucose_mmol_L)
                             anova_lm_log_E_DII_beta_cell_function <- Anova(lm_log_E_DII_beta_cell_function)
                             anova_lm_log_E_DII_Insulin_Sensibility <- Anova(lm_log_E_DII_Insulin_Sensibility)
                             anova_lm_log_E_DII_HOMA_IR <- Anova(lm_log_E_DII_HOMA_IR)
                             anova_lm_log_E_DII_Weight <- Anova(lm_log_E_DII_Weight)
                             anova_lm_log_E_DII_BMI <- Anova(lm_log_E_DII_BMI)
                             anova_lm_log_E_DII_Perc_fat <- Anova(lm_log_E_DII_Perc_fat)
                             anova_lm_log_E_DII_Waist_Circumference <- Anova(lm_log_E_DII_Waist_Circumference)
                             anova_lm_log_E_DII_Systolic_BP <- Anova(lm_log_E_DII_Systolic_BP)
                             anova_lm_log_E_DII_Diastolic_BP <- Anova(lm_log_E_DII_Diastolic_BP)
                             anova_lm_log_E_DII_Calories <- Anova(lm_log_E_DII_Calories)
                             anova_lm_log_E_DII_FIBER <- Anova(lm_log_E_DII_FIBER)
                             anova_lm_log_E_DII_Ca <- Anova(lm_log_E_DII_Ca)
                             anova_lm_log_E_DII_P <- Anova(lm_log_E_DII_P)
                             anova_lm_log_E_DII_Fe_total <- Anova(lm_log_E_DII_Fe_total)
                             anova_lm_log_E_DII_Na <- Anova(lm_log_E_DII_Na)
                             anova_lm_log_E_DII_K <- Anova(lm_log_E_DII_K)
                             anova_lm_log_E_DII_Mg <- Anova(lm_log_E_DII_Mg)
                             anova_lm_log_E_DII_Zn <- Anova(lm_log_E_DII_Zn)
                             anova_lm_log_E_DII_Cu <- Anova(lm_log_E_DII_Cu)
                             anova_lm_log_E_DII_Mn <- Anova(lm_log_E_DII_Mn)
                             anova_lm_log_E_DII_Vit_A_ER <- Anova(lm_log_E_DII_Vit_A_ER)
                             anova_lm_log_E_DII_B1 <- Anova(lm_log_E_DII_B1)
                             anova_lm_log_E_DII_B2 <- Anova(lm_log_E_DII_B2)
                             anova_lm_log_E_DII_B3 <- Anova(lm_log_E_DII_B3)
                             anova_lm_log_E_DII_Ac_pantoenico <- Anova(lm_log_E_DII_Ac_pantoenico) 
                             anova_lm_log_E_DII_B6 <- Anova(lm_log_E_DII_B6)
                             anova_lm_log_E_DII_Folic <- Anova(lm_log_E_DII_Folic)
                             anova_lm_log_E_DII_B12 <- Anova(lm_log_E_DII_B12)
                             anova_lm_log_E_DII_Vit_C <- Anova(lm_log_E_DII_Vit_C)
                             anova_lm_log_E_DII_Acetic_A_Feces <- Anova(lm_log_E_DII_Acetic_A_Feces) 
                             anova_lm_log_E_DII_Propionic_A_Feces <- Anova(lm_log_E_DII_Propionic_A_Feces)
                             anova_lm_log_E_DII_Butyric_A_Feces <- Anova(lm_log_E_DII_Butyric_A_Feces)
                             anova_lm_log_E_DII_Isobutyric_A_Feces <- Anova(lm_log_E_DII_Isobutyric_A_Feces)
                             anova_lm_log_E_DII_Acetic_A_Plasma <- Anova(lm_log_E_DII_Acetic_A_Plasma)
                             anova_lm_log_E_DII_Propionic_A_Plasma <- Anova(lm_log_E_DII_Propionic_A_Plasma)
                             anova_lm_log_E_DII_Isobutyric_A_Plasmas <- Anova(lm_log_E_DII_Isobutyric_A_Plasmas)
                             anova_lm_log_E_DII_Butyric_A_Plasma <- Anova(lm_log_E_DII_Butyric_A_Plasma)
                             anova_lm_log_E_DII_Valeric_A_Plasma <- Anova(lm_log_E_DII_Valeric_A_Plasma)
                             anova_lm_log_E_DII_DII_score <- Anova(lm_log_E_DII_DII_score)
                             anova_lm_log_E_DII_E_DII_score <- Anova(lm_log_E_DII_E_DII_score)
                             anova_lm_log_E_DII_RFM <- Anova(lm_log_E_DII_RFM)
                             anova_lm_log_E_DII_Prevotella <- Anova(lm_log_E_DII_Prevotella)
                             anova_lm_log_E_DII_Lachnospiraceae <- Anova(lm_log_E_DII_Lachnospiraceae)
                             anova_lm_log_E_DII_Pathogen <- Anova(lm_log_E_DII_Pathogen)
                             anova_lm_log_E_DII_Akkermansia.Bacteroidales <- Anova(lm_log_E_DII_Akkermansia.Bacteroidales) 
                             anova_lm_log_E_DII_Ruminococcaceae <- Anova(lm_log_E_DII_Ruminococcaceae)
                             anova_lm_log_E_DII_TPC_Folin <- Anova(lm_log_E_DII_TPC_Folin)
                             
                             
                             #summary ANOVA-Chormatografy KCal adjusted (BMI,sex, AGE_RANGE)
                             
                             MLR_Pvalue_T.E_DII_TERTILS <- cbind (anova_lm_log_E_DII_Age$`Pr(>F)`,
                                                                anova_lm_log_E_DII_ApoB_mg_dL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_HDL_mg_dL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_VLDL_mg_dL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_LDL_mg_dL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_T.Cholesterol_mg_dL_$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Atherogenic_index$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Creatinine_mg_dL_$`Pr(>F)`,
                                                                anova_lm_log_E_DII_hsCRP_mg_L$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Glucose_mg_dL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_HbA1c$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Leptina_ng_mL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Adiponectin_ug_ml$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Lep_Adip$`Pr(>F)`,
                                                                anova_lm_log_E_DII_TGs_mg_dL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Insulin_uU_mL$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Glucose_mmol_L$`Pr(>F)`,
                                                                anova_lm_log_E_DII_beta_cell_function$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Insulin_Sensibility$`Pr(>F)`,
                                                                anova_lm_log_E_DII_HOMA_IR$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Weight$`Pr(>F)`,
                                                                anova_lm_log_E_DII_BMI$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Perc_fat$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Waist_Circumference$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Systolic_BP$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Diastolic_BP$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Calories$`Pr(>F)`,
                                                                anova_lm_log_E_DII_FIBER$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Ca$`Pr(>F)`,
                                                                anova_lm_log_E_DII_P$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Fe_total$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Na$`Pr(>F)`,
                                                                anova_lm_log_E_DII_K$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Mg$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Zn$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Cu$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Mn$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Vit_A_ER$`Pr(>F)`,
                                                                anova_lm_log_E_DII_B1$`Pr(>F)`,
                                                                anova_lm_log_E_DII_B2$`Pr(>F)`,
                                                                anova_lm_log_E_DII_B3$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Ac_pantoenico$`Pr(>F)`,  
                                                                anova_lm_log_E_DII_B6$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Folic$`Pr(>F)`,
                                                                anova_lm_log_E_DII_B12$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Vit_C$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Acetic_A_Feces$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Propionic_A_Feces$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Butyric_A_Feces$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Isobutyric_A_Feces$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Acetic_A_Plasma$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Propionic_A_Plasma$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Isobutyric_A_Plasmas$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Butyric_A_Plasma$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Valeric_A_Plasma$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_DII_score$`Pr(>F)`,
                                                                anova_lm_log_E_DII_E_DII_score$`Pr(>F)`,
                                                                anova_lm_log_E_DII_RFM$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Prevotella$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Lachnospiraceae$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Pathogen$`Pr(>F)`,
                                                                anova_lm_log_E_DII_Akkermansia.Bacteroidales$`Pr(>F)`, 
                                                                anova_lm_log_E_DII_Ruminococcaceae$`Pr(>F)`,
                                                                anova_lm_log_E_DII_TPC_Folin$`Pr(>F)`)
                             
                             
                             ##Export and add manually the variables names in the list 
                             write.table(MLR_Pvalue_T.E_DII_TERTILS,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/lm/10.MLR_Pvalue_T.E_DII_TERTILS.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                             
                             
                             
                             
                             
                             
                             
                             
                             
    #Univariate stats FOLIN-TERTILS    (va en el paper)                        
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_TPC_Folin_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,36:58,66:79)]),
                                                  function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$TPC_Folin_Tertil,DB_Polyphenols))
               pval_Kruskal_TPC_Folin_Tertil <- sapply(Kruskal_TPC_Folin_Tertil, function(x) x$p.value) #Only p values
               name_kruskal_pval_Folin_Tertil <- colnames(DB_Polyphenols[c(5:25,27,28,33,34,36:58,66:79)])
               
               
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_TPC_Folin_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/1.pval_Kruskal_TPC_Folin_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               write.table(name_kruskal_pval_Folin_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/1.name_kruskal_pval_Folin_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               

               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_TPC_Folin_Tertil <- qvalue(pval_Kruskal_TPC_Folin_Tertil)
               plot(qValues_Kruskal_TPC_Folin_Tertil)
               hist(qValues_Kruskal_TPC_Folin_Tertil)
               write.table(qValues_Kruskal_TPC_Folin_Tertil[["qvalues"]],file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/1.1.qvalue_Kruskal_TPC_Folin_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #FOLIN TERTILES DUNN
               
               dunnTest_FOLIN_Tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               
               dunnTest_FOLIN_tertils_FIBER<- dunnTest(DB_Polyphenols$FIBER ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Ca<- dunnTest(DB_Polyphenols$Ca ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_P<- dunnTest(DB_Polyphenols$P ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Fe_total<- dunnTest(DB_Polyphenols$Fe_total ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Na<- dunnTest(DB_Polyphenols$Na ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_K<- dunnTest(DB_Polyphenols$K ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Mg<- dunnTest(DB_Polyphenols$Mg ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Zn<- dunnTest(DB_Polyphenols$Zn ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Cu<- dunnTest(DB_Polyphenols$Cu ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Mn<- dunnTest(DB_Polyphenols$Mn ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Vit_A_ER<- dunnTest(DB_Polyphenols$Vit_A_ER ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_B1<- dunnTest(DB_Polyphenols$B1 ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_B2<- dunnTest(DB_Polyphenols$B2 ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_B3<- dunnTest(DB_Polyphenols$B3 ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Ac_pantoenico<- dunnTest(DB_Polyphenols$Ac_pantoenico ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_B6<- dunnTest(DB_Polyphenols$B6 ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Folic<- dunnTest(DB_Polyphenols$Folic ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_B12<- dunnTest(DB_Polyphenols$B12 ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_tertils_Vit_C<- dunnTest(DB_Polyphenols$Vit_C ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_FOLIN_Tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_E_DII_score<- dunnTest(DB_Polyphenols$E_DII ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Tertils_TPC_Folin<- dunnTest(DB_Polyphenols$TPC_Folin ~ DB_Polyphenols$TPC_Folin_Tertil, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_FOLIN_TERTIL <- cbind (dunnTest_FOLIN_Tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_HbA1c[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Weight[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_BMI[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Perc_fat[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_Tertils_Systolic_BP[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_Tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                       
                                                       dunnTest_FOLIN_tertils_FIBER[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_tertils_Ca[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_P[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Fe_total[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Na[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_K[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Mg[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Zn[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Cu[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Mn[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Vit_A_ER[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_B1[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_B2[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_B3[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Ac_pantoenico[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_B6[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Folic[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_B12[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_tertils_Vit_C[["res"]][["P.adj"]],
                                                       
                                                       
                                                       dunnTest_FOLIN_Tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_E_DII_score[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_RFM[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Prevotella[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Pathogen[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Tertils_TPC_Folin[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_FOLIN_TERTIL,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/1.dunnTest_summary_FOLIN_TERTIL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
              
               
    #Univariate stats FOLIN-TERTILS/kCAL   (no va en el paper)                         
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_TPC_Folin_Tertil_Kcal_Adj <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                           function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj,DB_Polyphenols))
               pval_Kruskal_TPC_Folin_Tertil_Kcal_Adj <- sapply(Kruskal_TPC_Folin_Tertil_Kcal_Adj, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_TPC_Folin_Tertil_Kcal_Adj,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/2.pval_Kruskal_TPC_Folin_Tertil_Kcal_Adj.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_TPC_Folin_Tertil_Kcal_Adj <- qvalue(pval_Kruskal_TPC_Folin_Tertil_Kcal_Adj)
               plot(qValues_Kruskal_TPC_Folin_Tertil_Kcal_Adj)
               hist(qValues_Kruskal_TPC_Folin_Tertil_Kcal_Adj)
               write.table(qValues_Kruskal_TPC_Folin_Tertil_Kcal_Adj[["qvalues"]],file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/2.1.qvalue_Kruskal_TPC_Folin_Tertil_Kcal_Adj.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #FOLIN TERTILES DUNN
               
               dunnTest_FOLIN_Kcal_Tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_FOLIN_Kcal_Tertils_TPC_Folin<- dunnTest(DB_Polyphenols$TPC_Folin ~ DB_Polyphenols$TPC_Folin_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_FOLIN_Kcal_TERTIL <- cbind (dunnTest_FOLIN_Kcal_Tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_HbA1c[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Weight[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_BMI[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Perc_fat[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_Kcal_Tertils_Systolic_BP[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_Kcal_Tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                       dunnTest_FOLIN_Kcal_Tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_DII_score[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_RFM[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Prevotella[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Pathogen[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                       dunnTest_FOLIN_Kcal_Tertils_TPC_Folin[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_FOLIN_Kcal_TERTIL,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/2.dunnTest_summary_FOLIN_Kcal_TERTIL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
    #Univariate stats Chromatography      (no va en el paper)                      
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_TPC_Chrom_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                           function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$TPC_Chr_Tertil,DB_Polyphenols))
               pval_Kruskal_TPC_Chrom_Tertil<- sapply(Kruskal_TPC_Chrom_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_TPC_Chrom_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/3.pval_Kruskal_TPC_Chrom_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_TPC_Chrom <- qvalue(pval_Kruskal_TPC_Chrom_Tertil)
               plot(qValues_Kruskal_TPC_Chrom)
               hist(qValues_Kruskal_TPC_Chrom)
               write.table(qValues_Kruskal_TPC_Chrom[["qvalues"]],file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/3.1.qValues_Kruskal_TPC_Chrom.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               
      #perform Dunn's Test with Bonferroni correction for p-values
      #Chromatography TERTILES DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_Chrom_Tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_<- dunnTest(DB_Polyphenols$TPC_Folin ~ DB_Polyphenols$TPC_Chr_Tertil, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_Chrom_TERTIL <- cbind (dunnTest_Chrom_Tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_HbA1c[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Weight[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_BMI[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Perc_fat[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                            dunnTest_Chrom_Tertils_Systolic_BP[["res"]][["P.adj"]],
                                                            dunnTest_Chrom_Tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                            dunnTest_Chrom_Tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_DII_score[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_RFM[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Prevotella[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Pathogen[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                            dunnTest_Chrom_Tertils_[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_Chrom_TERTIL,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/3.dunnTest_summary_Chrom_TERTIL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               

               
  #Univariate stats Chromatography adjusted by KCAL  (no va en el paper)                         
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_TPC_Chrom_KCAL_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                  function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj,DB_Polyphenols))
               pval_Kruskal_TPC_Chrom_KCAL_Tertil<- sapply(Kruskal_TPC_Chrom_KCAL_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_TPC_Chrom_KCAL_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/4.pval_Kruskal_TPC_Chrom_KCAL_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_TPC_Chrom_KCAL <- qvalue(pval_Kruskal_TPC_Chrom_KCAL_Tertil)
               plot(qValues_Kruskal_TPC_Chrom_KCAL)
               hist(qValues_Kruskal_TPC_Chrom_KCAL)
               write.table(qValues_Kruskal_TPC_Chrom_KCAL[["qvalues"]],file = "~/R/FoodTree_All/.FoodTree_AWS/Figures/kruskal/4.1.qValues_Kruskal_TPC_Chrom_KCAL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #Chromatography TERTILES DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_Chrom_Tertils_Kcal_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Chrom_Tertils_Kcal<- dunnTest(DB_Polyphenols$TPC_Folin ~ DB_Polyphenols$TPC_Chr_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_Chrom_Kcal_TERTIL <- cbind (dunnTest_Chrom_Tertils_Kcal_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_HDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_LDL_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Atherogenic_index[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_HbA1c[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Lep_Adip[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_TGs_mg_dL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_beta_cell_function[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_HOMA_IR[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Weight[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_BMI[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Perc_fat[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Waist_Circumference[["res"]][["P.adj"]],
                                                       dunnTest_Chrom_Tertils_Kcal_Systolic_BP[["res"]][["P.adj"]],
                                                       dunnTest_Chrom_Tertils_Kcal_Diastolic_BP[["res"]][["P.adj"]],
                                                       dunnTest_Chrom_Tertils_Kcal_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_DII_score[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_RFM[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Prevotella[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Lachnospiraceae[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Pathogen[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal_Ruminococcaceae[["res"]][["P.adj"]], 
                                                       dunnTest_Chrom_Tertils_Kcal[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_Chrom_Kcal_TERTIL,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/4.dunnTest_summary_Chrom_Kcal_TERTIL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               

               

               
               
               
    #Univariate stats pHENOLIC ACIDS    (no va en el paper)                       
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_TPC_Phenolic_Acids_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                       function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$T.Phenolic.A_Tertil,DB_Polyphenols))
               pval_Kruskal_Phenolic_Acids_Tertil<- sapply(Kruskal_TPC_Chrom_KCAL_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_Phenolic_Acids_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/5.pval_Kruskal_pval_Kruskal_Phenolic_Acids_Tertil_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_Phenolic_Acids <- qvalue(pval_Kruskal_Phenolic_Acids_Tertil)
               plot(qValues_Kruskal_Phenolic_Acids)
               hist(qValues_Kruskal_Phenolic_Acids)
               write.table(qValues_Kruskal_Phenolic_Acids[["qvalues"]],file = "~/R/FoodTree_All/.FoodTree_AWS/Figures/kruskal/5.1.qValues_Kruskal_qValues_Kruskal_Phenolic_Acids.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #phenolic acids_ TERTILES DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_Phenolic_acids_tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_tertils<- dunnTest(DB_Polyphenols$T.Phenolic.A ~ DB_Polyphenols$T.Phenolic.A_Tertil, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_Phenolic_acids_TERTIL <- cbind (dunnTest_Phenolic_acids_tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_HbA1c[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Weight[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_BMI[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Perc_fat[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                            dunnTest_Phenolic_acids_tertils_Systolic_BP[["res"]][["P.adj"]],
                                                            dunnTest_Phenolic_acids_tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                            dunnTest_Phenolic_acids_tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_DII_score[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_RFM[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Prevotella[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Pathogen[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                            dunnTest_Phenolic_acids_tertils[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_Phenolic_acids_TERTIL,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/5.dunnTest_summary_dunnTest_summary_Phenolic_acids_TERTIL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

               
               
               
#Univariate stats pHENOLIC ACIDS/KCAL   (no va en el paper)                        
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
                 Kruskal_TPC_Phenolic_Acids_KCAL_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                             function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj,DB_Polyphenols))
               pval_Kruskal_Phenolic_Acids_Kcal_Tertil<- sapply(Kruskal_TPC_Phenolic_Acids_KCAL_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_Phenolic_Acids_Kcal_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/6.pval_Kruskal_pval_Kruskal_Phenolic_Acids_Kcal_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_Phenolic_Acids_Kcal <- qvalue(pval_Kruskal_Phenolic_Acids_Kcal_Tertil)
               plot(qValues_Kruskal_Phenolic_Acids)
               hist(qValues_Kruskal_Phenolic_Acids)
               write.table(qValues_Kruskal_Phenolic_Acids_Kcal[["qvalues"]],file = "~/R/FoodTree_All/.FoodTree_AWS/Figures/kruskal/6.1.qValues_Kruskal_Phenolic_Acids_Kcal.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #phenolic acids_ TERTILES DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_Phenolic_acids_kcal_tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Phenolic_acids_Kcal_tertils<- dunnTest(DB_Polyphenols$T.Phenolic.A_KCAL_Adj ~ DB_Polyphenols$T.Phenolic.A_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_Phenolic_acids_Kcal_TERTIL <- cbind (dunnTest_Phenolic_acids_kcal_tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_HbA1c[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Weight[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_BMI[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Perc_fat[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Systolic_BP[["res"]][["P.adj"]],
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_DII_score[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_RFM[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Prevotella[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Pathogen[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                                dunnTest_Phenolic_acids_Kcal_tertils[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_Phenolic_acids_Kcal_TERTIL,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/6.dunnTest_summary_dunnTest_summary_Phenolic_acids_Kcal_TERTIL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               

               
  
  #Univariate stats Flavonoids     (no va en el paper)                      
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_Flavonoids_KCAL_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                                function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$T.Flavonoids_Tertil,DB_Polyphenols))
               pval_Kruskal_Flavonoids_Tertil<- sapply(Kruskal_Flavonoids_KCAL_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_Flavonoids_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/7.pval_Kruskal_Flavonoids_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_Flavonoids <- qvalue(pval_Kruskal_Flavonoids_Tertil)
               plot(qValues_Kruskal_Phenolic_Acids)
               hist(qValues_Kruskal_Phenolic_Acids)
               write.table(qValues_Kruskal_Flavonoids[["qvalues"]],file = "~/R/FoodTree_All/.FoodTree_AWS/Figures/kruskal/7.1.qValues_Kruskal_Flavonoids.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #phenolic acids_ TERTILES DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_Flavonoids_tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_tertils<- dunnTest(DB_Polyphenols$T.Flavonoids ~ DB_Polyphenols$T.Flavonoids_Tertil, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_Flavonoids_TERTIL <- cbind (dunnTest_Flavonoids_tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_HbA1c[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Weight[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_BMI[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Perc_fat[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                                     dunnTest_Flavonoids_tertils_Systolic_BP[["res"]][["P.adj"]],
                                                                     dunnTest_Flavonoids_tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                                     dunnTest_Flavonoids_tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_DII_score[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_RFM[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Prevotella[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Pathogen[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                                     dunnTest_Flavonoids_tertils[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_Flavonoids_TERTIL,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/7.dunnTest_summary_dunnTest_summary_Flavonoids.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               

  #Univariate stats Flavonoids/kCAL     (no va en el paper)                       
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_Flavonoids_KCAL_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                        function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj,DB_Polyphenols))
               pval_Kruskal_Flavonoids_kCAL_Tertil<- sapply(Kruskal_Flavonoids_KCAL_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_Flavonoids_kCAL_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/8.pval_Kruskal_Flavonoids_kCAL_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_Flavonoids_kCAL <- qvalue(pval_Kruskal_Flavonoids_kCAL_Tertil)
               plot(qValues_Kruskal_Phenolic_Acids)
               hist(qValues_Kruskal_Phenolic_Acids)
               write.table(qValues_Kruskal_Flavonoids_kCAL[["qvalues"]],file = "~/R/FoodTree_All/.FoodTree_AWS/Figures/kruskal/8.1.qValues_Kruskal_Flavonoids_kCAL.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #phenolic acids_ TERTILES DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_Flavonoids_Kcal_tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               dunnTest_Flavonoids_Kcal_tertils<- dunnTest(DB_Polyphenols$T.Flavonoids_KCAL_Adj ~ DB_Polyphenols$T.Flavonoids_Tertil_Kcal_Adj, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_Flavonoids_Kcal_tertils <- cbind (dunnTest_Flavonoids_Kcal_tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_HbA1c[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Weight[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_BMI[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Perc_fat[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                            dunnTest_Flavonoids_Kcal_tertils_Systolic_BP[["res"]][["P.adj"]],
                                                            dunnTest_Flavonoids_Kcal_tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                            dunnTest_Flavonoids_Kcal_tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_DII_score[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_RFM[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Prevotella[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Pathogen[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                            dunnTest_Flavonoids_Kcal_tertils[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_Flavonoids_Kcal_tertils,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/8.dunnTest_summary_Flavonoids_Kcal_tertils.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
#Univariate stats DII        (no va en el paper)                   
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               
               Kruskal_DII_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,55:58,66:78)]),
                                                        function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$DII_TERTIL,DB_Polyphenols))
               pval_Kruskal_DII_Tertil<- sapply(Kruskal_DII_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_DII_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/9.pval_Kruskal_DII_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_DII <- qvalue(pval_Kruskal_DII_Tertil)
               plot(qValues_Kruskal_DII)
               hist(qValues_Kruskal_DII)
               write.table(qValues_Kruskal_DII[["qvalues"]],file = "~/R/FoodTree_All/.FoodTree_AWS/Figures/kruskal/9.1.qValues_Kruskal_DII.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #DII DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_DII_tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_DII_score<- dunnTest(DB_Polyphenols$DII_score ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_DII_TPC_Folin_tertils<- dunnTest(DB_Polyphenols$TPC_Folin ~ DB_Polyphenols$DII_TERTIL, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_DII_tertils <- cbind (dunnTest_DII_tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_HbA1c[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Weight[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_BMI[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Perc_fat[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                                  dunnTest_DII_tertils_Systolic_BP[["res"]][["P.adj"]],
                                                                  dunnTest_DII_tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                                  dunnTest_DII_tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_DII_score[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_RFM[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Prevotella[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Pathogen[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                                  dunnTest_DII_tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                      dunnTest_DII_TPC_Folin_tertils[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_DII_tertils,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/9.dunnTest_summary_DII_tertils.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
  #Univariate stats E_DII   (tabla paper)                       
               
               #Kruskal analysis (non parametric)
               
               colnames(DB_Polyphenols)
               name_kruskal_pval_E_DiI <- colnames(DB_Polyphenols[c(5:25,27,28,33,34,36:58,66:79)])
               Kruskal_E_DII_Tertil <- lapply(colnames(DB_Polyphenols [c(5:25,27,28,33,34,36:58,66:79)]),
                                            function(x) kruskal.test(DB_Polyphenols[,x],DB_Polyphenols$E_DII_TERTIL,DB_Polyphenols))
               pval_Kruskal_E_DII_Tertil<- sapply(Kruskal_E_DII_Tertil, function(x) x$p.value) #Only p values
               ##Export and add manually the variables names in the list 
               write.table(pval_Kruskal_E_DII_Tertil,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/10.pval_Kruskal_E_DII_Tertil.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               write.table(name_kruskal_pval_E_DiI,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/10.name_kruskal_pval_E_DiI.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               #qvalues (all variables)
               # get q-value object
               qValues_Kruskal_E_DII <- qvalue(pval_Kruskal_E_DII_Tertil)
               plot(qValues_Kruskal_E_DII)
               hist(qValues_Kruskal_E_DII)
               write.table(qValues_Kruskal_E_DII[["qvalues"]],file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/kruskal/10.qValues_Kruskal_E_DII.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               #perform Dunn's Test with Bonferroni correction for p-values
               #DII DUNN
               colnames(DB_Polyphenols)
               
               dunnTest_E_DII_tertils_ApoB_mg_dL<- dunnTest(DB_Polyphenols$ApoB_mg_dL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_HDL_mg_dL<- dunnTest(DB_Polyphenols$HDL_mg_dL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_VLDL_mg_dL<- dunnTest(DB_Polyphenols$VLDL_mg_dL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_LDL_mg_dL<- dunnTest(DB_Polyphenols$LDL_mg_dL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_T.Cholesterol_mg_dL_<- dunnTest(DB_Polyphenols$T.Cholesterol_mg_dL_ ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Atherogenic_index<- dunnTest(DB_Polyphenols$Atherogenic_index ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Creatinine_mg_dL_<- dunnTest(DB_Polyphenols$Creatinine_mg_dL_ ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_hsCRP_mg_L<- dunnTest(DB_Polyphenols$hsCRP_mg_L ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Glucose_mg_dL<- dunnTest(DB_Polyphenols$Glucose_mg_dL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_HbA1c<- dunnTest(DB_Polyphenols$HbA1c ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Leptina_ng_mL<- dunnTest(DB_Polyphenols$Leptina_ng_mL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Adiponectin_ug_ml<- dunnTest(DB_Polyphenols$Adiponectin_ug_ml ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Lep_Adip<- dunnTest(DB_Polyphenols$Lep_Adip ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_TGs_mg_dL<- dunnTest(DB_Polyphenols$TGs_mg_dL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Insulin_uU_mL<- dunnTest(DB_Polyphenols$Insulin_uU_mL ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Glucose_mmol_L<- dunnTest(DB_Polyphenols$Glucose_mmol_L ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_beta_cell_function<- dunnTest(DB_Polyphenols$beta_cell_function ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Insulin_Sensibility<- dunnTest(DB_Polyphenols$Insulin_Sensibility ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_HOMA_IR<- dunnTest(DB_Polyphenols$HOMA_IR ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Weight<- dunnTest(DB_Polyphenols$Weight ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_BMI<- dunnTest(DB_Polyphenols$BMI ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Perc_fat<- dunnTest(DB_Polyphenols$Perc_fat ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Waist_Circumference<- dunnTest(DB_Polyphenols$Waist_Circumference ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Systolic_BP<- dunnTest(DB_Polyphenols$Systolic_BP ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Diastolic_BP<- dunnTest(DB_Polyphenols$Diastolic_BP ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               
               dunnTest_E_DII_tertils_FIBER<- dunnTest(DB_Polyphenols$FIBER ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Ca<- dunnTest(DB_Polyphenols$Ca ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_P<- dunnTest(DB_Polyphenols$P ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Fe_total<- dunnTest(DB_Polyphenols$Fe_total ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Na<- dunnTest(DB_Polyphenols$Na ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_K<- dunnTest(DB_Polyphenols$K ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Mg<- dunnTest(DB_Polyphenols$Mg ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Zn<- dunnTest(DB_Polyphenols$Zn ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Cu<- dunnTest(DB_Polyphenols$Cu ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Mn<- dunnTest(DB_Polyphenols$Mn ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Vit_A_ER<- dunnTest(DB_Polyphenols$Vit_A_ER ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_B1<- dunnTest(DB_Polyphenols$B1 ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_B2<- dunnTest(DB_Polyphenols$B2 ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_B3<- dunnTest(DB_Polyphenols$B3 ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Ac_pantoenico<- dunnTest(DB_Polyphenols$Ac_pantoenico ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_B6<- dunnTest(DB_Polyphenols$B6 ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Folic<- dunnTest(DB_Polyphenols$Folic ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_B12<- dunnTest(DB_Polyphenols$B12 ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Vit_C<- dunnTest(DB_Polyphenols$Vit_C ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")

               dunnTest_E_DII_tertils_Acetic_A_Feces<- dunnTest(DB_Polyphenols$Acetic_A_Feces ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Propionic_A_Feces<- dunnTest(DB_Polyphenols$Propionic_A_Feces ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Butyric_A_Feces<- dunnTest(DB_Polyphenols$Butyric_A_Feces ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Isobutyric_A_Feces<- dunnTest(DB_Polyphenols$Isobutyric_A_Feces ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Acetic_A_Plasma<- dunnTest(DB_Polyphenols$Acetic_A_Plasma ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Propionic_A_Plasma<- dunnTest(DB_Polyphenols$Propionic_A_Plasma ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Isobutyric_A_Plasmas<- dunnTest(DB_Polyphenols$Isobutyric_A_Plasmas ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Butyric_A_Plasma<- dunnTest(DB_Polyphenols$Butyric_A_Plasma ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Valeric_A_Plasma<- dunnTest(DB_Polyphenols$Valeric_A_Plasma ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_E_DII_score<- dunnTest(DB_Polyphenols$E_DII ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_RFM<- dunnTest(DB_Polyphenols$RFM ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Prevotella<- dunnTest(DB_Polyphenols$Prevotella ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Lachnospiraceae<- dunnTest(DB_Polyphenols$Lachnospiraceae ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Pathogen<- dunnTest(DB_Polyphenols$Pathogen ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Akkermansia.Bacteroidales<- dunnTest(DB_Polyphenols$Akkermansia.Bacteroidales ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_tertils_Ruminococcaceae<- dunnTest(DB_Polyphenols$Ruminococcaceae ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               dunnTest_E_DII_TPC_Folin_tertils<- dunnTest(DB_Polyphenols$TPC_Folin ~ DB_Polyphenols$E_DII_TERTIL, DB_Polyphenols, method="bonferroni")
               
               
               dunnTest_summary_E_DII_tertils <- cbind (dunnTest_DII_tertils_ApoB_mg_dL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_HDL_mg_dL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_VLDL_mg_dL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_LDL_mg_dL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_T.Cholesterol_mg_dL_[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Atherogenic_index[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Creatinine_mg_dL_[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_hsCRP_mg_L[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Glucose_mg_dL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_HbA1c[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Leptina_ng_mL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Adiponectin_ug_ml[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Lep_Adip[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_TGs_mg_dL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Insulin_uU_mL[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Glucose_mmol_L[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_beta_cell_function[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Insulin_Sensibility[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_HOMA_IR[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Weight[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_BMI[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Perc_fat[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Waist_Circumference[["res"]][["P.adj"]],
                                                      dunnTest_DII_tertils_Systolic_BP[["res"]][["P.adj"]],
                                                      dunnTest_DII_tertils_Diastolic_BP[["res"]][["P.adj"]],
                                                      
                                                      dunnTest_E_DII_tertils_FIBER[["res"]][["P.adj"]], 
                                                      dunnTest_E_DII_tertils_Ca[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_P[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Fe_total[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Na[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_K[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Mg[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Zn[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Cu[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Mn[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Vit_A_ER[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_B1[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_B2[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_B3[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Ac_pantoenico[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_B6[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Folic[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_B12[["res"]][["P.adj"]],
                                                      dunnTest_E_DII_tertils_Vit_C[["res"]][["P.adj"]],
                                                      dunnTest_DII_tertils_Acetic_A_Feces[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Propionic_A_Feces[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Butyric_A_Feces[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Isobutyric_A_Feces[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Acetic_A_Plasma[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Propionic_A_Plasma[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Isobutyric_A_Plasmas[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Butyric_A_Plasma[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Valeric_A_Plasma[["res"]][["P.adj"]], 
                                                      dunnTest_E_DII_tertils_E_DII_score[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_RFM[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Prevotella[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Lachnospiraceae[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Pathogen[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Akkermansia.Bacteroidales[["res"]][["P.adj"]], 
                                                      dunnTest_DII_tertils_Ruminococcaceae[["res"]][["P.adj"]], 
                                                      dunnTest_DII_TPC_Folin_tertils[["res"]][["P.adj"]])
               
               #Export and add manually the variables names in the list 
               write.table(dunnTest_summary_E_DII_tertils,file = "~/R/FoodTree_All/3.FoodTree_AWS/Figures/Dunn/10.dunnTest_summary_E_DII_tertils.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
               
               
               
               
               
               
               
                              
               
               
               
               
               
               
               
                  
   # Correlation Network----
        #Figure #1     
          colnames(DB_Polyphenols)
          DB_Polyphenols_scale  <-  scale(DB_Polyphenols[c(-1:-4,-6,-7, -9,-14,-15,-21:-29,-32,-58:-73,-75:-102)])
          colnames(DB_Polyphenols_scale)
          
        tidy_cors <- DB_Polyphenols_scale %>% 
          correlate() %>% 
          stretch()
        
        # Convert correlations stronger than some value
        # to an undirected graph object
        
        
        graph_cors <- tidy_cors %>% 
          filter(abs(r) > 0.205) %>%
          graph_from_data_frame(directed = FALSE)
        
        ggraph(graph_cors) +
          geom_edge_link() +
          geom_node_point() +
          geom_node_text(aes(label = name))
        
        ggraph(graph_cors) +
          geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +  #cambie el edge_width = abs(r) por none
          guides(edge_alpha = "none", edge_width = "none") +
          scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff", "#fd240b")) + 
          geom_node_point(color = "black", size = 4) +
          geom_node_text(aes(label = name),size = 7, repel = TRUE) +
          theme_graph() +
          labs(title = "Global correlation (n=116)")
          
            #Export save as image  1000 x 1200
    
    # Corrplot
        
        correlations <- (cor(DB_Polyphenols_scale)) 
        corrplot(correlations, is.corr=FALSE, method="circle", type = "upper", tl.cex = 0.4, tl.srt = 90)
        write.table(correlations,file = "correlations.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
        
            #P value adjusted
            
              cor.mtest <- function(Adipo_scale, ...) {
                Adipo_scale <- as.matrix(Adipo_scale)
                n <- ncol(Adipo_scale)
                p.Adipo_scale<- matrix(NA, n, n)
                diag(p.Adipo_scale) <- 0
                for (i in 1:(n - 1)) {
                  for (j in (i + 1):n) {
                    tmp <- cor.test(Adipo_scale[, i], Adipo_scale[, j], ...)
                    p.Adipo_scale[i, j] <- p.Adipo_scale[j, i] <- tmp$p.value
                  }
                }
                colnames(p.Adipo_scale) <- rownames(p.Adipo_scale) <- colnames(Adipo_scale)
                p.Adipo_scale
              }
              
              # matrix of the p-value of the correlation
              p.Adipo_scale <- cor.mtest(Adipo_scale)
              head(p.Adipo_scale[, 1:5])
              #write.table(p.Lipidomic_cambio,file = "Lipidomic_cambio.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
              
              #corrplot with p_value
              correlations_Adipo_scale <- corrplot(correlations, is.corr=FALSE, method="square", type = "upper", p.mat = p.Adipo_scale, sig.level = 0.05, tl.cex = 0.8, tl.srt = 90,insig = "blank")
                  write.table(correlations_Adipo_scale,file = "Output/Tables/correlations_Adipo_scale.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  write.table(p.Adipo_scale,file = "Output/Tables/correlations_Pvalue.Adipo_scale.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
    
               
  
                  
   # PCA adipokines
      #(BD_adipo_ Solo adipoquinas, zonulina , SCFAs,HsCRP, LBP, oxLDL, CAG y DIeta)
                  
                  #Figure 3
                  colnames(DB_Polyphenols)
                  DB_Polyphenols_PCA <-  DB_Polyphenols[c(-1:-36,-55, -58:-83, -85:-102)]
                  colnames(DB_Polyphenols_PCA)
                  
                  res.pca <- PCA(DB_Polyphenols_PCA, scale.unit = TRUE, ncp = 10, graph = TRUE)
                  eig.val <- get_eigenvalue(res.pca_BMI) #Use this value for visualization of PCA biplot final
                  fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
                  
                  #Biplos
                  
                  #BMI
                  Biplot_BMI = fviz_pca_biplot(res.pca,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$BMI_Classification, palette = c("#dcedc1","#065535","#5ac18e"), pointshape = 21, pointsize = 6, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "norm", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_BMI, title = "", subtitle = "", caption = FALSE, xlab = "PC1(19.8%)", ylab = "PC2(11.2%)", legend.position = "top", legendsize = 8)
                  
                  #Status
                  Biplot_Status = fviz_pca_biplot(res.pca,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$Status_CMH, palette = c("#b8360a","#3d85c6", "#ae0c0c" ), pointshape = 21, pointsize = 6, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#90460a","#d06a15",  "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "t", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_Status, title = "", subtitle = "", caption = FALSE, xlab = "PC1(19.8%)", ylab = "PC2(11.2%)", legend.position = "top", legendsize = 8)
                  
                  #Waist circumference_Colombi

                  Biplot_WC_Col = fviz_pca_biplot(res.pca,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$WC_Classification_Col, palette = c("#eeeeee","#3d85c6"), pointshape = 21, pointsize = 6, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "t", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_WC_Col, title = "", subtitle = "", caption = FALSE, xlab = "PC1(19.8%)", ylab = "PC2(11.2%)", legend.position = "top", legendsize = 8)
                  
                  #Waist circumference_;MiSalud
                  
                  Biplot_WC_MiSalud = fviz_pca_biplot(res.pca,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$WC_Classification, palette = c("#3d85c6", "#ae0c0c", "#b8360a"), pointshape = 21, pointsize = 6, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "t", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_WC_MiSalud, title = "", subtitle = "", caption = FALSE, xlab = "PC1(19.8%)", ylab = "PC2(11.2%)", legend.position = "top", legendsize = 8)
                  
                  #Waist circumference + status
                  
                  Biplot_WC_Status = fviz_pca_biplot(res.pca,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$WC_phenotype_colombian, palette = c("#0b5394", "#cfe2f3" , "#990000", "#f4cccc"), pointshape = 21, pointsize = 6, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "t", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_WC_Status, title = "", subtitle = "", caption = FALSE, xlab = "PC1(19.8%)", ylab = "PC2(11.2%)", legend.position = "top", legendsize = 8)
                  
                  
                  #HDL
                  Biplot_HDL = fviz_pca_biplot(res.pca,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$a_cholesterol_HDL, palette = c("#990000", "#cc0000", "#e06666","#f4cccc"), pointshape = 21, pointsize = 6, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "t", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_HDL, title = "", subtitle = "", caption = FALSE, xlab = "PC1(19.8%)", ylab = "PC2(11.2%)", legend.position = "top", legendsize = 8)
                  
                  
                  
                  #Fat phenotype
                  res.pca_FAT <- PCA(DB_Polyphenols_No_Clinical_Variables, scale.unit = TRUE, ncp = 10, graph = TRUE)
                  eig.val_FAT <- get_eigenvalue(res.pca_WC ) #Use this value for visualization of PCA biplot final
                  
                  fviz_eig(res.pca_WC, addlabels = TRUE, ylim = c(0, 50))
                  
                  Biplot_FAT = fviz_pca_biplot(res.pca_FAT,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$BodyFat_phenotype, palette = c("#ffffff","#065535","#5ac18e"), pointshape = 21, pointsize = 5, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "t", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_FAT, title = "", subtitle = "", caption = FALSE, xlab = "PC1(17.46%)", ylab = "PC2(10.12%)", legend.position = "top", legendsize = 8)
                  
                  
                  #Wais circumference
                  
                  Biplot_WC_MiSalud = fviz_pca_biplot(res.pca,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = DB_Polyphenols$WC_Classification, palette = c("#ffffff", "#dcedc1","#065535","#5ac18e"), pointshape = 21, pointsize = 5, labelsize = 5, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "t", addEllipses = FALSE, ellipse.alpha = 0.2)
                  ggpubr::ggpar(Biplot_WC_MiSalud, title = "", subtitle = "", caption = FALSE, xlab = "PC1(19.8%)", ylab = "PC2(11.2%)", legend.position = "top", legendsize = 8)
                  
                  
                  
                  # Correlation and p values of variables with each components of PCA 
                  
                  res.desc_Variables <- dimdesc(res.pca, axes = c(1,2,3), proba = 0.05) #res.pca se optiene del codigo anterior en PCA
                  # Description of dimension 
                  res.desc_Variables$Dim.1
                  res.desc_Variables$Dim.2
                  res.desc_Variables$Dim.3
                  write.table(res.desc_Variables[["Dim.1"]][["quanti"]],file = "Output_Adipokines/Correlation_PCA/P_Value_PC1.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  write.table(res.desc_Variables[["Dim.2"]][["quanti"]],file = "Output_Adipokines/Correlation_PCA/P_Value_PC2.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

                  

    
                  #EXPLORATORIO PRELIMINAR (SOLO PARA VER COMO SE ASOCIA LOS POLIFENOLES CON ADIPOQUINAS)
                  
                  #Imputation. Nine values are under imputation (LDL: MI-078; PAS: MI:354; OXLDL: MI-201,MI-257, MI-262, MI-276, MI-354, MI-384,MI-447)
                  
                  #All variables
                  DB_Polyphenols_F_Im <- DB_Polyphenols [c(-1:-15)]
                  Prueba <- PCA(DB_Polyphenols_F_Im, graph = TRUE)
                  
                  #Only adipokines and phenols/kg
                  DB_Polyphenols_P_Im <- DB_Polyphenols [c(-1:-28, -70, -71, -79, -80)]
                  colnames (DB_Polyphenols_P_Im)
                  
                  Prueba <- PCA(DB_Polyphenols_P_Im, graph = TRUE)
                  
                  
                  # Red correlaciones
                  tidy_cors <- DB_Polyphenols_P_Im %>% 
                    correlate() %>% 
                    stretch()
                  # Convert correlations stronger than some value
                  # to an undirected graph object
                  graph_cors <- tidy_cors %>% 
                    filter(abs(r) > 0.25) %>%
                    graph_from_data_frame(directed = FALSE)
                  ggraph(graph_cors) +
                    geom_edge_link() +
                    geom_node_point() +
                    geom_node_text(aes(label = name))
                  ggraph(graph_cors) +
                    geom_edge_link(aes(edge_alpha = abs(r), edge_width = "none", color = r)) +  #cambie el edge_width = abs(r) por none
                    guides(edge_alpha = "none", edge_width = "none") +
                    scale_edge_colour_gradientn(limits = c(-0.7, 1), colors = c("#003366","#ffffff", "#fd240b")) + 
                    geom_node_point(color = "black", size = 4) +
                    geom_node_text(aes(label = name),size = 7, repel = TRUE) +
                    theme_graph() +
                    labs(title = "Adipokins & Phenols (n=116)")
                  
                  
                  
                  
                  
                    ###ANalysis by type of clasification
          
      
                  
      ### CUIDADO EN EL ARCHIVO DE PRISMA SOLO ESTAN ORGANIZADOS LOS DE IMC           
                  
            #BMI Clasification  
                  
                  State_Adipo <- DB_Polyphenols [c(-1, -2, -4:-55)]
                  Adipo_S <-data.frame(Adipo_scale)
                  Adipo_S1 <-Adipo_S [c(-1:-23)]
                  State_Adipo_scaled <- cbind(State_Adipo, Adipo_S1) #File to prisma
                  view(Adipo_scale)
                  #Output for prisma analysis
                  write.table(State_Adipo_scaled,file = "Output/Imputed/ZScore_Adipo_BMI_im.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  
                  
                  BD2_sub <- State_Adipo_scaled %>%
                    group_by(Clasification.1) %>%
                    select_if(is.numeric)
                  
                  
                  BD2_cor <- BD2_sub %>%
                    group_by(Clasification.1) %>% ## redundant but worth it for illustration
                    nest() %>%
                    mutate(data = map(data, purrr::compose(stretch, correlate))) %>%
                    unnest() %>%
                    select(x, y, r, Clasification.1) %>%
                    filter(abs(r) > .27) %>%
                    graph_from_data_frame(directed = FALSE)
                  
                  ggraph(BD2_cor, layout = "kk") +
                    geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 1.5) +
                    guides(edge_alpha = "none") +
                    scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff","#ff0000")) +
                    geom_node_point(color = "black", size = 3) +
                    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
                    facet_edges( ~ Clasification.1)  +
                    theme_minimal() +
                    labs(title = "BMI CLASSIFICATION")
                  
       
             
          
             #FAT PHENOTYPE Clasification  
                  
                  Fat_Adipo <- DB_Polyphenols [c(-1:-9, -11:-55)]
                  Adipo_S <-data.frame(Adipo_scale)
                  Adipo_S1 <-Adipo_S [c(-1:-23)]
                  Fat_Adipo_scaled <- cbind(Fat_Adipo, Adipo_S1) #File to prisma
                  view(Adipo_scale)
                  #Output for prisma analysis
                  write.table(Fat_Adipo_scaled,file = "Output/Imputed/ZScore_Adipo_PhenotypeFAT_im.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  
                  
                  BD2_sub <- State_Adipo_scaled %>%
                    group_by(Clasification.1) %>%
                    select_if(is.numeric)
                  
                  
                  BD2_cor <- BD2_sub %>%
                    group_by(Clasification.1) %>% ## redundant but worth it for illustration
                    nest() %>%
                    mutate(data = map(data, purrr::compose(stretch, correlate))) %>%
                    unnest() %>%
                    select(x, y, r, Clasification.1) %>%
                    filter(abs(r) > .27) %>%
                    graph_from_data_frame(directed = FALSE)
                  
                  ggraph(BD2_cor, layout = "kk") +
                    geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 1.5) +
                    guides(edge_alpha = "none") +
                    scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff","#ff0000")) +
                    geom_node_point(color = "black", size = 3) +
                    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
                    facet_edges( ~ Clasification.1)  +
                    theme_minimal() +
                    labs(title = "BMI CLASSIFICATION")
                  
                #Waist phenotype 
                  
                  Waist_Adipo <- DB_Polyphenols [c(-1:-8, -10:-55)]
                  Adipo_S <-data.frame(Adipo_scale)
                  Adipo_S1 <-Adipo_S [c(-1:-23)]
                  Waist_Adipo_scaled <- cbind(Waist_Adipo, Adipo_S1) #File to prisma
                  view(Adipo_scale)
                  #Output for prisma analysis
                  write.table(Waist_Adipo_scaled,file = "Output/Imputed/ZScore_Adipo_PhenotypeWaist_im.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  
                  
                  BD2_sub <- State_Adipo_scaled %>%
                    group_by(Clasification.1) %>%
                    select_if(is.numeric)
                  
                  
                  BD2_cor <- BD2_sub %>%
                    group_by(Clasification.1) %>% ## redundant but worth it for illustration
                    nest() %>%
                    mutate(data = map(data, purrr::compose(stretch, correlate))) %>%
                    unnest() %>%
                    select(x, y, r, Clasification.1) %>%
                    filter(abs(r) > .27) %>%
                    graph_from_data_frame(directed = FALSE)
                  
                  ggraph(BD2_cor, layout = "kk") +
                    geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 1.5) +
                    guides(edge_alpha = "none") +
                    scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff","#ff0000")) +
                    geom_node_point(color = "black", size = 3) +
                    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
                    facet_edges( ~ Clasification.1)  +
                    theme_minimal() +
                    labs(title = "BMI CLASSIFICATION")
                  
                  
              #Cardiometabolic status (Healthy vs Anormal)
                  
                  Caridom_status <- DB_Polyphenols [c(-1:-6, -8:-55)]
                  Adipo_S <-data.frame(Adipo_scale)
                  Adipo_S1 <-Adipo_S [c(-1:-23)]
                  Caridom_status_scaled <- cbind(Caridom_status, Adipo_S1) #File to prisma
                  view(Adipo_scale)
                  #Output for prisma analysis
                  write.table(Caridom_status_scaled,file = "Output/Imputed/ZScore_Adipo_Caridom_status_im.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  
                  
                  
                  BD2_sub <- State_Adipo_scaled %>%
                    group_by(Clasification.1) %>%
                    select_if(is.numeric)
                  
                  
                  BD2_cor <- BD2_sub %>%
                    group_by(Clasification.1) %>% ## redundant but worth it for illustration
                    nest() %>%
                    mutate(data = map(data, purrr::compose(stretch, correlate))) %>%
                    unnest() %>%
                    select(x, y, r, Clasification.1) %>%
                    filter(abs(r) > .27) %>%
                    graph_from_data_frame(directed = FALSE)
                  
                  ggraph(BD2_cor, layout = "kk") +
                    geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 1.5) +
                    guides(edge_alpha = "none") +
                    scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff","#ff0000")) +
                    geom_node_point(color = "black", size = 3) +
                    geom_node_text(aes(label = name), repel = TRUE, size = 4) +
                    facet_edges( ~ Clasification.1)  +
                    theme_minimal() +
                    labs(title = "BMI CLASSIFICATION")
                  
                  
               #Clinical 
                  
                  Caridom_status <- DB_Polyphenols [c(-1:-6, -8:-55)]
                  Adipo_S <-data.frame(Adipo_scale)
                  Adip_clinical_scaled <-Adipo_S [c(-15:-18,-24:-45 )]
                  Caridom_status_clinical_scaled <- cbind(Caridom_status, Adip_clinical_scaled) #File to prisma
                  view(Adipo_scale)
                  #Output for prisma analysis
                  write.table(Caridom_status_clinical_scaled,file = "Output/Imputed/ZScore_Adipo_Clinical_Caridom_status_im.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  
              
              #Cardiometabolic status (CAGs)
                  
                  CAG_classification <- DB_Polyphenols [c(-1:-5, -7:-55)]
                  Adipo_S <-data.frame(Adipo_scale)
                  Adipo_S1 <-Adipo_S [c(-1:-23)]
                  CAG_clasification_scaled <- cbind(CAG_classification, Adipo_S1) [c(-3:-7)] #File to prisma
                  view(Adipo_scale)
                  #Output for prisma analysis
                  write.table(CAG_clasification_scaled,file = "Output/Imputed/ZScore_CAG_clasification_scaled_im.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  
                  CAG_clasification <- DB_Polyphenols [c(-1:-6, -8:-55)]
              

#OXILIPINAS
#####
#####                  
#BD_Oxylipins (ESTE PRIMER ANALISIS NO ELIMINO OTULIERS- VER M?S ABAJO)

               
                  #####
                     
                  DB_Oxy_Colciencias <- DB_Proy_Colc_Final_R [c(-1:-36, -46:-51,-53:-58,-60:-69, -72:-104, -111:-112, -119:-134)] [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92, -106, -117),]
                  colnames(DB_Proy_Colc_Final_R)
                  colnames(DB_Oxy_Colciencias)

                  #sum oxylipns
                  library(dplyr)
                    DBtotalIsoPs <- DB_Oxy_Colciencias %>% mutate(Total_15F2t = DB_Oxy_Colciencias$X15.F2t.IsoP +  DB_Oxy_Colciencias$X2.3.dinor.15.F2t.IsoP + DB_Oxy_Colciencias$X2.3.dinor.15.epi.15F2t.Isop + DB_Oxy_Colciencias$X15.keto.15.F2t.IsoP)
                    DBtotalIsoPs <- DBtotalIsoPs %>% mutate(PGD_M = DB_Oxy_Colciencias$X11.b.PGF2a + DB_Oxy_Colciencias$X2.3.dinor.11b.PGF2a)
                    DBtotalIsoPs <- DBtotalIsoPs %>% mutate(Total_5F2t = DB_Oxy_Colciencias$X5.F2t +  DB_Oxy_Colciencias$X5.epi.5.F2t)
                    DBtotalIsoPs <- DBtotalIsoPs %>% mutate(Total_PGs = DB_Oxy_Colciencias$Tetranor.PGFM +  DB_Oxy_Colciencias$X2.3.dinor.11b.PGF2a + DB_Oxy_Colciencias$PGE2 +  DB_Oxy_Colciencias$X11.b.PGF2a)
                    DBtotalIsoPs <- DBtotalIsoPs %>% mutate(Total_IsoPs = DB_Oxy_Colciencias$X15.F2t.IsoP + DB_Oxy_Colciencias$X15.epi.15.F2t + DB_Oxy_Colciencias$X2.3.dinor.15.F2t.IsoP + DB_Oxy_Colciencias$X2.3.dinor.15.epi.15F2t.Isop + DB_Oxy_Colciencias$X5.F2t +  DB_Oxy_Colciencias$X5.epi.5.F2t  +  DB_Oxy_Colciencias$X15.epi.15E2t.Isop + DB_Oxy_Colciencias$X15.F1t.IsoP + DB_Oxy_Colciencias$X15.keto.15.F2t.IsoP)
                    DBtotalIsoPs <- DBtotalIsoPs %>% mutate(Total_2.3_dinor_oxy = DB_Oxy_Colciencias$X2.3.dinor.11b.PGF2a + DB_Oxy_Colciencias$X2.3.dinor.15.epi.15F2t.Isop + DB_Oxy_Colciencias$X2.3.dinor.15.F2t.IsoP)
                    
                       
                    prueba <- DBtotalIsoPs$X2.3.dinor.11b.PGF2a[DBtotalIsoPs$X2.3.dinor.11b.PGF2a == "90.8247569"] <- "NA"
                    
                    
                    #PCA
                    
                    #Imputation. Seven values are under imputation (LDL: MI-078; PAS: MI:354; OXLDL: MI-201,MI-257,  MI-354, MI-384,MI-447)
                    DB_Info_Colciencias_ox_Im <- DBtotalIsoPs 
                    res.comp.DB_Info_Colcie_ox = imputePCA(DB_Info_Colciencias_ox_Im,ncp=10)
                    res.pca.DB_info_Colcie_ox = PCA(res.comp.DB_Info_Colcie_ox$completeObs)
                    view(res.pca.DB_info_Colcie_ox[["call"]][["X"]])
                    fviz_eig(res.pca.DB_info_Colcie_ox, addlabels = TRUE, ylim = c(0, 50))
                    
                    
                    Biplot_ox_IMC = fviz_pca_biplot(res.pca.DB_info_Colcie_ox, axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = BD_clinical_oxyl_colci_final$BMI_Classification, palette = c( "#0000ff", "#ffa500", "#ff0000", "#008000", "#8a2be2"), pointshape = 21, pointsize = 3, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
                    ggpubr::ggpar(Biplot_ox_IMC, title = "Principal component analysis", subtitle = "Oxylipins profile", caption = FALSE, xlab = "PC1(24.5%)", ylab = "PC2(5.7%)", legend.position = "top", legendsize = 8)
                    
                    
                    
                    # VARIABLES: VALOR P (Mide la significacia entre la variable y el componente principal)
                    
                    res.desc <- dimdesc(res.pca.DB_info_Colcie_ox, axes = c(1,2,3), proba = 0.05) #res.pca se optiene del codigo anterior en PCA
                    # Description of dimension 
                    res.desc$Dim.1
                    res.desc$Dim.2
                    res.desc$Dim.3
                    write.table(res.desc[["Dim.1"]][["quanti"]],file = "Output_oxylipins/Databases_oxylipins/Pvalue_PCA1.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    write.table(res.desc[["Dim.2"]][["quanti"]],file = "Output_oxylipins/Databases_oxylipins/Pvalue_PCA2.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    write.table(res.desc[["Dim.3"]][["quanti"]],file = "P_Value_PC3.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
                    Classifications_oxylipins_colciencias <- DB_Proy_Colc_Final_R [c(-1, -3,-4,-6, -18:-148)][c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92, -106, -117),]
                    BD_clinical_oxyl_colci_final <- cbind(Classifications_oxylipins_colciencias, DBtotalIsoPs) #File to prisma
                    write.table(BD_clinical_oxyl_colci_final , file = "Output_oxylipins/Databases_oxylipins/BD_clinical_oxyl_colci_final.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    #datos escalados
                    BD_clinical_oxyl_colci_Scal <- scale(DBtotalIsoPs)
                    BD_clinical_oxyl_colci_final_Scal <- cbind(Classifications_oxylipins_colciencias, BD_clinical_oxyl_colci_Scal) #File to prisma
                    write.table(BD_clinical_oxyl_colci_final_Scal , file = "Output_oxylipins/Databases_oxylipins/BD_clinical_oxyl_colci_final_Scal.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
                    #datos log10
                    BD_clinical_oxyl_colci_log <- log10(BD_clinical_oxyl_colci_final_For_log)
                    BD_clinical_oxyl_colci_final_log <- cbind(Classifications_oxylipins_colciencias, BD_clinical_oxyl_colci_log) #File to prisma
                    write.table(BD_clinical_oxyl_colci_final_log , file = "Output_oxylipins/Databases_oxylipins/BD_clinical_oxyl_colci_final_log.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
                    colnames(DBtotalIsoPs)
                    output_Oxili <- boxplot(DBtotalIsoPs)
                    
                    
                    write.table(output_Oxili[["group"]], file = "Output_oxylipins/Databases_oxylipins/output_Oxili_group.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    final_outlier <- cbind(output_Oxili[["out"]] + output_Oxili[["names"]])
                    
                    #0.0000000
                    #Base de datos con totales
                    write.table(DBtotalIsoPs, file = "Output_oxylipins/Databases_oxylipins/DB_Colci_Oxyl_with_totals.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
                    BDtotalIsoPs_scaled <- scale (BDtotalIsoPs)
                    write.table(BDtotalIsoPs_scaled,file = "BDtotalIsoPs_scaled.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    write.table(BDtotalIsoPs,file = "BDtotalIsoPs.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
    #An?lisis final  PAPER y resumen CONGRESO de obesidad (n= 105) (incluida la ELIMINACION DEL OUtLIER MI381) 
                    
                    
        ###OJO! (Adicion de nuevas variables a la BD)
                    
            #SI SE VA ADJUNTAR NUEVAMENTELA BD DB_Proy_Colc_Final_R PARA EL ANALISIS DE OXILIPINAS, DEBE COINCIDIR 
            #CON LAS VARIABLES QUE FINALMENTE FUERON SELECCIONADAS PARA DB_Oxy_Colciencias_Out (eJ. zONULINA ES NUEVA) 
                    
                    
                  
                    
                    setwd("/Users/ssojlara/D:")
                    setwd("/Users/ssojlara/D:/VIDARIUM-LAB/FoodTree/Rcodes")
                    colnames(DB_Proy_Colc_Final_R)
                    
                #DATASETS----
                    
                    #1.DB_Proy_Colc_Final_R  # Complete database from colciencias Proyect "DB_Proy_Colc_Final_R.csv"
                    colnames(DB_Proy_Colc_Final_R)
                    
                    #2. MiSalud.phenol.otu_Oxyl (Generada a partir de la DB "MiSalud.phenol.otu2_All_MiSalud_n_ajusted by Kg". Estos datos fueron organizados directamente del an?lisis completo de MiSalud del Phenol Tree y donde se hizo el ajuste por Kg (peso corporal).
                    colnames(MiSalud.phenol.otu_Oxyl)
                    
                    #3. DB_Oxy_Colciencias_Out (Database filtered and generated for n=105)
                    DB_Oxy_Colciencias_Out <- DB_Proy_Colc_Final_R [c(-1:-36, -46:-51,-53:-58,-60:-69, -72:-104, -111:-112, -119:-134)] [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92,-97, -106, -117),]#MI381 outlier
                    colnames(DB_Proy_Colc_Final_R)
                    
                    #4. DBtotalOxylipins 
                      #Addition -> sum of oxylipns (Totals)
                      library(dplyr)
                      DBtotalOxylipins <- DB_Oxy_Colciencias_Out %>% mutate(Total_15F2t = DB_Oxy_Colciencias_Out$X15.F2t.IsoP + DB_Oxy_Colciencias_Out$X15.epi.15.F2t +  DB_Oxy_Colciencias_Out$X2.3.dinor.15.F2t.IsoP + DB_Oxy_Colciencias_Out$X2.3.dinor.15.epi.15F2t.Isop + DB_Oxy_Colciencias_Out$X15.keto.15.F2t.IsoP)
                      DBtotalOxylipins <- DBtotalOxylipins %>% mutate(PGD_M = DB_Oxy_Colciencias_Out$X11.b.PGF2a + DB_Oxy_Colciencias_Out$X2.3.dinor.11b.PGF2a)
                      DBtotalOxylipins <- DBtotalOxylipins %>% mutate(Total_5F2t = DB_Oxy_Colciencias_Out$X5.F2t +  DB_Oxy_Colciencias_Out$X5.epi.5.F2t)
                      DBtotalOxylipins <- DBtotalOxylipins %>% mutate(Total_PGs = DB_Oxy_Colciencias_Out$Tetranor.PGFM +  DB_Oxy_Colciencias_Out$X2.3.dinor.11b.PGF2a + DB_Oxy_Colciencias_Out$PGE2 +  DB_Oxy_Colciencias_Out$X11.b.PGF2a)
                      DBtotalOxylipins <- DBtotalOxylipins %>% mutate(Total_IsoPs = DB_Oxy_Colciencias_Out$X15.F2t.IsoP + DB_Oxy_Colciencias_Out$X15.epi.15.F2t + DB_Oxy_Colciencias_Out$X2.3.dinor.15.F2t.IsoP + DB_Oxy_Colciencias_Out$X2.3.dinor.15.epi.15F2t.Isop + DB_Oxy_Colciencias_Out$X5.F2t +  DB_Oxy_Colciencias_Out$X5.epi.5.F2t  +  DB_Oxy_Colciencias_Out$X15.epi.15E2t.Isop + DB_Oxy_Colciencias_Out$X15.F1t.IsoP + DB_Oxy_Colciencias_Out$X15.keto.15.F2t.IsoP)
                      DBtotalOxylipins <- DBtotalOxylipins %>% mutate(Total_2.3_dinor_oxy = DB_Oxy_Colciencias_Out$X2.3.dinor.11b.PGF2a + DB_Oxy_Colciencias_Out$X2.3.dinor.15.epi.15F2t.Isop + DB_Oxy_Colciencias_Out$X2.3.dinor.15.F2t.IsoP)
                      
                          #4.1 Oxy_scale_105 <- scale(DBtotalOxylipins) #Scaling
                          write.table(Oxy_scale_105, file = "Output_oxylipins/Databases_oxylipins/BD_clinical_oxyl_colci_final_scale_105.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                      
                          
                          #4.2 BMI_oxy_scaled_d (Oxylipins only For BMI Clasification)  
                          BMI_oxy_scaled <- cbind(Classifications_oxylipins_colciencias_out$BMI_Classification, Oxy_scale_105) #File to prisma
                          view(Adipo_scale)
                          BMI_oxy_scaled_d <- data.frame(BMI_oxy_scaled)
                          #Output for prisma analysis
                          

                          #5. BD_clinical_oxyl_colci_final_outl (all Categoric variables and oxylipins)
                          Classifications_oxylipins_colciencias_out <- DB_Proy_Colc_Final_R [c(-1, -3,-4,-6, -18:-148)][c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92,-97, -106, -117),]
                          BD_clinical_oxyl_colci_final_outl <- cbind(Classifications_oxylipins_colciencias_out, DBtotalOxylipins) #File to prisma
                          write.table(BD_clinical_oxyl_colci_final_outl , file = "Output_oxylipins/Databases_oxylipins/BD_clinical_oxyl_colci_final_outl.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
                    #5. BD_clinical_oxyl_colci_final_phenol_JSE (Work database for this paper))
                              
                      #Base de datos (DEL PAPER) JSE para an?lisis de ajustes + adicion de polifenoles de la dieta + adci?n de fibra + adici?n de AOX analizados por Evindi
                      # Ajuntar la base de datos MiSalud.phenol.otu_Oxyl -> Esta base de datos fue construida a partir de la base de datos original del analisis de polifenoles y dieta de todo Mi Salud "MiSalud.phenol.otu2_All_MiSalud_n_ajusted by Kg"
                      DB_Oxy_Colciencias_catego <- DB_Proy_Colc_Final_R [c(-37:-148)] [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92,-97, -106, -117),]#MI381 outlier
                      BD_clinical_oxyl_colci_final_phenol_JSE <- cbind(DB_Oxy_Colciencias_catego, DBtotalOxylipins, MiSalud.phenol.otu_Oxyl [c(-1)],MiSalud.Aox_n_microelements_Oxyl) 
                      write.table(BD_clinical_oxyl_colci_final_phenol_JSE , file = "Output_oxylipins/Databases_oxylipins/BD_clinical_oxyl_colci_final_phenol_JSE2.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

                      
                        #Optional without polyphenols intake
                              DB_Oxy_Colciencias_catego <- DB_Proy_Colc_Final_R [c(-37:-148)] [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92,-97, -106, -117),]#MI381 outlier
                              BD_clinical_oxyl_colci_final_JSE <- cbind(DB_Oxy_Colciencias_catego, DBtotalOxylipins) 
                              write.table(BD_clinical_oxyl_colci_final_JSE , file = "Output_oxylipins/Databases_oxylipins/BD_clinical_oxyl_colci_final_JSE.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
                    
                #FIGURE PCA (Figure 2 Paper oxylipins MiSalud)----
                    
                    #For this analysis, we excluded phenols from eggs and meet (Value =0)
                    
                      # (A) Without exclusion of variables that defined the sample (BMI, weight and body size)
                      Oxylipins_vs_phenols_105 <- BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-36,-41:-46,-48:-56,-82:-91, -94, -95, -103:-117)]
                      colnames(BD_clinical_oxyl_colci_final_phenol_JSE)
                      
                      # (B) exclusion of variables that defined the sample (BMI, weight and body size)
                      #Final paper figure
                      Oxylipins_vs_phenols_105b <- BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-39,-82:-91, -94, -95, -103:-117)]# sin fenoles dle huevo y carne
                      colnames(Oxylipins_vs_phenols_105b)
                    

                          #PCA (A)
                              Oxylipins_vs_phenols_105_scale <- scale (Oxylipins_vs_phenols_105) #scaling
                              
                              res.pca <- PCA(Oxylipins_vs_phenols_105, scale.unit = TRUE, ncp = 10, graph = FALSE)
                              fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

                              Biplot_Oxylipins_Phenols = fviz_pca_biplot(res.pca, axes = c(2,3), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification, palette = c( "#0000ff", "#ffa500", "#ff0000", "#008000", "#8a2be2"), pointshape = 21, pointsize = 2, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
                              ggpubr::ggpar(Biplot_Oxylipins_Phenols, title = "Principal component analysis", subtitle = "Oxylipins_phenols profile", caption = FALSE, xlab = "PC1(22.8%)", ylab = "PC2(18.5%)", legend.position = "top", legendsize = 8)
                            
                              # VARIABLES: VALOR P (Mide la significacia entre la variable y el componente principal)
                              
                              res.desc3 <- dimdesc(res.pca, axes = c(1,2,3), proba = 0.05) #res.pca se optiene del codigo anterior en PCA
                              # Description of dimension 
                              res.desc3$Dim.1
                              res.desc3$Dim.2
                              res.desc3$Dim.3
                    
                          #PCA (B) - (Figure Final)
                              Oxylipins_vs_phenols_105b_scale <- scale (Oxylipins_vs_phenols_105b)
                              
                              res.pca1 <- PCA(Oxylipins_vs_phenols_105b, scale.unit = TRUE, ncp = 10, graph = FALSE)
                              fviz_eig(res.pca1, addlabels = TRUE, ylim = c(0, 50))
                              
                              
                                Biplot_Oxylipins_Phenols1 = fviz_pca_biplot(res.pca1, axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification, palette = c( "#0000ff", "#ffa500", "#ff0000", "#008000", "#8a2be2"), pointshape = 21, pointsize = 2, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
                              ggpubr::ggpar(Biplot_Oxylipins_Phenols1, title = "Principal component analysis", subtitle = "Oxylipins_phenols profile", caption = FALSE, xlab = "PC1(18.4%)", ylab = "PC2(15.4%)", legend.position = "top", legendsize = 8)
                              
                              # VARIABLES: VALOR P (Mide la significacia entre la variable y el componente principal)
                              
                              res.desc1 <- dimdesc(res.pca1, axes = c(1,2,3), proba = 0.05) #res.pca se optiene del codigo anterior en PCA
                              # Description of dimension 
                              res.desc1$Dim.1
                              res.desc1$Dim.2
                              res.desc1$Dim.3
                        
                    
                #Supplementary Figure of correlation- Matrix and correlation network----
                              colnames(Oxylipins_vs_phenols_105_scale)
                              
                              tidy_cors <- Oxylipins_vs_phenols_105_scale %>% 
                                correlate() %>% 
                                stretch()
                              
                              # Convert correlations stronger than some value
                              # to an undirected graph object
                              
                              
                              graph_cors <- tidy_cors %>% 
                                filter(abs(r) > 0.25) %>%
                                graph_from_data_frame(directed = FALSE)
                              
                              ggraph(graph_cors) +
                                geom_edge_link() +
                                geom_node_point() +
                                geom_node_text(aes(label = name))
                              
                              ggraph(graph_cors) +
                                geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +  #cambie el edge_width = abs(r) por none
                                guides(edge_alpha = "none", edge_width = "none") +
                                scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff", "#fd240b")) + 
                                geom_node_point(color = "black", size = 3) +
                                geom_node_text(aes(label = name),size = 4, repel = TRUE) +
                                theme_graph() +
                                labs(title = "Global correlation (n=105)")
                              
 
                #STATs BMI (Tables #1-4, and Table S3 paper oxylipins MiSalud)----  
                      
                      #Database -> #5. BD_clinical_oxyl_colci_final_phenol_JSE
                              
                              #BMI_Classification or BMI (Lean vs Overweight vs Obese)
                              colnames(BD_clinical_oxyl_colci_final_phenol_JSE)  
                              
                          #Stats normality test
                              
                              #BMI
                              BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification
                              BMI_lean_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-9, -11:-36)], BMI_Classification == "1.Lean") 
                              BMI_Overweight_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-9, -11:-36)], BMI_Classification == "2.Overweight") 
                              BMI_Obese_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-9, -11:-36)], BMI_Classification == "3.Obese") 
                              DB_oxy_BMI <- rbind(BMI_lean_oxyl_normality,BMI_Overweight_oxyl_normality,BMI_Obese_oxyl_normality)
                              write.table(DB_oxy_BMI, file = "Output_oxylipins/Databases_oxylipins/DB_oxy_BMI.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              #Shapiro
                              #Sin la variable categorica y fenoles y fibrar para huevos,carnes y grasas (los valores en 0 no dejan dejan correr todo el shapiro)
                              BMI_lean_oxyl_normality_shapiro <- lapply(BMI_lean_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test) 
                              BMI_Overweight_oxyl_normality_shapiro <- lapply(BMI_Overweight_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test)
                              BMI_Obese_oyxl_normality_shapiro <- lapply(BMI_Obese_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test)
                              Shapiro_BMI_oxyl_Colciencias <- cbind(BMI_lean_oxyl_normality_shapiro,BMI_Overweight_oxyl_normality_shapiro,BMI_Obese_oyxl_normality_shapiro)
                              write.table(Shapiro_BMI_oxyl_Colciencias, file = "Output_oxylipins/Databases_oxylipins/Shapiro_BMI_oxyl_Colciencias.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 


                              #The following variables passed shapiro. Therefore, the distribution of the residuals will be analyzed by a QQ-plot. Find each variable to check
                              
                              #QQplot_BMI
                              variables_names_BMI_oxyl_ForQQplot<- dput(colnames(select_if(BMI_lean_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], is.numeric)))
                              
                                      #lean
                                      lean_oxyl_QQplot <- lapply(variables_names_BMI_oxyl_ForQQplot, function(item) {
                                        ggqqplot(BMI_lean_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = TRUE) +
                                          ggtitle(item)
                                      }
                                      )
                                      
                                      #Normal distribution shapiro -lEAN
                                      lean_QQplot[9]
                                      lean_QQplot[12]
                                      lean_QQplot[14]
                                      lean_QQplot[15]
                                      lean_QQplot[19]
                                      lean_QQplot[42]
                                      lean_QQplot[45]
                                      lean_QQplot[72]
                                      
                                      #overweight
                                      Overweight_Oxyl_QQplot <- lapply(variables_names_BMI_oxyl_ForQQplot, function(item) {
                                        ggqqplot(BMI_Overweight_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = FALSE) +
                                          ggtitle(item)
                                      }
                                      )
                                      #Normal distribution shapiro -Overwight
                                      Overweight_QQplot[9]
                                      Overweight_QQplot[12]
                                      Overweight_QQplot[14]
                                      Overweight_QQplot[15]
                                      Overweight_QQplot[19]
                                      Overweight_QQplot[42]
                                      Overweight_QQplot[45]
                                      Overweight_QQplot[72]
                                      
                                      #Obese
                                      Obese_Oxyl_QQplot <- lapply(variables_names_BMI_oxyl_ForQQplot, function(item) {
                                        ggqqplot(BMI_Obese_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = FALSE) +
                                          ggtitle(item)
                                      }
                                      )
                                      #Normal distribution shapiro- Obese
                                      Obese_QQplot[9]
                                      Obese_QQplot[12]
                                      Obese_QQplot[14]
                                      Obese_QQplot[15]
                                      Obese_QQplot[19]
                                      Obese_QQplot[42]
                                      Obese_QQplot[45]
                                      Obese_QQplot[72]
                              

                          # NON-Parametric (Kruskall)
                              Kruskal_BMI <- lapply(colnames(BD_clinical_oxyl_colci_final_phenol_JSE[37:123]),
                                                    function(x) kruskal.test(BD_clinical_oxyl_colci_final_phenol_JSE[,x],BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification,BD_clinical_oxyl_colci_final_phenol_JSE))
                              
                              pval_BMI <- sapply(Kruskal_BMI, function(x) x$p.value)
                              write.table(pval_BMI,file = "Output_oxylipins/Databases_oxylipins/Normality_test/Kruskal_by_BMI.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                                #In this file is necessary add manually all variables name from BD_clinical_oxyl_colci_final_phenol_JSE 
                              
                              
                          #q-values (P value Adjusted)
                              table_All_Pvalues_BMI <- list(pval_BMI)
                              
                              # import data
                              p_values_BMI <- table_All_Pvalues_BMI[[1]]
                              
                              # get q-value object
                              qobj_BMI <- qvalue(p_values_BMI)
                              plot(qobj)
                              hist(qobj)
                              
                              # Export q-value object
                              write.table(qobj_BMI[["qvalues"]],file = "Output_oxylipins/Databases_oxylipins/qvalues_BMI_alltables_1_to_4.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

                                    #e.g. By each variable
                                    kruskal.test(BD_clinical_oxyl_colci_final_phenol_JSE$LDL ~ BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification, BD_clinical_oxyl_colci_final_phenol_JSE)
                              
                          #perform Dunn's Test with Bonferroni correction for p-values
                              
                              #All variables
                              dunnTest_BMI <- lapply(colnames(BD_clinical_oxyl_colci_final_phenol_JSE[37:123]),
                                                     function(x) dunnTest(BD_clinical_oxyl_colci_final_phenol_JSE[,x],BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification,BD_clinical_oxyl_colci_final_phenol_JSE,method="bonferroni"))

                              #By each variable (Replace the interest variable)
                              dunnTest(BD_clinical_oxyl_colci_final_phenol_JSE$LDL ~ BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification, BD_clinical_oxyl_colci_final_phenol_JSE, method="bonferroni")
                              #These analysis is more simple in prisma graphpad (see -> Estadistica descriptiva_Oxilipinas_crudos n=105.pzfx)
                              
                              
                              
                          # ANOVA estimated from multiple linear regression (MLR). The values of selected variables were log-transformed and adjusted for potential confounders: age range, participants' city of origin, and sex at birth.
                              
                              #BMI_Classification (BMI)
                              colnames(BD_clinical_oxyl_colci_final_phenol_JSE) #Database #5
                              
                              #NOte: Before multiple linear model analysis, a constant was added to variables with zero values (less than 5% of all variables analyzed) to avoid infinite values in the transformation.
                            
                            #CLINICAL VARIABLES 
                              
                              #BMI
                              Oxy_BMI_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$BMI,2)
                              
                              modelo_adj_Oxy_BMI_log <-lm(Oxy_BMI_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_BMI_log<- Anova(modelo_adj_Oxy_BMI_log)
                              
                              #weight
                              Oxy_Weight_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Weight,2)
                              modelo_adj_Oxy_Weight_log <-lm(Oxy_Weight_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Weight_log<- Anova(modelo_adj_Oxy_Weight_log)
                              
                              #wAIST CIRCUNFERENCE
                              Oxy_Waist.Circ_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Waist.Circ,2)
                              modelo_adj_Oxy_Waist.Circ_log <-lm(Oxy_Waist.Circ_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Waist.Circ_log<- Anova(modelo_adj_Oxy_Waist.Circ_log)
                              
                              #BODY SIZE
                              Oxy_Body_size_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Body_size,2)
                              modelo_adj_Oxy_Body_size_log <-lm(Oxy_Body_size_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Body_size_log<- Anova(modelo_adj_Oxy_Body_size_log)
                              
                              
                              #ApoB
                              ApoB<- log(BD_clinical_oxyl_colci_final_phenol_JSE$ApoB,2)
                              modelo_adj_ApoB_log <-lm(ApoB~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                         BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                         BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_ApoB<- Anova(modelo_adj_ApoB_log)
                              
                              
                              #HDL
                              Oxy_HDL_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$HDL,2)
                              modelo_adj_Oxy_HDL_log <-lm(Oxy_HDL_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_HDL_log<- Anova(modelo_adj_Oxy_HDL_log)
                              
                              
                              #VLDL
                              Oxy_VLDL_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$VLDL,2)
                              modelo_adj_Oxy_VLDL_log <-lm(Oxy_VLDL_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_VLDL_log<- Anova(modelo_adj_Oxy_VLDL_log)
                              
                              
                              #LDL
                              Oxy_LDL_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$LDL,2)
                              modelo_adj_Oxy_LDL_log <-lm(Oxy_LDL_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_LDL_log<- Anova(modelo_adj_Oxy_LDL_log)
                              
                              
                              #TC
                              Oxy_TC_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$TC,2)
                              modelo_adj_Oxy_TC_log <-lm(Oxy_TC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_TC_log<- Anova(modelo_adj_Oxy_TC_log)
                              
                              #TGs
                              Oxy_TG_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$TG,2)
                              modelo_adj_Oxy_TG_log <-lm(Oxy_TG_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_TG_log<- Anova(modelo_adj_Oxy_TG_log)
                              
                              #Body.Fat
                              Oxy_Body.Fat_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Body.Fat,2)
                              
                              modelo_adj_Oxy_Body.Fat_log <-lm(Oxy_Body.Fat_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Body.Fat_log<- Anova(modelo_adj_Oxy_Body.Fat_log)
                              
                              #SBP
                              Oxy_SPB_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$SBP,2)
                              modelo_adj_Oxy_SPB_log <-lm(Oxy_SPB_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_SBP_log<- Anova(modelo_adj_Oxy_SPB_log)
                              
                              
                              #DBP
                              Oxy_DBP_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$DBP,2)
                              modelo_adj_Oxy_DBP_log <-lm(Oxy_DBP_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_DBP_log<- Anova(modelo_adj_Oxy_DBP_log)
                              
                              #Glucose
                              Oxy_Glucose_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Glucose,2)
                              modelo_adj_Oxy_Glucose_log <-lm(Oxy_Glucose_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Glucose_log<- Anova(modelo_adj_Oxy_Glucose_log)
                              
                              
                              
                              #INSULINE
                              Oxy_Insulin_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Insulin,2)
                              modelo_adj_Oxy_Insulin_log <-lm(Oxy_Insulin_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Insulin_log<- Anova(modelo_adj_Oxy_Insulin_log)
                              
                              
                              #b.cell.func
                              Oxy_b.cell.func_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$b.cell.func,2)
                              modelo_adj_Oxy_b.cell.func_log <-lm(Oxy_b.cell.func_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_b.cell.func_log<- Anova(modelo_adj_Oxy_b.cell.func_log)
                              
                              #Insulin sensibility
                              Oxy_IS_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$IS,2)
                              modelo_adj_Oxy_IS_log <-lm(Oxy_IS_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_IS_log<- Anova(modelo_adj_Oxy_IS_log)
                              
                              #Insulin Resistance (HOMA IR)
                              Oxy_IR_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$IR,2)
                              modelo_adj_Oxy_IR_log <-lm(Oxy_IR_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_IR_log<- Anova(modelo_adj_Oxy_IR_log)
                              
                              
                              #LBP
                              Oxy_LBP_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$LBP,2)
                              modelo_adj_Oxy_LBP_log <-lm(Oxy_LBP_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_LBP_log<- Anova(modelo_adj_Oxy_LBP_log)
                              
                              #CAG- prevotella
                              Oxy_Prevotella_log<- log(0.000001+BD_clinical_oxyl_colci_final_phenol_JSE$Prevotella,2)
                              modelo_adj_Oxy_Prevotella_log <-lm(Oxy_Prevotella_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Prevotella_log<- Anova(modelo_adj_Oxy_Prevotella_log)
                              
                              #CAG- lachnospiraceae
                              Oxy_Lachnospiraceae_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Lachnospiraceae,2)
                              modelo_adj_Oxy_Lachnospiraceae_log <-lm(Oxy_Lachnospiraceae_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Lachnospiraceae_log<- Anova(modelo_adj_Oxy_Lachnospiraceae_log)

                              #CAG- pathogen
                              Oxy_Pathogen_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Pathogen,2)
                              modelo_adj_Oxy_Pathogen_log <-lm(Oxy_Pathogen_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Pathogen_log<- Anova(modelo_adj_Oxy_Pathogen_log)
                              
                              #CAG- Akkermansia
                              Oxy_Akkermansia_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Akkermansia,2)
                              modelo_adj_Oxy_Akkermansia_log <-lm(Oxy_Akkermansia_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Akkermansia_log<- Anova(modelo_adj_Oxy_Akkermansia_log)
                              
                              
                              #CAG- Ruminococcaceae
                              Oxy_Ruminococcaceae_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Ruminococcaceae,2)
                              modelo_adj_Oxy_Ruminococcaceae_log <-lm(Oxy_Ruminococcaceae_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Oxy_Ruminococcaceae_log<- Anova(modelo_adj_Oxy_Ruminococcaceae_log)
                              
                            #OXYLIPINS
                              
                              #tetranor-PGFM
                              Oxy_Tetranor.PGFM_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Tetranor.PGFM,2)
                              modelo_adj_Oxy_Tetranor.PGFM_log <-lm(Oxy_Tetranor.PGFM_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Tetranor.PGFM_log<- Anova(modelo_adj_Oxy_Tetranor.PGFM_log)

                              #2.3.dinor.11b.PGF2a
                              Oxy_2.3.dinor.11b.PGF2a_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$X2.3.dinor.11b.PGF2a,2)
                              modelo_adj_Oxy_2.3.dinor.11b.PGF2a_log <-lm(Oxy_2.3.dinor.11b.PGF2a_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_2.3.dinor.11b.PGF2a_log<- Anova(modelo_adj_Oxy_2.3.dinor.11b.PGF2a_log)

                              #PGE2
                              Oxy_PGE2_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$PGE2,2)
                              modelo_adj_Oxy_PGE2_log <-lm(Oxy_PGE2_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_PGE2_log<- Anova(modelo_adj_Oxy_PGE2_log)
                              
                              #11B-PGF2a
                              Oxy_X11.b.PGF2a_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$X11.b.PGF2a,2)
                              modelo_adj_Oxy_X11.b.PGF2a_log <-lm(Oxy_X11.b.PGF2a_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_X11.b.PGF2a_log<- Anova(modelo_adj_Oxy_X11.b.PGF2a_log)
                              
                              #15F2t-IsoP
                              Oxy_X15.F2t.IsoP_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$X15.F2t.IsoP,2)
                              modelo_adj_Oxy_X15.F2t.IsoP_log <-lm(Oxy_X15.F2t.IsoP_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                     BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                     BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_X15.F2t.IsoP_log<- Anova(modelo_adj_Oxy_X15.F2t.IsoP_log)

                              #15-epi-15-F2t
                              Oxy_X15.epi.15.F2t_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$X15.epi.15.F2t,2)
                              modelo_adj_Oxy_X15.epi.15.F2t_log <-lm(Oxy_X15.epi.15.F2t_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                       BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                       BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_X15.epi.15.F2t_log<- Anova(modelo_adj_Oxy_X15.epi.15.F2t_log)

                              #2.3.dinor.15.epi.15F2t.Isop
                              Oxy_2.3.dinor.15.epi.15F2t.Isop_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$X2.3.dinor.15.epi.15F2t.Isop,2)
                              modelo_adj_Oxy_2.3.dinor.15.epi.15F2t.Isop_log <-lm(Oxy_2.3.dinor.15.epi.15F2t.Isop_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_2.3.dinor.15.epi.15F2t.Isop_log<- Anova(modelo_adj_Oxy_2.3.dinor.15.epi.15F2t.Isop_log)

                              #2.3.dinor.15.epi.15F2t.Isop
                              Oxy_2.3.dinor.15.F2t.IsoP_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$X2.3.dinor.15.F2t.IsoP,2)
                              modelo_adj_Oxy_2.3.dinor.15.F2t.IsoP_log <-lm(Oxy_2.3.dinor.15.F2t.IsoP_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                              BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_2.3.dinor.15.F2t.IsoP_log<- Anova(modelo_adj_Oxy_2.3.dinor.15.F2t.IsoP_log)

                              #X5.F2t
                              Oxy_X5.F2t_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$X5.F2t,2)
                              modelo_adj_Oxy_X5.F2t_log <-lm(Oxy_X5.F2t_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_X5.F2t_log<- Anova(modelo_adj_Oxy_X5.F2t_log)
                              #X5.epi-5F2t
                              Oxy_X5.epi.5.F2t_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$X5.epi.5.F2t,2)
                              modelo_adj_Oxy_X5.epi.5.F2t_log <-lm(Oxy_X5.epi.5.F2t_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                     BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                     BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_X5.epi.5.F2t_log<- Anova(modelo_adj_Oxy_X5.epi.5.F2t_log)

                              #X15.epi.15E2t.Isop
                              Oxy_X15.epi.15E2t.Isop_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$X15.epi.15E2t.Isop,2)
                              
                              modelo_adj_Oxy_X15.epi.15E2t.Isop_log <-lm(Oxy_X15.epi.15E2t.Isop_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_X15.epi.15E2t.Isop_log<- Anova(modelo_adj_Oxy_X15.epi.15E2t.Isop_log)
                              
                              
                              #X11.DH.TXB2
                              Oxy_X11.DH.TXB2_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$X11.DH.TXB2,2)
                              modelo_adj_Oxy_X11.DH.TXB2_log <-lm(Oxy_X11.DH.TXB2_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_X11.DH.TXB2_log<- Anova(modelo_adj_Oxy_X11.DH.TXB2_log)

                              #15-F1t-IsoP
                              Oxy_15.F1t.IsoP_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$X15.F1t.IsoP,2)
                              modelo_adj_Oxy_15.F1t.IsoP_log <-lm(Oxy_15.F1t.IsoP_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_15.F1t.IsoP_log<- Anova(modelo_adj_Oxy_15.F1t.IsoP_log)
                              
                              #15-keto-15-F2t-IsoP
                              Oxy_X15.keto.15.F2t.IsoP_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$X15.keto.15.F2t.IsoP,2)
                              modelo_adj_Oxy_X15.keto.15.F2t.IsoP_log <-lm(Oxy_X15.keto.15.F2t.IsoP_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                             BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                             BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_15.keto.15.F2t.IsoP_log<- Anova(modelo_adj_Oxy_X15.keto.15.F2t.IsoP_log)

                              #Total IsoPs
                              Oxy_Total_IsoPs_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_IsoPs,2)
                              modelo_adj_Oxy_Total_IsoPs_log <-lm(Oxy_Total_IsoPs_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_IsoPs_log<- Anova(modelo_adj_Oxy_Total_IsoPs_log)

                              #Total 15F2t
                              Oxy_Total_15F2t_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_15F2t,2)
                              modelo_adj_Oxy_Total_15F2t_log <-lm(Oxy_Total_15F2t_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_15F2t_log<- Anova(modelo_adj_Oxy_Total_15F2t_log)
                              
                              #Total 5F2t
                              Oxy_Total_5F2t_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_5F2t,2)
                              modelo_adj_Oxy_Total_5F2t_log <-lm(Oxy_Total_5F2t_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_5F2t_log<- Anova(modelo_adj_Oxy_Total_5F2t_log)
                              
                              #Total PGs
                              Oxy_Total_PGs_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_PGs,2)
                              modelo_adj_Oxy_Total_PGs_log <-lm(Oxy_Total_PGs_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_PGs_log<- Anova(modelo_adj_Oxy_Total_PGs_log)
                              
                              #Total PGD metabolites
                              Oxy_PGD_M_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$PGD_M,2)
                              modelo_adj_Oxy_PGD_M_log <-lm(Oxy_PGD_M_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_PGD_M_log<- Anova(modelo_adj_Oxy_PGD_M_log)
                              
                              #Total 2,3-dinor Oxylipins
                              Oxy_Total_2.3_dinor_oxy_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_2.3_dinor_oxy,2)
                              modelo_adj_Oxy_Total_2.3_dinor_oxy_log <-lm(Oxy_Total_2.3_dinor_oxy_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_2.3_dinor_oxy_log<- Anova(modelo_adj_Oxy_Total_2.3_dinor_oxy_log)
                              
                              #Total 2,3-dinor Oxylipins ajusted by polyphenols intake
                              Fenoles_vs_TotalDinorOxy <-  lm(Oxy_Total_2.3_dinor_oxy_log ~ BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification +
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$Total_Phenols_kg)
                              Anova_adj_modelo_Fenoles_vs_TotalDinorOxyy_log<- Anova(Fenoles_vs_TotalDinorOxy)
                              
                              #oxLDL
                              Oxy_oxLDL_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$oxLDL,2)
                              modelo_adj_Oxy_oxLDL_log <-lm(Oxy_oxLDL_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Total_Phenols_kg)
                              Anova_modelo_adj_Oxy_oxLDL_log<- Anova(modelo_adj_Oxy_oxLDL_log)

                            #Total_Phenols_FC (intake by each FNNDS category)
                              
                              #Total polyphenos intake
                              Oxy_Total_Phenols_FC_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_Phenols_FC,2)
                              modelo_adj_Oxy_Total_Phenols_FC_log <-lm(Oxy_Total_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                         BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                         BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Phenols_FC_log<- Anova(modelo_adj_Oxy_Total_Phenols_FC_log)
                              
                              #Milk_Phenols_FC
                              Oxy_Milk_Phenols_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Milk_Phenols_FC,2)
                              modelo_adj_Oxy_Milk_Phenols_FC_log <-lm(Oxy_Milk_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Milk_Phenols_FC_log<- Anova(modelo_adj_Oxy_Milk_Phenols_FC_log)
                              
                              
                              #Meat_Phenols_FC
                              Oxy_Meats_Phenols_FC_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Meats_Phenols_FC,2)
                              modelo_adj_Oxy_Meats_Phenols_FC_log <-lm(Oxy_Meats_Phenols_FC_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                         BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                         BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Meats_Phenols_FC_log<- Anova(modelo_adj_Oxy_Meats_Phenols_FC_log)
                              
                              
                              #Legumes
                              Oxy_Legumes_Phenols_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Legumes_Phenols_FC,2)
                              modelo_adj_Oxy_Legumes_Phenols_FC_log <-lm(Oxy_Legumes_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Legumes_Phenols_FC_log<- Anova(modelo_adj_Oxy_Legumes_Phenols_FC_log)
                              
                              
                              #Grains
                              Oxy_Grains_Phenols_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Grains_Phenols_FC,2)
                              modelo_adj_Grains_Phenols_FC_log <-lm(Oxy_Grains_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Grains_Phenols_FC_log<- Anova(modelo_adj_Grains_Phenols_FC_log)
                              
                              
                              #Fruits
                              Oxy_Fruits_Phenols_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Fruits_Phenols_FC,2)
                              modelo_adj_Fruits_Phenols_FC_log <-lm(Oxy_Fruits_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Fruits_Phenols_FC_log<- Anova(modelo_adj_Fruits_Phenols_FC_log)
                              
                              #Vegetables
                              Oxy_Vegetables_Phenols_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Vegetables_Phenols_FC,2)
                              modelo_adj_Vegetables_Phenols_FC_log <-lm(Oxy_Vegetables_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Vegetables_Phenols_FC_log<- Anova(modelo_adj_Vegetables_Phenols_FC_log)
                              
                              #Fat
                              Oxy_Fats_Phenols_FC_log<- log(0.0001+BD_clinical_oxyl_colci_final_phenol_JSE$Fats_Phenols_FC,2)
                              modelo_adj_Fats_Phenols_FC_log <-lm(Oxy_Fats_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Fats_Phenols_FC_log<- Anova(modelo_adj_Fats_Phenols_FC_log)

                              #Sweets and beverages
                              Oxy_Sweets_n_beverages_Phenols_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Sweets_n_beverages_Phenols_FC,2)
                              modelo_adj_Oxy_Sweets_n_beverages_Phenols_FC_log <-lm(Oxy_Sweets_n_beverages_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Sweets_n_beverages_Phenols_FC_log<- Anova(modelo_adj_Oxy_Sweets_n_beverages_Phenols_FC_log)
                              
                            #Total_Phenols_FC adjusted by body weight (intake/kg in each FNNDS category)
                              
                              #Total intake polyphenols/kg
                              Oxy_Total_Phenols_kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_Phenols_kg,2)
                              modelo_adj_Total_Phenols_kg_log <-lm(Oxy_Total_Phenols_kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                     BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                     BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Phenols_kg_log<- Anova(modelo_adj_Total_Phenols_kg_log)
                              
                              #Milk_Phenols/kG
                              Oxy_Milk_Phenols_Kg_log<- log(0.001+BD_clinical_oxyl_colci_final_phenol_JSE$Milk_Phenols_Kg,2)
                              modelo_adj_Oxy_Milk_Phenols_Kg_log <-lm(Oxy_Milk_Phenols_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Milk_Phenols_Kg_log<- Anova(modelo_adj_Oxy_Milk_Phenols_Kg_log)
                              
                              #Meats_Phenols/kG
                              Oxy_Meats_Phenols_Kg_log<- log(0.001+BD_clinical_oxyl_colci_final_phenol_JSE$Meats_Phenols_Kg,2)
                              modelo_adj_Oxy_Meats_Phenols_Kg_log <-lm(Oxy_Meats_Phenols_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                         BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                         BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Meats_Phenols_Kg_log<- Anova(modelo_adj_Oxy_Meats_Phenols_Kg_log)

                              #Legumes/kG
                              Oxy_Legumes_Phenols_Kg_log<- log(0.001+BD_clinical_oxyl_colci_final_phenol_JSE$Legumes_Phenols_Kg,2)
                              modelo_adj_Oxy_Legumes_Phenols_Kg_log <-lm(Oxy_Legumes_Phenols_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Legumes_Phenols_Kg_log<- Anova(modelo_adj_Oxy_Legumes_Phenols_Kg_log)

                              #Grains/Kg
                              Oxy_Grains_Phenols_Kg_log<- log(0.001+BD_clinical_oxyl_colci_final_phenol_JSE$Grains_Phenols_Kg,2)
                              modelo_adj_Oxy_Grains_Phenols_Kg_log <-lm(Oxy_Grains_Phenols_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Grains_Phenols_Kg_log<- Anova(modelo_adj_Oxy_Grains_Phenols_Kg_log)
                              
                              #Frutas/kG
                              Oxy_Fruits_Phenols_Kg_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Fruits_Phenols_Kg,2)
                              modelo_adj_Fruits_Phenols_Kg_log <-lm(Oxy_Fruits_Phenols_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Fruits_Phenols_Kg_log<- Anova(modelo_adj_Fruits_Phenols_Kg_log)
                              
                              #Vegetables/Kg
                              Oxy_Vegetables_Phenols_Kg_log<- log(0.001+BD_clinical_oxyl_colci_final_phenol_JSE$Vegetables_Phenols_Kg,2)
                              modelo_adj_Oxy_Vegetables_Phenols_Kg_log <-lm(Oxy_Vegetables_Phenols_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                              BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Vegetables_Phenols_Kg_log<- Anova(modelo_adj_Oxy_Vegetables_Phenols_Kg_log)
                              
                              
                              #Fat/KG
                              Oxy_Fats_Phenols_FC_log<- log(0.0001+BD_clinical_oxyl_colci_final_phenol_JSE$Fats_Phenols_FC,2)
                              modelo_adj_Fats_Phenols_FC_log <-lm(Oxy_Fats_Phenols_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Fats_Phenols_FC_log<- Anova(modelo_adj_Fats_Phenols_FC_log)

                              #Sweets and beverages/kg
                              Oxy_Sweets_n_beverages_Phenols_Kg_FC_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Sweets_n_beverages_Phenols_Kg,2)
                              modelo_adj_Oxy_Sweets_n_beverages_Phenols_Kg_log <-lm(Oxy_Sweets_n_beverages_Phenols_Kg_FC_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Sweets_n_beverages_Phenols_Kg_FC<- Anova(modelo_adj_Oxy_Sweets_n_beverages_Phenols_Kg_log)

                              
                            #Fiber intake by each FNNDS category
                              
                              #Total fiber
                              Oxy_Total_Fiber_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Total_Fiber,2)
                              modelo_adj_Total_Fiber_log <-lm(Oxy_Total_Fiber_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fiber_log<- Anova(modelo_adj_Total_Fiber_log)
                              
                              #Total fiber-milk
                              Oxy_Total_Fb_Milk_log<- log(0.001+BD_clinical_oxyl_colci_final_phenol_JSE$Total_Fb_Milk,2)
                              modelo_adj_Total_Fb_Milk_log <-lm(Oxy_Total_Fb_Milk_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fb_Milk_log<- Anova(modelo_adj_Total_Fb_Milk_log)
                              
                              #Total fiber-meats
                              Oxy_Total_Fb_.Meats_log<- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Total_Fb_.Meats,2)
                              modelo_adj_Total_Fb_.Meats_log <-lm(Oxy_Total_Fb_.Meats_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fb_.Meats_log<- Anova(modelo_adj_Total_Fb_.Meats_log)

                              #Total fiber-legumes
                              Oxy_Legumes_Fb_totals_log <- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Legumes_Fb_totals,2)
                              modelo_adj_Legumes_Fb_totals_log <-lm(Oxy_Legumes_Fb_totals_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                      BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fb_legumes_log<- Anova(modelo_adj_Legumes_Fb_totals_log)
                              
                              #Total fiber-grains
                              Oxy_Total_Fb_Grains_log <- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Total_Fb_Grains,2)
                              modelo_adj_Total_Fb_Grains_log <-lm(Oxy_Total_Fb_Grains_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fb_Grains_log<- Anova(modelo_adj_Total_Fb_Grains_log)
                              
                              #Total fiber-fruits
                              Oxy_Total_Fb_Fruits_log <- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Total_Fb_Fruits,2)
                              modelo_adj_Total_Fb_Fruits_log <-lm(Oxy_Total_Fb_Fruits_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fb_Fruits_log<- Anova(modelo_adj_Total_Fb_Fruits_log)
                              
                              #Total fiber-vegetable
                              Oxy_Total_Fb_Vegetables_log <- log(0.01+BD_clinical_oxyl_colci_final_phenol_JSE$Total_Fb_Vegetables,2)
                              modelo_adj_Total_Fb_Vegetables_log <-lm(Oxy_Total_Fb_Vegetables_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                        BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fb_Vegetables_log<- Anova(modelo_adj_Total_Fb_Vegetables_log)
                              
                              #Total fiber-Sweet and beverages
                              Oxy_Total_Fb_Sweets_n_beverages_log <- log(0.001+BD_clinical_oxyl_colci_final_phenol_JSE$Total_Fb_Sweets_n_beverages,2)
                              modelo_adj_Total_Fb_Sweets_n_beverages_log <-lm(Oxy_Total_Fb_Sweets_n_beverages_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                                                BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                                BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Total_Fb_Sweets_n_beverages_log<- Anova(modelo_adj_Total_Fb_Sweets_n_beverages_log)
                              
                            #Micronutrients intake by each FNNDS category
                              
                              #Fe
                              Oxy_Fe_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Fe,2)
                              
                              modelo_adj_Fe_log <-lm(Oxy_Fe_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Fe_log<- Anova(modelo_adj_Fe_log)
                              
                              #Zn
                              Oxy_Zn_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Zn,2)
                              modelo_adj_Zn_log <-lm(Oxy_Zn_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Zn_log<- Anova(modelo_adj_Zn_log)
                              
                              #Cu
                              Oxy_Cu_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Cu,2)
                              modelo_adj_Cu_log <-lm(Oxy_Cu_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Cu_log<- Anova(modelo_adj_Cu_log)
                              
                              
                              #Mn
                              Oxy_Mn_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Mn,2)
                              modelo_adj_Mn_log <-lm(Oxy_Mn_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                       BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Mn_log<- Anova(modelo_adj_Mn_log)
                              
                              
                              #Vit A
                              Oxy_Vit_A_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Vit_A,2)
                              modelo_adj_Vit_A_log <-lm(Oxy_Vit_A_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Vit_A_log<- Anova(modelo_adj_Vit_A_log)
                              
                              #Vit C
                              Oxy_Vit_C_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Vit_C,2)
                              
                              modelo_adj_Vit_C_log <-lm(Oxy_Vit_C_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Vit_C_log<- Anova(modelo_adj_Vit_C_log)
                              
                            #Total micronutrients adjusted by body weight (intake/kg in each FNNDS category)
                              
                              #Fe/kg
                              Oxy_Fe_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Fe_Kg,2)
                              modelo_adj_Fe_Kg_log <-lm(Oxy_Fe_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Fe_Kg_log<- Anova(modelo_adj_Fe_Kg_log)

                              #Zn/kg
                              Oxy_Zn_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Zn_Kg,2)
                              modelo_adj_Zn_Kg_log <-lm(Oxy_Zn_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Zn_Kg_log<- Anova(modelo_adj_Zn_Kg_log)
                              
                              #Cu/kg
                              Oxy_Cu_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Cu_Kg,2)
                              modelo_adj_Cu_Kg_log <-lm(Oxy_Cu_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Cu_Kg_log<- Anova(modelo_adj_Cu_Kg_log)
                              
                              #Mn/kg
                              Oxy_Mn_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Mn_Kg,2)
                              modelo_adj_Mn_Kg_log <-lm(Oxy_Mn_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                          BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Mn_Kg_log<- Anova(modelo_adj_Mn_Kg_log)
                              
                              
                              #Vitamina A/kg
                              Oxy_Vit_A_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Vit_A_Kg,2)
                              modelo_adj_Vit_A_Kg_log <-lm(Oxy_Vit_A_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Vit_A_Kg_log<- Anova(modelo_adj_Vit_A_Kg_log)
                              
                              #Vitamina C/kg
                              Oxy_Vit_C_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Vit_C_Kg,2)
                              modelo_adj_Vit_C_Kg_log <-lm(Oxy_Vit_C_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                             BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Vit_C_Kg_log<- Anova(modelo_adj_Vit_C_Kg_log)
                              
                              
                              #summary ANOVA MLR (adjusted: city,sex, age range)
                              DB_Oxy_variables_ajusted <- cbind(Anova_adj_modelo_adj_Oxy_BMI_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Weight_log $`Pr(>F)`,                                                  Anova_adj_modelo_adj_Oxy_Body_size_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Waist.Circ_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_ApoB$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_HDL_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_VLDL_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_LDL_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_TC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_TG_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Body.Fat_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_SBP_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_DBP_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Glucose_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Insulin_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_b.cell.func_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_IS_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_IR_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_LBP_log$`Pr(>F)`,
                                                                #oxLDL
                                                                Anova_modelo_adj_Oxy_oxLDL_log$`Pr(>F)`,
                                                                #CAGs
                                                                Anova_adj_modelo_adj_Oxy_Prevotella_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Lachnospiraceae_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Pathogen_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Akkermansia_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Oxy_Ruminococcaceae_log $`Pr(>F)`,
                                                                #OXYLIPINS
                                                                Anova_adj_modelo_adj_Tetranor.PGFM_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_2.3.dinor.11b.PGF2a_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_PGE2_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_X11.b.PGF2a_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_X15.F2t.IsoP_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_X15.epi.15.F2t_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_2.3.dinor.15.epi.15F2t.Isop_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_2.3.dinor.15.F2t.IsoP_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_X5.F2t_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_X5.epi.5.F2t_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_X15.epi.15E2t.Isop_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_X11.DH.TXB2_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_15.F1t.IsoP_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_15.keto.15.F2t.IsoP_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_15F2t_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_PGD_M_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_5F2t_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_PGs_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_IsoPs_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_2.3_dinor_oxy_log$`Pr(>F)`,
                                                                
                                                                #Phenols intake
                                                                Anova_adj_modelo_adj_Total_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Milk_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Meats_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Legumes_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Grains_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Fruits_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Vegetables_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Fats_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Sweets_n_beverages_Phenols_FC_log$`Pr(>F)`,
                                                                #Phenols intakeadjusted by body weigh
                                                                Anova_adj_modelo_adj_Total_Phenols_kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Milk_Phenols_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Meats_Phenols_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Legumes_Phenols_Kg_log $`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Grains_Phenols_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Fruits_Phenols_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Vegetables_Phenols_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Fats_Phenols_FC_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Sweets_n_beverages_Phenols_Kg_FC$`Pr(>F)`,
                                                                #Fiber 
                                                                Anova_adj_modelo_adj_Total_Fb_Milk_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_Fb_.Meats_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_Fb_legumes_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_Fb_Grains_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_Fb_Fruits_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_Fb_Vegetables_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Total_Fb_Sweets_n_beverages_log$`Pr(>F)`,
                                                                #Micronutrients and other antioxidants intake
                                                                Anova_adj_modelo_adj_Fe_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Zn_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Cu_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Mn_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Vit_A_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Vit_C_log$`Pr(>F)`,
                                                                #Micronutrients and other antioxidants intake adjusted by body weigh
                                                                Anova_adj_modelo_adj_Fe_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Zn_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Cu_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Mn_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Vit_A_Kg_log$`Pr(>F)`,
                                                                Anova_adj_modelo_adj_Vit_C_Kg_log$`Pr(>F)`)
                              #Export ANOVA MLR summary
                              write.table(DB_Oxy_variables_ajusted,file = "Output_oxylipins/Databases_oxylipins/DB_Oxy_variables_ajusted_BMI_sex_RangeAge_All.xls", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              #Note: Add manually the variable names
                              

                          #STATs CAG (Tables #S1 paper oxylipins MiSalud)----  
                              
                              #Database -> #5. BD_clinical_oxyl_colci_final_phenol_JSE
                              #Classification by CAG (Ruminococaceae vs	Akkermansia	vs Prevotella	vs Lachnospiraceae vs	Pathogen)
                              colnames(BD_clinical_oxyl_colci_final_phenol_JSE)  
                              #Stats normality test
                              
                              #CAG
                              
                              CAG_Akkermansia_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-12, -14:-36)], CAG == "Akkermansia") 
                              CAG_Lachnospiraceae_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-12, -14:-36)],CAG == "Lachnospiraceae") 
                              CAG_Prevotella_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-12, -14:-36)], CAG == "Prevotella") 
                              CAG_Pathogen_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-12, -14:-36)], CAG == "Pathogen") 
                              CAG_Ruminococaceae_oxyl_normality  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-12, -14:-36)], CAG == "Ruminococaceae") 

                              DB_oxy_CAG <- rbind(CAG_Ruminococaceae_oxyl_normality,CAG_Pathogen_oxyl_normality,CAG_Prevotella_oxyl_normality,CAG_Lachnospiraceae_oxyl_normality,CAG_Akkermansia_oxyl_normality)
                              write.table(DB_oxy_CAG,file = "Output_oxylipins/Databases_oxylipins/DB_oxy_CAG.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              #Shapiro
                              #Sin la variable categorica y fenoles y fibrar para huevos y carnes y grasas (los valores en 0 no dejan dejan correr todo el shapiro)
                              #Sin la variable categorica y fenoles para huevos y carnes (los valores en 0 no dejan dejan correr todo el shapiro)
                              CAG_Akkermansia_Oxyl_normality_shapiro <- lapply(CAG_Akkermansia_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test) 
                              CAG_Lachnospiraceae_Oxyl_normality_shapiro <- lapply(CAG_Lachnospiraceae_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test)
                              CAG_Prevotella_Oxyl_normality_shapiro <- lapply(CAG_Prevotella_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test)
                              CAG_Pathogen_Oxyl_normality_shapiro <- lapply(CAG_Pathogen_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test)
                              CAG_Ruminococaceae_Oxyl_normality_shapiro <- lapply(CAG_Ruminococaceae_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], shapiro.test)
                              
                              Shapiro_CAG_Oxyl_Colciencias <- cbind(CAG_Ruminococaceae_Oxyl_normality_shapiro,CAG_Pathogen_Oxyl_normality_shapiro,CAG_Prevotella_Oxyl_normality_shapiro,CAG_Lachnospiraceae_Oxyl_normality_shapiro,CAG_Akkermansia_Oxyl_normality_shapiro)
                              write.table(Shapiro_CAG_Oxyl_Colciencias,file = "Output/Normality_test_Adipo_colcie/Shapiro_CAG_Oxyl_Colciencias.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              #The following variables passed shapiro. Therefore, the distribution of the residuals will be analyzed by a QQ-plot. Find each variable to check
                              
                              #QQplot_BMI
                              variables_names_oxyl<- dput(colnames(select_if(CAG_Akkermansia_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], is.numeric)))
                              
                              #Akkermansia
                              CAG_Akkermansia_Oxyl_QQplot <- lapply(variables_names_oxyl, function(item) {
                                ggqqplot(CAG_Akkermansia_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = TRUE) +
                                  ggtitle(item)
                              }
                              )
                              
                              #Normal distribution shapiro -lEAN
                              CAG_Akkermansia_Oxyl_QQplot[5]
                              CAG_Akkermansia_Oxyl_QQplot[8]
                              CAG_Akkermansia_Oxyl_QQplot[12]
                              CAG_Akkermansia_Oxyl_QQplot[14]
                              CAG_Akkermansia_Oxyl_QQplot[16]
                              CAG_Akkermansia_Oxyl_QQplot[18]
                              CAG_Akkermansia_Oxyl_QQplot[19]
                              CAG_Akkermansia_Oxyl_QQplot[39]
                              CAG_Akkermansia_Oxyl_QQplot[49]
                              CAG_Akkermansia_Oxyl_QQplot[70]
                              

                              #Lachnospiraceae
                              CAG_Lachnospiraceae_Oxyl_QQplot <- lapply(variables_names_oxyl, function(item) {
                                ggqqplot(CAG_Lachnospiraceae_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = TRUE) +
                                  ggtitle(item)
                              }
                              )
                              
                              #Normal distribution shapiro 
                              CAG_Lachnospiraceae_Oxyl_QQplot[5]
                              CAG_Lachnospiraceae_Oxyl_QQplot[8]
                              CAG_Lachnospiraceae_Oxyl_QQplot[12]
                              CAG_Lachnospiraceae_Oxyl_QQplot[14]
                              CAG_Lachnospiraceae_Oxyl_QQplot[16]
                              CAG_Lachnospiraceae_Oxyl_QQplot[18]
                              CAG_Lachnospiraceae_Oxyl_QQplot[19]
                              CAG_Lachnospiraceae_Oxyl_QQplot[39]
                              CAG_Lachnospiraceae_Oxyl_QQplot[49]
                              CAG_Lachnospiraceae_Oxyl_QQplot[70]
                              
                              
                              #Prevotella
                              CAG_Prevotella_Oxyl_QQplot <- lapply(variables_names_oxyl, function(item) {
                                ggqqplot(CAG_Prevotella_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = TRUE) +
                                  ggtitle(item)
                              }
                              )
                              
                              #Normal distribution shapiro 
                              CAG_Prevotella_Oxyl_QQplot[5]
                              CAG_Prevotella_Oxyl_QQplot[8]
                              CAG_Prevotella_Oxyl_QQplot[12]
                              CAG_Prevotella_Oxyl_QQplot[14]
                              CAG_Prevotella_Oxyl_QQplot[16]
                              CAG_Prevotella_Oxyl_QQplot[18]
                              CAG_Prevotella_Oxyl_QQplot[19]
                              CAG_Prevotella_Oxyl_QQplot[39]
                              CAG_Prevotella_Oxyl_QQplot[49]
                              CAG_Prevotella_Oxyl_QQplot[70]
                              
                              
                              #Pathogen
                              CAG_Pathogen_Oxyl_QQplot <- lapply(variables_names_oxyl, function(item) {
                                ggqqplot(CAG_Pathogen_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = TRUE) +
                                  ggtitle(item)
                              }
                              )
                              
                              #Normal distribution shapiro 
                              CAG_Pathogen_Oxyl_QQplot[5]
                              CAG_Pathogen_Oxyl_QQplot[8]
                              CAG_Pathogen_Oxyl_QQplot[12]
                              CAG_Pathogen_Oxyl_QQplot[14]
                              CAG_Pathogen_Oxyl_QQplot[16]
                              CAG_Pathogen_Oxyl_QQplot[18]
                              CAG_Pathogen_Oxyl_QQplot[19]
                              CAG_Pathogen_Oxyl_QQplot[39]
                              CAG_Pathogen_Oxyl_QQplot[49]
                              CAG_Pathogen_Oxyl_QQplot[70]
                              
                              
                              #Pathogen
                              CAG_Ruminococaceae_Oxyl_QQplot <- lapply(variables_names_oxyl, function(item) {
                                ggqqplot(CAG_Ruminococaceae_oxyl_normality[c(-1,-49, -50,-55, -59, -60,-65,-70,-75)], x=item, combine = TRUE) +
                                  ggtitle(item)
                              }
                              )
                              
                              #Normal distribution shapiro 
                              CAG_Ruminococaceae_Oxyl_QQplot[5]
                              CAG_Ruminococaceae_Oxyl_QQplot[8]
                              CAG_Ruminococaceae_Oxyl_QQplot[12]
                              CAG_Ruminococaceae_Oxyl_QQplot[14]
                              CAG_Ruminococaceae_Oxyl_QQplot[16]
                              CAG_Ruminococaceae_Oxyl_QQplot[18]
                              CAG_Ruminococaceae_Oxyl_QQplot[19]
                              CAG_Ruminococaceae_Oxyl_QQplot[39]
                              CAG_Ruminococaceae_Oxyl_QQplot[49]
                              CAG_Ruminococaceae_Oxyl_QQplot[70]
                              
                              
                          # NON-Parametric (Kruskall)
                              Kruskal__Oxyl_CAG <- lapply(colnames(BD_clinical_oxyl_colci_final_phenol_JSE[37:123]),
                                                    function(x) kruskal.test(BD_clinical_oxyl_colci_final_phenol_JSE[,x],BD_clinical_oxyl_colci_final_phenol_JSE$CAG,BD_clinical_oxyl_colci_final_phenol_JSE))
                              
                              pval_Oxyl_CAG <- sapply(Kruskal__Oxyl_CAG, function(x) x$p.value)
                              write.table(pval_Oxyl_CAG,file = "Output_oxylipins/Databases_oxylipins/Normality_test/pval_Oxyl_BY_CAG.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              #In this file is necessary add manually all variables name from BD_clinical_oxyl_colci_final_phenol_JSE 
                              
                              
                              #q-values (P value Adjusted)
                              table_All_Pvalues_Oxyl_CAG <- list(pval_Oxyl_CAG)
                              
                              # import data
                              p_values_Oxyl_CAG <- table_All_Pvalues_Oxyl_CAG[[1]]
                              
                              # get q-value object
                              qobj_oxyl_CAG <- qvalue(p_values_Oxyl_CAG)
                              plot(qobj_oxyl_CAG)
                              hist(qobj_oxyl_CAG)
                              
                              # Export q-value object
                              write.table(qobj_oxyl_CAG[["qvalues"]],file = "Output_oxylipins/Databases_oxylipins/qobj_oxyl_CAG_table_S1.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              #e.g. By each variable
                              kruskal.test(BD_clinical_oxyl_colci_final_phenol_JSE$LDL ~ BD_clinical_oxyl_colci_final_phenol_JSE$CAG, BD_clinical_oxyl_colci_final_phenol_JSE)
                              
                              
                          # ANOVA estimated from multiple linear regression (MLR) only for a significant variables in the Kruskal analysis. The values of selected variables were log-transformed and adjusted for potential confounders: age range, participants' city of origin, and sex at birth.
                              
                               #Zn/kg/CAG
                              Oxy_Zn_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Zn_Kg,2)
                              modelo_adj_Zn_Kg_CAG_log <-lm(Oxy_Zn_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Zn_Kg_CAG_log<- Anova(modelo_adj_Zn_Kg_CAG_log)

                              #Fe/kg/CAG
                              Oxy_Zn_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Zn_Kg,2)
                              modelo_adj_Zn_Kg_CAG_log <-lm(Oxy_Zn_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Zn_Kg_CAG_log<- Anova(modelo_adj_Zn_Kg_CAG_log)
                              
                              #Fe/kg/CAG
                              Oxy_Fe_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Fe_Kg,2)
                              modelo_adj_Fe_Kg_CAG_log <-lm(Oxy_Fe_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Fe_Kg_CAG_log<- Anova(modelo_adj_Fe_Kg_CAG_log)
                              
                              #Mn/kg/CAG
                              Oxy_Mn_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Mn_Kg,2)
                              modelo_adj_Mn_Kg_CAG_log <-lm(Oxy_Mn_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                              BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Mn_Kg_CAG_log<- Anova(modelo_adj_Mn_Kg_CAG_log)
                              
                              
                              #BMI/CAG
                              
                              Oxy_BMI_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$BMI,2)
                              modelo_adj_bmi_CAG_log <-lm(Oxy_BMI_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_bmi_CAG_log<- Anova(modelo_adj_bmi_CAG_log)
                              
                              
                              #wEIGHT/CAG
                              Oxy_Weight_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Weight,2)
                              modelo_adj_Weight_CAG_log <-lm(Oxy_Weight_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Weight_CAG_log<- Anova(modelo_adj_Weight_CAG_log)
                              
                              #Body size/CAG
                              
                              Oxy_Body_size_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Body_size,2)
                              modelo_adj_Body_size_CAG_log <-lm(Oxy_Body_size_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                  BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_Body_size_CAG_log<- Anova(modelo_adj_Body_size_CAG_log)
                              
                              
                              #Waist circunference/CAG
                              
                              Oxy_Waist.Circ_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Waist.Circ,2)
                              modelo_adj_Waist.Circ_CAG_log <-lm(Oxy_Waist.Circ_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                   BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_Waist.Circ_CAG_log<- Anova(modelo_adj_Waist.Circ_CAG_log)
                              
                              #HDL/CAG
                              Oxy_HDL_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$HDL,2)
                              modelo_adj_HDL_CAG_log <-lm(Oxy_HDL_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_HDL_CAG_log<- Anova(modelo_adj_HDL_CAG_log)
                              
                              
                              #SBP/CAG
                              
                              Oxy_SBP_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$SBP,2)
                              modelo_adj_SBP_CAG_log <-lm(Oxy_SBP_log~BD_clinical_oxyl_colci_final_phenol_JSE$CAG+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                            BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              Anova_adj_modelo_adj_SBP_CAG_log<- Anova(modelo_adj_SBP_CAG_log)
                              

                          #STATs Cardiometabolic Status (Tables #S2 paper oxylipins MiSalud)----  
                              
                              #Database -> #5. BD_clinical_oxyl_colci_final_phenol_JSE
                              #Classification by cardiometabolic status (health vs unhelath)
                              colnames(BD_clinical_oxyl_colci_final_phenol_JSE)  
                              #Stats normality test
                              
                              #Shapiro, Mann whitney and p-value adjusted was also performed directly in prisma Graphpad "Estadistica descriptiva_Oxilipinas_crudos n=105"
                              

                              #Status
                              Healthy_normality_oxy  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-13, -15:-36, -84,-85, -94, -95, -103:-117)], State == "1.Healthy") #sin los valores con ceros (fenoles en huevos y carnes)
                              Abnormal_normality_oxy  <- filter(BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-13, -15:-36, -84,-85, -94, -95, -103:-117)], State == "2.Abnormal")  #sin los valores con ceros (fenoles en huevos y carnes)
                              
                              
                              #Shapiro
                              Healthy_normality_oxy_shapiro <- lapply(Healthy_normality_oxy[c(-1)], shapiro.test)
                              Abnormal_normality_oxy_shapiro <- lapply(Abnormal_normality_oxy[c(-1)], shapiro.test)
                              
                              Shapiro_Status_oxy_jse<- cbind(Healthy_normality_oxy_shapiro,Abnormal_normality_oxy_shapiro)
                              write.table(Shapiro_Status_oxy_jse,file = "Output_oxylipins/Databases_oxylipins/Shapiro_Status_oxy_jse.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              
                              Kruskal_state_JSE <- lapply(colnames(BD_clinical_oxyl_colci_final_phenol_JSE[37:123]),
                                                          function(x) kruskal.test(BD_clinical_oxyl_colci_final_phenol_JSE[,x],BD_clinical_oxyl_colci_final_phenol_JSE$State,BD_clinical_oxyl_colci_final_phenol_JSE))
                             
                               # or direct with pairwise.wilcox.test (Mann_Whitney_analysis). This is the same result
                              Oxyl_Status.Card_Mann_Whitney_analysis <- lapply(colnames(BD_clinical_oxyl_colci_final_phenol_JSE[37:123]),
                                                                                function(x) pairwise.wilcox.test(BD_clinical_oxyl_colci_final_phenol_JSE[,x],paired = FALSE, BD_clinical_oxyl_colci_final_phenol_JSE$State, p.adjust.method = "bonferroni"))
                              
                              Kruskal_pva_BD_clinical_oxyl_State_colci_final_phenol_JSE <- sapply(Kruskal_state_JSE, function(x) x$p.value)
                              write.table(Kruskal_pva_BD_clinical_oxyl_State_colci_final_phenol_JSE,file = "Output_oxylipins/Databases_oxylipins/ Kruskal_pva_BD_clinical_oxyl_state_final_phenol_JSE.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              #q-values (P value Adjusted)
                              
                              # import data
                              p.state <- Kruskal_pva_BD_clinical_oxyl_State_colci_final_phenol_JSE
                              
                              # get q-value object
                              qobj_oxyl_Status <- qvalue(Kruskal_pva_BD_clinical_oxyl_State_colci_final_phenol_JSE)
                              plot(qobj_oxyl_Status)
                              hist(qobj_oxyl_Status)
                              # Export q-value object
                              write.table(qobj_oxyl_Status[["qvalues"]],file = "Output_oxylipins/Databases_oxylipins/qobj_oxyl_Status_Table S2.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              
                          # ANOVA estimated from multiple linear regression (MLR) only for a significant variables in the Kruskal analysis. The values of selected variables were log-transformed and adjusted for potential confounders: age range, participants' city of origin, and sex at birth.
                              
                              
                              #BMI/Status
                              
                              Oxy_BMI_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$BMI,2)
                              
                              modelo_adj_bmi_status_log <-lm(Oxy_BMI_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_bmi_status_log<- Anova(modelo_adj_bmi_status_log)
                              
                              
                              #BMI/Status
                              
                              Oxy_BMI_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$BMI,2)
                              
                              modelo_adj_bmi_status_log <-lm(Oxy_BMI_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                               BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_bmi_status_log<- Anova(modelo_adj_bmi_status_log)
                              
                              #ApoB/status
                              
                              
                              Oxy_ApoB_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$ApoB,2)
                              
                              modelo_adj_ApoB_status_log <-lm(Oxy_ApoB_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_ApoB_status_log<- Anova(modelo_adj_ApoB_status_log)
                              
                              
                              #ApoB/status
                              
                              
                              Oxy_Body.Fat_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Body.Fat,2)
                              
                              modelo_adj_Body.Fat_status_log <-lm(Oxy_Body.Fat_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_Body.Fat_status_log<- Anova(modelo_adj_Body.Fat_status_log)
                              
                              
                              #oxLDL/status
                              
                              
                              Oxy_oxLDL_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$oxLDL,2)
                              
                              modelo_adj_oxLDL_status_log <-lm(Oxy_oxLDL_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_oxLDL_status_log<- Anova(modelo_adj_oxLDL_status_log)
                              
                              
                              #Pathogene/status
                              
                              
                              Oxy_Pathogen_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Pathogen,2)
                              
                              modelo_adj_Pathogen_status_log <-lm(Oxy_Pathogen_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                    BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_Pathogen_status_log<- Anova(modelo_adj_Pathogen_status_log)
                              
                              #Rumino/status
                              
                              
                              Oxy_Ruminococcaceae_log<- log(0.000001+BD_clinical_oxyl_colci_final_phenol_JSE$Ruminococcaceae,2)
                              
                              modelo_adj_Ruminococcaceae_status_log <-lm(Oxy_Ruminococcaceae_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                           BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_Ruminococcaceae_status_log<- Anova(modelo_adj_Ruminococcaceae_status_log)
                              
                              
                              #Vitamin A/status
                              
                              
                              Oxy_Vit_A_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Vit_A,2)
                              
                              modelo_adj_Vit_A_status_log <-lm(Oxy_Vit_A_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_Vit_A_status_log<- Anova(modelo_adj_Vit_A_status_log)
                              
                              
                              #Cu_Kg/status
                              
                              
                              Oxy_Cu_Kg_log<- log(BD_clinical_oxyl_colci_final_phenol_JSE$Cu_Kg,2)
                              
                              modelo_adj_Cu_Kg_status_log <-lm(Oxy_Cu_Kg_log~BD_clinical_oxyl_colci_final_phenol_JSE$Status_CMH
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$City+BD_clinical_oxyl_colci_final_phenol_JSE$Sex+
                                                                 BD_clinical_oxyl_colci_final_phenol_JSE$Age_range)
                              
                              Anova_adj_modelo_adj_Cu_Kg_status_log<- Anova(modelo_adj_Cu_Kg_status_log)
                              
                              
                              
                              
                      #Correlations between oxylipins (Figure S1)----
                              
                              
                              # Corrplot
                              colnames(BD_clinical_oxyl_colci_final_phenol_JSE)
                              colnames(Correlation_oxylypins3)
                              
                              #Options
                              Correlation_oxylypins <- BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-61, -76:-123)] #Only oxylipins
                              Correlation_oxylypins1 <- BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-61, -82:-91, -102:-123)] # oxylipins and phenol intake
                              Correlation_oxylypins2 <- BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-36,-38,-41:-46, -48:-61, -76:-123)] #oxylipins and BMI,WC, weight, body fat
                              Correlation_oxylypins3 <- BD_clinical_oxyl_colci_final_phenol_JSE [c(-1:-36,-38,-41:-46, -48:-76,-84,-85,-94,-95, -102:-123)] #oxylipins and BMI,WC, weight, body fat
                              
                              #Figure Option 2 (Figures S1)
                              correlations2 <- (cor(Correlation_oxylypins2)) 
                              correlations3 <- (cor(Correlation_oxylypins3)) 
                              
                              corrplot(correlations2, is.corr=FALSE, method="circle", type = "upper", tl.cex = 0.3, tl.srt = 90)
                              write.table(correlations,file = "correlations.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              #P value adjusted
                              
                              cor.mtest <- function(Correlation_oxylypins2, ...) {
                                Correlation_oxylypins <- as.matrix(Correlation_oxylypins2)
                                n <- ncol(Correlation_oxylypins2)
                                p.Correlation_oxylypins<- matrix(NA, n, n)
                                diag(p.Correlation_oxylypins) <- 0
                                for (i in 1:(n - 1)) {
                                  for (j in (i + 1):n) {
                                    tmp <- cor.test(Correlation_oxylypins2[, i], Correlation_oxylypins2[, j], ...)
                                    p.Correlation_oxylypins[i, j] <- p.Correlation_oxylypins[j, i] <- tmp$p.value
                                  }
                                }
                                colnames(p.Correlation_oxylypins) <- rownames(p.Correlation_oxylypins) <- colnames(Correlation_oxylypins2)
                                p.Correlation_oxylypins
                              }
                              
                              # matrix of the p-value of the correlation
                              p.Correlation_oxylypins <- cor.mtest(Correlation_oxylypins2)
                              head(p.Correlation_oxylypins[, 1:5])
                              #write.table(p.Lipidomic_cambio,file = "Lipidomic_cambio.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              col <- colorRampPalette(c("#4ca3dd", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
                              
                              #corrplot with p_value
                              correlations_Correlation_oxylypins <- corrplot(correlations2, is.corr=FALSE, method="square", type = "upper",col=col(200), order="hclust", addCoef.col = "black", p.mat = p.Correlation_oxylypins, tl.col="black", sig.level = 0.05, tl.cex = 0.5, tl.srt = 45,insig = "blank", diag=FALSE )
                              write.table(correlations_Correlation_oxylypins,file = "Output/Tables/correlations_Correlation_oxylypins.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              write.table(p.Correlation_oxylypins,file = "Output/Tables/correlations_Pvalue.Correlation_oxylypins.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              
                              #option 3
                              #P value adjusted
                              
                              cor.mtest <- function(Correlation_oxylypins3, ...) {
                                Correlation_oxylypins <- as.matrix(Correlation_oxylypins3)
                                n <- ncol(Correlation_oxylypins3)
                                p.Correlation_oxylypins<- matrix(NA, n, n)
                                diag(p.Correlation_oxylypins) <- 0
                                for (i in 1:(n - 1)) {
                                  for (j in (i + 1):n) {
                                    tmp <- cor.test(Correlation_oxylypins3[, i], Correlation_oxylypins3[, j], ...)
                                    p.Correlation_oxylypins[i, j] <- p.Correlation_oxylypins[j, i] <- tmp$p.value
                                  }
                                }
                                colnames(p.Correlation_oxylypins) <- rownames(p.Correlation_oxylypins) <- colnames(Correlation_oxylypins3)
                                p.Correlation_oxylypins
                              }
                              
                              # matrix of the p-value of the correlation
                              p.Correlation_oxylypins <- cor.mtest(Correlation_oxylypins3)
                              head(p.Correlation_oxylypins[, 1:5])
                              #write.table(p.Lipidomic_cambio,file = "Lipidomic_cambio.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              col <- colorRampPalette(c("#4ca3dd", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
                              
                              #corrplot with p_value
                              correlations_Correlation_oxylypins3 <- corrplot(correlations3, is.corr=FALSE, method="square", type = "upper",col=col(200), order="alphabet", addCoef.col = "black", p.mat = p.Correlation_oxylypins, tl.col="black", sig.level = 0.05, tl.cex = 0.5, tl.srt = 45,insig = "blank", diag=FALSE )
                              
                              
                              
                              
                              
                              #PENDIENTE ESTE MISMO AN?LISIS PARA LAS DEMAS AGRUPACIONES (CAGs y Fenotipo cardiometabolico)
                              
                              #Estadistica no parametrica PARA LOS DATOS CRUDOS
                              
                              #variable por variable y valor P ajustado 
                              
                              #normal vs sobrepeso vs obeso
                              
                              
                              
                              

                              
                              
                              
                              #Escalados (Parametrico)
                              
                              #normal vs sobrepeso vs obeso
                              
                              prueba <- lapply(colnames(BD_clinical_oxyl_colci_final_Scal[12:55]),
                                               function(x) pairwise.t.test(BD_clinical_oxyl_colci_final_Scal[,x],BD_clinical_oxyl_colci_final_Scal$BMI_Classification,p.adjust.method = "bonferroni", paired = FALSE))
                              ?pairwise.wilcox.test
                              
                              pval <- sapply(prueba, function(x) x$p.value)
                              write.table(pval,file = "Output_oxylipins/Databases_oxylipins/Normality_test/Anova.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                              
                              
                              #Prueba de esfericidad
                              
                              
                              library(psych)
                              
                              Correlacion <- DBtotalIsoPs [c(-8, -12,-19, -20)] 
                              cor_matrix <- cor(Correlacion)
                              cortest.bartlett(cor_matrix, n = nrow(DBtotalIsoPs))
                              
    
                              

                             
                              
                              
                              
                              
                              
                              #To create this correlation was neccesary to imput some empty values
                              #Imputation: Seven values are under imputation (LDL: MI-078; PAS: MI:354; OXLDL: MI-201,MI-257,  MI-354, MI-384,MI-447)
                              #BD_clinical_oxyl_colci_final_outl
                     #Imputation. Seven values are under imputation (LDL: MI-078; PAS: MI:354; OXLDL: MI-201,MI-257,  MI-354, MI-384,MI-447)
                    DB_Info_Colciencias_ox_Im2 <- DBtotalOxylipins
                    res.comp.DB_Info_Colcie_ox2 = imputePCA(DB_Info_Colciencias_ox_Im2,ncp=10)
                    res.pca.DB_info_Colcie_ox2 = PCA(res.comp.DB_Info_Colcie_ox2$completeObs)
                    view(res.pca.DB_info_Colcie_ox[["call"]][["X"]])
                    fviz_eig(res.pca.DB_info_Colcie_ox2, addlabels = TRUE, ylim = c(0, 50))
                    
                    
                    Biplot_ox_IMC2 = fviz_pca_biplot(res.pca.DB_info_Colcie_ox2, axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = BD_clinical_oxyl_colci_final_outl$BMI_Classification, palette = c( "#0000ff", "#ffa500", "#ff0000", "#008000", "#8a2be2"), pointshape = 21, pointsize = 3, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
                    ggpubr::ggpar(Biplot_ox_IMC2, title = "Principal component analysis", subtitle = "Clinical and Oxylipins profile", caption = FALSE, xlab = "PC1(23.1%)", ylab = "PC2(14.9%)", legend.position = "top", legendsize = 8)
                    
                    rm(Biplot_ox_IMC2)
                    
                    # VARIABLES: VALOR P (Mide la significacia entre la variable y el componente principal)
                    
                    res.desc2 <- dimdesc(res.pca.DB_info_Colcie_ox2, axes = c(1,2,3), proba = 0.05) #res.pca se optiene del codigo anterior en PCA
                    # Description of dimension 
                    res.desc2$Dim.1
                    res.desc2$Dim.2
                    res.desc$Dim.3
                    write.table(res.desc2[["Dim.1"]][["quanti"]],file = "Output_oxylipins/Databases_oxylipins/Pvalue_PCA1b.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    write.table(res.desc2[["Dim.2"]][["quanti"]],file = "Output_oxylipins/Databases_oxylipins/Pvalue_PCA2b.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    write.table(res.desc[["Dim.3"]][["quanti"]],file = "P_Value_PC3.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                    
                    
                    
                    
                    
                    # Red correlaciones (Por grupos)
                    
  
                      #Oxylipins_vs_phenols_105_scale
                      
                      ###BMI
  
                      Oxylipins_vs_phenols_105_corr <- cbind(BD_clinical_oxyl_colci_final_phenol_JSE$BMI_Classification, Oxylipins_vs_phenols_105_scale)
                      view(Adipo_scale)
                      
                      Oxylipins_vs_phenols_105_corr_D <-data.frame(Oxylipins_vs_phenols_105_corr)[c(-9,-13,-20,-21)]
                      
                      is.na(Oxylipins_vs_phenols_105_corr_D)
                      
                      BD2_sub <- Oxylipins_vs_phenols_105_corr_D %>%
                        group_by(V1) %>%
                        select_if(is.numeric)
                      
                      
                      BD2_cor <- BD2_sub %>%
                        group_by(V1) %>% ## redundant but worth it for illustration
                        nest() %>%
                        mutate(data = map(data, purrr::compose(stretch, correlate))) %>%
                        unnest() %>%
                        select(x, y, r, V1) %>%
                        filter(abs(r) > .25) %>%
                        graph_from_data_frame(directed = FALSE)
                      
                      ggraph(BD2_cor, layout = "kk") +
                        geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 1.5) +
                        guides(edge_alpha = "none") +
                        scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff","#ff0000")) +
                        geom_node_point(color = "black", size = 3) +
                        geom_node_text(aes(label = name), repel = TRUE, size = 3) +
                        facet_edges( ~ CV1) +
                        theme_minimal() 
                      
                      
                      
                      ###BMI oxilipinas vs fenoles
                      
                      Oxi_corr_info_colci <- res.pca.DB_info_Colcie_ox[["call"]][["X"]]
                      Oxi_corr_info_colci_D <-data.frame(Oxi_corr_info_colci)
                      
                      Oxi_corr_info_colci_F <- cbind(IMC_Info_Colciencias_ox, Oxi_corr_info_colci_D)
                      view(Adipo_scale)
                      
                      
                      BD2_sub <- Oxi_corr_info_colci_F %>%
                        group_by(Clasification.1) %>%
                        select_if(is.numeric)
                      
                      
                      BD2_cor <- BD2_sub %>%
                        group_by(Clasification.1) %>% ## redundant but worth it for illustration
                        nest() %>%
                        mutate(data = map(data, purrr::compose(stretch, correlate))) %>%
                        unnest() %>%
                        select(x, y, r, Clasification.1) %>%
                        filter(abs(r) > .25) %>%
                        graph_from_data_frame(directed = FALSE)
                      
                      ggraph(BD2_cor, layout = "kk") +
                        geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 1.5) +
                        guides(edge_alpha = "none") +
                        scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff","#ff0000")) +
                        geom_node_point(color = "black", size = 3) +
                        geom_node_text(aes(label = name), repel = TRUE, size = 3) +
                        facet_edges( ~ Clasification.1) +
                        theme_minimal() 
                    
                    

                    

                  

                      
                      
                
                      
                
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
                      
rm(prueba)
                  #Imputation. Two values are under imputation (LDL: MI-078; PAS: MI:354; OXLDL: MI-201,MI-257, MI-262, MI-276, MI-354, MI-384,MI-447)
                  
                  res.comp.DB_Oxy_Colciencias_im = imputePCA(DB_Oxy_Colciencias,ncp=10)
                  res.pca.DB_Oxy_Colciencias_im = PCA(res.comp.DB_Oxy_Colciencias_im$completeObs)
                  view(res.pca.DB_Polyphenols[["call"]][["X"]])
                  write.table(res.pca.DB_Oxy_Colciencias_im[["call"]][["X"]],file = "Output_oxylipins/Databases_oxylipins/Imputed_oxylipins/DB_Oxy_Colciencias_im.csv", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  eig.val.DB_Polyphenols <- get_eigenvalue(res.pca.DB_Polyphenols)
                  fviz_eig(res.pca.DB_Polyphenols, addlabels = TRUE, ylim = c(0, 20))
                  
                  #Data set for correlation analysis
                  DB_Oxy_Colciencias_im_scaled <- scale (res.pca.DB_Oxy_Colciencias_im[["call"]][["X"]])
                  write.table(DB_Oxy_Colciencias_im_scaled,file = "Output_oxylipins/Databases_oxylipins/Imputed_oxylipins/DB_Oxy_Colciencias_im_scaled.csv", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
                  
              
                      #Correlations
                  
                  # Red correlaciones
                  
                  tidy_cors <- DB_Oxy_Colciencias_im_scaled %>% 
                    correlate() %>% 
                    stretch()
                  
                  # Convert correlations stronger than some value
                  # to an undirected graph object
                  
                  
                  graph_cors <- tidy_cors %>% 
                    filter(abs(r) > 0.25) %>%
                    graph_from_data_frame(directed = FALSE)
                  
                  ggraph(graph_cors) +
                    geom_edge_link() +
                    geom_node_point() +
                    geom_node_text(aes(label = name))
                  
                  ggraph(graph_cors) +
                    geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +  #cambie el edge_width = abs(r) por none
                    guides(edge_alpha = "none", edge_width = "none") +
                    scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff", "#fd240b")) + 
                    geom_node_point(color = "black", size = 4) +
                    geom_node_text(aes(label = name),size = 7, repel = TRUE) +
                    theme_graph() +
                    labs(title = "Global correlation (n=106)")
                  
              
#PCA (Informe Colciencias)
              
              #Adipokines (n = 121)
              
              DB_Info_Colciencias <- DB_Proy_Colc_Final_R [c(-1:-48, -50:-109,-135:-148)]
              colnames(DB_Proy_Colc_Final_R)
              State_Info_Colciencias <- DB_Proy_Colc_Final_R [c(-1:-13, -15:-148)]
              CAG_Info_Colciencias <- DB_Proy_Colc_Final_R [c(-1:-12, -14:-148)]
              
               
              
              #Imputation. Nine values are under imputation (LDL: MI-078; PAS: MI:354; OXLDL: MI-201,MI-257, MI-262, MI-276, MI-354, MI-384,MI-447)
              DB_Info_Colciencias_Im <- DB_Info_Colciencias 
              res.comp.DB_Info_Colcie = imputePCA(DB_Info_Colciencias_Im,ncp=10)
              res.pca.DB_info_Colcie = PCA(res.comp.DB_Info_Colcie$completeObs)
              view(res.pca.DB_info_Colcie[["call"]][["X"]])
              

              Biplot = fviz_pca_biplot(res.pca.DB_info_Colcie, geom.var = c("text","point"), geom.ind = c("point"), fill.ind = State_Info_Colciencias$State, palette = c( "#0000ff", "#ffa500"), pointshape = 21, pointsize = 3, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
              ggpubr::ggpar(Biplot, title = "Principal component analysis", subtitle = "Adipokines profile", caption = FALSE, xlab = "PC1(22.3%)", ylab = "PC2(13.5%)", legend.position = "top", legendsize = 8)
              
              Biplot_CAG = fviz_pca_biplot(res.pca.DB_info_Colcie, geom.var = c("text","point"), geom.ind = c("point"), fill.ind = CAG_Info_Colciencias$CAG, palette = c( "#0000ff", "#ffa500", "#ff0000", "#008000", "#8a2be2"), pointshape = 21, pointsize = 3, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
              ggpubr::ggpar(Biplot_CAG, title = "Principal component analysis", subtitle = "Adipokines profile", caption = FALSE, xlab = "PC1(22.3%)", ylab = "PC2(13.5%)", legend.position = "top", legendsize = 8)
              
              
        #Oxylipins and clinical 
              
              
              DB_Info_Colciencias_ox <- DB_Proy_Colc_Final_R [c(-1:-36, -46:-51,-53:-58,-60:-69, -72:-104, -110:-134)] [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92, -106, -117),]
              colnames(DB_Proy_Colc_Final_R)
              State_Info_Colciencias_ox <- DB_Proy_Colc_Final_R [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92, -106, -117),]  [c(-1:-13, -15:-148)] 
              CAG_Info_Colciencias_ox <- DB_Proy_Colc_Final_R [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92, -106, -117),] [c(-1:-12, -14:-148)]
              IMC_Info_Colciencias_ox <- DB_Proy_Colc_Final_R [c(-17,-28, -31, -37, -43,- 55, -57, -62, -64, -67, -70, -82, -92, -106, -117),] [c(-1:-9, -11:-148)]
              
              
              #Imputation. Nine values are under imputation (LDL: MI-078; PAS: MI:354; OXLDL: MI-201,MI-257, MI-262, MI-276, MI-354, MI-384,MI-447)
              DB_Info_Colciencias_ox_Im <- DB_Info_Colciencias_ox 
              res.comp.DB_Info_Colcie_ox = imputePCA(DB_Info_Colciencias_ox_Im,ncp=10)
              res.pca.DB_info_Colcie_ox = PCA(res.comp.DB_Info_Colcie_ox$completeObs)
              view(res.pca.DB_info_Colcie_ox[["call"]][["X"]])
              
              
              Biplot_ox = fviz_pca_biplot(res.pca.DB_info_Colcie_ox,axes = c(1,2), geom.var = c("text","point"), geom.ind = c("point"), fill.ind = State_Info_Colciencias_ox$State, palette = c( "#0000ff", "#ffa500"), pointshape = 21, pointsize = 3, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
              ggpubr::ggpar(Biplot_ox, title = "Principal component analysis", subtitle = "Oxylipins profile", caption = FALSE, xlab = "PC1(22.3%)", ylab = "PC2(13.5%)", legend.position = "top", legendsize = 8)
              
              Biplot_ox_IMC = fviz_pca_biplot(res.pca.DB_info_Colcie_ox, geom.var = c("text","point"), geom.ind = c("point"), fill.ind = IMC_Info_Colciencias_ox$Clasification.1, palette = c( "#0000ff", "#ffa500", "#ff0000", "#008000", "#8a2be2"), pointshape = 21, pointsize = 3, labelsize = 4, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c( "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "euclid", addEllipses = TRUE)
              ggpubr::ggpar(Biplot_ox_IMC, title = "Principal component analysis", subtitle = "Oxylipins profile", caption = FALSE, xlab = "PC1(22.3%)", ylab = "PC2(13.5%)", legend.position = "top", legendsize = 8)
              
              
              ###State
              Oxi_corr_info_colci <- res.pca.DB_info_Colcie_ox[["call"]][["X"]]
              Oxi_corr_info_colci_D <-data.frame(Oxi_corr_info_colci)

              Oxi_corr_info_colci_F <- cbind(IMC_Info_Colciencias_ox, Oxi_corr_info_colci_D)
              view(Adipo_scale)
              
              
              BD2_sub <- Oxi_corr_info_colci_F %>%
                group_by(Clasification.1) %>%
                select_if(is.numeric)
              
              
              BD2_cor <- BD2_sub %>%
                group_by(Clasification.1) %>% ## redundant but worth it for illustration
                nest() %>%
                mutate(data = map(data, purrr::compose(stretch, correlate))) %>%
                unnest() %>%
                select(x, y, r, Clasification.1) %>%
                filter(abs(r) > .30) %>%
                graph_from_data_frame(directed = FALSE)
              
              ggraph(BD2_cor, layout = "kk") +
                geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 1.5) +
                guides(edge_alpha = "none") +
                scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff","#ff0000")) +
                geom_node_point(color = "black", size = 3) +
                geom_node_text(aes(label = name), repel = TRUE, size = 4) +
                facet_edges( ~ Clasification.1) +
                theme_minimal() 
              
              ## global corr
              tidy_cors <- Oxi_corr_info_colci_D %>% 
                correlate() %>% 
                stretch()
              
              # Convert correlations stronger than some value
              # to an undirected graph object
              
              
              graph_cors <- tidy_cors %>% 
                filter(abs(r) > 0.25) %>%
                graph_from_data_frame(directed = FALSE)
              
              ggraph(graph_cors) +
                geom_edge_link() +
                geom_node_point() +
                geom_node_text(aes(label = name))
              
              ggraph(graph_cors) +
                geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +  #cambie el edge_width = abs(r) por none
                guides(edge_alpha = "none", edge_width = "none") +
                scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#003366","#ffffff", "#fd240b")) +
                geom_node_point(color = "black", size = 4) +
                geom_node_text(aes(label = name),size = 7, repel = TRUE) +
                theme_graph() +
                labs(title = "Global correlation")
  
  #Funcion variable cardiometabolico           
     
              BD <- mutate(SCM=case_when(as.numeric(TG>150) +
              as.numeric(Glucosa_mg > 110) +
                as.numeric(Per.Cintura > 88) + 
                as.numeric(PAS > 130) + as.numeric(PAD> 88) +
                as.numeric (HDL<40 )+(sexo==2)| 
                as.numeric (HDL<50 )+(sexo==1) >=3 ~ "SCM", TRUE ~ "Sano"))         
              

## oxilipinas e IMC

BD4 <- BD_Colciencias_MiSalud_impu [c(-1:-5, -7, -8, -10:-57)]
BD4_scaled <- scale(BD4)




BD1_sub <- BD4 %>%
  group_by(feno_imc2) %>%
  select_if(is.numeric)



BD1_cor <- BD1_sub %>%
  group_by(feno_imc2) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_imc2) %>%
  filter(abs(r) > .27) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD1_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_imc2) +
  theme_minimal() 
  
##estado
BD4 <- BD_Colciencias_MiSalud_impu [c(-1:-5, -7, -8, -10:-57)]

BD4e <- BD_Colciencias_MiSalud_impu [c(-1:-3, -5:-8, -10:-57)]


BD2_sub <- BD4e %>%
  group_by(estado) %>%
  select_if(is.numeric)


BD2_cor <- BD2_sub %>%
  group_by(estado) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, estado) %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD2_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~estado) +
  theme_minimal() 


##Feno_grasa


BD4g <- BD_Colciencias_MiSalud_impu [c(-1:-7, -10:-57)]

BD2_sub <- BD4g %>%
  group_by(feno_grasa) %>%
  select_if(is.numeric)

BD2_cor <- BD2_sub %>%
  group_by(feno_grasa) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_grasa) %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD2_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#0971b0", "#fd240b")) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_grasa) +
  theme_minimal() 

library(RColorBrewer)

?heat.colors




## oxilipinas e inflamaci?n

BD5 <- BD_Colciencias_MiSalud_impu [c(-1:-5, -7:-34, -36:-42)]
BD4_scaled <- scale(BD4)




BD1_sub <- BD5 %>%
  group_by(feno_imc2) %>%
  select_if(is.numeric)



BD1_cor <- BD1_sub %>%
  group_by(feno_imc2) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_imc2) %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD1_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_imc2) +
  theme_minimal() 

##estado
BD5e <- BD_Colciencias_MiSalud_impu [c(-1:-3, -5:-34, -36, -38:-42)]


BD2_sub <- BD5e %>%
  group_by(estado) %>%
  select_if(is.numeric)


BD2_cor <- BD2_sub %>%
  group_by(estado) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, estado) %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD2_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~estado) +
  theme_minimal() 



##Feno_grasa
BD5e <- BD_Colciencias_MiSalud_impu [c(-1:-3, -5:-34, -36, -38:-42)]

BD5g <- BD_Colciencias_MiSalud_impu [c(-1:-7, -9:-34,-36, -38:-42)]

BD2_sub <- BD5g %>%
  group_by(feno_grasa) %>%
  select_if(is.numeric)

BD2_cor <- BD2_sub %>%
  group_by(feno_grasa) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_grasa) %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD2_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#0971b0", "#fd240b")) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_grasa) +
  theme_minimal() 



## adipoquinas y cags

BD6 <- BD_Colciencias_MiSalud_impu [c(-1:-5, -7, -8,-9, -10:-37, -59:-72)]




BD1_sub <- BD6 %>%
  group_by(feno_imc2) %>%
  select_if(is.numeric)



BD1_cor <- BD1_sub %>%
  group_by(feno_imc2) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_imc2) %>%
  filter(abs(r) > .28) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD1_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_imc2) +
  theme_minimal() 

## adipoquinas, cags y SCFAs

BD7 <- BD_Colciencias_MiSalud_impu [c(-1:-5, -7, -8,-9, -10:-30, -34:-36, -59:-72)]




BD1_sub <- BD7 %>%
  group_by(feno_imc2) %>%
  select_if(is.numeric)



BD1_cor <- BD1_sub %>%
  group_by(feno_imc2) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_imc2) %>%
  filter(abs(r) > .28) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD1_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_imc2) +
  theme_minimal() 

##estado
BD7 <- BD_Colciencias_MiSalud_impu [c(-1:-5, -7, -8,-9, -10:-30, -34:-36, -59:-72)]
BD7e <- BD_Colciencias_MiSalud_impu [c(-1:-3, -5:-8, -10:-30, -34:-36, -59:-72)]


BD2_sub <- BD7e %>%
  group_by(estado) %>%
  select_if(is.numeric)


BD2_cor <- BD2_sub %>%
  group_by(estado) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, estado) %>%
  filter(abs(r) > .25) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD2_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~estado) +
  theme_minimal() 


##Feno_grasa
BD7e <- BD_Colciencias_MiSalud_impu [c(-1:-3, -5:-8, -10:-30, -34:-36, -59:-72)]

BD7g <- BD_Colciencias_MiSalud_impu [c(-1:-7, -10:-30, -34:-36, -59:-72)]

BD2_sub <- BD7g %>%
  group_by(feno_grasa) %>%
  select_if(is.numeric)

BD2_cor <- BD2_sub %>%
  group_by(feno_grasa) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_grasa) %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD2_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#0971b0", "#fd240b")) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_grasa) +
  theme_minimal() 




#toda completa

BD8 <- BD_Colciencias_MiSalud_impu [c(-1:-5,-7, -8, -11, -18, -19, -30)]


BD1_sub <- BD8 %>%
  group_by(feno_imc2) %>%
  select_if(is.numeric)



BD1_cor <- BD1_sub %>%
  group_by(feno_imc2) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_imc2) %>%
  filter(abs(r) > .28) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD1_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_imc2) +
  theme_minimal() 


#estado

BD8 <- BD_Colciencias_MiSalud_impu [c(-1:-5,-7, -8, -11, -18, -19, -30)]
BD8e <- BD_Colciencias_MiSalud_impu [c(-1:-3, -5:-8, -11,-18, -19, -30)]

col.
write.table(correlations,file = "correlations.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 


BD2_sub <- BD8e %>%
  group_by(estado) %>%
  select_if(is.numeric)


BD2_cor <- BD2_sub %>%
  group_by(estado) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, estado) %>%
  filter(abs(r) > .25) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD2_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~estado) +
  theme_minimal() 



### para la estdistica



BD8 <- BD_Colciencias_MiSalud_impu [c(-1:-5,-7, -8, -11, -18, -19, -30)]

#### t-sne y correlaciones

library(BiocManager)
?BiocManager

#La tarea con la base de datos de pollen et al es saber como colocar los nombres de los celltypes para poder generar los clusters
# aqui esta la funcion de tsne https://github.com/crj32/M3C/blob/master/R/tsne.R


#instalacion del M3C que es el paquete con la funcion tsne

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

# The following initializes usage of Bioc devel
BiocManager::install(version='devel')

BiocManager::install("M3C")

library(M3C)
library(ggplot2)
View(pollen)



### EJEMPLO PARA LOS DATOS DE OXILIPINAS (FUNCION?)

### transponer 

df_transpose <- data.frame(t(Oxylipins_CGAs_FoamCells_tsne1))
tsne(df_transpose,colvec=c('gold'))
write.table(df_transpose,file = "df_transpose.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 


tsne(df_transpose,labels=TRUE)

write.table(df_transpose)

tsne(df_transpose,labels=as.factor(TTOs_names))



tsne(df_transpose,
     labels=scale(as.numeric(df_transpose[row.names(df_transpose)=='X.PGDM',])),
     controlscale = TRUE,scale=2)

tsne(df_transpose,
     labels=scale(as.numeric(df_transpose[row.names(df_transpose)=='X.PGE1',])),
     controlscale = TRUE,scale=2, text=as.factor(TTOs_names))


pca(df_transpose,labels=scale(as.numeric(df_transpose[row.names(df_transpose)=='X.PGDM',])),
    controlscale=TRUE,scale=2,text=as.factor(TTOs_names),
    textlabelsize = 2)

### ENSAYO FALTANTES



tsne(Faltantes,labels=as.factor(TTOs_names)) # no funciona cuando faltan datos 


### Base de datos MiSalud





MiSalud <- data.frame(t(BD_colciencias [c(-1:-8)]))
MiSalud_scaled <- scale(MiSalud)

Ciudad <- data.frame(t(BD_colciencias [c(-2:-103)]))
Sexo <- data.frame(t(BD_colciencias [c(-1, -3: -103)]))
Edad <- data.frame(t(BD_colciencias [c(-1:-2, -4: -103)]))
CAG <- data.frame(t(BD_colciencias [c(-1: -3, -5:-103)]))
estado <- data.frame(t(BD_colciencias [c(-1: -4, -6: -103)]))
feno_imc <- data.frame(t(BD_colciencias [c(-1: -5, -7: -103)]))
feno_cintura <- data.frame(t(BD_colciencias [c(-1: -6, -8: -103)]))
feno_grasa <- data.frame(t(BD_colciencias [c(-1: -7, -9: -103)]))





tsne(MiSalud)
tsne(MiSalud,labels=as.factor(feno_grasa))
tsne(MiSalud,
     labels=scale(as.numeric(MiSalud[row.names(MiSalud)=='LBP.ug.ml',])),
     controlscale = TRUE,scale=2, text=as.factor(CAG))

tsne(MiSalud_scaled)
tsne(MiSalud_scaled,labels=as.factor(feno_grasa))
tsne(MiSalud_scaled,
     labels=scale(as.numeric(MiSalud[row.names(MiSalud)=='IL18',])),
     controlscale = TRUE,scale=2, text=as.factor(CAG))


### Base de datos MiSalud reducida

MiSalud3 <- data.frame(t(BD_colciencias_reducida [c(-1:-8)]))
MiSalud3_scaled <- scale(MiSalud3)

Ciudad <- data.frame(t(BD_colciencias [c(-2:-103)]))
Sexo <- data.frame(t(BD_colciencias [c(-1, -3: -103)]))
Edad <- data.frame(t(BD_colciencias [c(-1:-2, -4: -103)]))
CAG <- data.frame(t(BD_colciencias [c(-1: -3, -5:-103)]))
estado <- data.frame(t(BD_colciencias [c(-1: -4, -6: -103)]))
feno_imc <- data.frame(t(BD_colciencias [c(-1: -5, -7: -103)]))
feno_cintura <- data.frame(t(BD_colciencias [c(-1: -6, -8: -103)]))
feno_grasa <- data.frame(t(BD_colciencias [c(-1: -7, -9: -103)]))

View(MiSalud3)

tsne(MiSalud3,labels=as.factor(CAG))
tsne(MiSalud3,
     labels=scale(as.numeric(MiSalud3[row.names(MiSalud3)=='CCL2',])),
     controlscale = TRUE,scale=2, text=as.factor(estado))

tsne(MiSalud_scaled)
tsne(MiSalud_scaled,labels=as.factor(feno_grasa))
tsne(MiSalud_scaled,
     labels=scale(as.numeric(MiSalud[row.names(MiSalud)=='IL18',])),
     controlscale = TRUE,scale=2, text=as.factor(CAG))



pca(MiSalud,labels=scale(as.numeric(MiSalud[row.names(MiSalud)=='IL18',])),
    controlscale=TRUE,scale=2,text=as.factor(CAG),
    textlabelsize = 2)

?tsne
## MiSalud escalado


?pca
tsne(MiSalud_scaled, perplex = 15)
tsne(MiSalud_scaled,labels=as.factor(CAG))
tsne(MiSalud_scaled,
     labels=scale(as.numeric(MiSalud_scaled[row.names(MiSalud_scaled)=='IL18',])),
     controlscale = TRUE,scale=2, text=as.factor(CAG))

pca(MiSalud_scaled,labels=scale(as.numeric(MiSalud_scaled[row.names(MiSalud_scaled)=='IL18',])),
    controlscale=TRUE,scale=2,text=as.factor(CAG),
    textlabelsize = 2)


###3r intento ( la BD de esta forma no se puede correr con la funcion tsne del M3c (transponer columnas y filas))
Missing <- is.na(BD_colciencias)
write.table(Missing,file = "Missing.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

BD <- BD_colciencias [c(-1:-8)]
Missing <- is.na(BD)

Ciudad1 <- BD_colciencias [c(-2:-103)]
Sexo1 <- BD_colciencias [c(-1, -3: -103)]
Edad1 <- BD_colciencias [c(-1:-2, -4: -103)]
CAG1 <- BD_colciencias [c(-1: -3, -5:-103)]
estado1 <- BD_colciencias [c(-1: -4, -6: -103)]
feno_imc1 <- BD_colciencias [c(-1: -5, -7: -103)]
feno_cintura1 <- BD_colciencias [c(-1: -6, -8: -103)]
feno_grasa1 <- BD_colciencias [c(-1: -7, -9: -103)]

view(CAG1)

MiSalud1 <- data.frame(t(BD_colciencias))

Ciudad2 <- data.frame(t(BD_colciencias [c(-2:-103)]))
Sexo2 <- data.frame(t(BD_colciencias [c(-1, -3: -103)]))
Edad2 <- data.frame(t(BD_colciencias [c(-1:-2, -4: -103)]))
CAG2 <- data.frame(t(BD_colciencias [c(-1: -3, -5:-103)]))
estado2 <- data.frame(t(BD_colciencias [c(-1: -4, -6: -103)]))
feno_imc2 <- data.frame(t(BD_colciencias [c(-1: -5, -7: -103)]))
feno_cintura2 <- data.frame(t(BD_colciencias [c(-1: -6, -8: -103)]))
feno_grasa2 <- data.frame(t(BD_colciencias [c(-1: -7, -9: -103)]))


library(Rtsne)

prueba <- Rtsne(BD)

plot(prueba$Y,  main="tsne")
text(prueba$Y, labels=BD_colciencias$CAG,  col= "black")

?plot


tsne(BD)

tsne(BD,
     labels=scale(BD$IL18))

pca(MiSalud,labels=scale(as.numeric(MiSalud[row.names(MiSalud)=='IL18',])),
    controlscale=TRUE,scale=2,text=as.factor(CAG),
    textlabelsize = 2)

?tsne

tsne(BD,
     labels=scale(as.numeric(BD$IL18)),
     controlscale = TRUE,scale=2)

pca(MiSalud,labels=scale(as.numeric(MiSalud[row.names(MiSalud)=='IL18',])),
    controlscale=TRUE,scale=2,text=as.factor(CAG),
    textlabelsize = 2)

tsne(BD,
     labels=scale(as.numeric(MiSalud[row.names(MiSalud)=='Adiponectin',])),
     controlscale = TRUE,scale=2, text=as.factor(CAG))

?row.names

tsne_results <- Rtsne(BD, perplexity=30, check_duplicates = FALSE)
par(mfrow=c(1,2))
plot(tsne_results$Y, col = "blue", pch = 19, cex = 1.5)
plot(tsne_results$Y, col = "blue", bg= CAG1, pch = 21, cex = 1.5)

?tsne
?colnames
### PCA
# Principal components analysis
#Ejemplo
res.pca <- PCA(BD_colciencias_completa, scale.unit = TRUE, ncp = 10, graph = true)
eig.val <- get_eigenvalue(res.pca)
eig.val
write.table(eig.val,file = "Autovalores.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 100))

#visualizaci?n (reemplzar BD$groups por la base de datos y la variable categorica para poner los nombres, en este caso se tienen 3 grupos con 74 individuos)
ScorePlot = fviz_pca_ind(res.pca,axes = c(1,2), geom.ind = c("point", "text"), fill.ind = "black", pointshape = 21, pointsize = 3, labelsize = 5, center = c(0, 1),  legend.title = list(fill = "GROUP", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "norm", addEllipses = FALSE)
ggpubr::ggpar(ScorePlot, title = "Baseline", subtitle = "Lipidomic data set", caption = FALSE, xlab = "PC1(31.7%)", ylab = "PC2(13.5%)", legend.position = "top", legendsize = 8)

LoadingPlot = fviz_pca_var(res.pca, axes = c(1,2), geom.var = c("text","point"), pointshape = 21, pointsize = 5, labelsize = 3, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(color = "QUALITY"), repel = TRUE, mean.point = FALSE, ellipse.type = "convex", addEllipses = FALSE)
ggpubr::ggpar(LoadingPlot, title = "Baseline", subtitle = "Lipidomic data set", caption = FALSE, xlab = "PC1(22.3%)", ylab = "PC2(16.5%)", legend.position = "top", legendsize = 8)


Biplot = fviz_pca_biplot(res.pca, geom.var = c("text","point"), geom.ind = c("point"), fill.ind = BD$Groups, palette = c( "#ecd817", "#b8360a", "#fbfaf9"), pointshape = 21, pointsize = 5, labelsize = 3, center = c(0, 1), fill.var = "black", col.var = "cos2", gradient.cols = c("#d06a15", "#90460a", "#4d2605"), legend.title = list(fill = "Groups", color = "QUALITY"), repel = TRUE, mean.point = TRUE, ellipse.type = "convex", addEllipses = FALSE)
ggpubr::ggpar(Biplot, title = "Baseline", subtitle = "Lipidomic data set", caption = FALSE, xlab = "PC1(22.3%)", ylab = "PC2(13.5%)", legend.position = "top", legendsize = 8)




Missing <- is.na(MiSalud)
is.na(MiSalud)
write.table(Missing,file = "Missing.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 
rm(BD_colciencias_completa)

tsne(df_transpose,labels=as.factor(TTOs_names))
?tsne
rm(MiSalud)

#LIBRERIAS (ejecutar dos veces, todas al tiempo)
library(factoextra) 
library(utils) 
library(stats) 
library(FactoMineR) 
library(ggfortify) 
library(ggplot2) 
library("corrplot") 
library(cluster) 
library(ggpubr) 
library(magrittr) 
library("NbClust") 
library(REdaS) 
library(DiscriMiner)
library(corrr)
library(tidyverse)
library(ggraph)
library(devtools)
library(igraph)
library(dplyr)

#RED DE CORRELACIONES

BD <- BD_colciencias [c(-1:-8)]
BD1 <- BD_colciencias [c(-1:-3, -5:-8)]
BD_scaled <- scale(BD)


BDr <- BD_colciencias1 [c(-1:-8)]
BDr_scaled <- scale(BDr)

rm (BDr)

#Reemplazar nombre
# Create a tidy data frame of correlations
tidy_cors <- BDr_scaled %>% 
  correlate() %>% 
  stretch()

# Convert correlations stronger than some value
# to an undirected graph object


graph_cors <- tidy_cors %>% 
  filter(abs(r) > 0.27) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +  #cambie el edge_width = abs(r) por none
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#0971b0", "#fd240b")) +
  geom_node_point(color = "black", size = 4) +
  geom_node_text(aes(label = name),size = 6, repel = TRUE) +
  theme_graph() +
  labs(title = "BD_MiSalud")

rm(graph_cors)

?ggraph
#CORRELACIONES
#Reemplazar nombre
?corrplot
correlations <- (cor(BD_scaled)) 
correlations
corrplot(correlations, is.corr=FALSE, method="circle", type = "upper", tl.cex = 0.4, tl.srt = 90)
write.table(correlations,file = "correlations.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

#ajuste valor P en la matris
cor.mtest <- function(BD_scaled, ...) {
  BD_scaled <- as.matrix(BD_scaled)
  n <- ncol(BD_scaled)
  p.BD_scaled<- matrix(NA, n, n)
  diag(p.BD_scaled) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(BD_scaled[, i], BD_scaled[, j], ...)
      p.BD_scaled[i, j] <- p.BD_scaled[j, i] <- tmp$p.value
    }
  }
  colnames(p.BD_scaled) <- rownames(p.BD_scaled) <- colnames(BD_scaled)
  p.BD_scaled
}

# matrix of the p-value of the correlation
p.BD_scaled <- cor.mtest(BD_scaled)
head(p.BD_scaled[, 1:5])
#write.table(p.Lipidomic_cambio,file = "Lipidomic_cambio.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

#generar el corrplot
corrplot(correlations, is.corr=FALSE, method="square", type = "upper", p.mat = p.BD_scaled, sig.level = 0.05, tl.cex = 0.4, tl.srt = 90,insig = "blank")
corrplot(correlations, is.corr=FALSE, method="square", type = "upper", order="hclust", p.mat = p.BD_scaled, sig.level = 0.05, tl.cex = 0.4, tl.srt = 90,insig = "blank")

correlations1 <-  corrplot(correlations, is.corr=FALSE, method="square", type = "upper", p.mat = p.BD_scaled, sig.level = 0.05, tl.cex = 0.4, tl.srt = 90,insig = "blank")
write.table(correlations1,file = "correlations_P_value.xl", sep = "\t", eol = "\n", dec = ".", row.names = TRUE, col.names = TRUE) 

### correlacion por CAG
BD1 <- BD_colciencias1 [c(-1:-4, -6:-8, -11, -13,-14, -17, -20:-22, -26,  -27:-30, -53:-66)]

#adipoquinas vs feno_imc
BD2 <- BD_colciencias1 [c(-1:-5, -7:-30, -53:-66)]
BD3 <- BD_colciencias1 [c(-1:-5, -7, -8, -10: -23,-25: -52)]

rm(BD1_sub)

View(BD1_sub)

BD1_sub <- BD3 %>%
  group_by(feno_imc) %>%
  select_if(is.numeric)

BD1_sub<- BD1 %>%
  group_by(CAG)

rm(BD1_sub)

BD1_cor <- BD1_sub %>%
  group_by(feno_imc) %>% ## redundant but worth it for illustration
  nest() %>%
  mutate(data = map(data, purrr::compose(stretch, correlate))) %>% 
  unnest() %>%
  select(x, y, r, feno_imc) %>%
  filter(abs(r) > .35) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(BD1_cor, layout = "kk") +
  geom_edge_link(aes(edge_alpha = abs(r), color = r), edge_width = 2) +
  guides(edge_alpha = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = heat.colors(5)) +
  geom_node_point(color = "black", size = 3) +
  geom_node_text(aes(label = name), repel = TRUE) +
  facet_edges(~feno_imc) +
  theme_minimal() 



## oxilipinas y IMC
BD4 <- BD_colciencias1 [c(-1:-8, -10: -23,-25: -52)]

BD4_scaled <- scale(BD4)

tidy_cors <- BD4_scaled %>% 
  correlate() %>% 
  stretch()

# Convert correlations stronger than some value
# to an undirected graph object


graph_cors <- tidy_cors %>% 
  filter(abs(r) > 0.25) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +  #cambie el edge_width = abs(r) por none
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("#0971b0", "#fd240b")) +
  geom_node_point(color = "black", size = 4) +
  geom_node_text(aes(label = name),size = 6, repel = TRUE) +
  theme_graph() +
  labs(title = "BD_Oxyk_IMC")

