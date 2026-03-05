rm(list=ls())

#load packages
library(gtsummary)
library(purrr)
library(stringr)
library(dplyr)
library(forcats)
library(tidyr)
library(glue)
library(ggplot2)
library(patchwork)
library(data.table)

SHAP_DATA <- list()
m <- 20
k_outer <- 1
top_n_select <- 20
top_n_select_oneWay <- 10

#Var names ----
#basic set of predictors
all_features <- c(
  "CENTRE",
  "AGE_MEDSOC",
  "SEXE_MEDSOC",
  "NIVETUD_MEDSOC_cat",
  #"LISTE_CLASSE_DIAG1R",#not if anal only in one dx group
  
  "COMOR_PSY", 
  "Rx",#"FGA", "SGA", "noRx",#"ANX", "THY", "ATD", 
  "addict",
  "SOMA_1","TTTSOMA_1",
  "EGF",	
  "CGI_SEVERITE",
  
  "SIT_FAM_cat", "ETRE_PARENT",
  "ADRSSR_cat",
  "LGMT_cat",
  "SIT_PRO_cat","RQTH",
  "DUREE_MALADIE_cat",
  # "NBR_HOSPI_cat", 
  # "DUREE_HOSPI_cat",
  "NBR_TS",#"NBR_TS","TS",
  "MARGIN_hx",#"MARGIN", "MARGIN_ACTPASS",
  "ATCD_MEDLEG",
  
  "outcome" #outcome
  )

all_features_goodNames <- c(
  "CENTRE",
  "AGE",
  "SEX",
  "EDUCATION",
  #"LISTE_CLASSE_DIAG1R",#not if anal only in one dx group
  
  "PSYCH. COMORB.", 
  "ANTIPSYCHOTICS",#"PSYCH. Rx",#"FGA", "SGA", "noRx",#"ANX", "THY", "ATD", 
  "ADDICTIONS",
  "PHYS. COMORB.","PHYS. Rx",
  "GAF",	
  "CGI",
  
  "MARITAL STATUS", "CHILDREN",
  "REFERRER",
  "HOUSING",
  "EMPLOYMENT","DIS. WORK. BENEF.",
  "DUR. ILLNESS",
  #"# ADMISSION", 
  #"DUR. ADMISSION",
  "SUICIDE ATT.",#"NBR_TS","TS",
  "NO FIXED ABODE",#"MARGIN", "MARGIN_ACTPASS",
  "FORENSIC Hx",
  
  "outcome" #outcome
)
all_features_goodNames <- ifelse(
  all_features_goodNames %in% c("GAF", "CGI"),
  all_features_goodNames,
  str_to_title(all_features_goodNames)
)

load("all_feature_values.RData")
all_feature_values_goodNames <- c(
  "01_SUR/CL3R/TS2A", "11_CESAR_SAMSAH", "02_C3R", "03_REHALISE", "04_C2RL", "05_C2RP", "06_CReATIV", "07_CRCR", "Other", 
  "09_CPA01", "19_REPI", "2", "0", "1", "4", "3", "10+", "5-10 yrs", "No", "Yes", "]0-3[", "Nil", "[6-12[", "[3-6[", 
  "12 mths+", "<2 yrs", "10 yrs+", "2-5 yrs", "Bach.", "No dipl.", "Mast.", 
  "HS dipl.", "Fam. H.", "Pers. H.", "Hless", "Gp H.", "Pub. HC", 
  "Pr. HC", "Pat.", "Soc. W.", "Nil", "Subst.", "Both", "Behav.", "SGA", "Both", "Nil",
  "FGA", "Current", "Past", "4+", "Unempl.", "Empl.(reg.)", "Empl.(spec.)", "Male", "Female", 
  "Single", "In a rel.", "Div./Wid."
)


all_algo <- c(
  "SL.mean_All", "SL.caret.xgboost_All", "SL.caret.xgboost_screen.randomForest", 
  "SL.caret.ranger_All", "SL.caret.ranger_screen.randomForest",
  "SL.glm_All", "SL.glm_screen.randomForest", 
  "SL.caret.step.interaction_screen.randomForest", 
  "SL.caret.glmnet_All", "SL.caret.glmnet_screen.randomForest"
)
all_algo_goodNames <- c(
  "Mean", "XGBoost", "XGBoost - RF screen", 
  "RF", "RF - RF screen", 
  "GLM", "GLM - RF screen", 
  "GLM IA - RF screen", 
  "GLMNET", "GLMNET - RF screen"
)
all_algo_goodNames <- c(
  "Mean", 
  "XGB", "XGB - RF scr.", 
  "RF", "RF - RF scr.", 
  "GLM", "GLM - RF scr.", 
  "GLM Int - RF scr.", 
  "GLMNet",  "GLMNet - RF scr."
)

#function to rename levels
rename_levels <- function(df) {
  for (col in names(df)) {
    if (!is.numeric(df[[col]])) {
      #make it a factor
      df[[col]] <- as.factor(df[[col]])
      
      #features
      levels(df[[col]]) <- sapply(levels(df[[col]]), function(x) {
        match_index <- which(all_features == x)
        if (length(match_index) > 0) {
          all_features_goodNames[match_index[1]]
        } else {
          x
        }
      })
      
      #feature levels
      levels(df[[col]]) <- sapply(levels(df[[col]]), function(x) {
        match_index <- which(all_feature_values == x)
        if (length(match_index) > 0) {
          all_feature_values_goodNames[match_index[1]]
        } else {
          x
        }
      })
      
      #algo
      levels(df[[col]]) <- sapply(levels(df[[col]]), function(x) {
        match_index <- which(all_algo == x)
        if (length(match_index) > 0) {
          all_algo_goodNames[match_index[1]]
        } else {
          x
        }
      })
      
      #varOI
      levels(df[[col]]) <- sapply(levels(df[[col]]), function(x) {
        match_index <- which(all_varOI == x)
        if (length(match_index) > 0) {
          all_varOI_goodNames[match_index[1]]
        } else {
          x
        }
      })
      
    }#if
  }#for
  return(df)
}

#load and arrange data ----
#load all datasets from RData files
rdata_files <- list.files(path="C://Users/Guillaume/Desktop/SocioDemo--Rehab/results_xBL/",pattern="^EAS.*\\.RData$")
all_varOI <- sub("_anal.*", "", rdata_files)
all_varOI_goodNames <- c("WEMWBS", "SERS", "BIS", "SQOL18", "MARS",  "ISMI")
all_varOI_goodNames <- c(
   "SAS: SOC",#vie affective et relations sociales
   "SAS: COM", #relations avec exterieur - organisation trspt, sorties, comm, organiser une journee    
   "SAS: FIN",#gestion des ressources
   "SAS: PER",#soins personnels
   "SAS: BAS"#gestion de la vie quotidienne
)

for (i in 1:length(rdata_files)) {
  
  load(file = paste0("C://Users/Guillaume/Desktop/SocioDemo--Rehab/results_xBL/",rdata_files[i]))
  outcome_name <- sub("_anal.*", "", rdata_files[i])

  #print N and total number of missing values
  print(rdata_files[i]); print(nrow(MERGED_IMPUTED[[1]]$data)); ((sum(is.na(MERGED_IMPUTED[[1]]$data))) / prod(dim(MERGED_IMPUTED[[1]]$data))) %>% print
  #next 
  
  ##for future Table 1 ----
  assign(paste0("final_table",i), MERGED_IMPUTED[[1]]$data %>%
    mutate(across(where(is.factor), ~fct_explicit_na(.x, na_level = "(Missing)"))) %>%
    
    rename(!!outcome_name:=outcome) %>%
      
    tbl_summary(
      type=list(CGI_SEVERITE ~"continuous",
                !!sym(outcome_name) ~"continuous"),
      statistic = all_continuous() ~"{mean}, ({sd})",
      missing_text = "(Missing)",
      missing_stat = "{N_miss} ({p_miss}%)"
    )
  )
  
  ## performance metrics ----
  r2_results <- map_dfr(1:20, function(x) {
    
    predictions <- as.data.frame(SL[[1]][[x]]$library.predict) %>%
      mutate(SL_ens = SL[[1]][[x]]$SL.predict)
    observed <- IMP_TRAIN[[1]][[x]]$outcome
    
    r2_values <- c(apply(predictions,2,function(pred_col) {
      caret::R2(pred=pred_col, obs=observed)
    }),
    SL_ens_test=R2[[1]][[x]]
    )
      
    
    tibble::tibble(
      x=x,
      algorithm=names(r2_values),
      R2=r2_values
    )
    
  }) %>% 
  mutate(outcome=outcome_name)
  assign(paste0("r2_all",i), r2_results
         )
  
  ##for coeff of the SL ----
  list_of_dfs <- lapply(1:20, function(x) {
    
    coef_vec <- SL[[1]][[x]]$coef
    as.data.frame(as.list(coef_vec))
    
  })
  assign(paste0("coef_SL",i), do.call(rbind, list_of_dfs) %>% 
           mutate(outcome=outcome_name)
  )
}



#Table 1----
table1_EAS <- MERGED_IMPUTED[[1]]$data %>%
         mutate(across(where(is.factor), ~fct_explicit_na(.x, na_level = "(Missing)"))) %>%
         rename(!!outcome_name:=outcome) 

table1_raw <- rename_levels(table1_EAS) 
table1_raw %>% tbl_summary(
           type=list(CGI_SEVERITE ~"continuous",
                     !!sym(outcome_name) ~"continuous"),
           statistic = all_continuous() ~"{mean}, ({sd})",
           missing_text = "(Missing)",
           missing_stat = "{N_miss} ({p_miss}%)"
         )




extract_summary_stats <- function(tbl_summary_obj) {
  
  tbl_summary_obj$table_body %>%
    select(variable,row_type,label,stat_0) %>%
    filter(row_type %in% c("level","missing")) %>%
    mutate(
      #n=as.numeric(str_extract(stat_0,"\\d+")),
      n=str_extract(stat_0, "\\d{1,3}(?:,\\d{3})*") %>%
        str_remove_all(",") %>%
        as.numeric(),
      pct=as.numeric(str_extract(stat_0,"(?<=\\()\\d+\\.?\\d*(?=%)"))
    )
  
}

extract_cont_stats <- function(tbl_summary_obj) {
  
  tbl_summary_obj$table_body %>%
    select(variable,var_type,row_type,label,stat_0) %>%
    filter(var_type=="continuous") 
}


all_stats <- list(final_table1,final_table2,final_table3,final_table4,final_table5) %>%
  map_dfr(extract_summary_stats,.id="dataset")
pre_cont_stats <- list(final_table1,final_table2,final_table3,final_table4,final_table5) %>%
  map_dfr(extract_cont_stats,.id="dataset")


cat_stats <- all_stats %>%
  group_by(variable, label, row_type) %>%
  summarise(
    n_range=paste0(min(n, na.rm=TRUE), "-", max(n, na.rm=TRUE)),
    pct_range=paste0(
      round(min(pct, na.rm=TRUE),1), "%-",
      round(max(pct, na.rm=TRUE),1), "%"
    ),
      .groups = "drop"
)
    
cont_stats <- pre_cont_stats %>%
  mutate(
    mean=str_extract(stat_0, "^[-+]?\\d{1,3}(?:,\\d{3})*\\.?\\d*") %>%
      str_remove_all(",") %>%
      as.numeric(),
    sd=str_extract(stat_0, "(?<=\\()[-+]?\\d{1,3}(?:,\\d{3})*\\.?\\d*(?=\\))") %>%
      str_remove_all(",") %>%
      as.numeric() 
    ) %>%
  group_by(variable, label) %>%
  summarise(
    mean_range=paste0(
        round(min(mean, na.rm=TRUE),1), "-",
        round(max(mean, na.rm=TRUE),1)
      ),
    sd_range=paste0(
      round(min(sd, na.rm=TRUE),1), "-",
      round(max(sd, na.rm=TRUE),1)
    ),
    .groups="drop"
    )

#combine cat and cont
final_table <- bind_rows(
  cat_stats %>%
    mutate(stat_range=glue("{n_range} ({pct_range})")) %>%
    select(variable, label, stat_range)
  , 
  cont_stats %>%
    mutate(stat_range=glue("{mean_range} ({sd_range})")) %>%
    select(variable, label, stat_range)
)
table1_raw <- rename_levels(final_table) 


#format with gt
table1_raw %>%
  gt::gt(groupname_col = "variable") %>%
  gt::cols_label(
    variable="Variable",
    label="Category",
    stat_range="Range (N and % / Mean +- SD)"
    )

#Performance metrics ----
combined_df <- bind_rows(r2_all1,r2_all2,r2_all3,r2_all4,r2_all5
) %>% 
  pivot_wider(
    names_from = outcome,
    values_from = R2
  ) %>%
  unnest(where(is.list)) %>%
  select(-x)
all_r2 <- rename_levels(combined_df) 
# Create a named vector for renaming
rename_vector <- setNames(all_varOI_goodNames, all_varOI)
# Rename the columns and change order of columns and levels
colnames(all_r2) <- rename_vector[colnames(all_r2)]; colnames(all_r2)[1] <- "algorithm"
all_r2 <- all_r2 %>% select(algorithm,all_of(all_varOI_goodNames))


all_r2 %>%
  tbl_summary(
    by=algorithm,
    statistic=all_continuous() ~"{mean} ({sd})",
    missing="no"
  )  



#coefficients of the SL ----
combined_df <- bind_rows(coef_SL1,coef_SL2,coef_SL3,coef_SL4,coef_SL5
                         ) %>%
  pivot_longer(
    cols=-outcome, 
    names_to = "algo",
    values_to = "value"
  ) %>% 
  pivot_wider(
    names_from = outcome,
    values_from = value
  ) %>%
  unnest(where(is.list))

all_coef <- rename_levels(combined_df) 
# Create a named vector for renaming
rename_vector <- setNames(all_varOI_goodNames, all_varOI)
# Rename the columns and change order of columns and levels
colnames(all_coef) <- rename_vector[colnames(all_coef)]; colnames(all_coef)[1] <- "algo"
all_coef <- all_coef %>% select(algo,all_of(all_varOI_goodNames))

all_coef %>%
  tbl_summary(
    by=algo, 
    statistic=all_continuous() ~"{mean} ({sd})"
  ) 

stop()


#Var importance ----
#which_file <- "EAS_SOINSPERSO_anal_t0_noLeak_SocioDemo_Rehab.RData"
#outcome_name <- sub("_anal.*", "", which_file)

# [1] "EAS_AFFECTIVE_anal_t0_noLeak_SocioDemo_Rehab.RData"              
# [2] "EAS_RELATIONSEXT_anal_t0_noLeak_SocioDemo_Rehab.RData"           
# [3] "EAS_RESSOURCES_anal_t0_noLeak_SocioDemo_Rehab.RData"             
# [4] "EAS_SOINSPERSO_anal_t0_noLeak_SocioDemo_Rehab.RData"             
# [5] "EAS_VIEQUOT_anal_t0_noLeak_SocioDemo_Rehab.RData"    
all_which_file <- c(
  "EAS_VIEQUOT_anal_t0_noLeak_SocioDemo_Rehab.RData",
  "EAS_RESSOURCES_anal_t0_noLeak_SocioDemo_Rehab.RData"   ,
  "EAS_RELATIONSEXT_anal_t0_noLeak_SocioDemo_Rehab.RData" ,   
  "EAS_AFFECTIVE_anal_t0_noLeak_SocioDemo_Rehab.RData"
)

fn_all_var_imp <- function(which_file) {
load(file = paste0("C://Users/Guillaume/Desktop/SocioDemo--Rehab/results_xBL/",which_file))
outcome_name <- sub("_anal.*", "", which_file)
  
#loop over outer folds
for (outer_idx in 1:k_outer) {
  
  SHAP_DATA[[outer_idx]] <- list()
  
  #loop over imputed datasets
  for (m_idx in 1:m) {
    IMP_TEST[[outer_idx]][[m_idx]] -> imp.test
    IMP_TRAIN[[outer_idx]][[m_idx]] -> imp.train
    
    SHAP_VIZ[[outer_idx]][[m_idx]] -> Shap_values
    shap_df <- as.data.frame(Shap_values)
    
    #Calculate mean absolute SHAP values to determine feature importance:
    mean_shap <- colMeans(abs(shap_df))
    sorted_features <- names(sort(mean_shap, decreasing = TRUE))
    
    #Select the top N most important features (e.g., top 5):
    top_features <- head(sorted_features, top_n_select)
    
    #Create dependence plots for each top feature:
    shap_viz <- shapviz::shapviz(Shap_values,
                        X = imp.train %>% dplyr::select(-outcome)
    )
    
    SHAP_DATA[[outer_idx]][[m_idx]] <- lapply(top_features, function(feature) {
      
      shap_data <- data.frame(
        feature_name = feature,
        feature_value = shap_viz$X[[feature]],
        shap_value = as.data.frame(shap_viz$S)[[feature]],
        imp_dataset=m_idx
      )
      ggplot(shap_data, aes(x = feature_value, y = shap_value)) +
        geom_jitter(width = 0.2, height = 0) +
        labs(x = "Feature Value", y = "SHAP Value", title = paste("SHAP vs", feature)) +
        theme_minimal()
      
      shap_data
      
    })
  }
}


# Combine all data into a single data frame
all_data <- rbindlist(
  lapply(1:m, function(y) {
    rbindlist(lapply(1:length(SHAP_DATA[[k_outer]][[y]]), function(z) {
      SHAP_DATA[[k_outer]][[y]][[z]]
    }))
  })
) %>%
  mutate(outcome=outcome_name)

importance_data <- all_data %>%
  group_by(feature_name) %>%
  summarize(mean_abs_shap = mean(abs(shap_value))) %>%
  arrange(desc(mean_abs_shap)) 
importance_data <- rename_levels(importance_data)

pos_OI <- which(all_varOI == outcome_name)
# Create the plot
ggp_imp <- ggplot(importance_data, aes(x = reorder(feature_name, mean_abs_shap), y = mean_abs_shap)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Feature", y = "Mean Absolute SHAP Value", title = all_varOI_goodNames[pos_OI]

       ) +
  theme_minimal() + 
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13))
ggp_imp
}
all_gg_imp <- lapply(all_which_file,fn_all_var_imp)
#all_gg_imp <- lapply(rdata_files[1],fn_all_var_imp)

# Combine and collect axes
wrap_plots(all_gg_imp, nrow = 2, ncol = 2) +
  plot_layout(axis_titles = "collect")



# One-way dpd plot for each feature ----
# Define the releveling rules for each feature
order_lookup <- list(
  Housing = c("Gp H.", "Fam. H.", "Pers. H.", "Hless"),
  Education = c("No dipl.", "HS dipl.", "Bach.", "Mast."),
  `Dur. Illness` = c("<2 yrs", "2-5 yrs", "5-10 yrs", "10 yrs+"),
  Antipsychotics = c("Nil", "FGA", "SGA", "Both"),
  Referrer = c("Pub. HC", "Pr. HC", "Soc. W.", "Pat.", "Other"),
  Addictions = c("Nil", "Behav.", "Subst.", "Both"),
  `No Fixed Abode` = c("No", "Past", "Current"),
  `Marital Status` = c("In a rel.", "Div./Wid.", "Single")
)
fn_all_one_way <- function(which_file, which_filter) {
  load(file = paste0("C://Users/Guillaume/Desktop/SocioDemo--Rehab/results_xBL/",which_file))
  outcome_name <- sub("_anal.*", "", which_file)
  
  #loop over outer folds
  for (outer_idx in 1:k_outer) {
    
    SHAP_DATA[[outer_idx]] <- list()
    
    #loop over imputed datasets
    for (m_idx in 1:m) {
      IMP_TEST[[outer_idx]][[m_idx]] -> imp.test
      IMP_TRAIN[[outer_idx]][[m_idx]] -> imp.train
      
      SHAP_VIZ[[outer_idx]][[m_idx]] -> Shap_values
      shap_df <- as.data.frame(Shap_values)
      
      #Calculate mean absolute SHAP values to determine feature importance:
      mean_shap <- colMeans(abs(shap_df))
      sorted_features <- names(sort(mean_shap, decreasing = TRUE))  %>% setdiff("CENTRE")
      
      #Select the top or bottom N most important features:
      if (which_filter=="top") {
      top_features <- head(sorted_features, top_n_select_oneWay)
      }
      else if (which_filter=="bottom") {
        top_features <- tail(sorted_features, top_n_select_oneWay)
      }
      
      #Create dependence plots for each top feature:
      shap_viz <- shapviz::shapviz(Shap_values,
                                   X = imp.train %>% dplyr::select(-outcome)
      )
      
      SHAP_DATA[[outer_idx]][[m_idx]] <- lapply(top_features, function(feature) {
        
        shap_data <- data.frame(
          feature_name = feature,
          feature_value = shap_viz$X[[feature]],
          shap_value = as.data.frame(shap_viz$S)[[feature]],
          imp_dataset=m_idx
        )
        ggplot(shap_data, aes(x = feature_value, y = shap_value)) +
          geom_jitter(width = 0.2, height = 0) +
          labs(x = "Feature Value", y = "SHAP Value", title = paste("SHAP vs", feature)) +
          theme_minimal() +

          theme(axis.text = element_text(size = 11),
                axis.title = element_text(size = 13))
        
        shap_data
        
      })
    }
  }
  
  
  # Combine all data into a single data frame
  all_data <- rbindlist(
    lapply(1:m, function(y) {
      rbindlist(lapply(1:length(SHAP_DATA[[k_outer]][[y]]), function(z) {
        SHAP_DATA[[k_outer]][[y]][[z]]
      }))
    })
  ) %>%
    mutate(outcome=outcome_name)
num_features <- c("AGE_MEDSOC","EGF", "CGI_SEVERITE")
all_gg <- list()

for (feature in top_features) {
  feature_data <- all_data %>%
    filter(feature_name==feature)
  
  if(feature %in% num_features) {
  feature_data$feature_value <- as.numeric(as.character(feature_data$feature_value))
  } else if (feature=="NBR_TS") {
    feature_data$feature_value <- factor(feature_data$feature_value, levels = c("0","1","2","3","4+"))
  } else if (feature=="NBR_HOSPI_cat") {
    feature_data$feature_value <- factor(feature_data$feature_value, levels = c("0","1","2","3","4","[5-10[","10 et plus"))
  } else if (feature=="DUREE_MALADIE_cat") {
    feature_data$feature_value <- factor(feature_data$feature_value, levels = c("<2 ans","[2-5[","[5-10[","10 ans et plus"))
  }
  
  #relevel
  feature_data <- rename_levels(feature_data)
    # Relevel feature_value for the current feature if it exists in order_lookup
  if (unique(feature_data$feature_name) %in% names(order_lookup)) {
    feature_data$feature_value <- factor(feature_data$feature_value, 
                                         levels=order_lookup[[paste0(unique(feature_data$feature_name))]])
  }
  
  ggp_oneway <- ggplot(feature_data, aes(x = feature_value, y = shap_value)) + #, color = factor(imp_dataset)
  geom_jitter(alpha = 0.6, width = 0.2, height = 0) +
  #facet_wrap(~ feature_name, scales = "free", ncol = 4) +
  scale_color_viridis_d() +
  labs(x = "Feature Value", y = "SHAP Value") + #, color = "Imputed Dataset"
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "none"#, 
    #axis.title = element_blank()
  ) +
  ggtitle(feature_data$feature_name)
ggp_oneway %>% print

all_gg[[feature]] <- ggp_oneway

}
return(all_gg)
}
all_gg_one_way <- fn_all_one_way(which_file=all_which_file[2], which_filter = "top")#top or bottom
# Combine and collect axes
wrap_plots(all_gg_one_way, nrow = 5, ncol = 2) +
  plot_layout(axis_titles = "collect")
