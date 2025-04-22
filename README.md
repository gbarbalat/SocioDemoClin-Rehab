# SocioDemoClin-Rehab

This project investigates whether socio-demographic factors (such as age, sex, employment status) and basic clinical predictors (including clinical severity, global assessment of functioning, and clinical history) can predict rehabilitation outcomes. These outcomes include various aspects of quality of life, autonomy, self-esteem, self-stigmatization, adherence to treatment, and insight.

## Research Question
Do rehabilitation outcomes depend on characteristics that are typically measured in clinical settings?

If the answer is yes, this would suggest that rehabilitation outcomes are similar to other clinical outcomes and may be predicted using standard clinical and demographic variables.

If the answer is no, it may indicate that rehabilitation outcomes are distinct from traditional clinical outcomes, highlighting the need for specialized approaches and potentially supporting the value of sub-specialized rehabilitation programs.

## Methodology
To address this question, we will employ a predictive modeling strategy using ensemble machine learning techniques. Specifically, we will use a SuperLearner approach, combining multiple base learners to assess whether socio-demographic and basic clinical characteristics can reliably predict rehabilitation outcomes.

## Data
From REHABase.
Background factors (see variable list below).
Typical cleaning (regrouping rare levels).
Variables:
"CENTRE", "AGE_MEDSOC", "SEXE", "NIVETUD_cat", "COMOR_PSY", "Rx", "addict", "SOMA_1", "TTTSOMA_1", "EGF", "CGI_SEVERITE", "SIT_FAM_cat", "ETRE_PARENT", "ADRSSR_cat", "LGMT_cat", "SIT_PRO_cat", "RQTH", "DUREE_MALADIE", "NBR_HOSPI", "DUREE_HOSPI", "TS", "NBR_TS", "MARGIN_ACTPASS", "ATCD_MEDLEG"

## Initial analysis plan  
Based on a previous study whose analysis plan is in https://github.com/gbarbalat/predict-missing-REHABase

### Strategy for missing data
Imputation: Use R mice package.
Keep all observations (if possible based on outflux-influx plots and the lambda parameter).
Discard observations with full missingness.
Auxiliary variables: Not planned, due to similar missingness process across variables.
m = 15 (more thn the pct of missing data). maxit = 10. Standard mice imputation models.
Split sample: Imputation will precede predictive modeling and will be done separately in each training vs. testing sets using the ignore argument in the mice function


### Predictive modeling
SuperLearner ensemble model (R SuperLearner package).
Basis learners: glm, interactions, regularized regression, RF, XGBoost.
Do to increased computational time, we had to refrain from using a glm interaction algorithm without any pre-screen.
Ex-ante screens: RF importance.
caret adaptive hyperparameter tuning (tuneLength=10, nfolds=10).
V=8 folds.
stratifyCV=TRUE.
CIMENT server (Grenoble). Reduce V or algorithms/screens if needed.
Assess stability of missing data imputation methods by using Fraction of Missing Information-Lambda parameter. Remove variables with high FMI
Training (70%) / Testing split.


### Variable importance
fastshap R package: SHAP values (nsim=100) for each fold and training observation.
SHAP plots with shapviz R package.
set.seed
set.seed=123.


### Results
Fit calculated over 15 imputed datasets.
SHAP values calculated over 15 imputed datasets.
