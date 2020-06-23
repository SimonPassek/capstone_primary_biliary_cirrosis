# load/install libraries
if (!require("tidyverse"))install.packages("tidyverse")
if(!require("survival"))install.packages("survival")
if(!require("survminer"))install.packages("survminer")
if(!require("survMisc"))install.packages("survMisc")
if(!require("KMsurv"))install.packages("KMsurv")
if(!require("km.ci"))install.packages("km.ci")
if(!require("xtable"))install.packages("xtable")
if(!require("hdnom"))install.packages("hdnom")
if(!require("shape"))install.packages("shape")
if(!require("ncvreg"))install.packages("ncvreg")
if(!require("penalized"))install.packages("penalized")
if(!require("survAUC"))install.packages("survAUC")
if(!require("rmarkdown"))install.packages("rmarkdown")
if(!require("GGally"))install.packages("GGally")
if(!require("scales"))install.packages("scales")
if(!require("ggforce"))install.packages("ggforce")
if(!require("pec"))install.packages("pec")
if(!require("mice"))install.packages("mice")
if(!require("Hmisc"))install.packages("Hmisc")
if(!require("remotes"))install.packages("remotes")
if(!require("mlr3learners"))install.packages("mlr3learners")
if(!require("mlr3"))install.packages("mlr3")
if(!require("mlr3proba"))install.packages("mlr3proba")
if(!require("mlr3learners.randomforestsrc"))remotes::install_github("mlr3learners/mlr3learners.randomforestsrc")
if(!require("mlr3viz"))install.packages("mlr3viz")
if(!require("mlr3measures"))install.packages("mlr3measures")
if(!require("data.table"))install.packages("data.table")
if(!require("pracma"))install.packages("pracma")
if(!require("mlr3learners.penalized"))remotes::install_github("mlr3learners/mlr3learners.penalized")
if(!require("ranger"))install.packages("ranger")
if(!require("ggRandomForests"))install.packages("ggRandomForests")

# set basic plotting style
theme_set(theme_bw())

# import data 
url <- "http://www.mayo.edu/research/documents/pbcdat/DOC-10026921"

pbc_data <- read.delim(url, header = FALSE, sep = "")
# create column names
# the colnames are taken from the Appendix D of Fleming and Harrington
# link https://www.mayo.edu/research/documents/pbchtml/doc-10027635

col_names_pbc_data <- c("id", "time", "status", "trt","age","sex","ascites","hepato", "spiders", 
                        "edema", "bili", "chol", "albumin", "copper", "alk.phos", "ast", "trig", "platelet",
                        "protime","stage") 

# set colnames
colnames(pbc_data) <- col_names_pbc_data

# recode levels of factor variables
pbc_data <- pbc_data %>% mutate(sex = sex, trt = as.numeric(trt), ascites = as.numeric(ascites), hepato = as.numeric(hepato), spiders = as.numeric(spiders), chol = as.numeric(chol), copper = as.numeric(copper), alk.phos = as.numeric(alk.phos), ast = as.numeric(ast), trig = as.numeric(trig), platelet = as.numeric(platelet), protime = as.numeric(protime), stage = as.numeric(stage))
# age is documented in days --> transform it into years
pbc_data <- pbc_data %>% mutate(age = round(age/365.25, 0)) 
# save data
saveRDS(pbc_data, "pbc_data.rds")


tribble(~colnames_data,    ~explanation,
        "id",                "case number",
        "time",              "number of days between registration and death,",
        "",                   "transplantion, or end of study (1986)",
        "satus",             "status: 0=alive, 1=liver transplant, 2=dead",
        "trt",                "drug: 1 = D_penicillamine vs. 0 = Placebo",
        "age",                "age in days",
        "sex",                "sex: 0=male, 1=female",
        "ascites",              "presence of ascites: 0=no 1=yes",
        "hepato",               "presence of hepatomegaly 0=no 1=yes",
        "spiders",              "presence of 'spiders' 0=no 1=yes",
        "edema",               "presence of edema 0=no,", 
        "",                 "0.5 = edema present without diuretics,",                                                                                                 "",                "or edema resolved by diuretics", 
        "",                  "1 = edema despite diuretic therapy",
        "bili",                 "serum bilirubin in mg/dl",
        "chol",                 "serum cholesterol in mg/dl",
        "albumin",             "albumin in gm/dl",
        "copper",             "urine copper in ug/day",
        "alk.phos",          "alkaline phosphatase in U/liter",
        "ast",               "SGOT in U/ml",
        "trig",              "triglicerides in mg/dl",
        "platelet",          "platelets per cubic ml / 1000",
        "protime",          "prothrombin time in seconds",
        "stage" ,         "histologic stage of disease stage 1 - 4"
)

#check for NA
pbc_data %>% mutate(study_participant = c(rep("yes", times = 312), rep("no", times = 106))) %>% gather(key = "features", "values", 2:20) %>% group_by(study_participant)%>% group_by(features) %>%  filter(is.na(values)) %>% ggplot(aes(features, fill = study_participant))+geom_bar()+scale_fill_brewer(palette = "Accent")+geom_text(stat = 'count', aes(label =..count..), vjust= -0.8, cex = 3)




#set colors of groups manually
cbp2 <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

pbc_data %>% select(sex,time, age,bili,chol, albumin, copper, alk.phos, ast, trig, platelet, protime)%>% pivot_longer(names_to = "features", values_to = "values", 2:ncol(.))%>% mutate(sex = as.factor(sex)) %>%  ggplot(aes(values, fill = sex))+scale_fill_brewer(palette = "Dark2")+geom_histogram(col = "black")+geom_freqpoly(col = "red", alpha = 0.4)+facet_wrap(~features, scales = "free")



pbc_data %>% select(sex,time, age,bili,chol, albumin, copper, alk.phos, ast, trig, platelet, protime)%>% pivot_longer(names_to = "features", values_to = "values", 2:ncol(.)) %>% mutate(sex = as.factor(sex)) %>%  ggplot(aes(features,values, group = sex, fill = sex, col = sex))+geom_boxplot(col = "black")+geom_point(alpha = 0.1, position = position_jitterdodge())+scale_fill_brewer(palette = "Dark2")+scale_color_brewer(palette = "Dark2")+facet_wrap(~features, scales = "free", nrow = 3)


pbc_data %>% mutate(alk.phos = log2(alk.phos), ast = log2(ast), copper = log2(copper), trig = log2(trig), protime = log2(protime), bili = log2(bili), chol = log2(chol)) %>% select(sex, time, age,bili,chol, albumin, copper, alk.phos, ast, trig, platelet, protime)%>% pivot_longer(names_to = "features", values_to = "values", 2:ncol(.))%>% mutate(sex = as.factor(sex)) %>%  ggplot(aes( values, fill = sex))+geom_histogram(color = "black")+scale_fill_brewer(palette = "Dark2")+facet_wrap(~features, scales = "free", nrow = 2)


#log2() transform the data
pbc_data_transformed <- pbc_data %>% mutate(alk.phos = log2(alk.phos), ast = log2(ast), copper = log2(copper), trig = log2(trig), protime = log2(protime), bili = log2(bili), chol = log2(chol))

pbc_data %>% mutate(alk.phos = log2(alk.phos), ast = log2(ast), copper = log2(copper), trig = log2(trig), protime = log2(protime), bili = log2(bili), chol = log2(chol)) %>% select(sex, time, age,bili,chol, albumin, copper, alk.phos, ast, trig, platelet, protime)%>% pivot_longer(names_to = "features", values_to = "values", 2:ncol(.))%>% mutate(sex = as.factor(sex)) %>%  ggplot(aes(sample = values, col = sex))+scale_color_brewer(palette = "Dark2")+stat_qq()+stat_qq_line()+facet_wrap(~features, scales = "free", nrow = 2)


pbc_data %>% select(sex,status, ascites, hepato, spiders, edema, stage) %>% pivot_longer(names_to = "features", values_to = "values", 2:ncol(.))%>% mutate(sex = as.factor(sex)) %>% ggplot(aes(values,  fill = sex))+geom_bar()+scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))+scale_fill_brewer(palette = "Dark2") + geom_text(stat = 'count', aes(label =..count..), vjust= -0.8, cex = 3)+facet_wrap(~features, scale = "free", nrow = 2)


pbc_data_transformed %>% select(time, age,bili,chol, albumin, copper, alk.phos, ast, trig, platelet, protime,sex,status, ascites, hepato, spiders, edema, stage) %>% ggcorr(label = TRUE, label_size = 2.8, method = c("pairwise", "spearman"))





# -----extract 312 patients from randomized trial------------------------

pbc_data_transformed_study_patients <- pbc_data_transformed %>%slice(seq(1, 312, 1))

# change status 0 and 1 into 1
pbc_data_transformed <- pbc_data_transformed %>% mutate(
  status = case_when(
    status == 0 ~ 0,
    status ==1 ~ 1,
    status == 2 ~1
  ))

# aregImpute() to umpute missing values-------------------------------
smarti <- aregImpute(~ as.factor(ascites) + as.factor(hepato)+as.factor(spiders)+as.factor(edema)+bili+chol+albumin+copper+alk.phos+ast+trig+platelet+protime+as.factor(stage), n.impute = 10, match = "kclosest", data = pbc_data_transformed_study_patients)

imputed <- impute.transcan(smarti, imputation = 1, data = pbc_data_transformed_study_patients, list.out = TRUE, pr = FALSE, check = FALSE)
pbc_data_transformed_study_patients_imputed_areg <- pbc_data_transformed_study_patients
pbc_data_transformed_study_patients_imputed_areg [names(imputed)]<- imputed


#  k-nearest neighbour algorithm (knn) to impute missing values-----------------------------------
# using this approach with automatically center and scale the data
# because of this all categorical values have to be transforemd into factors

pbc_data_transformed_study_patients_for_knn <- pbc_data_transformed_study_patients %>% mutate(sex = as.factor(sex),
                                                                                              trt = as.factor(trt),
                                                                                              ascites = as.factor(ascites),
                                                                                              hepato = as.factor(hepato),
                                                                                              spiders = as.factor(spiders),
                                                                                              edema = as.factor(edema),
                                                                                              stage = as.factor(stage)
)
preProcessed_knn <- caret::preProcess(pbc_data_transformed_study_patients_for_knn[,-c(1,2,3,5)], method = "knnImpute")

#with predict() the missing values imputation is calculated on the dataset
pbc_data_knn_transformed_study_patients <- predict(preProcessed_knn, pbc_data_transformed_study_patients_for_knn)

# missing values imputation with aregImpute() for all available patients
#impute all available data for later use

smarti <- aregImpute(~ as.factor(trt)+as.factor(ascites) + as.factor(hepato)+as.factor(spiders)+as.factor(edema)+bili+chol+albumin+copper+alk.phos+ast+trig+platelet+protime+as.factor(stage), n.impute = 10, match = "kclosest", data = pbc_data_transformed)

imputed <- impute.transcan(smarti, imputation = 1, data = pbc_data_transformed, list.out = TRUE, pr = FALSE, check = FALSE)
pbc_data_transformed_imputed <- pbc_data_transformed
pbc_data_transformed_imputed[names(imputed)]<- imputed


# bagged trees for imputation of all available data--------------------------------
# pbc_data_for_bg_impute<- pbc_data_transformed %>% mutate(trt = as.factor(trt),
#                                                                                               ascites = as.factor(ascites),
#                                                                                               hepato = as.factor(hepato),
#                                                                                               spiders = as.factor(spiders),
#                                                                                               edema = as.factor(edema),
#                                                                                               stage = as.factor(stage)
#                                                                                               )
# 
# 
# preProcessed_bagged <- caret::preProcess(pbc_data_for_bg_impute[,-c(1,2,3,5)], method = "bagImpute")
# pbc_data_transformed_imputed_bagged <- predict(preProcessed_bagged, pbc_data_for_bg_impute)

# train_test split-----------------------------------------------
# for further analyiss we will make train/test split of 70/30 on the 312 trial patients
set.seed(12)
index_training <- createDataPartition(pbc_data_knn_transformed_study_patients$status, p = 0.7, times = 1, list = FALSE)
training_data_pbc_trial <- pbc_data_knn_transformed_study_patients[index_training,]
testing_data_pbc_trial <- pbc_data_knn_transformed_study_patients[-index_training,]


training_data_pbc_trial_areg <- pbc_data_transformed_study_patients_imputed_areg[index_training,]
testing_data_pbc_trial_areg <- pbc_data_transformed_study_patients_imputed_areg[-index_training,]



# join survival status into 0 and 1
pbc_data_transformed <- pbc_data_transformed %>% mutate(
  status = case_when(
    status == 0 ~ 0,
    status ==1 ~ 1,
    status == 2 ~1
  ))

# plot Kaplan Meier Curve for one survival function

ggsurvplot(survfit(Surv(time, status)~1, data = pbc_data_transformed))

# plot the survival curve regarding stratified by treatment
ggsurvplot(survfit(Surv(time, status)~ascites, data = pbc_data_transformed), pval = TRUE, linetype = "strata", palette = c("#E7B800", "#2E9FDF"), risk.table = TRUE)

```{r}
# surv_form of all vailabe features
surv_form <- Surv(time, status) ~ trt +age+sex+hepato+spiders+edema+bili+chol+albumin+copper + alk.phos+ast+trig+platelet+protime+stage+ascites

# stepwise variable selection based on fastbw() function of the rms package
set.seed(12)
fit_cox_AIC <- pec::selectCox(surv_form, data = training_data_pbc_trial, rule = "aic")
fit_cox_AIC


# in comparison lets use areg imputed NA data---------------

# stepwise variable selection based on fastbw() function of the rms package
set.seed(12)
fit_cox_AIC_areg <- pec::selectCox(surv_form, data = training_data_pbc_trial_areg, rule = "aic")
fit_cox_AIC_areg

# matrix of features
x <- as.matrix(training_data_pbc_trial_areg[,-c(1,2,3)])
# Surv function
time <- training_data_pbc_trial_areg$time
event <- training_data_pbc_trial_areg$status
y <- Surv(time, event)
# elastic net modelling, # 10 cross validations and set model selection criterion for autmomatic model tuning
# for automatic model tuning one has to asses model performance in each step by a loss function. 
# set random seed for parameter tuning. 
fit_training_elastic_net <- fit_enet(x, y, nfolds = 10, rule = "lambda.1se", seed = c(5, 7))
fit_training_elastic_net


# plot nomogramm of the model
nom <- as_nomogram(
  fit_training_elastic_net,
  x, time, event, 
  pred.at = 365.25 *4,#make predictionat 4 years
  funlabel = "4-Year Overall Survival Probability"
)

plot(nom)


#fit multivariate unpenalized cox model on testing data
coxph(Surv(time, status)~ascites+edema+bili +albumin+protime+ stage, data = training_data_pbc_trial_areg)

#fit multivariate unpenalized cox model on training data
coxph(Surv(time, status)~ascites+edema+bili +albumin+protime+ stage, data = testing_data_pbc_trial_areg)
# first internal validation by time dependent AUC with "Uno estimator" 
# internal validation has no real evidence
# doing it for first reference basis

# c Uno's AUC values for seq(1, 5, 0.5) * 365
# use training data 10 fild bootstrapping

val_intern <- validate(x, time, event,
                       model.type = "enet",
                       alpha = fit_training_elastic_net$alpha,
                       lambda = fit_training_elastic_net$lambda,
                       method = "bootstrap", boot.times = 10,
                       tauc.type = "UNO", tauc.time = seq(1, 5, 0.5) * 365,
                       seed = 42, trace = FALSE)

# plot the Uno#s AUC
plot(val_intern)+geom_line(col = "darkred") + xlab(" time in days")

# external validation
# more meaningful for performance evaluation
x_test <- as.matrix(testing_data_pbc_trial_areg[,-c(1,2,3)])
time_test <- testing_data_pbc_trial_areg$time
event_test <- testing_data_pbc_trial_areg$status

val_extern <- validate_external(
  fit_training_elastic_net, x, time, event,
  x_test, time_test, event_test,
  tauc.type = "UNO",
  tauc.time = seq(0.25, 5, 0.25) * 365)
plot(val_extern)+geom_line(color = "darkred")+theme(axis.text.x = element_text(angle = 60, size = 8))+xlab(" time in days")

# internal calibration
# using 10 fold cross validation
# ngroup = number of groups for calibration

cal_internal <- calibrate(x, 
                          time, 
                          event, model.type = "enet", 
                          alpha = fit_training_elastic_net$alpha,
                          lambda = fit_training_elastic_net$lambda,
                          method = "cv",
                          nfolds = 10,
                          pred.at = 365*3,
                          ngroup = 3,
                          seed = 12,
                          trace = FALSE)
plot(cal_internal)

# compute cal testin, calibration
cal_testing <- calibrate_external(fit_training_elastic_net, x, time, event,
                                  x_test, time_test, event_test, pred.at = 365*3, ngroup = 3)
plot(cal_testing)

# draw risk stratified caplan meier curves based on elastic net model, using the traing set
kmplot(cal_internal,
       group.name = c("high risk", "medium risk", "low risk"),
       time.at = 1:10 * 365)

# draw risk stratified caplan meier curves based on elastic net model, using the test set
kmplot(cal_testing,
       group.name = c("high risk", "medium risk", "low risk"),
       time.at = 1:10 * 365)

# compute model comparison with other penalized cox learners
model_comparison_training <- compare_by_validate(x, time, event, model.type = c("lasso", "enet", "aenet", "flasso", "snet", "mnet", "mcp"),
                                                 method = "cv", nfolds = 10, tauc.type = "UNO", tauc.time = seq(0.25, 5, 0.25)*365, 
                                                 seed = 12, trace = FALSE)
plot(model_comparison_training)+theme(axis.text.x = element_text(angle = 60))


# random forests ----------------------------------------------------------

#fit ensemble of trees with default values, exception: set samptype to sampling with replacement.
set.seed(12)
rsf_1 <- rfsrc(Surv(time, status)~., training_data_pbc_trial_areg[,-1],importance = TRUE,bootstrap = "by.root", samptype = "swr", seed = 12)
rsf_1

# make prediction on test data
set.seed(12)
prediction_rsf_1 <-predict(rsf_1$forest, testing_data_pbc_trial_areg)
prediction_rsf_1

# as default use mehtod = "md" = minimal depth; feature selection
set.seed(12)
variable_selection_training <- var.select(Surv(time, status)~., training_data_pbc_trial_areg[,-1], refit = TRUE)
variable_selection_training$rfsrc.refit.obj

# make prediction with minimal depth selected random forest
set.seed(12)
prediction_variable_selection_training <- predict(variable_selection_training$rfsrc.refit.obj, testing_data_pbc_trial_areg)
prediction_variable_selection_training


# benchmark ---------------------------------------------------------------

# create task from training data

# change class of imputed columns from S3imputed to numeric

training_data_pbc_trial_areg <- 
  training_data_pbc_trial_areg %>% 
  mutate(chol = as.numeric(chol), copper = as.numeric(copper), trig = as.numeric(trig), platelet = as.numeric(platelet))

# create task
pbc_task <- TaskSurv$new(id = "pbc_task", backend = training_data_pbc_trial_areg, time = "time", event = "status")
# define learners
pbc_learner <- lapply(c("surv.coxph",  "surv.rfsrc", "surv.xgboost", "surv.rpart", "surv.ranger","surv.glmnet", "surv.cvglmnet"), mlr3::lrn)

# define resampling 10fold cv
pbc_resampling <- mlr3::rsmp(.key = "cv")
# lets compute a first benchmark
pbc_design <- mlr3::benchmark_grid(tasks = pbc_task, learners = pbc_learner, resamplings = pbc_resampling)
# define performance measures
pbc_measures <- lapply(c("surv.unoAUC", "surv.unoC",  "surv.harrellC"), msr)
set.seed(24)
pbc_bmr <- benchmark(pbc_design)
#Harrels C-Index is the default measure, aggregate the results over the 10 cv
pbc_bmr$aggregate(measures = pbc_measures)
