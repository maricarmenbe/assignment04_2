library(readr)
library(readxl)
library(tidyverse)
library(caret)
library(tidymodels)
library(zoo)
library(randomForest)
library(kknn)
library(rpart)
library(rpart.plot)

# ensuring no scientific notation is used----
options(scipen=999)

# reading in natural event data, source: EM-DAT, Int. Disaster Database----
natevent_df <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQt_WzQUzggYSN6404YIJvTngtXPY-jwxNwXzqWnRHPwL1urdoTIsek5B9hjPEu6BFUz9LS3negITtG/pub?output=csv")

# loading refugee data from UNHCR----
devtools::install_github("unhcr/unhcrdatapackage") # run if 'unhcrdatapackage' package not already installed to computer
refugee_df <- unhcrdatapackage::end_year_population_totals %>% 
  filter(Year>=1990 & Year<=2020,
         CountryOriginCode %in% natevent_df$iso_code) %>% 
  group_by(CountryOriginCode,Year) %>% 
  summarize(refugees=sum(REF),
            assylum_seekers=sum(ASY),
            IDPs=sum(IDP)) %>% 
  rename("iso_code"="CountryOriginCode","year"="Year") %>% 
  as_tibble()


# merging refugee df with natevent df... "df" will be the primary name for the data frame
df <- left_join(refugee_df, natevent_df,by=c("iso_code"="iso_code","year"="year")) %>% 
  filter(!is.na(country_origin))

# getting government variables from v-dem----
devtools::install_github("vdeminstitute/vdemdata") # run if 'vdemdata' package is not already installed to computer
# loading vdem data from 1990 to 2020
vdem_df <- vdemdata::vdem %>% 
  filter(year>=1990 & year<=2020) %>%  
  select(-ends_with(c("ord","osp","codehigh","codelow","_sd","mean","_nr"))) %>% 
  arrange(country_name)
unique(vdem_df$country_name)
# selecting target government variables from v-dem 
vdem_df <- vdem_df %>% 
  select(country_text_id,
         year,
         v2ellocgov, # local govt. exists, 0=no, 1=yes
         v2elreggov, # regional govt. exists, 0=no, 1=yes
         v2x_liberal, # liberal principle of democracy achieved, 0=not achieved, 1=achieved
         v2xeg_eqdr, # equal distribution of resources in society index, 0=not equal, 1=equal
         v2clrgunev, # equal respect per subnational regions by govt, low=not equal, 1=equal
         v2clrspct, # impartial pub administration, low=law not respected by pub officials, high=respected
         v2exbribe, # executive bribery and corrupt exchanges, low=routine/expected, high=never/hardly happens
         v2dlengage, # engaged society, low=no public deliberation, high=common public deliberation
         v2xcl_dmove, # freedom of domestic movement, 0=no freedom,1=freedom
         v2xcl_slave # freedom from forced labor, 0=no freedom, 1=freedom
  ) %>%
  rename(locgovt_dummy=v2ellocgov,
         reggovt_dummy=v2elreggov,
         liberal_dem=v2x_liberal,
         equal_dist=v2xeg_eqdr,
         equal_area_respect=v2clrgunev,
         govt_respect=v2clrspct,
         exec_bribery=v2exbribe,
         engaged_society=v2dlengage,
         free_movement=v2xcl_dmove,
         freedom_from_slavery=v2xcl_slave)

# merging df with vdem df
df <- left_join(df,vdem_df, by=c("iso_code"="country_text_id","year"="year"))

# reading data from team google spreadsheet----
conflict_economy_df <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTrNaSHs6itvG-rvlhWjXZylNEi_eOZnFNT0_ugde2ySaebr-OPhPDsSWCOsyhI0Y6iCRruAxhJqKxk/pub?output=csv") %>% 
  select(year,iso_code, 
         ext_conflict, int_conflict, battle_deaths, 
         elect_violence, cpi_inflation, life_exp, 
         fdipercent, gdp, gdppc, gdppcgrowth, oilpercent, 
         urban, totalpop)

# merging df with vdem df
df <- left_join(df,conflict_economy_df, by=c("iso_code"="iso_code","year"="year"))

# removing other data frames from environment----
rm(conflict_economy_df,natevent_df,refugee_df,vdem_df)



#_____________________________________________________________________________________________________________________________________________________________________________________________
# Descriptive statistics and exploratory data----

#Reading in data from Displacement_Data Google sheet again for descriptive statistics
conflict_economy_df <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTrNaSHs6itvG-rvlhWjXZylNEi_eOZnFNT0_ugde2ySaebr-OPhPDsSWCOsyhI0Y6iCRruAxhJqKxk/pub?output=csv")
  

#Some exploratory data analysis with the data.
conflict_economy_df %>%
  group_by(income) %>%
  select(income, refugees, year) %>%
  filter(year<2001) %>%
  summarize(refugees_income = sum(refugees)) %>%
  ggplot() +
  geom_col(mapping = aes(x = income, y = refugees_income, fill = income))+
  xlab("Country Income Level") +
  ylab("# of Refugees") +
  labs(title = "From 1990 - 2000, Countries with Lower Income Levels Show Higher Number of Refugees")+
  labs(caption = "Data comes from 1990 to 2000")+
  scale_y_continuous(breaks = seq(0, 45000000, by = 5000000))+
  theme_minimal()+
  theme(legend.position = "none")

ggsave("refugees_income.png")

#Explore data by country
refugees_by_country_Africa <- conflict_economy_df %>%
  group_by(country_origin) %>%
  filter(region=="Sub-Saharan Africa") %>%
  select(country_origin, refugees) %>%
  summarize(total_refugees = sum(refugees))

print(refugees_by_country_Africa)

#Why are some LATAM countries like Argentina included in refugees_by_country_Africa?

#Exlore data by year
refugees_year_country <- conflict_economy_df %>%
  group_by(year, country_origin) %>%
  select(year, country_origin, refugees) %>%
  count(refugees)

print(refugees_year_country)

#Count total refugees by country from 1990-202

count_refugees <- sum(refugees_by_country$total_refugees)

#Visualize total number of refugees by country in Sub-Saharan Africa
ggplot(data = refugees_by_country_Africa) +
  geom_bar(mapping = aes(x = country_origin, y=total_refugees), fill = "blue", stat="identity", color="black") +
  coord_flip() +
  labs(title = "Number of Refugees by Country", x="", y="Refugees",fill="Country", 
       subtitle = paste(count_refugees," refugees by country")) +
  theme(plot.title = element_text(size=15, hjust=0.5,face="bold")) +
  theme(plot.subtitle = element_text(size=12, hjust=0.5)) +
  theme(panel.background = element_rect(fill = "white", color="black")) +
  theme(axis.text.y = element_text(size=8,color="black")) +
  theme(axis.text.x = element_text(size=8,color="black")) +
  scale_y_continuous(breaks = c(0,1000000,2000000,3000000,4000000,5000000,6000000,7000000,8000000,9000000,
                                10000000, 11000000,12000000,13000000,14000000,15000000,15000000,16000000,
                                17000000,18000000,19000000,20000000,21000000,22000000),limits = c(0,22000000)) +
  geom_text(mapping=aes(x=country_origin,y=total_refugees,label=total_refugees), hjust=-.5, size=2)





#_____________________________________________________________________________________________________________________________________________________________________________________________
# machine learning starts here----

set.seed(20201020)

# create split object
disp_split <- initial_split(data = df, prop = 0.90)

# create training and testing data
disp_train <- training(x = disp_split)
disp_test <- testing(x = disp_split)

# create recipe
disp_recipe <- recipe(refugees ~ ., data = disp_train) %>%
  update_role(year, country_origin, new_role = "ID") #keep the ID variables for future reference

# create resamples 
disp_cv <- vfold_cv(data = disp_train, v = 5)

# knn with resampling and hyperparameter tuning 

## model specification 
knn <- nearest_neighbor(neighbors = tune()) %>%
  set_engine(engine = "kknn") %>%
  set_mode(mode = "regression") 
knn

## create workflow 
knn_workflow <-
  workflow() %>%
  add_model(knn) %>%
  add_recipe(disp_recipe)

## create tuning grid 
knn_grid <- tibble(neighbors = seq(from = 1, to = 30, by = 2))
knn_grid

## resampling models for tuning grid 
knn_grid_rs <-
  knn_workflow %>%
  tune_grid(resamples = disp_cv,
            grid = knn_grid,
            control = control_grid(save_pred = TRUE))

## evaluate the resampling models and select best 
knn_grid_rs %>% collect_metrics()          
knn_grid_rs %>% show_best(metric = "rmse")
best_knn <- knn_grid_rs %>% select_best(metric = "rmse")
knn_grid_rs %>% collect_predictions()

## best model
best_knn_workflow <- knn_workflow %>%
  finalize_workflow(best_knn)

set.seed(7)
knn_best_rs <- 
  best_knn_workflow %>%
  fit_resamples(resamples = disp_cv) 

collect_metrics(knn_best_rs)

# train the model on the training set 
knn_fit_test <-
  knn_workflow %>%
  fit(data = disp_train)

# predictions using testing set 
disp_pred <- predict(knn_fit_test, disp_test)

# df with predictions
model_pred <- bind_cols(disp_test$refugees, disp_pred)
model_pred <- rename(model_pred, refugees = ...1, prediction = .pred)

# calculate RMSE
rmse(model_pred, truth = refugees, estimate = prediction)
rsq(model_pred, truth = refugees, estimate = prediction)

##################################################################################################

# random forest

## model 
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(disp_recipe) %>%
  add_model(tune_spec)

set.seed(7)
tune_res <- tune_grid(
  tune_wf,
  resamples = disp_cv,
  grid = 10
)
tune_res

tune_res %>% collect_metrics() 
tune_res %>% show_best(metric = "rmse")
best_rf <- tune_res %>% select_best(metric = "rmse")

## best model

final_rf <- finalize_model(
  tune_spec,
  best_rf
)
final_rf

final_wf <- workflow() %>%
  add_recipe(disp_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(disp_split)

final_res %>%
  collect_metrics()

# df with predictions
model2_pred <- final_res %>% collect_predictions() 

model2_pred <- select(model2_pred, refugees, .pred)

model2_pred <- rename(model2_pred, prediction = .pred)

# metrics
rmse(model2_pred, truth = refugees, estimate = prediction)
rsq(model2_pred, truth = refugees, estimate = prediction)

##################################################################################################

# CART

## model specification 
decision_tree <- decision_tree() %>%
  set_engine(engine = "rpart") %>%
  set_mode(mode = "regression")

## workflow 
tree_workflow <-
  workflow() %>%
  add_model(decision_tree) %>%
  add_recipe(disp_recipe)

# resampling models for random forest model
set.seed(7)
tree_fit_rs <-
  tree_workflow %>%
  fit_resamples(resamples = disp_cv) 

collect_metrics(tree_fit_rs)

# train the best model on the training set 
tree_fit_test <-
  tree_workflow %>%
  fit(data = disp_train)

# predictions using testing set 
model3_pred <- predict(tree_fit_test, disp_test)

# df with predictions
model3_pred <- bind_cols(disp_test$refugees, model3_pred)
model3_pred <- rename(model3_pred, refugees = ...1, prediction = .pred)

# metrics
rmse(model3_pred, truth = refugees, estimate = prediction)
rsq(model3_pred, truth = refugees, estimate = prediction)

##################################################################################################

# CLASSIFICATION MODELS #

df <- read_excel("/Users/ankushi/Desktop/disp2.xlsx")

df2 <- df %>% slice(1:1417)


df2 <- select(df, displacement_event, year, country_origin, 
              asylum_seekers, IDPs, 
              ext_conflict, int_conflict, battle_deaths, 
              elect_violence, cpi_inflation, life_exp, fe_etfra, 
              fdipercent, gdp, gdppc, gdppcgrowth, oilpercent, 
              urban, totalpop, totaff_natevents)

df2$displacement_event <- as.factor(df2$displacement_event)
df2 <- as.numeric(df2)
df2 <- mutate_at(df2, "totaff_natevents", ~replace(., is.na(.), 0))
df2 <- na.locf(df2)

set.seed(20211117)

# create split object
disp_split <- initial_split(data = df2, prop = 0.90)

# create training and testing data
disp_train <- training(x = disp_split)
disp_test <- testing(x = disp_split)

set.seed(20201020)

# create recipe
disp_recipe <- recipe(displacement_event ~ ., data = disp_train) %>%
  themis::step_downsample(displacement_event) %>% #since the EDA shows class imbalance in the data toward "micro" and "small" displacement events
  update_role(year, country_origin, new_role = "ID") %>% #keep the ID variables for future reference
  step_normalize(all_numeric_predictors()) #scale and center the predictors 

# create resamples 
disp_cv <- vfold_cv(data = disp_train, v = 5)

# knn with resampling and hyperparameter tuning 

## model specification 
knn <- nearest_neighbor(neighbors = tune()) %>%
  set_engine(engine = "kknn") %>%
  set_mode(mode = "classification") 
knn

## create workflow 
knn_workflow <-
  workflow() %>%
  add_model(knn) %>%
  add_recipe(disp_recipe)

## create tuning grid 
knn_grid <- tibble(neighbors = seq(from = 1, to = 30, by = 2))
knn_grid

## resampling models for tuning grid 
knn_grid_rs <-
  knn_workflow %>%
  tune_grid(resamples = disp_cv,
            grid = knn_grid,
            control = control_grid(save_pred = TRUE))

## evaluate the resampling models and select best 
knn_grid_rs %>% collect_metrics()          
knn_grid_rs %>% show_best(metric = "accuracy")
best_knn <- knn_grid_rs %>% select_best(metric = "accuracy")
knn_grid_rs %>% collect_predictions()

## best model
best_knn_workflow <- knn_workflow %>%
  finalize_workflow(best_knn)

## train the best model on the training set 
best_knn_fit <- 
  best_knn_workflow %>%
  fit(data=disp_train)
best_knn_fit

# predictions using testing set 
disp_pred <- predict(best_knn_fit, disp_test)

# df with predictions
model4_pred <- bind_cols(disp_test$displacement_event, disp_pred)
model4_pred <- rename(model4_pred, event = ...1, 
                      prediction = .pred_class)

# accuracy 
accuracy(model4_pred, truth = event, estimate = prediction)

##################################################################################################

# random forest with hyperparameter tuning

## model 
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>% 
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(disp_recipe) %>%
  add_model(tune_spec)

set.seed(7)
tune_res <- tune_grid(
  tune_wf,
  resamples = disp_cv,
  grid = 10
)
tune_res

tune_res %>% collect_metrics() 
tune_res %>% show_best(metric = "accuracy")
best_rf <- tune_res %>% select_best(metric = "accuracy")

## best model

final_rf <- finalize_model(
  tune_spec,
  best_rf
)
final_rf

final_wf <- workflow() %>%
  add_recipe(disp_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(disp_split)

final_res %>%
  collect_metrics()

# df with predictions
model5_pred <- final_res %>% collect_predictions() 

model5_pred <- select(model5_pred, displacement_event, .pred_class)

model5_pred <- rename(model5_pred, prediction = .pred_class)

# metrics
accuracy(model5_pred, truth = displacement_event, 
         estimate = prediction)

##################################################################################################

# CART

## model specification 
decision_tree <- decision_tree() %>%
  set_engine(engine = "rpart") %>%
  set_mode(mode = "classification")

## workflow 
tree_workflow <-
  workflow() %>%
  add_model(decision_tree) %>%
  add_recipe(disp_recipe)

# resampling models for random forest model
set.seed(7)
tree_fit_rs <-
  tree_workflow %>%
  fit_resamples(resamples = disp_cv) 

collect_metrics(tree_fit_rs)

# train the best model on the training set 
tree_fit_test <-
  tree_workflow %>%
  fit(data = disp_train)

# predictions using testing set 
model6_pred <- predict(tree_fit_test, disp_test)

# df with predictions
model6_pred <- bind_cols(disp_test$displacement_event, model6_pred)
model6_pred <- rename(model6_pred, event = ...1, prediction = .pred_class)

# metrics
accuracy(model6_pred, truth = event, estimate = prediction)




