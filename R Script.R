library(Robyn)
packageVersion("Robyn")

## Force multi-core use when running RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)

## Check simulated dataset or load your own dataset
data("dt_simulated_weekly")
head(dt_simulated_weekly)

data("dt_prophet_holidays")
head(dt_prophet_holidays)

# Directory where you want to export results to (will create new folders)
robyn_directory <- "~/Desktop"

InputCollect <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  date_var = "DATE", # date format must be "2020-01-01"
  dep_var = "revenue", # currently one dependent variable allowed
  dep_var_type = "revenue", # "revenue" or "conversion" allowed
  prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday" allowed
  prophet_country = "DE", # 123 countries included in dt_prophet_holidays
  context_vars = c("competitor_sales_B", "events"), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("tv_S", "ooh_S", "print_S", "facebook_S", "search_S"), # mandatory input
  paid_media_vars = c("tv_S", "ooh_S", "print_S", "facebook_I", "search_clicks_P"), # if provided,
  # Robyn will use this instead of paid_media_spends for modelling. Media exposure metrics
  # include typically, but not limited to impressions, GRP etc. If not applicable, use spend instead.
  organic_vars = c("newsletter"), # marketing activity without media spend
  factor_vars = c("events"), # indicate categorical varibales in context_vars or organic_vars
  window_start = "2016-01-01",
  window_end = "2018-12-31",
  adstock = "geometric" # geometric or weibull_pdf
)
print(InputCollect)

hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

# Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  facebook_I_alphas = c(0.5, 3),
  facebook_I_gammas = c(0.3, 1),
  facebook_I_thetas = c(0, 0.3),
  print_S_alphas = c(0.5, 1),
  print_S_gammas = c(0.3, 1),
  print_S_thetas = c(0.1, 0.4),
  tv_S_alphas = c(0.5, 1),
  tv_S_gammas = c(0.3, 1),
  tv_S_thetas = c(0.3, 0.8),
  search_clicks_P_alphas = c(0.5, 3),
  search_clicks_P_gammas = c(0.3, 1),
  search_clicks_P_thetas = c(0, 0.3),
  ooh_S_alphas = c(0.5, 1),
  ooh_S_gammas = c(0.3, 1),
  ooh_S_thetas = c(0.1, 0.4),
  newsletter_alphas = c(0.5, 3),
  newsletter_gammas = c(0.3, 1),
  newsletter_thetas = c(0.1, 0.4),
  train_size = c(0.5, 0.8)
)

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)

# Check spend exposure fit and consider channel split if applicable
InputCollect$ExposureCollect$plot_spend_exposure


# Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  cores = NULL, # NULL defaults to (max available - 1)
  iterations = 2000, # 2000 recommended for the dummy dataset with no calibration
  trials = 5, # 5 recommended for the dummy dataset
  ts_validation = FALSE, # 3-way-split time series for NRMSE validation.
  add_penalty_factor = FALSE # Experimental feature to add more flexibility
)
print(OutputModels)

OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  pareto_fronts = "auto", # automatically pick how many pareto-fronts to fill min_candidates (100)
  # min_candidates = 100, # top pareto models for clustering. Default to 100
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  export = TRUE, # this will create files locally
  plot_folder = robyn_directory, # path for plots exports and files creation
  plot_pareto = TRUE # Set to FALSE to deactivate plotting and saving model one-pagers
)
print(OutputCollect)

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "5_257_2" # Pick one of the models from OutputCollect to proceed

#### Version >=3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model, export = TRUE)
print(ExportedModel)

# To plot any model's one-pager:
myOnePager <- robyn_onepagers(InputCollect, OutputCollect, select_model, export = FALSE)

print(ExportedModel)
InputCollect$paid_media_selected

AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  # date_range = "all", # Default to "all"
  # total_budget = NULL, # When NULL, default is total spend in date_range
  channel_constr_low = 0.7,
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  # channel_constr_multiplier = 3,
  scenario = "max_response"
)
# Print & plot allocator's output
print(AllocatorCollect1)
plot(AllocatorCollect1)

AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  date_range = "last_10", # Last 10 periods, same as c("2018-10-22", "2018-12-31")
  total_budget = 1500000, # Total budget for date_range period simulation
  channel_constr_low = c(0.8, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = 1.5,
  channel_constr_multiplier = 5, # Customise bound extension for wider insights
  scenario = "max_response"
)
print(AllocatorCollect2)
plot(AllocatorCollect2)

AllocatorCollect3 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  #channel_constr_low = 0.1,
  #channel_constr_up = 10,
  # date_range = NULL, # Default: "all" available dates
  scenario = "target_efficiency",
  # target_value = 5 # Customize target ROAS or CPA value
)
print(AllocatorCollect3)
plot(AllocatorCollect3)

json_file = "~/Desktop/Robyn_202412181043_init/RobynModel-5_257_2.json"
AllocatorCollect4 <- robyn_allocator(
  json_file = json_file, # Using json file from robyn_write() for allocation
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  # date_range = NULL,
  scenario = "target_efficiency",
  target_value = 2, # Customize target ROAS or CPA value
  plot_folder = "~/Desktop",
  plot_folder_sub = "my_subdir"
)

RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 4,
  refresh_iters = 2000,
  refresh_trials = 5
)

json_file_rf1 <- "~/Desktop/Robyn_202412181043_init/Robyn_202412181054_rf1/RobynModel-1_133_7.json"
RobynRefresh <- robyn_refresh(
  json_file = json_file_rf1,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 4,
  refresh_iters = 2000,
  refresh_trials = 5
)

InputCollectX <- RobynRefresh$listRefresh1$InputCollect
OutputCollectX <- RobynRefresh$listRefresh1$OutputCollect
select_modelX <- RobynRefresh$listRefresh1$OutputCollect$selectID

Response <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_I"
)
Response$plot

Spend1 <- 80000
Response1 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_I",
  metric_value = Spend1, # total budget for date_range
  date_range = "last_10" # last two periods
)
Response1$plot

Spend2 <- Spend1 + 100
Response2 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "facebook_I",
  metric_value = Spend2,
  date_range = "last_10"
)

# ROAS for the 100$ from Spend1 level
(Response2$sim_mean_response - Response1$sim_mean_response) /
  (Response2$sim_mean_spend - Response1$sim_mean_spend)

# Example of getting organic media exposure response curves
sendings <- 30000
response_sending <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  metric_name = "newsletter",
  metric_value = sendings,
  date_range = "last_10"
)
# Simulated cost per thousand sendings
response_sending$sim_mean_spend / response_sending$sim_mean_response * 1000

# Manually create JSON file with inputs data only
robyn_write(InputCollect, dir = "~/Desktop")

# Manually create JSON file with inputs and specific model results
robyn_write(InputCollect, OutputCollect, select_model)

# Pick any exported model (initial or refreshed)
json_file <- "~/Desktop/Robyn_202412181043_init/RobynModel-5_257_2.json"

# Optional: Manually read and check data stored in file
json_data <- robyn_read(json_file)
print(json_data)

# Re-create InputCollect
InputCollectX <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  json_file = json_file)

# Re-create OutputCollect
OutputCollectX <- robyn_run(
  InputCollect = InputCollectX,
  json_file = json_file
)

# Or re-create both by simply using robyn_recreate()
RobynRecreated <- robyn_recreate(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  quiet = FALSE)
InputCollectX <- RobynRecreated$InputCollect
OutputCollectX <- RobynRecreated$OutputCollect

# Re-export or rebuild a model and check summary
myModel <- robyn_write(InputCollectX, OutputCollectX, export = FALSE, dir = "~/Desktop")
print(myModel)

# Re-create one-pager
myModelPlot <- robyn_onepagers(InputCollectX, OutputCollectX, export = FALSE)
# myModelPlot[[1]]$patches$plots[[7]]

# Refresh any imported model
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = InputCollectX$dt_input,
  dt_holidays = InputCollectX$dt_holidays,
  refresh_steps = 6,
  refresh_mode = "manual",
  refresh_iters = 1000,
  refresh_trials = 1
)

# Recreate response curves
robyn_response(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  metric_name = "newsletter",
  metric_value = 50000
)