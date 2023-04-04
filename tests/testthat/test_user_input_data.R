# Check National data ---------------------
devtools::load_all()

load("data/national_FPsource_format.rda")

# Test 1
surveydata_filepath = "tests/test_checkformat/national_user_input_test_1.xlsx" # wrong country name
national_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(national_FPsource_format, national_FPsource_data)

# Test 2
surveydata_filepath = "tests/test_checkformat/national_user_input_test_2.xlsx" # wrong proportion value
national_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(national_FPsource_format, national_FPsource_data)

# Test 3
surveydata_filepath = "tests/test_checkformat/national_user_input_test_3.xlsx" # missing N, sector name wrong
national_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(national_FPsource_format, national_FPsource_data)

# Test 4
surveydata_filepath = "tests/test_checkformat/national_user_input_test_4.xlsx" # wrong sector name
national_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(national_FPsource_format, national_FPsource_data)

# Test 5
surveydata_filepath = "tests/test_checkformat/national_user_input_test_5.xlsx" # missing SE.proportion
national_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(national_FPsource_format, national_FPsource_data)

# Test 7
surveydata_filepath = "tests/test_checkformat/national_user_input_test_correct.xlsx" # correct
national_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(national_FPsource_format, national_FPsource_data)

# Check data runs in the next function : local model ----------------
raw_subnatdata <- mcmsupply::get_national_data(local=TRUE, mycountry="Afghanistan", surveydata_filepath="tests/test_checkformat/national_user_input_test_correct.xlsx")
pkg_data <- get_national_modelinputs(fp2030=TRUE, local=TRUE, mycountry="Afghanistan", startyear=1990, endyear=2025.5, nsegments=12, raw_data = national_FPsource_data)

###############################################
# Check Subnational data ---------------------
###############################################

devtools::load_all()

load("data/subnat_FPsource_format.rda")
# Test 1
surveydata_filepath = "tests/test_checkformat/subnational_user_input_test_1.xlsx" # wrong region name
subnational_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(subnat_FPsource_format, subnational_FPsource_data)

# Test 2
surveydata_filepath = "tests/test_checkformat/subnational_user_input_test_2.xlsx" # wrong proportion value
subnational_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(subnat_FPsource_format, subnational_FPsource_data)

# Test 3
surveydata_filepath = "tests/test_checkformat/subnational_user_input_test_3.xlsx" # missing N, sector name wrong
subnational_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(subnat_FPsource_format, subnational_FPsource_data)

# Test 4
surveydata_filepath = "tests/test_checkformat/subnational_user_input_test_4.xlsx" # wrong sector name
subnational_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(subnat_FPsource_format, subnational_FPsource_data)

# Test 5
surveydata_filepath = "tests/test_checkformat/subnational_user_input_test_5.xlsx" # missing SE.proportion
subnational_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(subnat_FPsource_format, subnational_FPsource_data)

# Test 6
surveydata_filepath = "tests/test_checkformat/subnational_user_input_test_correct.xlsx" # correct
subnational_FPsource_data <- readxl::read_xlsx(surveydata_filepath)
check_format(subnat_FPsource_format, subnational_FPsource_data)

# Check data runs in the next function : local model ----------------
raw_subnatdata <- mcmsupply::get_subnational_data(local=TRUE, mycountry="Ethiopia", surveydata_filepath="tests/test_checkformat/ethiopia_subnat_customdata.xlsx")
pkg_data <- get_subnational_modelinputs(fp2030=TRUE, local=TRUE, mycountry="Ethiopia", startyear=1990, endyear=2025.5, nsegments=12, raw_subnatdata = raw_subnatdata)
run_subnational_jags_model(pkg_data = pkg_data, local=TRUE, spatial=TRUE, main_path = "results/subnational/local_spatial_custom/",
                           n_iter = 80000, n_burnin = 10000, n_thin = 35, mycountry="Ethiopia")
