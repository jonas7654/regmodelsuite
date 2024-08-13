########## For testing only ###################

library(haven)
library(rstudioapi)
library(tidyverse)
library(glmnet)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

school_data <- read_dta("ca_school_testscore.dta") %>%
  dplyr::select('math_score',
                'str_s' ,
                'te_avgyr_s' ,
                'exp_1000_1999_d' ,
                'med_income_z' ,
                'frpm_frac_s' ,
                'ell_frac_s' ,
                'freem_frac_s',
                'enrollment_s' ,
                'fep_frac_s' ,
                'edi_s' ,
                're_aian_frac_s',
                're_asian_frac_s',
                're_baa_frac_s' ,
                're_fil_frac_s' ,
                're_hl_frac_s' ,
                're_hpi_frac_s' ,
                're_tom_frac_s' ,
                're_nr_frac_s' ,
                'te_fte_s' ,
                'te_1yr_frac_s' ,
                'te_2yr_frac_s' ,
                'te_tot_fte_rat_s' ,
                'exp_2000_2999_d' ,
                'exp_3000_3999_d' ,
                'exp_4000_4999_d' ,
                'exp_5000_5999_d' ,
                'exp_6000_6999_d' ,
                'exp_7000_7999_d' ,
                'exp_8000_8999_d' ,
                'expoc_1000_1999_d',
                'expoc_2000_2999_d' ,
                'expoc_3000_3999_d' ,
                'expoc_4000_4999_d' ,
                'expoc_5000_5999_d' ,
                'revoc_8010_8099_d' ,
                'revoc_8100_8299_d' ,
                'revoc_8300_8599_d' ,
                'revoc_8600_8799_d' ,
                'age_frac_5_17_z' ,
                'age_frac_18_24_z' ,
                'age_frac_25_34_z' ,
                'age_frac_35_44_z' ,
                'age_frac_45_54_z' ,
                'age_frac_55_64_z' ,
                'age_frac_65_74_z' ,
                'age_frac_75_older_z' ,
                'pop_1_older_z' ,
                'sex_frac_male_z',
                'ms_frac_now_married_z' ,
                'ms_frac_now_divorced_z' ,
                'ms_frac_now_widowed_z' ,
                'ed_frac_hs_z' ,
                'ed_frac_sc_z' ,
                'ed_frac_ba_z' ,
                'ed_frac_grd_z' ,
                'hs_frac_own_z' ,
                'moved_frac_samecounty_z',
                'moved_frac_difcounty_z',
                'moved_frac_difstate_z' ,
                'moved_frac_abroad_z'
  )
head(school_data)


#prepare data (Split data into training and test set 50/50)
n = nrow(school_data)

train_rows = sample(n, 0.5 * n)

train_data = school_data[train_rows,]
test_data = school_data[-train_rows,]

# Large model with all variables, squared variables and cubic variables as well as all interactions
reg_equation <- formula(math_score ~ .)
dim(model.matrix(reg_equation, train_data))

### Test with synthetic data ###
n <- 1000
p <- 5
true_beta <- runif(p , min = 0, 10)
X <- matrix(rnorm(n * p), n, p)
dimnames(X)[[2]] <- paste0("x",1:p)
y <- X %*% true_beta + rnorm(n)

dataframe <- data.frame(y = y, X)

# linear model
sum(abs(lm(y ~ x1 + x2 + x3 + x4 + x5 + 0, data = dataframe)$coefficients - true_beta))

# lasso
sum(abs(regmodel(y ~ x1 + x2 + x3 + x4 + x5, data = dataframe, model = "lasso" ,lambda = 0) - true_beta))
regmodel(y ~ x1 + x2 + x3 + x4 + x5, data = dataframe, model = "lasso" , lambda = 1)$coefficients
coef(glmnet(X, y, intercept = F, lambda = 0))
true_beta

# ridge
regmodel(y ~ x1 + x2 + x3 + x4 + x5, data = dataframe, model = "ridge" , lambda = 1)$coefficients


