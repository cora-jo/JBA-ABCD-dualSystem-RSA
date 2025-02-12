######################################################
#################primary analysis#################
######################################################

######################original dataset#############################
sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp <- read.csv("sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp.csv")

##################correlation####################################
sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp$imb <- scale(sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp$basm_sum) - scale(sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp$nihtbx_flanker_uncorrected)
selected_vars <- c("eventname","cbcl_scr_dsm5_adhd_r", "pvg", "nihtbx_flanker_uncorrected", "basm_sum",  "demo_sex_v2", "interview_age",  "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p","sum_diag")
cor_data2 <- sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp[sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp$eventname == "2_year_follow_up_y_arm_1", selected_vars]
cor_data2 <- cor_data2 %>%
  select(
    pvg = pvg,
    c_adhd = cbcl_scr_dsm5_adhd_r,
    ic = nihtbx_flanker_uncorrected,
    rs = basm_sum,
    sex = demo_sex_v2,
    age = interview_age,
    inc = demo_comb_income_v2_l.y,
    edu = highest_ed,
    mdd = ksads_1_840_p,
    ocd = ksads_11_917_p,
    k_adhd = sum_diag
  )

cor_data4 <- sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp[sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp$eventname == "4_year_follow_up_y_arm_1", selected_vars]
cor_data4 <- cor_data4 %>%
  select(
    pvg = pvg,
    c_adhd = cbcl_scr_dsm5_adhd_r,
    ic = nihtbx_flanker_uncorrected,
    rs = basm_sum,
    sex = demo_sex_v2,
    age = interview_age,
    inc = demo_comb_income_v2_l.y,
    edu = highest_ed
  )

mycor2 <- cor(cor_data2, use = "complete.obs")
p_values2 <- matrix(NA, ncol = ncol(mycor2), nrow = ncol(mycor2))
for (i in 1:(ncol(mycor2) - 1)) {
  for (j in (i + 1):ncol(mycor2)) {
    cor_test_result <- cor.test(cor_data2[[i]], cor_data2[[j]], use = "complete.obs")
    p_values2[i, j] <- cor_test_result$p.value
    p_values2[j, i] <- cor_test_result$p.value
  }
}

cor_data4 <- cor_data4[, !(names(cor_data4) %in% c("ksads_1_840_p", "ksads_11_917_p", "eventname"))]
mycor4 <- cor(cor_data4, use = "complete.obs")
print(mycor4)
p_values4 <- matrix(NA, ncol = ncol(mycor4), nrow = ncol(mycor4))
for (i in 1:(ncol(mycor4) - 1)) {
  for (j in (i + 1):ncol(mycor4)) {
    cor_test_result <- cor.test(cor_data4[[i]], cor_data4[[j]], use = "complete.obs")
    p_values4[i, j] <- cor_test_result$p.value
    p_values4[j, i] <- cor_test_result$p.value
  }
}
blank_matrix <- matrix(0, ncol = ncol(mycor), nrow = ncol(mycor))

##################  RSA  ########################################
library(RSA) 
library(lavaan)

###original data RSA###
#two year 5.1
two_year_multi_data <- sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp[sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp$eventname == "2_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "sum_diag", "pvg", "nihtbx_flanker_uncorrected", "basm_sum", "demo_sex_v2", "interview_age",  "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p","site_id_l")]
#four year 5.1
four_year_multi_data <- sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp[sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp$eventname == "4_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "sum_diag", "pvg", "nihtbx_flanker_uncorrected", "basm_sum", "demo_sex_v2", "interview_age",  "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p","site_id_l")]

variables_to_standardize <- c("nihtbx_flanker_uncorrected", "basm_sum", "interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed", "ksads_1_840_p", "ksads_11_917_p", "pvg",  "cbcl_scr_dsm5_adhd_r")  
two_year_multi_data_standardized <- two_year_multi_data 
two_year_multi_data_standardized[variables_to_standardize] <- scale(two_year_multi_data[variables_to_standardize])  

four_year_multi_data <- subset(four_year_multi_data, select = -c(ksads_1_840_p, ksads_11_917_p))
four_year_multi_data_standardized <- four_year_multi_data 

four_year_multi_data_standardized <- merge(four_year_multi_data_standardized,
                                           two_year_multi_data[, c("src_subject_id", "ksads_1_840_p", "ksads_11_917_p")],
                                           by = "src_subject_id",
                                           all.x = TRUE)
four_year_multi_data_standardized <- scale(four_year_multi_data_standardized[variables_to_standardize])  

model21 <- RSA(pvg ~ basm_sum * nihtbx_flanker_uncorrected, two_year_multi_data_standardized,  missing = 'listwise', control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p"), center.control.variables = FALSE)
summary(model21)
model22 <- RSA(cbcl_scr_dsm5_adhd_r ~ basm_sum * nihtbx_flanker_uncorrected, two_year_multi_data_standardized,  missing = 'listwise', control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p"), center.control.variables = FALSE)
summary(model22)

model41 <- RSA(pvg ~ basm_sum * nihtbx_flanker_uncorrected, four_year_multi_data_standardized,  missing = 'listwise', control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p"), center.control.variables = FALSE)
summary(model41)
model42 <- RSA(cbcl_scr_dsm5_adhd_r ~ basm_sum * nihtbx_flanker_uncorrected, four_year_multi_data_standardized,  missing = 'listwise', control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p"), center.control.variables = FALSE)
summary(model42)


## contour plot for risk patterns of PVG and ADHD
sorted_two_year <- two_year_multi_data_standardized[order(two_year_multi_data_standardized$pvg, decreasing = TRUE), ]
# n * prevalence of PVG
sorted_two_year$pvg[487]
sorted_two_year$pvg[877]
sorted_two_year_adhd <- two_year_multi_data_standardized[order(two_year_multi_data_standardized$cbcl_scr_dsm5_adhd_r, decreasing = TRUE), ]
# n * prevalence of PVG
sorted_two_year$cbcl_scr_dsm5_adhd_r[487]
# adhd plot 
adhd_cont <- read.csv("adhd contour.csv")
adhd_x <- adhd_cont[1:49,1]  # x axis data
adhd_y <- adhd_cont[1:49,2]  # y axis data
adhd_z <- as.matrix(sapply(adhd_cont[1:49, 3:51], as.numeric))  
# contour plot 
png("contour_plot_adhd.png", width = 1734, height = 1484)
par(cex.axis = 2.5, cex.main = 2.5)
contour(adhd_x, adhd_y, adhd_z, main = "ADHD contour", xlab = "RS", ylab = "IC")  
z_geq_1_2 <- adhd_z >= 1.338
lighter_blue_rgb1 <- rgb(232, 247, 248, maxColorValue = 255)   
image(adhd_x, adhd_y, matrix(ifelse(z_geq_1_2, 1, NA), ncol = ncol(adhd_z)),   
      col = lighter_blue_rgb1, add = TRUE) 
z_geq_1_3 <- adhd_z >= 2.061 
image(adhd_x, adhd_y, matrix(ifelse(z_geq_1_3, 1, NA), ncol = ncol(adhd_z)),   
      col = adjustcolor("lightblue", alpha.f = 1), add = TRUE) 
dev.off()

# pvg  
pvg_cont <- read.csv("pvg contour.csv")
pvg_x <- pvg_cont[1:49,1]  # x axis data  
pvg_y <- pvg_cont[1:49,2]  # y axis data 
pvg_z <- as.matrix(sapply(pvg_cont[1:49, 3:51], as.numeric))  
png("contour_plot_pvg.png", width = 1734, height = 1484)
par(cex.axis = 2.5, cex.main = 2.5)
contour(pvg_x, pvg_y, pvg_z, main = "PVG contour", xlab = "RS", ylab = "IC")  
# 877 data prevalence
pvg_z_geq_1_3 <- pvg_z >= 1.35
image(pvg_x, pvg_y, matrix(ifelse(pvg_z_geq_1_3, 1, NA), ncol = ncol(pvg_z)),   
      col = lighter_pink, add = TRUE) 
contour(pvg_x, pvg_y, pvg_z_geq_1_2 * max(pvg_z), levels = 1, add = TRUE, col = lighter_pink, lwd = 2)
pvg_z_geq_1_0 <- pvg_z >= 1.8
lighter_pink <- rgb(247, 230, 241, maxColorValue = 255)   
image(pvg_x, pvg_y, matrix(ifelse(pvg_z_geq_1_0, 1, NA), ncol = ncol(pvg_z)),   
      col = "pink", add = TRUE) # add = TRUE 表示添加到现有图形上 
dev.off()


######################################################
#################sensitivity analysis#################
######################################################

install.packages("mice")  
library(mice)
imp <- mice(sex_race_site_fam_age_race_igd_sma_adhd_upps_ss_fs_ic_ksads_dep_comp, m = 100, maxit = 100, method = 'pmm', seed = 500)  
save(imp, file = "imp_data.RData")

#############multiple imputation data RSA#############
library(mice)  
library(RSA)  
library(dplyr)  
library(tidyr) 

variables_to_standardize <- c("nihtbx_flanker_uncorrected", "basm_sum", "interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed", "ksads_1_840_p", "ksads_11_917_p", "pvg", "sum_diag", "cbcl_scr_dsm5_adhd_r")  

#PVG two year Iterate the multiple imputation dataset
run_rsa_analysis <- function(data) {  
  two_data <- data[data$eventname == "2_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "sum_diag", "pvg", "nihtbx_flanker_uncorrected", "basm_sum", "demo_sex_v2", "interview_age",  "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p","site_id_l")]
  data_standardized <- two_data %>%  
    mutate(across(all_of(variables_to_standardize), scale))  
  
  model <- RSA(pvg ~ basm_sum * nihtbx_flanker_uncorrected,   
               data = data_standardized,    
               missing = 'listwise',   
               control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p"),  
               center.control.variables = FALSE)  
  
  model_summary <- summary(model)  
  return(list(model = model, summary = model_summary))  
}  
rsa_results <- lapply(1:100, function(i) {  
  imputed_data <- complete(imp, i)  
  result <- run_rsa_analysis(imputed_data)  
  result  
})  

target_values_matrix <- matrix(NA, nrow = 100, ncol = 10)  
for (i in 1:length(rsa_results)) {  
  values <- rsa_results[[i]]$model$models$full@ParTable$est  
  first_five_values <- values[1:5]  
  last_five_values <- if (length(values) >= 95) {  
    values[91:95]  
  } else {  
    rep(NA, 5)  
  }  
  target_values_matrix[i, ] <- c(first_five_values, last_five_values)  
}  
mean_mat_pvg2 <- colMeans(target_values_matrix, na.rm = TRUE)  
se_mat_pvg2 <- apply(target_values_matrix, 2, function(x) sd(x, na.rm = TRUE))  
print(mean_mat_pvg2)  
print(se_mat_pvg2)

sd_results_pvg2 <- list()  
for (i in 1:length(rsa_results)) {  
  values <- rsa_results[[i]]$model$models$full@ParTable$se  
  
  if (length(values) >= 15) {  
    selected_values <- c(values[1:5], values[91:95])  
    sd_results_pvg2[[i]] <- selected_values  
  } else {  
    sd_results_pvg2[[i]] <- rep(NA, 10)    
  }  
}  
mean_sd_pvg2 <- numeric(10)  
valid_count <- 0  
for (i in 1:length(sd_results_pvg2)) {  
  values <- sd_results_pvg2[[i]]  
  if (length(values) == 10 && !any(is.na(values))) {  
    for (j in 1:10) {  
      mean_sd_pvg2[j] <- mean_sd_pvg2[j] + values[j]  
    }  
    valid_count <- valid_count + 1  
  }  
}  
if (valid_count > 0) {  
  mean_sd_pvg2 <- mean_sd_pvg2 / valid_count  
} else {  
  warning("没有有效的结果集来计算均值。")  
}  
print(mean_sd_pvg2)

#### t & p value###
calculate_t_and_p100 <- function(mean_values, sd_values, n = 12, mu_0 = 0) {  
  results <- list()  
  for (i in seq_along(mean_values)) {  
    mean_val <- mean_values[i]  
    sd_val <- sd_values[i]  
    t_val <- (mean_val - mu_0) / (sd_val / sqrt(n))  
    p_val <- 2 * pt(-abs(t_val), df = n - 1)  
    results[[i]] <- list(mean = mean_val, sd = sd_val, t_value = t_val, p_value = p_val)  
  }  
  return(results)  
}  
results_tp_pvg2 <- calculate_t_and_p100(mean_mat_pvg2, se_mat_pvg2)  
for (i in seq_along(results_tp_pvg2)) {  
  cat("参数", i, ":\n")  
  cat("  均值:", results_tp_pvg2[[i]]$mean, "\n")  
  cat("  标准误:", results_tp_pvg2[[i]]$sd, "\n")  
  cat("  t值:", results_tp_pvg2[[i]]$t_value, "\n")  
  cat("  p值:", results_tp_pvg2[[i]]$p_value, "\n\n")  
}

#ADHD Iterate the multiple imputation dataset
run_rsa_analysis_adhd <- function(data) {  
  two_data <- data[data$eventname == "2_year_follow_up_y_arm_1", c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "sum_diag", "pvg", "nihtbx_flanker_uncorrected", "basm_sum", "demo_sex_v2", "interview_age",  "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p","site_id_l")]
  data_standardized <- two_data %>%  
    mutate(across(all_of(variables_to_standardize), scale))  
  
  model <- RSA(cbcl_scr_dsm5_adhd_r ~ basm_sum * nihtbx_flanker_uncorrected,   
               data = data_standardized,    
               missing = 'listwise',   
               control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y", "highest_ed","ksads_1_840_p", "ksads_11_917_p"),  
               center.control.variables = FALSE)  
  
  return(list(model = model))  
}  
rsa_results_adhd <- lapply(1:100, function(i) {  
  imputed_data <- complete(imp, i)  
  result <- run_rsa_analysis_adhd(imputed_data)  
  result  
})  

target_matrix_adhd2 <- matrix(NA, nrow = 100, ncol = 10)  
for (i in 1:length(rsa_results_adhd)) {  
  values <- rsa_results_adhd[[i]]$model$models$full@ParTable$est  
  first_five_values <- values[1:5]  
  last_five_values <- if (length(values) >= 95) {  
    values[91:95]  
  } else {  
    rep(NA, 5)  
  }  
  target_matrix_adhd2[i, ] <- c(first_five_values, last_five_values)  
}  
mean_mat_adhd2 <- colMeans(target_matrix_adhd2, na.rm = TRUE)  
se_mat_adhd2 <- apply(target_matrix_adhd2, 2, function(x) sd(x, na.rm = TRUE))  
print(mean_mat_adhd2)  
print(se_mat_adhd2)

#mean of sd
sd_adhd2 <- list()  
for (i in 1:length(rsa_results_adhd)) {  
  values <- rsa_results_adhd[[i]]$model$models$full@ParTable$se  
  
  if (length(values) >= 15) {  
    selected_values <- c(values[1:5], values[91:95])  
    sd_adhd2[[i]] <- selected_values  
  } else {  
    sd_adhd2[[i]] <- rep(NA, 10)   
  }  
}  
mean_sd_adhd2 <- numeric(10)  
valid_count <- 0   
for (i in 1:length(sd_adhd2)) {  
  values <- sd_adhd2[[i]]  
  if (length(values) == 10 && !any(is.na(values))) {    
    for (j in 1:10) {  
      mean_sd_adhd2[j] <- mean_sd_adhd2[j] + values[j]  
    }  
    valid_count <- valid_count + 1   
  }  
}  
if (valid_count > 0) {  
  mean_sd_adhd2 <- mean_sd_adhd2 / valid_count  
} else {  
  warning("没有有效的结果集来计算均值。")  
}  
print(mean_sd_adhd2)

#### t & p value###
calculate_t_and_p100 <- function(mean_values, sd_values, n = 12, mu_0 = 0) {  
  results <- list()  
  for (i in seq_along(mean_values)) {  
    mean_val <- mean_values[i]  
    sd_val <- sd_values[i]  
    t_val <- (mean_val - mu_0) / (sd_val / sqrt(n))  
    p_val <- 2 * pt(-abs(t_val), df = n - 1)  
    results[[i]] <- list(mean = mean_val, sd = sd_val, t_value = t_val, p_value = p_val)  
  }  
  return(results)  
}  
results_tp_adhd2 <- calculate_t_and_p100(mean_mat_adhd2, se_mat_adhd2)  
for (i in seq_along(results_tp_adhd2)) {  
  cat("参数", i, ":\n")  
  cat("  均值:", results_tp_adhd2[[i]]$mean, "\n")  
  cat("  标准误:", results_tp_adhd2[[i]]$sd, "\n")  
  cat("  t值:", results_tp_adhd2[[i]]$t_value, "\n")  
  cat("  p值:", results_tp_adhd2[[i]]$p_value, "\n\n")  
}

##four-year pvg and ADHD####
##four-year pvg##
variables_to_standardize_y4 <- c("nihtbx_flanker_uncorrected", "basm_sum", "interview_age", "demo_sex_v2",   
                                 "demo_comb_income_v2_l.y", "highest_ed", "ksads_1_840_p", "ksads_11_917_p",
                                 "pvg", "cbcl_scr_dsm5_adhd_r")  
run_rsa_analysis <- function(data) {  
  four_data <- data[data$eventname == "4_year_follow_up_y_arm_1",   
                    c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "pvg", "nihtbx_flanker_uncorrected",   
                      "basm_sum", "demo_sex_v2", "interview_age", "demo_comb_income_v2_l.y", "highest_ed",  
                      "ksads_1_840_p", "ksads_11_917_p", "site_id_l")]  
    data_standardized <- four_data %>%  
    dplyr::mutate(across(all_of(variables_to_standardize_y4), ~scale(.x, scale = TRUE)))  
  
  modelPvg4 <- RSA(pvg ~ basm_sum * nihtbx_flanker_uncorrected,  
                   data = data_standardized,  
                   missing = 'listwise',  
                   control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y",   
                                         "highest_ed", "ksads_1_840_p", "ksads_11_917_p"),  
                   center.control.variables = FALSE)  
  
  model_summary_pvg4 <- summary(modelPvg4)  
  
  return(list(model = modelPvg4, summary = model_summary_pvg4))  
}  

rsa_results_pvg4 <- lapply(1:100, function(i) {  
  imputed_data <- complete(imp, i)  
  
  result_pvg4 <- tryCatch({  
    run_rsa_analysis(imputed_data)  
  }, error = function(e) {  
    cat(sprintf("Error processing imputation set %d: %s\n", i, e$message))  
    return(NULL)  
  })  
  
  return(result_pvg4)  
})  

rsa_results_pvg4_filtered <- Filter(Negate(is.null), rsa_results_pvg4) 

target_matrix_pvg4 <- matrix(NA, nrow = 100, ncol = 10)  
for (i in 1:length(rsa_results_pvg4_filtered)) {  
  values <- rsa_results_pvg4_filtered[[i]]$model$models$full@ParTable$est  
  first_five_values <- values[1:5]  
  last_five_values <- if (length(values) >= 95) {  
    values[91:95]  
  } else {  
    rep(NA, 5)  
  }  
  target_matrix_pvg4[i, ] <- c(first_five_values, last_five_values)  
}  
mean_mat_pvg4 <- colMeans(target_matrix_pvg4, na.rm = TRUE)  
se_mat_pvg4 <- apply(target_matrix_pvg4, 2, function(x) sd(x, na.rm = TRUE))  
print(mean_mat_pvg4)  
print(se_mat_pvg4)

## t& p value##
calculate_t_and_p100 <- function(mean_values, sd_values, n = 12, mu_0 = 0) {  
  results <- list()  
  for (i in seq_along(mean_values)) {  
    mean_val <- mean_values[i]  
    sd_val <- sd_values[i]  
    t_val <- (mean_val - mu_0) / (sd_val / sqrt(n))  
    p_val <- 2 * pt(-abs(t_val), df = n - 1)  
    results[[i]] <- list(mean = mean_val, sd = sd_val, t_value = t_val, p_value = p_val)  
  }  
  return(results)  
}  
results_tp_pvg4 <- calculate_t_and_p100(mean_mat_pvg4, se_mat_pvg4)  
pvg4_t <- numeric(10)   
pvg4_p <- numeric(10)
for (i in 1:10) {  
  pvg4_t[i] <- results_tp_pvg4[[i]]$t_value 
  pvg4_p[i] <- results_tp_pvg4[[i]]$p_value 
}
print(pvg4_t)
print(pvg4_p)

## mean of sd ##
variance_values_pvg4 <- numeric(10)  
for (i in 1:length(results_pvg4)) {  
  values <- results_pvg4[[i]]  
  if (length(values) == 10) {  
    for (j in 1:10) {  
      variance_values_pvg4[j] <- variance_values_pvg4[j] + (values[j] - mean_values_pvg4[j])^2  
    }  
  } else {  
    warning(paste("结果集", i, "不包含10个值，已跳过计算方差。"))  
  }  
}  
n_valid_results <- sum(sapply(results_pvg4, length) == 10)    
variance_values_pvg4 <- variance_values_pvg4 / (n_valid_results - 1)  
sd_values_pvg4 <- sqrt(variance_values_pvg4)  
print(sd_values_pvg4)

#########################four-year adhd######################
variables_to_standardize_y4 <- c("nihtbx_flanker_uncorrected", "basm_sum", "interview_age", "demo_sex_v2",   
                                 "demo_comb_income_v2_l.y", "highest_ed", "ksads_1_840_p", "ksads_11_917_p",
                                 "pvg", "cbcl_scr_dsm5_adhd_r")  
run_rsa_analysis <- function(data) {  
  four_data <- data[data$eventname == "4_year_follow_up_y_arm_1",   
                    c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "pvg", "nihtbx_flanker_uncorrected",   
                      "basm_sum", "demo_sex_v2", "interview_age", "demo_comb_income_v2_l.y", "highest_ed",  
                      "ksads_1_840_p", "ksads_11_917_p", "site_id_l")]  
    data_standardized <- four_data %>%  
    dplyr::mutate(across(all_of(variables_to_standardize_y4), ~scale(.x, scale = TRUE)))  
  
  modeladhd4 <- RSA(cbcl_scr_dsm5_adhd_r ~ basm_sum * nihtbx_flanker_uncorrected,  
                    data = data_standardized,  
                    missing = 'listwise',  
                    control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y",   
                                          "highest_ed", "ksads_1_840_p", "ksads_11_917_p"),  
                    center.control.variables = FALSE)  
  model_summary_adhd4 <- summary(modeladhd4)  
  
  return(list(model = modeladhd4, summary = model_summary_adhd4))  
}  

rsa_results_adhd4 <- lapply(1:100, function(i) {  
  imputed_data <- complete(imp, i)  
  
  result_adhd4 <- tryCatch({  
    run_rsa_analysis(imputed_data)  
  }, error = function(e) {  
    cat(sprintf("Error processing imputation set %d: %s\n", i, e$message))  
    return(NULL)   
  })  
  
  return(result_adhd4)  
})  

rsa_results_adhd4_filtered <- Filter(Negate(is.null), rsa_results_adhd4) 
## mean of sd #
target_matrix_adhd4 <- matrix(NA, nrow = 100, ncol = 10)  
for (i in 1:length(rsa_results_adhd4_filtered)) {  
  values <- rsa_results_adhd4_filtered[[i]]$model$models$full@ParTable$est  
  first_five_values <- values[1:5]  
  last_five_values <- if (length(values) >= 95) {  
    values[91:95]  
  } else {  
    rep(NA, 5)  
  }  
  target_matrix_adhd4[i, ] <- c(first_five_values, last_five_values)  
}  
mean_mat_adhd4 <- colMeans(target_matrix_adhd4, na.rm = TRUE)  
se_mat_adhd4 <- apply(target_matrix_adhd4, 2, function(x) sd(x, na.rm = TRUE))  
print(mean_mat_adhd4)  
print(se_mat_adhd4)

## t & p value##
calculate_t_and_p100 <- function(mean_values, sd_values, n = 12, mu_0 = 0) {  
  results <- list()  
  for (i in seq_along(mean_values)) {  
    mean_val <- mean_values[i]  
    sd_val <- sd_values[i]  
    t_val <- (mean_val - mu_0) / (sd_val / sqrt(n))  
    p_val <- 2 * pt(-abs(t_val), df = n - 1)  
    results[[i]] <- list(mean = mean_val, sd = sd_val, t_value = t_val, p_value = p_val)  
  }  
  return(results)  
}  
results_tp_adhd4 <- calculate_t_and_p100(mean_mat_adhd4, se_mat_adhd4)  
adhd4_t <- numeric(10)  
adhd4_p <- numeric(10)
for (i in 1:10) {  
  adhd4_t[i] <- results_tp_adhd4[[i]]$t_value 
  adhd4_p[i] <- results_tp_adhd4[[i]]$p_value 
}

results_adhd4 <- list()  
for (i in 1:length(rsa_results_adhd4_filtered)) {  
  values <- rsa_results_adhd4_filtered[[i]]$model$models$full@ParTable$est  
  if (length(values) >= 15) {  
    selected_values <- c(values[1:5], values[91:95])  
    results_adhd4[[i]] <- selected_values  
  } else {  
    results_adhd4[[i]] <- rep(NA, 10)   
  }  
}  

mean_values <- numeric(10)  
for (i in 1:length(results_adhd4)) {  
  values <- results_adhd4[[i]]  
  if (length(values) == 10) {  
    for (j in 1:10) {  
      mean_values[j] <- mean_values[j] + values[j]  
    }  
  } else {  
    warning(paste("结果集", i, "不包含10个值，已跳过。"))  
  }  
}  

mean_values_adhd4 <- mean_values / length(results_adhd4) 
sd_values <- numeric(10)  
for (i in 1:length(results_adhd4)) {  
  values <- results_adhd4[[i]]  
  if (length(values) == 10) {  
    for (j in 1:10) {  
      if (!is.na(values[j])) {  # make sure !NA  
        deviation <- values[j] - mean_values_adhd4[j]    
        sd_values[j] <- sd_values[j] + deviation^2    
      }  
    }  
  } else {  
  }  
}  

n_valid <- sum(!sapply(results_adhd4, function(x) any(is.na(x))))   
sd_values_adhd4 <- sqrt(sd_values / (n_valid - 1))  

if (n_valid <= 1) {  
  warning("没有足够的非NA结果集来计算标准差，标准差将被设置为NA。")  
  sd_values_adhd4 <- rep(NA, 10)  
}  

###two-year—pvg separate test###
prepare_data_pvg2 <- function(data) {  
  two_data <- data[data$eventname == "2_year_follow_up_y_arm_1",  
                   c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "pvg", "nihtbx_flanker_uncorrected",  
                     "basm_sum", "demo_sex_v2", "interview_age", "demo_comb_income_v2_l.y", "highest_ed",  
                     "ksads_1_840_p", "ksads_11_917_p", "site_id_l")]  
  
  data_standardized <- two_data %>%  
    dplyr::mutate(across(all_of(variables_to_standardize), ~scale(.x, scale = TRUE)))  
  
  return(data_standardized)  
}

run_rsa_model <- function(data) {  
  modelPvg2 <- RSA::RSA(pvg ~ basm_sum * nihtbx_flanker_uncorrected,  
                        data = data,  
                        missing = 'listwise',  
                        control.variables = c("interview_age", "demo_sex_v2", "demo_comb_income_v2_l.y",  
                                              "highest_ed", "ksads_1_840_p", "ksads_11_917_p"),  
                        center.control.variables = FALSE)  
  
  model_summary_pvg2 <- summary(modelPvg2)  
  return(list(model = modelPvg2, summary = model_summary_pvg2))  
}

rsa_results_pvg2 <- lapply(1:100, function(i) {  
  imputed_data <- mice::complete(imp, i)  
  
  prepared_data_pvg2 <- prepare_data_pvg2(imputed_data)  
  
  result_pvg2 <- tryCatch({  
    run_rsa_model(prepared_data_pvg2)  
  }, error = function(e) {  
    cat(sprintf("Error processing imputation set %d: %s\n", i, e$message))  
    return(NULL)   
  })  
  
  return(list(prepared_data_pvg2 = prepared_data_pvg2, rsa_results_pvg2 = result_pvg2))  
})  

beta_indices_pvg2 <- 2:12  
beta_averages_pvg2 <- numeric(length(beta_indices_adhd))  
r_squared_average_pvg2 <- 0  
for (i in 1:length(rsa_results_pvg2)) {  
  beta_values_pvg2 <- rsa_results_pvg2[[i]]$model$models$full@Model@GLIST$beta[1, beta_indices_pvg2]  
  beta_averages_pvg2 <- beta_averages_pvg2 + beta_values_pvg2  
  r_squared_value_pvg2 <- rsa_results_pvg2[[i]]$model$r.squared  
  r_squared_average_pvg2 <- r_squared_average_pvg2 + r_squared_value_pvg2  
}  
beta_averages_pvg2 <- beta_averages_pvg2 / length(rsa_results_pvg2)  
r_squared_average_pvg2 <- r_squared_average_pvg2 / length(rsa_results_pvg2)  
rsa_results_pvg4_filtered <- Filter(function(x) !is.null(x$rsa_result), rsa_results_pvg4)  

sel_data <- function(data) {  
  rsa_data4 <- data[data$eventname == "4_year_follow_up_y_arm_1",  
                    c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "pvg", "nihtbx_flanker_uncorrected",  
                      "basm_sum", "demo_sex_v2", "interview_age", "demo_comb_income_v2_l.y", "highest_ed",  
                      "ksads_1_840_p", "ksads_11_917_p", "site_id_l")]  
  return(rsa_data4)  
}  

rsa_data_list <- lapply(1:100, function(i) {  
  imputed_data <- mice::complete(imp, i)  
  
  return(sel_data(imputed_data))  
})  

# 计算每套数据集中 ksads_11_917_p 的均值  
ksads_11_917_p_means <- lapply(rsa_data_list, function(data) {  
  mean_value <- mean(data$ksads_11_917_p, na.rm = TRUE)  
  return(mean_value)  
})  

ksads_11_917_p_means_vector <- unlist(ksads_11_917_p_means)  
imputation_names <- paste("Imputation", 1:100, sep = "_")  

ksads_11_917_p_means_df <- data.frame(  
  Imputation = imputation_names,  
  Mean_Value = ksads_11_917_p_means_vector  
)  

count_zeros <- sum(ksads_11_917_p_means_df$Mean_Value == 0)  

mean_values <- numeric(imp$m) 

for (i in 1:imp$m) {  
  imputed_data <- mice::complete(imp, i)  
  mean_value <- mean(imputed_data$ksads_11_917_p, na.rm = TRUE)  
  mean_values[i] <- mean_value  
}  

##selected two-year only
prepare_data <- function(data) {  
  two_data <- data[data$eventname == "2_year_follow_up_y_arm_1",  
                   c("src_subject_id", "eventname", "cbcl_scr_dsm5_adhd_r", "pvg", "nihtbx_flanker_uncorrected",  
                     "basm_sum", "demo_sex_v2", "interview_age", "demo_comb_income_v2_l.y", "highest_ed",  
                     "ksads_1_840_p", "ksads_11_917_p", "site_id_l")]  
  
  variables_to_standardize <- c("cbcl_scr_dsm5_adhd_r", "pvg", "nihtbx_flanker_uncorrected", "basm_sum", # 举例，根据需要调整  
                                "interview_age", "demo_comb_income_v2_l.y", "ksads_1_840_p", "ksads_11_917_p", "demo_sex_v2", "highest_ed")  
  data_standardized <- four_data %>%  
    dplyr::mutate(across(all_of(intersect(names(four_data), variables_to_standardize)), ~scale(.x, scale = TRUE)))  
  return(data_standardized)  
}  

rsa_results_pvg2 <- lapply(1:100, function(i) {  
  imputed_data <- mice::complete(imp, i)  
  prepared_data <- prepare_data(imputed_data)  
  return(prepared_data)  
})  

## est/se
t_value <- 0.64705882 
# = n-12-1
df <- 10961
abs_t_value <- abs(t_value)  
p_value_one_sided <- 1 - pt(abs_t_value, df)  
p_value_abs_t <- 2 * p_value_one_sided  

t_value <- 0.011 / 0.017   
p_value <- 0.525  

find_df <- function(df) {  
  abs(pt(t_value, df) - p_value)  
}  

result <- optimize(find_df, interval = c(1, 10000))  

estimated_df <- result$minimum  
print(paste("Estimated degrees of freedom:", estimated_df))  

#freedom of a1-a5：n（10973）- 6（full model）- 6（control variable）
t_value <- 0.003684791 / 0.013
df <- 5234-12
abs_t_value <- abs(t_value)  
p_value_one_sided <- 1 - pt(abs_t_value, df)  
2 * p_value_one_sided  
pt(t_value, df) 

########### sensitivity analysis-multileve RSA########
ic.M.fix <- mean( two_year_multi_data$nihtbx_flanker_uncorrected, na.rm = TRUE )
fs.M.fix <- mean( two_year_multi_data$bis_y_ss_bas_fs, na.rm = TRUE )
bas.M.fix <- mean( two_year_multi_data$basm_sum, na.rm = TRUE )
two_year_multi_data$ic.fix <- two_year_multi_data$nihtbx_flanker_uncorrected - ic.M.fix
two_year_multi_data$fs.fix <- two_year_multi_data$bis_y_ss_bas_fs - fs.M.fix
two_year_multi_data$bas.fix <- two_year_multi_data$basm_sum - bas.M.fix
two_year_multi_data$ic2.fix <- two_year_multi_data$ic.fix^2
two_year_multi_data$icfs.fix <- two_year_multi_data$ic.fix*two_year_multi_data$fs.fix
two_year_multi_data$fs2.fix <- two_year_multi_data$fs.fix^2
two_year_multi_data$bas2.fix <- two_year_multi_data$bas.fix^2
two_year_multi_data$icbas.fix <- two_year_multi_data$ic.fix*two_year_multi_data$bas.fix

# two year multi RSA
head(two_year_multi_data_standardized)
head(four_year_multi_data_standardized)
two_year_multi_data_standardized$ic2.fix <- two_year_multi_data_standardized$nihtbx_flanker_uncorrected^2
two_year_multi_data_standardized$icbas.fix <- two_year_multi_data_standardized$nihtbx_flanker_uncorrected*two_year_multi_data_standardized$basm_sum
two_year_multi_data_standardized$bas2.fix <- two_year_multi_data_standardized$basm_sum^2
four_year_multi_data_standardized$ic2.fix <- four_year_multi_data_standardized$nihtbx_flanker_uncorrected^2
four_year_multi_data_standardized$icbas.fix <- four_year_multi_data_standardized$nihtbx_flanker_uncorrected*four_year_multi_data_standardized$basm_sum
four_year_multi_data_standardized$bas2.fix <- four_year_multi_data_standardized$basm_sum^2

library(lme4)
library(lmerTest)
library(RSA)
library(gridExtra)
library(lavaan)
library(ggplot2)
library(lattice)
library(lmerTest)
source("MultilevelRSA.R") 
library(boot) 

# pvg
m1 <- lmer(pvg_scale ~ basm_sum + nihtbx_flanker_uncorrected + ic2.fix + icbas.fix + bas2.fix + demo_sex_v2+interview_age+demo_comb_income_v2_l.y+highest_ed+ksads_1_840_p+ksads_11_917_p
           +(1|site_id_l),data=two_year_multi_data_standardized,REML=F,
           control = lmerControl(optimizer = "bobyqa"))
summary(m1)
boot_m1 <- bootMer(m1, FUN = fixef, nsim = 1000)
boot_ci1 <- boot.ci(boot_m1, type = "perc", index = 2, conf = 0.9)
summary(boot_m1)
summary(boot_ci1)
print(boot_ci1) 
print(boot_m1)
MLRSA_AverageSurface(m1,name_vars=c("basm_sum","nihtbx_flanker_uncorrected","ic2.fix","icbas.fix","bas2.fix"),random_vars=c(NA, NA, NA, NA, NA)) 
coefficients_m1_fix <- coef(m1_fix)
vcov_matrix_m1_fix <- vcov(m1_fix)
result_m1_fix <- MLRSA_AverageSurface_FixedEffects(coefficients_m1_fix, vcov_matrix_m1_fix)
MLRSA_AverageSurfacePlot(m1,name_vars=c("ic.fix","fs.fix","ic2.fix","icfs.fix","fs2.fix"),
                         outcome="pvg",data=two_year_multi_data,
                         xlab="inhibitory control",ylab="fun seeking",zlab="problematic video gaming")

# adhd
m2 <- lmer(adhd_scale ~ basm_sum + nihtbx_flanker_uncorrected + bas2.fix + icbas.fix + ic2.fix + demo_sex_v2+interview_age+demo_comb_income_v2_l.y+highest_ed+ksads_1_840_p+ksads_11_917_p
           +(1|site_id_l),data=two_year_multi_data_standardized,REML=F,
           control = lmerControl(optimizer = "bobyqa"))
summary(m2)
boot_m2 <- bootMer(m2, FUN = fixef, nsim = 1000)
boot_ci_adhd2 <- boot.ci(boot_m2, type = "basic", index = 2, conf=0.9991)
print(boot_m2)
print(boot_ci_adhd2) 
MLRSA_AverageSurface(m2,name_vars=c("basm_sum","nihtbx_flanker_uncorrected","bas2.fix","icbas.fix","ic2.fix"),random_vars=c(NA,NA,NA,NA,NA)) 
MLRSA_AverageSurfacePlot(m2,name_vars=c("ic.c","fs.c","ic2.c","icfs.c","fs2.c"),
                         outcome="cbcl_scr_dsm5_adhd_t",data=two_year_multi_data,
                         xlab="inhibitory control",ylab="fun seeking",zlab="cbcl_adhd")

# four year multi RSA

four_m1 <- lmer(pvg_scale ~ basm_sum + nihtbx_flanker_uncorrected + bas2.fix + icbas.fix + ic2.fix + demo_sex_v2+interview_age+demo_comb_income_v2_l.y+highest_ed+ksads_1_840_p+ksads_11_917_p
                +(1|site_id_l),data=four_year_multi_data_standardized,REML=F,
                control = lmerControl(optimizer = "bobyqa"))
summary(four_m1)

boot_four_m1 <- bootMer(four_m1, FUN = fixef, nsim = 1000)
boot_four_ci1 <- boot.ci(boot_four_m1, type = "perc", index = 2, conf = 0.951)
summary(boot_four_m1)
summary(boot_four_ci1)
MLRSA_AverageSurface(four_m1,name_vars=c("basm_sum","nihtbx_flanker_uncorrected","bas2.fix","icbas.fix","ic2.fix"),random_vars=c(NA,NA,NA,NA,NA)) 
MLRSA_AverageSurfacePlot(four_m1,name_vars=c("ic.c","fs.c","ic2.c","icfs.c","fs2.c"),
                         outcome="pvg",data=four_year_multi_data,
                         xlab="inhibitory control",ylab="fun seeking",zlab="cbcl_adhd")

four_m2 <- lmer(adhd_scale ~ basm_sum + nihtbx_flanker_uncorrected + bas2.fix + icbas.fix + ic2.fix + demo_sex_v2+interview_age+demo_comb_income_v2_l.y+highest_ed+ksads_1_840_p+ksads_11_917_p
                +(1|site_id_l),data=four_year_multi_data_standardized,REML=F,
                control = lmerControl(optimizer = "bobyqa"))
summary(four_m2)
MLRSA_AverageSurface(four_m2,name_vars=c("basm_sum","nihtbx_flanker_uncorrected","bas2.fix","icbas.fix","ic2.fix"),random_vars=c(NA,NA,NA,NA,NA)) 



