options(scipen = 999)

# Step 1: Read experimental data
EG <- read.csv("Random Walk-Based (Single-Spending, Model 4).csv", skip = 6)

# Step 2: Read control group data (assumed to be a vector of exit rates)
CG <- read.csv("realword_data.csv")  # Replace with your actual file name
CG_exit_rate <- CG$whole_london      # Replace with the appropriate column name
CG_exit_rate <- subset(CG, year >= 2013 & year <= 2023)$whole_london

# Step 3: Get all unique run numbers
run_ids <- unique(EG$X.run.number.)

# Step 4: Initialize a data frame to store K-S test results
ks_results <- data.frame(run_id = run_ids, p_value = NA_real_)

# Step 5: Loop through each run and perform the Kolmogorov-Smirnov (K-S) test
for (i in seq_along(run_ids)) {
  run_i <- run_ids[i]
  
  # Filter steps: only include those where step mod 12 == 1 and step > 61
  run_data <- subset(EG, X.run.number. == run_i & X.step. %% 12 == 1 & X.step. >= 121 & X.step. <= 241)$exit.rate
  
  # Perform the K-S test against the control group
  ks_test <- ks.test(run_data, CG_exit_rate)
  
  # Store the p-value
  ks_results$p_value[i] <- ks_test$p.value
}

# Step 6: Identify the run with the highest p-value (best match to the control group)
best_match <- ks_results[which.max(ks_results$p_value), ]

# Output all results and highlight the best-matching run
print(ks_results)
cat("\nThe best-matching run (highest p-value) is Run:", best_match$run_id, ", p =", best_match$p_value, "\n")

library(dplyr)
library(ggplot2)

# Step 7: 提取 run_id 和参数的唯一组合
params <- EG %>%
  select(X.run.number., constant_a, reach_distance, constant_b, constant_c) %>%
  distinct()

# 合并 ks_results 和参数
ks_results_params <- ks_results %>%
  left_join(params, by = c("run_id" = "X.run.number."))

# Step 8: 计算各参数组合的汇总统计
ks_summary <- ks_results_params %>%
  group_by(constant_a, reach_distance, constant_b, constant_c) %>%
  summarise(
    mean_p = mean(p_value, na.rm = TRUE),
    median_p = median(p_value, na.rm = TRUE),
    sd_p = sd(p_value, na.rm = TRUE),
    iqr_p = IQR(p_value, na.rm = TRUE),
    min_p = min(p_value, na.rm = TRUE),
    max_p = max(p_value, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Step 9: 排序（p > 0.05 优先，其次按 IQR 排序）
ks_summary <- ks_summary %>%
  mutate(
    p_group = ifelse(mean_p > 0.05, 1, 0)
  ) %>%
  arrange(desc(p_group), iqr_p) %>%
  select(-p_group)

# 基于 ks_summary 生成组合排序
ordered_groups <- ks_summary %>%
  mutate(group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")) %>%
  arrange(desc(mean_p > 0.05), iqr_p) %>%
  pull(group)

# Step 10: 绘制箱型图
ks_results_params %>%
  mutate(group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-"),
         group = factor(group, levels = ordered_groups)) %>%
  ggplot(aes(x = group, y = p_value)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "skyblue", size = 0.6) + # 0.05 虚线
  scale_y_continuous(
    breaks = c(0, 0.05, 0.2, 0.4, 0.6, 0.8),
    limits = c(0, 1)
  ) +
  coord_flip() +
  labs(
    title = "p_value distribution (Model 4)",
    x = "parameter combination",
    y = "p_value"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 8),
    plot.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85")
  )


library(dplyr)
library(ggplot2)

# Step 7: 提取 run_id 和参数的唯一组合
params <- EG %>%
  select(X.run.number., constant_a, reach_distance, constant_b, constant_c) %>%
  distinct()

# 合并 ks_results 和参数
ks_results_params <- ks_results %>%
  left_join(params, by = c("run_id" = "X.run.number."))

# Step 8: 计算各参数组合的汇总统计
ks_summary <- ks_results_params %>%
  group_by(constant_a, reach_distance, constant_b, constant_c) %>%
  summarise(
    mean_p = mean(p_value, na.rm = TRUE),
    median_p = median(p_value, na.rm = TRUE),
    sd_p = sd(p_value, na.rm = TRUE),
    iqr_p = IQR(p_value, na.rm = TRUE),
    min_p = min(p_value, na.rm = TRUE),
    max_p = max(p_value, na.rm = TRUE),
    n = n()
  ) %>%
  ungroup()

# Step 9: 排序（p > 0.05 优先，其次按 IQR 排序）
ks_summary <- ks_summary %>%
  mutate(
    p_group = ifelse(mean_p > 0.05, 1, 0)
  ) %>%
  arrange(desc(p_group), iqr_p) %>%
  select(-p_group)

# 基于 ks_summary 生成组合排序
ordered_groups <- ks_summary %>%
  mutate(group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")) %>%
  arrange(desc(mean_p > 0.05), iqr_p) %>%
  pull(group)

# Step 10: 绘制箱型图
ks_results_params %>%
  mutate(group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-"),
         group = factor(group, levels = ordered_groups)) %>%
  ggplot(aes(x = group, y = p_value)) +
  geom_boxplot(outlier.size = 0.5) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "skyblue", size = 0.6) + # 0.05 虚线
  scale_y_continuous(
    breaks = c(0, 0.05, 0.2, 0.4, 0.6, 0.8),
    limits = c(0, 1)
  ) +
  coord_flip() +
  labs(
    title = "p_value distribution (Model 4)",
    x = "parameter combination",
    y = "p_value"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 8),
    plot.title = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey85")
  )
