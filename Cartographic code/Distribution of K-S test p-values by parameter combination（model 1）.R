options(scipen = 999)  # Prevent scientific notation in outputs

# Step 1: Read experimental data
EG <- read.csv("Gravity Model-Based (Distributed Spending, Model 1).csv", skip = 6)


# Step 2: Read control group data (assumed to be a vector of exit rates)
CG <- read.csv("realword_data.csv")  # Replace with your actual file name
CG_exit_rate <- CG$whole_london      # Replace with the correct column name for control data
CG_exit_rate <- subset(CG, year >= 2013 & year <= 2023)$whole_london

# Step 3: Get all unique run numbers
run_ids <- unique(EG$X.run.number.)

# Step 4: Initialize results storage
ks_results <- data.frame(run_id = run_ids, p_value = NA_real_)

# Step 5: Loop through each run and perform K-S test
for (i in seq_along(run_ids)) {
  run_i <- run_ids[i]
  
  run_data <- subset(EG, X.run.number. == run_i & X.step. >= 5 & X.step. <= 15)$exit.rate
  
  # Perform Kolmogorov-Smirnov (K-S) test against control group
  ks_test <- ks.test(run_data, CG_exit_rate)
  
  # Store the p-value
  ks_results$p_value[i] <- ks_test$p.value
}

# Step 6: Identify the run with the highest p-value (i.e., the best match to control group)
best_match <- ks_results[which.max(ks_results$p_value), ]

# Output all results and the best-matching run
print(ks_results)
cat("\nThe best-matching run (highest p-value) is Run:", best_match$run_id, ", p =", best_match$p_value, "\n")


library(dplyr)

# 提取 run_id 和参数的唯一组合
params <- EG %>%
  select(X.run.number., constant_a, reach_distance, constant_b, constant_c) %>%
  distinct()

# 合并 ks_results 和 params
ks_results_params <- ks_results %>%
  left_join(params, by = c("run_id" = "X.run.number."))

# 查看结果
print(ks_results_params)



model <- lm(p_value ~ constant_a + reach_distance + constant_b + constant_c, data = ks_results_params)
summary(model)


cor(ks_results_params %>% select(constant_a, reach_distance, constant_b, constant_c))

library(ggplot2)

ggplot(ks_results_params, aes(x = constant_a, y = p_value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()



library(ggplot2)
library(tidyr)

ks_results_params %>%
  pivot_longer(cols = c(constant_a, reach_distance, constant_b, constant_c), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, y = p_value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal()


model <- lm(p_value ~ constant_a + reach_distance + constant_b + constant_c, data = ks_results_params)
summary(model)



library(dplyr)

ks_summary <- ks_results_params %>%
  group_by(constant_a, reach_distance, constant_b, constant_c) %>%
  summarise(
    mean_p = mean(p_value, na.rm = TRUE),        # 平均值
    median_p = median(p_value, na.rm = TRUE),    # 中位数
    sd_p = sd(p_value, na.rm = TRUE),            # 标准差
    iqr_p = IQR(p_value, na.rm = TRUE),          # 四分位距
    min_p = min(p_value, na.rm = TRUE),
    max_p = max(p_value, na.rm = TRUE),
    n = n()                                      # 样本数量
  ) %>%
  ungroup()


ks_summary <- ks_summary %>%
  arrange(sd_p)   # 或者用 arrange(iqr_p) 依据IQR排序

ks_summary <- ks_summary %>%
  mutate(
    p_group = ifelse(mean_p > 0.05, 1, 0)  # 1 = 好的（p>0.05），0 = 其他
  ) %>%
  arrange(desc(p_group), iqr_p) %>%
  select(-p_group)  # 排序后可以移除临时列

# 基于 ks_summary 得到组合排序
ordered_groups <- ks_summary %>%
  mutate(group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")) %>%
  arrange(desc(mean_p > 0.05), iqr_p) %>%   # p>0.05优先，然后按IQR排序
  pull(group)


library(ggplot2)

ks_results_params %>%
  mutate(group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")) %>%
  ggplot(aes(x = group, y = p_value)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "p_value分布 (按参数组合分组)",
       x = "参数组合", y = "p_value")


library(ggplot2)

ks_results_params %>%
  mutate(group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-"),
         group = factor(group, levels = ordered_groups)) %>%
  ggplot(aes(x = group, y = p_value)) +
  geom_boxplot(outlier.size = 0.5) +  
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "skyblue", size = 0.6) +  # 0.05 浅蓝虚线
  scale_y_continuous(
    breaks = c(0, 0.05, 0.2, 0.4, 0.6, 0.8),  
    limits = c(0, 1)
  ) +
  coord_flip() +
  labs(
    title = "p_value distribution (Model 1)",
    x = "parameter combination", 
    y = "p_value"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 5),
    axis.text.x = element_text(size = 8),
    plot.title = element_text(size = 12),
    panel.grid.minor = element_blank(),               # 去掉所有次要网格
    panel.grid.major.x = element_line(color = "grey85"),  # 保留横线 (p_value方向)             # 去掉纵线 (参数组合方向)
  )



