library(readr)
library(dplyr)
library(car)

# 1. 读取 CSV
df <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/regression/pub_evolution_run4_a1.001_b0.2_c100_reach5_cleaned.csv")

# 2. 找出 tick == 40 且 age < 30 的存活酒吧（即存活时间过短）
to_exclude <- df %>%
  filter(tick == 40, age < 30) %>%
  pull(pubid)

# 3. 保留每个 pub 的最后 tick（即最终状态）
df <- df %>%
  group_by(pubid) %>%
  slice_max(order_by = tick, n = 1) %>%
  ungroup()

# 4. 剔除存活但 age < 30 的酒吧，创建 age_binary，并剔除 age = 0/1 的异常值
df <- df %>%
  filter(!(pubid %in% to_exclude)) %>%
  filter(!(age %in% c(0, 1))) %>%
  mutate(age_binary = ifelse(age >= 30, 1, 0))

# 5. 移除缺失值（只保留分析所需变量）
df_clean <- df %>%
  select(age_binary, spend.y, reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent) %>%
  na.omit()

# 标准化连续变量
df_clean <- df_clean %>%
  mutate(across(c(reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent), scale))

# 6. 构建逻辑回归模型
model <- glm(age_binary ~ reachable_consumers + reachable_spending + nearby_pubs + rating + pubrent,
             data = df_clean, family = binomial)

# 7. 查看模型结果
summary(model)

# 8. 检查多重共线性
vif(model)

# 9. 逐步回归
step_model <- step(model)
summary(step_model)

# 10. 输出 OR (几率比)
exp(coef(step_model))

vif(step_model)


library(ggplot2)
library(tidyr)
library(dplyr)

# 1. 选取需要可视化的变量（确保全部为数值型）
plot_data <- df_clean %>%
  mutate(across(c(reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent), as.numeric)) %>%
  select(age_binary, reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent) %>%
  pivot_longer(cols = -age_binary, names_to = "variable", values_to = "value")

# 2. 绘图：散点 + 逻辑回归曲线
ggplot(plot_data, aes(x = value, y = age_binary)) +
  geom_jitter(height = 0.05, alpha = 0.2, color = "grey") +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ x,
              color = "blue",
              size = 1) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Relationship Between Variables and Pub Survival",
    x = "Variable Value",
    y = "Probability of Survival"
  ) +
  theme_minimal(base_size = 14)

# 5. 移除缺失值（只保留分析所需变量）
df_clean_raw <- df %>%
  select(age_binary, spend.y, reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent) %>%
  na.omit()

# 保留一份标准化后的数据用于回归
df_clean <- df_clean_raw %>%
  mutate(across(c(reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent), scale))

# ... 回归建模部分保持不变 ...

# 1. 绘图使用未标准化数据
plot_data <- df_clean_raw %>%
  select(age_binary, reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent) %>%
  pivot_longer(cols = -age_binary, names_to = "variable", values_to = "value")

# 2. 绘制散点 + 逻辑回归曲线
ggplot(plot_data, aes(x = value, y = age_binary)) +
  geom_jitter(height = 0.05, alpha = 0.2, color = "grey") +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              formula = y ~ x,
              color = "blue",
              size = 1) +
  facet_wrap(~ variable, scales = "free_x") +
  labs(
    title = "Relationship Between Variables and Pub Survival",
    x = "Variable Value",
    y = "Probability of Survival"
  ) +
  theme_minimal(base_size = 14)
