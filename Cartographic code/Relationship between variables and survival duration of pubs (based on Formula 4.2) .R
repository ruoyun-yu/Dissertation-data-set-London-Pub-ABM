library(readr)
library(dplyr)
library(car)

# 1. 读取 CSV
df <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/regression/pub_evolution_run4_a1.001_b0.2_c100_reach5_cleaned.csv")

# 2. 找出 tick == 40 且 age < 30 的存活酒吧（短期生存者）
to_exclude <- df %>%
  filter(tick == 40, age < 30) %>%
  pull(pubid)

# 3. 保留每个 pub 的最后 tick（最终状态）
df <- df %>%
  group_by(pubid) %>%
  slice_max(order_by = tick, n = 1) %>%
  ungroup()

# 4. 剔除短期存活酒吧和异常值
df <- df %>%       
  filter(!(age %in% c(0, 1)))           # 剔除异常值

# 5. 移除缺失值（仅保留分析变量）
df_clean <- df %>%
  select(age, spend.y, reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent) %>%
  na.omit()

# 6. 标准化连续变量（可选）
df_clean <- df_clean %>%
  mutate(across(c( reachable_consumers, reachable_spending, nearby_pubs, rating, pubrent), scale))

# 7. 线性回归模型
model <- lm(age ~ reachable_consumers + reachable_spending + nearby_pubs + rating + pubrent,
            data = df_clean)

# 8. 查看模型结果
summary(model)

# 9. 检查多重共线性
vif(model)
