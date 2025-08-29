library(readr)
library(dplyr)
library(broom)

# 1. 获取当前路径下所有 CSV 文件
csv_files <- list.files(pattern = "pub_evolution.*\\.csv$", full.names = TRUE)

# 2. 初始化存储结果的数据框
r2_results <- data.frame(
  file = character(),
  r_squared = numeric(),
  stringsAsFactors = FALSE
)

# 3. 循环读取文件并计算 R²
for (file in csv_files) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # 检查是否包含回归所需的列
  if (all(c("age", "spend.y", "reachable_consumers", "reachable_spending", 
            "nearby_pubs", "rating", "pubrent", "pubid", "tick") %in% colnames(df))) {
    
    # 数据清洗：同一 pubid 只保留 tick 最大的行
    df <- df %>%
      mutate(
        spend.y = as.numeric(spend.y),
        reachable_consumers = as.numeric(reachable_consumers),
        reachable_spending = as.numeric(reachable_spending)
      ) %>%
      group_by(pubid) %>%
      slice_max(order_by = tick, n = 1) %>%
      ungroup()
    
    # 回归模型（新增 reachable_spending）
    model <- lm(age ~ spend.y + reachable_consumers + reachable_spending + nearby_pubs + rating + pubrent, data = df)
    r2 <- summary(model)$r.squared
    
    # 保存结果
    r2_results <- rbind(r2_results, data.frame(file = file, r_squared = r2))
  } else {
    message(paste("文件缺少必要列:", file))
  }
}

# 4. 按 R² 排序并生成排名
r2_results <- r2_results %>%
  arrange(desc(r_squared)) %>%
  mutate(rank = row_number())

# 5. 输出完整排名
print("所有文件 R² 排名：")
print(r2_results)

# 6. 输出 R² 最高的文件
best_file <- r2_results %>% slice(1)
print("R² 最高的文件：")
print(best_file)
