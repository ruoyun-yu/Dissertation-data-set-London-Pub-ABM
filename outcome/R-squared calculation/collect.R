library(readr)
library(dplyr)
library(broom)
library(stringr)

# 1. 获取所有 pub 文件
csv_files <- list.files(pattern = "pub.*\\.csv$", full.names = TRUE)

# 2. 初始化结果表
r2_results <- data.frame(
  file = character(),
  r_squared = numeric(),
  stringsAsFactors = FALSE
)

# 3. 循环处理每个 pub 文件
for (file in csv_files) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # 检查必要列
  if (all(c("age", "spend", "nearby_pubs", "rating", "pubrent", "pubid", "tick", "xcor", "ycor") %in% colnames(df))) {
    
    # 数据清洗
    df <- df %>%
      mutate(spend = as.numeric(spend)) %>%
      group_by(pubid) %>%
      slice_max(order_by = tick, n = 1) %>%
      ungroup()
    
    # 提取 run 名称
    run_name <- str_extract(basename(file), "run[0-9]+")
    
    # 查找对应的 patch 文件
    patch_file <- list.files(pattern = paste0("patch.*", run_name, ".*\\.csv$"), full.names = TRUE)
    
    if (length(patch_file) == 1) {
      patch_df <- read_csv(patch_file, show_col_types = FALSE)
      
      if (all(c("xcor", "ycor") %in% colnames(patch_df))) {
        
        # 四舍五入坐标
        df <- df %>%
          mutate(xcor = round(xcor), ycor = round(ycor))
        
        patch_df <- patch_df %>%
          mutate(xcor = round(xcor), ycor = round(ycor))
        
        # 给 patch 的非坐标列加前缀
        patch_df <- patch_df %>%
          rename_with(~ paste0("patch_", .), -c(xcor, ycor))
        
        # 连接 patch 信息
        df <- df %>%
          left_join(patch_df, by = c("xcor", "ycor"))
        
        # 打印检查：未匹配到 patch 的 pub 数量
        unmatched <- df %>% filter(is.na(patch_patchrent)) %>% nrow()
        message(paste(basename(file), ": 有", unmatched, "个 pub 没有匹配到 patch"))
        
      } else {
        message(paste("Patch 文件缺少 xcor 或 ycor:", patch_file))
      }
    } else {
      message(paste("未找到或找到多个 patch 文件:", run_name))
    }
    
    # 回归模型
    model <- lm(age ~ spend + nearby_pubs + rating + pubrent, data = df)
    r2 <- summary(model)$r.squared
    
    # 保存结果
    r2_results <- rbind(r2_results, data.frame(file = file, r_squared = r2))
    
    # 输出合并后的文件
    out_file <- gsub("\\.csv$", "_merged.csv", file)
    write_csv(df, out_file)
    
  } else {
    message(paste("文件缺少必要列:", file))
  }
}

# 4. 排序并排名
r2_results <- r2_results %>%
  arrange(desc(r_squared)) %>%
  mutate(rank = row_number())

# 5. 输出
print("所有文件 R² 排名：")
print(r2_results)

best_file <- r2_results %>% slice(1)
print("R² 最高的文件：")
print(best_file)
