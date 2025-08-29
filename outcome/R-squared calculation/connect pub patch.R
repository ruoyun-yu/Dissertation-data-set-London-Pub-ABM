#对数据进行预清洗（选择最后的tick的数据），对数据进行连接将patch数据连接到pub上

library(readr)
library(dplyr)
library(stringr)
library(purrr)

# 1. 获取所有带 pub 的 csv 文件
pub_files <- list.files(pattern = "pub.*\\.csv$", full.names = TRUE)

# 2. 初始化结果存储
cleaned_data_list <- list()

for (pub_file in pub_files) {
  # 2.1 读取 pub 文件
  pub_df <- read_csv(pub_file, show_col_types = FALSE)
  
  # 2.2 新增列：是否在 tick=40 存活
  pub_df <- pub_df %>%
    group_by(pubid) %>%
    mutate(
      alive_at_40 = if_else(any(tick == 39), 1, 0)  # 如果 tick=40 存在记录，标记 1，否则 0
    ) %>%
    ungroup()
  
  # 2.2 清洗 pub 数据
  pub_df <- pub_df %>%
    group_by(pubid) %>%
    slice_max(order_by = tick, n = 1) %>%   # 1) 只保留 tick 最大的行
    ungroup() %>%
    mutate(
      int_xcor = round(xcor),               # 2) 生成整数坐标
      int_ycor = round(ycor)
    )
  
  # 2.3 提取 run 号
  run_id <- str_extract(pub_file, "run\\d+")
  
  if (!is.na(run_id)) {
    # 2.4 找到对应的 patch 文件
    patch_file <- list.files(pattern = paste0("patch.*", run_id, ".*\\.csv$"), full.names = TRUE)
    
    if (length(patch_file) == 1) {
      patch_df <- read_csv(patch_file, show_col_types = FALSE)
      
      # 2.5 连接 patch 数据 (基于整数坐标)
      pub_df <- pub_df %>%
        left_join(
          patch_df,
          by = c("int_xcor" = "pxcor", "int_ycor" = "pycor")
        )
    } else {
      warning(paste("没有找到或找到多个对应的 patch 文件:", run_id))
    }
  } else {
    warning(paste("文件未找到 run ID:", pub_file))
  }
  
  # 2.6 存储清洗后的数据
  cleaned_data_list[[pub_file]] <- pub_df
}

# 3. 选择性导出清洗后的数据
for (file_name in names(cleaned_data_list)) {
  out_file <- str_replace(file_name, "\\.csv$", "_cleaned.csv")
  write_csv(cleaned_data_list[[file_name]], out_file)
}
