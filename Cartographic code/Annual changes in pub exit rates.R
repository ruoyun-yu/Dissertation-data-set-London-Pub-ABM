library(readr)
library(ggplot2)
library(dplyr)
library(viridis)
library(grid)   # unit()

# ---------------------------
# 通用绘图函数（满足：x 轴整数网格；去掉上/右轴线）
# ---------------------------
make_exit_rate_plot <- function(df, model_title) {
  df <- df %>% dplyr::filter(step_year >= 1)
  ggplot(
    df,
    aes(
      x = step_year,
      y = `exit-rate`,
      group = group,
      colour = group
    )
  ) +
    geom_line(linewidth = 0.3, alpha = 0.7, lineend = "round") +
    geom_point(size = 0.6, alpha = 0.6) +
    # x 轴从 1 年开始；只给出整数刻度 -> 只会生成整数间隔的“主网格”
    scale_x_continuous(
      breaks = seq(1, max(df$step_year, na.rm = TRUE), by = 1),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(expand = c(0.02, 0.02)) +
    scale_colour_viridis_d(option = "D", end = 0.95, name = "Parameter set") +
    labs(
      title = model_title,
      x = "Years",
      y = "Exit Rate"
    ) +
    theme_classic(base_size = 14) +  # 仅有下/左轴线、无面板边框
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14),
      
      # 轴线：保留下/左；显式关闭上/右
      axis.line = element_line(linewidth = 0.6),
      axis.line.x.top = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks = element_line(linewidth = 0.6),
      
      legend.position = "right",
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      
      # 网格：只开“主网格”，且更细；关闭所有次网格（避免 0.5 等）
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.2),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor   = element_blank()
    ) +
    guides(colour = guide_legend(ncol = 1))   # 图例单列
}

# ============================
# Model 1 - Gravity (Distributed Spending)
# ============================
df1 <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/gis_data/spatialdata/sample/KStest/汇总/Gravity Model-Based (Distributed Spending, Model 1).csv", skip = 6) %>%
  filter(`[step]` %% 1 == 0) %>%
  mutate(
    step_year = (`[step]` %/% 1),
    group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")
  )
p1 <- make_exit_rate_plot(df1, "Exit Rate by Year — Model 1 (Gravity, Distributed Spending)")
print(p1)

# ============================
# Model 2 - Gravity (Single Spending)
# ============================
df2 <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/gis_data/spatialdata/sample/KStest/汇总/Gravity Model-Based (Single-Spending, Model 2).csv", skip = 6) %>%
  filter(`[step]` %% 12 == 1) %>%
  mutate(
    step_year = (`[step]` %/% 12),
    group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")
  )
p2 <- make_exit_rate_plot(df2, "Exit Rate by Year — Model 2 (Gravity, Single Spending)")
print(p2)

# ============================
# Model 3 - Intervening Opportunities
# ============================
df3 <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/gis_data/spatialdata/sample/KStest/汇总/Intervening Opportunities Model-Based (Single-Spending, Model 3).csv", skip = 6) %>%
  filter(`[step]` %% 12 == 1) %>%
  mutate(
    step_year = (`[step]` %/% 12),
    group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")
  )
p3 <- make_exit_rate_plot(df3, "Exit Rate by Year — Model 3 (Intervening Opportunities)")
print(p3)

# ============================
# Model 4 - Random Walk
# ============================
df4 <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/gis_data/spatialdata/sample/KStest/汇总/Random Walk-Based (Single-Spending, Model 4).csv", skip = 6) %>%
  filter(`[step]` %% 12 == 1) %>%
  mutate(
    step_year = (`[step]` %/% 12),
    group = paste(constant_a, reach_distance, constant_b, constant_c, sep = "-")
  )
p4 <- make_exit_rate_plot(df4, "Exit Rate by Year — Model 4 (Random Walk)")
print(p4)

# ---- 高分辨率导出（保持全页宽）----
# ggsave("model1_exit_rate.png", p1, width = 12, height = 6, dpi = 400)
# ggsave("model2_exit_rate.png", p2, width = 12, height = 6, dpi = 400)
# ggsave("model3_exit_rate.png", p3, width = 12, height = 6, dpi = 400)
# ggsave("model4_exit_rate.png", p4, width = 12, height = 6, dpi = 400)



make_exit_rate_plot <- function(df, model_title, line_width = 0.3, point_size = 0.6) {
  df <- df %>% dplyr::filter(step_year >= 1 & step_year <= 24)
  ggplot(
    df,
    aes(x = step_year, y = `exit-rate`, group = group, colour = group)
  ) +
    geom_line(linewidth = line_width, alpha = 0.7, lineend = "round") +
    geom_point(size = point_size, alpha = 0.6) +
    scale_x_continuous(
      breaks = seq(1, max(df$step_year, na.rm = TRUE), by = 1),
      expand = c(0.01, 0.01)
    ) +
    scale_y_continuous(expand = c(0.02, 0.02)) +
    scale_colour_viridis_d(option = "D", end = 0.95, name = "Parameter set") +
    labs(title = model_title, x = "Years", y = "Exit Rate") +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14),
      axis.line = element_line(linewidth = 0.6),
      axis.line.x.top = element_blank(),
      axis.line.y.right = element_blank(),
      axis.ticks = element_line(linewidth = 0.6),
      legend.position = "right",
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 8),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.2),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor   = element_blank()
    ) +
    guides(colour = guide_legend(ncol = 4))
}


# 先按你现在的代码生成 p1
p1 <- make_exit_rate_plot(df1, "Exit Rate by Year — Model 1 (Gravity, Distributed Spending)")

# 只把 p1 的线和点调细
p1$layers[[1]]$aes_params$linewidth <- 0.08  # 线更细
p1$layers[[2]]$aes_params$size      <- 0.2   # 点更小（可选）

print(p1)
