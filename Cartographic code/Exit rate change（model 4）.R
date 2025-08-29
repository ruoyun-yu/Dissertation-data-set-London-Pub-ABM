library(dplyr)
library(ggplot2)

# 读取数据
EG2 <- read.csv("Gravity Model-Based (Distributed Spending, Model 1).csv", skip = 6)

# 筛选目标参数组合
filtered_data <- EG2 %>%
  filter(constant_a == 1.001,
         reach_distance == 5,
         constant_b == 0.2,
         constant_c == 100) %>%
  mutate(step = X.step.)

# 统计每个 tick（step）的 mean 和 sd（exit 数量）
exit_summary <- filtered_data %>%
  group_by(step) %>%
  summarise(
    mean_exit = mean(tick.exit, na.rm = TRUE),
    sd_exit = sd(tick.exit, na.rm = TRUE),
    n = n()
  )

# 绘图
ggplot(exit_summary, aes(x = step, y = mean_exit)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = mean_exit - sd_exit,
                  ymax = mean_exit + sd_exit),
              alpha = 0.2, fill = "grey70") +
  labs(
    title = "Exit Count per Step (Model 1)",
    subtitle = "Parameters: a = 1.001, b = 0.2, c = 100, reach = 5",
    x = "Step (tick)",
    y = "Mean Exit Count"
  ) +
  theme_minimal()


# 每个 run 每个 tick 的 exit 数
runwise_exit <- filtered_data %>%
  group_by(X.run.number., step) %>%
  summarise(exit_count = mean(tick.exit, na.rm = TRUE), .groups = "drop")

# 绘图：每条线是一个 run 的 exit 数随 tick 变化
ggplot(runwise_exit, aes(x = step, y = exit_count, group = X.run.number.)) +
  geom_line(linewidth = 0.3, alpha = 0.6, color = "black") +
  geom_point(size = 0.6, alpha = 0.6, color = "black") +
  scale_x_continuous(
    breaks = seq(min(runwise_exit$step), max(runwise_exit$step), by = 1),
    limits = c(min(runwise_exit$step), max(runwise_exit$step))
  ) +
  labs(
    title = "Exit Count per Step (Model 1)",
    subtitle = "Parameters: a = 1.001, b = 0.2, c = 100, reach = 5",
    x = "year",
    y = "Exit Count"
  ) +
  theme_minimal()









library(dplyr)
library(ggplot2)

# 读取数据
EG <- read.csv("Random Walk-Based (Single-Spending, Model 4).csv", skip = 6)

# 筛选参数组合，并转换为“年”，仅保留前26年
filtered_data <- EG %>%
  filter(constant_a == 1.005,
         reach_distance == 5,
         constant_b == 0.2,
         constant_c == 80,
         X.step. <= 311) %>%
  mutate(year = X.step. %/% 12 + 1)  # 每年12个tick，从 step=0 开始算第1年

# 每年统计 tick.exit 的均值、标准差
annual_exit_summary <- filtered_data %>%
  group_by(year) %>%
  summarise(
    mean_exit = mean(tick.exit, na.rm = TRUE),
    sd_exit = sd(tick.exit, na.rm = TRUE),
    n = n()
  )

# 绘图：年度平均退出量 + 区间带
ggplot(annual_exit_summary, aes(x = year, y = mean_exit)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(
    ymin = mean_exit - sd_exit,
    ymax = mean_exit + sd_exit
  ), alpha = 0.2, fill = "grey70") +
  labs(
    title = "Annual Exit Count (Model 4)",
    subtitle = "Parameters: a = 1.005, b = 0.2, c = 80, reach = 5 (first 26 years)",
    x = "Year",
    y = "Mean Exit Count"
  ) +
  theme_minimal()

# 每个 run 每年平均 tick.exit
runwise_exit <- filtered_data %>%
  group_by(X.run.number., year) %>%
  summarise(exit_count = mean(tick.exit, na.rm = TRUE), .groups = "drop")

# 绘图：每条线表示一个 run 的退出数量随年变化
ggplot(runwise_exit, aes(x = year, y = exit_count, group = X.run.number.)) +
  geom_line(linewidth = 0.3, alpha = 0.6, color = "black") +
  geom_point(size = 0.6, alpha = 0.6, color = "black") +
  scale_x_continuous(
    breaks = seq(1, 26, by = 1),
    limits = c(1, 26)
  ) +
  labs(
    title = "Exit Count per Year (Model 4)",
    subtitle = "Parameters: a = 1.005, b = 0.2, c = 80, reach = 5",
    x = "Year",
    y = "Exit Count"
  ) +
  theme_minimal()
