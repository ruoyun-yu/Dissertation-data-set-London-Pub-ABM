# 平均线加区间
# 筛选参数组合
EG <- read.csv("Random Walk-Based (Single-Spending, Model 4).csv", skip = 6)

filtered_data <- EG %>%
  filter(constant_a == 1.005,
         reach_distance == 5,
         constant_b == 0.2,
         constant_c == 80)
filtered_data <- filtered_data %>%
  mutate(year = X.step. %/% 12 + 1)  # step 从 0 开始，所以 +1
annual_exit_rate <- filtered_data %>%
  group_by(year) %>%
  summarise(mean_exit_rate = mean(exit.rate, na.rm = TRUE))
ggplot(annual_exit_rate, aes(x = year, y = mean_exit_rate)) +
  geom_line(size = 0.7) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(min(annual_exit_rate$year), max(annual_exit_rate$year), by = 1)) +
  labs(
    title = "Annual Exit Rate Over Time",
    subtitle = "Parameter: a=1.005, b=0.2, c=80, reach=5",
    x = "Year (simulated)",
    y = "Mean Exit Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

nrow(filtered_data)
filtered_data %>%
  distinct(X.run.number.) %>%
  nrow()

# 平均线加区间
EG2 <- read.csv("Gravity Model-Based (Distributed Spending, Model 1).csv", skip = 6)
# 筛选目标参数组合的数据
filtered_data <- EG2 %>%
  filter(constant_a == 1.001,
         reach_distance == 5,
         constant_b == 0.2,
         constant_c == 100)
# 将 step 转为年（+1 是为了让 step=0 对应第1年）
filtered_data <- filtered_data %>%
  mutate(year = X.step. )
annual_exit_rate <- filtered_data %>%
  group_by(year) %>%
  summarise(
    mean_exit_rate = mean(exit.rate, na.rm = TRUE),
    sd_exit_rate = sd(exit.rate, na.rm = TRUE),
    n = n()
  )
ggplot(annual_exit_rate, aes(x = year, y = mean_exit_rate)) +
  geom_line(color = "black", linewidth = 0.7) +
  geom_point(size = 1.5) +
  geom_ribbon(aes(ymin = mean_exit_rate - sd_exit_rate,
                  ymax = mean_exit_rate + sd_exit_rate),
              alpha = 0.2, fill = "grey70") +
  labs(
    title = "Annual Exit Rate (Model 1)",
    subtitle = "Parameters: a = 1.001, b = 0.2, c = 100, reach = 5",
    x = "Year (step / 12)",
    y = "Mean Exit Rate"
  ) +
  theme_minimal()



# 分开线
library(dplyr)
library(ggplot2)

# 添加年变量
filtered_data <- EG %>%
  filter(constant_a == 1.005,
         reach_distance == 5,
         constant_b == 0.2,
         constant_c == 80) %>%
  mutate(year = X.step. %/% 12 + 1)

# 每个 run 每年取平均退出率
runwise_exit_rate <- filtered_data %>%
  group_by(X.run.number., year) %>%
  summarise(exit_rate = mean(exit.rate, na.rm = TRUE), .groups = "drop")

# 绘图：每个 run 一条线
ggplot(runwise_exit_rate, aes(x = year, y = exit_rate, group = X.run.number.)) +
  geom_line(linewidth = 0.3, alpha = 0.6, color = "black") +
  geom_point(size = 0.6, alpha = 0.6, color = "black") +
  scale_x_continuous(breaks = seq(min(runwise_exit_rate$year), max(runwise_exit_rate$year), by = 1)) +
  labs(
    title = "Exit Rate per Year (Model 4)",
    subtitle = "Parameter: a = 1.005, b = 0.2, c = 80, reach = 5",
    x = "Year",
    y = "Exit Rate"
  ) +
  theme_minimal()



# 分开线
library(dplyr)
library(ggplot2)

EG2 <- read.csv("Gravity Model-Based (Distributed Spending, Model 1).csv", skip = 6)

# 筛选目标参数组合
filtered_data <- EG2 %>%
  filter(constant_a == 1.001,
         reach_distance == 5,
         constant_b == 0.2,
         constant_c == 100) %>%
  mutate(year = X.step.)

# 每个 run 每年退出率
runwise_exit_rate <- filtered_data %>%
  group_by(X.run.number., year) %>%
  summarise(exit_rate = mean(exit.rate, na.rm = TRUE), .groups = "drop")

# 绘图：每条线对应一个 run
ggplot(runwise_exit_rate, aes(x = year, y = exit_rate, group = X.run.number.)) +
  geom_line(linewidth = 0.3, alpha = 0.6, color = "black") +
  geom_point(size = 0.6, alpha = 0.6, color = "black") +
  scale_x_continuous(
    breaks = seq(0, 26, by = 1),
    limits = c(0, 26)
  ) +
  labs(
    title = "Exit Rate per Year (Model 1)",
    subtitle = "Parameters: a = 1.001, b = 0.2, c = 100, reach = 5",
    x = "Year",
    y = "Exit Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11)
  )
