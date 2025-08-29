# 加载必要库
install.packages("shadowtext")
library(ggplot2)
library(shadowtext)  # 用于给文字加白边

# 1. 创建数据框
df <- data.frame(
  year = 2001:2023,
  number_of_pubs = c(
    4835, 4875, 4530, 4455, 4445, 4335, 4295, 4210, 4025, 3890,
    3770, 3770, 3665, 3615, 3675, 3615, 3530, 3540, 3530, 3575,
    3555, 3575, 3535
  ),
  exit_rate = c(
    NA, -8.273009307, 70.76923077, 16.55629139, 2.244668911, 24.74690664, 9.2272203,
    19.79045402, 43.94299287, 33.54037267, 30.84832905, 0, 27.85145889, 13.6425648,
    -16.59751037, 16.32653061, 23.5131397, -2.83286119, 2.824858757, -12.74787535,
    5.594405594, -5.625879044, 11.18881119
  )
)

# 2. 设置比例和偏移量
scale_factor <- 40
offset <- 2200

# 3. 绘图
ggplot(df, aes(x = year)) +
  # 柱状图：酒吧数量
  geom_col(aes(y = number_of_pubs), fill = "grey70") +
  
  # 折线图和点图：退出率（上移）
  geom_line(aes(y = exit_rate * scale_factor + offset), color = "red", size = 1.2, na.rm = TRUE) +
  geom_point(aes(y = exit_rate * scale_factor + offset), color = "red", size = 1.5, na.rm = TRUE) +
  
  # 数值标签（带白色描边）
  geom_shadowtext(
    aes(
      y = exit_rate * scale_factor + offset,
      label = round(exit_rate, 1)
    ),
    bg.color = "white",     # 白边
    color = "red",          # 字体颜色
    size = 3.2,
    vjust = -0.8,
    na.rm = TRUE
  ) +
  # 设置 X 轴更精细


  # 设置双轴
  scale_y_continuous(
    name = "Number of Pubs",
    sec.axis = sec_axis(
      trans = ~ (. - offset) / scale_factor,
      name = "Exit Rate (‰)"
    )
  ) +
  # 设置横轴显示所有年份
  scale_x_continuous(
    breaks = df$year
  ) +
  
  # 样式和网格
  theme_minimal() +
  labs(
    title = "Number of Pubs and Exit Rate (‰) 2001–2023",
    x = "Year"
  ) +
  theme(
    axis.title.y = element_text(color = "black", size = 12),
    axis.title.y.right = element_text(color = "red", size = 12),
    axis.text.y.right = element_text(color = "red"),
    axis.line.y.right = element_line(color = "red"),
    panel.grid.major = element_line(color = "grey80", size = 0.4),
    panel.grid.minor = element_line(color = "grey90", size = 0.2)
  )
