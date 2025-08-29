# Updated closure rate data (‰)
closure_rate <- c(
  4.305705059, 60.54054054, 21.86421174, 8.235294118, 22.53855279,
  20.63106796, 24.78314746, 50.82592122, 22.75769746, 17.80821918,
  0, 59.972106, 25.22255193, -18.26484018, 4.484304933, 30.03003003,
  9.287925697, -12.5, 1.543209877, 10.81916538, -15.625, 21.53846154
)

# Load ggplot2
library(ggplot2)

# Plot histogram with custom x-axis ticks and no mean line
ggplot(data.frame(rate = closure_rate), aes(x = rate)) +
  geom_histogram(binwidth = 5, color = "black", fill = "skyblue") +
  scale_x_continuous(breaks = seq(-20, 65, by = 5)) +
  labs(
    title = "Distribution of Annual Pub Closure Rates in London (2002–2023)",
    x = "Closure Rate (‰)",
    y = "Frequency"
  ) +
  theme_minimal()
