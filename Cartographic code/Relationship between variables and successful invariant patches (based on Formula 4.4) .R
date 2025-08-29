#单次回归加二次回归

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)

# 1. 读取数据
longterm_df <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/patch_analysis/patch/patch_longterm_summary.csv", show_col_types = FALSE)
summary_df <- read_csv("/Users/amyyu/Documents/CASA/dissertation_pubs/regression/patch_summary_run4_a1.001_b0.2_c100_reach5.csv", show_col_types = FALSE)

# 2. 数据合并
merged_df <- longterm_df %>%
  inner_join(summary_df, by = c("pxcor", "pycor")) %>%
  mutate(category_binary = ifelse(category == "successful", 1, 0)) %>%
  filter(!is.na(reachable_pubs), 
         !is.na(reachable_consumers), 
         !is.na(reachable_spending), 
         !is.na(initial_pubs)) %>%
  mutate(across(c(reachable_pubs, reachable_consumers, reachable_spending, initial_pubs), scale))

# ========== 1. 因变量与自变量关系图 ==========
plot_data <- merged_df %>%
  select(category_binary, reachable_pubs, reachable_consumers, reachable_spending, initial_pubs) %>%
  pivot_longer(cols = -category_binary, names_to = "variable", values_to = "value")

ggplot(plot_data, aes(x = value, y = category_binary)) +
  geom_jitter(height = 0.05, alpha = 0.2, color = "gray") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "blue", size = 1) +
  facet_wrap(~variable, scales = "free_x") +
  labs(x = "Variable Value", y = "Probability of Success",
       title = "Relationship Between Variables and Patch Success") +
  theme_minimal()


# ========== 2. 单次项逻辑回归 ==========
model_linear <- glm(category_binary ~ reachable_pubs + reachable_consumers + reachable_spending + initial_pubs,
                    data = merged_df,
                    family = binomial)
summary(model_linear)
vif(model_linear)  # 多重共线性检查

#model_base <- glm(category_binary ~ reachable_pubs + I(reachable_consumers^2) + I(reachable_spending^2) + initial_pubs,data = merged_df, family = binomial)
#summary(model_base)
#vif(model_base)

#reachable_pubs * reachable_consumers
model_interact1 <- glm(category_binary ~ reachable_pubs * reachable_consumers + reachable_spending + initial_pubs,
                       family = binomial, data = merged_df)
summary(model_interact1)
vif(model_interact1)

model_interact2 <- glm(category_binary ~ reachable_pubs * reachable_spending + reachable_consumers + initial_pubs,
                       family = binomial, data = merged_df)
summary(model_interact2)

model_interact3 <- glm(category_binary ~ reachable_consumers * reachable_spending + reachable_pubs + initial_pubs,
                       family = binomial, data = merged_df)
summary(model_interact3)

model_interact4 <- glm(category_binary ~ initial_pubs * reachable_spending + reachable_pubs + reachable_consumers,
                       family = binomial, data = merged_df)
summary(model_interact4)

model_interact_all <- glm(category_binary ~ reachable_pubs * reachable_consumers +
                            reachable_pubs * reachable_spending +
                            reachable_consumers * reachable_spending +
                            initial_pubs,
                          family = binomial, data = merged_df)
summary(model_interact_all)

AIC(model_linear, model_interact1, model_interact2, model_interact3,model_interact4)

anova(model_linear, test = "Chisq")

vif(model_interact_all)


summary(model_interact4)
vif(model_interact4)