# 加载必要的包
if(!require(pacman)) install.packages("pacman")
pacman::p_load(readxl, tidyverse, FactoMineR, factoextra, corrplot, ggpubr, broom)

# 读取Excel数据
raw_data <- read_excel("data//raw//journal.pone.0258761.s010.xlsx")

# 数据预处理
clean_data <- raw_data %>%
  # 转换所有数值列为数字类型
  mutate(across(-c(ID_troup, ID_animal, Vegz, Loc,PHZ), 
                ~ as.numeric(as.character(.x)))) %>%
  # 移除全缺失列
  select(where(~sum(!is.na(.x)) > 0)) %>%
  # 移除包含缺失值的行
  drop_na() %>%
  # 因子化分类变量
  mutate(across(c(Loc, Vegz, ID_animal), as.factor))

# 描述性统计
desc_stats <- clean_data %>%
  group_by(Loc) %>%
  summarise(across(where(is.numeric),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))

# 统计检验（ANOVA with Tukey HSD）
perform_anova <- function(data, variable) {
  formula <- as.formula(paste(variable, "~ Loc"))
  aov_result <- aov(formula, data = data)
  tukey <- TukeyHSD(aov_result)
  return(list(anova = tidy(aov_result), posthoc = tidy(tukey)))
}

# 对所有数值变量执行ANOVA
numeric_vars <- names(select(clean_data, where(is.numeric)))
anova_results <- map(numeric_vars, ~perform_anova(clean_data, .x)) 
names(anova_results) <- numeric_vars

# 相关性分析
cor_matrix <- cor(select(clean_data, where(is.numeric)), use = "complete.obs")

# PCA分析
pca_res <- clean_data %>%
  select(where(is.numeric)) %>%
  scale() %>%
  PCA(graph = FALSE)

# 可视化部分
## 1. 箱线图矩阵
var_selection <- c("BW", "HW", "HL", "BD") # 选择关键变量
box_plots <- map(var_selection, ~{
  ggplot(clean_data, aes_string(x = "Loc", y = .x, fill = "Loc")) +
    geom_boxplot() +
    labs(title = paste(.x, "Distribution by Region"),
         y = "Measurement (cm)") +
    theme_minimal()
})
print(box_plots)
## 2. 相关矩阵热图
cor_plot <- corrplot(cor_matrix, method = "circle", type = "upper",
                     tl.col = "black", tl.srt = 45)
## 3. PCA可视化
pca_var <- fviz_pca_var(pca_res, col.var = "contrib",
                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                        repel = TRUE)
print(pca_var)
pca_ind <- fviz_pca_ind(pca_res, geom = "point", 
                        col.ind = clean_data$Loc, palette = "jco",
                        addEllipses = TRUE, legend.title = "Region")
print(pca_ind)
# 结果保存
## 保存统计结果
write_csv(desc_stats, "descriptive_statistics.csv")
map(names(anova_results), ~{
  write_csv(anova_results[[.x]]$anova, paste0("ANOVA_", .x, ".csv"))
  write_csv(anova_results[[.x]]$posthoc, paste0("PostHoc_", .x, ".csv"))
})

## 保存图形
##ggsave("correlation_matrix.png", plot = cor_plot, 
##       width = 12, height = 10, dpi = 300)
ggsave("pca_analysis.png", plot = grid.arrange(pca_var, pca_ind, ncol = 2), 
       width = 16, height = 8)
map2(box_plots, var_selection, ~ggsave(paste0("Boxplot_", .y, ".png"), 
                                       plot = .x, width = 8, height = 6))

# 结果输出
## 显示PCA方差解释
fviz_eig(pca_res, addlabels = TRUE, 
         main = "PCA - Variance Explained",
         xlab = "Principal Components")