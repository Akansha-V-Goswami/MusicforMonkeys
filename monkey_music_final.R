
#mannwhitney test and bonferroni correction for during and after slow/fast music

setwd("C:/Users/13013/Downloads")
library(readxl)

install.packages(dplyr)
library(dplyr)
install.packages("agricolae")  
install.packages("PMCMRplus")  


library(agricolae)
library(PMCMRplus)


library(readxl)
data <- read_excel("C:/Users/13013/Downloads/Monkey data/files for test/GHLTs/GHLTSAF.xlsx")



#View(data)


library(tidyr)

data_long <- data %>%
  pivot_longer(cols = -Treatment,
               names_to ="Behavior",
               values_to = "Count")

head(data_long)  
#View(data_long)

names <- unique(data_long$Behavior)
results <- list()


alpha <- 0.05 
num_comparisons <- length(names)
adjusted_alpha <- alpha / num_comparisons
print(adjusted_alpha)

results <- list()

for (name in names) {
  data_name <- data_long %>% filter(Behavior == name)
  
  result <- wilcox.test(Count ~ Treatment, data = data_name, exact = FALSE, correct = TRUE)
  
  # Extract the p-value from the result
  p_value <- result$p.value
  
  # Adjust p-value using Bonferroni correction
  adjusted_p_value <- p.adjust(p_value, method = "bonferroni", n = num_comparisons)
  
  results[[name]] <- list(
    name = name, 
    statistic = result$statistic, 
    p.value = p_value,
    adjusted_p.value = adjusted_p_value
  )
}

results_df <- bind_rows(results)

print(results_df)

View(results_df)


#mannwhitney test for OOS


library(readxl)
data <- read_excel("C:/Users/13013/Downloads/Monkey data/files for test/GHLTs/GHLTSAF_OOS.xlsx")



#View(data)


library(tidyr)

data_long <- data %>%
  pivot_longer(cols = -Treatment,
               names_to ="Behavior",
               values_to = "Count")

head(data_long)  
#View(data_long)


result <-  wilcox.test(Count ~ Treatment, data = data_long, exact = FALSE, correct = TRUE)
print(result)

##########Find average and stdev of each behavior per trial


library(dplyr)
library(readxl)
data <- read_excel("C:/Users/13013/Downloads/Monkey data/files for test/GHLTs/GHLTSAF_OOS.xlsx",  col_names = TRUE)
#View(data)


avg_counts <-data %>%
  group_by(data$Treatment) %>%
  summarise(
    across(-Treatment, list(mean = mean, sd = sd)),
  )

print(avg_counts)




