#library imported
library(ggplot2)
#loading the dataset
company_data <- read.csv("D:/data/final_data.csv")
#View the dataset
View(company_data)
#Structure of the dataset
str(company_data)
#Finding null values
colSums(is.na(company_data))
# Summary statistics
summary(company_data)
# Clean Level from Involvement column 
company_data$Level_Clean <- ifelse(
  grepl("Entry", company_data$Involvement, ignore.case = TRUE),
  "Entry-level",
  "Mid/Senior")
# Creating contingency table
ml_table <- table(company_data$Level_Clean, company_data$ML)
ml_table
# Converting the contigence table to data frame
ct_df <- as.data.frame(ml_table)
colnames(ct_df) <- c("Level", "ML", "Count")

# Converting ML labels
ct_df$ML <- factor(ct_df$ML, levels = c(0,1),
                   labels = c("ML Not Required", "ML Required"))

# Heatmap plot
p1 <-ggplot(ct_df, aes(x = ML, y = Level, fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Contingency Table: ML Requirement by Position Level",
    x = "ML Requirement",
    y = "Position Level",
    fill = "Count") +
  theme_minimal()
ggsave("D:/data/heatmap_ml_table.png", p1, width=6, height=4)

# Calculating proportions
company_data$ML_Factor <- factor(company_data$ML, levels = c(0,1),
                                 labels = c("ML Not Required", "ML Required"))
prop_df <- as.data.frame(prop.table(ml_table, margin = 1))
colnames(prop_df) <- c("Level", "ML", "Proportion")
# Bar Plot
p2 <-ggplot(prop_df, aes(x = Level, y = Proportion, fill = ML)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Job Postings Requiring ML by Experience Level",
       x = "Position Level",
       y = "Proportion of Postings",
       fill = "ML Requirement") +
  theme_minimal()
ggsave("D:/data/prop_bar_ml.png", p2, width=6, height=4)