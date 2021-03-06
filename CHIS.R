# Explore the dataset with summary and str
summary(adult)
str(adult)

# Age histogram
ggplot(adult, aes (x = SRAGE_P)) +
  geom_histogram()

# BMI histogram
ggplot(adult, aes (x = BMI_P)) +
  geom_histogram()

# Age colored by BMI, binwidth = 1
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1)

# Keep adults younger than or equal to 84
adult <- adult[adult$SRAGE_P <= 84, ] 

# Keep adults with BMI at least 16 and less than 52
adult <- adult[adult$BMI_P >= 16 & adult$BMI_P < 52, ]

# Relabel the race variable
adult$RACEHPR2 <- factor(adult$RACEHPR2, labels = c("Latino", "Asian", "African American", "White"))

# Relabel the BMI categories variable
adult$RBMI <- factor(adult$RBMI, labels = c("Under-weight", "Normal-weight", "Over-weight", "Obese"))

# The color scale used in the plot
BMI_fill <- scale_fill_brewer("BMI Category", palette = "Reds")

# Theme to fix category display in faceted plot
fix_strips <- theme(strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0.1, size = 14),
                    strip.background = element_blank(),
                    legend.position = "none")

# Histogram, add BMI_fill and customizations
ggplot(adult, aes (x = SRAGE_P, fill= factor(RBMI))) + 
  geom_histogram(binwidth = 1) +
  fix_strips + BMI_fill + facet_grid(RBMI~.) + theme_classic()