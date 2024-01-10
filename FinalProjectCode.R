### Biostat 203A Final Project ###

## Hsin Ying (Angel) Tseng ##

library(nhanesA)
library(dplyr)
library(tidyverse)
library(stringr)
library(shiny)
library(ggplot2)
library(car)

# Look up code for interested data sets
nhanesTables("DEMO", 2017)
nhanesTables("LAB", 2017)
nhanesTables("Q", 2017)

# Load in data sets
demo = nhanes("DEMO_J")
chole = nhanes("TCHOL_J")
insur = nhanes("HIQ_J")


## Data curation and processing

# Selecting interested variables of ID, race, age, gender, cholesterol
demo_cleaned = demo %>%
  select(SEQN, RIDRETH3, RIDAGEYR, RIAGENDR)
chole_cleaned = chole %>%
  select(SEQN, LBXTC)

# Formatting & labels
insur$HIQ031A[insur$HIQ031A==14] = "Covered by private insurance"
insur$HIQ031A[insur$HIQ031A!="Covered by private insurance"] = NA
insur$HIQ031B[insur$HIQ031B==15] = "Covered by Medicare/Medicaid"
insur$HIQ260[insur$HIQ260==1] = "Covered by Medicare/Medicaid"
insur$HIQ260[insur$HIQ260!="Covered by Medicare/Medicaid"] = NA
insur$HIQ031C[insur$HIQ031C==16] = "Covered by Medi-Gap"
insur$HIQ031D[insur$HIQ031D==17] = "Covered by Medicare/Medicaid"
insur$HIQ031E[insur$HIQ031E==18] = "Covered by other government insurance"
insur$HIQ031F[insur$HIQ031F==19] = "Covered by other government insurance"
insur$HIQ031H[insur$HIQ031H==21] = "Covered by other government insurance"
insur$HIQ031I[insur$HIQ031I==22] = "Covered by other government insurance"
insur$HIQ031J[insur$HIQ031J==23] = "Covered by private insurance"

demo_cleaned$RIDRETH3[demo_cleaned$RIDRETH3==1] = "Mexican American"
demo_cleaned$RIDRETH3[demo_cleaned$RIDRETH3==2] = "Other Races"
demo_cleaned$RIDRETH3[demo_cleaned$RIDRETH3==3] = "Non-Hispanic White"
demo_cleaned$RIDRETH3[demo_cleaned$RIDRETH3==4] = "Non-Hispanic Black"
demo_cleaned$RIDRETH3[demo_cleaned$RIDRETH3==6] = "Non-Hispanic Asian"
demo_cleaned$RIDRETH3[demo_cleaned$RIDRETH3==7] = 
  "Other Races"

# Remove observations based on inclusion criteria
insur_clean = insur %>% 
  unite(coverage, c(HIQ031A:HIQ031J, HIQ260), 
        na.rm = TRUE, remove = FALSE) %>%
  select(-c(HIQ031A:HIQ031J, HIQ260)) %>%
  filter(!str_detect(coverage, "_"))

# Merge data sets
merged = merge(demo_cleaned, insur_clean, by = "SEQN", all = T)
total_merge = merge(merged, chole_cleaned, by = "SEQN", all = T)

# Remove observations based on inclusion criteria
cleaned = total_merge %>%
  filter(HIQ011 == 1) %>%
  filter(!is.na(LBXTC)) %>%
  filter(coverage != "") %>%
  select(-c(HIQ031AA, HIQ105, HIQ270, HIQ210)) %>%
  rename(gender = RIAGENDR, age = RIDAGEYR, race = RIDRETH3, 
         total_cholesterol = LBXTC, insurance = HIQ011) %>%
  mutate(gender = as.factor(gender), insurance = as.factor(insurance),
         race = as.factor(race), coverage = as.factor(coverage))

# Structure of data
dim(cleaned)
str(cleaned)
structure(cleaned)


## Baseline characteristics
# Table 1

# Means and SE of cholesterol for each insurance and race group
options(pillar.sigfig=4)
cleaned %>%
  group_by(coverage, race) %>%
  summarise(mean_chol = mean(total_cholesterol, na.rm = T),
            sd = sd(total_cholesterol)) %>%
  mutate(se = sd / sqrt(n()),
         lower_ci = mean_chol - qt(1 - (0.05 / 2), n() - 1) * se,
         upper_ci = mean_chol + qt(1 - (0.05 / 2), n() - 1) * se)

# Proportion of gender for each group
gender_base = cleaned %>%
  group_by(coverage, race, gender) %>%
  summarize(counts = n()) %>%
  mutate(prop_gen = counts / sum(counts))
print(gender_base, n=30)

# Mean ages and se for each group
cleaned %>%
  group_by(coverage, race) %>%
  summarise(mean_age = mean(age, na.rm = T),
            sd = sd(age)) %>%
  mutate(se = sd / sqrt(n()),
         lower_ci = mean_age - qt(1 - (0.05 / 2), n() - 1) * se,
         upper_ci = mean_age + qt(1 - (0.05 / 2), n() - 1) * se)

## RShiny
# Figure 1: Boxplots as visualizing the data

# Input
ui = fluidPage(
  # Title of app
  titlePanel("Total Cholesterol by Race/Ethnicity and Insurance"),
  selectInput(inputId = "race", 
              label = "Choose race/ethnicity of interest:", 
              choices = levels(cleaned$race)),
  plotOutput("boxplot")
)

# Server
server = function(input, output) {
  
  # Filter data based on input
  filtered = reactive({
    filtered_df = cleaned %>% filter(race == input$race)
    return(filtered_df)
  })
  
  output$boxplot = renderPlot({
    # Plot boxplot of total cholesterol to health insurance
    ggplot(filtered(), aes(x = coverage, y = total_cholesterol, 
                           fill = coverage)) +
      geom_boxplot(color = "blue") +
      labs(title = "Boxplot of Total Cholesterol by Race and Health Insurance",
           x = "Health Insurance Type",
           y = "Total Cholesterol (mg/L)",
           fill = "Health Insurance Type") +
      theme_minimal()
  })
}

# Run RShiny app
shinyApp(ui = ui, server = server)


## Factorial ANOVA
# Table 2

# 2-Way ANOVA
model = aov(total_cholesterol ~ race*coverage, data = cleaned)
summary(model)

# Normality of dependent variable
hist(model$residuals)
qqnorm(model$residuals)

# Homogeneity of variances
result = leveneTest(total_cholesterol ~ interaction(race, coverage), 
                    data = cleaned)
result

# Post-hoc tests

# Change labels for groups

# Run post-hoc test again with updated labels
# Recode factor levels
new_lab = c("M.A.", "N.H.A.", "N.H.B.", "N.H.W.", "O")
new_lab_co = c("M.M", "Gov", "Private")
levels(cleaned$race) = new_lab
levels(cleaned$coverage) = new_lab_co

model_new = aov(total_cholesterol ~ race*coverage, data = cleaned)
race_tukey = TukeyHSD(model_new, "race")
plot(TukeyHSD(model_new, "coverage"))
TukeyHSD(model_new, "coverage")

race_tukey
plot(race_tukey)
