### Biostat 203A Final Project ###

## Hsin Ying (Angel) Tseng ##

library(nhanesA)
library(dplyr)
library(tidyverse)
library(stringr)
library(car)


nhanesTables("DEMO", 2017)
nhanesTables("LAB", 2017)
nhanesTables("Q", 2017)

demo = nhanes("DEMO_J")
chole = nhanes("TCHOL_J")
insur = nhanes("HIQ_J")

demo_cleaned = demo %>%
  select(SEQN, RIDRETH3, RIDAGEYR, RIAGENDR)
chole_cleaned = chole %>%
  select(SEQN, LBXTC)

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

insur_clean = insur %>% 
  unite(coverage, c(HIQ031A:HIQ031J, HIQ260), 
        na.rm = TRUE, remove = FALSE) %>%
  select(-c(HIQ031A:HIQ031J, HIQ260)) %>%
  filter(!str_detect(coverage, "_"))

merged = merge(demo_cleaned, insur_clean, by = "SEQN", all = T)
total_merge = merge(merged, chole_cleaned, by = "SEQN", all = T)

cleaned = total_merge %>%
  filter(HIQ011 == 1) %>%
  filter(!is.na(LBXTC)) %>%
  filter(coverage != "") %>%
  select(-c(HIQ031AA, HIQ105, HIQ270, HIQ210)) %>%
  rename(gender = RIAGENDR, age = RIDAGEYR, race = RIDRETH3, 
         total_cholesterol = LBXTC, insurance = HIQ011) %>%
  mutate(gender = as.factor(gender), insurance = as.factor(insurance),
         race = as.factor(race), coverage = as.factor(coverage))

dim(cleaned)
str(cleaned)
structure(cleaned)

mean(cleaned$total_cholesterol)
sd(cleaned$total_cholesterol) / sqrt(nrow(cleaned))

## Factorial ANOVA

# Normality of dependent variable
hist(log(cleaned$total_cholesterol))

# Homogeneity of variances
result = leveneTest(log(total_cholesterol) ~ interaction(race, coverage), 
                    data = cleaned)
result

# 2-Way ANOVA
model = aov(lm(log(total_cholesterol) ~ race*coverage, data = cleaned))
summary(model)

# Post-hoc tests
race_tukey = TukeyHSD(model, "race")
TukeyHSD(model, "coverage")
plot(race_tukey)
