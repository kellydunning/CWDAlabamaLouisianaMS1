############################################################
# CWD Hunter Analysis – Reproducible R Code (Journal Script)
############################################################

## 0. Setup ------------------------------------------------
## (Uncomment install.packages(...) the first time you run this script)

# install.packages("readxl")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("MASS")

library(readxl)
library(dplyr)
library(ggplot2)
library(MASS)


## 1. Import data ------------------------------------------
## Replace the path below with the actual path to your Excel file

CWD_CLEAN_2_ <- read_excel("Downloads/CWD_CLEAN (2).xlsx")
# View(CWD_CLEAN_2_)  # optional sanity check


## 2. License intention outcome (binary) -------------------
## license_next_yr = 1 if Yes or lifetime license, 0 if No / Not sure

CWD_CLEAN_2_ <- CWD_CLEAN_2_ %>%
  mutate(
    license_next_yr = if_else(
      `license next yr` %in% c("Yes", "I have a lifetime hunting license"),
      1, 0,
      missing = NA_real_
    )
  )

# Quick check
table(CWD_CLEAN_2_$license_next_yr, useNA = "ifany")


## 3. Recode belief Likert items (1–6) ---------------------
## For: I believe_1, I believe_2, I believe_3, I believe_4

recode_likert <- function(x) {
  map <- c(
    "Strongly disagree"          = 1,
    "Disagree"                   = 2,
    "Somewhat disagree"          = 3,
    "Neither agree nor disagree" = 4,
    "Somewhat agree"             = 5,
    "Strongly agree"             = 6
  )
  unname(as.numeric(map[x]))
}

CWD_CLEAN_2_ <- CWD_CLEAN_2_ %>%
  mutate(
    I_believe_1_num = recode_likert(`I believe_1`),
    I_believe_2_num = recode_likert(`I believe_2`),
    I_believe_3_num = recode_likert(`I believe_3`),
    I_believe_4_num = recode_likert(`I believe_4`)
  )


## 4. Knowledge index and Knowledgeable_num ----------------
## Score 6 knowledge items as 1 = correct, 0 = incorrect / Not sure
## Items:
##  - CWD wild           (TRUE is correct)
##  - CWD captive        (FALSE is correct)
##  - CWD test cost      (TRUE is correct)
##  - CWD all border     (FALSE is correct)
##  - Positive county    ("Lauderdale" is correct)
##  - Dispose carcass    ("Buried" is correct)

CWD_CLEAN_2_ <- CWD_CLEAN_2_ %>%
  mutate(
    correct_wild     = if_else(`CWD wild` %in% c(TRUE, "TRUE"),   1, 0, missing = NA_real_),
    correct_captive  = if_else(`CWD captive` %in% c(FALSE, "FALSE"), 1, 0, missing = NA_real_),
    correct_testcost = if_else(`CWD test cost` %in% c(TRUE, "TRUE"), 1, 0, missing = NA_real_),
    correct_border   = if_else(`CWD all border state` %in% c(FALSE, "FALSE"), 1, 0, missing = NA_real_),
    correct_positive = if_else(`Positive county` == "Lauderdale", 1, 0, missing = NA_real_),
    correct_dispose  = if_else(`Dispose carcass` == "Buried",     1, 0, missing = NA_real_)
  )

## Percent correct across the six items (0–100)
CWD_CLEAN_2_ <- CWD_CLEAN_2_ %>%
  mutate(
    knowledge_score = rowMeans(
      cbind(correct_wild, correct_captive, correct_testcost,
            correct_border, correct_positive, correct_dispose),
      na.rm = TRUE
    ) * 100
  )

## Median split for Knowledgeable_num (0 = at/below median, 1 = above median)
median_knowledge <- median(CWD_CLEAN_2_$knowledge_score, na.rm = TRUE)
median_knowledge

CWD_CLEAN_2_ <- CWD_CLEAN_2_ %>%
  mutate(
    Knowledgeable_num = if_else(
      !is.na(knowledge_score) & knowledge_score > median_knowledge,
      1, 0,
      missing = NA_real_
    )
  )

# Check distribution of knowledge_score and Knowledgeable_num
summary(CWD_CLEAN_2_$knowledge_score)
table(CWD_CLEAN_2_$Knowledgeable_num, useNA = "ifany")


## 5. State subsets: Alabama (AL) and Louisiana (LA) -------

AL <- CWD_CLEAN_2_ %>% filter(State == "AL")
LA <- CWD_CLEAN_2_ %>% filter(State == "LA")

nrow(AL)  # total Alabama respondents
nrow(LA)  # total Louisiana respondents


## 6. Logistic regression: license intention ----------------
## Outcome: license_next_yr (0/1)
## Predictors: Knowledgeable_num + I_believe_1–4_num

### Alabama
model_AL_license <- glm(
  license_next_yr ~ Knowledgeable_num +
    I_believe_1_num + I_believe_2_num +
    I_believe_3_num + I_believe_4_num,
  data   = AL,
  family = binomial
)

summary(model_AL_license)
exp(coef(model_AL_license))   # odds ratios
nobs(model_AL_license)        # observations used

### Louisiana
model_LA_license <- glm(
  license_next_yr ~ Knowledgeable_num +
    I_believe_1_num + I_believe_2_num +
    I_believe_3_num + I_believe_4_num,
  data   = LA,
  family = binomial
)

summary(model_LA_license)
exp(coef(model_LA_license))
nobs(model_LA_license)


## 7. Ordinal outcome: Comfort with hunting in CWD regions --
## Question: "I am comfortable_1"
## Scale: Extremely uncomfortable → Extremely comfortable (5 levels)

CWD_CLEAN_2_ <- CWD_CLEAN_2_ %>%
  mutate(
    Comfortable_1_ord = factor(
      `I am comfortable_1`,
      ordered = TRUE,
      levels = c(
        "Extremely uncomfortable",
        "Somewhat uncomfortable",
        "Neither comfortable nor uncomfortable",
        "Somewhat comfortable",
        "Extremely comfortable"
      )
    )
  )

AL_comf <- CWD_CLEAN_2_ %>% filter(State == "AL")
LA_comf <- CWD_CLEAN_2_ %>% filter(State == "LA")

### Alabama comfort model
model_AL_comfort <- polr(
  Comfortable_1_ord ~ Knowledgeable_num +
    I_believe_1_num + I_believe_2_num +
    I_believe_3_num + I_believe_4_num,
  data = AL_comf,
  Hess = TRUE
)

summary(model_AL_comfort)

ctable_AL_comf <- coef(summary(model_AL_comfort))
p_AL_comf <- 2 * pnorm(abs(ctable_AL_comf[, "t value"]), lower.tail = FALSE)
ctable_AL_comf <- cbind(ctable_AL_comf, "p value" = p_AL_comf)
ctable_AL_comf

exp(coef(model_AL_comfort))   # odds ratios

### Louisiana comfort model
model_LA_comfort <- polr(
  Comfortable_1_ord ~ Knowledgeable_num +
    I_believe_1_num + I_believe_2_num +
    I_believe_3_num + I_believe_4_num,
  data = LA_comf,
  Hess = TRUE
)

summary(model_LA_comfort)

ctable_LA_comf <- coef(summary(model_LA_comfort))
p_LA_comf <- 2 * pnorm(abs(ctable_LA_comf[, "t value"]), lower.tail = FALSE)
ctable_LA_comf <- cbind(ctable_LA_comf, "p value" = p_LA_comf)
ctable_LA_comf

exp(coef(model_LA_comfort))


## 8. Ordinal outcome: Concern about spread of CWD ----------
## Question: "I am concerned_2"
## Scale: Strongly disagree → Strongly agree (5 levels)

CWD_CLEAN_2_ <- CWD_CLEAN_2_ %>%
  mutate(
    Concerned_2_ord = factor(
      `I am concerned_2`,
      ordered = TRUE,
      levels = c(
        "Strongly disagree",
        "Somewhat disagree",
        "Neither agree nor disagree",
        "Somewhat agree",
        "Strongly agree"
      )
    )
  )

AL_conc <- CWD_CLEAN_2_ %>% filter(State == "AL")
LA_conc <- CWD_CLEAN_2_ %>% filter(State == "LA")

### Alabama concern model
model_AL_concern <- polr(
  Concerned_2_ord ~ Knowledgeable_num +
    I_believe_1_num + I_believe_2_num +
    I_believe_3_num + I_believe_4_num,
  data = AL_conc,
  Hess = TRUE
)

summary(model_AL_concern)

ctable_AL_conc <- coef(summary(model_AL_concern))
p_AL_conc <- 2 * pnorm(abs(ctable_AL_conc[, "t value"]), lower.tail = FALSE)
ctable_AL_conc <- cbind(ctable_AL_conc, "p value" = p_AL_conc)
ctable_AL_conc

exp(coef(model_AL_concern))

### Louisiana concern model
model_LA_concern <- polr(
  Concerned_2_ord ~ Knowledgeable_num +
    I_believe_1_num + I_believe_2_num +
    I_believe_3_num + I_believe_4_num,
  data = LA_conc,
  Hess = TRUE
)

summary(model_LA_concern)

ctable_LA_conc <- coef(summary(model_LA_concern))
p_LA_conc <- 2 * pnorm(abs(ctable_LA_conc[, "t value"]), lower.tail = FALSE)
ctable_LA_conc <- cbind(ctable_LA_conc, "p value" = p_LA_conc)
ctable_LA_conc

exp(coef(model_LA_concern))


## 9. Knowledge visualization by state ----------------------
## Violin + boxplot of knowledge_score by State (AL vs LA)

plot_data <- CWD_CLEAN_2_ %>%
  filter(!is.na(knowledge_score), State %in% c("AL", "LA"))

ggplot(plot_data, aes(x = State, y = knowledge_score, fill = State)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.4) +
  labs(
    x = "State",
    y = "CWD knowledge score (% correct)",
    title = "CWD knowledge distribution by state"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
