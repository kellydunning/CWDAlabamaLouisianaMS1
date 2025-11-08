Title:
CWD Hunter Analysis – Code and Data Supplement

Overview
This supplement contains the R code and data used to analyze hunter responses to the emergence of chronic wasting disease (CWD) in Alabama and Louisiana. The script reproduces all data processing, variable construction, and statistical models reported in the manuscript, including:

Binary logistic regression models of license intention

Ordinal logistic regression models of comfort hunting in CWD areas

Ordinal logistic regression models of concern about the spread of CWD

Visualizations of CWD knowledge scores by state

Files

CWD_hunter_analysis.R
R script containing all steps from data import through final models and figures, with comments.

CWD_CLEAN (2).xlsx
Cleaned survey dataset used in the analyses. Each row represents one hunter; columns include state, knowledge items, belief items, comfort and concern outcomes, and license intention.

(Adjust the filenames above to match exactly what you’re submitting.)

Software and Packages

Analyses were conducted in R (version X.X.X). The script uses the following packages:

readxl – import Excel data

dplyr – data wrangling and recoding

ggplot2 – visualization

MASS – ordinal logistic regression (polr())

To install packages (first run only):

install.packages(c("readxl", "dplyr", "ggplot2", "MASS"))


How to Run the Script

Place the script and the data file in the same directory.

Open CWD_hunter_analysis.R in RStudio.

Edit the path in the read_excel() line if necessary so it correctly points to CWD_CLEAN (2).xlsx.

Run the script from top to bottom (e.g., Code → Run Region → Run All in RStudio).

The script will:

Recode knowledge items into a knowledge score (0–100%) and a binary Knowledgeable_num variable (above vs. at/below median score).

Recode Likert belief items (I believe_1–I believe_4) into numeric scales (1–6).

Create binary license_next_yr (Yes / lifetime vs. No / Not sure).

Create ordinal factors for comfort (I am comfortable_1) and concern (I am concerned_2).

Subset the data by state (AL, LA).

Fit and summarize:

Binary logistic models for license intention (Alabama and Louisiana).

Ordinal logistic models for comfort (Alabama and Louisiana).

Ordinal logistic models for concern (Alabama and Louisiana).

Produce a violin + boxplot figure of knowledge_score by State.

Model outputs (coefficients, odds ratios, p-values) correspond to the results and tables reported in the manuscript.

Reproducibility Notes

Observations with missing data on any variables used in a given model are excluded via listwise deletion (handled directly by glm() and polr()).

The median split for the knowledge index is computed within the script; the exact median value is printed to the console.

No random processes (e.g., resampling, simulation) are used, so results are deterministic given the input dataset and R version.
