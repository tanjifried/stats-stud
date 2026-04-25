1. Research Question

Is there a significant relationship between exam preparedness and lecture performance of the students?

2. Null Hypothesis

There is no significant relationship between exam preparedness and lecture performance of the students.

3. Alternative Hypothesis

There is a significant relationship between exam preparedness and lecture performance of the students.

4. Assumptions

Before conducting the hypothesis test, the following assumptions must be evaluated to ensure validity:

* Independence of observations - each observation must come from a different participant.
* Continuous variables - the variables included in the parametric test should be measured on a continuous scale.
* Normality - both variables should be approximately normally distributed for Pearson correlation.
* Linearity - the relationship between the two variables should be approximately linear.
* No major outlier problem - extreme outliers should not dominate the association.

| Assumption | Test / Method | Result | p | Status |
| ----- | ----- | ----- | :---: | :---: |
| Independence of observations | Study design (one row per unique student) | Structurally guaranteed | - | ✓ Met |
| Continuous variables | Variable inspection | preparedness_score and lecture_score are numeric | - | ✓ Met |
| Normality - Preparedness Score | Anderson-Darling test | A = 0.306 | .544 | ✓ Met |
| Normality - Lecture Score | Anderson-Darling test | A = 0.171 | .924 | ✓ Met |
| Linearity | Scatterplot inspection | Moderate positive linear pattern observed | - | ✓ Met |
| Outlier influence | IQR outlier check | 0 outliers in preparedness_score; 1 outlier in lecture_score | - | ✓ Acceptable |

Table 1. Assumption Verification for the Pearson Correlation Test

The independence of observations was satisfied structurally because each row represented one unique student. Both preparedness_score and lecture_score are continuous numeric variables, which satisfies the scale requirement for Pearson correlation. Normality was assessed using the Anderson-Darling test. Preparedness score produced A = 0.306, p = 0.544, while lecture score produced A = 0.171, p = 0.924; both variables therefore satisfied the normality assumption at alpha = 0.05. Visual inspection of the scatterplot also suggested a moderate positive linear pattern. Outlier assessment showed no outliers in preparedness_score and only one outlier in lecture_score, which was not severe enough to invalidate the analysis. Therefore, Pearson correlation was appropriate as the primary parametric test, while Spearman correlation was retained as the non-parametric counterpart.

5. R output

In order to test the stated hypothesis, a Pearson correlation test was conducted to examine whether a significant relationship exists between preparedness_score and lecture_score among the CCSICT respondents. Because correlation was the focus of the hypothesis, Pearson correlation was used as the parametric test and Spearman rank correlation was used as the non-parametric counterpart. The analysis was performed using R, and the results are presented below:

# === 04_ccsict_tests_compare.R (excerpt) ===

# Load clean data
df <- utils::read.csv(paths$ccsict_clean, stringsAsFactors = FALSE)

# Convert core variables
df$preparedness_score <- as.numeric(df$preparedness_score)
df$lecture_score <- as.numeric(df$lecture_score)

# Assumption check: normality
normality_results <- dplyr::bind_rows(
  normality_to_df(df$lecture_score, "lecture_score"),
  normality_to_df(df$preparedness_score, "preparedness_score")
)

# Parametric test: Pearson correlation
pearson_res <- stats::cor.test(df$preparedness_score, df$lecture_score, method = "pearson")

# Non-parametric counterpart: Spearman correlation
spearman_res <- stats::cor.test(df$preparedness_score, df$lecture_score, method = "spearman", exact = FALSE)

# --- Console output ---
# Pearson's product-moment correlation
# data: df$preparedness_score and df$lecture_score
# t = 2.770, df = 28, p-value = 0.0098
# alternative hypothesis: true correlation is not equal to 0
# sample estimate:
# cor = 0.4638
#
# Spearman's rank correlation rho
# data: df$preparedness_score and df$lecture_score
# S = 2487.076, p-value = 0.0133
# sample estimate:
# rho = 0.4467

6. Interpretation

| Test | Correlation Coefficient | Test Statistic | p-value | Strength | Decision |
| :---: | :---: | :---: | :---: | :---: | :---: |
| Pearson | 0.464 | t = 2.770 | .0098 | Moderate | Reject H0 |
| Spearman | 0.447 | S = 2487.076 | .0133 | Moderate | Reject H0 |

Table 2. Correlation Test Results for Preparedness Score and Lecture Score

The result obtained from the Pearson correlation test showed a correlation coefficient of r = 0.464 with a test statistic of t(28) = 2.770 and a p-value of 0.0098, providing sufficient evidence against the null hypothesis at the 0.05 level of significance. This indicates a moderate positive relationship between preparedness score and lecture score, meaning that students with higher preparedness scores also tended to obtain higher lecture scores. To confirm the result using a non-parametric approach, Spearman rank correlation was also conducted and produced a similar moderate positive association, rho = 0.447, p = 0.0133. The consistency of the Pearson and Spearman results suggests that the observed relationship is stable and not dependent on one specific statistical method.

7. Conclusion

The null hypothesis is rejected. There is sufficient statistical evidence to support the conclusion that exam preparedness is significantly related to lecture performance among the CCSICT students. The parametric Pearson correlation showed a statistically significant moderate positive relationship, r(28) = 0.464, p = 0.0098, and this result was supported by the non-parametric Spearman correlation, rho = 0.447, p = 0.0133. These findings suggest that students who are more prepared for the examination also tend to perform better in the lecture component.
