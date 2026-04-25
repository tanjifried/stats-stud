The **CCSICT replicated dataset** contains 30 records. It includes 8 variables: respondent_id, sex, program, preparation_mode, preparedness_score, preparedness_level, lecture_score, and lab_completed. One column is an identifier, 5 columns contain categorical data, and 2 columns contain numerical data.

| Variable | Mean | Median | Standard Deviation |
| :---: | :---: | :---: | :---: |
| preparedness_score | 3.89 | 3.75 | 0.59 |
| lecture_score | 66.77 | 67 | 14.35 |

***Figure 1. Mean, Median and Standard Deviation of preparedness_score and lecture_score***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/tables/ccsict_numeric_descriptives.csv`

Based on the descriptive statistics, the **preparedness_score** variable has a mean of 3.89, a median of 3.75, and a standard deviation of 0.59, indicating that most students reported moderate to high levels of preparedness with relatively limited spread. The **lecture_score** variable has a mean of 66.77, a median of 67, and a standard deviation of 14.35, showing a wider spread in lecture performance compared with the preparedness scores.

| Variable | IQR | Upper fence | Lower fence | Outliers |
| :---: | :---: | :---: | :---: | :---: |
| preparedness_score | 0.72 | 5.42 | 2.55 | 0 |
| lecture_score | 15.50 | 98.75 | 36.75 | 1 |

***Figure 2. IQR and Outliers of preparedness_score and lecture_score***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/tables/ccsict_outlier_summary.csv`

Based on the computed Interquartile Range (IQR), as well as the lower fence and upper fence values for each column, very few outliers were detected in the dataset. The preparedness_score variable has no outliers, while the lecture_score column has only 1 outlier. This suggests that the CCSICT data is generally compact and consistent. The detected lecture score outlier is still plausible as a valid high-performing observation rather than an obvious data entry error.

***Figure 3. Boxplot Chart of preparedness_score and lecture_score***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/figures/ccsict_box_overall_scores.png`

The boxplot illustrates the distribution of preparedness_score and lecture_score, showing the median, quartiles, range, and potential outliers. The boxes represent the middle 50% of the values, with the line inside each box indicating the median, while the whiskers show the spread of most observations within 1.5 times the interquartile range. The preparedness scores are more tightly clustered, while the lecture scores are more dispersed. A single point above the whisker in the lecture_score boxplot indicates one high outlier.

***Figure 4. Histogram of Preparedness Score***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/figures/ccsict_hist_preparedness_score.png`

As can be seen from Figure 4, most of the students have preparedness scores between approximately 3.5 and 4.5, with a mean of 3.89 and a median of 3.75. The spread of the distribution is relatively small, as shown by the standard deviation of 0.59. This suggests that most respondents reported fairly similar levels of coding preparedness.

***Figure 5. Histogram of Lecture Score***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/figures/ccsict_hist_lecture_score.png`

Figure 5 shows that the lecture scores are more widely distributed than the preparedness scores. The mean lecture score is 66.77, and the median is 67, which are very close, suggesting a fairly balanced center. However, the standard deviation of 14.35 shows that there is more variation in lecture performance across students. The histogram also shows that while many students scored in the 60s to 80s range, some scored much lower and one scored very high.

***Figure 6. Distribution of Sex***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/figures/ccsict_bar_sex.png`

The graph indicates the number of female and male students. The total number of female students is 14, which represents 46.67%, and the total number of male students is 16, which represents 53.33%. This shows that the dataset is fairly balanced in terms of sex distribution.

***Figure 7. Distribution of Preparation Mode***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/figures/ccsict_bar_preparation_mode.png`

The majority of students are in the **with_ai** group. The number of students in the with_ai group is 17 (56.67%), while the number of students in the without_ai group is 13 (43.33%). This indicates that the sample slightly favors students whose preparation involved AI, although both groups are still sufficiently represented for comparison.

***Figure 8. Distribution of Program***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/figures/ccsict_bar_program.png`

The graph shows an even split between the two programs in the dataset. There are 15 students from BSCS and 15 students from BSDSA, with each group representing 50% of the sample. This balanced composition is useful in comparing the CCSICT dataset across programs.

***Figure 9. Scatterplot of Preparedness Score vs Lecture Score***

Figure path: `/home/jade/dev/projects/stats-stud/outputs/ccsict/figures/ccsict_scatter_preparedness_score_vs_lecture_score.png`

Figure 9 shows that there is a moderate positive linear association between preparedness_score and lecture_score. Students with higher preparedness scores also tend to have higher lecture scores. Moreover, the Pearson correlation coefficient is 0.464, showing that the relationship is positive and moderate in strength.

The dataset has 30 data points and 8 variables, with one identifier variable, five categorical variables, and two numeric variables. The descriptive statistics reveal that preparedness scores are relatively concentrated, while lecture scores show greater variation. The histograms and boxplot represent the distributions of the two numeric variables, and the bar charts provide an overview of the key categorical variables such as sex, preparation mode, and program. The scatterplot illustrates a moderate positive relationship between preparedness and lecture performance, indicating that students who are more prepared also tend to perform better in the lecture component.
