# stats-stud

Comparative Statistical Analysis of a Public Student Dataset and a CCSICT Replicated Dataset (R).

## Folder layout

- `data/public/` public dataset (input + cleaned copies)
- `data/ccsict/` CCSICT survey, score sheet, raw merged dataset, and analysis dataset
- `scripts/public/` public-dataset analysis scripts
- `scripts/ccsict/` CCSICT study scripts
- `scripts/00_setup.R` shared setup and utilities
- `outputs/public/` public analysis plots and tables
- `outputs/ccsict/` CCSICT analysis plots and tables
- `paper/` IMRAD paper draft and references
- `notes/` guide and project notes

## Run order

1. `scripts/public/01_public_clean_eda.R`
2. `scripts/public/02_public_tests.R`
3. `scripts/public/05_stat_guidance.R`
4. `scripts/public/06_outlier_analysis.R`
5. `scripts/public/07_hypothesis_testing.R`
6. `scripts/ccsict/00_build_ccsict_dataset_raw.R`
7. `scripts/ccsict/01_build_ccsict_dataset.R`
8. `scripts/ccsict/02_build_ccsict_dataset_clean.R`
9. `scripts/ccsict/03_ccsict_clean_eda.R`
10. `scripts/ccsict/04_ccsict_tests_compare.R`

## Data sources

- Public: `data/public/StudentsPerformance.csv`
- CCSICT survey: `data/ccsict/ccsict_survey.csv`
- CCSICT scores: `data/ccsict/program_scores.csv`
- CCSICT raw merged dataset: `data/ccsict/ccsict-dataset-raw.csv`
- CCSICT merged dataset: `data/ccsict/ccsict-dataset.csv`
- CCSICT clean dataset: `data/ccsict/ccsict-dataset-clean.csv`
- Guide: `notes/Study_1_Guide.pdf`
- Repo workflow notes: `notes/repo_steps.md`
