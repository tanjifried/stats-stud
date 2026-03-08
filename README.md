# stats-stud

Comparative Statistical Analysis of a Public Student Dataset and a CCSICT Replicated Dataset (R).

## Folder layout

- `data/public/` public dataset (input + cleaned copies)
- `data/ccsict/` CCSICT survey exports (input + cleaned copies)
- `scripts/` R scripts (run in order)
- `outputs/figures/` plots for the paper
- `outputs/tables/` descriptive stats + hypothesis test tables
- `paper/` IMRAD paper draft and references
- `notes/` guide and project notes

## Run order

1. `scripts/00_setup.R`
2. `scripts/01_public_clean_eda.R`
3. `scripts/02_public_tests.R`
4. (After CCSICT data collection) `scripts/03_ccsict_clean_eda.R`
5. `scripts/04_ccsict_tests_compare.R`

## Data sources

- Public: `data/public/StudentsPerformance.csv`
- Guide: `notes/Study_1_Guide.pdf`
