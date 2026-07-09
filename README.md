# Supplementary Materials

[![Reproduce analysis](https://github.com/virufy/paper-org-supplement/actions/workflows/reproduce.yml/badge.svg)](https://github.com/virufy/paper-org-supplement/actions/workflows/reproduce.yml)

This repository contains the statistical appendix for the paper:

> **Organizational and leadership support and its relationship to volunteer motivation in a remote-first nonprofit startup**
>
> Future Business Journal. https://doi.org/10.1186/s43093-026-00914-6

---

## Contents

This repository includes the following files for the purposes of transparency and reproducibility:

- `anonymized_dataset.csv`: Anonymized item-level dataset (N = 112 after consent filtering). Contains the consent field, the eight Organizational and Leadership Support survey items (OLS1-OLS8), and the Future Motivation outcome item. See Data Availability below for what is withheld and why.
- `sem_analysis_script.r`: The complete R script used to perform the structural equation modeling (SEM) described in the paper. The code is annotated to explain the analysis steps. Run with `Rscript sem_analysis_script.r` in this directory; it reads `anonymized_dataset.csv`.
- `sem_analysis_output.txt`: The full, unedited console output produced by running `sem_analysis_script.r` on `anonymized_dataset.csv`, including all model fit indices, parameter estimates, and modification indices.

Note on model labeling: the script fits the 8-item specification as its first model and the paper's primary 7-item model (OLS2_Consequence excluded) in the sensitivity section. The paper's primary results (standardized beta = 0.745, R2 = 0.556) appear in the "OLS2 excluded" section of the output; the 8-item specification (beta = 0.734) is reported in the paper as a sensitivity check.

---

## Data Availability

The dataset above contains the item-level responses supporting the paper's quantitative analyses (descriptive statistics, correlations, factor analysis, and SEM).

Demographic variables (gender, age group, ethnicity, country of residence, role, tenure) and open-ended text responses are withheld to protect participant privacy: the study organization is publicly named and the sample is small, so combinations of demographic attributes could be re-identifying. These data were collected under ethical protocols that restrict public distribution.

---

## Citation

If you use these materials, please cite the original article. We recommend citing this repository using the following DOI (concept DOI, resolves to the latest version):
https://doi.org/10.5281/zenodo.16737452

---

© 2025-2026 Amil Khanzada and Takuji Takemoto. For academic use only.
