# ===========================================================================
# SEM Analysis Script: Organizational and Leadership Support Predicting
# Volunteer Motivation in a Global Nonprofit Startup
# Date: 2025-05-08
# Description: This script loads, cleans, and prepares survey data,
#              then fits a single-factor Structural Equation Model (SEM)
#              using the lavaan package. The model tests the relationship
#              between a latent construct of 'Organizational and Leadership
#              Support' and observed 'Future Motivation'.
#
# NOTE FOR REPRODUCIBILITY:
# This script is provided to reproduce the statistical analysis presented
# in the paper. It reads data from 'anonymized_dataset.csv' and prints
# analysis results to the console (stdout). Plotting and file saving
# commands are commented out to ensure a clean console output ('output.txt').
# ===========================================================================

# --- 1. Load and Install Packages ---
# Ensure necessary packages are available, installing them if not.
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,     # For data manipulation
  stringr,   # For string manipulation
  lavaan,    # For SEM analysis
  corrplot,  # For correlation matrix calculation
  psych      # For descriptive statistics and reliability (alpha)
)

# --- 2. Configuration and File Paths ---
# Define input data file name.
data_file <- "anonymized_dataset.csv"

# --- Missing Data Handling Strategy ---
# FIML (Full Information Maximum Likelihood) is used throughout.
# Non-numeric strings are converted to NA during data preparation;
# no imputation is applied. FIML in lavaan handles all missing values
# during model fitting (Enders & Bandalos, 2001; Graham, 2009).
missing_data_strategy <- "fiml"

# --- 3. Helper Function: Clean Column Names ---
# Standardizes column names by removing special characters, spaces, etc.
clean_col_name <- function(col_name) {
  if (!is.character(col_name)) col_name <- as.character(col_name)
  col_name <- gsub("\\[.*?\\]", "", col_name) # Remove text in square brackets
  col_name <- gsub("\\(.*?\\)", "", col_name) # Remove text in parentheses
  col_name <- gsub("\\?", "", col_name)      # Remove question marks
  col_name <- gsub("[^[:alnum:]_]+", "_", col_name) # Replace non-alphanumeric (except underscore) with underscore
  col_name <- gsub("^_|_$", "", col_name)     # Remove leading/trailing underscores
  col_name <- gsub("_+", "_", col_name)       # Replace multiple underscores with single
  col_name <- trimws(col_name)                # Trim leading/trailing whitespace
  if (grepl("^[0-9]", col_name)) col_name <- paste0("V_", col_name) # Add "V_" prefix if name starts with a number
  return(col_name)
}

# --- 4. Load, Clean, and Filter Data ---
cat(paste("Loading data from:", data_file, "\n"))

tryCatch({
    # Explicitly read the specified CSV file
    if (!file.exists(data_file)) {
        stop(paste0("Error: Data file not found at '", data_file, "'."))
    }
    # Use check.names = FALSE initially to preserve original names for cleaning
    df_raw <- read.csv(data_file, check.names = FALSE, na.strings=c("","NA","N/A","n/a")) # Use read.csv for CSV
    cat("Successfully loaded CSV file.\n")
}, error = function(e) {
    stop(paste("Error loading data from CSV '", data_file, "': ", e$message, "\n"))
})

# Apply column name cleaning
colnames(df_raw) <- sapply(colnames(df_raw), clean_col_name)
cleaned_columns <- colnames(df_raw)
cat("Cleaned column names.\n")

# Filter by consent (assuming a consent column exists and contains "agree")
consent_col_options <- c("Consent", "consent", "X1_Consent") # Common possibilities for the consent column name after cleaning
consent_col_name_found <- intersect(cleaned_columns, consent_col_options)[1] # Find the first match

if (!is.na(consent_col_name_found)) {
  initial_rows <- nrow(df_raw)
  # Use the actual found column name for filtering
  df <- df_raw %>% filter(str_detect(tolower(!!sym(consent_col_name_found)), "agree")) %>% select(-!!sym(consent_col_name_found))
  cat(paste("Filtered by consent. Rows remaining:", nrow(df), "(removed", initial_rows - nrow(df), ")\n"))
} else {
  df <- df_raw
  cat("Warning: 'Consent' column not found based on common names (", paste(consent_col_options, collapse=", "), "). Proceeding with all rows.\n")
  cat("Columns found in data:", paste(cleaned_columns, collapse=", "), "\n")
}

# --- 5. Define SEM Structure & Variable Mapping (Single-Factor Model) ---
# Defines the latent factor and its indicators, plus the outcome variable,
# using the cleaned column names from the data.

latent_variable_mapping <- list(
  'OLS_Organizational_And_Leadership_Support' = c(
    "To_what_extent_did_you_feel_that_there_was_clarity_in_your_purpose_for_volunteering_at_Organization",
    "To_what_extent_do_you_think_that_having_clear_consequences_for_missing_key_deadlines_is_important_for_keeping_contributors_engaged",
    "How_much_did_seeing_the_founder_working_hard_inspire_you_to_contribute_to_Organization",
    "How_much_did_seeing_your_team_lead_working_hard_inspire_you_to_contribute_to_Organization",
    "How_did_personalized_thank_you_messages_from_the_founder_and_team_leads_affect_your_commitment_to_Organization",
    "How_much_did_Organization_provide_opportunities_for_you_to_grow_as_a_professional",
    "How_much_did_opportunities_for_career_advancement_affect_your_engagement_with_Organization",
    "How_much_did_having_a_respected_job_title_impact_your_sense_of_value_at_Organization"
  ),
  'Future_Motivation_Outcome' = "Considering_your_overall_experience_with_Organization_how_motivated_do_you_feel_to_contribute_to_similar_organizations_or_projects_in_the_future"
)

# Define shorter aliases for variables for easier plotting/reporting if needed
# Note: These aliases are primarily for internal use in script/potential plots
# and do not affect the underlying SEM analysis performed by lavaan.
variable_aliases <- list(
  "OLS_Organizational_And_Leadership_Support" = "Org/Lead\nSupport",
  "Future_Motivation_Outcome" = "Future\nMotivation",
  "To_what_extent_did_you_feel_that_there_was_clarity_in_your_purpose_for_volunteering_at_Organization" = "OLS1_Purpose",
  "To_what_extent_do_you_think_that_having_clear_consequences_for_missing_key_deadlines_is_important_for_keeping_contributors_engaged" = "OLS2_Consequence",
  "How_much_did_seeing_the_founder_working_hard_inspire_you_to_contribute_to_Organization" = "OLS3_FounderInsp",
  "How_much_did_seeing_your_team_lead_working_hard_inspire_you_to_contribute_to_Organization" = "OLS4_LeadInsp",
  "How_did_personalized_thank_you_messages_from_the_founder_and_team_leads_affect_your_commitment_to_Organization" = "OLS5_Thanks",
  "How_much_did_Organization_provide_opportunities_for_you_to_grow_as_a_professional" = "OLS6_GrowthOpp",
  "How_much_did_opportunities_for_career_advancement_affect_your_engagement_with_Organization" = "OLS7_CareerOpp",
  "How_much_did_having_a_respected_job_title_impact_your_sense_of_value_at_Organization" = "OLS8_JobTitle"
)


# --- 6. Prepare Data for SEM ---
set.seed(12345); cat("\nSet random seed to 12345 for reproducibility.\n") # Ensures consistency where random processes are involved

# Select only the columns relevant for the SEM from the filtered dataframe
all_defined_vars <- unlist(latent_variable_mapping, use.names = FALSE)
cat("\nValidating indicator and outcome columns defined in mapping...\n")
valid_cols <- all_defined_vars[all_defined_vars %in% names(df)]
missing_cols <- setdiff(all_defined_vars, valid_cols)

if (length(missing_cols) > 0) {
    cat("Warning: Variables in mapping NOT found in cleaned data:\n")
    for(m_col in missing_cols){ cat(sprintf("  - '%s'\n", m_col)) }
    warning("Some defined variables not found in data. This could impact model specification.", immediate. = TRUE)
}
cat(sprintf("Found %d unique valid columns to proceed with, out of %d items defined in mapping.\n", length(valid_cols), length(all_defined_vars)))

if (length(valid_cols) == 0) stop("FATAL: No valid columns found from mapping. Check variable names and data loading/cleaning.")
df_sem_prep <- df %>% select(all_of(valid_cols))
cat(sprintf("\nPreparing %d columns for SEM...\n", ncol(df_sem_prep)))

# Convert columns to numeric. Non-numeric entries (e.g. "N/A" strings) become NA.
# NAs are intentionally retained and handled by FIML during model fitting.
# No median imputation is applied, as FIML is the preferred missing-data strategy
# (see: Enders & Bandalos, 2001; Graham, 2009).
cols_to_remove <- c()
cat("Converting columns to numeric. NAs retained for FIML (no imputation).\n")
for (col in names(df_sem_prep)) {
  original_values_col <- df_sem_prep[[col]]
  numeric_col <- suppressWarnings(tryCatch(
    as.numeric(as.character(original_values_col)),
    error = function(e) { rep(NA_real_, length(original_values_col)) }
  ))
  nas_from_conversion <- sum(is.na(numeric_col)) - sum(is.na(original_values_col))
  if (nas_from_conversion > 0) {
    cat(sprintf("  --> %d non-numeric entries converted to NA in '%s' (will be handled by FIML).\n",
                nas_from_conversion, col))
  }
  if (any(!is.na(numeric_col))) {
    df_sem_prep[[col]] <- numeric_col
  } else {
    cols_to_remove <- c(cols_to_remove, col)
    warning(paste0("Column '", col, "' is entirely NA and will be REMOVED."), immediate. = TRUE)
  }
}
if (length(cols_to_remove) > 0) {
  df_sem <- df_sem_prep %>% select(-all_of(cols_to_remove))
  cat(sprintf("Removed %d all-NA columns: %s\n", length(cols_to_remove), paste(cols_to_remove, collapse=", ")))
} else {
  df_sem <- df_sem_prep
}
cat("\nFinished data preparation.\n")

# Report NA counts per column (FIML will handle these during model fitting)
missing_per_col <- sapply(df_sem, function(x) sum(is.na(x)))
missing_per_col <- missing_per_col[missing_per_col > 0]
if (length(missing_per_col) > 0) {
  cat("NAs present (passed to FIML):\n")
  for (cn in names(missing_per_col)) cat(sprintf("  %s: %d NA(s)\n", cn, missing_per_col[[cn]]))
} else {
  cat("No NAs in prepared data.\n")
}
missing_after_strategy <- sum(sapply(df_sem, function(x) sum(is.na(x))))
cat(paste("Total NAs in data for SEM:", missing_after_strategy, "(handled by FIML).\n"))
# Ensure sufficient data for SEM
if (ncol(df_sem) < 2 || nrow(df_sem) < 20) {
  stop(paste0("Insufficient data (Cols:", ncol(df_sem), ", Rows:", nrow(df_sem),") for SEM."))
}


# --- 7. Data Exploration & Diagnostics ---
cat("\n--- Descriptive Statistics ---\n")
# Using psych::describe for standard descriptive output
if (ncol(df_sem) > 0 && nrow(df_sem) > 0) {
  options(width = 120); # Wide format for descriptives
  print(psych::describe(df_sem, fast = TRUE), digits = 2);
  options(width = 80) # Restore default width
} else { cat("No data for descriptives.\n") }

cat("\n--- Normality Checks ---\n")
# Check for univariate normality based on skewness and kurtosis thresholds
if (ncol(df_sem) > 0 && nrow(df_sem) > 0) {
  normality_info <- psych::describe(df_sem);
  highly_non_normal_cols <- character(0);
  cat("Checking for Skewness > |1.0| or Kurtosis (excess) > |3.0|...\n");
  for (col in colnames(df_sem)) {
    if (col %in% rownames(normality_info)) {
      s <- normality_info[col, "skew"];
      k <- normality_info[col, "kurtosis"];
      if (!is.na(s) && !is.na(k)) {
        cat(sprintf("'%s...': Skew=%.2f, Kurt=%.2f\n", substring(col, 1, 40), s, k));
        if (abs(s) > 1.0 || abs(k) > 3.0) {
          highly_non_normal_cols <- c(highly_non_normal_cols, col);
          cat(sprintf("  -> Potential non-normality noted for '%s'\n", col))
        }
      }
    }
  }
  if (length(highly_non_normal_cols) > 0) { cat("\n---> Non-normality detected. MLR estimator is appropriate.\n") } else { cat("\n---> No major univariate normality deviations based on skew/kurtosis thresholds.\n") }
} else { cat("No data for normality checks.\n") }

cat("\n--- Scale Reliability (Cronbach's Alpha) ---\n")
alpha_scores <- list();
defined_multi_item_lvs <- names(latent_variable_mapping)[sapply(latent_variable_mapping, function(x) is.character(x) && length(x) > 1)];
for (latent in defined_multi_item_lvs) {
  indicators_defined <- latent_variable_mapping[[latent]];
  present_indicators <- indicators_defined[indicators_defined %in% names(df_sem)];
  if (length(present_indicators) >= 2) {
      # Check for zero variance items which break alpha calculation
      variances <- sapply(df_sem[present_indicators], var, na.rm = TRUE);
      if(any(is.na(variances)) || any(variances == 0, na.rm = TRUE)){
          cat(paste("Skipping Alpha calculation for:", latent, "- item(s) with zero or NA variance.\n")); next
      };
      alpha_results <- tryCatch( psych::alpha(df_sem[present_indicators], check.keys = TRUE), error = function(e) { NA });
      if (is.list(alpha_results)) {
          alpha_score <- alpha_results$total$raw_alpha;
          if (!is.null(alpha_score) && !is.na(alpha_score)) {
              alpha_scores[[latent]] <- alpha_score;
              cat(paste("Scale:", latent, "(Items:", length(present_indicators), ") Alpha:", round(alpha_score, 3), "\n"));
              if (alpha_score < 0.7) { warning(paste0("Low alpha (< 0.7) for '", latent, "'. Interpret results with caution."), immediate. = TRUE) }
          } else {cat(paste("Alpha for '",latent,"' could not be computed (NA).\n"))}
      } else { cat(paste("Alpha calculation failed for:", latent, "\n")) }
  } else { cat(paste("Skipping Alpha calculation for:", latent, "- less than 2 indicators (", length(present_indicators), " found in data).\n")) }
}
cat(paste(rep("-", 30), collapse=""), "\n")

# --- Calculate Correlation Matrix ---
cat("\n--- Correlation Matrix ---\n")
if (ncol(df_sem) >= 2 && nrow(df_sem) > 1) {
   cor_matrix <- cor(df_sem, use = "pairwise.complete.obs")
   cat("Correlation matrix calculated.\n")

   # Print numeric correlation table with short aliases
   cor_aliases <- sapply(colnames(cor_matrix), function(cn) {
     alias <- variable_aliases[[cn]]
     if (is.null(alias) || alias == "") substring(cn, 1, 20) else alias
   })
   cor_matrix_print <- cor_matrix
   colnames(cor_matrix_print) <- cor_aliases
   rownames(cor_matrix_print) <- cor_aliases
   cat("\nCorrelation Matrix (pairwise complete obs, rounded to 2 dp):\n")
   options(width = 140)
   print(round(cor_matrix_print, 2))
   options(width = 80)
} else { cat("Skipping correlation matrix calculation (insufficient data).\n") }


# --- 8. Define SEM Model Syntax (Single-Factor Predictor Model) ---
cat("\n--- Defining SEM Model Components (Single-Factor Predictor Model) ---\n")
outcome_var_name <- latent_variable_mapping$'Future_Motivation_Outcome'
if (! (outcome_var_name %in% names(df_sem)) ) { stop(paste("FATAL: Outcome variable '", outcome_var_name, "' not found in data.", sep="")) }

# Measurement model syntax part
measurement_model_parts <- c();
latent_vars_in_model <- names(alpha_scores)  # Single factor: OLS_Organizational_And_Leadership_Support
for (latent in latent_vars_in_model) {
  indicators_defined <- latent_variable_mapping[[latent]];
  indicators_present <- indicators_defined[indicators_defined %in% colnames(df_sem)];
  if (length(indicators_present) >= 2) {
    measurement_model_parts <- c(measurement_model_parts, paste0("  ", latent, " =~ ", paste(indicators_present, collapse = " + ")));
    cat(paste("Measurement model defined for:", latent, "(", length(indicators_present)," items)\n"))
  } else {
     cat(paste("Warning: Skipping measurement model for", latent, "- less than 2 indicators present in data.\n"))
  }
}
measurement_model_syntax <- paste(measurement_model_parts, collapse = "\n")

# Structural model syntax part (Regression)
structural_model_parts <- c();
if (length(latent_vars_in_model) > 0) {
    structural_predictors <- paste(latent_vars_in_model, collapse = " + ");
    structural_model_parts <- c(structural_model_parts, paste0("  ", outcome_var_name, " ~ ", structural_predictors));
    cat(paste("Regression defined: '", outcome_var_name, "' ~ ", structural_predictors, "\n", sep=""));
} else {
    cat("No latent variables defined for structural model.\n")
}
structural_model_syntax <- paste(structural_model_parts, collapse = "\n");

# Combine measurement and structural models into the full SEM syntax string
sem_model_string <- paste(
  "# Measurement Model\n", measurement_model_syntax,
  "\n\n# Structural Model\n", structural_model_syntax,
  sep = ""
)

cat("\n--- Final SEM Model Syntax --- \n")
cat(sem_model_string)
cat("\n-------------------------------\n")


# --- 9. Fit the SEM Model ---
cat("\nFitting SEM model...\n");
fit <- NULL; # Initialize fit object

# Basic validation checks before attempting to fit
model_is_defined <- nchar(trimws(sem_model_string)) > 0 && grepl("~", sem_model_string);
# Check if the outcome variable and at least one predictor/indicator are in the data frame
data_has_necessary_vars <- (outcome_var_name %in% names(df_sem)) && (length(valid_cols) > 1) # Need outcome + at least one predictor/indicator

if (model_is_defined && data_has_necessary_vars) {
    # Select only numeric columns for lavaan
    df_sem_numeric <- df_sem %>% select(where(is.numeric));

    # Get observed variables actually required by the defined model syntax
    observed_vars_in_syntax <- character(0)
    tryCatch({
        observed_vars_in_syntax <- lavaan::lavNames(sem_model_string, "ov")
    }, error = function(e) {
        cat("Warning: Could not get observed variable names from model syntax using lavNames.\n")
        # Fallback: try to extract names manually (less robust)
        temp_ovs <- unlist(str_extract_all(sem_model_string, "[a-zA-Z0-9_]+"))
        observed_vars_in_syntax <- unique(temp_ovs)
    })

    if (length(observed_vars_in_syntax) == 0) {
        cat("No observed variables identified as required by the model syntax. Skipping fit.\n"); fit <- NULL
    } else {
        # Check if all observed variables required by syntax are present in the numeric data frame
        missing_observed_vars <- setdiff(observed_vars_in_syntax, names(df_sem_numeric));
        if (length(missing_observed_vars) > 0) {
            cat("Error: Observed variables required by model syntax are MISSING from the numeric data frame:\n", paste(" - ", missing_observed_vars, collapse="\n"), "\nSkipping fit.\n"); fit <- NULL
        } else {
            # Select only the necessary columns for lavaan fitting
            data_for_lavaan <- df_sem_numeric %>% select(all_of(observed_vars_in_syntax));
            if(nrow(data_for_lavaan) < 30){ warning(paste0("Small sample size (N=", nrow(data_for_lavaan), "). Results may be unstable."), immediate. = TRUE)};
            cat(paste("Fitting model with", nrow(data_for_lavaan), "rows and", ncol(data_for_lavaan), "columns.\n"));

            # Fit the SEM model using MLR estimator and FIML for missing data
            fit <- tryCatch({
                lavaan::sem(model = sem_model_string,
                            data = data_for_lavaan,
                            estimator = "MLR",          # Robust Maximum Likelihood
                            missing = missing_data_strategy,# FIML as defined
                            warn = TRUE,                # Show warnings from lavaan
                            mimic="Mplus"               # Match Mplus output for fit indices
                           )
            }, error = function(e) {
                message("\nError SEM fitting: ", e$message); return(NULL)
            })
        }
    }
} else {
    cat("Skipping SEM: Model not defined or insufficient variables/data for fitting.\n")
}


# --- 10. Summarize and Evaluate Results ---
# Print full summary if model converged. Otherwise, print failure message.
if (!is.null(fit) && lavInspect(fit, "converged")) {
  cat("\n--- SEM converged. ---\n");
  cat("\n--- SEM Summary (Fit Measures, Estimates, R-Square) ---\n");
  options(max.print=2000); # Temporarily increase max.print for summary table
  # Print the full summary including fit measures, standardized estimates, and R-square
  print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE));
  options(max.print=99999) # Restore default or higher

  # Extract and print key results explicitly for clarity in output.txt
  # These replicate parts of the full summary but are easy to find.
  cat("\n--- Key Fit Measures (Recap) ---\n")
  fit_indices <- tryCatch({
      fitMeasures(fit, c("npar", "chisq.scaled", "df.scaled", "pvalue.scaled",
                          "cfi.scaled", "tli.scaled", "rmsea.scaled",
                          "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
  }, error = function(e) { cat("Error extracting key fit measures:", e$message, "\n"); return(NULL) });
  if (!is.null(fit_indices)) {
      print(round(fit_indices, 3));
      cat("\nFit Guide (MLR):\n  Pval Chisq: >.05\n  CFI/TLI: >.90/.95\n  RMSEA: <.08/.06 (CI upper <.10)\n  SRMR: <.08\n");
      # Simple interpretation
      cfi_ok <- !is.na(fit_indices["cfi.scaled"]) && fit_indices["cfi.scaled"] > 0.90;
      tli_ok <- !is.na(fit_indices["tli.scaled"]) && fit_indices["tli.scaled"] > 0.90;
      rmsea_ok <- !is.na(fit_indices["rmsea.scaled"]) && fit_indices["rmsea.scaled"] < 0.08;
      srmr_ok <- !is.na(fit_indices["srmr"]) && fit_indices["srmr"] < 0.08;
      if(cfi_ok && tli_ok && rmsea_ok && srmr_ok) { cat("Overall Fit: Acceptable/Good based on general guidelines.\n") } else { cat("Overall Fit: Review indices against guidelines.\n") }
  } else { cat("Could not extract key fit measures.\n") }

  cat("\n--- Regression Paths (Standardized) ---\n");
  reg_table <- parameterEstimates(fit, standardized = TRUE);
  reg_paths <- reg_table[reg_table$op == "~", ]; # Select regression paths
  if (nrow(reg_paths) > 0) {
      # Print relevant columns for regression paths
      print(reg_paths[, c("lhs", "op", "rhs", "est", "se", "z", "pvalue", "std.all")], digits = 3)
      # Identify and print significant paths
      sig_paths <- reg_paths[reg_paths$pvalue < 0.05, ]
      if (nrow(sig_paths) > 0) { cat("\nSignificant Predictors (p<0.05, Standardized Estimates):\n"); print(sig_paths[, intersect(c("lhs", "rhs", "est", "std.all", "pvalue"), colnames(sig_paths)), drop = FALSE], digits = 3) } else { cat("\nNo predictors significant at p<0.05.\n") }
  } else { cat("No regression paths defined in the model.\n") }

  cat("\n--- R-Square for Outcome ---\n");
  rsq_table <- parameterEstimates(fit, rsquare = TRUE);
  outcome_rsq_val <- rsq_table[rsq_table$op == "r2" & rsq_table$lhs == outcome_var_name, "est"];
  if (length(outcome_rsq_val) == 1) { cat(paste("R2 for '", outcome_var_name, "': ", round(outcome_rsq_val, 3), "\n", sep="")) } else { cat("Could not extract R-Square for outcome variable.\n") }

  # Modification Indices (Print only if MI > threshold)
  cat("\n--- Modification Indices --- \n");
  mods <- tryCatch({ modificationIndices(fit, sort. = TRUE, minimum.value = 3.84) }, # MI > ~3.84 corresponds to chi-sq diff > 3.84 (p < .05)
                    error = function(e){ cat("Error calculating modification indices:", e$message, "\n"); return(NULL) });
  if (!is.null(mods) && nrow(mods) > 0) {
      cat(paste0("Top Modification Indices (MI > ", 3.84, "):\n"));
      print(head(mods, 10), digits=3) # Print top 10 or fewer if less than 10 meet criteria
  } else if (!is.null(mods) && nrow(mods) == 0) {
      cat(paste0("No Modification Indices > ", 3.84, ".\n"));
  } else { cat("Modification Index calculation failed.\n") }

  # --- Convergent Validity: AVE and Composite Reliability (CR) ---
  # AVE = mean(λ²); CR = (Σλ)² / ((Σλ)² + Σ(1−λ²))
  # Thresholds: AVE >= 0.50, CR >= 0.70 (Fornell & Larcker, 1981; Hair et al., 2010)
  cat("\n--- Convergent Validity: AVE and Composite Reliability (CR) ---\n")
  tryCatch({
    param_est_cv <- parameterEstimates(fit, standardized = TRUE)
    lv_names_cv  <- unique(param_est_cv$lhs[param_est_cv$op == "=~"])
    if (length(lv_names_cv) == 0) {
      cat("No latent variable loadings found; cannot compute AVE/CR.\n")
    } else {
      for (lv in lv_names_cv) {
        lambdas <- param_est_cv$std.all[param_est_cv$op == "=~" & param_est_cv$lhs == lv]
        lambdas <- lambdas[!is.na(lambdas)]
        if (length(lambdas) < 2) next
        lambda2  <- lambdas^2
        delta2   <- 1 - lambda2
        ave      <- mean(lambda2)
        cr       <- sum(lambdas)^2 / (sum(lambdas)^2 + sum(delta2))
        ave_flag <- if (ave >= 0.50) "[AVE >= 0.50: OK]" else "[AVE < 0.50: concern]"
        cr_flag  <- if (cr  >= 0.70) "[CR >= 0.70: OK]"  else "[CR < 0.70: concern]"
        cat(sprintf("  %-45s  AVE = %.3f %s  CR = %.3f %s\n", lv, ave, ave_flag, cr, cr_flag))
      }
      cat("Ref: Fornell & Larcker (1981); Hair et al. (2010). Standard thresholds: AVE>=0.50, CR>=0.70.\n")
    }
  }, error = function(e) { cat("Error computing AVE/CR:", e$message, "\n") })

} else {
    # Message if SEM fitting failed or did not converge
    if (is.null(fit)) { cat("\n--- SEM fit failed or was skipped. Check error messages above. ---\n") } else {
        warning("SEM did NOT converge.", immediate. = TRUE);
        cat("\n--- Non-Converged Model (Partial Output) ---\n");
        # Print minimal summary for non-converged model
        print(summary(fit))
    }
}

# --- 11. OLS2 Sensitivity Check ---
# Re-fit SEM excluding OLS2_Consequence from the latent factor to verify that
# main results are not driven by this item. OLS2 asks about respondents' beliefs
# about deadline consequences, which is conceptually distinct from the direct
# organizational support experience captured by OLS1, OLS3–OLS8.
cat("\n--- OLS2 Sensitivity Check (Model Without OLS2_Consequence) ---\n")
cat("Rationale: OLS2 measures beliefs about hypothetical deadline consequences,\n")
cat("  unlike OLS1/OLS3-8 which measure direct personal support experiences.\n")
tryCatch({
  ols2_col <- "To_what_extent_do_you_think_that_having_clear_consequences_for_missing_key_deadlines_is_important_for_keeping_contributors_engaged"
  if (!(ols2_col %in% names(df_sem))) {
    cat("OLS2 column not found in data — skipping sensitivity check.\n")
  } else if (is.null(fit) || !lavInspect(fit, "converged")) {
    cat("Main model did not converge — skipping OLS2 sensitivity check.\n")
  } else {
    ols_indicators_full   <- latent_variable_mapping[["OLS_Organizational_And_Leadership_Support"]]
    ols_indicators_no_ols2 <- ols_indicators_full[ols_indicators_full != ols2_col]
    ols_indicators_no_ols2 <- ols_indicators_no_ols2[ols_indicators_no_ols2 %in% colnames(df_sem)]

    if (length(ols_indicators_no_ols2) < 2) {
      cat("Fewer than 2 indicators remain after removing OLS2 — cannot fit sensitivity model.\n")
    } else {
      sens_meas   <- paste0("  OLS_Organizational_And_Leadership_Support =~ ",
                            paste(ols_indicators_no_ols2, collapse = " + "))
      sens_struct <- paste0("  ", outcome_var_name, " ~ OLS_Organizational_And_Leadership_Support")
      sens_syntax <- paste("# Measurement Model (OLS2 excluded)\n", sens_meas,
                           "\n\n# Structural Model\n", sens_struct, sep = "")
      cat("Sensitivity model (OLS2 excluded):\n"); cat(sens_syntax); cat("\n")

      sens_ovs       <- lavaan::lavNames(sens_syntax, "ov")
      data_for_sens  <- df_sem %>% select(where(is.numeric)) %>%
                          select(all_of(intersect(sens_ovs, names(.))))

      set.seed(12345)  # reset before sensitivity fit to match standalone sensitivity_ols2_excluded.txt
      fit_sens <- tryCatch(
        lavaan::sem(model = sens_syntax, data = data_for_sens,
                    estimator = "MLR", missing = missing_data_strategy,
                    warn = TRUE, mimic = "Mplus"),
        error = function(e) { message("Sensitivity model error: ", e$message); NULL }
      )

      if (!is.null(fit_sens) && lavInspect(fit_sens, "converged")) {
        cat("\n-- Sensitivity model converged. --\n")

        sens_fi <- tryCatch(
          fitMeasures(fit_sens, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr")),
          error = function(e) NULL)
        if (!is.null(sens_fi)) { cat("Fit Indices (OLS2 excluded):\n"); print(round(sens_fi, 3)) }

        sens_reg  <- parameterEstimates(fit_sens, standardized = TRUE)
        sens_path <- sens_reg[sens_reg$op == "~", ]
        if (nrow(sens_path) > 0) {
          cat("\nRegression Path (OLS2 excluded):\n")
          print(sens_path[, c("lhs","op","rhs","est","se","z","pvalue","std.all")], digits = 3)
        }

        sens_rsq <- parameterEstimates(fit_sens, rsquare = TRUE)
        sens_r2  <- sens_rsq[sens_rsq$op == "r2" & sens_rsq$lhs == outcome_var_name, "est"]
        if (length(sens_r2) == 1) cat(sprintf("\nR2 (OLS2 excluded): %.3f\n", sens_r2))

        cat("\nConclusion: If β, p-value, R², and fit indices are similar to the full model,\n")
        cat("  the main finding is robust to the exclusion of OLS2_Consequence.\n")
      } else {
        cat("Sensitivity model did not converge or failed to fit.\n")
      }
    }
  }
}, error = function(e) { cat("Error in OLS2 sensitivity check:", e$message, "\n") })


# --- 12. WLSMV Robustness Check ---
cat("\n--- WLSMV Robustness Check ---\n")
cat("Note: WLSMV treats all items as ordinal/categorical. With 5-point Likert items\n")
cat("exhibiting ceiling effects and skewness, RMSEA may be elevated relative to MLR.\n")
cat("MLR with FIML remains the primary estimator; WLSMV is reported for completeness.\n\n")

tryCatch({
  if (!is.null(fit) && lavInspect(fit, "converged")) {
    wlsmv_ovs  <- tryCatch(lavaan::lavNames(sem_model_string, "ov"), error = function(e) NULL)
    if (!is.null(wlsmv_ovs)) {
      data_wlsmv <- df_sem %>% select(where(is.numeric)) %>%
                      select(all_of(intersect(wlsmv_ovs, names(.))))
      fit_wlsmv <- lavaan::sem(model = sem_model_string, data = data_wlsmv,
                               estimator = "WLSMV", ordered = TRUE,
                               missing = "pairwise", warn = TRUE)
      if (lavInspect(fit_wlsmv, "converged")) {
        cat("WLSMV model converged.\n")
        fi_w <- fitMeasures(fit_wlsmv, c("cfi.robust","tli.robust","rmsea.robust",
                                         "rmsea.ci.lower.robust","rmsea.ci.upper.robust","srmr"))
        cat(sprintf("CFI=%.3f, TLI=%.3f, RMSEA=%.3f [%.3f, %.3f], SRMR=%.3f\n",
                    fi_w["cfi.robust"], fi_w["tli.robust"], fi_w["rmsea.robust"],
                    fi_w["rmsea.ci.lower.robust"], fi_w["rmsea.ci.upper.robust"], fi_w["srmr"]))
        pe_w      <- parameterEstimates(fit_wlsmv, standardized = TRUE)
        struct_w  <- pe_w[pe_w$op == "~", ]
        cat(sprintf("Structural path: b=%.3f, SE=%.3f, z=%.3f, p=%.3f, std.beta=%.3f\n",
                    struct_w$est, struct_w$se, struct_w$z, struct_w$pvalue, struct_w$std.all))
      } else {
        cat("WLSMV model did not converge.\n")
      }
    }
  } else {
    cat("Skipping WLSMV check (main MLR model did not converge).\n")
  }
}, error = function(e) { cat("Error in WLSMV check:", e$message, "\n") })


# --- 13. Completion ---
cat("\n--- R Script Completed ---\n")