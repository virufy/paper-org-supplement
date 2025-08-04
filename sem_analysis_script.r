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
# FIML (Full Information Maximum Likelihood) is used in lavaan for SEM fitting.
# Note: Median imputation is applied below for non-numeric entries converting to NA,
# but FIML handles any remaining structural or intermittent NAs during model fitting.
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
    "To_what_extent_did_you_feel_that_there_was_clarity_in_your_purpose_for_volunteering_at_Virufy",
    "To_what_extent_do_you_think_that_having_clear_consequences_for_missing_key_deadlines_is_important_for_keeping_contributors_engaged",
    "How_much_did_seeing_the_founder_Amil_working_hard_inspire_you_to_contribute_to_Virufy",
    "How_much_did_seeing_your_team_lead_working_hard_inspire_you_to_contribute_to_Virufy",
    "How_did_personalized_thank_you_messages_from_the_founder_and_team_leads_affect_your_commitment_to_Virufy",
    "How_much_did_Virufy_provide_opportunities_for_you_to_grow_as_a_professional",
    "How_much_did_opportunities_for_career_advancement_affect_your_engagement_with_Virufy",
    "How_much_did_having_a_respected_job_title_impact_your_sense_of_value_at_Virufy"
  ),
  'Future_Motivation_Outcome' = "Considering_your_overall_experience_with_Virufy_how_motivated_do_you_feel_to_contribute_to_similar_organizations_or_projects_in_the_future"
)

# Define shorter aliases for variables for easier plotting/reporting if needed
# Note: These aliases are primarily for internal use in script/potential plots
# and do not affect the underlying SEM analysis performed by lavaan.
variable_aliases <- list(
  "OLS_Organizational_And_Leadership_Support" = "Org/Lead\nSupport",
  "Future_Motivation_Outcome" = "Future\nMotivation",
  "To_what_extent_did_you_feel_that_there_was_clarity_in_your_purpose_for_volunteering_at_Virufy" = "OLS1_Purpose",
  "To_what_extent_do_you_think_that_having_clear_consequences_for_missing_key_deadlines_is_important_for_keeping_contributors_engaged" = "OLS2_Consequence",
  "How_much_did_seeing_the_founder_Amil_working_hard_inspire_you_to_contribute_to_Virufy" = "OLS3_FounderInsp",
  "How_much_did_seeing_your_team_lead_working_hard_inspire_you_to_contribute_to_Virufy" = "OLS4_LeadInsp",
  "How_did_personalized_thank_you_messages_from_the_founder_and_team_leads_affect_your_commitment_to_Virufy" = "OLS5_Thanks",
  "How_much_did_Virufy_provide_opportunities_for_you_to_grow_as_a_professional" = "OLS6_GrowthOpp",
  "How_much_did_opportunities_for_career_advancement_affect_your_engagement_with_Virufy" = "OLS7_CareerOpp",
  "How_much_did_having_a_respected_job_title_impact_your_sense_of_value_at_Virufy" = "OLS8_JobTitle"
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

# Convert columns to numeric and apply median imputation for values becoming NA
cols_to_remove <- c(); issues_found <- FALSE
cat("Attempting to convert columns to numeric. Applying median imputation for non-numeric entries that become NA.\n")
for (col in names(df_sem_prep)) {
  original_values_col <- df_sem_prep[[col]]
  # Attempt conversion to numeric. Handle potential non-numeric entries leading to NAs.
  numeric_col <- suppressWarnings(tryCatch(as.numeric(as.character(original_values_col)), error = function(e) { rep(NA_real_, length(original_values_col)) }))
  nas_from_conversion <- sum(is.na(numeric_col)) - sum(is.na(original_values_col))
  if (nas_from_conversion > 0) { cat(sprintf("  --> Note: %d NAs from non-numeric entries in '%s'.\n", nas_from_conversion, col)) }

  if (any(!is.na(numeric_col))) { # Check if there's *any* valid numeric data in the column
      nas_to_impute_in_step <- sum(is.na(numeric_col))
      if(nas_to_impute_in_step > 0) {
          median_val <- median(numeric_col, na.rm = TRUE)
          if(!is.na(median_val)) { # Check if median calculation was successful
              numeric_col[is.na(numeric_col)] <- median_val
              # cat(sprintf("  Imputed %d NA(s) in '%s' with median %.2f.\n", nas_to_impute_in_step, col, median_val)) # Keep this line if matching output needed
          } else { # Median was NA (e.g., column was all NAs to begin with)
             cat(sprintf("  --> Warn: Median NA for '%s'. Column may still contain NAs or be removed.\n", col)); issues_found <- TRUE
          }
      }
      df_sem_prep[[col]] <- numeric_col # Assign the (imputed) numeric column back
  } else { # Column became all NAs or was all NAs/non-numeric initially
    cols_to_remove <- c(cols_to_remove, col)
    warning(paste0("Column '", col, "' became all NAs after attempted conversion/imputation and will be REMOVED."), immediate. = TRUE)
    issues_found <- TRUE
  }
}
if (length(cols_to_remove) > 0) { df_sem <- df_sem_prep %>% select(-all_of(cols_to_remove)); cat(sprintf("Removed %d all-NA columns: %s\n", length(cols_to_remove), paste(cols_to_remove, collapse=", "))) } else { df_sem <- df_sem_prep }
cat("\nFinished data preparation.\n"); if(issues_found){ cat("*** Review Warnings Above Carefully! ***\n") }

# Note: FIML handles remaining NAs. Listwise deletion is not used for final model.
missing_after_strategy <- sum(sapply(df_sem, function(x) sum(is.na(x))))
cat(paste("Final check: Total NAs in data for SEM:", missing_after_strategy, ".\n"))
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

# --- Calculate Correlation Matrix (Plotting Skipped) ---
cat("\n--- Calculating Correlation Matrix (Plotting Skipped for Reproducibility) ---\n")
if (ncol(df_sem) >= 2 && nrow(df_sem) > 1) {
   cor_matrix <- cor(df_sem, use = "pairwise.complete.obs") # Keep calculation
   # Plotting and saving commented out
   # ... plotting code ...
   cat("Correlation matrix calculated.\n")
} else { cat("Skipping correlation matrix calculation (insufficient data).\n") }


# --- 8. Define SEM Model Syntax (Single-Factor Predictor Model) ---
cat("\n--- Defining SEM Model Components (Single-Factor Predictor Model) ---\n")
outcome_var_name <- latent_variable_mapping$'Future_Motivation_Outcome'
if (! (outcome_var_name %in% names(df_sem)) ) { stop(paste("FATAL: Outcome variable '", outcome_var_name, "' not found in data.", sep="")) }

# Measurement model syntax part
measurement_model_parts <- c();
latent_vars_in_model <- names(alpha_scores) # Assumes only scales with alpha >= 0.7 are used as LVs, or all defined LVs with >=2 items
# In this single factor model, it should be just OLS_Organizational_And_Leadership_Support
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

# --- Variable Legend (Created internally, Saving Skipped) ---
# This section creates a dataframe mapping full variable names to aliases,
# useful for internal checks or potentially plotting code, but the dataframe
# is not saved to disk as part of the reproducibility output.
cat("\n--- Creating variable legend dataframe (Saving Skipped) ---\n")
tryCatch({
    legend_df_list <- list();
    used_vars <- colnames(df_sem);

    # Get variable names from the defined latent_variable_mapping that are present in the data
    all_vars_in_mapping_and_data <- unlist(latent_variable_mapping, use.names = FALSE)
    all_vars_in_mapping_and_data <- all_vars_in_mapping_and_data[all_vars_in_mapping_and_data %in% used_vars]

    for(full_name in all_vars_in_mapping_and_data) {
        alias <- variable_aliases[[full_name]]; # Get defined alias
        if (is.null(alias) || alias == "") alias <- substring(full_name, 1, 25); # Use start of name if no alias

        var_type <- "Observed";
        mapped_lv <- "N/A";

        if (full_name == outcome_var_name) {
            var_type <- "Outcome (in Model)";
            mapped_lv = "Directly Included" # Clarify it's the outcome
        } else {
            # Check if it's an indicator for any defined latent variable
            for (lv_name in names(latent_variable_mapping)) {
                if (is.character(latent_variable_mapping[[lv_name]]) && full_name %in% latent_variable_mapping[[lv_name]]) {
                    # Check if the latent variable itself is included in the model (has >= 2 indicators)
                    if (lv_name %in% latent_vars_in_model) { # Check against LVs used in SEM syntax
                         var_type <- "Indicator (in Model)";
                         mapped_lv <- lv_name;
                         break # Stop after finding the first LV it maps to
                    } else {
                         var_type <- "Indicator (Excluded LV)"; # Indicator but its LV was excluded
                         mapped_lv <- lv_name;
                         break
                    }
                }
            }
        }
        legend_df_list[[length(legend_df_list) + 1]] <- data.frame(
            Alias = alias,
            Full_Variable_Name = full_name,
            Type = var_type,
            Mapped_Latent = mapped_lv
        )
    }
    if(length(legend_df_list) > 0) {
      legend_df <- bind_rows(legend_df_list) %>% arrange(Type, Mapped_Latent, Alias) %>% distinct();
      # Saving commented out
      # write.csv(legend_df, "variable_legend.csv", row.names = FALSE, quote = TRUE);
      cat("Legend dataframe created internally.\n")
    } else {
      cat("No variables found to create legend dataframe.\n")
    }
}, error = function(e){ cat("\nError creating legend dataframe:", e$message, "\n") })


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

} else {
    # Message if SEM fitting failed or did not converge
    if (is.null(fit)) { cat("\n--- SEM fit failed or was skipped. Check error messages above. ---\n") } else {
        warning("SEM did NOT converge.", immediate. = TRUE);
        cat("\n--- Non-Converged Model (Partial Output) ---\n");
        # Print minimal summary for non-converged model
        print(summary(fit))
    }
}

# --- 11. Completion Summary ---
cat("\n--- R Script Completed ---\n")
cat("Statistical analysis executed.\n")
cat("Correlation Matrix Calculation: Performed.\n")
cat("SEM Model Fitting: Attempted and reported above.\n")
cat("Figure Plotting and File Saving: Skipped as configured for reproducibility package.\n")