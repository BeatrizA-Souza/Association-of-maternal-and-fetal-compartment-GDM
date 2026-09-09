# ============================================================
# CLINICAL DATA ANALYSIS
# FINAL CLEAN VERSION
#
# CTR vs GDM
#
# Figures:
# Mean ± SEM + individual observations
#
# Tables:
# N, mean, SD, SEM, median, IQR
#
# Adjusted models:
# Newborn weight ~ Group + Gestational age + Fetal sex
# Placental weight ~ Group + Gestational age + Fetal sex
#
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

setwd(
  "C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros/figira dados totais"
)

required_packages <- c(
  "dplyr",
  "tidyr",
  "readr",
  "stringr",
  "ggplot2",
  "openxlsx",
  "car",
  "patchwork",
  "tibble"
)

missing_packages <- required_packages[
  !required_packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
  library(openxlsx)
  library(car)
  library(patchwork)
  library(tibble)
})

set.seed(123)


# ============================================================
# 2. OUTPUT FOLDERS
# ============================================================

output_dir <- "Clinical_analysis_FINAL"

figure_dir <- file.path(
  output_dir,
  "Figures"
)

table_dir <- file.path(
  output_dir,
  "Tables"
)

diagnostic_dir <- file.path(
  output_dir,
  "Model_diagnostics"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  diagnostic_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. GROUP COLORS
# ============================================================

group_colors <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)


# ============================================================
# 4. INPUT FILE
# ============================================================

clinical_file <- file.path(
  "C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros",
  "tabela dados .csv"
)

if (!file.exists(clinical_file)) {
  stop(
    paste(
      "Clinical data file was not found:",
      clinical_file
    )
  )
}


# ============================================================
# 5. READ DATA
# ============================================================

clinical_data_raw <- readr::read_csv2(
  clinical_file,
  na = c(
    "-",
    "",
    "NA"
  ),
  show_col_types = FALSE
)


# ============================================================
# 6. CHECK REQUIRED COLUMNS
# ============================================================

required_columns <- c(
  "Sample type",
  "Age",
  "Gestational Age at collection",
  "Pre-gestational weight",
  "weight",
  "height",
  "BMI pre",
  "BMI actually",
  "Weight gain",
  "Systolic blood pressure",
  "Diastolic blood pressure",
  "Fasting blood glucose",
  "GTT Result 1 hour",
  "GTT Result 2h",
  "newborn weight (Kg)",
  "placenta weight (gr)"
)

missing_columns <- setdiff(
  required_columns,
  names(clinical_data_raw)
)

if (length(missing_columns) > 0) {
  
  stop(
    paste0(
      "The following columns were not found in the CSV:\n",
      paste(
        missing_columns,
        collapse = "\n"
      )
    )
  )
}


# ============================================================
# 7. RENAME VARIABLES
#
# After this point, the script uses standardized names.
# ============================================================

clinical_data <- clinical_data_raw %>%
  
  dplyr::rename(
    
    Group =
      `Sample type`,
    
    Gestational_age_collection =
      `Gestational Age at collection`,
    
    Pregestational_weight =
      `Pre-gestational weight`,
    
    Current_weight =
      weight,
    
    Height =
      height,
    
    Pregestational_BMI =
      `BMI pre`,
    
    Current_BMI =
      `BMI actually`,
    
    Weight_gain =
      `Weight gain`,
    
    Systolic_BP =
      `Systolic blood pressure`,
    
    Diastolic_BP =
      `Diastolic blood pressure`,
    
    Fasting_glucose =
      `Fasting blood glucose`,
    
    GTT_1h =
      `GTT Result 1 hour`,
    
    GTT_2h =
      `GTT Result 2h`,
    
    Newborn_weight_kg =
      `newborn weight (Kg)`,
    
    Placental_weight_g =
      `placenta weight (gr)`
    
  ) %>%
  
  dplyr::mutate(
    
    Group = stringr::str_trim(
      toupper(Group)
    ),
    
    Group = factor(
      Group,
      levels = c(
        "CTR",
        "GDM"
      )
    )
    
  )


# ============================================================
# 8. NUMERIC VARIABLES
# ============================================================

numeric_variables <- c(
  "Age",
  "Gestational_age_collection",
  "Pregestational_weight",
  "Current_weight",
  "Height",
  "Pregestational_BMI",
  "Current_BMI",
  "Weight_gain",
  "Systolic_BP",
  "Diastolic_BP",
  "Fasting_glucose",
  "GTT_1h",
  "GTT_2h",
  "Newborn_weight_kg",
  "Placental_weight_g"
)


# ============================================================
# 9. ENSURE NUMERIC FORMAT
# ============================================================

clinical_data <- clinical_data %>%
  
  dplyr::mutate(
    
    dplyr::across(
      
      dplyr::all_of(
        numeric_variables
      ),
      
      ~ {
        
        if (is.numeric(.x)) {
          
          as.numeric(.x)
          
        } else {
          
          readr::parse_number(
            as.character(.x),
            locale = readr::locale(
              decimal_mark = ",",
              grouping_mark = "."
            )
          )
          
        }
        
      }
      
    )
    
  )


# ============================================================
# 10. VARIABLE LABELS
# ============================================================

variable_labels <- c(
  
  Age =
    "Age (years)",
  
  Gestational_age_collection =
    "Gestational age at collection (weeks)",
  
  Pregestational_weight =
    "Pre-gestational weight (kg)",
  
  Current_weight =
    "Weight (kg)",
  
  Height =
    "Height (m)",
  
  Pregestational_BMI =
    "Pre-gestational BMI (kg/m²)",
  
  Current_BMI =
    "Current BMI (kg/m²)",
  
  Weight_gain =
    "Weight gain (kg)",
  
  Systolic_BP =
    "Systolic blood pressure (mmHg)",
  
  Diastolic_BP =
    "Diastolic blood pressure (mmHg)",
  
  Fasting_glucose =
    "Fasting blood glucose (mg/dL)",
  
  GTT_1h =
    "GTT 1 h (mg/dL)",
  
  GTT_2h =
    "GTT 2 h (mg/dL)",
  
  Newborn_weight_kg =
    "Newborn weight (kg)",
  
  Placental_weight_g =
    "Placental weight (g)"
)


# ============================================================
# 11. SIGNIFICANCE LABEL
# ============================================================

significance_label <- function(p) {
  
  dplyr::case_when(
    
    is.na(p) ~ "",
    
    p < 0.05 ~ "*",
    
    TRUE ~ "ns"
    
  )
  
}


# ============================================================
# 12. CTR vs GDM STATISTICAL FUNCTION
#
# If both groups pass Shapiro-Wilk:
# Welch's t-test
#
# Otherwise:
# Two-sided Mann-Whitney test
#
# ============================================================

compare_groups <- function(
    df,
    variable
) {
  
  ctr <- df %>%
    
    dplyr::filter(
      Group == "CTR"
    ) %>%
    
    dplyr::pull(
      dplyr::all_of(
        variable
      )
    ) %>%
    
    stats::na.omit()
  
  
  gdm <- df %>%
    
    dplyr::filter(
      Group == "GDM"
    ) %>%
    
    dplyr::pull(
      dplyr::all_of(
        variable
      )
    ) %>%
    
    stats::na.omit()
  
  
  n_ctr <- length(ctr)
  
  n_gdm <- length(gdm)
  
  
  # ----------------------------------------------------------
  # Insufficient data
  # ----------------------------------------------------------
  
  if (
    n_ctr < 3 ||
    n_gdm < 3 ||
    length(unique(ctr)) <= 1 ||
    length(unique(gdm)) <= 1
  ) {
    
    return(
      
      data.frame(
        
        Variable = variable,
        
        Variable_label =
          unname(
            variable_labels[
              variable
            ]
          ),
        
        N_CTR = n_ctr,
        
        N_GDM = n_gdm,
        
        Shapiro_P_CTR =
          NA_real_,
        
        Shapiro_P_GDM =
          NA_real_,
        
        Statistical_test =
          NA_character_,
        
        Test_statistic =
          NA_real_,
        
        Effect_estimate_GDM_minus_CTR =
          NA_real_,
        
        CI95_lower =
          NA_real_,
        
        CI95_upper =
          NA_real_,
        
        P_value =
          NA_real_,
        
        Significance =
          ""
        
      )
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Shapiro-Wilk
  # ----------------------------------------------------------
  
  shapiro_ctr <-
    stats::shapiro.test(
      ctr
    )$p.value
  
  
  shapiro_gdm <-
    stats::shapiro.test(
      gdm
    )$p.value
  
  
  # ----------------------------------------------------------
  # Welch's t-test
  # ----------------------------------------------------------
  
  if (
    shapiro_ctr > 0.05 &&
    shapiro_gdm > 0.05
  ) {
    
    test_result <-
      stats::t.test(
        
        gdm,
        
        ctr,
        
        var.equal = FALSE,
        
        alternative =
          "two.sided",
        
        conf.level =
          0.95
        
      )
    
    
    test_name <-
      "Welch's t-test"
    
    
    statistic <-
      unname(
        test_result$statistic
      )
    
    
    p_value <-
      test_result$p.value
    
    
    effect_estimate <-
      mean(
        gdm,
        na.rm = TRUE
      ) -
      mean(
        ctr,
        na.rm = TRUE
      )
    
    
    ci_lower <-
      test_result$conf.int[1]
    
    
    ci_upper <-
      test_result$conf.int[2]
    
    
  } else {
    
    # --------------------------------------------------------
    # Mann-Whitney
    #
    # Safe fallback if CI estimation fails because of
    # ties or small sample size.
    # --------------------------------------------------------
    
    test_result_ci <- tryCatch(
      
      suppressWarnings(
        
        stats::wilcox.test(
          
          gdm,
          
          ctr,
          
          paired = FALSE,
          
          alternative =
            "two.sided",
          
          exact = FALSE,
          
          conf.int = TRUE,
          
          conf.level = 0.95
          
        )
        
      ),
      
      error = function(e) NULL
      
    )
    
    
    if (is.null(test_result_ci)) {
      
      test_result <-
        suppressWarnings(
          
          stats::wilcox.test(
            
            gdm,
            
            ctr,
            
            paired = FALSE,
            
            alternative =
              "two.sided",
            
            exact = FALSE,
            
            conf.int = FALSE
            
          )
          
        )
      
      
      effect_estimate <-
        NA_real_
      
      ci_lower <-
        NA_real_
      
      ci_upper <-
        NA_real_
      
      
    } else {
      
      test_result <-
        test_result_ci
      
      
      if (
        !is.null(
          test_result$estimate
        ) &&
        length(
          test_result$estimate
        ) > 0
      ) {
        
        effect_estimate <-
          unname(
            test_result$estimate
          )
        
      } else {
        
        effect_estimate <-
          NA_real_
        
      }
      
      
      if (
        !is.null(
          test_result$conf.int
        ) &&
        length(
          test_result$conf.int
        ) == 2
      ) {
        
        ci_lower <-
          test_result$conf.int[1]
        
        ci_upper <-
          test_result$conf.int[2]
        
      } else {
        
        ci_lower <-
          NA_real_
        
        ci_upper <-
          NA_real_
        
      }
      
    }
    
    
    test_name <-
      "Mann-Whitney U test"
    
    
    statistic <-
      unname(
        test_result$statistic
      )
    
    
    p_value <-
      test_result$p.value
    
  }
  
  
  data.frame(
    
    Variable =
      variable,
    
    Variable_label =
      unname(
        variable_labels[
          variable
        ]
      ),
    
    N_CTR =
      n_ctr,
    
    N_GDM =
      n_gdm,
    
    Shapiro_P_CTR =
      shapiro_ctr,
    
    Shapiro_P_GDM =
      shapiro_gdm,
    
    Statistical_test =
      test_name,
    
    Test_statistic =
      statistic,
    
    Effect_estimate_GDM_minus_CTR =
      effect_estimate,
    
    CI95_lower =
      ci_lower,
    
    CI95_upper =
      ci_upper,
    
    P_value =
      p_value,
    
    Significance =
      significance_label(
        p_value
      )
    
  )
  
}


# ============================================================
# 13. RUN ALL CTR vs GDM TESTS
# ============================================================

group_comparison_results <- do.call(
  
  rbind,
  
  lapply(
    
    numeric_variables,
    
    function(variable) {
      
      compare_groups(
        clinical_data,
        variable
      )
      
    }
    
  )
  
)

print(
  group_comparison_results
)


# ============================================================
# 14. DESCRIPTIVE STATISTICS
# ============================================================

descriptive_statistics_long <- clinical_data %>%
  
  dplyr::select(
    Group,
    dplyr::all_of(
      numeric_variables
    )
  ) %>%
  
  tidyr::pivot_longer(
    
    cols =
      -Group,
    
    names_to =
      "Variable",
    
    values_to =
      "Value"
    
  ) %>%
  
  dplyr::group_by(
    Group,
    Variable
  ) %>%
  
  dplyr::summarise(
    
    N =
      sum(
        !is.na(Value)
      ),
    
    Mean =
      mean(
        Value,
        na.rm = TRUE
      ),
    
    SD =
      stats::sd(
        Value,
        na.rm = TRUE
      ),
    
    SEM = dplyr::if_else(
      
      N > 1,
      
      SD /
        sqrt(N),
      
      NA_real_
      
    ),
    
    Median =
      median(
        Value,
        na.rm = TRUE
      ),
    
    IQR =
      stats::IQR(
        Value,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
    
  ) %>%
  
  dplyr::mutate(
    
    Variable_label =
      unname(
        variable_labels[
          Variable
        ]
      )
    
  )


# ============================================================
# 15. DESCRIPTIVE STATISTICS - WIDE
# ============================================================

descriptive_statistics <- descriptive_statistics_long %>%
  
  dplyr::select(
    Variable,
    Variable_label,
    Group,
    N,
    Mean,
    SD,
    SEM,
    Median,
    IQR
  ) %>%
  
  tidyr::pivot_wider(
    
    names_from =
      Group,
    
    values_from = c(
      N,
      Mean,
      SD,
      SEM,
      Median,
      IQR
    ),
    
    names_glue =
      "{Group}_{.value}"
    
  )


# ============================================================
# 16. COMPLETE CLINICAL SUMMARY
# ============================================================

clinical_summary <-
  descriptive_statistics %>%
  
  dplyr::left_join(
    
    group_comparison_results %>%
      
      dplyr::select(
        -Variable_label
      ),
    
    by =
      "Variable"
    
  ) %>%
  
  dplyr::mutate(
    
    `CTR Mean ± SD` =
      paste0(
        round(
          CTR_Mean,
          2
        ),
        " ± ",
        round(
          CTR_SD,
          2
        )
      ),
    
    `CTR Mean ± SEM` =
      paste0(
        round(
          CTR_Mean,
          2
        ),
        " ± ",
        round(
          CTR_SEM,
          2
        )
      ),
    
    `CTR Median (IQR)` =
      paste0(
        round(
          CTR_Median,
          2
        ),
        " (",
        round(
          CTR_IQR,
          2
        ),
        ")"
      ),
    
    `GDM Mean ± SD` =
      paste0(
        round(
          GDM_Mean,
          2
        ),
        " ± ",
        round(
          GDM_SD,
          2
        )
      ),
    
    `GDM Mean ± SEM` =
      paste0(
        round(
          GDM_Mean,
          2
        ),
        " ± ",
        round(
          GDM_SEM,
          2
        )
      ),
    
    `GDM Median (IQR)` =
      paste0(
        round(
          GDM_Median,
          2
        ),
        " (",
        round(
          GDM_IQR,
          2
        ),
        ")"
      )
    
  ) %>%
  
  dplyr::select(
    
    Variable,
    
    Variable_label,
    
    CTR_N,
    
    GDM_N,
    
    `CTR Mean ± SD`,
    
    `CTR Mean ± SEM`,
    
    `CTR Median (IQR)`,
    
    `GDM Mean ± SD`,
    
    `GDM Mean ± SEM`,
    
    `GDM Median (IQR)`,
    
    Statistical_test,
    
    Test_statistic,
    
    Effect_estimate_GDM_minus_CTR,
    
    CI95_lower,
    
    CI95_upper,
    
    P_value,
    
    Significance,
    
    Shapiro_P_CTR,
    
    Shapiro_P_GDM,
    
    dplyr::everything()
    
  )


# ============================================================
# 17. NORMALITY TABLE
# ============================================================

normality_table <-
  group_comparison_results %>%
  
  dplyr::transmute(
    
    Variable,
    
    Variable_label,
    
    N_CTR,
    
    N_GDM,
    
    Shapiro_P_CTR,
    
    Shapiro_P_GDM,
    
    Normal_CTR =
      dplyr::case_when(
        
        is.na(
          Shapiro_P_CTR
        ) ~ "Not assessed",
        
        Shapiro_P_CTR >
          0.05 ~ "Yes",
        
        TRUE ~ "No"
        
      ),
    
    Normal_GDM =
      dplyr::case_when(
        
        is.na(
          Shapiro_P_GDM
        ) ~ "Not assessed",
        
        Shapiro_P_GDM >
          0.05 ~ "Yes",
        
        TRUE ~ "No"
        
      )
    
  )


# ============================================================
# 18. PARTICIPANT COUNTS
# ============================================================

participant_counts <-
  clinical_data %>%
  
  dplyr::count(
    Group,
    name =
      "N participants"
  )


# ============================================================
# 19. GRAPH SETTINGS
# ============================================================

graph_config <- list(
  
  Age = list(
    ylab = "Age (years)",
    ymin = 0,
    ymax = 50,
    breaks = seq(0, 50, 5),
    bar_base = 0,
    y_sig = 50
  ),
  
  Gestational_age_collection = list(
    ylab = "Gestational age at collection",
    ymin = 0,
    ymax = 45,
    breaks = seq(0, 45, 5),
    y_sig = 44
  ),
  
  Pregestational_weight = list(
    ylab = "Pre-gestational weight (kg)",
    ymin = 0,
    ymax = 100,
    breaks = seq(0, 100, 20),
    bar_base = 0,
    y_sig = 100
  ),
  
  Current_weight = list(
    ylab = "Weight (kg)",
    ymin = 0,
    ymax = 130,
    breaks = seq(0, 140, 20),
    bar_base = 0,
    y_sig = 130
  ),
  
  Height = list(
    ylab = "Height (m)",
    ymin = 1.00,
    ymax = 1.80,
    breaks = seq(1.00, 1.80, 0.10),
    bar_base = 1.00,
    y_sig = 1.75
  ),
  
  Pregestational_BMI = list(
    ylab = "Pre-gestational BMI (kg/m²)",
    ymin = 0,
    ymax = 50,
    breaks = seq(0, 50, 10),
    bar_base = 0,
    y_sig = 45 
  ),
  
  Current_BMI = list(
    ylab = "Current BMI (kg/m²)",
    ymin = 0,
    ymax = 50,
    breaks = seq(0, 50, 10),
    bar_base = 0,
    y_sig = 45 
  ),
  
  Weight_gain = list(
    ylab = "Weight gain (kg)",
    ymin = -5,
    ymax = 30,
    breaks = seq(-5, 30, 5),
    bar_base = -5,
    y_sig = 27
  ),
  
  Systolic_BP = list(
    ylab = "Systolic blood pressure (mmHg)",
    ymin = 0,
    ymax = 130L,
    breaks = seq(0, 130, 10),
    bar_base = 0,
    y_sig = 120
  ),
  
  Diastolic_BP = list(
    ylab = "Diastolic blood pressure (mmHg)",
    ymin = 0,
    ymax = 80,
    breaks = seq(0, 80, 10),
    bar_base = 0,
    y_sig = 75
  ),
  
  Fasting_glucose = list(
    ylab = "Fasting blood glucose (mg/dL)",
    ymin = 0,
    ymax = 140,
    breaks = seq(0, 140, 10),
    bar_base = 0,
    y_sig = 130
  ),
  
  GTT_1h = list(
    ylab = "GTT 1 h (mg/dL)",
    ymin = 0,
    ymax = 210,
    breaks = seq(0, 210, 20),
    bar_base = 0,
    y_sig = 202
  ),
  
  GTT_2h = list(
    ylab = "GTT 2 h (mg/dL)",
    ymin = 0,
    ymax = 200,
    breaks = seq(0, 200, 20),
    bar_base = 0,
    y_sig = 198
  )
  
)


# ============================================================
# 20. FUNCTION FOR CTR vs GDM FIGURES
#
# Bar = mean
# Error bar = SEM
# Black dots = individual observations
#
# No participant labels.
# ============================================================

create_group_figure <- function(
    df,
    variable,
    p_value,
    config
) {
  
  plot_data <-
    df %>%
    
    dplyr::select(
      
      Group,
      
      Value =
        dplyr::all_of(
          variable
        )
      
    ) %>%
    
    dplyr::filter(
      !is.na(Group),
      !is.na(Value)
    )
  
  
  summary_data <-
    plot_data %>%
    
    dplyr::group_by(
      Group
    ) %>%
    
    dplyr::summarise(
      
      N =
        dplyr::n(),
      
      Mean =
        mean(
          Value,
          na.rm = TRUE
        ),
      
      SD =
        stats::sd(
          Value,
          na.rm = TRUE
        ),
      
      SEM =
        SD /
        sqrt(N),
      
      Lower =
        Mean -
        SEM,
      
      Upper =
        Mean +
        SEM,
      
      .groups =
        "drop"
      
    )
  
  
  max_value <- max(
    c(
      plot_data$Value,
      summary_data$Upper
    ),
    na.rm = TRUE
  )
  
  
  min_value <- min(
    c(
      plot_data$Value,
      summary_data$Lower
    ),
    na.rm = TRUE
  )
  
  
  range_value <-
    max_value -
    min_value
  
  
  if (
    !is.finite(
      range_value
    ) ||
    range_value == 0
  ) {
    
    range_value <- 1
    
  }
  
  
  if (
    is.null(
      config$y_sig
    )
  ) {
    
    y_sig <-
      max_value +
      0.10 *
      range_value
    
  } else {
    
    y_sig <-
      config$y_sig
    
  }
  
  
  p <-
    ggplot2::ggplot(
      
      plot_data,
      
      ggplot2::aes(
        x = Group,
        y = Value
      )
      
    ) +
    
    ggplot2::geom_col(
      
      data =
        summary_data,
      
      ggplot2::aes(
        x = Group,
        y = Mean,
        fill = Group
      ),
      
      inherit.aes =
        FALSE,
      
      width =
        0.65,
      
      alpha =
        0.75,
      
      color =
        "black",
      
      linewidth =
        0.8
      
    ) +
    
    ggplot2::geom_errorbar(
      
      data =
        summary_data,
      
      ggplot2::aes(
        x = Group,
        ymin = Lower,
        ymax = Upper
      ),
      
      inherit.aes =
        FALSE,
      
      width =
        0.15,
      
      linewidth =
        0.9,
      
      color =
        "black"
      
    ) +
    
    ggplot2::geom_jitter(
      
      width =
        0.10,
      
      height =
        0,
      
      size =
        3,
      
      shape =
        16,
      
      color =
        "black"
      
    ) +
    
    ggplot2::scale_fill_manual(
      values =
        group_colors
    ) +
    
    ggplot2::labs(
      x = NULL,
      y = config$ylab
    ) +
    
    ggplot2::theme_classic(
      base_family =
        "Arial"
    ) +
    
    ggplot2::theme(
      
      legend.position =
        "none",
      
      axis.text =
        ggplot2::element_text(
          size = 16,
          color = "black"
        ),
      
      axis.title.y =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      
      axis.line =
        ggplot2::element_line(
          color = "black",
          linewidth = 0.8
        )
      
    )
  
  
  if (
    !is.null(
      config$breaks
    )
  ) {
    
    p <-
      p +
      
      ggplot2::scale_y_continuous(
        breaks =
          config$breaks
      )
    
  }
  
  
  if (
    !is.null(
      config$ymax
    )
  ) {
    
    p <-
      p +
      
      ggplot2::coord_cartesian(
        
        ylim = c(
          config$ymin,
          config$ymax
        ),
        
        clip =
          "off"
        
      )
    
  } else {
    
    p <-
      p +
      
      ggplot2::coord_cartesian(
        
        ylim = c(
          config$ymin,
          NA
        ),
        
        clip =
          "off"
        
      )
    
  }
  
  
  if (
    !is.na(
      p_value
    ) &&
    p_value < 0.05
  ) {
    
    sig <-
      significance_label(
        p_value
      )
    
    
    leg_height <-
      range_value *
      0.03
    
    
    p <-
      p +
      
      ggplot2::annotate(
        "segment",
        x = 1,
        xend = 2,
        y = y_sig,
        yend = y_sig,
        linewidth = 0.8
      ) +
      
      ggplot2::annotate(
        "segment",
        x = 1,
        xend = 1,
        y = y_sig,
        yend =
          y_sig -
          leg_height,
        linewidth = 0.8
      ) +
      
      ggplot2::annotate(
        "segment",
        x = 2,
        xend = 2,
        y = y_sig,
        yend =
          y_sig -
          leg_height,
        linewidth = 0.8
      ) +
      
      ggplot2::annotate(
        "text",
        x = 1.5,
        y =
          y_sig +
          0.04 *
          range_value,
        label = sig,
        size = 9,
        family = "Arial"
      )
    
  }
  
  
  return(p)
  
}


# ============================================================
# 21. SIMPLE FIGURE VARIABLES
#
# Newborn and placental weight are excluded here because
# their displayed P values come from adjusted models.
# ============================================================

simple_figure_variables <- setdiff(
  
  numeric_variables,
  
  c(
    "Newborn_weight_kg",
    "Placental_weight_g"
  )
  
)


# ============================================================
# 22. CREATE AND SAVE SIMPLE FIGURES
# ============================================================

clinical_figures <- list()


for (
  variable in
  simple_figure_variables
) {
  
  p_value <-
    group_comparison_results %>%
    
    dplyr::filter(
      Variable ==
        variable
    ) %>%
    
    dplyr::pull(
      P_value
    )
  
  
  fig <-
    create_group_figure(
      
      df =
        clinical_data,
      
      variable =
        variable,
      
      p_value =
        p_value,
      
      config =
        graph_config[[variable]]
       )
  
  clinical_figures[[variable]] <- fig
  
  
  clean_name <-
    stringr::str_replace_all(
      variable,
      "[^A-Za-z0-9]+",
      "_"
    ) %>%
    
    stringr::str_replace_all(
      "_$",
      ""
    )
  
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        figure_dir,
        paste0(
          clean_name,
          ".tiff"
        )
      ),
    
    plot =
      fig,
    
    width =
      4,
    
    height =
      5,
    
    dpi =
      600,
    
    compression =
      "lzw"
    
  )
  
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        figure_dir,
        paste0(
          clean_name,
          ".png"
        )
      ),
    
    plot =
      fig,
    
    width =
      4,
    
    height =
      5,
    
    dpi =
      600
    
  )
  
}


# ============================================================
# 23. DATA FOR ADJUSTED NEWBORN AND PLACENTAL WEIGHT MODELS
#
# These data are retained from the original analysis.
# ============================================================

weight_model_data <- data.frame(
  
  Group = c(
    "GDM","CTR","GDM","GDM","GDM","CTR","CTR","CTR",
    "GDM","GDM","CTR","CTR","GDM","CTR",
    "GDM","GDM","GDM","GDM","CTR","CTR","GDM","CTR",
    "CTR","GDM","CTR","CTR","GDM"
  ),
  
  Gestational_age = c(
    37,38,38,38,38,39,39,39,39,39,40,38,38,39,
    37,38,38,38,39,39,39,40,40,40,38,39,39
  ),
  
  Newborn_weight_kg = c(
    3.480,4.120,3.265,3.020,3.180,3.350,3.670,3.740,
    3.305,2.915,4.110,2.990,3.090,3.575,
    3.775,2.760,3.585,3.565,3.225,3.175,3.880,3.960,
    4.100,3.140,4.180,3.580,3.700
  ),
  
  Fetal_sex = c(
    "Female","Female","Female","Female","Female","Female","Female",
    "Female","Female","Female","Female","Female","Female","Female",
    "Male","Male","Male","Male","Male","Male","Male","Male","Male",
    "Male","Male","Male","Male"
  ),
  
  Placental_weight_g = c(
    585,645,755,460,665,505,735,740,625,615,700,580,505,770,
    565,560,765,600,630,535,740,685,870,410,885,675,505
  )
  
)


weight_model_data$Group <-
  factor(
    weight_model_data$Group,
    levels = c(
      "CTR",
      "GDM"
    )
  )


weight_model_data$Fetal_sex <-
  factor(
    weight_model_data$Fetal_sex,
    levels = c(
      "Female",
      "Male"
    )
  )


# ============================================================
# 24. ADJUSTED NEWBORN WEIGHT MODEL
# ============================================================

newborn_weight_model <-
  stats::lm(
    
    Newborn_weight_kg ~
      Group +
      Gestational_age +
      Fetal_sex,
    
    data =
      weight_model_data
    
  )


newborn_weight_anova <-
  car::Anova(
    
    newborn_weight_model,
    
    type =
      2
    
  )


newborn_weight_group_p <-
  newborn_weight_anova[
    "Group",
    "Pr(>F)"
  ]


# ============================================================
# 25. ADJUSTED PLACENTAL WEIGHT MODEL
# ============================================================

placental_weight_model <-
  stats::lm(
    
    Placental_weight_g ~
      Group +
      Gestational_age +
      Fetal_sex,
    
    data =
      weight_model_data
    
  )


placental_weight_anova <-
  car::Anova(
    
    placental_weight_model,
    
    type =
      2
    
  )


placental_weight_group_p <-
  placental_weight_anova[
    "Group",
    "Pr(>F)"
  ]


# ============================================================
# 26. MODEL COEFFICIENT TABLES WITH 95% CI
# ============================================================

newborn_weight_coefficients <-
  summary(
    newborn_weight_model
  )$coefficients %>%
  
  as.data.frame() %>%
  
  tibble::rownames_to_column(
    "Term"
  )


newborn_weight_ci <-
  stats::confint(
    newborn_weight_model
  ) %>%
  
  as.data.frame() %>%
  
  tibble::rownames_to_column(
    "Term"
  )


names(
  newborn_weight_ci
)[2:3] <- c(
  "CI95_lower",
  "CI95_upper"
)


newborn_weight_coefficients <-
  newborn_weight_coefficients %>%
  
  dplyr::left_join(
    newborn_weight_ci,
    by = "Term"
  )


placental_weight_coefficients <-
  summary(
    placental_weight_model
  )$coefficients %>%
  
  as.data.frame() %>%
  
  tibble::rownames_to_column(
    "Term"
  )


placental_weight_ci <-
  stats::confint(
    placental_weight_model
  ) %>%
  
  as.data.frame() %>%
  
  tibble::rownames_to_column(
    "Term"
  )


names(
  placental_weight_ci
)[2:3] <- c(
  "CI95_lower",
  "CI95_upper"
)


placental_weight_coefficients <-
  placental_weight_coefficients %>%
  
  dplyr::left_join(
    placental_weight_ci,
    by = "Term"
  )


# ============================================================
# 27. ANOVA TABLES
# ============================================================

newborn_weight_anova_table <-
  newborn_weight_anova %>%
  
  as.data.frame() %>%
  
  tibble::rownames_to_column(
    "Effect"
  )


placental_weight_anova_table <-
  placental_weight_anova %>%
  
  as.data.frame() %>%
  
  tibble::rownames_to_column(
    "Effect"
  )


# ============================================================
# 28. WEIGHT DESCRIPTIVE STATISTICS
# ============================================================

weight_descriptive_summary <-
  weight_model_data %>%
  
  dplyr::group_by(
    Group
  ) %>%
  
  dplyr::summarise(
    
    N =
      dplyr::n(),
    
    Newborn_weight_mean =
      mean(
        Newborn_weight_kg,
        na.rm = TRUE
      ),
    
    Newborn_weight_SD =
      stats::sd(
        Newborn_weight_kg,
        na.rm = TRUE
      ),
    
    Newborn_weight_SEM =
      Newborn_weight_SD /
      sqrt(N),
    
    Newborn_weight_median =
      median(
        Newborn_weight_kg,
        na.rm = TRUE
      ),
    
    Newborn_weight_IQR =
      stats::IQR(
        Newborn_weight_kg,
        na.rm = TRUE
      ),
    
    Placental_weight_mean =
      mean(
        Placental_weight_g,
        na.rm = TRUE
      ),
    
    Placental_weight_SD =
      stats::sd(
        Placental_weight_g,
        na.rm = TRUE
      ),
    
    Placental_weight_SEM =
      Placental_weight_SD /
      sqrt(N),
    
    Placental_weight_median =
      median(
        Placental_weight_g,
        na.rm = TRUE
      ),
    
    Placental_weight_IQR =
      stats::IQR(
        Placental_weight_g,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
    
  )


# ============================================================
# 29. WEIGHT FIGURE FUNCTION
# ============================================================

create_weight_figure <- function(
    df,
    variable,
    ylab,
    p_value,
    ymin = 0,
    ymax = NULL,
    breaks = NULL,
    y_sig = NULL
) {
  
  plot_data <-
    df %>%
    
    dplyr::select(
      Group,
      Value =
        dplyr::all_of(
          variable
        )
    ) %>%
    
    dplyr::filter(
      !is.na(Value)
    )
  
  
  summary_data <-
    plot_data %>%
    
    dplyr::group_by(
      Group
    ) %>%
    
    dplyr::summarise(
      
      N =
        dplyr::n(),
      
      Mean =
        mean(
          Value,
          na.rm = TRUE
        ),
      
      SD =
        stats::sd(
          Value,
          na.rm = TRUE
        ),
      
      SEM =
        SD /
        sqrt(N),
      
      Lower =
        Mean -
        SEM,
      
      Upper =
        Mean +
        SEM,
      
      .groups =
        "drop"
      
    )
  
  
  max_value <- max(
    c(
      plot_data$Value,
      summary_data$Upper
    ),
    na.rm = TRUE
  )
  
  
  min_value <- min(
    c(
      plot_data$Value,
      summary_data$Lower
    ),
    na.rm = TRUE
  )
  
  
  range_value <-
    max_value -
    min_value
  
  
  if (
    !is.finite(
      range_value
    ) ||
    range_value == 0
  ) {
    
    range_value <- 1
    
  }
  
  
  if (
    is.null(
      y_sig
    )
  ) {
    
    y_sig <-
      max_value +
      0.10 *
      range_value
    
  }
  
  
  p <-
    ggplot2::ggplot(
      
      plot_data,
      
      ggplot2::aes(
        x = Group,
        y = Value
      )
      
    ) +
    
    ggplot2::geom_col(
      
      data =
        summary_data,
      
      ggplot2::aes(
        x = Group,
        y = Mean,
        fill = Group
      ),
      
      inherit.aes =
        FALSE,
      
      width =
        0.65,
      
      alpha =
        0.75,
      
      color =
        "black",
      
      linewidth =
        0.8
      
    ) +
    
    ggplot2::geom_errorbar(
      
      data =
        summary_data,
      
      ggplot2::aes(
        x = Group,
        ymin = Lower,
        ymax = Upper
      ),
      
      inherit.aes =
        FALSE,
      
      width =
        0.15,
      
      linewidth =
        0.9,
      
      color =
        "black"
      
    ) +
    
    ggplot2::geom_jitter(
      
      width =
        0.10,
      
      height =
        0,
      
      size =
        3,
      
      shape =
        16,
      
      color =
        "black"
      
    ) +
    
    ggplot2::scale_fill_manual(
      values =
        group_colors
    ) +
    
    ggplot2::labs(
      x = NULL,
      y = ylab
    ) +
    
    ggplot2::theme_classic(
      base_family =
        "Arial"
    ) +
    
    ggplot2::theme(
      
      legend.position =
        "none",
      
      axis.text =
        ggplot2::element_text(
          size = 16,
          color = "black"
        ),
      
      axis.title.y =
        ggplot2::element_text(
          size = 18,
          color = "black"
        ),
      
      axis.line =
        ggplot2::element_line(
          linewidth = 0.8,
          color = "black"
        )
      
    )
  
  
  if (
    !is.null(
      breaks
    )
  ) {
    
    p <-
      p +
      
      ggplot2::scale_y_continuous(
        breaks =
          breaks
      )
    
  }
  
  
  if (
    !is.null(
      ymax
    )
  ) {
    
    p <-
      p +
      
      ggplot2::coord_cartesian(
        
        ylim = c(
          ymin,
          ymax
        ),
        
        clip =
          "off"
        
      )
    
  }
  
  
  if (
    !is.na(
      p_value
    ) &&
    p_value < 0.05
  ) {
    
    sig <-
      significance_label(
        p_value
      )
    
    
    leg_height <-
      range_value *
      0.03
    
    
    p <-
      p +
      
      ggplot2::annotate(
        "segment",
        x = 1,
        xend = 2,
        y = y_sig,
        yend = y_sig,
        linewidth = 0.8
      ) +
      
      ggplot2::annotate(
        "segment",
        x = 1,
        xend = 1,
        y = y_sig,
        yend =
          y_sig -
          leg_height,
        linewidth = 0.8
      ) +
      
      ggplot2::annotate(
        "segment",
        x = 2,
        xend = 2,
        y = y_sig,
        yend =
          y_sig -
          leg_height,
        linewidth = 0.8
      ) +
      
      ggplot2::annotate(
        "text",
        x = 1.5,
        y =
          y_sig +
          0.04 *
          range_value,
        label = sig,
        size = 9,
        family = "Arial"
      )
    
  }
  
  
  return(p)
  
}


# ============================================================
# 30. NEWBORN WEIGHT FIGURE
# ============================================================

newborn_weight_figure <-
  create_weight_figure(
    
    df =
      weight_model_data,
    
    variable =
      "Newborn_weight_kg",
    
    ylab =
      "Newborn weight (kg)",
    
    p_value =
      newborn_weight_group_p,
    
    ymin =
      0,
    
    ymax =
      5.2,
    
    breaks =
      seq(
        0,
        5,
        0.5
      ),
    
    y_sig =
      4.65
    
  )


# ============================================================
# 31. PLACENTAL WEIGHT FIGURE
# ============================================================

placental_weight_figure <-
  create_weight_figure(
    
    df =
      weight_model_data,
    
    variable =
      "Placental_weight_g",
    
    ylab =
      "Placental weight (g)",
    
    p_value =
      placental_weight_group_p,
    
    ymin =
      0,
    
    ymax =
      1000,
    
    breaks =
      seq(
        0,
        1000,
        250
      ),
    
    y_sig =
      900
    
  )


# ============================================================
# 32. SAVE WEIGHT FIGURES
# ============================================================

ggplot2::ggsave(
  
  filename =
    file.path(
      figure_dir,
      "Newborn_weight_adjusted.tiff"
    ),
  
  plot =
    newborn_weight_figure,
  
  width =
    4,
  
  height =
    5,
  
  dpi =
    600,
  
  compression =
    "lzw"
  
)


ggplot2::ggsave(
  
  filename =
    file.path(
      figure_dir,
      "Newborn_weight_adjusted.png"
    ),
  
  plot =
    newborn_weight_figure,
  
  width =
    4,
  
  height =
    5,
  
  dpi =
    600
  
)


ggplot2::ggsave(
  
  filename =
    file.path(
      figure_dir,
      "Placental_weight_adjusted.tiff"
    ),
  
  plot =
    placental_weight_figure,
  
  width =
    4,
  
  height =
    5,
  
  dpi =
    600,
  
  compression =
    "lzw"
  
)


ggplot2::ggsave(
  
  filename =
    file.path(
      figure_dir,
      "Placental_weight_adjusted.png"
    ),
  
  plot =
    placental_weight_figure,
  
  width =
    4,
  
  height =
    5,
  
  dpi =
    600
  
)


# ============================================================
# 33. ADJUSTED MODEL SUMMARY
# ============================================================

adjusted_weight_results <-
  data.frame(
    
    Outcome = c(
      "Newborn weight",
      "Placental weight"
    ),
    
    Adjustment = c(
      "Gestational age + fetal sex",
      "Gestational age + fetal sex"
    ),
    
    Group_P_value = c(
      newborn_weight_group_p,
      placental_weight_group_p
    ),
    
    Significance = c(
      significance_label(
        newborn_weight_group_p
      ),
      significance_label(
        placental_weight_group_p
      )
    )
    
  )


# ============================================================
# 34. MODEL DIAGNOSTICS
# ============================================================

png(
  
  filename =
    file.path(
      diagnostic_dir,
      "Newborn_weight_model_diagnostics.png"
    ),
  
  width =
    2400,
  
  height =
    2400,
  
  res =
    300
  
)

par(
  mfrow =
    c(
      2,
      2
    )
)

plot(
  newborn_weight_model
)

dev.off()


png(
  
  filename =
    file.path(
      diagnostic_dir,
      "Placental_weight_model_diagnostics.png"
    ),
  
  width =
    2400,
  
  height =
    2400,
  
  res =
    300
  
)

par(
  mfrow =
    c(
      2,
      2
    )
)

plot(
  placental_weight_model
)

dev.off()


par(
  mfrow =
    c(
      1,
      1
    )
)


# ============================================================
# 35. MAIN CLINICAL FIGURE - 3 x 3
# ============================================================
# MAIN CLINICAL FIGURE - 3 x 3 PANEL
#
# Panel order follows the Results section:
#
# A - Fasting blood glucose
# B - GTT 1 h
# C - GTT 2 h
# D - Pre-pregnancy BMI
# E - Pre-pregnancy weight
# F - Gestational weight gain
# G - Gestational age at sample collection
# H - Placental weight
# I - Newborn weight
# ============================================================

fasting_glucose_figure <-
  clinical_figures[["Fasting_glucose"]]

gtt_1h_figure <-
  clinical_figures[["GTT_1h"]]

gtt_2h_figure <-
  clinical_figures[["GTT_2h"]]

prepregnancy_bmi_figure <-
  clinical_figures[["Pregestational_BMI"]]

prepregnancy_weight_figure <-
  clinical_figures[["Pregestational_weight"]]

weight_gain_figure <-
  clinical_figures[["Weight_gain"]]

gestational_age_figure <-
  clinical_figures[["Gestational_age_collection"]]


# ------------------------------------------------------------
# Assemble Figure 1
# ------------------------------------------------------------

clinical_panel <- patchwork::wrap_plots(
  
  # A
  fasting_glucose_figure,
  
  # B
  gtt_1h_figure,
  
  # C
  gtt_2h_figure,
  
  # D
  prepregnancy_bmi_figure,
  
  # E
  prepregnancy_weight_figure,
  
  # F
  weight_gain_figure,
  
  # G
  gestational_age_figure,
  
  # H
  placental_weight_figure,
  
  # I
  newborn_weight_figure,
  
  ncol = 3
  
) +
  
  patchwork::plot_annotation(
    tag_levels = "A"
  ) &
  
  ggplot2::theme(
    
    plot.tag = ggplot2::element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black"
    )
    
  )


# Display panel
clinical_panel

# ============================================================
# SAVE FINAL CLINICAL PANEL
# ============================================================

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "Figure_1_Clinical_characteristics.tiff"
  ),
  plot = clinical_panel,
  width = 12,
  height = 13,
  dpi = 600,
  compression = "lzw"
)

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "Figure_1_Clinical_characteristics.png"
  ),
  plot = clinical_panel,
  width = 12,
  height = 13,
  dpi = 600
)

ggplot2::ggsave(
  filename = file.path(
    figure_dir,
    "Figure_1_Clinical_characteristics.pdf"
  ),
  plot = clinical_panel,
  width = 12,
  height = 13
)
# ============================================================
# 37. CREATE EXCEL WORKBOOK
# ============================================================

workbook <-
  openxlsx::createWorkbook()


header_style <-
  openxlsx::createStyle(
    
    textDecoration =
      "bold",
    
    halign =
      "center",
    
    valign =
      "center",
    
    border =
      "Bottom"
    
  )


# ============================================================
# 38. ADD EXCEL SHEETS
# ============================================================

sheet_data <- list(
  
  Clinical_summary =
    clinical_summary,
  
  Descriptive_statistics =
    descriptive_statistics,
  
  Group_comparisons =
    group_comparison_results,
  
  Normality =
    normality_table,
  
  Participant_counts =
    participant_counts,
  
  Adjusted_weight_summary =
    adjusted_weight_results,
  
  Weight_descriptive =
    weight_descriptive_summary,
  
  Newborn_weight_ANOVA =
    newborn_weight_anova_table,
  
  Newborn_weight_coeff =
    newborn_weight_coefficients,
  
  Placental_weight_ANOVA =
    placental_weight_anova_table,
  
  Placental_weight_coeff =
    placental_weight_coefficients,
  
  Weight_model_data =
    weight_model_data
  
)


for (
  sheet_name in
  names(
    sheet_data
  )
) {
  
  openxlsx::addWorksheet(
    workbook,
    sheet_name
  )
  
  
  openxlsx::writeData(
    workbook,
    sheet_name,
    sheet_data[[sheet_name]]
  )
  
  
  openxlsx::addStyle(
    
    workbook,
    
    sheet =
      sheet_name,
    
    style =
      header_style,
    
    rows =
      1,
    
    cols =
      seq_len(
        ncol(
          sheet_data[[sheet_name]]
        )
      ),
    
    gridExpand =
      TRUE
    
  )
  
  
  openxlsx::freezePane(
    
    workbook,
    
    sheet =
      sheet_name,
    
    firstRow =
      TRUE
    
  )
  
  
  openxlsx::setColWidths(
    
    workbook,
    
    sheet =
      sheet_name,
    
    cols =
      seq_len(
        ncol(
          sheet_data[[sheet_name]]
        )
      ),
    
    widths =
      "auto"
    
  )
  
}


# ============================================================
# 39. SAVE EXCEL WORKBOOK
# ============================================================

excel_file <-
  file.path(
    table_dir,
    "Clinical_analysis_complete.xlsx"
  )


openxlsx::saveWorkbook(
  
  workbook,
  
  excel_file,
  
  overwrite =
    TRUE
  
)


# ============================================================
# 40. SAVE CSV TABLES
# ============================================================

readr::write_csv(
  
  clinical_summary,
  
  file.path(
    table_dir,
    "Clinical_summary.csv"
  )
  
)


readr::write_csv(
  
  descriptive_statistics,
  
  file.path(
    table_dir,
    "Descriptive_statistics.csv"
  )
  
)


readr::write_csv(
  
  group_comparison_results,
  
  file.path(
    table_dir,
    "Group_comparisons.csv"
  )
  
)


readr::write_csv(
  
  normality_table,
  
  file.path(
    table_dir,
    "Normality_results.csv"
  )
  
)


readr::write_csv(
  
  adjusted_weight_results,
  
  file.path(
    table_dir,
    "Adjusted_weight_results.csv"
  )
  
)


readr::write_csv(
  
  weight_descriptive_summary,
  
  file.path(
    table_dir,
    "Weight_descriptive_statistics.csv"
  )
  
)


readr::write_csv(
  
  newborn_weight_anova_table,
  
  file.path(
    table_dir,
    "Newborn_weight_ANOVA.csv"
  )
  
)


readr::write_csv(
  
  newborn_weight_coefficients,
  
  file.path(
    table_dir,
    "Newborn_weight_coefficients.csv"
  )
  
)


readr::write_csv(
  
  placental_weight_anova_table,
  
  file.path(
    table_dir,
    "Placental_weight_ANOVA.csv"
  )
  
)


readr::write_csv(
  
  placental_weight_coefficients,
  
  file.path(
    table_dir,
    "Placental_weight_coefficients.csv"
  )
  
)


# ============================================================
# 41. SAVE SESSION INFORMATION
# ============================================================

capture.output(
  
  sessionInfo(),
  
  file =
    file.path(
      output_dir,
      "sessionInfo.txt"
    )
  
)


# ============================================================
# 42. FINAL CHECK
#
# This checks that no object created in the global environment
# still contains "birth_weight" in its name.
# ============================================================

old_birth_objects <-
  grep(
    "birth_weight",
    ls(),
    value = TRUE,
    ignore.case = TRUE
  )


if (
  length(
    old_birth_objects
  ) > 0
) {
  
  warning(
    paste(
      "Old birth_weight object names detected:",
      paste(
        old_birth_objects,
        collapse = ", "
      )
    )
  )
  
}


# ============================================================
# 43. FINAL CONSOLE MESSAGE
# ============================================================

cat(
  "\nClinical analysis completed successfully.\n"
)

cat(
  "\nMain output folder:\n",
  output_dir,
  "\n"
)

cat(
  "\nFigures:\n",
  figure_dir,
  "\n"
)

cat(
  "\nTables:\n",
  table_dir,
  "\n"
)

cat(
  "\nModel diagnostics:\n",
  diagnostic_dir,
  "\n"
)

cat(
  "\nExcel workbook:\n",
  excel_file,
  "\n"
)


# ============================================================
# END OF SCRIPT
# ============================================================