# ============================================================
# FLOW CYTOMETRY ANALYSIS
# Updated: 2026-09-29
#
# Statistical workflow:
# - CTR vs GDM: two-sided Welch t-test or Mann-Whitney test within each compartment; nominal p-values.
# - Global effects: linear mixed-effects model, Value ~ Compartment * Group + (1 | Code).
# - Compartment pairwise comparisons within each group: emmeans with Benjamini-Hochberg correction.
# - Figures: mean +/- SEM plus individual observations.

# ============================================================

packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "ggplot2",
  "lme4",
  "lmerTest",
  "emmeans",
  "openxlsx",
  "svglite",
  "officer",
  "rvg"
)


missing_packages <- packages[
  !packages %in% rownames(
    installed.packages()
  )
]


if (length(missing_packages) > 0) {
  
  install.packages(
    missing_packages
  )
  
}


library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(openxlsx)
library(svglite)
library(officer)
library(rvg)


set.seed(123)


options(
  contrasts = c(
    "contr.sum",
    "contr.poly"
  )
)


setwd(".")


input_file <- #"Analysis citometro.xls"


results_dir <-
  "Flow_Cytometry_Results_FINAL"


png_dir <- file.path(
  results_dir,
  "Figures_PNG"
)


tiff_dir <- file.path(
  results_dir,
  "Figures_TIFF"
)


svg_dir <- file.path(
  results_dir,
  "Figures_Editable_SVG"
)


dir.create(
  results_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  png_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  tiff_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  svg_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


group_colors <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)


raw_import <- readxl::read_excel(
  input_file,
  sheet = 1,
  col_names = TRUE,
  .name_repair = "unique"
)


flow_columns <- names(
  raw_import
)[3:ncol(raw_import)]


column_map <- tibble::tibble(
  
  Column = flow_columns,
  
  Marker = as.character(
    unlist(
      raw_import[
        1,
        flow_columns
      ]
    )
  )
  
) %>%
  
  dplyr::mutate(
    
    Compartment = dplyr::case_when(
      
      stringr::str_detect(
        Column,
        "^Maternal blood"
      ) ~ "Maternal blood",
      
      stringr::str_detect(
        Column,
        "^Umbilical cord blood"
      ) ~ "Umbilical cord blood",
      
      stringr::str_detect(
        Column,
        "^Decidual tissue"
      ) ~ "Decidual tissue",
      
      TRUE ~ NA_character_
      
    ),
    
    Marker = stringr::str_trim(
      Marker
    )
    
  )


print(column_map)


raw_data <- raw_import[
  -1,
]


raw_data <- raw_data %>%
  
  dplyr::mutate(
    
    Code = trimws(
      as.character(Code)
    ),
    
    Group = trimws(
      as.character(
        `Sample type`
      )
    ),
    
    Group = stringr::str_to_upper(
      Group
    )
    
  ) %>%
  
  dplyr::filter(
    
    !is.na(Code),
    
    Group %in% c(
      "CTR",
      "GDM"
    )
    
  ) %>%
  
  dplyr::mutate(
    
    Group = factor(
      Group,
      levels = c(
        "CTR",
        "GDM"
      )
    ),
    
    Code = factor(
      Code
    )
    
  )


long_data <- raw_data %>%
  
  tidyr::pivot_longer(
    
    cols = dplyr::all_of(
      flow_columns
    ),
    
    names_to = "Column",
    
    values_to = "Value"
    
  ) %>%
  
  dplyr::left_join(
    column_map,
    by = "Column"
  )


long_data <- long_data %>%
  
  dplyr::mutate(
    
    Value = as.character(
      Value
    ),
    
    Value = stringr::str_trim(
      Value
    ),
    
    Value = dplyr::na_if(
      Value,
      ""
    ),
    
    Value = dplyr::na_if(
      Value,
      "-"
    ),
    
    Value = dplyr::na_if(
      Value,
      "NA"
    ),
    
    Value = dplyr::na_if(
      Value,
      "Undetermined"
    ),
    
    Value = stringr::str_replace_all(
      Value,
      ",",
      "."
    ),
    
    Value = suppressWarnings(
      as.numeric(
        Value
      )
    ),
    
    Marker = stringr::str_trim(
      Marker
    ),
    
    Compartment = factor(
      Compartment,
      levels = c(
        "Maternal blood",
        "Umbilical cord blood",
        "Decidual tissue"
      )
    ),
    
    Group = factor(
      Group,
      levels = c(
        "CTR",
        "GDM"
      )
    )
    
  )


markers <- long_data %>%
  
  dplyr::filter(
    !is.na(Marker)
  ) %>%
  
  dplyr::distinct(
    Marker
  ) %>%
  
  dplyr::pull(
    Marker
  )


print(markers)


compartments <- c(
  "Maternal blood",
  "Umbilical cord blood",
  "Decidual tissue"
)


sample_size_table <- long_data %>%
  
  dplyr::filter(
    !is.na(Value)
  ) %>%
  
  dplyr::count(
    
    Marker,
    Compartment,
    Group,
    
    name = "n"
    
  )


descriptive_statistics <- long_data %>%
  
  dplyr::filter(
    !is.na(Value)
  ) %>%
  
  dplyr::group_by(
    
    Marker,
    Compartment,
    Group
    
  ) %>%
  
  dplyr::summarise(
    
    n = dplyr::n(),
    
    mean_value = mean(
      Value,
      na.rm = TRUE
    ),
    
    sd = stats::sd(
      Value,
      na.rm = TRUE
    ),
    
    mean_valuena = mean_valuen(
      Value,
      na.rm = TRUE
    ),
    
    IQR = stats::IQR(
      Value,
      na.rm = TRUE
    ),
    
    minimum = min(
      Value,
      na.rm = TRUE
    ),
    
    maximum = max(
      Value,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  )


significance_label <- function(p) {
  
  dplyr::case_when(
    
    is.na(p) ~ "",
    
    p < 0.05 ~ "*",
    
    TRUE ~ "ns"
    
  )
  
}


compare_ctr_gdm <- function(
    df,
    marker,
    compartment
) {
  
  marker_data <- df %>%
    
    dplyr::filter(
      
      Marker == marker,
      
      Compartment == compartment,
      
      !is.na(Value),
      
      !is.na(Group)
      
    )
  
  
  
  ctr <- marker_data %>%
    
    dplyr::filter(
      Group == "CTR"
    ) %>%
    
    dplyr::pull(
      Value
    )
  
  
  gdm <- marker_data %>%
    
    dplyr::filter(
      Group == "GDM"
    ) %>%
    
    dplyr::pull(
      Value
    )
  
  
  n_ctr <- length(
    ctr
  )
  
  
  n_gdm <- length(
    gdm
  )
  
  
  
  if (
    n_ctr < 2 ||
    n_gdm < 2
  ) {
    
    return(
      
      tibble::tibble(
        
        Marker = marker,
        
        Compartment = compartment,
        
        n_CTR = n_ctr,
        
        n_GDM = n_gdm,
        
        Mean_CTR = ifelse(
          n_ctr > 0,
          mean(ctr),
          NA_real_
        ),
        
        Mean_GDM = ifelse(
          n_gdm > 0,
          mean(gdm),
          NA_real_
        ),
        
        Median_CTR = ifelse(
          n_ctr > 0,
          mean_valuen(ctr),
          NA_real_
        ),
        
        Median_GDM = ifelse(
          n_gdm > 0,
          mean_valuen(gdm),
          NA_real_
        ),
        
        Shapiro_CTR_p =
          NA_real_,
        
        Shapiro_GDM_p =
          NA_real_,
        
        Test =
          "Insufficient sample size",
        
        Statistic =
          NA_real_,
        
        p_value =
          NA_real_,
        
        Significance =
          ""
        
      )
      
    )
    
  }
  
  
  
  shapiro_ctr <- if (
    n_ctr >= 3 &&
    length(unique(ctr)) > 1
  ) {
    
    stats::shapiro.test(
      ctr
    )$p.value
    
  } else {
    
    NA_real_
    
  }
  
  
  
  shapiro_gdm <- if (
    n_gdm >= 3 &&
    length(unique(gdm)) > 1
  ) {
    
    stats::shapiro.test(
      gdm
    )$p.value
    
  } else {
    
    NA_real_
    
  }
  
  
  
  normal <- !is.na(
    shapiro_ctr
  ) &&
    !is.na(
      shapiro_gdm
    ) &&
    shapiro_ctr > 0.05 &&
    shapiro_gdm > 0.05
  
  
  
  if (normal) {
    
    
    test_result <- stats::t.test(
      
      ctr,
      
      gdm,
      
      alternative =
        "two.sided",
      
      var.equal =
        FALSE
      
    )
    
    
    nome_test_result <-
      "Welch t-test"
    
    
    statistic_value <-
      as.numeric(
        test_result$statistic
      )
    
    
  } else {
    
    
    test_result <- stats::wilcox.test(
      
      ctr,
      
      gdm,
      
      alternative =
        "two.sided",
      
      exact =
        FALSE
      
    )
    
    
    nome_test_result <-
      "Mann-Whitney"
    
    
    statistic_value <-
      as.numeric(
        test_result$statistic
      )
    
  }
  
  
  
  tibble::tibble(
    
    Marker =
      marker,
    
    Compartment =
      compartment,
    
    n_CTR =
      n_ctr,
    
    n_GDM =
      n_gdm,
    
    Mean_CTR =
      mean(
        ctr,
        na.rm = TRUE
      ),
    
    Mean_GDM =
      mean(
        gdm,
        na.rm = TRUE
      ),
    
    Median_CTR =
      mean_valuen(
        ctr,
        na.rm = TRUE
      ),
    
    Median_GDM =
      mean_valuen(
        gdm,
        na.rm = TRUE
      ),
    
    Shapiro_CTR_p =
      shapiro_ctr,
    
    Shapiro_GDM_p =
      shapiro_gdm,
    
    Test =
      nome_test_result,
    
    Statistic =
      statistic_value,
    
    p_value =
      test_result$p.value,
    
    Significance =
      significance_label(
        test_result$p.value
      )
    
  )
  
}


between_group_results <- purrr::map_dfr(
  
  compartments,
  
  function(comp) {
    
    purrr::map_dfr(
      
      markers,
      
      function(m) {
        
        compare_ctr_gdm(
          
          df = long_data,
          
          marker = m,
          
          compartment = comp
          
        )
        
      }
      
    )
    
  }
  
)


between_group_results <- between_group_results %>%
  
  dplyr::mutate(
    
    Direction = dplyr::case_when(
      
      Mean_CTR >
        Mean_GDM ~
        "CTR > GDM",
      
      Mean_GDM >
        Mean_CTR ~
        "GDM > CTR",
      
      TRUE ~
        "CTR = GDM"
      
    )
    
  ) %>%
  
  dplyr::arrange(
    Compartment,
    Marker
  )


significant_between_group <-
  between_group_results %>%
  
  dplyr::filter(
    p_value < 0.05
  )


fit_mixed_model <- function(
    df,
    marker
) {
  
  marker_data <- df %>%
    
    dplyr::filter(
      
      Marker ==
        marker,
      
      !is.na(
        Value
      ),
      
      !is.na(
        Group
      ),
      
      !is.na(
        Compartment
      ),
      
      !is.na(
        Code
      )
      
    ) %>%
    
    droplevels()
  
  
  
  if (
    nrow(marker_data) < 6 ||
    dplyr::n_distinct(
      marker_data$Group
    ) < 2 ||
    dplyr::n_distinct(
      marker_data$Compartment
    ) < 2
  ) {
    
    return(
      NULL
    )
    
  }
  
  
  
  model <- lmerTest::lmer(
    
    Value ~
      Compartment *
      Group +
      (1 | Code),
    
    data =
      marker_data,
    
    REML =
      TRUE
    
  )
  
  
  
  is_singular <- lme4::isSingular(
    
    model,
    
    tol = 1e-4
    
  )
  
  
  
  mixed_model_anova <- anova(
    
    model,
    
    type = 3
    
  ) %>%
    
    as.data.frame() %>%
    
    tibble::rownames_to_column(
      "Effect"
    ) %>%
    
    dplyr::mutate(
      Marker =
        marker
    )
  
  
  
  emm_comp <- emmeans::emmeans(
    
    model,
    
    ~ Compartment |
      Group
    
  )
  
  
  posthoc_comp <- pairs(
    
    emm_comp,
    
    adjust =
      "none"
    
  ) %>%
    
    as.data.frame() %>%
    
    dplyr::group_by(
      Group
    ) %>%
    
    dplyr::mutate(
      
      p_BH =
        stats::p.adjust(
          p.value,
          method = "BH"
        ),
      
      BH_significance =
        significance_label(
          p_BH
        )
      
    ) %>%
    
    dplyr::ungroup() %>%
    
    dplyr::mutate(
      Marker =
        marker
    )
  
  
  
  mean_values_marginais <-
    emmeans::emmeans(
      
      model,
      
      ~ Group *
        Compartment
      
    ) %>%
    
    as.data.frame() %>%
    
    dplyr::mutate(
      Marker =
        marker
    )
  
  
  
  list(
    
    model =
      model,
    
    anova =
      mixed_model_anova,
    
    posthoc_comp =
      posthoc_comp,
    
    emmeans =
      mean_values_marginais,
    
    is_singular =
      is_singular
    
  )
  
}


model_results <- purrr::map(
  
  markers,
  
  function(m) {
    
    tryCatch(
      
      fit_mixed_model(
        
        df = long_data,
        
        marker = m
        
      ),
      
      error = function(e) {
        
        message(
          
          "\nERRO em ",
          
          m,
          
          ": ",
          
          e$message
          
        )
        
        
        return(
          NULL
        )
        
      }
      
    )
    
  }
  
)


names(
  model_results
) <- markers


valid_model_results <-
  model_results[
    
    !vapply(
      
      model_results,
      
      is.null,
      
      logical(1)
      
    )
    
  ]


mixed_model_anova <- purrr::map_dfr(
  
  valid_model_results,
  
  "anova"
  
)


compartment_posthoc <-
  purrr::map_dfr(
    
    valid_model_results,
    
    "posthoc_comp"
    
  )


estimated_marginal_means <- purrr::map_dfr(
  
  valid_model_results,
  
  "emmeans"
  
)


significant_compartment_posthoc <-
  compartment_posthoc %>%
  
  dplyr::filter(
    p_BH < 0.05
  )


model_diagnostics <-
  purrr::imap_dfr(
    
    valid_model_results,
    
    function(
    result,
    marker
    ) {
      
      vc <- as.data.frame(
        
        lme4::VarCorr(
          result$model
        )
        
      )
      
      
      tibble::tibble(
        
        Marker =
          marker,
        
        Singular =
          result$is_singular,
        
        Code_variance =
          ifelse(
            nrow(vc) > 0,
            vc$vcov[1],
            NA_real_
          )
        
      )
      
    }
    
  )


compartment_labels <- c(
  
  "Maternal blood" =
    "Maternal\nblood",
  
  "Umbilical cord blood" =
    "Umbilical cord\nblood",
  
  "Decidual tissue" =
    "Decidual\ntissue"
  
)


plot_cfg <- list()


get_plot_cfg <- function(
    marker
) {
  
  cfg <-
    plot_cfg[[marker]]
  if (
    is.null(
      cfg
    )
  ) {
    
    cfg <- list(
      
      ymin = 0,
      
      ymax = NULL,
      
      breaks = NULL
      
    )
    
  }
  
  
  cfg
  
}


split_compartment_contrast <-
  function(
    contrast_text
  ) {
    
    parts <-
      stringr::str_split_fixed(
        
        contrast_text,
        
        " - ",
        
        2
        
      )
    
    
    tibble::tibble(
      
      Compartment1 =
        parts[, 1],
      
      Compartment2 =
        parts[, 2]
      
    )
    
  }


create_final_plot <- function(
    df,
    marker
) {
  
  
  
  plot_data <- df %>%
    
    dplyr::filter(
      
      Marker ==
        marker,
      
      !is.na(
        Value
      ),
      
      !is.na(
        Group
      ),
      
      !is.na(
        Compartment
      )
      
    )
  
  
  
  
  summary_stats <- plot_data %>%
    
    dplyr::group_by(
      Compartment,
      Group
    ) %>%
    
    dplyr::summarise(
      
      n = sum(!is.na(Value)),
      
      mean_value = mean(
        Value,
        na.rm = TRUE
      ),
      
      sd = stats::sd(
        Value,
        na.rm = TRUE
      ),
      
      sem = sd / sqrt(n),
      
      .groups = "drop"
      
    ) %>%
    
    dplyr::mutate(
      
      sem = dplyr::if_else(
        is.na(sem),
        0,
        sem
      )
      
    )
  
  
  ydata_max <- max(
    
    c(
      
      plot_data$Value,
      
      summary_stats$mean_value +
        summary_stats$sem
      
    ),
    
    na.rm = TRUE
    
  )
  
  if (
    !is.finite(ydata_max) ||
    ydata_max <= 0
  ) {
    
    ydata_max <- 1
    
  }
  
  spacing <- ydata_max * 0.10
  
  
  group_stars <-
    between_group_results %>%
    
    dplyr::filter(
      
      Marker ==
        marker,
      
      !is.na(
        p_value
      ),
      
      p_value < 0.05
      
    ) %>%
    
    dplyr::mutate(
      
      Compartment =
        factor(
          
          Compartment,
          
          levels = c(
            "Maternal blood",
            "Umbilical cord blood",
            "Decidual tissue"
          )
          
        ),
      
      x_position =
        as.numeric(
          Compartment
        ),
      
      Significance =
        "*"
      
    )
  
  
  
  observed_top <- plot_data %>%
    
    dplyr::group_by(
      Compartment
    ) %>%
    
    dplyr::summarise(
      
      observed_max =
        max(
          Value,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
      
    )
  
  
  top_value_summary_stats <- summary_stats %>%
    
    dplyr::group_by(
      Compartment
    ) %>%
    
    dplyr::summarise(
      
      max_summary_stats =
        max(
          mean_value + sem,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
      
    )
  
  
  compartment_top <- observed_top %>%
    
    dplyr::left_join(
      
      top_value_summary_stats,
      
      by =
        "Compartment"
      
    ) %>%
    
    dplyr::mutate(
      
      top_value =
        pmax(
          
          observed_max,
          
          max_summary_stats,
          
          na.rm = TRUE
          
        )
      
    )
  
  
  group_stars <-
    group_stars %>%
    
    dplyr::left_join(
      
      compartment_top,
      
      by =
        "Compartment"
      
    ) %>%
    
    dplyr::mutate(
      
      y_bracket =
        top_value +
        spacing * 0.40,
      
      y_star =
        top_value +
        spacing * 0.62
      
    )
  
  
  
  significant_compartment <-
    compartment_posthoc %>%
    
    dplyr::filter(
      
      Marker ==
        marker,
      
      !is.na(
        p_BH
      ),
      
      p_BH < 0.05
      
    )
  
  
  
  if (
    nrow(
      significant_compartment
    ) > 0
  ) {
    
    
    parts <-
      split_compartment_contrast(
        
        significant_compartment$contrast
        
      )
    
    
    significant_compartment <-
      dplyr::bind_cols(
        
        significant_compartment,
        
        parts
        
      )
    
    
    positions <- c(
      
      "Maternal blood" = 1,
      
      "Umbilical cord blood" = 2,
      
      "Decidual tissue" = 3
      
    )
    
    
    significant_compartment <-
      significant_compartment %>%
      
      dplyr::mutate(
        
        x1_base =
          unname(
            positions[
              Compartment1
            ]
          ),
        
        x2_base =
          unname(
            positions[
              Compartment2
            ]
          ),
        
        offset =
          dplyr::if_else(
            
            Group ==
              "CTR",
            
            -0.19,
            
            0.19
            
          ),
        
        x1 =
          x1_base +
          offset,
        
        x2 =
          x2_base +
          offset,
        
        span =
          abs(
            x2_base -
              x1_base
          )
        
      ) %>%
      
      
      dplyr::arrange(
        
        span,
        Group,
        x1
        
      ) %>%
      
      dplyr::mutate(
        
        order_index =
          dplyr::row_number(),
        
        y_bracket =
          ydata_max +
          spacing *
          (
            1.20 +
              (
                order_index -
                  1
              ) *
              0.55
          ),
        
        y_star =
          y_bracket +
          spacing * 0.22,
        
        Significance =
          "*"
        
      )
    
  }
  
  
  
  p <- ggplot2::ggplot(
    
    plot_data,
    
    ggplot2::aes(
      
      x =
        Compartment,
      
      y =
        Value,
      
      fill =
        Group
      
    )
    
  ) +
    
    
  
  ggplot2::geom_col(
    
    data =
      summary_stats,
    
    ggplot2::aes(
      
      x =
        Compartment,
      
      y =
        mean_value,
      
      fill =
        Group
      
    ),
    
    inherit.aes =
      FALSE,
    
    position =
      ggplot2::position_dodge(
        width = 0.75
      ),
    
    width =
      0.65,
    
    alpha =
      0.75,
    
    color =
      "black",
    
    linewidth =
      0.7
    
  ) +
    
    
  
  ggplot2::geom_errorbar(
    
    data = summary_stats,
    
    ggplot2::aes(
      
      x = Compartment,
      
      ymin = mean_value - sem,
      
      ymax = mean_value + sem,
      
      group = Group
      
    ),
    
    inherit.aes = FALSE,
    
    position = ggplot2::position_dodge(
      width = 0.75
    ),
    
    width = 0.15,
    
    linewidth = 0.8,
    
    color = "black"
    
  ) +
    
  
  ggplot2::geom_point(
    
    ggplot2::aes(
      group =
        Group
    ),
    
    position =
      ggplot2::position_jitterdodge(
        
        jitter.width =
          0.08,
        
        dodge.width =
          0.75
        
      ),
    
    size =
      2.7,
    
    shape =
      16,
    
    color =
      "black"
    
  ) +
    
    
  
  ggplot2::scale_fill_manual(
    values =
      group_colors
  ) +
    
    
  
  ggplot2::scale_x_discrete(
    labels =
      compartment_labels
  ) +
    
    
  
  ggplot2::labs(
    
    x =
      NULL,
    
    y =
      "Frequency (%)",
    
    fill =
      NULL
    
  ) +
    
    
  
  ggplot2::theme_classic(
    base_family =
      "Arial"
  ) +
    
    
    ggplot2::theme(
      
      legend.position =
        "top",
      
      legend.title =
        ggplot2::element_blank(),
      
      legend.text =
        ggplot2::element_text(
          size = 13
        ),
      
      axis.text.x =
        ggplot2::element_text(
          
          size = 13,
          
          color =
            "black"
          
        ),
      
      axis.text.y =
        ggplot2::element_text(
          
          size = 14,
          
          color =
            "black"
          
        ),
      
      axis.title.y =
        ggplot2::element_text(
          
          size = 16,
          
          color =
            "black"
          
        ),
      
      axis.line =
        ggplot2::element_line(
          
          linewidth = 0.8,
          
          color =
            "black"
          
        ),
      
      axis.ticks =
        ggplot2::element_line(
          
          color =
            "black"
          
        ),
      
      plot.margin =
        ggplot2::margin(
          
          20,
          20,
          10,
          10
          
        )
      
    )
  
  
  
  if (
    nrow(
      group_stars
    ) > 0
  ) {
    
    
    
    p <- p +
      
      ggplot2::geom_segment(
        
        data =
          group_stars,
        
        ggplot2::aes(
          
          x =
            x_position - 0.19,
          
          xend =
            x_position + 0.19,
          
          y =
            y_bracket,
          
          yend =
            y_bracket
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      
      ggplot2::geom_segment(
        
        data =
          group_stars,
        
        ggplot2::aes(
          
          x =
            x_position - 0.19,
          
          xend =
            x_position - 0.19,
          
          y =
            y_bracket,
          
          yend =
            y_bracket -
            spacing * 0.10
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      
      ggplot2::geom_segment(
        
        data =
          group_stars,
        
        ggplot2::aes(
          
          x =
            x_position + 0.19,
          
          xend =
            x_position + 0.19,
          
          y =
            y_bracket,
          
          yend =
            y_bracket -
            spacing * 0.10
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      
      ggplot2::geom_text(
        
        data =
          group_stars,
        
        ggplot2::aes(
          
          x =
            x_position,
          
          y =
            y_star,
          
          label =
            Significance
          
        ),
        
        inherit.aes =
          FALSE,
        
        family =
          "Arial",
        
        fontface =
          "bold",
        
        size =
          5.5
        
      )
    
  }
  
  
  
  if (
    nrow(
      significant_compartment
    ) > 0
  ) {
    
    
    
    p <- p +
      
      ggplot2::geom_segment(
        
        data =
          significant_compartment,
        
        ggplot2::aes(
          
          x =
            x1,
          
          xend =
            x2,
          
          y =
            y_bracket,
          
          yend =
            y_bracket
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      
      ggplot2::geom_segment(
        
        data =
          significant_compartment,
        
        ggplot2::aes(
          
          x =
            x1,
          
          xend =
            x1,
          
          y =
            y_bracket,
          
          yend =
            y_bracket -
            spacing * 0.12
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      
      ggplot2::geom_segment(
        
        data =
          significant_compartment,
        
        ggplot2::aes(
          
          x =
            x2,
          
          xend =
            x2,
          
          y =
            y_bracket,
          
          yend =
            y_bracket -
            spacing * 0.12
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      
      ggplot2::geom_text(
        
        data =
          significant_compartment,
        
        ggplot2::aes(
          
          x =
            (
              x1 +
                x2
            ) / 2,
          
          y =
            y_star,
          
          label =
            Significance
          
        ),
        
        inherit.aes =
          FALSE,
        
        family =
          "Arial",
        
        fontface =
          "bold",
        
        size =
          5.5
        
      )
    
  }
  
  
  
  highest_annotation <-
    ydata_max
  
  
  if (
    nrow(
      group_stars
    ) > 0
  ) {
    
    highest_annotation <- max(
      
      highest_annotation,
      
      group_stars$y_star,
      
      na.rm = TRUE
      
    )
    
  }
  
  
  if (
    nrow(
      significant_compartment
    ) > 0
  ) {
    
    highest_annotation <- max(
      
      highest_annotation,
      
      significant_compartment$y_star,
      
      na.rm = TRUE
      
    )
    
  }
  
  
  
  cfg <-
    get_plot_cfg(
      marker
    )
  
  
  
  if (
    is.null(
      cfg$breaks
    )
  ) {
    
    p <- p +
      
      ggplot2::scale_y_continuous(
        
        expand =
          ggplot2::expansion(
            
            mult = c(
              0,
              0.02
            )
            
          )
        
      )
    
    
  } else {
    
    p <- p +
      
      ggplot2::scale_y_continuous(
        
        breaks =
          cfg$breaks,
        
        expand =
          ggplot2::expansion(
            
            mult = c(
              0,
              0.02
            )
            
          )
        
      )
    
  }
  
  
  
  if (
    is.null(
      cfg$ymax
    )
  ) {
    
    upper_limit <-
      highest_annotation +
      spacing * 0.50
    
    
  } else {
    
    upper_limit <-
      max(
        
        cfg$ymax,
        
        highest_annotation +
          spacing * 0.30
        
      )
    
  }
  
  
  p <- p +
    
    ggplot2::coord_cartesian(
      
      ylim = c(
        
        cfg$ymin,
        
        upper_limit
        
      ),
      
      clip =
        "off"
      
    )
  
  
  return(
    p
  )
  
}


editable_ppt <-
  officer::read_pptx()


for (
  m in markers
) {
  
  
  n_values <- long_data %>%
    
    dplyr::filter(
      
      Marker ==
        m,
      
      !is.na(
        Value
      )
      
    ) %>%
    
    nrow()
  
  
  if (
    n_values == 0
  ) {
    
    next
    
  }
  
  
  
  plot_obj <- create_final_plot(
    
    df =
      long_data,
    
    marker =
      m
    
  )
  
  
  
  nome_input_file <- m %>%
    
    stringr::str_replace_all(
      
      "\\+",
      
      "pos"
      
    ) %>%
    
    stringr::str_replace_all(
      
      "[^A-Za-z0-9]+",
      
      "_"
      
    ) %>%
    
    stringr::str_remove(
      "_$"
    )
  
  
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        
        png_dir,
        
        paste0(
          nome_input_file,
          ".png"
        )
        
      ),
    
    plot =
      plot_obj,
    
    width =
      7,
    
    height =
      5.5,
    
    dpi =
      600
    
  )
  
  
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        
        tiff_dir,
        
        paste0(
          nome_input_file,
          ".tiff"
        )
        
      ),
    
    plot =
      plot_obj,
    
    width =
      7,
    
    height =
      5.5,
    
    dpi =
      600,
    
    compression =
      "lzw"
    
  )
  
  
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        
        svg_dir,
        
        paste0(
          nome_input_file,
          ".svg"
        )
        
      ),
    
    plot =
      plot_obj,
    
    width =
      7,
    
    height =
      5.5,
    
    device =
      svglite::svglite
    
  )
  
  
  
  editable_ppt <-
    editable_ppt %>%
    
    officer::add_slide(
      
      layout =
        "Blank",
      
      master =
        "Office Theme"
      
    ) %>%
    
    officer::ph_with(
      
      value =
        rvg::dml(
          ggobj =
            plot_obj
        ),
      
      location =
        officer::ph_location(
          
          left =
            0.5,
          
          top =
            0.5,
          
          width =
            9,
          
          height =
            6.8
          
        )
      
    )
  
}


ppt_file <- file.path(
  
  results_dir,
  
  "Flow_Cytometry_Figures_Editable.pptx"
  
)


print(
  
  editable_ppt,
  
  target =
    ppt_file
  
)


excel_file <- file.path(
  
  results_dir,
  
  "Flow_Cytometry_Results_FINAL.xlsx"
  
)


openxlsx::write.xlsx(
  
  list(
    
    Sample_size =
      sample_size_table,
    
    Descriptive_statistics =
      descriptive_statistics,
    
    CTR_vs_GDM_nominal =
      between_group_results,
    
    CTR_vs_GDM_significant =
      significant_between_group,
    
    Mixed_model_ANOVA =
      mixed_model_anova,
    
    Compartments_BH =
      compartment_posthoc,
    
    Significant_compartments =
      significant_compartment_posthoc,
    
    EMM =
      estimated_marginal_means,
    
    Model_diagnostics =
      model_diagnostics
    
  ),
  
  file =
    excel_file,
  
  overwrite =
    TRUE
  
)



descriptive_statistics_EN <- descriptive_statistics %>%
  dplyr::mutate(SEM = sd / sqrt(n))

sample_size_EN <- sample_size_table
between_group_results_EN <- between_group_results
significant_between_group_EN <- significant_between_group
mixed_model_anova_EN <- mixed_model_anova
compartment_posthoc_EN <- compartment_posthoc
significant_compartment_posthoc_EN <- significant_compartment_posthoc
estimated_marginal_means_EN <- estimated_marginal_means
model_diagnostics_EN <- model_diagnostics

english_excel_file <- file.path(
  results_dir,
  "Flow_Cytometry_Complete_Results.xlsx"
)

openxlsx::write.xlsx(
  list(
    Sample_size = sample_size_EN,
    Descriptive_statistics = descriptive_statistics_EN,
    CTR_vs_GDM_nominal_p = between_group_results_EN,
    CTR_vs_GDM_significant = significant_between_group_EN,
    Mixed_model_ANOVA = mixed_model_anova_EN,
    Compartment_posthoc_BH = compartment_posthoc_EN,
    Significant_compartment_BH = significant_compartment_posthoc_EN,
    Estimated_marginal_means = estimated_marginal_means_EN,
    Model_diagnostics = model_diagnostics_EN
  ),
  file = english_excel_file,
  overwrite = TRUE
)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(svglite)

set.seed(123)

setwd(".")


data_file <- "Analise citometro.xls"

results_file <- file.path(
  "Flow_Cytometry_Results_FINAL",
  "Flow_Cytometry_Results_FINAL.xlsx"
)


plot_objures_dir <- "Flow_Cytometry_Figures_FINAL"

png_dir <- file.path(
  plot_objures_dir,
  "PNG"
)

tiff_dir <- file.path(
  plot_objures_dir,
  "TIFF"
)

svg_dir <- file.path(
  plot_objures_dir,
  "Editable_SVG"
)

dir.create(
  plot_objures_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  png_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  tiff_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  svg_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


raw_import <- readxl::read_excel(
  data_file,
  sheet = 1,
  col_names = TRUE,
  .name_repair = "unique"
)


flow_columns <- names(raw_import)[3:ncol(raw_import)]


column_map <- tibble::tibble(
  Column = flow_columns,
  
  Marker = as.character(
    unlist(
      raw_import[
        1,
        flow_columns
      ]
    )
  )
) %>%
  
  dplyr::mutate(
    
    Compartment = dplyr::case_when(
      
      stringr::str_detect(
        Column,
        "^Maternal blood"
      ) ~ "Maternal blood",
      
      stringr::str_detect(
        Column,
        "^Umbilical cord blood"
      ) ~ "Umbilical cord blood",
      
      stringr::str_detect(
        Column,
        "^Decidual tissue"
      ) ~ "Decidual tissue",
      
      TRUE ~ NA_character_
    ),
    
    Marker = stringr::str_trim(
      Marker
    )
  )


raw_data <- raw_import[-1, ]


raw_data <- raw_data %>%
  
  dplyr::mutate(
    
    Code = trimws(
      as.character(Code)
    ),
    
    Group = trimws(
      as.character(`Sample type`)
    ),
    
    Group = stringr::str_to_upper(
      Group
    )
  ) %>%
  
  dplyr::filter(
    !is.na(Code),
    Group %in% c(
      "CTR",
      "GDM"
    )
  ) %>%
  
  dplyr::mutate(
    
    Group = factor(
      Group,
      levels = c(
        "CTR",
        "GDM"
      )
    ),
    
    Code = factor(Code)
  )


long_data <- raw_data %>%
  
  tidyr::pivot_longer(
    
    cols = dplyr::all_of(
      flow_columns
    ),
    
    names_to = "Column",
    
    values_to = "Value"
  ) %>%
  
  dplyr::left_join(
    column_map,
    by = "Column"
  ) %>%
  
  dplyr::mutate(
    
    Value = as.character(
      Value
    ),
    
    Value = stringr::str_trim(
      Value
    ),
    
    Value = dplyr::na_if(
      Value,
      ""
    ),
    
    Value = dplyr::na_if(
      Value,
      "-"
    ),
    
    Value = dplyr::na_if(
      Value,
      "NA"
    ),
    
    Value = dplyr::na_if(
      Value,
      "Undetermined"
    ),
    
    Value = stringr::str_replace_all(
      Value,
      ",",
      "."
    ),
    
    Value = suppressWarnings(
      as.numeric(Value)
    ),
    
    Marker = stringr::str_trim(
      Marker
    ),
    
    Compartment = factor(
      Compartment,
      levels = c(
        "Maternal blood",
        "Umbilical cord blood",
        "Decidual tissue"
      )
    ),
    
    Group = factor(
      Group,
      levels = c(
        "CTR",
        "GDM"
      )
    )
  )


between_group_results <- readxl::read_excel(
  results_file,
  sheet = "CTR_vs_GDM_nominal"
)

compartment_posthoc <- readxl::read_excel(
  results_file,
  sheet = "Compartments_BH"
)


cat(
  "\nColumns de long_data:\n"
)

print(
  names(long_data)
)

cat(
  "\nMarkeres encontrados:\n"
)

print(
  unique(long_data$Marker)
)


group_colors <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)

compartment_labels <- c(
  "Maternal blood" = "Maternal\nblood",
  "Umbilical cord blood" = "Umbilical cord\nblood",
  "Decidual tissue" = "Decidual\ntissue"
)


central_letters <- tibble::tribble(
  
  ~Marker,                  ~Compartment, ~Letter,
  
  "CD45+CD3+CD4-CD28+",       "Maternal blood",        "b",
  "CD45+CD3+CD4-CD28+",       "Umbilical cord blood",          "a",
  
  "CD45+CD3+CD4-CTLA4+",      "Maternal blood",        "b",
  "CD45+CD3+CD4-CTLA4+",      "Umbilical cord blood",          "b",
  "CD45+CD3+CD4-CTLA4+",      "Decidual tissue",       "a"
)


separate_letters <- tibble::tribble(
  
  ~Marker,                  ~Compartment, ~Group, ~Letter,
  
  "CD45+CD3+CD4-CD28+",       "Decidual tissue",       "CTR",  "c",
  "CD45+CD3+CD4-CD28+",       "Decidual tissue",       "GDM",  "b"
)


create_cytometry_plot <- function(
    marker,
    plot_title,
    titulo_y = "Frequency (%)"
) {
  
  
  plot_data <- long_data %>%
    dplyr::filter(
      Marker == marker,
      !is.na(Value),
      !is.na(Group),
      !is.na(Compartment)
    )
  
  
  summary_stats <- plot_data %>%
    dplyr::group_by(
      Compartment,
      Group
    ) %>%
    dplyr::summarise(
      
      n = sum(!is.na(Value)),
      
      mean_value = mean(
        Value,
        na.rm = TRUE
      ),
      
      sd = stats::sd(
        Value,
        na.rm = TRUE
      ),
      
      sem = sd / sqrt(n),
      
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      
      sem = dplyr::if_else(
        is.na(sem),
        0,
        sem
      )
    )
  
  
  group_top_value <- plot_data %>%
    dplyr::group_by(
      Compartment,
      Group
    ) %>%
    dplyr::summarise(
      
      observed_max = max(
        Value,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      summary_stats,
      by = c(
        "Compartment",
        "Group"
      )
    ) %>%
    dplyr::mutate(
      
      top_value = pmax(
        observed_max,
        mean_value + sem,
        na.rm = TRUE
      )
    )
  
  
  data_max <- max(
    group_top_value$top_value,
    na.rm = TRUE
  )
  
  if (!is.finite(data_max)) {
    data_max <- 1
  }
  
  spacing <- data_max * 0.08
  
  if (spacing <= 0) {
    spacing <- 1
  }
  
  
  group_significance <- between_group_results %>%
    dplyr::filter(
      Marker == marker,
      !is.na(p_value),
      p_value < 0.05
    ) %>%
    dplyr::mutate(
      
      Compartment = factor(
        Compartment,
        levels = c(
          "Maternal blood",
          "Umbilical cord blood",
          "Decidual tissue"
        )
      ),
      
      x = as.numeric(
        Compartment
      )
    )
  
  
  compartment_top <- group_top_value %>%
    dplyr::group_by(
      Compartment
    ) %>%
    dplyr::summarise(
      
      compartment_top = max(
        top_value,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  
  group_significance <- group_significance %>%
    dplyr::left_join(
      compartment_top,
      by = "Compartment"
    ) %>%
    dplyr::mutate(
      y_bracket =
        compartment_top +
        spacing * 0.55,
      
      y_star =
        compartment_top +
        spacing * 1.10
    )
  
  
  letters_c <- central_letters %>%
    dplyr::filter(
      Marker == marker
    ) %>%
    dplyr::mutate(
      
      Compartment = factor(
        Compartment,
        levels = c(
          "Maternal blood",
          "Umbilical cord blood",
          "Decidual tissue"
        )
      )
    ) %>%
    dplyr::left_join(
      compartment_top,
      by = "Compartment"
    ) %>%
    dplyr::mutate(
      
      y_letter =
        compartment_top +
        spacing * 1.25
    )
  
  
  letters_s <- separate_letters %>%
    dplyr::filter(
      Marker == marker
    ) %>%
    dplyr::mutate(
      
      Compartment = factor(
        Compartment,
        levels = c(
          "Maternal blood",
          "Umbilical cord blood",
          "Decidual tissue"
        )
      ),
      
      Group = factor(
        Group,
        levels = c(
          "CTR",
          "GDM"
        )
      )
    ) %>%
    dplyr::left_join(
      group_top_value,
      by = c(
        "Compartment",
        "Group"
      )
    ) %>%
    dplyr::mutate(
      
      y_letter =
        top_value +
        spacing * 1.25,
      
      x_letter =
        as.numeric(Compartment) +
        dplyr::if_else(
          Group == "CTR",
          -0.19,
          0.19
        )
    )
  
  
  if (nrow(group_significance) > 0) {
    
    letters_c <- letters_c %>%
      dplyr::left_join(
        
        group_significance %>%
          dplyr::select(
            Compartment,
            y_star
          ),
        
        by = "Compartment"
      ) %>%
      dplyr::mutate(
        
        y_letter = dplyr::if_else(
          !is.na(y_star),
          y_star + spacing * 1.50,
          y_letter
        )
      ) %>%
      dplyr::select(
        -y_star
      )
    
    letters_s <- letters_s %>%
      dplyr::left_join(
        
        group_significance %>%
          dplyr::select(
            Compartment,
            y_star
          ),
        
        by = "Compartment"
      ) %>%
      dplyr::mutate(
        
        y_letter = dplyr::if_else(
          !is.na(y_star),
          y_star + spacing * 0.90,
          y_letter
        )
      ) %>%
      dplyr::select(
        -y_star
      )
  }
  
  
  valores_top_value <- c(
    data_max
  )
  
  if (nrow(group_significance) > 0) {
    
    valores_top_value <- c(
      valores_top_value,
      group_significance$y_star
    )
  }
  
  if (nrow(letters_c) > 0) {
    
    valores_top_value <- c(
      valores_top_value,
      letters_c$y_letter
    )
  }
  
  if (nrow(letters_s) > 0) {
    
    valores_top_value <- c(
      valores_top_value,
      letters_s$y_letter
    )
  }
  
  upper_limit <- max(
    valores_top_value,
    na.rm = TRUE
  ) + spacing * 1.00
  
  
  if (upper_limit <= 5) {
    
    upper_limit <-
      ceiling(
        upper_limit * 2
      ) / 2
    
  } else if (upper_limit <= 20) {
    
    upper_limit <-
      ceiling(
        upper_limit
      )
    
  } else {
    
    upper_limit <-
      ceiling(
        upper_limit / 10
      ) * 10
  }
  
  
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = Compartment,
      y = Value,
      fill = Group
    )
  ) +
    
  
  ggplot2::geom_col(
    
    data = summary_stats,
    
    ggplot2::aes(
      x = Compartment,
      y = mean_value,
      fill = Group
    ),
    
    inherit.aes = FALSE,
    
    position = ggplot2::position_dodge(
      width = 0.75
    ),
    
    width = 0.65,
    
    alpha = 0.75,
    
    color = "black",
    
    linewidth = 0.7
  ) +
    
  
  ggplot2::geom_errorbar(
    
    data = summary_stats,
    
    ggplot2::aes(
      x = Compartment,
      ymin = mean_value - sem,
      ymax = mean_value + sem,
      group = Group
    ),
    
    inherit.aes = FALSE,
    
    position = ggplot2::position_dodge(
      width = 0.75
    ),
    
    width = 0.15,
    
    linewidth = 0.8,
    
    color = "black"
  ) +
    
  
  ggplot2::geom_point(
    
    ggplot2::aes(
      group = Group
    ),
    
    position = ggplot2::position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.75
    ),
    
    size = 2.7,
    
    shape = 16,
    
    color = "black"
  ) +
    
  
  ggplot2::scale_fill_manual(
    values = group_colors
  ) +
    
  
  ggplot2::scale_x_discrete(
    labels = compartment_labels
  ) +
    
  
  ggplot2::scale_y_continuous(
    
    limits = c(
      0,
      upper_limit
    ),
    
    expand = ggplot2::expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
    
  
  ggplot2::labs(
    x = NULL,
    y = titulo_y,
    title = plot_title
  ) +
    
  
  ggplot2::theme_classic(
    base_family = "Arial"
  ) +
    
    ggplot2::theme(
      
      legend.position = "none",
      
      plot.title = ggplot2::element_text(
        size = 18,
        face = "bold",
        hjust = 0.5,
        color = "black"
      ),
      
      axis.text.x = ggplot2::element_text(
        size = 16,
        color = "black"
      ),
      
      axis.text.y = ggplot2::element_text(
        size = 14,
        color = "black"
      ),
      
      axis.title.y = ggplot2::element_text(
        size = 16,
        color = "black"
      ),
      
      axis.line = ggplot2::element_line(
        linewidth = 0.8,
        color = "black"
      ),
      
      axis.ticks = ggplot2::element_line(
        color = "black"
      ),
      
      plot.margin = ggplot2::margin(
        15,
        15,
        10,
        10
      )
    )
  
  
  if (nrow(group_significance) > 0) {
    
    p <- p +
      
      ggplot2::geom_segment(
        
        data = group_significance,
        
        ggplot2::aes(
          x = x - 0.19,
          xend = x + 0.19,
          y = y_bracket,
          yend = y_bracket
        ),
        
        inherit.aes = FALSE,
        
        linewidth = 0.7
      ) +
      
      ggplot2::geom_segment(
        
        data = group_significance,
        
        ggplot2::aes(
          x = x - 0.19,
          xend = x - 0.19,
          y = y_bracket,
          yend = y_bracket - spacing * 0.15
        ),
        
        inherit.aes = FALSE,
        
        linewidth = 0.7
      ) +
      
      ggplot2::geom_segment(
        
        data = group_significance,
        
        ggplot2::aes(
          x = x + 0.19,
          xend = x + 0.19,
          y = y_bracket,
          yend = y_bracket - spacing * 0.15
        ),
        
        inherit.aes = FALSE,
        
        linewidth = 0.7
      ) +
      
      ggplot2::geom_text(
        
        data = group_significance,
        
        ggplot2::aes(
          x = x,
          y = y_star,
          label = "*"
        ),
        
        inherit.aes = FALSE,
        
        family = "Arial",
        
        fontface = "bold",
        
        size = 8
      )
  }
  
  
  if (nrow(letters_c) > 0) {
    
    p <- p +
      
      ggplot2::geom_text(
        
        data = letters_c,
        
        ggplot2::aes(
          x = Compartment,
          y = y_letter,
          label = Letter
        ),
        
        inherit.aes = FALSE,
        
        family = "Arial",
        
        fontface = "bold",
        
        size = 6
      )
  }
  
  
  if (nrow(letters_s) > 0) {
    
    p <- p +
      
      ggplot2::geom_text(
        
        data = letters_s,
        
        ggplot2::aes(
          x = x_letter,
          y = y_letter,
          label = Letter
        ),
        
        inherit.aes = FALSE,
        
        family = "Arial",
        
        fontface = "bold",
        
        size = 6
      )
  }
  
  return(p)
}


plot_obj_cd28 <- create_cytometry_plot(
  marker = "CD45+CD3+CD4-CD28+",
  plot_title = "CD45+CD3+CD4-CD28+"
)

plot_obj_cd28


plot_obj_ctla4 <- create_cytometry_plot(
  marker = "CD45+CD3+CD4-CTLA4+",
  plot_title = "CD45+CD3+CD4-CTLA4+"
)

plot_obj_ctla4


plot_objures_dir <- "Flow_Cytometry_Figures_FINAL"

png_dir <- file.path(
  plot_objures_dir,
  "PNG"
)

tiff_dir <- file.path(
  plot_objures_dir,
  "TIFF"
)

svg_dir <- file.path(
  plot_objures_dir,
  "Editable_SVG"
)

dir.create(
  png_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  tiff_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  svg_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


ggplot2::ggsave(
  filename = file.path(
    png_dir,
    "CD45_CD3_CD4neg_CD28.png"
  ),
  plot = plot_obj_cd28,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    tiff_dir,
    "CD45_CD3_CD4neg_CD28.tiff"
  ),
  plot = plot_obj_cd28,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    svg_dir,
    "CD45_CD3_CD4neg_CD28.svg"
  ),
  plot = plot_obj_cd28,
  width = 7,
  height = 5.5,
  units = "in",
  device = svglite::svglite,
  bg = "white"
)


ggplot2::ggsave(
  filename = file.path(
    png_dir,
    "CD45_CD3_CD4neg_CTLA4.png"
  ),
  plot = plot_obj_ctla4,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    tiff_dir,
    "CD45_CD3_CD4neg_CTLA4.tiff"
  ),
  plot = plot_obj_ctla4,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    svg_dir,
    "CD45_CD3_CD4neg_CTLA4.svg"
  ),
  plot = plot_obj_ctla4,
  width = 7,
  height = 5.5,
  units = "in",
  device = svglite::svglite,
  bg = "white"
)


cat(
  "\nFigures saved to:\n",
  normalizePath(plot_objures_dir),
  "\n"
)
