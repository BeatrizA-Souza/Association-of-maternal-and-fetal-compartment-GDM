# ============================================================
# FINAL SENSITIVITY ANALYSIS - miRNAs
# BMI + GESTATIONAL AGE
#
# Reproduces the ORIGINAL mixed-model analysis first
# and then adds the sensitivity adjustment.
#
# ORIGINAL ANALYSIS:
#   - miRNAs analyzed as log2(relative expression)
#   - miR-155: Cord + Decidua only
#   - Type III ANOVA
#   - contr.sum / contr.poly
#   - REML = TRUE
#
# MODELS:
#
# ORIGINAL:
# log2(expression) ~ Compartment * Group + (1 | Code)
#
# ADJUSTED:
# log2(expression) ~ Compartment * Group +
#                    PrePreg_BMI +
#                    Gestational_Age +
#                    (1 | Code)
#
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

pacotes <- c(
  "tidyverse",
  "readxl",
  "lme4",
  "lmerTest",
  "emmeans",
  "broom.mixed",
  "openxlsx"
)

novos <- pacotes[
  !(pacotes %in% installed.packages()[, "Package"])
]

if (length(novos) > 0) {
  install.packages(novos)
}

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom.mixed)
library(openxlsx)


# ============================================================
# 2. TYPE III CONTRASTS
#
# EXACTLY AS IN ORIGINAL SCRIPT
# ============================================================

options(
  contrasts = c(
    "contr.sum",
    "contr.poly"
  )
)


# ============================================================
# 3. SELECT FILES
# ============================================================

cat("\nSelecione o arquivo: compartimentos.xlsx\n")
arquivo_mirna <- file.choose()

cat("\nSelecione o arquivo: dados mães.xlsx\n")
arquivo_clinico <- file.choose()


saida <- file.path(
  dirname(arquivo_mirna),
  "FINAL_miRNA_Sensitivity_CORRECTED_LOG2.xlsx"
)


# ============================================================
# 4. READ miRNA DATA
# ============================================================

dados <- readxl::read_excel(
  arquivo_mirna
)


# ============================================================
# 5. CLEAN COLUMN NAMES
#
# EXACTLY AS ORIGINAL SCRIPT
# ============================================================

names(dados) <- names(dados) |>
  
  stringr::str_replace_all(
    "[\r\n]",
    " "
  ) |>
  
  stringr::str_squish()


# ============================================================
# 6. ORGANIZE ID AND GROUP
# ============================================================

dados <- dados |>
  
  dplyr::rename(
    Grupo = `Sample type`
  ) |>
  
  dplyr::filter(
    !is.na(Code),
    Grupo %in% c(
      "CTR",
      "GDM"
    )
  ) |>
  
  dplyr::mutate(
    
    Code = factor(Code),
    
    Grupo = factor(
      Grupo,
      levels = c(
        "CTR",
        "GDM"
      )
    )
  )


# ============================================================
# 7. READ CLINICAL DATA
# ============================================================

clinico <- readxl::read_excel(
  arquivo_clinico,
  sheet = "maes"
)


# ============================================================
# 8. NUMERIC CONVERSION FUNCTION
# ============================================================

para_numerico <- function(x) {
  
  x <- as.character(x)
  
  x[x %in% c(
    "",
    "-",
    "NA",
    "NaN",
    "NULL",
    "Undetermined"
  )] <- NA
  
  x <- stringr::str_replace_all(
    x,
    ",",
    "."
  )
  
  suppressWarnings(
    as.numeric(x)
  )
}


# ============================================================
# 9. CLINICAL COVARIATES
# ============================================================

covariaveis <- dados |>
  
  dplyr::transmute(
    
    Code = as.character(Code),
    
    Gestational_Age =
      para_numerico(
        `Gestational Age at collection`
      )
  ) |>
  
  dplyr::left_join(
    
    clinico |>
      
      dplyr::filter(
        !is.na(Code)
      ) |>
      
      dplyr::transmute(
        
        Code =
          as.character(Code),
        
        PrePreg_BMI =
          para_numerico(
            `Pre-pregnancy BMI`
          )
      ),
    
    by = "Code"
  )


# ============================================================
# 10. CHECK COVARIATES
# ============================================================

cat("\n=============================\n")
cat("COVARIATE CHECK\n")
cat("=============================\n")

cat(
  "Participants:",
  n_distinct(covariaveis$Code),
  "\n"
)

cat(
  "Missing BMI:",
  sum(is.na(covariaveis$PrePreg_BMI)),
  "\n"
)

cat(
  "Missing gestational age:",
  sum(is.na(covariaveis$Gestational_Age)),
  "\n"
)


# ============================================================
# 11. ORIGINAL CLEANING FUNCTION
# ============================================================

limpar_valores <- function(df) {
  
  df |>
    
    dplyr::mutate(
      
      dplyr::across(
        c(
          Mother,
          Cord,
          Decidua
        ),
        as.character
      )
    ) |>
    
    tidyr::pivot_longer(
      
      cols = c(
        Mother,
        Cord,
        Decidua
      ),
      
      names_to = "Compartimento",
      
      values_to = "Valor"
    ) |>
    
    dplyr::mutate(
      
      Valor =
        stringr::str_trim(
          Valor
        ),
      
      Valor =
        dplyr::na_if(
          Valor,
          ""
        ),
      
      Valor =
        dplyr::na_if(
          Valor,
          "-"
        ),
      
      Valor =
        dplyr::na_if(
          Valor,
          "NA"
        ),
      
      Valor =
        dplyr::na_if(
          Valor,
          "Undetermined"
        ),
      
      Valor =
        stringr::str_replace_all(
          Valor,
          ",",
          "."
        ),
      
      Valor =
        suppressWarnings(
          as.numeric(
            Valor
          )
        )
    )
}


# ============================================================
# 12. miR-29a-3p
# ============================================================

mir29 <- dados |>
  
  dplyr::select(
    
    Code,
    Grupo,
    
    Mother =
      `Relative expression miR 29a-3p Mother`,
    
    Cord =
      `Relative expression miR 29a-3p Cord`,
    
    Decidua =
      `Relative expression miR 29a-3p Decidua`
  ) |>
  
  limpar_valores() |>
  
  dplyr::mutate(
    miRNA = "miR-29a-3p"
  )


# ============================================================
# 13. miR-132-3p
# ============================================================

mir132 <- dados |>
  
  dplyr::select(
    
    Code,
    Grupo,
    
    Mother =
      `Relative expression miR 132-3p Mother`,
    
    Cord =
      `Relative expression miR 132-3p Cord`,
    
    Decidua =
      `Relative expression miR 132-3p Decidua`
  ) |>
  
  limpar_valores() |>
  
  dplyr::mutate(
    miRNA = "miR-132-3p"
  )


# ============================================================
# 14. miR-150-5p
# ============================================================

mir150 <- dados |>
  
  dplyr::select(
    
    Code,
    Grupo,
    
    Mother =
      `Relative expression miR 150-5p Mother`,
    
    Cord =
      `Relative expression miR 150-5p Cord`,
    
    Decidua =
      `Relative expression miR 150-5p Placenta`
  ) |>
  
  limpar_valores() |>
  
  dplyr::mutate(
    miRNA = "miR-150-5p"
  )


# ============================================================
# 15. miR-155-5p
# ============================================================

mir155 <- dados |>
  
  dplyr::select(
    
    Code,
    Grupo,
    
    Mother =
      `Relative expression miR 155-5p Mother`,
    
    Cord =
      `Relative expression miR 155-5p Cord`,
    
    Decidua =
      `Relative expression miR 155-5p Placenta`
  ) |>
  
  limpar_valores() |>
  
  dplyr::mutate(
    miRNA = "miR-155-5p"
  )


# ============================================================
# 16. miR-222-3p
# ============================================================

mir222 <- dados |>
  
  dplyr::select(
    
    Code,
    Grupo,
    
    Mother =
      `Relative expression miR 222-3p Mother`,
    
    Cord =
      `Relative expression miR 222-3p Cord`,
    
    Decidua =
      `Relative expression miR 222-3p Decidua`
  ) |>
  
  limpar_valores() |>
  
  dplyr::mutate(
    miRNA = "miR-222-3p"
  )


# ============================================================
# 17. COMBINE
# ============================================================

todos_long <- dplyr::bind_rows(
  
  mir29,
  mir132,
  mir150,
  mir155,
  mir222
  
) |>
  
  dplyr::mutate(
    
    Compartimento =
      factor(
        Compartimento,
        levels = c(
          "Mother",
          "Cord",
          "Decidua"
        )
      ),
    
    Grupo =
      factor(
        Grupo,
        levels = c(
          "CTR",
          "GDM"
        )
      ),
    
    Code =
      factor(
        Code
      )
  )


# ============================================================
# 18. CRITICAL STEP:
# LOG2 TRANSFORMATION
#
# EXACTLY AS ORIGINAL ANALYSIS
# ============================================================

todos_long <- todos_long |>
  
  dplyr::mutate(
    
    Valor_analise =
      dplyr::case_when(
        
        !is.na(Valor) &
          Valor > 0 ~
          
          log2(
            Valor
          ),
        
        TRUE ~
          NA_real_
      )
  )


# ============================================================
# 19. ADD BMI + GESTATIONAL AGE
# ============================================================

todos_long <- todos_long |>
  
  dplyr::mutate(
    Code_char =
      as.character(
        Code
      )
  ) |>
  
  dplyr::left_join(
    
    covariaveis |>
      
      dplyr::rename(
        Code_char = Code
      ),
    
    by = "Code_char"
  )


# ============================================================
# 20. CHECK LOG2 DATA
# ============================================================

cat("\n=============================\n")
cat("DATA CHECK AFTER LOG2\n")
cat("=============================\n")

print(
  
  todos_long |>
    
    dplyr::filter(
      !is.na(Valor_analise)
    ) |>
    
    dplyr::count(
      miRNA,
      Grupo,
      Compartimento
    )
)


# ============================================================
# 21. ANALYSIS FUNCTION
# ============================================================

rodar_sensibilidade <- function(
    df,
    mirna
) {
  
  
  cat(
    "\n========================================\n"
  )
  
  cat(
    "ANALYZING:",
    mirna,
    "\n"
  )
  
  cat(
    "========================================\n"
  )
  
  
  # ==========================================================
  # ORIGINAL DATASET
  # ==========================================================
  
  dados_original <- df |>
    
    dplyr::filter(
      
      miRNA == mirna,
      
      !is.na(
        Valor_analise
      ),
      
      !is.na(
        Code
      ),
      
      !is.na(
        Grupo
      ),
      
      !is.na(
        Compartimento
      )
    )
  
  
  # ==========================================================
  # SPECIAL RULE FOR miR-155
  #
  # ORIGINAL ANALYSIS:
  # CORD + DECIDUA ONLY
  # ==========================================================
  
  if (
    mirna ==
    "miR-155-5p"
  ) {
    
    dados_original <-
      dados_original |>
      
      dplyr::filter(
        
        Compartimento %in%
          c(
            "Cord",
            "Decidua"
          )
      ) |>
      
      dplyr::mutate(
        
        Compartimento =
          factor(
            
            Compartimento,
            
            levels = c(
              "Cord",
              "Decidua"
            )
          )
      )
  }
  
  
  dados_original <-
    droplevels(
      dados_original
    )
  
  
  # ==========================================================
  # COMPLETE CASE FOR BMI + GA
  # ==========================================================
  
  dados_cc <- dados_original |>
    
    dplyr::filter(
      
      !is.na(
        PrePreg_BMI
      ),
      
      !is.na(
        Gestational_Age
      )
    ) |>
    
    droplevels()
  
  
  # ==========================================================
  # SAMPLE SIZES
  # ==========================================================
  
  n_original_participants <-
    n_distinct(
      dados_original$Code
    )
  
  n_original_observations <-
    nrow(
      dados_original
    )
  
  n_cc_participants <-
    n_distinct(
      dados_cc$Code
    )
  
  n_cc_observations <-
    nrow(
      dados_cc
    )
  
  
  cat(
    "Original participants:",
    n_original_participants,
    "\n"
  )
  
  cat(
    "Original observations:",
    n_original_observations,
    "\n"
  )
  
  cat(
    "Complete-case participants:",
    n_cc_participants,
    "\n"
  )
  
  cat(
    "Complete-case observations:",
    n_cc_observations,
    "\n"
  )
  
  
  # ==========================================================
  # MODEL 1:
  # ORIGINAL
  #
  # EXACT ORIGINAL ANALYSIS
  # ==========================================================
  
  modelo_original <-
    lmerTest::lmer(
      
      Valor_analise ~
        Compartimento *
        Grupo +
        (1 | Code),
      
      data =
        dados_original,
      
      REML =
        TRUE
    )
  
  
  # ==========================================================
  # MODEL 2:
  # COMPLETE-CASE UNADJUSTED
  #
  # SAME SAMPLE AS ADJUSTED
  # ==========================================================
  
  modelo_cc <-
    lmerTest::lmer(
      
      Valor_analise ~
        Compartimento *
        Grupo +
        (1 | Code),
      
      data =
        dados_cc,
      
      REML =
        TRUE
    )
  
  
  # ==========================================================
  # MODEL 3:
  # ADJUSTED FOR BMI + GA
  # ==========================================================
  
  modelo_adjusted <-
    lmerTest::lmer(
      
      Valor_analise ~
        Compartimento *
        Grupo +
        PrePreg_BMI +
        Gestational_Age +
        (1 | Code),
      
      data =
        dados_cc,
      
      REML =
        TRUE
    )
  
  
  # ==========================================================
  # TYPE III ANOVA
  # ==========================================================
  
  extrair_anova <- function(
    modelo,
    nome_modelo,
    n_part,
    n_obs
  ) {
    
    resultado <-
      anova(
        modelo,
        type = 3
      ) |>
      
      as.data.frame() |>
      
      tibble::rownames_to_column(
        "Effect"
      )
    
    
    resultado |>
      
      dplyr::mutate(
        
        miRNA =
          mirna,
        
        Model =
          nome_modelo,
        
        N_participants =
          n_part,
        
        N_observations =
          n_obs,
        
        Singular =
          lme4::isSingular(
            modelo,
            tol = 1e-4
          )
      )
  }
  
  
  anova_original <-
    extrair_anova(
      
      modelo_original,
      
      "Original",
      
      n_original_participants,
      
      n_original_observations
    )
  
  
  anova_cc <-
    extrair_anova(
      
      modelo_cc,
      
      "Complete-case unadjusted",
      
      n_cc_participants,
      
      n_cc_observations
    )
  
  
  anova_adjusted <-
    extrair_anova(
      
      modelo_adjusted,
      
      "Adjusted BMI + GA",
      
      n_cc_participants,
      
      n_cc_observations
    )
  
  
  anova_all <-
    dplyr::bind_rows(
      
      anova_original,
      
      anova_cc,
      
      anova_adjusted
    )
  
  
  # ==========================================================
  # COEFFICIENTS + 95% CI
  # ==========================================================
  
  extrair_coeficientes <- function(
    modelo,
    nome_modelo
  ) {
    
    broom.mixed::tidy(
      
      modelo,
      
      effects =
        "fixed",
      
      conf.int =
        TRUE,
      
      conf.level =
        0.95
      
    ) |>
      
      dplyr::mutate(
        
        miRNA =
          mirna,
        
        Model =
          nome_modelo
      )
  }
  
  
  coeficientes <-
    dplyr::bind_rows(
      
      extrair_coeficientes(
        modelo_original,
        "Original"
      ),
      
      extrair_coeficientes(
        modelo_cc,
        "Complete-case unadjusted"
      ),
      
      extrair_coeficientes(
        modelo_adjusted,
        "Adjusted BMI + GA"
      )
    )
  
  
  # ==========================================================
  # EMMEANS:
  # COMPARTMENT WITHIN GROUP
  # ==========================================================
  
  extrair_compartimentos <- function(
    modelo,
    nome_modelo
  ) {
    
    
    emm <-
      emmeans::emmeans(
        
        modelo,
        
        ~ Compartimento |
          Grupo
      )
    
    
    resultado <-
      pairs(
        
        emm,
        
        adjust =
          "none"
        
      ) |>
      
      as.data.frame() |>
      
      dplyr::group_by(
        Grupo
      ) |>
      
      dplyr::mutate(
        
        p_BH =
          p.adjust(
            
            p.value,
            
            method =
              "BH"
          )
      ) |>
      
      dplyr::ungroup() |>
      
      dplyr::mutate(
        
        miRNA =
          mirna,
        
        Model =
          nome_modelo
      )
    
    
    resultado
  }
  
  
  posthoc_compartimento <-
    dplyr::bind_rows(
      
      extrair_compartimentos(
        modelo_original,
        "Original"
      ),
      
      extrair_compartimentos(
        modelo_cc,
        "Complete-case unadjusted"
      ),
      
      extrair_compartimentos(
        modelo_adjusted,
        "Adjusted BMI + GA"
      )
    )
  
  
  # ==========================================================
  # EMMEANS:
  # CTR vs GDM WITHIN COMPARTMENT
  #
  # MODEL-BASED ONLY.
  # DOES NOT REPLACE THE NOMINAL WELCH/MANN-WHITNEY
  # PRIMARY COMPARISONS.
  # ==========================================================
  
  extrair_grupo <- function(
    modelo,
    nome_modelo
  ) {
    
    
    emm <-
      emmeans::emmeans(
        
        modelo,
        
        ~ Grupo |
          Compartimento
      )
    
    
    resultado <-
      pairs(
        
        emm,
        
        adjust =
          "none"
        
      ) |>
      
      as.data.frame() |>
      
      dplyr::mutate(
        
        miRNA =
          mirna,
        
        Model =
          nome_modelo
      )
    
    
    resultado
  }
  
  
  posthoc_grupo <-
    dplyr::bind_rows(
      
      extrair_grupo(
        modelo_original,
        "Original"
      ),
      
      extrair_grupo(
        modelo_cc,
        "Complete-case unadjusted"
      ),
      
      extrair_grupo(
        modelo_adjusted,
        "Adjusted BMI + GA"
      )
    )
  
  
  # ==========================================================
  # DIAGNOSTICS
  # ==========================================================
  
  diagnostico <-
    data.frame(
      
      miRNA =
        mirna,
      
      Model =
        c(
          "Original",
          "Complete-case unadjusted",
          "Adjusted BMI + GA"
        ),
      
      N_participants =
        c(
          n_original_participants,
          n_cc_participants,
          n_cc_participants
        ),
      
      N_observations =
        c(
          n_original_observations,
          n_cc_observations,
          n_cc_observations
        ),
      
      Singular =
        c(
          
          lme4::isSingular(
            modelo_original,
            tol = 1e-4
          ),
          
          lme4::isSingular(
            modelo_cc,
            tol = 1e-4
          ),
          
          lme4::isSingular(
            modelo_adjusted,
            tol = 1e-4
          )
        )
    )
  
  
  # ==========================================================
  # RETURN
  # ==========================================================
  
  list(
    
    anova =
      anova_all,
    
    coefficients =
      coeficientes,
    
    compartment_posthoc =
      posthoc_compartimento,
    
    group_posthoc =
      posthoc_grupo,
    
    diagnostics =
      diagnostico
  )
}


# ============================================================
# 22. RUN ALL miRNAs
# ============================================================

moleculas <- c(
  
  "miR-29a-3p",
  "miR-132-3p",
  "miR-150-5p",
  "miR-155-5p",
  "miR-222-3p"
)


resultados <-
  purrr::map(
    
    moleculas,
    
    function(x) {
      
      rodar_sensibilidade(
        todos_long,
        x
      )
    }
  )


names(
  resultados
) <- moleculas


# ============================================================
# 23. COMBINE OUTPUTS
# ============================================================

anova_final <-
  purrr::map_dfr(
    resultados,
    "anova"
  )


coeficientes_final <-
  purrr::map_dfr(
    resultados,
    "coefficients"
  )


posthoc_comp_final <-
  purrr::map_dfr(
    resultados,
    "compartment_posthoc"
  )


posthoc_group_final <-
  purrr::map_dfr(
    resultados,
    "group_posthoc"
  )


diagnosticos_final <-
  purrr::map_dfr(
    resultados,
    "diagnostics"
  )


# ============================================================
# 24. SELECT MAIN EFFECTS
# ============================================================

anova_resumo <-
  anova_final |>
  
  dplyr::filter(
    
    Effect %in%
      c(
        "Grupo",
        "Compartimento",
        "Grupo:Compartimento",
        "Compartimento:Grupo",
        "PrePreg_BMI",
        "Gestational_Age"
      )
  )


# ============================================================
# 25. PRINT ORIGINAL RESULTS
#
# THESE SHOULD NOW MATCH THE ORIGINAL
# SUPPLEMENTARY TABLE.
# ============================================================

cat(
  "\n\n============================================\n"
)

cat(
  "ORIGINAL MODEL RESULTS\n"
)

cat(
  "THESE SHOULD MATCH THE ORIGINAL TABLE\n"
)

cat(
  "============================================\n\n"
)


original_check <-
  anova_resumo |>
  
  dplyr::filter(
    Model ==
      "Original"
  ) |>
  
  dplyr::select(
    miRNA,
    Effect,
    `F value`,
    `Pr(>F)`
  )


print(
  original_check
)


# ============================================================
# 26. PRINT ADJUSTED RESULTS
# ============================================================

cat(
  "\n\n============================================\n"
)

cat(
  "ADJUSTED BMI + GESTATIONAL AGE\n"
)

cat(
  "============================================\n\n"
)


adjusted_check <-
  anova_resumo |>
  
  dplyr::filter(
    Model ==
      "Adjusted BMI + GA"
  ) |>
  
  dplyr::select(
    miRNA,
    Effect,
    `F value`,
    `Pr(>F)`
  )


print(
  adjusted_check
)


# ============================================================
# 27. COMPARISON TABLE
# ============================================================

comparacao_modelos <-
  anova_resumo |>
  
  dplyr::select(
    
    miRNA,
    Effect,
    Model,
    `F value`,
    `Pr(>F)`,
    N_participants,
    N_observations,
    Singular
    
  ) |>
  
  tidyr::pivot_wider(
    
    names_from =
      Model,
    
    values_from =
      c(
        `F value`,
        `Pr(>F)`,
        N_participants,
        N_observations,
        Singular
      )
  )


# ============================================================
# 28. SAMPLE SIZE TABLE
# ============================================================

sample_sizes <-
  todos_long |>
  
  dplyr::filter(
    !is.na(
      Valor_analise
    )
  ) |>
  
  dplyr::mutate(
    
    Included_original =
      TRUE,
    
    Included_adjusted =
      !is.na(
        PrePreg_BMI
      ) &
      !is.na(
        Gestational_Age
      )
  ) |>
  
  dplyr::group_by(
    miRNA,
    Grupo,
    Compartimento
  ) |>
  
  dplyr::summarise(
    
    N_original =
      n_distinct(
        Code[
          Included_original
        ]
      ),
    
    N_adjusted =
      n_distinct(
        Code[
          Included_adjusted
        ]
      ),
    
    .groups =
      "drop"
  )


# ============================================================
# 29. COVARIATE SUMMARY
# ============================================================

covariate_summary <-
  covariaveis |>
  
  dplyr::left_join(
    
    dados |>
      
      dplyr::transmute(
        Code =
          as.character(
            Code
          ),
        Group =
          as.character(
            Grupo
          )
      ),
    
    by =
      "Code"
  ) |>
  
  dplyr::distinct(
    Code,
    Group,
    PrePreg_BMI,
    Gestational_Age
  ) |>
  
  dplyr::group_by(
    Group
  ) |>
  
  dplyr::summarise(
    
    N =
      n(),
    
    BMI_mean =
      mean(
        PrePreg_BMI,
        na.rm = TRUE
      ),
    
    BMI_SD =
      sd(
        PrePreg_BMI,
        na.rm = TRUE
      ),
    
    GA_mean =
      mean(
        Gestational_Age,
        na.rm = TRUE
      ),
    
    GA_SD =
      sd(
        Gestational_Age,
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
  )


# ============================================================
# 30. README
# ============================================================

readme <-
  data.frame(
    
    Section =
      c(
        
        "Purpose",
        
        "Outcome scale",
        
        "Original model",
        
        "Complete-case unadjusted",
        
        "Adjusted model",
        
        "miR-155",
        
        "Estimator",
        
        "Type III contrasts",
        
        "Global tests",
        
        "Compartment post hoc",
        
        "CTR vs GDM nominal tests",
        
        "Interpretation"
      ),
    
    Description =
      c(
        
        "Exploratory sensitivity analysis assessing potential confounding by pre-pregnancy BMI and gestational age.",
        
        "Mixed-effects models are fitted to log2(relative expression), reproducing the original miRNA analysis.",
        
        "log2(relative expression) ~ Compartment * Group + (1 | participant), using all available valid observations.",
        
        "Same unadjusted model restricted to observations with complete BMI and gestational-age data.",
        
        "log2(relative expression) ~ Compartment * Group + pre-pregnancy BMI + gestational age + (1 | participant).",
        
        "miR-155-5p is analyzed using umbilical cord blood and decidual tissue only, reproducing the original analysis.",
        
        "REML = TRUE.",
        
        "contr.sum and contr.poly, reproducing the original Type III analysis.",
        
        "Type III ANOVA from lmerTest.",
        
        "Estimated marginal means followed by pairwise compartment comparisons; Benjamini-Hochberg correction is calculated within group.",
        
        "The original nominal Welch/Mann-Whitney CTR vs GDM comparisons remain unchanged and are not replaced by model-based contrasts in this sensitivity analysis.",
        
        "Adjusted analyses are exploratory. Changes between Original and Complete-case reflect sample restriction; changes between Complete-case and Adjusted reflect covariate adjustment."
      ),
    
    stringsAsFactors =
      FALSE
  )


# ============================================================
# 31. EXPORT EXCEL
# ============================================================

wb <-
  openxlsx::createWorkbook()


sheet_data <-
  list(
    
    "README" =
      readme,
    
    "Original_CHECK" =
      original_check,
    
    "Adjusted_CHECK" =
      adjusted_check,
    
    "Model_comparison" =
      comparacao_modelos,
    
    "Global_tests" =
      anova_resumo,
    
    "Coefficients_95CI" =
      coeficientes_final,
    
    "Compartment_posthoc_BH" =
      posthoc_comp_final,
    
    "Model_based_CTR_vs_GDM" =
      posthoc_group_final,
    
    "Diagnostics" =
      diagnosticos_final,
    
    "Sample_sizes" =
      sample_sizes,
    
    "Covariates" =
      covariate_summary
  )


header_style <-
  openxlsx::createStyle(
    
    fontName =
      "Arial",
    
    fontSize =
      11,
    
    textDecoration =
      "bold",
    
    fgFill =
      "#D9EAF7",
    
    border =
      "Bottom",
    
    halign =
      "center",
    
    valign =
      "center"
  )


body_style <-
  openxlsx::createStyle(
    
    fontName =
      "Arial",
    
    fontSize =
      10,
    
    valign =
      "center"
  )


for (
  sheet_name in
  names(
    sheet_data
  )
) {
  
  openxlsx::addWorksheet(
    wb,
    sheet_name
  )
  
  openxlsx::writeData(
    wb,
    sheet_name,
    sheet_data[
      [sheet_name]
    ]
  )
  
  
  n_rows <-
    nrow(
      sheet_data[
        [sheet_name]
      ]
    ) + 1
  
  
  n_cols <-
    ncol(
      sheet_data[
        [sheet_name]
      ]
    )
  
  
  if (
    n_cols > 0
  ) {
    
    openxlsx::addStyle(
      
      wb,
      sheet_name,
      
      header_style,
      
      rows =
        1,
      
      cols =
        1:n_cols,
      
      gridExpand =
        TRUE
    )
    
    
    if (
      n_rows >= 2
    ) {
      
      openxlsx::addStyle(
        
        wb,
        sheet_name,
        
        body_style,
        
        rows =
          2:n_rows,
        
        cols =
          1:n_cols,
        
        gridExpand =
          TRUE
      )
    }
    
    
    openxlsx::freezePane(
      wb,
      sheet_name,
      firstRow =
        TRUE
    )
    
    
    openxlsx::setColWidths(
      wb,
      sheet_name,
      cols =
        1:n_cols,
      widths =
        "auto"
    )
  }
}


# ============================================================
# 32. SAVE
# ============================================================

openxlsx::saveWorkbook(
  
  wb,
  
  saida,
  
  overwrite =
    TRUE
)


cat(
  "\n\n============================================\n"
)

cat(
  "FINISHED!\n"
)

cat(
  "============================================\n"
)

cat(
  "File created:\n",
  saida,
  "\n"
)

cat(
  "\nFIRST CHECK THE SHEET: Original_CHECK\n"
)

cat(
  "The p-values there should reproduce the original supplementary table.\n"
)

cat(
  "Only after confirming that, interpret Adjusted_CHECK.\n"
)