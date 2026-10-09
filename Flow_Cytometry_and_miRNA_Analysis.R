# ============================================================
# COMPLETE FINAL ANALYSIS
# FLOW CYTOMETRY + miRNA + FINAL PANEL
# ============================================================
#
# This consolidated script preserves the final statistical workflows
# and creates the five final figure objects plus the A-E panel.
#
# FINAL FIGURE CONVENTION:
#   - Bars: mean
#   - Error bars: SEM
#   - Black dots: individual observations
#   - CTR: blue (#2761F5)
#   - GDM: red  (#F52727)
#   - Lowercase letters: compartment comparisons
#   - Stars/brackets: CTR vs GDM within compartment
#
# IMPORTANT:
#   The script intentionally changes working directory when it enters
#   the flow-cytometry and miRNA sections, because the original data
#   files are stored in different folders.
#
# Run this file from TOP TO BOTTOM in a clean R session.
# ============================================================


# ============================================================
# CITOMETRIA - ANÁLISE ESTATÍSTICA FINAL
# Versão final para arquivo local / manuscrito
# Atualizada: 29/09/2026
# ============================================================
# ============================================================
# 1. PACOTES
# ============================================================

pacotes <- c(
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


pacotes_faltando <- pacotes[
  !pacotes %in% rownames(
    installed.packages()
  )
]


if (length(pacotes_faltando) > 0) {
  
  install.packages(
    pacotes_faltando
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


# ============================================================
# 2. CONTRASTES PARA ANOVA TIPO III
# ============================================================

options(
  contrasts = c(
    "contr.sum",
    "contr.poly"
  )
)


# ============================================================
# 3. DIRETÓRIO DE TRABALHO
# ============================================================

setwd(
  "C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros/figuras citometro"
)


# ============================================================
# 4. ARQUIVO
# ============================================================

arquivo <- "Analise citometro.xls"


# ============================================================
# 5. PASTAS DE RESULTADOS
# ============================================================

pasta_resultados <-
  "Resultados_Citometria_FINAL"


pasta_png <- file.path(
  pasta_resultados,
  "Graficos_PNG"
)


pasta_tiff <- file.path(
  pasta_resultados,
  "Graficos_TIFF"
)


pasta_svg <- file.path(
  pasta_resultados,
  "Graficos_EDITAVEIS_SVG"
)


dir.create(
  pasta_resultados,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  pasta_png,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  pasta_tiff,
  recursive = TRUE,
  showWarnings = FALSE
)


dir.create(
  pasta_svg,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 6. CORES
# ============================================================

cores <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)


# ============================================================
# 7. LER PLANILHA
# ============================================================

dados_temp <- readxl::read_excel(
  arquivo,
  sheet = 1,
  col_names = TRUE,
  .name_repair = "unique"
)


# ============================================================
# 8. IDENTIFICAR COLUNAS DE CITOMETRIA
# ============================================================

colunas_citometria <- names(
  dados_temp
)[3:ncol(dados_temp)]


# ============================================================
# 9. MAPEAR MARCADORES E COMPARTIMENTOS
# ============================================================
#
# A primeira linha após o cabeçalho contém
# os nomes dos marcadores.
#
# ============================================================

mapa_colunas <- tibble::tibble(
  
  Coluna = colunas_citometria,
  
  Marcador = as.character(
    unlist(
      dados_temp[
        1,
        colunas_citometria
      ]
    )
  )
  
) %>%
  
  dplyr::mutate(
    
    Compartimento = dplyr::case_when(
      
      stringr::str_detect(
        Coluna,
        "^Sangue materno"
      ) ~ "Mother",
      
      stringr::str_detect(
        Coluna,
        "^Sangue de cordão"
      ) ~ "Cord",
      
      stringr::str_detect(
        Coluna,
        "^Decídua"
      ) ~ "Decidua",
      
      TRUE ~ NA_character_
      
    ),
    
    Marcador = stringr::str_trim(
      Marcador
    )
    
  )


print(mapa_colunas)


# ============================================================
# 10. REMOVER PRIMEIRA LINHA
# ============================================================
#
# Essa linha contém os nomes dos marcadores,
# não os valores dos participantes.
#
# ============================================================

dados_raw <- dados_temp[
  -1,
]


# ============================================================
# 11. PREPARAR CODE E GRUPO
# ============================================================

dados_raw <- dados_raw %>%
  
  dplyr::mutate(
    
    Code = trimws(
      as.character(Code)
    ),
    
    Grupo = trimws(
      as.character(
        `Sample type`
      )
    ),
    
    Grupo = stringr::str_to_upper(
      Grupo
    )
    
  ) %>%
  
  dplyr::filter(
    
    !is.na(Code),
    
    Grupo %in% c(
      "CTR",
      "GDM"
    )
    
  ) %>%
  
  dplyr::mutate(
    
    Grupo = factor(
      Grupo,
      levels = c(
        "CTR",
        "GDM"
      )
    ),
    
    Code = factor(
      Code
    )
    
  )


# ============================================================
# 12. TRANSFORMAR PARA FORMATO LONGO
# ============================================================

dados_long <- dados_raw %>%
  
  tidyr::pivot_longer(
    
    cols = dplyr::all_of(
      colunas_citometria
    ),
    
    names_to = "Coluna",
    
    values_to = "Valor"
    
  ) %>%
  
  dplyr::left_join(
    mapa_colunas,
    by = "Coluna"
  )


# ============================================================
# 13. LIMPAR VALORES
# ============================================================

dados_long <- dados_long %>%
  
  dplyr::mutate(
    
    Valor = as.character(
      Valor
    ),
    
    Valor = stringr::str_trim(
      Valor
    ),
    
    Valor = dplyr::na_if(
      Valor,
      ""
    ),
    
    Valor = dplyr::na_if(
      Valor,
      "-"
    ),
    
    Valor = dplyr::na_if(
      Valor,
      "NA"
    ),
    
    Valor = dplyr::na_if(
      Valor,
      "Undetermined"
    ),
    
    Valor = stringr::str_replace_all(
      Valor,
      ",",
      "."
    ),
    
    Valor = suppressWarnings(
      as.numeric(
        Valor
      )
    ),
    
    Marcador = stringr::str_trim(
      Marcador
    ),
    
    Compartimento = factor(
      Compartimento,
      levels = c(
        "Mother",
        "Cord",
        "Decidua"
      )
    ),
    
    Grupo = factor(
      Grupo,
      levels = c(
        "CTR",
        "GDM"
      )
    )
    
  )


# ============================================================
# 14. LISTA DE MARCADORES
# ============================================================

marcadores <- dados_long %>%
  
  dplyr::filter(
    !is.na(Marcador)
  ) %>%
  
  dplyr::distinct(
    Marcador
  ) %>%
  
  dplyr::pull(
    Marcador
  )


print(marcadores)


# ============================================================
# 15. LISTA DE COMPARTIMENTOS
# ============================================================

compartimentos <- c(
  "Mother",
  "Cord",
  "Decidua"
)


# ============================================================
# 16. N POR MARCADOR / GRUPO / COMPARTIMENTO
# ============================================================

tabela_n <- dados_long %>%
  
  dplyr::filter(
    !is.na(Valor)
  ) %>%
  
  dplyr::count(
    
    Marcador,
    Compartimento,
    Grupo,
    
    name = "n"
    
  )


# ============================================================
# 17. ESTATÍSTICA DESCRITIVA
# ============================================================

resultado_descritivo <- dados_long %>%
  
  dplyr::filter(
    !is.na(Valor)
  ) %>%
  
  dplyr::group_by(
    
    Marcador,
    Compartimento,
    Grupo
    
  ) %>%
  
  dplyr::summarise(
    
    n = dplyr::n(),
    
    media = mean(
      Valor,
      na.rm = TRUE
    ),
    
    sd = stats::sd(
      Valor,
      na.rm = TRUE
    ),
    
    mediana = median(
      Valor,
      na.rm = TRUE
    ),
    
    IQR = stats::IQR(
      Valor,
      na.rm = TRUE
    ),
    
    minimo = min(
      Valor,
      na.rm = TRUE
    ),
    
    maximo = max(
      Valor,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  )


# ============================================================
# 18. FUNÇÃO DE SIGNIFICÂNCIA
# ============================================================
#
# Qualquer p < 0.05 = *
#
# ============================================================

converter_significancia <- function(p) {
  
  dplyr::case_when(
    
    is.na(p) ~ "",
    
    p < 0.05 ~ "*",
    
    TRUE ~ "ns"
    
  )
  
}


# ============================================================
# ============================================================
#
# PARTE A
#
# CTR vs GDM DENTRO DE CADA COMPARTIMENTO
#
# SEM BH
#
# ============================================================
# ============================================================


# ============================================================
# 19. FUNÇÃO CTR vs GDM
# ============================================================

analisar_ctr_gdm <- function(
    df,
    marcador,
    compartimento
) {
  
  dados_m <- df %>%
    
    dplyr::filter(
      
      Marcador == marcador,
      
      Compartimento == compartimento,
      
      !is.na(Valor),
      
      !is.na(Grupo)
      
    )
  
  
  # ----------------------------------------------------------
  # Separar grupos
  # ----------------------------------------------------------
  
  ctr <- dados_m %>%
    
    dplyr::filter(
      Grupo == "CTR"
    ) %>%
    
    dplyr::pull(
      Valor
    )
  
  
  gdm <- dados_m %>%
    
    dplyr::filter(
      Grupo == "GDM"
    ) %>%
    
    dplyr::pull(
      Valor
    )
  
  
  n_ctr <- length(
    ctr
  )
  
  
  n_gdm <- length(
    gdm
  )
  
  
  # ----------------------------------------------------------
  # Caso N insuficiente
  # ----------------------------------------------------------
  
  if (
    n_ctr < 2 ||
    n_gdm < 2
  ) {
    
    return(
      
      tibble::tibble(
        
        Marcador = marcador,
        
        Compartimento = compartimento,
        
        n_CTR = n_ctr,
        
        n_GDM = n_gdm,
        
        Media_CTR = ifelse(
          n_ctr > 0,
          mean(ctr),
          NA_real_
        ),
        
        Media_GDM = ifelse(
          n_gdm > 0,
          mean(gdm),
          NA_real_
        ),
        
        Mediana_CTR = ifelse(
          n_ctr > 0,
          median(ctr),
          NA_real_
        ),
        
        Mediana_GDM = ifelse(
          n_gdm > 0,
          median(gdm),
          NA_real_
        ),
        
        Shapiro_CTR_p =
          NA_real_,
        
        Shapiro_GDM_p =
          NA_real_,
        
        Teste =
          "N insuficiente",
        
        Estatistica =
          NA_real_,
        
        p_value =
          NA_real_,
        
        Significancia =
          ""
        
      )
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Shapiro CTR
  # ----------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------
  # Shapiro GDM
  # ----------------------------------------------------------
  
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
  
  
  # ----------------------------------------------------------
  # Normalidade
  # ----------------------------------------------------------
  
  normal <- !is.na(
    shapiro_ctr
  ) &&
    !is.na(
      shapiro_gdm
    ) &&
    shapiro_ctr > 0.05 &&
    shapiro_gdm > 0.05
  
  
  # ----------------------------------------------------------
  # Escolher teste
  # ----------------------------------------------------------
  
  if (normal) {
    
    # ========================================================
    # Welch t-test
    # ========================================================
    
    teste <- stats::t.test(
      
      ctr,
      
      gdm,
      
      alternative =
        "two.sided",
      
      var.equal =
        FALSE
      
    )
    
    
    nome_teste <-
      "Welch t-test"
    
    
    estatistica <-
      as.numeric(
        teste$statistic
      )
    
    
  } else {
    
    # ========================================================
    # Mann-Whitney
    # ========================================================
    
    teste <- stats::wilcox.test(
      
      ctr,
      
      gdm,
      
      alternative =
        "two.sided",
      
      exact =
        FALSE
      
    )
    
    
    nome_teste <-
      "Mann-Whitney"
    
    
    estatistica <-
      as.numeric(
        teste$statistic
      )
    
  }
  
  
  # ----------------------------------------------------------
  # Resultado
  # ----------------------------------------------------------
  
  tibble::tibble(
    
    Marcador =
      marcador,
    
    Compartimento =
      compartimento,
    
    n_CTR =
      n_ctr,
    
    n_GDM =
      n_gdm,
    
    Media_CTR =
      mean(
        ctr,
        na.rm = TRUE
      ),
    
    Media_GDM =
      mean(
        gdm,
        na.rm = TRUE
      ),
    
    Mediana_CTR =
      median(
        ctr,
        na.rm = TRUE
      ),
    
    Mediana_GDM =
      median(
        gdm,
        na.rm = TRUE
      ),
    
    Shapiro_CTR_p =
      shapiro_ctr,
    
    Shapiro_GDM_p =
      shapiro_gdm,
    
    Teste =
      nome_teste,
    
    Estatistica =
      estatistica,
    
    p_value =
      teste$p.value,
    
    Significancia =
      converter_significancia(
        teste$p.value
      )
    
  )
  
}


# ============================================================
# 20. RODAR CTR vs GDM
# ============================================================

resultados_ctr_gdm <- purrr::map_dfr(
  
  compartimentos,
  
  function(comp) {
    
    purrr::map_dfr(
      
      marcadores,
      
      function(m) {
        
        analisar_ctr_gdm(
          
          df = dados_long,
          
          marcador = m,
          
          compartimento = comp
          
        )
        
      }
      
    )
    
  }
  
)


# ============================================================
# 21. DIREÇÃO CTR vs GDM
# ============================================================

resultados_ctr_gdm <- resultados_ctr_gdm %>%
  
  dplyr::mutate(
    
    Direcao = dplyr::case_when(
      
      Media_CTR >
        Media_GDM ~
        "CTR > GDM",
      
      Media_GDM >
        Media_CTR ~
        "GDM > CTR",
      
      TRUE ~
        "CTR = GDM"
      
    )
    
  ) %>%
  
  dplyr::arrange(
    Compartimento,
    Marcador
  )


# ============================================================
# 22. SIGNIFICATIVOS CTR vs GDM
# ============================================================

ctr_gdm_significativos <-
  resultados_ctr_gdm %>%
  
  dplyr::filter(
    p_value < 0.05
  )


# ============================================================
# ============================================================
#
# PARTE B
#
# MODELO MISTO
#
# COMPARAÇÃO ENTRE COMPARTIMENTOS
#
# BH NOS PÓS-HOC
#
# ============================================================
# ============================================================


# ============================================================
# 23. FUNÇÃO DO MODELO MISTO
# ============================================================

analisar_modelo_misto <- function(
    df,
    marcador
) {
  
  dados_m <- df %>%
    
    dplyr::filter(
      
      Marcador ==
        marcador,
      
      !is.na(
        Valor
      ),
      
      !is.na(
        Grupo
      ),
      
      !is.na(
        Compartimento
      ),
      
      !is.na(
        Code
      )
      
    ) %>%
    
    droplevels()
  
  
  # ----------------------------------------------------------
  # Verificações mínimas
  # ----------------------------------------------------------
  
  if (
    nrow(dados_m) < 6 ||
    dplyr::n_distinct(
      dados_m$Grupo
    ) < 2 ||
    dplyr::n_distinct(
      dados_m$Compartimento
    ) < 2
  ) {
    
    return(
      NULL
    )
    
  }
  
  
  # ----------------------------------------------------------
  # Modelo misto
  # ----------------------------------------------------------
  
  modelo <- lmerTest::lmer(
    
    Valor ~
      Compartimento *
      Grupo +
      (1 | Code),
    
    data =
      dados_m,
    
    REML =
      TRUE
    
  )
  
  
  # ----------------------------------------------------------
  # Singularidade
  # ----------------------------------------------------------
  
  singular <- lme4::isSingular(
    
    modelo,
    
    tol = 1e-4
    
  )
  
  
  # ----------------------------------------------------------
  # ANOVA tipo III
  # ----------------------------------------------------------
  
  resultado_anova <- anova(
    
    modelo,
    
    type = 3
    
  ) %>%
    
    as.data.frame() %>%
    
    tibble::rownames_to_column(
      "Efeito"
    ) %>%
    
    dplyr::mutate(
      Marcador =
        marcador
    )
  
  
  # ==========================================================
  # Comparar compartimentos dentro de cada grupo
  # ==========================================================
  
  emm_comp <- emmeans::emmeans(
    
    modelo,
    
    ~ Compartimento |
      Grupo
    
  )
  
  
  posthoc_comp <- pairs(
    
    emm_comp,
    
    adjust =
      "none"
    
  ) %>%
    
    as.data.frame() %>%
    
    dplyr::group_by(
      Grupo
    ) %>%
    
    dplyr::mutate(
      
      p_BH =
        stats::p.adjust(
          p.value,
          method = "BH"
        ),
      
      Significancia_BH =
        converter_significancia(
          p_BH
        )
      
    ) %>%
    
    dplyr::ungroup() %>%
    
    dplyr::mutate(
      Marcador =
        marcador
    )
  
  
  # ----------------------------------------------------------
  # Médias marginais
  # ----------------------------------------------------------
  
  medias_marginais <-
    emmeans::emmeans(
      
      modelo,
      
      ~ Grupo *
        Compartimento
      
    ) %>%
    
    as.data.frame() %>%
    
    dplyr::mutate(
      Marcador =
        marcador
    )
  
  
  # ----------------------------------------------------------
  # Retorno
  # ----------------------------------------------------------
  
  list(
    
    modelo =
      modelo,
    
    anova =
      resultado_anova,
    
    posthoc_comp =
      posthoc_comp,
    
    emmeans =
      medias_marginais,
    
    singular =
      singular
    
  )
  
}


# ============================================================
# 24. RODAR MODELOS
# ============================================================

resultados_modelos <- purrr::map(
  
  marcadores,
  
  function(m) {
    
    tryCatch(
      
      analisar_modelo_misto(
        
        df = dados_long,
        
        marcador = m
        
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
  resultados_modelos
) <- marcadores


# ============================================================
# 25. MODELOS VÁLIDOS
# ============================================================

resultados_validos <-
  resultados_modelos[
    
    !vapply(
      
      resultados_modelos,
      
      is.null,
      
      logical(1)
      
    )
    
  ]


# ============================================================
# 26. JUNTAR ANOVA
# ============================================================

resultado_anova <- purrr::map_dfr(
  
  resultados_validos,
  
  "anova"
  
)


# ============================================================
# 27. JUNTAR COMPARAÇÕES ENTRE COMPARTIMENTOS
# ============================================================

resultado_compartimentos <-
  purrr::map_dfr(
    
    resultados_validos,
    
    "posthoc_comp"
    
  )


# ============================================================
# 28. JUNTAR EMMEANS
# ============================================================

resultado_emmeans <- purrr::map_dfr(
  
  resultados_validos,
  
  "emmeans"
  
)


# ============================================================
# 29. SIGNIFICATIVOS ENTRE COMPARTIMENTOS
# ============================================================

compartimentos_significativos <-
  resultado_compartimentos %>%
  
  dplyr::filter(
    p_BH < 0.05
  )


# ============================================================
# 30. DIAGNÓSTICO DOS MODELOS
# ============================================================

diagnostico_modelos <-
  purrr::imap_dfr(
    
    resultados_validos,
    
    function(
    resultado,
    marcador
    ) {
      
      vc <- as.data.frame(
        
        lme4::VarCorr(
          resultado$modelo
        )
        
      )
      
      
      tibble::tibble(
        
        Marcador =
          marcador,
        
        Singular =
          resultado$singular,
        
        Variancia_Code =
          ifelse(
            nrow(vc) > 0,
            vc$vcov[1],
            NA_real_
          )
        
      )
      
    }
    
  )


# ============================================================
# ============================================================
#
# PARTE C
#
# GRÁFICOS
#
# ============================================================
# ============================================================


# ============================================================
# 31. NOMES DOS COMPARTIMENTOS
# ============================================================

nomes_compartimentos <- c(
  
  "Mother" =
    "Maternal\nblood",
  
  "Cord" =
    "Umbilical cord\nblood",
  
  "Decidua" =
    "Decidual\ntissue"
  
)


# ============================================================
# 32. CONFIGURAÇÃO INDIVIDUAL DOS EIXOS
# ============================================================
#
# Pode deixar vazio e o eixo será automático.
#
# Depois, se quiser ajustar um marcador:
#
# config_graficos <- list(
#
#   "CD45+CD3+CD4+CTLA4+" = list(
#     ymin = 0,
#     ymax = 5,
#     breaks = seq(0, 5, 1)
#   ),
#
#   "CD45+CD3+CD4+CD28+" = list(
#     ymin = 0,
#     ymax = 120,
#     breaks = seq(0, 120, 20)
#   )
#
# )
#
# ============================================================

config_graficos <- list()


# ============================================================
# 33. PEGAR CONFIGURAÇÃO DO EIXO
# ============================================================

obter_config <- function(
    marcador
) {
  
  config <-
    config_graficos[[marcador]]
  if (
    is.null(
      config
    )
  ) {
    
    config <- list(
      
      ymin = 0,
      
      ymax = NULL,
      
      breaks = NULL
      
    )
    
  }
  
  
  config
  
}


# ============================================================
# 34. SEPARAR CONTRASTE DE COMPARTIMENTOS
# ============================================================
#
# "Mother - Cord"
#
# vira:
#
# Mother
# Cord
#
# ============================================================

separar_contraste_compartimento <-
  function(
    contraste
  ) {
    
    partes <-
      stringr::str_split_fixed(
        
        contraste,
        
        " - ",
        
        2
        
      )
    
    
    tibble::tibble(
      
      Compartimento1 =
        partes[, 1],
      
      Compartimento2 =
        partes[, 2]
      
    )
    
  }


# ============================================================
# 35. FUNÇÃO DO GRÁFICO
# ============================================================

criar_grafico_final <- function(
    df,
    marcador
) {
  
  
  # ----------------------------------------------------------
  # Dados
  # ----------------------------------------------------------
  
  dados_plot <- df %>%
    
    dplyr::filter(
      
      Marcador ==
        marcador,
      
      !is.na(
        Valor
      ),
      
      !is.na(
        Grupo
      ),
      
      !is.na(
        Compartimento
      )
      
    )
  
  
  # ----------------------------------------------------------
  # Média + SEM
  # ----------------------------------------------------------
  
  # ==========================================================
  # MÉDIA + SEM
  # ==========================================================
  
  resumo <- dados_plot %>%
    
    dplyr::group_by(
      Compartimento,
      Grupo
    ) %>%
    
    dplyr::summarise(
      
      n = sum(!is.na(Valor)),
      
      media = mean(
        Valor,
        na.rm = TRUE
      ),
      
      sd = stats::sd(
        Valor,
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
  
  # ==========================================================
  # ALTURA DOS DADOS
  # ==========================================================
  
  ymax_dados <- max(
    
    c(
      
      dados_plot$Valor,
      
      resumo$media +
        resumo$sem
      
    ),
    
    na.rm = TRUE
    
  )
  
  if (
    !is.finite(ymax_dados) ||
    ymax_dados <= 0
  ) {
    
    ymax_dados <- 1
    
  }
  
  espaco <- ymax_dados * 0.10
  
  # ==========================================================
  # CTR vs GDM
  # ==========================================================
  #
  # p bruto
  #
  # ==========================================================
  
  estrelas_grupo <-
    resultados_ctr_gdm %>%
    
    dplyr::filter(
      
      Marcador ==
        marcador,
      
      !is.na(
        p_value
      ),
      
      p_value < 0.05
      
    ) %>%
    
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
      
      xpos =
        as.numeric(
          Compartimento
        ),
      
      Significancia =
        "*"
      
    )
  
  
  # ==========================================================
  # TOPO DE CADA COMPARTIMENTO
  # ==========================================================
  
  topo_observado <- dados_plot %>%
    
    dplyr::group_by(
      Compartimento
    ) %>%
    
    dplyr::summarise(
      
      max_observado =
        max(
          Valor,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
      
    )
  
  
  topo_resumo <- resumo %>%
    
    dplyr::group_by(
      Compartimento
    ) %>%
    
    dplyr::summarise(
      
      max_resumo =
        max(
          media + sem,
          na.rm = TRUE
        ),
      
      .groups =
        "drop"
      
    )
  
  
  topo_comp <- topo_observado %>%
    
    dplyr::left_join(
      
      topo_resumo,
      
      by =
        "Compartimento"
      
    ) %>%
    
    dplyr::mutate(
      
      topo =
        pmax(
          
          max_observado,
          
          max_resumo,
          
          na.rm = TRUE
          
        )
      
    )
  
  
  estrelas_grupo <-
    estrelas_grupo %>%
    
    dplyr::left_join(
      
      topo_comp,
      
      by =
        "Compartimento"
      
    ) %>%
    
    dplyr::mutate(
      
      y_barra =
        topo +
        espaco * 0.40,
      
      y_estrela =
        topo +
        espaco * 0.62
      
    )
  
  
  # ==========================================================
  # COMPARAÇÃO ENTRE COMPARTIMENTOS
  # ==========================================================
  #
  # p_BH
  #
  # ==========================================================
  
  comp_sig <-
    resultado_compartimentos %>%
    
    dplyr::filter(
      
      Marcador ==
        marcador,
      
      !is.na(
        p_BH
      ),
      
      p_BH < 0.05
      
    )
  
  
  # ==========================================================
  # PREPARAR BRACKETS ENTRE COMPARTIMENTOS
  # ==========================================================
  
  if (
    nrow(
      comp_sig
    ) > 0
  ) {
    
    
    partes <-
      separar_contraste_compartimento(
        
        comp_sig$contrast
        
      )
    
    
    comp_sig <-
      dplyr::bind_cols(
        
        comp_sig,
        
        partes
        
      )
    
    
    posicoes <- c(
      
      "Mother" = 1,
      
      "Cord" = 2,
      
      "Decidua" = 3
      
    )
    
    
    comp_sig <-
      comp_sig %>%
      
      dplyr::mutate(
        
        x1_base =
          unname(
            posicoes[
              Compartimento1
            ]
          ),
        
        x2_base =
          unname(
            posicoes[
              Compartimento2
            ]
          ),
        
        deslocamento =
          dplyr::if_else(
            
            Grupo ==
              "CTR",
            
            -0.19,
            
            0.19
            
          ),
        
        x1 =
          x1_base +
          deslocamento,
        
        x2 =
          x2_base +
          deslocamento,
        
        amplitude =
          abs(
            x2_base -
              x1_base
          )
        
      ) %>%
      
      # Comparações mais curtas primeiro.
      # As mais longas ficam mais acima.
      
      dplyr::arrange(
        
        amplitude,
        Grupo,
        x1
        
      ) %>%
      
      dplyr::mutate(
        
        ordem =
          dplyr::row_number(),
        
        y_barra =
          ymax_dados +
          espaco *
          (
            1.20 +
              (
                ordem -
                  1
              ) *
              0.55
          ),
        
        y_estrela =
          y_barra +
          espaco * 0.22,
        
        Significancia =
          "*"
        
      )
    
  }
  
  
  # ==========================================================
  # GRÁFICO BASE
  # ==========================================================
  
  p <- ggplot2::ggplot(
    
    dados_plot,
    
    ggplot2::aes(
      
      x =
        Compartimento,
      
      y =
        Valor,
      
      fill =
        Grupo
      
    )
    
  ) +
    
    
    # ========================================================
  # BARRAS
  # ========================================================
  
  ggplot2::geom_col(
    
    data =
      resumo,
    
    ggplot2::aes(
      
      x =
        Compartimento,
      
      y =
        media,
      
      fill =
        Grupo
      
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
    
    
    # ========================================================
  # ERRO = SEM
  # ========================================================
  
  ggplot2::geom_errorbar(
    
    data = resumo,
    
    ggplot2::aes(
      
      x = Compartimento,
      
      ymin = media - sem,
      
      ymax = media + sem,
      
      group = Grupo
      
    ),
    
    inherit.aes = FALSE,
    
    position = ggplot2::position_dodge(
      width = 0.75
    ),
    
    width = 0.15,
    
    linewidth = 0.8,
    
    color = "black"
    
  ) +
    
    # ========================================================
  # PONTOS INDIVIDUAIS
  # ========================================================
  
  ggplot2::geom_point(
    
    ggplot2::aes(
      group =
        Grupo
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
    
    
    # ========================================================
  # CORES
  # ========================================================
  
  ggplot2::scale_fill_manual(
    values =
      cores
  ) +
    
    
    # ========================================================
  # NOMES DOS COMPARTIMENTOS
  # ========================================================
  
  ggplot2::scale_x_discrete(
    labels =
      nomes_compartimentos
  ) +
    
    
    # ========================================================
  # TÍTULOS
  # ========================================================
  
  ggplot2::labs(
    
    x =
      NULL,
    
    y =
      "Frequency (%)",
    
    fill =
      NULL
    
  ) +
    
    
    # ========================================================
  # TEMA
  # ========================================================
  
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
  
  
  # ==========================================================
  # BRACKETS CTR vs GDM
  # ==========================================================
  
  if (
    nrow(
      estrelas_grupo
    ) > 0
  ) {
    
    
    # Linha horizontal
    
    p <- p +
      
      ggplot2::geom_segment(
        
        data =
          estrelas_grupo,
        
        ggplot2::aes(
          
          x =
            xpos - 0.19,
          
          xend =
            xpos + 0.19,
          
          y =
            y_barra,
          
          yend =
            y_barra
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      # Perninha esquerda
      
      ggplot2::geom_segment(
        
        data =
          estrelas_grupo,
        
        ggplot2::aes(
          
          x =
            xpos - 0.19,
          
          xend =
            xpos - 0.19,
          
          y =
            y_barra,
          
          yend =
            y_barra -
            espaco * 0.10
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      # Perninha direita
      
      ggplot2::geom_segment(
        
        data =
          estrelas_grupo,
        
        ggplot2::aes(
          
          x =
            xpos + 0.19,
          
          xend =
            xpos + 0.19,
          
          y =
            y_barra,
          
          yend =
            y_barra -
            espaco * 0.10
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      # Asterisco
      
      ggplot2::geom_text(
        
        data =
          estrelas_grupo,
        
        ggplot2::aes(
          
          x =
            xpos,
          
          y =
            y_estrela,
          
          label =
            Significancia
          
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
  
  
  # ==========================================================
  # BRACKETS ENTRE COMPARTIMENTOS
  # ==========================================================
  
  if (
    nrow(
      comp_sig
    ) > 0
  ) {
    
    
    # Linha horizontal
    
    p <- p +
      
      ggplot2::geom_segment(
        
        data =
          comp_sig,
        
        ggplot2::aes(
          
          x =
            x1,
          
          xend =
            x2,
          
          y =
            y_barra,
          
          yend =
            y_barra
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      # Perninha esquerda
      
      ggplot2::geom_segment(
        
        data =
          comp_sig,
        
        ggplot2::aes(
          
          x =
            x1,
          
          xend =
            x1,
          
          y =
            y_barra,
          
          yend =
            y_barra -
            espaco * 0.12
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      # Perninha direita
      
      ggplot2::geom_segment(
        
        data =
          comp_sig,
        
        ggplot2::aes(
          
          x =
            x2,
          
          xend =
            x2,
          
          y =
            y_barra,
          
          yend =
            y_barra -
            espaco * 0.12
          
        ),
        
        inherit.aes =
          FALSE,
        
        linewidth =
          0.7
        
      ) +
      
      
      # Asterisco
      
      ggplot2::geom_text(
        
        data =
          comp_sig,
        
        ggplot2::aes(
          
          x =
            (
              x1 +
                x2
            ) / 2,
          
          y =
            y_estrela,
          
          label =
            Significancia
          
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
  
  
  # ==========================================================
  # DESCOBRIR MAIOR ANOTAÇÃO
  # ==========================================================
  
  maior_anotacao <-
    ymax_dados
  
  
  if (
    nrow(
      estrelas_grupo
    ) > 0
  ) {
    
    maior_anotacao <- max(
      
      maior_anotacao,
      
      estrelas_grupo$y_estrela,
      
      na.rm = TRUE
      
    )
    
  }
  
  
  if (
    nrow(
      comp_sig
    ) > 0
  ) {
    
    maior_anotacao <- max(
      
      maior_anotacao,
      
      comp_sig$y_estrela,
      
      na.rm = TRUE
      
    )
    
  }
  
  
  # ==========================================================
  # CONFIGURAÇÃO DO EIXO
  # ==========================================================
  
  config <-
    obter_config(
      marcador
    )
  
  
  # ----------------------------------------------------------
  # Breaks automáticos
  # ----------------------------------------------------------
  
  if (
    is.null(
      config$breaks
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
          config$breaks,
        
        expand =
          ggplot2::expansion(
            
            mult = c(
              0,
              0.02
            )
            
          )
        
      )
    
  }
  
  
  # ----------------------------------------------------------
  # Limite superior
  # ----------------------------------------------------------
  
  if (
    is.null(
      config$ymax
    )
  ) {
    
    limite_superior <-
      maior_anotacao +
      espaco * 0.50
    
    
  } else {
    
    limite_superior <-
      max(
        
        config$ymax,
        
        maior_anotacao +
          espaco * 0.30
        
      )
    
  }
  
  
  p <- p +
    
    ggplot2::coord_cartesian(
      
      ylim = c(
        
        config$ymin,
        
        limite_superior
        
      ),
      
      clip =
        "off"
      
    )
  
  
  return(
    p
  )
  
}


# ============================================================
# 36. CRIAR POWERPOINT EDITÁVEL
# ============================================================
#
# Cada gráfico será colocado como vetor editável.
#
# ============================================================

ppt_editavel <-
  officer::read_pptx()


# ============================================================
# 37. GERAR TODOS OS GRÁFICOS
# ============================================================

for (
  m in marcadores
) {
  
  
  n_valores <- dados_long %>%
    
    dplyr::filter(
      
      Marcador ==
        m,
      
      !is.na(
        Valor
      )
      
    ) %>%
    
    nrow()
  
  
  if (
    n_valores == 0
  ) {
    
    next
    
  }
  
  
  # ----------------------------------------------------------
  # Criar gráfico
  # ----------------------------------------------------------
  
  fig <- criar_grafico_final(
    
    df =
      dados_long,
    
    marcador =
      m
    
  )
  
  
  # ----------------------------------------------------------
  # Nome seguro
  # ----------------------------------------------------------
  
  nome_arquivo <- m %>%
    
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
  
  
  # ==========================================================
  # PNG
  # ==========================================================
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        
        pasta_png,
        
        paste0(
          nome_arquivo,
          ".png"
        )
        
      ),
    
    plot =
      fig,
    
    width =
      7,
    
    height =
      5.5,
    
    dpi =
      600
    
  )
  
  
  # ==========================================================
  # TIFF
  # ==========================================================
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        
        pasta_tiff,
        
        paste0(
          nome_arquivo,
          ".tiff"
        )
        
      ),
    
    plot =
      fig,
    
    width =
      7,
    
    height =
      5.5,
    
    dpi =
      600,
    
    compression =
      "lzw"
    
  )
  
  
  # ==========================================================
  # SVG EDITÁVEL
  # ==========================================================
  
  ggplot2::ggsave(
    
    filename =
      file.path(
        
        pasta_svg,
        
        paste0(
          nome_arquivo,
          ".svg"
        )
        
      ),
    
    plot =
      fig,
    
    width =
      7,
    
    height =
      5.5,
    
    device =
      svglite::svglite
    
  )
  
  
  # ==========================================================
  # POWERPOINT EDITÁVEL
  # ==========================================================
  
  ppt_editavel <-
    ppt_editavel %>%
    
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
            fig
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


# ============================================================
# 38. SALVAR POWERPOINT EDITÁVEL
# ============================================================

arquivo_ppt <- file.path(
  
  pasta_resultados,
  
  "Graficos_Citometria_EDITAVEIS.pptx"
  
)


print(
  
  ppt_editavel,
  
  target =
    arquivo_ppt
  
)


# ============================================================
# ============================================================
#
# PARTE D
#
# EXPORTAR RESULTADOS
#
# ============================================================
# ============================================================


# ============================================================
# 39. EXCEL FINAL
# ============================================================

arquivo_excel <- file.path(
  
  pasta_resultados,
  
  "Resultados_Citometria_FINAL.xlsx"
  
)


openxlsx::write.xlsx(
  
  list(
    
    N_por_grupo =
      tabela_n,
    
    Descritiva =
      resultado_descritivo,
    
    CTR_vs_GDM_sem_BH =
      resultados_ctr_gdm,
    
    CTR_vs_GDM_significativos =
      ctr_gdm_significativos,
    
    ANOVA_modelo_misto =
      resultado_anova,
    
    Compartimentos_BH =
      resultado_compartimentos,
    
    Compartimentos_significativos =
      compartimentos_significativos,
    
    EMM =
      resultado_emmeans,
    
    Diagnostico_modelos =
      diagnostico_modelos
    
  ),
  
  file =
    arquivo_excel,
  
  overwrite =
    TRUE
  
)


# ============================================================
# 40. EXPORTAR TABELA COMPLETA EM INGLÊS
# ============================================================
# Este bloco cria uma cópia das tabelas para artigo/suplemento.
# Os objetos usados nas análises NÃO são alterados.
# "CD8+" é mantido internamente na análise porque corresponde
# ao nome original no arquivo-fonte; apenas a cópia exportada
# é apresentada como "CD4-".
# ============================================================

traduzir_compartimento <- function(x) {
  dplyr::recode(
    as.character(x),
    "Mother" = "Maternal blood",
    "Cord" = "Umbilical cord blood",
    "Decidua" = "Decidual tissue",
    .default = as.character(x)
  )
}

corrigir_marcador_exportacao <- function(x) {
  stringr::str_replace_all(as.character(x), "CD8\\+", "CD4-")
}

traduzir_tabela_exportacao <- function(df) {
  out <- df

  if ("Marcador" %in% names(out)) {
    out$Marcador <- corrigir_marcador_exportacao(out$Marcador)
  }

  if ("Compartimento" %in% names(out)) {
    out$Compartimento <- traduzir_compartimento(out$Compartimento)
  }

  if ("contrast" %in% names(out)) {
    out$contrast <- as.character(out$contrast) %>%
      stringr::str_replace_all("Mother", "Maternal blood") %>%
      stringr::str_replace_all("Cord", "Umbilical cord blood") %>%
      stringr::str_replace_all("Decidua", "Decidual tissue")
  }

  if ("Efeito" %in% names(out)) {
    out$Efeito <- dplyr::recode(
      as.character(out$Efeito),
      "Compartimento" = "Compartment",
      "Grupo" = "Group",
      "Compartimento:Grupo" = "Compartment:Group",
      .default = as.character(out$Efeito)
    )
  }

  names(out) <- dplyr::recode(
    names(out),
    "Marcador" = "Marker",
    "Compartimento" = "Compartment",
    "Grupo" = "Group",
    "media" = "Mean",
    "sd" = "SD",
    "mediana" = "Median",
    "minimo" = "Minimum",
    "maximo" = "Maximum",
    "Media_CTR" = "Mean_CTR",
    "Media_GDM" = "Mean_GDM",
    "Mediana_CTR" = "Median_CTR",
    "Mediana_GDM" = "Median_GDM",
    "Teste" = "Test",
    "Estatistica" = "Statistic",
    "Significancia" = "Significance",
    "Direcao" = "Direction",
    "Efeito" = "Effect",
    "Significancia_BH" = "BH_significance",
    "Singular" = "Singular_fit",
    "Variancia_Code" = "Code_variance",
    .default = names(out)
  )

  out
}

# Descriptive table with SEM added explicitly
resultado_descritivo_EN <- resultado_descritivo %>%
  dplyr::mutate(SEM = sd / sqrt(n)) %>%
  traduzir_tabela_exportacao()

tabela_n_EN <- traduzir_tabela_exportacao(tabela_n)
resultados_ctr_gdm_EN <- traduzir_tabela_exportacao(resultados_ctr_gdm)
ctr_gdm_significativos_EN <- traduzir_tabela_exportacao(ctr_gdm_significativos)
resultado_anova_EN <- traduzir_tabela_exportacao(resultado_anova)
resultado_compartimentos_EN <- traduzir_tabela_exportacao(resultado_compartimentos)
compartimentos_significativos_EN <- traduzir_tabela_exportacao(compartimentos_significativos)
resultado_emmeans_EN <- traduzir_tabela_exportacao(resultado_emmeans)
diagnostico_modelos_EN <- traduzir_tabela_exportacao(diagnostico_modelos)

arquivo_excel_ingles <- file.path(
  pasta_resultados,
  "Flow_Cytometry_Complete_Results.xlsx"
)

openxlsx::write.xlsx(
  list(
    Sample_size = tabela_n_EN,
    Descriptive_statistics = resultado_descritivo_EN,
    CTR_vs_GDM_nominal_p = resultados_ctr_gdm_EN,
    CTR_vs_GDM_significant = ctr_gdm_significativos_EN,
    Mixed_model_ANOVA = resultado_anova_EN,
    Compartment_posthoc_BH = resultado_compartimentos_EN,
    Significant_compartment_BH = compartimentos_significativos_EN,
    Estimated_marginal_means = resultado_emmeans_EN,
    Model_diagnostics = diagnostico_modelos_EN
  ),
  file = arquivo_excel_ingles,
  overwrite = TRUE
)

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(svglite)

set.seed(123)

setwd(
  "C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros/figuras citometro")

# ============================================================
# ARQUIVOS
# ============================================================

arquivo_dados <- "Analise citometro.xls"

arquivo_resultados <- file.path(
  "Resultados_Citometria_FINAL",
  "Resultados_Citometria_FINAL.xlsx"
)

# ============================================================
# PASTA NOVA APENAS PARA FIGURAS
# ============================================================

pasta_figuras <- "Figuras_Citometria_FINAL"

pasta_png <- file.path(
  pasta_figuras,
  "PNG"
)

pasta_tiff <- file.path(
  pasta_figuras,
  "TIFF"
)

pasta_svg <- file.path(
  pasta_figuras,
  "SVG_EDITAVEIS"
)

dir.create(
  pasta_figuras,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  pasta_png,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  pasta_tiff,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  pasta_svg,
  recursive = TRUE,
  showWarnings = FALSE
)
# ============================================================
# CARREGAR E ORGANIZAR DADOS DA CITOMETRIA
# ============================================================

# ------------------------------------------------------------
# 1. LER ARQUIVO ORIGINAL
# ------------------------------------------------------------

dados_temp <- readxl::read_excel(
  arquivo_dados,
  sheet = 1,
  col_names = TRUE,
  .name_repair = "unique"
)

# ------------------------------------------------------------
# 2. COLUNAS DA CITOMETRIA
# ------------------------------------------------------------

colunas_citometria <- names(dados_temp)[3:ncol(dados_temp)]

# ------------------------------------------------------------
# 3. PRIMEIRA LINHA CONTÉM OS NOMES DOS MARCADORES
# ------------------------------------------------------------

mapa_colunas <- tibble::tibble(
  Coluna = colunas_citometria,
  
  Marcador = as.character(
    unlist(
      dados_temp[
        1,
        colunas_citometria
      ]
    )
  )
) %>%
  
  dplyr::mutate(
    
    Compartimento = dplyr::case_when(
      
      stringr::str_detect(
        Coluna,
        "^Sangue materno"
      ) ~ "Mother",
      
      stringr::str_detect(
        Coluna,
        "^Sangue de cordão"
      ) ~ "Cord",
      
      stringr::str_detect(
        Coluna,
        "^Decídua"
      ) ~ "Decidua",
      
      TRUE ~ NA_character_
    ),
    
    Marcador = stringr::str_trim(
      Marcador
    )
  )

# ------------------------------------------------------------
# 4. REMOVER PRIMEIRA LINHA
# ------------------------------------------------------------

dados_raw <- dados_temp[-1, ]

# ------------------------------------------------------------
# 5. ORGANIZAR ID E GRUPO
# ------------------------------------------------------------

dados_raw <- dados_raw %>%
  
  dplyr::mutate(
    
    Code = trimws(
      as.character(Code)
    ),
    
    Grupo = trimws(
      as.character(`Sample type`)
    ),
    
    Grupo = stringr::str_to_upper(
      Grupo
    )
  ) %>%
  
  dplyr::filter(
    !is.na(Code),
    Grupo %in% c(
      "CTR",
      "GDM"
    )
  ) %>%
  
  dplyr::mutate(
    
    Grupo = factor(
      Grupo,
      levels = c(
        "CTR",
        "GDM"
      )
    ),
    
    Code = factor(Code)
  )

# ------------------------------------------------------------
# 6. TRANSFORMAR PARA FORMATO LONG
# ------------------------------------------------------------

dados_long <- dados_raw %>%
  
  tidyr::pivot_longer(
    
    cols = dplyr::all_of(
      colunas_citometria
    ),
    
    names_to = "Coluna",
    
    values_to = "Valor"
  ) %>%
  
  dplyr::left_join(
    mapa_colunas,
    by = "Coluna"
  ) %>%
  
  dplyr::mutate(
    
    Valor = as.character(
      Valor
    ),
    
    Valor = stringr::str_trim(
      Valor
    ),
    
    Valor = dplyr::na_if(
      Valor,
      ""
    ),
    
    Valor = dplyr::na_if(
      Valor,
      "-"
    ),
    
    Valor = dplyr::na_if(
      Valor,
      "NA"
    ),
    
    Valor = dplyr::na_if(
      Valor,
      "Undetermined"
    ),
    
    Valor = stringr::str_replace_all(
      Valor,
      ",",
      "."
    ),
    
    Valor = suppressWarnings(
      as.numeric(Valor)
    ),
    
    Marcador = stringr::str_trim(
      Marcador
    ),
    
    Compartimento = factor(
      Compartimento,
      levels = c(
        "Mother",
        "Cord",
        "Decidua"
      )
    ),
    
    Grupo = factor(
      Grupo,
      levels = c(
        "CTR",
        "GDM"
      )
    )
  )

# ============================================================
# 7. CARREGAR RESULTADOS ESTATÍSTICOS
# ============================================================

resultados_ctr_gdm <- readxl::read_excel(
  arquivo_resultados,
  sheet = "CTR_vs_GDM_sem_BH"
)

resultado_compartimentos <- readxl::read_excel(
  arquivo_resultados,
  sheet = "Compartimentos_BH"
)

# ============================================================
# 8. CONFERIR
# ============================================================

cat(
  "\nColunas de dados_long:\n"
)

print(
  names(dados_long)
)

cat(
  "\nMarcadores encontrados:\n"
)

print(
  unique(dados_long$Marcador)
)

# ============================================================
# CONFIGURAÇÕES GERAIS
# ============================================================

cores <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)

nomes_compartimentos <- c(
  "Mother" = "Maternal\nblood",
  "Cord" = "Umbilical cord\nblood",
  "Decidua" = "Decidual\ntissue"
)

# ============================================================
# LETRAS DOS COMPARTIMENTOS
# ============================================================

# Letras centrais:
# usadas quando CTR e GDM têm a mesma letra naquele compartimento

letras_centrais <- tibble::tribble(
  
  ~Marcador,                  ~Compartimento, ~Letra,
  
  # CD8+CD28+
  "CD45+CD3+CD8+CD28+",       "Mother",        "b",
  "CD45+CD3+CD8+CD28+",       "Cord",          "a",
  
  # CD8+CTLA4+
  "CD45+CD3+CD8+CTLA4+",      "Mother",        "b",
  "CD45+CD3+CD8+CTLA4+",      "Cord",          "b",
  "CD45+CD3+CD8+CTLA4+",      "Decidua",       "a"
)


# ============================================================
# LETRAS SEPARADAS
# ============================================================
#
# Usadas quando CTR e GDM apresentam letras diferentes
#

letras_separadas <- tibble::tribble(
  
  ~Marcador,                  ~Compartimento, ~Grupo, ~Letra,
  
  # CD8+CD28+ - Decidua
  "CD45+CD3+CD8+CD28+",       "Decidua",       "CTR",  "c",
  "CD45+CD3+CD8+CD28+",       "Decidua",       "GDM",  "b"
)


# ============================================================
# FUNÇÃO PARA CRIAR O GRÁFICO
# ============================================================

criar_grafico_citometria <- function(
    marcador,
    titulo_grafico,
    titulo_y = "Frequency (%)"
) {
  
  # ----------------------------------------------------------
  # 1. DADOS
  # ----------------------------------------------------------
  
  dados_plot <- dados_long %>%
    dplyr::filter(
      Marcador == marcador,
      !is.na(Valor),
      !is.na(Grupo),
      !is.na(Compartimento)
    )
  
  # ----------------------------------------------------------
  # 2. MÉDIA E SEM
  # ----------------------------------------------------------
  
  resumo <- dados_plot %>%
    dplyr::group_by(
      Compartimento,
      Grupo
    ) %>%
    dplyr::summarise(
      
      n = sum(!is.na(Valor)),
      
      media = mean(
        Valor,
        na.rm = TRUE
      ),
      
      sd = stats::sd(
        Valor,
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
  
  # ----------------------------------------------------------
  # 3. TOPO REAL DE CADA BARRA
  # ----------------------------------------------------------
  
  topo_grupo <- dados_plot %>%
    dplyr::group_by(
      Compartimento,
      Grupo
    ) %>%
    dplyr::summarise(
      
      max_observado = max(
        Valor,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ) %>%
    dplyr::left_join(
      resumo,
      by = c(
        "Compartimento",
        "Grupo"
      )
    ) %>%
    dplyr::mutate(
      
      topo = pmax(
        max_observado,
        media + sem,
        na.rm = TRUE
      )
    )
  
  # ----------------------------------------------------------
  # 4. MÁXIMO GERAL
  # ----------------------------------------------------------
  
  max_dados <- max(
    topo_grupo$topo,
    na.rm = TRUE
  )
  
  if (!is.finite(max_dados)) {
    max_dados <- 1
  }
  
  espaco <- max_dados * 0.08
  
  if (espaco <= 0) {
    espaco <- 1
  }
  
  # ==========================================================
  # 5. CTR vs GDM SIGNIFICATIVOS
  # ==========================================================
  
  grupo_sig <- resultados_ctr_gdm %>%
    dplyr::filter(
      Marcador == marcador,
      !is.na(p_value),
      p_value < 0.05
    ) %>%
    dplyr::mutate(
      
      Compartimento = factor(
        Compartimento,
        levels = c(
          "Mother",
          "Cord",
          "Decidua"
        )
      ),
      
      x = as.numeric(
        Compartimento
      )
    )
  
  # ----------------------------------------------------------
  # topo geral por compartimento
  # ----------------------------------------------------------
  
  topo_comp <- topo_grupo %>%
    dplyr::group_by(
      Compartimento
    ) %>%
    dplyr::summarise(
      
      topo_comp = max(
        topo,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
  
  # ----------------------------------------------------------
  # posição do bracket e estrela
  # ----------------------------------------------------------
  
  grupo_sig <- grupo_sig %>%
    dplyr::left_join(
      topo_comp,
      by = "Compartimento"
    ) %>%
    dplyr::mutate(
      y_barra =
        topo_comp +
        espaco * 0.55,
      
      y_estrela =
        topo_comp +
        espaco * 1.10
    )
  
  # ==========================================================
  # 6. LETRAS CENTRAIS
  # ==========================================================
  
  letras_c <- letras_centrais %>%
    dplyr::filter(
      Marcador == marcador
    ) %>%
    dplyr::mutate(
      
      Compartimento = factor(
        Compartimento,
        levels = c(
          "Mother",
          "Cord",
          "Decidua"
        )
      )
    ) %>%
    dplyr::left_join(
      topo_comp,
      by = "Compartimento"
    ) %>%
    dplyr::mutate(
      
      y_letra =
        topo_comp +
        espaco * 1.40
    )
  
  # ==========================================================
  # 7. LETRAS SEPARADAS
  # ==========================================================
  
  letras_s <- letras_separadas %>%
    dplyr::filter(
      Marcador == marcador
    ) %>%
    dplyr::mutate(
      
      Compartimento = factor(
        Compartimento,
        levels = c(
          "Mother",
          "Cord",
          "Decidua"
        )
      ),
      
      Grupo = factor(
        Grupo,
        levels = c(
          "CTR",
          "GDM"
        )
      )
    ) %>%
    dplyr::left_join(
      topo_grupo,
      by = c(
        "Compartimento",
        "Grupo"
      )
    ) %>%
    dplyr::mutate(
      
      y_letra =
        topo +
        espaco * 1.25,
      
      x_letra =
        as.numeric(Compartimento) +
        dplyr::if_else(
          Grupo == "CTR",
          -0.19,
          0.19
        )
    )
  
  # ==========================================================
  # 8. EVITAR LETRA EM CIMA DA ESTRELA
  # ==========================================================
  
  if (nrow(grupo_sig) > 0) {
    
    letras_c <- letras_c %>%
      dplyr::left_join(
        
        grupo_sig %>%
          dplyr::select(
            Compartimento,
            y_estrela
          ),
        
        by = "Compartimento"
      ) %>%
      dplyr::mutate(
        
        y_letra = dplyr::if_else(
          !is.na(y_estrela),
          y_estrela + espaco * 1.50,
          y_letra
        )
      ) %>%
      dplyr::select(
        -y_estrela
      )
    
    letras_s <- letras_s %>%
      dplyr::left_join(
        
        grupo_sig %>%
          dplyr::select(
            Compartimento,
            y_estrela
          ),
        
        by = "Compartimento"
      ) %>%
      dplyr::mutate(
        
        y_letra = dplyr::if_else(
          !is.na(y_estrela),
          y_estrela + espaco * 0.90,
          y_letra
        )
      ) %>%
      dplyr::select(
        -y_estrela
      )
  }
  
  # ==========================================================
  # 9. LIMITE SUPERIOR AUTOMÁTICO
  # ==========================================================
  
  valores_topo <- c(
    max_dados
  )
  
  if (nrow(grupo_sig) > 0) {
    
    valores_topo <- c(
      valores_topo,
      grupo_sig$y_estrela
    )
  }
  
  if (nrow(letras_c) > 0) {
    
    valores_topo <- c(
      valores_topo,
      letras_c$y_letra
    )
  }
  
  if (nrow(letras_s) > 0) {
    
    valores_topo <- c(
      valores_topo,
      letras_s$y_letra
    )
  }
  
  limite_superior <- max(
    valores_topo,
    na.rm = TRUE
  ) + espaco * 1.00
  
  # ----------------------------------------------------------
  # arredondar eixo
  # ----------------------------------------------------------
  
  if (limite_superior <= 5) {
    
    limite_superior <-
      ceiling(
        limite_superior * 2
      ) / 2
    
  } else if (limite_superior <= 20) {
    
    limite_superior <-
      ceiling(
        limite_superior
      )
    
  } else {
    
    limite_superior <-
      ceiling(
        limite_superior / 10
      ) * 10
  }
  
  # ==========================================================
  # 10. GRÁFICO BASE
  # ==========================================================
  
  p <- ggplot2::ggplot(
    dados_plot,
    ggplot2::aes(
      x = Compartimento,
      y = Valor,
      fill = Grupo
    )
  ) +
    
    # --------------------------------------------------------
  # barras
  # --------------------------------------------------------
  
  ggplot2::geom_col(
    
    data = resumo,
    
    ggplot2::aes(
      x = Compartimento,
      y = media,
      fill = Grupo
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
    
    # --------------------------------------------------------
  # SEM
  # --------------------------------------------------------
  
  ggplot2::geom_errorbar(
    
    data = resumo,
    
    ggplot2::aes(
      x = Compartimento,
      ymin = media - sem,
      ymax = media + sem,
      group = Grupo
    ),
    
    inherit.aes = FALSE,
    
    position = ggplot2::position_dodge(
      width = 0.75
    ),
    
    width = 0.15,
    
    linewidth = 0.8,
    
    color = "black"
  ) +
    
    # --------------------------------------------------------
  # pontos individuais
  # --------------------------------------------------------
  
  ggplot2::geom_point(
    
    ggplot2::aes(
      group = Grupo
    ),
    
    position = ggplot2::position_jitterdodge(
      jitter.width = 0.08,
      dodge.width = 0.75
    ),
    
    size = 2.7,
    
    shape = 16,
    
    color = "black"
  ) +
    
    # --------------------------------------------------------
  # cores
  # --------------------------------------------------------
  
  ggplot2::scale_fill_manual(
    values = cores
  ) +
    
    # --------------------------------------------------------
  # eixo X
  # --------------------------------------------------------
  
  ggplot2::scale_x_discrete(
    labels = nomes_compartimentos
  ) +
    
    # --------------------------------------------------------
  # eixo Y
  # --------------------------------------------------------
  
  ggplot2::scale_y_continuous(
    
    limits = c(
      0,
      limite_superior
    ),
    
    expand = ggplot2::expansion(
      mult = c(
        0,
        0
      )
    )
  ) +
    
    # --------------------------------------------------------
  # títulos
  # --------------------------------------------------------
  
  ggplot2::labs(
    x = NULL,
    y = titulo_y,
    title = titulo_grafico
  ) +
    
    # --------------------------------------------------------
  # tema
  # --------------------------------------------------------
  
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
  
  # ==========================================================
  # 11. BRACKET CTR vs GDM
  # ==========================================================
  
  if (nrow(grupo_sig) > 0) {
    
    p <- p +
      
      # linha horizontal
      ggplot2::geom_segment(
        
        data = grupo_sig,
        
        ggplot2::aes(
          x = x - 0.19,
          xend = x + 0.19,
          y = y_barra,
          yend = y_barra
        ),
        
        inherit.aes = FALSE,
        
        linewidth = 0.7
      ) +
      
      # ponta esquerda
      ggplot2::geom_segment(
        
        data = grupo_sig,
        
        ggplot2::aes(
          x = x - 0.19,
          xend = x - 0.19,
          y = y_barra,
          yend = y_barra - espaco * 0.15
        ),
        
        inherit.aes = FALSE,
        
        linewidth = 0.7
      ) +
      
      # ponta direita
      ggplot2::geom_segment(
        
        data = grupo_sig,
        
        ggplot2::aes(
          x = x + 0.19,
          xend = x + 0.19,
          y = y_barra,
          yend = y_barra - espaco * 0.15
        ),
        
        inherit.aes = FALSE,
        
        linewidth = 0.7
      ) +
      
      # estrela
      ggplot2::geom_text(
        
        data = grupo_sig,
        
        ggplot2::aes(
          x = x,
          y = y_estrela,
          label = "*"
        ),
        
        inherit.aes = FALSE,
        
        family = "Arial",
        
        fontface = "bold",
        
        size = 8
      )
  }
  
  # ==========================================================
  # 12. LETRAS CENTRAIS
  # ==========================================================
  
  if (nrow(letras_c) > 0) {
    
    p <- p +
      
      ggplot2::geom_text(
        
        data = letras_c,
        
        ggplot2::aes(
          x = Compartimento,
          y = y_letra,
          label = Letra
        ),
        
        inherit.aes = FALSE,
        
        family = "Arial",
        
        fontface = "bold",
        
        size = 6
      )
  }
  
  # ==========================================================
  # 13. LETRAS SEPARADAS CTR / GDM
  # ==========================================================
  
  if (nrow(letras_s) > 0) {
    
    p <- p +
      
      ggplot2::geom_text(
        
        data = letras_s,
        
        ggplot2::aes(
          x = x_letra,
          y = y_letra,
          label = Letra
        ),
        
        inherit.aes = FALSE,
        
        family = "Arial",
        
        fontface = "bold",
        
        size = 6
      )
  }
  
  return(p)
}

# ============================================================
# CRIAR FIGURA CD8+CD28+
# ============================================================

fig_cd28 <- criar_grafico_citometria(
  marcador = "CD45+CD3+CD8+CD28+",
  titulo_grafico = "CD45+CD3+CD4-CD28+"
)

fig_cd28


# ============================================================
# CRIAR FIGURA CD8+CTLA4+
# ============================================================

fig_ctla4 <- criar_grafico_citometria(
  marcador = "CD45+CD3+CD8+CTLA4+",
  titulo_grafico = "CD45+CD3+CD4-CTLA4+"
)

fig_ctla4


# ============================================================
# SALVAR GRÁFICOS FINAIS DA CITOMETRIA
# ============================================================

pasta_figuras <- "Figuras_Citometria_FINAL"

pasta_png <- file.path(
  pasta_figuras,
  "PNG"
)

pasta_tiff <- file.path(
  pasta_figuras,
  "TIFF"
)

pasta_svg <- file.path(
  pasta_figuras,
  "SVG_EDITAVEIS"
)

dir.create(
  pasta_png,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  pasta_tiff,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  pasta_svg,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# SALVAR CD45+CD3+CD8+CD28+
# ============================================================

ggplot2::ggsave(
  filename = file.path(
    pasta_png,
    "CD45_CD3_CD4neg_CD28.png"
  ),
  plot = fig_cd28,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_tiff,
    "CD45_CD3_CD4neg_CD28.tiff"
  ),
  plot = fig_cd28,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_svg,
    "CD45_CD3_CD4neg_CD28.svg"
  ),
  plot = fig_cd28,
  width = 7,
  height = 5.5,
  units = "in",
  device = svglite::svglite,
  bg = "white"
)


# ============================================================
# SALVAR CD45+CD3+CD8+CTLA4+
# ============================================================

ggplot2::ggsave(
  filename = file.path(
    pasta_png,
    "CD45_CD3_CD4neg_CTLA4.png"
  ),
  plot = fig_ctla4,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_tiff,
    "CD45_CD3_CD4neg_CTLA4.tiff"
  ),
  plot = fig_ctla4,
  width = 7,
  height = 5.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_svg,
    "CD45_CD3_CD4neg_CTLA4.svg"
  ),
  plot = fig_ctla4,
  width = 7,
  height = 5.5,
  units = "in",
  device = svglite::svglite,
  bg = "white"
)


# ============================================================
# CONFIRMAR ONDE FORAM SALVOS
# ============================================================

cat(
  "\nGráficos salvos em:\n",
  normalizePath(pasta_figuras),
  "\n"
)





# ============================================================
# END OF FLOW-CYTOMETRY WORKFLOW
# ============================================================


# ============================================================
# START OF miRNA WORKFLOW
# ============================================================

# ============================================================
# miRNA ANALYSIS - COMPLETE FINAL SCRIPT
# ============================================================
#
# Analyses included:
#   1. CTR vs GDM in maternal blood
#   2. CTR vs GDM in umbilical cord blood
#   3. CTR vs GDM in decidual tissue
#   4. Compartment analysis using linear mixed-effects models
#
# Statistical strategy:
#   - CTR vs GDM within each compartment:
#       Shapiro-Wilk -> two-sided Welch t-test or Mann-Whitney
#       Nominal p-values (no multiple-testing correction)
#
#   - Comparisons among compartments:
#       Value ~ Compartment * Group + (1 | Code)
#       emmeans post hoc comparisons with Benjamini-Hochberg correction
#
# Figures:
#   Mean ± SEM + individual observations
#   Y-axis limits and breaks are determined automatically from the plotted data
#   Figure typography and dimensions standardized to the final flow-cytometry figures
#   Compartment letters = BH-adjusted mixed-model post hoc comparisons
#   CTR vs GDM stars = nominal p from Welch t-test / Mann-Whitney
#
# Compartment terminology:
#   Maternal blood
#   Umbilical cord blood
#   Decidual tissue
#
# NOTE:
# The four original analysis sections are kept sequentially in this
# consolidated file so that their original input files and analysis
# logic remain unchanged.
# ============================================================



# ============================================================
# PART 1 - MATERNAL BLOOD: CTR VS GDM
# ============================================================

# ============================================================
# ANÁLISE miRNA - MATERNAL BLOOD
# CTR vs GDM
# Shapiro -> Welch t-test ou Mann-Whitney
# ============================================================

library(tidyverse)
library(writexl)

setwd("C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros/analise micros")


# ============================================================
# 1. LER DADOS
# ============================================================

dados <- read.csv(
  "mir.maes.csv",
  sep = ";",
  dec = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ============================================================
# 2. TRANSFORMAR PARA FORMATO LONGO
# ============================================================

dados_long <- dados %>%
  
  mutate(
    Group = str_trim(as.character(Group)),
    Group = str_to_upper(Group)
  ) %>%
  
  filter(
    Group %in% c("CTR", "GDM")
  ) %>%
  
  pivot_longer(
    cols = -c(Group, Sample),
    names_to = "miRNA",
    values_to = "Valor"
  ) %>%
  
  mutate(
    
    Valor = as.character(Valor),
    
    Valor = na_if(Valor, ""),
    Valor = na_if(Valor, "NA"),
    Valor = na_if(Valor, "Undetermined"),
    Valor = na_if(Valor, "-"),
    
    Valor = str_replace_all(
      Valor,
      ",",
      "."
    ),
    
    Valor = suppressWarnings(
      as.numeric(Valor)
    ),
    
    Group = factor(
      Group,
      levels = c("CTR", "GDM")
    )
  )


# ============================================================
# 3. FUNÇÃO DE ANÁLISE
# ============================================================

analisar_miRNA <- function(df, mirna) {
  
  dados_mir <- df %>%
    filter(
      miRNA == mirna,
      !is.na(Valor),
      !is.na(Group)
    )
  
  
  ctr <- dados_mir %>%
    filter(Group == "CTR") %>%
    pull(Valor)
  
  gdm <- dados_mir %>%
    filter(Group == "GDM") %>%
    pull(Valor)
  
  
  n_ctr <- length(ctr)
  n_gdm <- length(gdm)
  
  
  # ----------------------------------------------------------
  # Shapiro-Wilk
  # ----------------------------------------------------------
  
  shapiro_ctr <- if (n_ctr >= 3) {
    shapiro.test(ctr)$p.value
  } else {
    NA_real_
  }
  
  
  shapiro_gdm <- if (n_gdm >= 3) {
    shapiro.test(gdm)$p.value
  } else {
    NA_real_
  }
  
  
  # ----------------------------------------------------------
  # ESCOLHA DO TESTE
  # ----------------------------------------------------------
  
  normal <- !is.na(shapiro_ctr) &&
    !is.na(shapiro_gdm) &&
    shapiro_ctr > 0.05 &&
    shapiro_gdm > 0.05
  
  
  if (normal) {
    
    # Welch t-test bilateral
    teste <- t.test(
      ctr,
      gdm,
      alternative = "two.sided",
      var.equal = FALSE
    )
    
    nome_teste <- "Welch t-test"
    estatistica <- as.numeric(teste$statistic)
    
  } else {
    
    # Mann-Whitney bilateral
    teste <- wilcox.test(
      ctr,
      gdm,
      alternative = "two.sided",
      exact = FALSE
    )
    
    nome_teste <- "Mann-Whitney"
    estatistica <- as.numeric(teste$statistic)
  }
  
  
  # ----------------------------------------------------------
  # RESULTADOS
  # ----------------------------------------------------------
  
  tibble(
    
    miRNA = mirna,
    
    n_CTR = n_ctr,
    n_GDM = n_gdm,
    
    Media_CTR = mean(ctr, na.rm = TRUE),
    Media_GDM = mean(gdm, na.rm = TRUE),
    
    Mediana_CTR = median(ctr, na.rm = TRUE),
    Mediana_GDM = median(gdm, na.rm = TRUE),
    
    Shapiro_CTR_p = shapiro_ctr,
    Shapiro_GDM_p = shapiro_gdm,
    
    Teste = nome_teste,
    
    Estatistica = estatistica,
    
    p_value = teste$p.value
  )
}


# ============================================================
# 4. RODAR ANÁLISE PARA TODOS OS miRNAs
# ============================================================

moleculas <- unique(dados_long$miRNA)

resultados <- map_dfr(
  moleculas,
  ~ analisar_miRNA(dados_long, .x)
)



resultados_maternal <- resultados |>
  dplyr::mutate(
    Compartimento = "Mother"
  )

# CTR vs GDM comparisons are reported using nominal two-sided p-values,
# without multiple-testing correction, matching the flow-cytometry analysis.


# ============================================================
# 6. VER RESULTADOS
# ============================================================

resultados


# ============================================================
# 7. EXPORTAR RESULTADOS
# ============================================================

write_xlsx(
  resultados,
  "Results_Maternal_Blood_CTR_vs_GDM.xlsx"
)


# ============================================================
# 8. CONFIGURAÇÕES DOS GRÁFICOS
# ============================================================

# Cores dos grupos
cores <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)


# Tema
theme_padrao <- theme_classic(base_family = "Arial") +
  theme(

    axis.text.x = element_text(
      color = "black",
      size = 16
    ),

    axis.text.y = element_text(
      color = "black",
      size = 14
    ),

    axis.title.x = element_blank(),

    axis.title.y = element_text(
      color = "black",
      size = 16
    ),

    plot.title = element_blank(),

    legend.position = "none",

    axis.line = element_line(
      color = "black",
      linewidth = 0.8
    ),

    axis.ticks = element_line(
      color = "black"
    ),

    plot.margin = margin(
      15, 15, 10, 10
    )
  )


# Criar pasta para os gráficos
dir.create(
  "Figures_Maternal_Blood",
  showWarnings = FALSE
)


# ============================================================
# 9. FUNÇÃO PARA CRIAR OS GRÁFICOS
#
# Barra = média
# Erro = SEM
# Pontos pretos = valores individuais
# ============================================================

criar_grafico_miRNA <- function(
    df,
    mirna,
    ylab,
    nome_base,
    y_limits,
    y_breaks
) {
  
  dados_plot <- df %>%
    filter(
      miRNA == mirna,
      !is.na(Valor),
      !is.na(Group)
    )
  
  
  p <- ggplot(
    dados_plot,
    aes(
      x = Group,
      y = Valor,
      fill = Group
    )
  ) +
    
    # Barra = média
    stat_summary(
      fun = mean,
      geom = "bar",
      width = 0.6,
      alpha = 0.7,
      color = "black",
      linewidth = 0.7
    ) +
    
    # Barra de erro = média ± SEM
    stat_summary(
      fun.data = mean_se,
      fun.args = list(mult = 1),
      geom = "errorbar",
      width = 0.15,
      linewidth = 0.8,
      color = "black"
    ) +
    
    # Valores individuais
    geom_jitter(
      width = 0.10,
      size = 2.7,
      shape = 16,
      color = "black"
    ) +
    
    # Cores
    scale_fill_manual(
      values = cores
    ) +
    
    # Marcas do eixo Y
    scale_y_continuous(
      breaks = y_breaks,
      expand = expansion(
        mult = c(0, 0.05)
      )
    ) +
    
    # Limites visuais do eixo Y
    coord_cartesian(
      ylim = y_limits
    ) +
    
    # Título do eixo Y
    labs(
      x = NULL,
      y = ylab
    ) +
    
    theme_padrao
  
  
  # ----------------------------------------------------------
  # SALVAR TIFF
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(
      "Figures_Maternal_Blood",
      paste0(nome_base, ".tiff")
    ),
    plot = p,
    width = 7,
    height = 5.5,
    dpi = 600,
    compression = "lzw"
  )
  
  
  # ----------------------------------------------------------
  # SALVAR PNG
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(
      "Figures_Maternal_Blood",
      paste0(nome_base, ".png")
    ),
    plot = p,
    width = 7,
    height = 5.5,
    dpi = 600
  )
  
  
  return(p)
}


# ============================================================
# 10. GRÁFICOS - MÃES
# ============================================================


# ------------------------------------------------------------
# miR-39
# ------------------------------------------------------------

fig_mir39 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR- 39",
  ylab = "CT",
  nome_base = "Maternal_Blood_miR_39_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-29a
# ------------------------------------------------------------

fig_mir29a <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 29a",
  ylab = "Relative expression (fold change)",
  nome_base = "Maternal_Blood_miR_29a_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-132
# ------------------------------------------------------------

fig_mir132 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 132",
  ylab = "Relative expression (fold change)",
  nome_base = "Maternal_Blood_miR_132_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-150-5p
# ------------------------------------------------------------

fig_mir150 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 150-5p",
  ylab = "Relative expression (fold change)",
  nome_base = "Maternal_Blood_miR_150-5p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-222-3p
# ------------------------------------------------------------

fig_mir222 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 222-3p",
  ylab = "Relative expression (fold change)",
  nome_base = "Maternal_Blood_miR_222-3p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ============================================================
# PART 2 - UMBILICAL CORD BLOOD: CTR VS GDM
# ============================================================

# ============================================================
# ANÁLISE miRNA - UMBILICAL CORD BLOOD
# CTR vs GDM
# Shapiro -> Welch t-test ou Mann-Whitney
# ============================================================

library(tidyverse)
library(writexl)

setwd("C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros/analise micros")


# ============================================================
# 1. LER DADOS
# ============================================================

dados <- read.csv(
  "mir.cordao.csv",
  sep = ";",
  dec = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ============================================================
# 2. TRANSFORMAR PARA FORMATO LONGO
# ============================================================

dados_long <- dados %>%
  
  mutate(
    Group = str_trim(as.character(Group)),
    Group = str_to_upper(Group)
  ) %>%
  
  filter(
    Group %in% c("CTR", "GDM")
  ) %>%
  
  pivot_longer(
    cols = -c(Group, Sample),
    names_to = "miRNA",
    values_to = "Valor"
  ) %>%
  
  mutate(
    
    Valor = as.character(Valor),
    
    Valor = na_if(Valor, ""),
    Valor = na_if(Valor, "NA"),
    Valor = na_if(Valor, "Undetermined"),
    Valor = na_if(Valor, "-"),
    
    Valor = str_replace_all(
      Valor,
      ",",
      "."
    ),
    
    Valor = suppressWarnings(
      as.numeric(Valor)
    ),
    
    Group = factor(
      Group,
      levels = c("CTR", "GDM")
    )
  )


# ============================================================
# 3. CONFERIR OS DADOS
# ============================================================

unique(dados_long$miRNA)

dados_long %>%
  count(miRNA, Group)


# ============================================================
# 4. FUNÇÃO DE ANÁLISE
# ============================================================

analisar_miRNA <- function(df, mirna) {
  
  dados_mir <- df %>%
    filter(
      miRNA == mirna,
      !is.na(Valor),
      !is.na(Group)
    )
  
  
  ctr <- dados_mir %>%
    filter(Group == "CTR") %>%
    pull(Valor)
  
  gdm <- dados_mir %>%
    filter(Group == "GDM") %>%
    pull(Valor)
  
  
  n_ctr <- length(ctr)
  n_gdm <- length(gdm)
  
  
  # ----------------------------------------------------------
  # Shapiro-Wilk
  #
  # Só roda quando:
  # n >= 3
  # E existem pelo menos dois valores diferentes
  # ----------------------------------------------------------
  
  shapiro_ctr <- if (
    n_ctr >= 3 &&
    length(unique(ctr)) > 1
  ) {
    
    shapiro.test(ctr)$p.value
    
  } else {
    
    NA_real_
    
  }
  
  
  shapiro_gdm <- if (
    n_gdm >= 3 &&
    length(unique(gdm)) > 1
  ) {
    
    shapiro.test(gdm)$p.value
    
  } else {
    
    NA_real_
    
  }
  
  
  # ----------------------------------------------------------
  # ESCOLHA DO TESTE
  # ----------------------------------------------------------
  
  normal <- !is.na(shapiro_ctr) &&
    !is.na(shapiro_gdm) &&
    shapiro_ctr > 0.05 &&
    shapiro_gdm > 0.05
  
  
  if (normal) {
    
    # Welch t-test bilateral
    
    teste <- t.test(
      ctr,
      gdm,
      alternative = "two.sided",
      var.equal = FALSE
    )
    
    nome_teste <- "Welch t-test"
    estatistica <- as.numeric(teste$statistic)
    
    
  } else {
    
    # Mann-Whitney bilateral
    
    teste <- wilcox.test(
      ctr,
      gdm,
      alternative = "two.sided",
      exact = FALSE
    )
    
    nome_teste <- "Mann-Whitney"
    estatistica <- as.numeric(teste$statistic)
  }
  
  
  # ----------------------------------------------------------
  # RESULTADOS
  # ----------------------------------------------------------
  
  tibble(
    
    miRNA = mirna,
    
    n_CTR = n_ctr,
    n_GDM = n_gdm,
    
    Media_CTR = mean(ctr, na.rm = TRUE),
    Media_GDM = mean(gdm, na.rm = TRUE),
    
    Mediana_CTR = median(ctr, na.rm = TRUE),
    Mediana_GDM = median(gdm, na.rm = TRUE),
    
    Shapiro_CTR_p = shapiro_ctr,
    Shapiro_GDM_p = shapiro_gdm,
    
    Teste = nome_teste,
    
    Estatistica = estatistica,
    
    p_value = teste$p.value
  )
}


# ============================================================
# 5. RODAR ANÁLISE PARA TODOS OS miRNAs
# ============================================================

moleculas <- unique(dados_long$miRNA)

resultados <- map_dfr(
  moleculas,
  ~ analisar_miRNA(dados_long, .x)
)



resultados_cord <- resultados |>
  dplyr::mutate(
    Compartimento = "Cord"
  )

# CTR vs GDM comparisons are reported using nominal two-sided p-values,
# without multiple-testing correction, matching the flow-cytometry analysis.


# ============================================================
# 7. VER RESULTADOS
# ============================================================

resultados


# ============================================================
# 8. EXPORTAR RESULTADOS
# ============================================================

write_xlsx(
  resultados,
  "Results_Umbilical_Cord_Blood_CTR_vs_GDM.xlsx"
)


# ============================================================
# 9. CONFIGURAÇÕES DOS GRÁFICOS
# ============================================================

# Cores dos grupos

cores <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)


# Tema

theme_padrao <- theme_classic(base_family = "Arial") +
  theme(

    axis.text.x = element_text(
      color = "black",
      size = 16
    ),

    axis.text.y = element_text(
      color = "black",
      size = 14
    ),

    axis.title.x = element_blank(),

    axis.title.y = element_text(
      color = "black",
      size = 16
    ),

    plot.title = element_blank(),

    legend.position = "none",

    axis.line = element_line(
      color = "black",
      linewidth = 0.8
    ),

    axis.ticks = element_line(
      color = "black"
    ),

    plot.margin = margin(
      15, 15, 10, 10
    )
  )


# Criar pasta para os gráficos

dir.create(
  "Figures_Umbilical_Cord_Blood",
  showWarnings = FALSE
)


# ============================================================
# 10. FUNÇÃO PARA CRIAR OS GRÁFICOS
#
# Barra = média
# Erro = SEM
# Pontos pretos = valores individuais
# ============================================================

criar_grafico_miRNA <- function(
    df,
    mirna,
    ylab,
    nome_base,
    y_limits,
    y_breaks
) {
  
  dados_plot <- df %>%
    filter(
      miRNA == mirna,
      !is.na(Valor),
      !is.na(Group)
    )
  
  
  p <- ggplot(
    dados_plot,
    aes(
      x = Group,
      y = Valor,
      fill = Group
    )
  ) +
    
    # --------------------------------------------------------
  # Barra = média
  # --------------------------------------------------------
  
  stat_summary(
    fun = mean,
    geom = "bar",
    width = 0.6,
    alpha = 0.7,
    color = "black",
    linewidth = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # Barra de erro = média ± SEM
  # --------------------------------------------------------
  
  stat_summary(
    fun.data = mean_se,
    fun.args = list(mult = 1),
    geom = "errorbar",
    width = 0.15,
    linewidth = 0.9,
    color = "black"
  ) +
    
    
    # --------------------------------------------------------
  # Valores individuais
  # --------------------------------------------------------
  
  geom_jitter(
    width = 0.10,
    size = 3,
    shape = 16,
    color = "black"
  ) +
    
    
    # --------------------------------------------------------
  # Cores
  # --------------------------------------------------------
  
  scale_fill_manual(
    values = cores
  ) +
    
    
    # --------------------------------------------------------
  # Marcas do eixo Y
  # --------------------------------------------------------
  
  scale_y_continuous(
    breaks = y_breaks,
    expand = expansion(
      mult = c(0, 0.05)
    )
  ) +
    
    
    # --------------------------------------------------------
  # Limites visuais do eixo Y
  # --------------------------------------------------------
  
  coord_cartesian(
    ylim = y_limits
  ) +
    
    
    # --------------------------------------------------------
  # Título do eixo Y
  # --------------------------------------------------------
  
  labs(
    x = NULL,
    y = ylab
  ) +
    
    theme_padrao
  
  
  # ----------------------------------------------------------
  # SALVAR TIFF
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(
      "Figures_Umbilical_Cord_Blood",
      paste0(nome_base, ".tiff")
    ),
    plot = p,
    width = 7,
    height = 5.5,
    dpi = 600,
    compression = "lzw"
  )
  
  
  # ----------------------------------------------------------
  # SALVAR PNG
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(
      "Figures_Umbilical_Cord_Blood",
      paste0(nome_base, ".png")
    ),
    plot = p,
    width = 7,
    height = 5.5,
    dpi = 600
  )
  
  
  return(p)
}


# ============================================================
# 11. GRÁFICOS - CORDÃO
# ============================================================


# ------------------------------------------------------------
# miR-39
# ------------------------------------------------------------

fig_mir39 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR- 39",
  ylab = "CT",
  nome_base = "Umbilical_Cord_Blood_miR_39_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-29a
# ------------------------------------------------------------

fig_mir29a <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 29a",
  ylab = "Relative expression (fold change)",
  nome_base = "Umbilical_Cord_Blood_miR_29a_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-132
# ------------------------------------------------------------

fig_mir132 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 132",
  ylab = "Relative expression (fold change)",
  nome_base = "Umbilical_Cord_Blood_miR_132_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-150-5p
# ------------------------------------------------------------

fig_mir150 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 150-5p",
  ylab = "Relative expression (fold change)",
  nome_base = "Umbilical_Cord_Blood_miR_150-5p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-155-5p
# ------------------------------------------------------------

fig_mir155 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 155-5p",
  ylab = "Relative expression (fold change)",
  nome_base = "Umbilical_Cord_Blood_miR_155-5p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-222-3p
# ------------------------------------------------------------

fig_mir222 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 222-3p",
  ylab = "Relative expression (fold change)",
  nome_base = "Umbilical_Cord_Blood_miR_222-3p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ============================================================
# PART 3 - DECIDUAL TISSUE: CTR VS GDM
# ============================================================

# ============================================================
# ANÁLISE miRNA - DECIDUAL TISSUE
# CTR vs GDM
# Shapiro -> Welch t-test ou Mann-Whitney
# ============================================================

library(tidyverse)
library(writexl)

setwd("C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros/analise micros")


# ============================================================
# 1. LER DADOS
# ============================================================

dados <- read.csv(
  "mir.decidua.csv",
  sep = ";",
  dec = ",",
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ============================================================
# 2. TRANSFORMAR PARA FORMATO LONGO
# ============================================================

dados_long <- dados %>%
  
  mutate(
    Group = str_trim(as.character(Group)),
    Group = str_to_upper(Group)
  ) %>%
  
  filter(
    Group %in% c("CTR", "GDM")
  ) %>%
  
  pivot_longer(
    cols = -c(Group, Sample),
    names_to = "miRNA",
    values_to = "Valor"
  ) %>%
  
  mutate(
    
    Valor = as.character(Valor),
    
    Valor = na_if(Valor, ""),
    Valor = na_if(Valor, "NA"),
    Valor = na_if(Valor, "Undetermined"),
    Valor = na_if(Valor, "-"),
    
    Valor = str_replace_all(
      Valor,
      ",",
      "."
    ),
    
    Valor = suppressWarnings(
      as.numeric(Valor)
    ),
    
    Group = factor(
      Group,
      levels = c("CTR", "GDM")
    )
  )


# ============================================================
# 3. CONFERIR OS DADOS
# ============================================================

unique(dados_long$miRNA)

dados_long %>%
  count(miRNA, Group)


# ============================================================
# 4. FUNÇÃO DE ANÁLISE
# ============================================================

analisar_miRNA <- function(df, mirna) {
  
  dados_mir <- df %>%
    filter(
      miRNA == mirna,
      !is.na(Valor),
      !is.na(Group)
    )
  
  
  ctr <- dados_mir %>%
    filter(Group == "CTR") %>%
    pull(Valor)
  
  gdm <- dados_mir %>%
    filter(Group == "GDM") %>%
    pull(Valor)
  
  
  n_ctr <- length(ctr)
  n_gdm <- length(gdm)
  
  
  # ----------------------------------------------------------
  # SHAPIRO-WILK
  #
  # Só roda se:
  # n >= 3
  # e houver mais de um valor diferente
  # ----------------------------------------------------------
  
  shapiro_ctr <- if (
    n_ctr >= 3 &&
    length(unique(ctr)) > 1
  ) {
    
    shapiro.test(ctr)$p.value
    
  } else {
    
    NA_real_
  }
  
  
  shapiro_gdm <- if (
    n_gdm >= 3 &&
    length(unique(gdm)) > 1
  ) {
    
    shapiro.test(gdm)$p.value
    
  } else {
    
    NA_real_
  }
  
  
  # ----------------------------------------------------------
  # ESCOLHA DO TESTE
  #
  # Ambos normais -> Welch t-test
  # Pelo menos um não normal -> Mann-Whitney
  # Shapiro não disponível -> Mann-Whitney
  # ----------------------------------------------------------
  
  normal <- !is.na(shapiro_ctr) &&
    !is.na(shapiro_gdm) &&
    shapiro_ctr > 0.05 &&
    shapiro_gdm > 0.05
  
  
  if (normal) {
    
    # Welch t-test bilateral
    
    teste <- t.test(
      ctr,
      gdm,
      alternative = "two.sided",
      var.equal = FALSE
    )
    
    nome_teste <- "Welch t-test"
    estatistica <- as.numeric(teste$statistic)
    
  } else {
    
    # Mann-Whitney bilateral
    
    teste <- wilcox.test(
      ctr,
      gdm,
      alternative = "two.sided",
      exact = FALSE
    )
    
    nome_teste <- "Mann-Whitney"
    estatistica <- as.numeric(teste$statistic)
  }
  
  
  # ----------------------------------------------------------
  # RESULTADOS
  # ----------------------------------------------------------
  
  tibble(
    
    miRNA = mirna,
    
    n_CTR = n_ctr,
    n_GDM = n_gdm,
    
    Media_CTR = mean(ctr, na.rm = TRUE),
    Media_GDM = mean(gdm, na.rm = TRUE),
    
    Mediana_CTR = median(ctr, na.rm = TRUE),
    Mediana_GDM = median(gdm, na.rm = TRUE),
    
    Shapiro_CTR_p = shapiro_ctr,
    Shapiro_GDM_p = shapiro_gdm,
    
    Teste = nome_teste,
    
    Estatistica = estatistica,
    
    p_value = teste$p.value
  )
}


# ============================================================
# 5. RODAR ANÁLISE PARA TODOS OS miRNAs
# ============================================================

moleculas <- unique(dados_long$miRNA)

resultados <- map_dfr(
  moleculas,
  ~ analisar_miRNA(dados_long, .x)
)



resultados_decidua <- resultados |>
  dplyr::mutate(
    Compartimento = "Decidua"
  )

# CTR vs GDM comparisons are reported using nominal two-sided p-values,
# without multiple-testing correction, matching the flow-cytometry analysis.


# ============================================================
# 7. VER RESULTADOS
# ============================================================

resultados


# ============================================================
# 8. EXPORTAR RESULTADOS
# ============================================================

write_xlsx(
  resultados,
  "Results_Decidual_Tissue_CTR_vs_GDM.xlsx"
)


# ============================================================
# 9. CONFIGURAÇÕES DOS GRÁFICOS
# ============================================================

cores <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)


theme_padrao <- theme_classic(base_family = "Arial") +
  theme(

    axis.text.x = element_text(
      color = "black",
      size = 16
    ),

    axis.text.y = element_text(
      color = "black",
      size = 14
    ),

    axis.title.x = element_blank(),

    axis.title.y = element_text(
      color = "black",
      size = 16
    ),

    plot.title = element_blank(),

    legend.position = "none",

    axis.line = element_line(
      color = "black",
      linewidth = 0.8
    ),

    axis.ticks = element_line(
      color = "black"
    ),

    plot.margin = margin(
      15, 15, 10, 10
    )
  )


# Criar pasta para os gráficos

dir.create(
  "Figures_Decidual_Tissue",
  showWarnings = FALSE
)


# ============================================================
# 10. FUNÇÃO PARA CRIAR OS GRÁFICOS
#
# Barra = média
# Erro = SEM
# Pontos pretos = valores individuais
# ============================================================

criar_grafico_miRNA <- function(
    df,
    mirna,
    ylab,
    nome_base,
    y_limits,
    y_breaks
) {
  
  dados_plot <- df %>%
    filter(
      miRNA == mirna,
      !is.na(Valor),
      !is.na(Group)
    )
  
  
  p <- ggplot(
    dados_plot,
    aes(
      x = Group,
      y = Valor,
      fill = Group
    )
  ) +
    
    # Barra = média
    stat_summary(
      fun = mean,
      geom = "bar",
      width = 0.6,
      alpha = 0.7,
      color = "black",
      linewidth = 0.7
    ) +
    
    # Barra de erro = média ± SEM
    stat_summary(
      fun.data = mean_se,
      fun.args = list(mult = 1),
      geom = "errorbar",
      width = 0.15,
      linewidth = 0.8,
      color = "black"
    ) +
    
    # Valores individuais
    geom_jitter(
      width = 0.10,
      size = 2.7,
      shape = 16,
      color = "black"
    ) +
    
    # Cores
    scale_fill_manual(
      values = cores
    ) +
    
    # Marcas do eixo Y
    scale_y_continuous(
      breaks = y_breaks,
      expand = expansion(
        mult = c(0, 0.05)
      )
    ) +
    
    # Limites VISUAIS
    # Não remove observações da análise
    coord_cartesian(
      ylim = y_limits
    ) +
    
    labs(
      x = NULL,
      y = ylab
    ) +
    
    theme_padrao
  
  
  # ----------------------------------------------------------
  # SALVAR TIFF
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(
      "Figures_Decidual_Tissue",
      paste0(nome_base, ".tiff")
    ),
    plot = p,
    width = 7,
    height = 5.5,
    dpi = 600,
    compression = "lzw"
  )
  
  
  # ----------------------------------------------------------
  # SALVAR PNG
  # ----------------------------------------------------------
  
  ggsave(
    filename = file.path(
      "Figures_Decidual_Tissue",
      paste0(nome_base, ".png")
    ),
    plot = p,
    width = 7,
    height = 5.5,
    dpi = 600
  )
  
  
  return(p)
}


# ============================================================
# 11. GRÁFICOS - DECÍDUA
# ============================================================


# ------------------------------------------------------------
# miR-39
# ------------------------------------------------------------

fig_mir39 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR- 39",
  ylab = "CT",
  nome_base = "Decidual_Tissue_miR_39_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-29a
# ------------------------------------------------------------

fig_mir29a <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 29a",
  ylab = "Relative expression (fold change)",
  nome_base = "Decidual_Tissue_miR_29a_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-132
# ------------------------------------------------------------

fig_mir132 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 132",
  ylab = "Relative expression (fold change)",
  nome_base = "Decidual_Tissue_miR_132_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-150-5p
# ------------------------------------------------------------

fig_mir150 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 150-5p",
  ylab = "Relative expression (fold change)",
  nome_base = "Decidual_Tissue_miR_150-5p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-155-5p
# ------------------------------------------------------------

fig_mir155 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 155-5p",
  ylab = "Relative expression (fold change)",
  nome_base = "Decidual_Tissue_miR_155-5p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ------------------------------------------------------------
# miR-222-3p
# ------------------------------------------------------------

fig_mir222 <- criar_grafico_miRNA(
  df = dados_long,
  mirna = "miR 222-3p",
  ylab = "Relative expression (fold change)",
  nome_base = "Decidual_Tissue_miR_222-3p_Mean_SEM",
  y_limits = NULL,
  y_breaks = waiver()
)


# ============================================================
# PART 4 - COMPARTMENT ANALYSIS: MIXED-EFFECTS MODEL
# ============================================================

# ============================================================
# ANÁLISE FINAL - miRNAs ENTRE COMPARTIMENTOS
#
# Modelo misto:
# Valor ~ Compartimento * Grupo + (1 | Code)
#
# miR-39:
#   análise em Ct
#
# miRNAs de interesse:
#   análise em log2(fold change)
#
# miR-155:
#   somente Cord + Decidua
#
# Gráficos:
#   média ± SEM + pontos individuais
# ============================================================


# ============================================================
# 1. PACOTES
# ============================================================

library(tidyverse)
library(readxl)
library(lme4)
library(lmerTest)
library(emmeans)
library(openxlsx)

if (!requireNamespace("multcompView", quietly = TRUE)) {
  install.packages("multcompView")
}
library(multcompView)


# ============================================================
# 2. DIRETÓRIO
# ============================================================

setwd(
  "C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/Analise micros/analise micros"
)


# ============================================================
# 3. CONTRASTES PARA ANOVA TIPO III
# ============================================================

options(
  contrasts = c(
    "contr.sum",
    "contr.poly"
  )
)


# ============================================================
# 4. LER O ARQUIVO
# ============================================================

dados <- readxl::read_excel(
  "compartimentos.xlsx"
)


# ============================================================
# 5. LIMPAR NOMES DAS COLUNAS
# ============================================================

names(dados) <- names(dados) |>
  
  stringr::str_replace_all(
    "[\r\n]",
    " "
  ) |>
  
  stringr::str_squish()


# ============================================================
# 6. ORGANIZAR ID E GRUPO
# ============================================================

dados <- dados |>
  
  dplyr::rename(
    Grupo = `Sample type`
  ) |>
  
  dplyr::filter(
    !is.na(Code),
    Grupo %in% c("CTR", "GDM")
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
# 7. FUNÇÃO PARA LIMPAR OS VALORES
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
      
      Valor = stringr::str_trim(Valor),
      
      Valor = dplyr::na_if(
        Valor,
        ""
      ),
      
      Valor = dplyr::na_if(
        Valor,
        "-"
      ),
      
      Valor = dplyr::na_if(
        Valor,
        "NA"
      ),
      
      Valor = dplyr::na_if(
        Valor,
        "Undetermined"
      ),
      
      Valor = stringr::str_replace_all(
        Valor,
        ",",
        "."
      ),
      
      Valor = suppressWarnings(
        as.numeric(Valor)
      )
      
    )
}


# ============================================================
# 8. miR-39
# ============================================================

mir39 <- dados |>
  
  dplyr::select(
    
    Code,
    Grupo,
    
    Mother =
      `Ct miR- 39-3p Mother`,
    
    Cord =
      `Ct miR- 39-3p Cord`,
    
    Decidua =
      `Ct miR- 39-3p Decidua`
    
  ) |>
  
  limpar_valores() |>
  
  dplyr::mutate(
    miRNA = "miR-39",
    Tipo = "Ct"
  )


# ============================================================
# 9. miR-29a-3p
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
    miRNA = "miR-29a-3p",
    Tipo = "Expressao"
  )


# ============================================================
# 10. miR-132-3p
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
    miRNA = "miR-132-3p",
    Tipo = "Expressao"
  )


# ============================================================
# 11. miR-150-5p
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
    miRNA = "miR-150-5p",
    Tipo = "Expressao"
  )


# ============================================================
# 12. miR-155-5p
#
# Ele entra no todos_long,
# mas será analisado separadamente depois.
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
    miRNA = "miR-155-5p",
    Tipo = "Expressao"
  )


# ============================================================
# 13. miR-222-3p
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
    miRNA = "miR-222-3p",
    Tipo = "Expressao"
  )


# ============================================================
# 14. CRIAR todos_long
# ============================================================

todos_long <- dplyr::bind_rows(
  
  mir39,
  mir29,
  mir132,
  mir150,
  mir155,
  mir222
  
) |>
  
  dplyr::mutate(
    
    Compartimento = factor(
      Compartimento,
      levels = c(
        "Mother",
        "Cord",
        "Decidua"
      )
    ),
    
    Grupo = factor(
      Grupo,
      levels = c(
        "CTR",
        "GDM"
      )
    ),
    
    Code = factor(Code)
    
  )


# ============================================================
# 15. CONFERIR SE TODOS OS miRNAs ENTRARAM
# ============================================================

print(
  unique(
    todos_long$miRNA
  )
)


# Deve aparecer:
#
# miR-39
# miR-29a-3p
# miR-132-3p
# miR-150-5p
# miR-155-5p
# miR-222-3p


# ============================================================
# 16. CONTAGEM AMOSTRAL
# ============================================================

contagem_amostras <- todos_long |>
  
  dplyr::filter(
    !is.na(Valor)
  ) |>
  
  dplyr::count(
    miRNA,
    Grupo,
    Compartimento,
    name = "n"
  )


print(
  contagem_amostras
)


# ============================================================
# 17. VARIÁVEL PARA ANÁLISE
#
# miR-39 = Ct original
#
# demais = log2(fold change)
#
# O gráfico continua usando Valor ORIGINAL.
# ============================================================

todos_long <- todos_long |>
  
  dplyr::mutate(
    
    Valor_analise =
      dplyr::case_when(
        
        Tipo == "Ct" ~
          Valor,
        
        Tipo == "Expressao" &
          !is.na(Valor) &
          Valor > 0 ~
          log2(Valor),
        
        TRUE ~
          NA_real_
        
      )
    
  )


# ============================================================
# 18. FUNÇÃO PARA MODELO MISTO
#
# Esta função será usada para:
#
# miR-39
# miR-29
# miR-132
# miR-150
# miR-222
#
# NÃO para miR-155.
# ============================================================

analisar_modelo <- function(
    df,
    mirna
) {
  
  dados_mir <- df |>
    
    dplyr::filter(
      
      miRNA == mirna,
      
      !is.na(Valor_analise),
      
      !is.na(Code),
      
      !is.na(Grupo),
      
      !is.na(Compartimento)
      
    ) |>
    
    droplevels()
  
  
  # ----------------------------------------------------------
  # MODELO MISTO
  # ----------------------------------------------------------
  
  modelo <- lmerTest::lmer(
    
    Valor_analise ~
      Compartimento * Grupo +
      (1 | Code),
    
    data = dados_mir,
    
    REML = TRUE
    
  )
  
  
  # ----------------------------------------------------------
  # ANOVA TIPO III
  # ----------------------------------------------------------
  
  anova_modelo <- anova(
    
    modelo,
    
    type = 3
    
  ) |>
    
    as.data.frame() |>
    
    tibble::rownames_to_column(
      "Efeito"
    ) |>
    
    dplyr::mutate(
      miRNA = mirna
    )
  
  
  # ----------------------------------------------------------
  # COMPARAÇÃO ENTRE COMPARTIMENTOS
  # DENTRO DE CTR E GDM
  # ----------------------------------------------------------
  
  emm_grupo <- emmeans::emmeans(
    
    modelo,
    
    ~ Compartimento | Grupo
    
  )
  
  
  comparacoes_grupo <- pairs(
    
    emm_grupo,
    
    adjust = "none"
    
  ) |>
    
    as.data.frame() |>
    
    dplyr::group_by(
      Grupo
    ) |>
    
    dplyr::mutate(
      
      p_BH = p.adjust(
        p.value,
        method = "BH"
      )
      
    ) |>
    
    dplyr::ungroup() |>
    
    dplyr::mutate(
      
      miRNA = mirna,
      
      significancia =
        dplyr::case_when(
          
          p_BH < 0.0001 ~ "****",
          
          p_BH < 0.001 ~ "***",
          
          p_BH < 0.01 ~ "**",
          
          p_BH < 0.05 ~ "*",
          
          TRUE ~ "ns"
          
        )
      
    )
  
  
  # ----------------------------------------------------------
  # COMPARAÇÃO GERAL ENTRE COMPARTIMENTOS
  # ----------------------------------------------------------
  
  emm_comp <- emmeans::emmeans(
    
    modelo,
    
    ~ Compartimento
    
  )
  
  
  comparacoes_comp <- pairs(
    
    emm_comp,
    
    adjust = "none"
    
  ) |>
    
    as.data.frame() |>
    
    dplyr::mutate(
      
      p_BH = p.adjust(
        p.value,
        method = "BH"
      ),
      
      miRNA = mirna
      
    )
  
  
  return(
    
    list(
      
      modelo =
        modelo,
      
      anova =
        anova_modelo,
      
      comparacoes_grupo =
        comparacoes_grupo,
      
      comparacoes_comp =
        comparacoes_comp
      
    )
    
  )
  
}


# ============================================================
# 19. miRNAs PARA O MODELO DE 3 COMPARTIMENTOS
#
# miR-155 NÃO entra aqui.
# ============================================================

moleculas <- c(
  
  "miR-39",
  "miR-29a-3p",
  "miR-132-3p",
  "miR-150-5p",
  "miR-222-3p"
  
)


# ============================================================
# 20. RODAR MODELOS
# ============================================================

resultados_modelos <- purrr::map(
  
  moleculas,
  
  function(x) {
    
    analisar_modelo(
      todos_long,
      x
    )
    
  }
  
)


names(
  resultados_modelos
) <- moleculas


# ============================================================
# 21. JUNTAR RESULTADOS
# ============================================================

resultado_anova <- purrr::map_dfr(
  resultados_modelos,
  "anova"
)


resultado_pos_grupo <- purrr::map_dfr(
  resultados_modelos,
  "comparacoes_grupo"
)


resultado_compartimento <- purrr::map_dfr(
  resultados_modelos,
  "comparacoes_comp"
)


# ============================================================
# 21B. LETRAS AUTOMÁTICAS DOS COMPARTIMENTOS
#
# As letras representam as comparações entre compartimentos
# dentro de cada grupo (CTR ou GDM), usando os p-valores BH
# já calculados em resultado_pos_grupo.
#
# Mesma letra = sem diferença significativa.
# Letras diferentes = diferença significativa (BH p < 0.05).
# ============================================================

gerar_letras_compartimentos <- function(df) {

  if (nrow(df) == 0) {
    return(
      tibble::tibble(
        Compartimento = character(),
        Letra = character()
      )
    )
  }

  pvals <- df$p_BH

  names(pvals) <- stringr::str_replace_all(
    df$contrast,
    " ",
    ""
  )

  letras <- multcompView::multcompLetters(
    pvals,
    threshold = 0.05
  )$Letters

  tibble::tibble(
    Compartimento = names(letras),
    Letra = unname(letras)
  )
}


letras_modelo <- resultado_pos_grupo |>
  dplyr::filter(
    !is.na(p_BH)
  ) |>
  dplyr::group_by(
    miRNA,
    Grupo
  ) |>
  dplyr::group_modify(
    ~ gerar_letras_compartimentos(.x)
  ) |>
  dplyr::ungroup()


# ------------------------------------------------------------
# LETRAS CENTRAIS
# CTR e GDM receberam a mesma letra no compartimento
# ------------------------------------------------------------

letras_centrais_auto <- letras_modelo |>
  dplyr::select(
    miRNA,
    Grupo,
    Compartimento,
    Letra
  ) |>
  tidyr::pivot_wider(
    names_from = Grupo,
    values_from = Letra
  ) |>
  dplyr::filter(
    !is.na(CTR),
    !is.na(GDM),
    CTR == GDM
  ) |>
  dplyr::transmute(
    miRNA,
    Compartimento,
    Letra = CTR
  )


# ------------------------------------------------------------
# LETRAS SEPARADAS
# CTR e GDM receberam letras diferentes no compartimento
# ------------------------------------------------------------

letras_separadas_auto <- letras_modelo |>
  dplyr::select(
    miRNA,
    Grupo,
    Compartimento,
    Letra
  ) |>
  tidyr::pivot_wider(
    names_from = Grupo,
    values_from = Letra
  ) |>
  dplyr::filter(
    is.na(CTR) |
      is.na(GDM) |
      CTR != GDM
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::any_of(
      c("CTR", "GDM")
    ),
    names_to = "Grupo",
    values_to = "Letra"
  ) |>
  dplyr::filter(
    !is.na(Letra)
  )


print(letras_modelo)
print(letras_centrais_auto)
print(letras_separadas_auto)


# ============================================================
# 22. miR-155:
# SOMENTE CORD + DECIDUA
# ============================================================

dados_mir155_CD <- todos_long |>
  
  dplyr::filter(
    
    miRNA == "miR-155-5p",
    
    Compartimento %in% c(
      "Cord",
      "Decidua"
    ),
    
    !is.na(Valor_analise),
    
    !is.na(Code),
    
    !is.na(Grupo)
    
  ) |>
  
  dplyr::mutate(
    
    Compartimento = factor(
      
      Compartimento,
      
      levels = c(
        "Cord",
        "Decidua"
      )
      
    )
    
  ) |>
  
  droplevels()


# ============================================================
# 23. CONFERIR N DO miR-155
# ============================================================

print(
  
  dados_mir155_CD |>
    
    dplyr::count(
      Grupo,
      Compartimento
    )
  
)


# ============================================================
# 24. MODELO DO miR-155
# ============================================================

modelo_mir155_CD <- lmerTest::lmer(
  
  Valor_analise ~
    Compartimento * Grupo +
    (1 | Code),
  
  data = dados_mir155_CD,
  
  REML = TRUE
  
)


# ============================================================
# 25. ANOVA DO miR-155
# ============================================================

anova_mir155_CD <- anova(
  
  modelo_mir155_CD,
  
  type = 3
  
) |>
  
  as.data.frame() |>
  
  tibble::rownames_to_column(
    "Efeito"
  ) |>
  
  dplyr::mutate(
    miRNA = "miR-155-5p"
  )


print(
  anova_mir155_CD
)


# ============================================================
# 26. miR-155:
# CORD vs DECIDUA DENTRO DE CTR/GDM
# ============================================================

emm_mir155_CD <- emmeans::emmeans(
  
  modelo_mir155_CD,
  
  ~ Compartimento | Grupo
  
)


comparacoes_mir155_CD <- pairs(
  
  emm_mir155_CD,
  
  adjust = "none"
  
) |>
  
  as.data.frame() |>
  
  dplyr::group_by(
    Grupo
  ) |>
  
  dplyr::mutate(
    
    p_BH = p.adjust(
      p.value,
      method = "BH"
    ),
    
    significancia =
      dplyr::case_when(
        
        p_BH < 0.0001 ~ "****",
        
        p_BH < 0.001 ~ "***",
        
        p_BH < 0.01 ~ "**",
        
        p_BH < 0.05 ~ "*",
        
        TRUE ~ "ns"
        
      )
    
  ) |>
  
  dplyr::ungroup() |>
  
  dplyr::mutate(
    miRNA = "miR-155-5p"
  )


print(
  comparacoes_mir155_CD
)


# ============================================================
# 27. miR-155:
# CTR vs GDM DENTRO DE CORD/DECIDUA
# ============================================================

emm_mir155_grupo <- emmeans::emmeans(
  
  modelo_mir155_CD,
  
  ~ Grupo | Compartimento
  
)


comparacoes_mir155_grupo <- pairs(
  
  emm_mir155_grupo,
  
  adjust = "none"
  
) |>
  
  as.data.frame() |>
  
  dplyr::mutate(
    
    p_BH = p.adjust(
      p.value,
      method = "BH"
    ),
    
    significancia =
      dplyr::case_when(
        
        p_BH < 0.0001 ~ "****",
        
        p_BH < 0.001 ~ "***",
        
        p_BH < 0.01 ~ "**",
        
        p_BH < 0.05 ~ "*",
        
        TRUE ~ "ns"
        
      ),
    
    miRNA = "miR-155-5p"
    
  )


print(
  comparacoes_mir155_grupo
)


# ============================================================
# 27B. LETRAS AUTOMÁTICAS DO miR-155
# CORD vs DECIDUA DENTRO DE CTR/GDM
# ============================================================

letras_mir155 <- comparacoes_mir155_CD |>
  dplyr::group_by(
    Grupo
  ) |>
  dplyr::group_modify(
    ~ gerar_letras_compartimentos(.x)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    miRNA = "miR-155-5p"
  )


letras_centrais_mir155 <- letras_mir155 |>
  dplyr::select(
    Grupo,
    Compartimento,
    Letra
  ) |>
  tidyr::pivot_wider(
    names_from = Grupo,
    values_from = Letra
  ) |>
  dplyr::filter(
    !is.na(CTR),
    !is.na(GDM),
    CTR == GDM
  ) |>
  dplyr::transmute(
    Compartimento,
    Letra = CTR
  )


letras_separadas_mir155 <- letras_mir155 |>
  dplyr::select(
    Grupo,
    Compartimento,
    Letra
  ) |>
  tidyr::pivot_wider(
    names_from = Grupo,
    values_from = Letra
  ) |>
  dplyr::filter(
    is.na(CTR) |
      is.na(GDM) |
      CTR != GDM
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::any_of(
      c("CTR", "GDM")
    ),
    names_to = "Grupo",
    values_to = "Letra"
  ) |>
  dplyr::filter(
    !is.na(Letra)
  )


print(letras_mir155)


# ============================================================
# 28. SALVAR RESULTADOS ESTATÍSTICOS
# ============================================================

openxlsx::write.xlsx(
  
  list(
    
    N_amostral =
      contagem_amostras,
    
    Modelo_misto =
      resultado_anova,
    
    Compartimentos_por_grupo =
      resultado_pos_grupo,
    
    Compartimentos_geral =
      resultado_compartimento,
    
    miR155_Modelo_Cord_Decidua =
      anova_mir155_CD,
    
    miR155_Cord_vs_Decidua =
      comparacoes_mir155_CD,
    
    miR155_CTR_vs_GDM =
      comparacoes_mir155_grupo
    
  ),
  
  file =
    "Resultados_Modelo_Misto_Compartimentos_FINAL.xlsx",
  
  overwrite = TRUE
  
)


# ============================================================
# 29. CORES
# ============================================================

cores_grupo <- c(
  "CTR" = "#2761F5",
  "GDM" = "#F52727"
)


# ============================================================
# 30. NOMES DOS COMPARTIMENTOS
# ============================================================

nomes_compartimentos <- c(
  "Mother" = "Maternal\nblood",
  "Cord" = "Umbilical cord\nblood",
  "Decidua" = "Decidual\ntissue"
)


# ============================================================
# 31. CRIAR PASTA DOS GRÁFICOS
# ============================================================

dir.create(
  "Graficos_Compartimentos_FINAL",
  showWarnings = FALSE
)


# ============================================================
# 32. FUNÇÃO PARA CALCULAR MÉDIA E SEM
# ============================================================

resumir_grafico <- function(df) {

  df |>
    dplyr::group_by(
      Compartimento,
      Grupo
    ) |>
    dplyr::summarise(

      n = sum(!is.na(Valor)),

      media = mean(
        Valor,
        na.rm = TRUE
      ),

      sd = stats::sd(
        Valor,
        na.rm = TRUE
      ),

      maximo = max(
        Valor,
        na.rm = TRUE
      ),

      .groups = "drop"

    ) |>
    dplyr::mutate(

      sd = tidyr::replace_na(
        sd,
        0
      ),

      sem = dplyr::if_else(
        n > 0,
        sd / sqrt(n),
        0
      ),

      sem = tidyr::replace_na(
        sem,
        0
      ),

      ymin = media - sem,

      ymax = media + sem,

      topo = pmax(
        ymax,
        maximo
      )

    )
}




# ============================================================
# 32A. RESULTADOS NOMINAIS CTR vs GDM POR COMPARTIMENTO
#
# Estes objetos são preservados das três análises independentes:
# Maternal blood, Umbilical cord blood e Decidual tissue.
# ============================================================

resultados_nominais_grupo <- dplyr::bind_rows(
  resultados_maternal,
  resultados_cord,
  resultados_decidua
) |>
  dplyr::mutate(
    miRNA = dplyr::recode(
      miRNA,
      "miR- 39"    = "miR-39",
      "miR 29a"    = "miR-29a-3p",
      "miR 132"    = "miR-132-3p",
      "miR 150-5p" = "miR-150-5p",
      "miR 155-5p" = "miR-155-5p",
      "miR 222-3p" = "miR-222-3p"
    )
  )

# Check standardized names used by the plotting functions
print(
  unique(
    resultados_nominais_grupo$miRNA
  )
)

print(
  resultados_nominais_grupo
)


# ============================================================
# 32B. ASTERISCOS CTR vs GDM - P NOMINAL
#
# Welch t-test ou Mann-Whitney, conforme a análise principal.
# Sem correção por múltiplas comparações.
# ============================================================

p_para_estrela_nominal <- function(p) {

  dplyr::case_when(
    is.na(p)      ~ "",
    p < 0.0001    ~ "****",
    p < 0.001     ~ "***",
    p < 0.01      ~ "**",
    p < 0.05      ~ "*",
    TRUE          ~ ""
  )
}


# ============================================================
# 33. FUNÇÃO GERAL DOS GRÁFICOS
#
# Usada para:
# miR-39
# miR-29a-3p
# miR-132-3p
# miR-150-5p
# miR-222-3p
# ============================================================

criar_grafico_final <- function(
    df,
    mirna,
    ylab,
    nome_base,
    y_limits = NULL,
    y_breaks = waiver(),
    tamanho_eixo_x = 16,
    tamanho_eixo_y = 14,
    tamanho_titulo_y = 16,
    tamanho_letra = 6,
    largura = 7,
    altura = 5.5
) {

  # ----------------------------------------------------------
  # DADOS
  # ----------------------------------------------------------

  dados_plot <- df |>
    dplyr::filter(
      miRNA == mirna,
      !is.na(Valor)
    ) |>
    droplevels()


  # ----------------------------------------------------------
  # MÉDIA ± SEM
  # ----------------------------------------------------------

  resumo <- resumir_grafico(
    dados_plot
  )


  # ----------------------------------------------------------
  # TOPO REAL DE CADA BARRA
  # ----------------------------------------------------------

  topo_grupo <- dados_plot |>
    dplyr::group_by(
      Compartimento,
      Grupo
    ) |>
    dplyr::summarise(
      max_observado = max(
        Valor,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      resumo,
      by = c(
        "Compartimento",
        "Grupo"
      )
    ) |>
    dplyr::mutate(
      topo = pmax(
        max_observado,
        media + sem,
        na.rm = TRUE
      )
    )


  max_dados <- max(
    topo_grupo$topo,
    na.rm = TRUE
  )

  if (!is.finite(max_dados)) {
    max_dados <- 1
  }

  amplitude <- diff(
    range(
      c(
        dados_plot$Valor,
        resumo$ymin,
        resumo$ymax
      ),
      na.rm = TRUE
    )
  )

  espaco <- max(
    max_dados * 0.08,
    amplitude * 0.08,
    0.05
  )


  # ----------------------------------------------------------
  # LETRAS CENTRAIS
  # ----------------------------------------------------------

  topo_comp <- topo_grupo |>
    dplyr::group_by(
      Compartimento
    ) |>
    dplyr::summarise(
      topo_comp = max(
        topo,
        na.rm = TRUE
      ),
      .groups = "drop"
    )


  letras_c <- letras_centrais_auto |>
    dplyr::filter(
      miRNA == mirna
    ) |>
    dplyr::mutate(
      Compartimento = factor(
        Compartimento,
        levels = c(
          "Mother",
          "Cord",
          "Decidua"
        )
      )
    ) |>
    dplyr::left_join(
      topo_comp,
      by = "Compartimento"
    ) |>
    dplyr::mutate(
      y_letra = topo_comp + espaco * 1.65
    )


  # ----------------------------------------------------------
  # LETRAS SEPARADAS CTR / GDM
  # ----------------------------------------------------------

  letras_s <- letras_separadas_auto |>
    dplyr::filter(
      miRNA == mirna
    ) |>
    dplyr::mutate(
      Compartimento = factor(
        Compartimento,
        levels = c(
          "Mother",
          "Cord",
          "Decidua"
        )
      ),
      Grupo = factor(
        Grupo,
        levels = c(
          "CTR",
          "GDM"
        )
      )
    ) |>
    dplyr::left_join(
      topo_grupo,
      by = c(
        "Compartimento",
        "Grupo"
      )
    ) |>
    dplyr::mutate(
      y_letra = topo + espaco * 1.65,
      x_letra =
        as.numeric(Compartimento) +
        dplyr::if_else(
          Grupo == "CTR",
          -0.19,
          0.19
        )
    )


  # ----------------------------------------------------------
  # ASTERISCOS CTR vs GDM - P NOMINAL
  # ----------------------------------------------------------

  sig_grupo <- resultados_nominais_grupo |>
    dplyr::filter(
      miRNA == mirna,
      !is.na(p_value)
    ) |>
    dplyr::mutate(
      estrelas = p_para_estrela_nominal(
        p_value
      )
    ) |>
    dplyr::filter(
      estrelas != ""
    ) |>
    dplyr::mutate(
      Compartimento = dplyr::recode(
        Compartimento,
        "Maternal blood" = "Mother",
        "Umbilical cord blood" = "Cord",
        "Decidual tissue" = "Decidua",
        .default = Compartimento
      ),
      Compartimento = factor(
        Compartimento,
        levels = c(
          "Mother",
          "Cord",
          "Decidua"
        )
      )
    ) |>
    dplyr::left_join(
      topo_comp,
      by = "Compartimento"
    )


  if (nrow(sig_grupo) > 0) {

    sig_grupo <- sig_grupo |>
      dplyr::mutate(
        y_bracket = topo_comp + espaco * 0.45,
        y_texto = topo_comp + espaco * 0.70,
        x_num = as.numeric(Compartimento),
        x1 = x_num - 0.19,
        x2 = x_num + 0.19
      )
  }


  # ----------------------------------------------------------
  # LIMITES AUTOMÁTICOS
  # ----------------------------------------------------------

  valores_topo <- c(
    max_dados
  )

  if (nrow(letras_c) > 0) {
    valores_topo <- c(
      valores_topo,
      letras_c$y_letra
    )
  }

  if (nrow(letras_s) > 0) {
    valores_topo <- c(
      valores_topo,
      letras_s$y_letra
    )
  }

  if (nrow(sig_grupo) > 0) {
    valores_topo <- c(
      valores_topo,
      sig_grupo$y_texto
    )
  }

  ymax_auto <- max(
    valores_topo,
    na.rm = TRUE
  ) + espaco


  ymin_dados <- min(
    c(
      dados_plot$Valor,
      resumo$ymin
    ),
    na.rm = TRUE
  )

  if (
    is.finite(ymin_dados) &&
    ymin_dados < 0
  ) {
    ymin_auto <- ymin_dados - espaco * 0.25
  } else {
    ymin_auto <- 0
  }


  if (is.null(y_limits)) {
    limite_y <- c(
      ymin_auto,
      ymax_auto
    )
  } else {
    limite_y <- y_limits
  }


  # ----------------------------------------------------------
  # GRÁFICO
  # ----------------------------------------------------------

  p <- ggplot2::ggplot(
    dados_plot,
    ggplot2::aes(
      x = Compartimento,
      y = Valor,
      fill = Grupo
    )
  ) +

    ggplot2::geom_col(
      data = resumo,
      ggplot2::aes(
        x = Compartimento,
        y = media,
        fill = Grupo,
        group = Grupo
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
      data = resumo,
      ggplot2::aes(
        x = Compartimento,
        ymin = media - sem,
        ymax = media + sem,
        group = Grupo
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
        group = Grupo
      ),
      position =
        ggplot2::position_jitterdodge(
          jitter.width = 0.08,
          dodge.width = 0.75
        ),
      size = 2.7,
      shape = 16,
      color = "black"
    ) +

    ggplot2::scale_fill_manual(
      values = cores_grupo
    ) +

    ggplot2::scale_x_discrete(
      labels = nomes_compartimentos
    ) +

    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      expand = ggplot2::expansion(
        mult = c(
          0,
          0
        )
      )
    ) +

    ggplot2::coord_cartesian(
      ylim = limite_y,
      clip = "off"
    ) +

    ggplot2::labs(
      title = mirna,
      x = NULL,
      y = ylab,
      fill = NULL
    ) +

    ggplot2::theme_classic(
      base_family = "Arial"
    ) +

    ggplot2::theme(
      plot.title =
        ggplot2::element_text(
          family = "Arial",
          size = 18,
          face = "bold",
          hjust = 0.5,
          color = "black"
        ),
      axis.text.x =
        ggplot2::element_text(
          color = "black",
          size = tamanho_eixo_x
        ),
      axis.text.y =
        ggplot2::element_text(
          color = "black",
          size = tamanho_eixo_y
        ),
      axis.title.y =
        ggplot2::element_text(
          color = "black",
          size = tamanho_titulo_y
        ),
      axis.title.x =
        ggplot2::element_blank(),
      legend.position = "none",
      axis.line =
        ggplot2::element_line(
          color = "black",
          linewidth = 0.8
        ),
      axis.ticks =
        ggplot2::element_line(
          color = "black"
        ),
      plot.margin =
        ggplot2::margin(
          20, 20, 10, 10
        )
    )


  # ----------------------------------------------------------
  # LETRAS CENTRAIS
  # ----------------------------------------------------------

  if (nrow(letras_c) > 0) {

    p <- p +
      ggplot2::geom_text(
        data = letras_c,
        ggplot2::aes(
          x = Compartimento,
          y = y_letra,
          label = Letra
        ),
        inherit.aes = FALSE,
        family = "Arial",
        fontface = "bold",
        size = tamanho_letra,
        color = "black"
      )
  }


  # ----------------------------------------------------------
  # LETRAS SEPARADAS CTR / GDM
  # ----------------------------------------------------------

  if (nrow(letras_s) > 0) {

    p <- p +
      ggplot2::geom_text(
        data = letras_s,
        ggplot2::aes(
          x = x_letra,
          y = y_letra,
          label = Letra
        ),
        inherit.aes = FALSE,
        family = "Arial",
        fontface = "bold",
        size = tamanho_letra,
        color = "black"
      )
  }


  # ----------------------------------------------------------
  # BRACKET + ASTERISCO CTR vs GDM
  # p NOMINAL do Welch t-test / Mann-Whitney
  # ----------------------------------------------------------

  if (nrow(sig_grupo) > 0) {

    altura_perna <- espaco * 0.22

    p <- p +

      ggplot2::geom_segment(
        data = sig_grupo,
        ggplot2::aes(
          x = x1,
          xend = x2,
          y = y_bracket,
          yend = y_bracket
        ),
        inherit.aes = FALSE,
        linewidth = 0.8,
        color = "black"
      ) +

      ggplot2::geom_segment(
        data = sig_grupo,
        ggplot2::aes(
          x = x1,
          xend = x1,
          y = y_bracket,
          yend = y_bracket - altura_perna
        ),
        inherit.aes = FALSE,
        linewidth = 0.8,
        color = "black"
      ) +

      ggplot2::geom_segment(
        data = sig_grupo,
        ggplot2::aes(
          x = x2,
          xend = x2,
          y = y_bracket,
          yend = y_bracket - altura_perna
        ),
        inherit.aes = FALSE,
        linewidth = 0.8,
        color = "black"
      ) +

      ggplot2::geom_text(
        data = sig_grupo,
        ggplot2::aes(
          x = x_num,
          y = y_texto,
          label = estrelas
        ),
        inherit.aes = FALSE,
        family = "Arial",
        fontface = "bold",
        size = 5.5,
        color = "black"
      )
  }


  # ----------------------------------------------------------
  # SALVAR
  # ----------------------------------------------------------

  ggplot2::ggsave(
    filename = file.path(
      "Graficos_Compartimentos_FINAL",
      paste0(
        nome_base,
        ".tiff"
      )
    ),
    plot = p,
    width = largura,
    height = altura,
    units = "in",
    dpi = 600,
    compression = "lzw"
  )


  ggplot2::ggsave(
    filename = file.path(
      "Graficos_Compartimentos_FINAL",
      paste0(
        nome_base,
        ".png"
      )
    ),
    plot = p,
    width = largura,
    height = altura,
    units = "in",
    dpi = 600
  )


  return(p)
}


# ============================================================
# 34. FUNÇÃO ESPECÍFICA DO miR-155
#
# SOMENTE CORD + DECIDUA
# ============================================================

criar_grafico_mir155 <- function(
    df,
    ylab = "Relative expression (fold change)",
    nome_base = "miR155_Cord_Decidua",
    y_limits = NULL,
    y_breaks = waiver(),
    tamanho_eixo_x = 16,
    tamanho_eixo_y = 14,
    tamanho_titulo_y = 16,
    tamanho_asterisco = 5.5,
    tamanho_letra = 5.5,
    largura = 7,
    altura = 5.5
) {

  # ----------------------------------------------------------
  # DADOS
  # ----------------------------------------------------------

  dados_plot <- df |>
    dplyr::filter(
      miRNA == "miR-155-5p",
      Compartimento %in% c("Cord", "Decidua"),
      !is.na(Valor)
    ) |>
    dplyr::mutate(
      Compartimento = factor(
        Compartimento,
        levels = c("Cord", "Decidua")
      ),
      Grupo = factor(
        Grupo,
        levels = c("CTR", "GDM")
      )
    ) |>
    droplevels()


  # ----------------------------------------------------------
  # MÉDIA ± SEM
  # ----------------------------------------------------------

  resumo <- resumir_grafico(dados_plot)


  topo_grupo <- dados_plot |>
    dplyr::group_by(
      Compartimento,
      Grupo
    ) |>
    dplyr::summarise(
      max_observado = max(
        Valor,
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      resumo,
      by = c(
        "Compartimento",
        "Grupo"
      )
    ) |>
    dplyr::mutate(
      topo = pmax(
        max_observado,
        media + sem,
        na.rm = TRUE
      )
    )


  topo_comp <- topo_grupo |>
    dplyr::group_by(
      Compartimento
    ) |>
    dplyr::summarise(
      topo_comp = max(
        topo,
        na.rm = TRUE
      ),
      .groups = "drop"
    )


  max_dados <- max(
    topo_grupo$topo,
    na.rm = TRUE
  )

  if (!is.finite(max_dados) || max_dados <= 0) {
    max_dados <- 1
  }


  amplitude <- diff(
    range(
      c(
        dados_plot$Valor,
        resumo$ymin,
        resumo$ymax
      ),
      na.rm = TRUE
    )
  )

  espaco <- max(
    max_dados * 0.08,
    amplitude * 0.08,
    0.05
  )


  # ----------------------------------------------------------
  # LETRAS DOS COMPARTIMENTOS - BH
  # mesma lógica usada nos demais miRNAs
  # ----------------------------------------------------------

  letras_c <- letras_centrais_mir155 |>
    dplyr::mutate(
      Compartimento = factor(
        Compartimento,
        levels = c("Cord", "Decidua")
      )
    ) |>
    dplyr::left_join(
      topo_comp,
      by = "Compartimento"
    ) |>
    dplyr::mutate(
      y_letra = dplyr::if_else(
        Compartimento == "Cord",
        topo_comp + espaco * 2.40,  # sobe bastante o "a"
        topo_comp + espaco * 1.35   # mantém o "b" onde está
      )
    )


  letras_s <- letras_separadas_mir155 |>
    dplyr::mutate(
      Compartimento = factor(
        Compartimento,
        levels = c("Cord", "Decidua")
      ),
      Grupo = factor(
        Grupo,
        levels = c("CTR", "GDM")
      )
    ) |>
    dplyr::left_join(
      topo_grupo,
      by = c(
        "Compartimento",
        "Grupo"
      )
    ) |>
    dplyr::mutate(
      y_letra = topo + espaco * 1.35,
      x_letra =
        as.numeric(Compartimento) +
        dplyr::if_else(
          Grupo == "CTR",
          -0.19,
          0.19
        )
    )


  # ----------------------------------------------------------
  # ASTERISCOS CTR vs GDM - P NOMINAL
  # Welch t-test / Mann-Whitney
  # ----------------------------------------------------------

  sig_grupo <- resultados_nominais_grupo |>
    dplyr::filter(
      miRNA == "miR-155-5p",
      !is.na(p_value),
      p_value < 0.05
    ) |>
    dplyr::mutate(
      Compartimento = dplyr::recode(
        Compartimento,
        "Maternal blood" = "Mother",
        "Umbilical cord blood" = "Cord",
        "Decidual tissue" = "Decidua",
        .default = Compartimento
      )
    ) |>
    dplyr::filter(
      Compartimento %in% c("Cord", "Decidua")
    ) |>
    dplyr::mutate(
      Compartimento = factor(
        Compartimento,
        levels = c("Cord", "Decidua")
      ),
      estrelas = p_para_estrela_nominal(
        p_value
      )
    ) |>
    dplyr::left_join(
      topo_comp,
      by = "Compartimento"
    )


  if (nrow(sig_grupo) > 0) {

    sig_grupo <- sig_grupo |>
      dplyr::mutate(
        y_bracket = topo_comp + espaco * 0.45,
        y_texto = topo_comp + espaco * 0.70,
        x_num = as.numeric(Compartimento),
        x1 = x_num - 0.19,
        x2 = x_num + 0.19
      )
  }


  # ----------------------------------------------------------
  # LIMITES
  # ----------------------------------------------------------

  valores_topo <- c(max_dados)

  if (nrow(letras_c) > 0) {
    valores_topo <- c(
      valores_topo,
      letras_c$y_letra
    )
  }

  if (nrow(letras_s) > 0) {
    valores_topo <- c(
      valores_topo,
      letras_s$y_letra
    )
  }

  if (nrow(sig_grupo) > 0) {
    valores_topo <- c(
      valores_topo,
      sig_grupo$y_texto
    )
  }

  ymax_auto <- max(
    valores_topo,
    na.rm = TRUE
  ) + espaco


  ymin_dados <- min(
    c(
      dados_plot$Valor,
      resumo$ymin
    ),
    na.rm = TRUE
  )

  if (
    is.finite(ymin_dados) &&
    ymin_dados < 0
  ) {
    ymin_auto <- ymin_dados - espaco * 0.25
  } else {
    ymin_auto <- 0
  }


  if (is.null(y_limits)) {
    limite_y <- c(
      ymin_auto,
      ymax_auto
    )
  } else {
    limite_y <- y_limits
  }


  # ----------------------------------------------------------
  # GRÁFICO - MESMA ESTÉTICA DA CITOMETRIA
  # ----------------------------------------------------------

  p <- ggplot2::ggplot(
    dados_plot,
    ggplot2::aes(
      x = Compartimento,
      y = Valor,
      fill = Grupo
    )
  ) +

    ggplot2::geom_col(
      data = resumo,
      ggplot2::aes(
        x = Compartimento,
        y = media,
        fill = Grupo,
        group = Grupo
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
      data = resumo,
      ggplot2::aes(
        x = Compartimento,
        ymin = media - sem,
        ymax = media + sem,
        group = Grupo
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
        group = Grupo
      ),
      position =
        ggplot2::position_jitterdodge(
          jitter.width = 0.08,
          dodge.width = 0.75
        ),
      size = 2.7,
      shape = 16,
      color = "black"
    ) +

    ggplot2::scale_fill_manual(
      values = cores_grupo
    ) +

    ggplot2::scale_x_discrete(
      labels = c(
        "Cord" = "Umbilical cord\nblood",
        "Decidua" = "Decidual\ntissue"
      )
    ) +

    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      expand = ggplot2::expansion(
        mult = c(0, 0)
      )
    ) +

    ggplot2::coord_cartesian(
      ylim = limite_y,
      clip = "off"
    ) +

    ggplot2::labs(
      title = "miR-155-5p",
      x = NULL,
      y = ylab,
      fill = NULL
    ) +

    ggplot2::theme_classic(
      base_family = "Arial"
    ) +

    ggplot2::theme(
      plot.title =
        ggplot2::element_text(
          family = "Arial",
          size = 18,
          face = "bold",
          hjust = 0.5,
          color = "black"
        ),
      legend.position = "none",
      axis.text.x =
        ggplot2::element_text(
          size = tamanho_eixo_x,
          color = "black"
        ),
      axis.text.y =
        ggplot2::element_text(
          size = tamanho_eixo_y,
          color = "black"
        ),
      axis.title.y =
        ggplot2::element_text(
          size = tamanho_titulo_y,
          color = "black"
        ),
      axis.title.x =
        ggplot2::element_blank(),
      axis.line =
        ggplot2::element_line(
          linewidth = 0.8,
          color = "black"
        ),
      axis.ticks =
        ggplot2::element_line(
          color = "black"
        ),
      plot.margin =
        ggplot2::margin(
          20, 20, 10, 10
        )
    )


  # ----------------------------------------------------------
  # LETRAS CENTRAIS
  # ----------------------------------------------------------

  if (nrow(letras_c) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = letras_c,
        ggplot2::aes(
          x = Compartimento,
          y = y_letra,
          label = Letra
        ),
        inherit.aes = FALSE,
        family = "Arial",
        fontface = "bold",
        size = tamanho_letra,
        color = "black"
      )
  }


  # ----------------------------------------------------------
  # LETRAS SEPARADAS CTR / GDM
  # ----------------------------------------------------------

  if (nrow(letras_s) > 0) {
    p <- p +
      ggplot2::geom_text(
        data = letras_s,
        ggplot2::aes(
          x = x_letra,
          y = y_letra,
          label = Letra
        ),
        inherit.aes = FALSE,
        family = "Arial",
        fontface = "bold",
        size = tamanho_letra,
        color = "black"
      )
  }


  # ----------------------------------------------------------
  # BRACKET + ASTERISCO CTR vs GDM
  # ----------------------------------------------------------

  if (nrow(sig_grupo) > 0) {

    altura_perna <- espaco * 0.22

    p <- p +

      ggplot2::geom_segment(
        data = sig_grupo,
        ggplot2::aes(
          x = x1,
          xend = x2,
          y = y_bracket,
          yend = y_bracket
        ),
        inherit.aes = FALSE,
        linewidth = 0.7,
        color = "black"
      ) +

      ggplot2::geom_segment(
        data = sig_grupo,
        ggplot2::aes(
          x = x1,
          xend = x1,
          y = y_bracket,
          yend = y_bracket - altura_perna
        ),
        inherit.aes = FALSE,
        linewidth = 0.7,
        color = "black"
      ) +

      ggplot2::geom_segment(
        data = sig_grupo,
        ggplot2::aes(
          x = x2,
          xend = x2,
          y = y_bracket,
          yend = y_bracket - altura_perna
        ),
        inherit.aes = FALSE,
        linewidth = 0.7,
        color = "black"
      ) +

      ggplot2::geom_text(
        data = sig_grupo,
        ggplot2::aes(
          x = x_num,
          y = y_texto,
          label = estrelas
        ),
        inherit.aes = FALSE,
        family = "Arial",
        fontface = "bold",
        size = tamanho_asterisco,
        color = "black"
      )
  }


  # ----------------------------------------------------------
  # SALVAR
  # ----------------------------------------------------------

  ggplot2::ggsave(
    filename = file.path(
      "Graficos_Compartimentos_FINAL",
      paste0(
        nome_base,
        ".tiff"
      )
    ),
    plot = p,
    width = largura,
    height = altura,
    units = "in",
    dpi = 600,
    compression = "lzw"
  )

  ggplot2::ggsave(
    filename = file.path(
      "Graficos_Compartimentos_FINAL",
      paste0(
        nome_base,
        ".png"
      )
    ),
    plot = p,
    width = largura,
    height = altura,
    units = "in",
    dpi = 600
  )

  return(p)
}

# ============================================================
# 35. GERAR TODOS OS GRÁFICOS
# ============================================================


# ------------------------------------------------------------
# miR-39
# ------------------------------------------------------------

fig_mir39 <- criar_grafico_final(
  
  df = todos_long,
  
  mirna = "miR-39",
  
  ylab = "Ct",
  
  nome_base = "miR39_Compartimentos",
  
  y_limits = NULL,
  
  y_breaks = waiver(),
  
  tamanho_eixo_x = 15
  
)


# ------------------------------------------------------------
# miR-29a-3p
# ------------------------------------------------------------

fig_mir29 <- criar_grafico_final(
  
  df = todos_long,
  
  mirna = "miR-29a-3p",
  
  ylab = "Relative expression (fold change)",
  
  nome_base = "miR29a_Compartimentos",
  
  y_limits = NULL,
  
  y_breaks = waiver(),
  
  tamanho_eixo_x = 15
  
)


# ------------------------------------------------------------
# miR-132-3p
# ------------------------------------------------------------

fig_mir132 <- criar_grafico_final(
  
  df = todos_long,
  
  mirna = "miR-132-3p",
  
  ylab = "Relative expression (fold change)",
  
  nome_base = "miR132_Compartimentos",
  
  y_limits = NULL,
  
  y_breaks = waiver(),
  
  tamanho_eixo_x = 15
  
)


# ------------------------------------------------------------
# miR-150-5p
# ------------------------------------------------------------

fig_mir150 <- criar_grafico_final(
  
  df = todos_long,
  
  mirna = "miR-150-5p",
  
  ylab = "Relative expression (fold change)",
  
  nome_base = "miR150_Compartimentos",
  
  y_limits = NULL,
  
  y_breaks = waiver(),
  
  tamanho_eixo_x = 15
  
)


# ------------------------------------------------------------
# miR-155-5p
#
# SOMENTE CORD + DECIDUA
# ------------------------------------------------------------

fig_mir155 <- criar_grafico_mir155(
  
  df = todos_long,
  
  ylab = "Relative expression (fold change)",
  
  nome_base = "miR155_Cord_Decidua",
  
  y_limits = NULL,
  
  y_breaks = waiver(),
  
  tamanho_eixo_x = 15
  
)


# ------------------------------------------------------------
# miR-222-3p
# ------------------------------------------------------------

fig_mir222 <- criar_grafico_final(
  
  df = todos_long,
  
  mirna = "miR-222-3p",
  
  ylab = "Relative expression (fold change)",
  
  nome_base = "miR222_Compartimentos",
  
  y_limits = NULL,
  
  y_breaks = waiver(),
  
  tamanho_eixo_x = 15
  
)


# ============================================================
# 36. MOSTRAR TODOS OS GRÁFICOS
# ============================================================

fig_mir39

fig_mir29

fig_mir132

fig_mir150

fig_mir155

fig_mir222


# ============================================================
# 37. RESULTADOS SIGNIFICATIVOS
# DOS MODELOS DE 3 COMPARTIMENTOS
# ============================================================

resultado_pos_grupo |>
  dplyr::filter(
    p_BH < 0.05
  ) |>
  dplyr::select(
    miRNA,
    Grupo,
    contrast,
    estimate,
    p.value,
    p_BH,
    significancia
  )


# ============================================================
# 38. RESULTADOS DO miR-155
# ============================================================

comparacoes_mir155_CD

comparacoes_mir155_grupo



# ============================================================
# 39. SINGLE ENGLISH EXCEL WORKBOOK WITH ALL STATISTICAL RESULTS
# ============================================================

translate_results_to_english <- function(df) {

  if (is.null(df)) {
    return(NULL)
  }

  df <- as.data.frame(df)

  # Translate column names
  name_map <- c(
    "Compartimento" = "Compartment",
    "Grupo" = "Group",
    "Efeito" = "Effect",
    "Media_CTR" = "Mean_CTR",
    "Media_GDM" = "Mean_GDM",
    "Mediana_CTR" = "Median_CTR",
    "Mediana_GDM" = "Median_GDM",
    "Teste" = "Test",
    "Estatistica" = "Statistic",
    "significancia" = "Significance",
    "Letra" = "Letter",
    "p_BH" = "BH_adjusted_p"
  )

  new_names <- names(df)
  idx <- new_names %in% names(name_map)
  new_names[idx] <- unname(name_map[new_names[idx]])
  names(df) <- new_names

  # Translate compartment/group/effect/contrast text wherever present
  df <- dplyr::mutate(
    df,
    dplyr::across(
      where(is.factor),
      as.character
    )
  )

  df <- dplyr::mutate(
    df,
    dplyr::across(
      where(is.character),
      ~ stringr::str_replace_all(
        .x,
        c(
          "Mother" = "Maternal blood",
          "Cord" = "Umbilical cord blood",
          "Decidua" = "Decidual tissue",
          "Compartimento" = "Compartment",
          "Grupo" = "Group"
        )
      )
    )
  )

  return(df)
}


# ------------------------------------------------------------
# TABLES FOR EXPORT
# ------------------------------------------------------------

excel_sample_size <- translate_results_to_english(
  contagem_amostras
)

excel_nominal <- translate_results_to_english(
  resultados_nominais_grupo
)

excel_anova <- translate_results_to_english(
  resultado_anova
)

excel_posthoc_group <- translate_results_to_english(
  resultado_pos_grupo
)

excel_overall_compartment <- translate_results_to_english(
  resultado_compartimento
)

excel_letters <- translate_results_to_english(
  letras_modelo
)

excel_mir155_anova <- translate_results_to_english(
  anova_mir155_CD
)

excel_mir155_compartment <- translate_results_to_english(
  comparacoes_mir155_CD
)

excel_mir155_letters <- translate_results_to_english(
  letras_mir155
)

# For CTR vs GDM, use the NOMINAL Welch/Mann-Whitney results,
# not the BH-adjusted emmeans group comparison.
excel_mir155_nominal <- excel_nominal |>
  dplyr::filter(
    miRNA == "miR-155-5p",
    Compartment %in% c(
      "Umbilical cord blood",
      "Decidual tissue"
    )
  )


# ------------------------------------------------------------
# README / ANALYSIS KEY
# ------------------------------------------------------------

excel_readme <- data.frame(
  Section = c(
    "CTR vs GDM within each compartment",
    "Compartment comparisons within CTR or GDM",
    "Mixed model",
    "Multiple-testing correction",
    "Graph error bars",
    "Compartment letters",
    "CTR vs GDM asterisks"
  ),
  Description = c(
    "Two-sided Welch t-test or Mann-Whitney test selected according to Shapiro-Wilk normality results. Nominal p-values are reported without multiple-testing correction.",
    "Estimated marginal means from the mixed-effects model followed by pairwise compartment comparisons.",
    "Value ~ Compartment * Group + (1 | Code).",
    "Benjamini-Hochberg correction is applied to post hoc comparisons among compartments.",
    "Mean +/- SEM.",
    "Same letter indicates no significant difference; different letters indicate BH-adjusted p < 0.05 for compartment comparisons within the same group.",
    "Asterisks indicate nominal CTR vs GDM p-values from Welch t-test or Mann-Whitney: * p < 0.05; ** p < 0.01; *** p < 0.001; **** p < 0.0001."
  ),
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------
# CREATE FORMATTED WORKBOOK
# ------------------------------------------------------------

wb_english <- openxlsx::createWorkbook()

sheet_data <- list(
  "README" = excel_readme,
  "Sample_Size" = excel_sample_size,
  "CTR_vs_GDM_Nominal" = excel_nominal,
  "Mixed_Model_ANOVA" = excel_anova,
  "Compartment_Posthoc_BH" = excel_posthoc_group,
  "Overall_Compartment_BH" = excel_overall_compartment,
  "Compartment_Letters" = excel_letters,
  "miR155_ANOVA" = excel_mir155_anova,
  "miR155_Compartment_BH" = excel_mir155_compartment,
  "miR155_CTR_vs_GDM_Nominal" = excel_mir155_nominal,
  "miR155_Letters" = excel_mir155_letters
)

header_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 11,
  textDecoration = "bold",
  fgFill = "#D9EAF7",
  border = "Bottom",
  halign = "center",
  valign = "center"
)

body_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 10,
  valign = "center"
)

p_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 10,
  numFmt = "0.0000"
)

for (sheet_name in names(sheet_data)) {

  dat <- sheet_data[[sheet_name]]

  openxlsx::addWorksheet(
    wb_english,
    sheet_name
  )

  openxlsx::writeData(
    wb_english,
    sheet = sheet_name,
    x = dat,
    headerStyle = header_style,
    withFilter = TRUE
  )

  if (nrow(dat) > 0 && ncol(dat) > 0) {

    openxlsx::addStyle(
      wb_english,
      sheet = sheet_name,
      style = body_style,
      rows = 2:(nrow(dat) + 1),
      cols = 1:ncol(dat),
      gridExpand = TRUE,
      stack = TRUE
    )

    # Format p-value columns
    p_cols <- grep(
      "p$|p_value|p.value|adjusted_p|Shapiro",
      names(dat),
      ignore.case = TRUE
    )

    if (length(p_cols) > 0) {
      openxlsx::addStyle(
        wb_english,
        sheet = sheet_name,
        style = p_style,
        rows = 2:(nrow(dat) + 1),
        cols = p_cols,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
  }

  openxlsx::freezePane(
    wb_english,
    sheet = sheet_name,
    firstRow = TRUE
  )

  openxlsx::setColWidths(
    wb_english,
    sheet = sheet_name,
    cols = 1:ncol(dat),
    widths = "auto"
  )

  # Prevent excessively wide columns
  if (ncol(dat) > 0) {
    for (j in seq_len(ncol(dat))) {
      openxlsx::setColWidths(
        wb_english,
        sheet = sheet_name,
        cols = j,
        widths = min(
          max(
            nchar(names(dat)[j]) + 2,
            12
          ),
          35
        )
      )
    }
  }
}


# README needs more width for the description
openxlsx::setColWidths(
  wb_english,
  sheet = "README",
  cols = 1,
  widths = 32
)

openxlsx::setColWidths(
  wb_english,
  sheet = "README",
  cols = 2,
  widths = 90
)

openxlsx::addStyle(
  wb_english,
  sheet = "README",
  style = openxlsx::createStyle(
    wrapText = TRUE,
    valign = "top",
    fontName = "Arial",
    fontSize = 10
  ),
  rows = 2:(nrow(excel_readme) + 1),
  cols = 1:2,
  gridExpand = TRUE,
  stack = TRUE
)


# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

openxlsx::saveWorkbook(
  wb_english,
  file = "Supplementary_miRNA_Statistical_Results.xlsx",
  overwrite = TRUE
)

cat(
  "\nEnglish Excel workbook created:\n",
  "Supplementary_miRNA_Statistical_Results.xlsx\n"
)



# ============================================================
# FINAL PANEL — FLOW CYTOMETRY + miRNA
# ============================================================
# Expected objects generated above:
#   fig_cd28, fig_ctla4, fig_mir132, fig_mir29, fig_mir155
#
# Layout:
#   A-B = flow cytometry
#   C-E = miRNAs
# ============================================================

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop(
    "Package 'patchwork' is required for the final panel. ",
    "Install it once with install.packages('patchwork')."
  )
}

# Check that all final figure objects exist before assembling the panel
objetos_painel <- c(
  "fig_cd28",
  "fig_ctla4",
  "fig_mir132",
  "fig_mir29",
  "fig_mir155"
)

objetos_ausentes <- objetos_painel[
  !vapply(
    objetos_painel,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(objetos_ausentes) > 0) {
  stop(
    "The following figure objects were not created: ",
    paste(objetos_ausentes, collapse = ", ")
  )
}

# Re-enable legends only in copies used by patchwork.
# patchwork will collect them into one legend at the bottom.
fig_cd28_leg <- fig_cd28 +
  ggplot2::theme(
    legend.position = "bottom"
  )

fig_ctla4_leg <- fig_ctla4 +
  ggplot2::theme(
    legend.position = "bottom"
  )

fig_mir132_leg <- fig_mir132 +
  ggplot2::theme(
    legend.position = "bottom"
  )

fig_mir29_leg <- fig_mir29 +
  ggplot2::theme(
    legend.position = "bottom"
  )

fig_mir155_leg <- fig_mir155 +
  ggplot2::theme(
    legend.position = "bottom"
  )


# ------------------------------------------------------------
# TOP ROW — FLOW CYTOMETRY
# ------------------------------------------------------------

painel_citometria <- patchwork::wrap_plots(
  fig_cd28_leg,
  fig_ctla4_leg,
  ncol = 2
)


# ------------------------------------------------------------
# BOTTOM ROW — miRNAs
# ------------------------------------------------------------

painel_mirnas <- patchwork::wrap_plots(
  fig_mir132_leg,
  fig_mir29_leg,
  fig_mir155_leg,
  ncol = 3
)


# ------------------------------------------------------------
# COMPLETE PANEL
# ------------------------------------------------------------

painel_final <- patchwork::wrap_plots(
  painel_citometria,
  painel_mirnas,
  ncol = 1,
  heights = c(
    1.15,
    1
  )
) +
  patchwork::plot_layout(
    guides = "collect"
  ) +
  patchwork::plot_annotation(
    tag_levels = "A"
  ) &
  ggplot2::theme(
    legend.position = "bottom",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = "Arial",
      size = 14,
      color = "black"
    ),
    legend.key.size = grid::unit(
      0.6,
      "cm"
    ),
    plot.tag = ggplot2::element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      color = "black"
    )
  )

print(painel_final)


# ------------------------------------------------------------
# SAVE FINAL PANEL
# ------------------------------------------------------------

pasta_painel <- "Painel_FINAL"

dir.create(
  pasta_painel,
  recursive = TRUE,
  showWarnings = FALSE
)

ggplot2::ggsave(
  filename = file.path(
    pasta_painel,
    "Painel_FINAL.png"
  ),
  plot = painel_final,
  width = 16,
  height = 10,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_painel,
    "Painel_FINAL.tiff"
  ),
  plot = painel_final,
  width = 16,
  height = 10,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_painel,
    "Painel_FINAL.svg"
  ),
  plot = painel_final,
  width = 16,
  height = 10,
  units = "in",
  device = svglite::svglite,
  bg = "white"
)

cat(
  "\n============================================================\n",
  "ANALYSIS COMPLETED.\n",
  "Final panel saved in:\n",
  normalizePath(pasta_painel),
  "\n============================================================\n"
)
