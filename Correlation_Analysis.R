
# ============================================================
# 1. PACKAGES
# ============================================================

pacotes <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "purrr",
  "tibble",
  "ggplot2",
  "patchwork",
  "openxlsx",
  "svglite"
)

for (pkg in pacotes) {
  
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  
  library(
    pkg,
    character.only = TRUE
  )
}


# ============================================================
# 2. WORKING DIRECTORY
# ============================================================

pasta_principal <- paste0(
  "C:/Users/beatr/Dropbox/Beatriz/Resultados miRs/",
  "Coreelação novo/mãe"
)

setwd(
  pasta_principal
)


arquivo <- file.path(
  pasta_principal,
  "dados mães.xlsx"
)


# ============================================================
# 3. OUTPUT FOLDERS
# ============================================================

pasta_resultados <- file.path(
  pasta_principal,
  "Resultados_Correlacoes_FINAL"
)

pasta_figuras <- file.path(
  pasta_principal,
  "Figuras_Correlacoes_FINAL"
)

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
# 4. RANDOM SEED
# ============================================================

set.seed(
  1234
)


# ============================================================
# 5. READ WORKBOOK
# ============================================================

abas <- readxl::excel_sheets(
  arquivo
)

print(
  abas
)


# ============================================================
# 6. AUTOMATICALLY IDENTIFY COMPARTMENT SHEETS
# ============================================================

normalizar_texto <- function(x) {
  
  x %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all(
      "[áàãâä]",
      "a"
    ) %>%
    stringr::str_replace_all(
      "[éèêë]",
      "e"
    ) %>%
    stringr::str_replace_all(
      "[íìîï]",
      "i"
    ) %>%
    stringr::str_replace_all(
      "[óòõôö]",
      "o"
    ) %>%
    stringr::str_replace_all(
      "[úùûü]",
      "u"
    )
}


abas_norm <- normalizar_texto(
  abas
)


achar_aba <- function(
    padroes
) {
  
  indice <- which(
    Reduce(
      `|`,
      lapply(
        padroes,
        function(p) {
          stringr::str_detect(
            abas_norm,
            p
          )
        }
      )
    )
  )
  
  
  if (length(indice) == 0) {
    return(NA_character_)
  }
  
  
  abas[
    indice[1]
  ]
}


aba_mother <- achar_aba(
  c(
    "mae",
    "mother",
    "maternal"
  )
)

aba_cord <- achar_aba(
  c(
    "cord",
    "cordao",
    "cordão"
  )
)

aba_decidua <- achar_aba(
  c(
    "decid",
    "descid"
  )
)


cat(
  "\nSheets identified:\n",
  "Mother: ", aba_mother, "\n",
  "Cord: ", aba_cord, "\n",
  "Decidua: ", aba_decidua, "\n"
)


if (
  any(
    is.na(
      c(
        aba_mother,
        aba_cord,
        aba_decidua
      )
    )
  )
) {
  
  stop(
    paste0(
      "Could not automatically identify all three compartments.\n",
      "Check the sheet names printed above."
    )
  )
}


# ============================================================
# 7. READ DATA
# ============================================================

dados_mother <- readxl::read_excel(
  arquivo,
  sheet = aba_mother
)

dados_cord <- readxl::read_excel(
  arquivo,
  sheet = aba_cord
)

dados_decidua <- readxl::read_excel(
  arquivo,
  sheet = aba_decidua
)


# ============================================================
# 8. STANDARDIZE BASIC COLUMNS
# ============================================================

padronizar_base <- function(df) {
  
  names(df) <- stringr::str_trim(
    names(df)
  )
  
  
  # ----------------------------------------------------------
  # Find participant ID
  # ----------------------------------------------------------
  
  coluna_code <- names(df)[
    normalizar_texto(
      names(df)
    ) %in%
      c(
        "code",
        "codigo",
        "id",
        "participant"
      )
  ][1]
  
  
  if (
    is.na(
      coluna_code
    )
  ) {
    stop(
      "Participant ID column not found."
    )
  }
  
  
  # ----------------------------------------------------------
  # Find group
  # ----------------------------------------------------------
  
  nomes_norm <- normalizar_texto(
    names(df)
  )
  
  
  candidatos_grupo <- which(
    nomes_norm %in%
      c(
        "sample type",
        "sample.type",
        "group",
        "grupo"
      )
  )
  
  
  if (
    length(
      candidatos_grupo
    ) == 0
  ) {
    
    candidatos_grupo <- which(
      stringr::str_detect(
        nomes_norm,
        "sample.*type|grupo|group"
      )
    )
  }
  
  
  coluna_grupo <- names(df)[
    candidatos_grupo[1]
  ]
  
  
  if (
    is.na(
      coluna_grupo
    )
  ) {
    stop(
      "Group column not found."
    )
  }
  
  
  names(df)[
    names(df) == coluna_code
  ] <- "Code"
  
  
  names(df)[
    names(df) == coluna_grupo
  ] <- "Group"
  
  
  df %>%
    
    dplyr::mutate(
      
      Code = as.character(
        Code
      ),
      
      Group = stringr::str_to_upper(
        stringr::str_trim(
          as.character(
            Group
          )
        )
      ),
      
      Group = dplyr::case_when(
        
        Group %in%
          c(
            "CONTROL",
            "CONTROLE",
            "CTR"
          ) ~ "CTR",
        
        Group %in%
          c(
            "GDM",
            "DMG"
          ) ~ "GDM",
        
        TRUE ~ Group
      )
    )
}


dados_mother <- padronizar_base(
  dados_mother
)

dados_cord <- padronizar_base(
  dados_cord
)

dados_decidua <- padronizar_base(
  dados_decidua
)


# ============================================================
# 9. DATA LIST
# ============================================================

lista_dados <- list(
  
  Mother = dados_mother,
  
  Cord = dados_cord,
  
  Decidua = dados_decidua
)


# ============================================================
# 10. VARIABLE DEFINITIONS
# ============================================================

variaveis_mirna <- c(
  "miR29a",
  "miR132",
  "miR150-5p",
  "miR155-5p",
  "miR222-3p"
)


variaveis_citometria <- c(
  
  "Lymphocytes",
  
  "CD45+CD3+",
  
  "CD45+CD3+CD4+",
  
  "CD45+CD3+CD4+CD28+",
  
  "CD45+CD3+CD4+CTLA4+",
  
  "CD45+CD3+CD4+IFNG+",
  
  "CD45+CD3+CD4+IL2+",
  
  "CD45+CD3+CD4+IL17+",
  
  "CD45+CD3+CD4+TGFB+",
  
  "CD45+CD3+CD8+",
  
  "CD45+CD3+CD8+CD28+",
  
  "CD45+CD3+CD8+CTLA4+",
  
  "CD45+CD3+CD8+IFNG+",
  
  "CD45+CD3+CD8+IL2+",
  
  "CD45+CD3+CD8+IL17+",
  
  "CD45+CD3+CD8+TGFB+"
)


# ============================================================
# 11. IDENTIFY ACTUAL COLUMN NAMES
#
# Allows minor differences such as:
# miR 29a / miR29a / miR-29a-3p
# ============================================================

identificar_mirna <- function(
    nomes,
    alvo
) {
  
  nomes_s <- nomes %>%
    
    normalizar_texto() %>%
    
    stringr::str_replace_all(
      "[^a-z0-9]",
      ""
    )
  
  
  alvo_s <- alvo %>%
    
    normalizar_texto() %>%
    
    stringr::str_replace_all(
      "[^a-z0-9]",
      ""
    )
  
  
  idx <- which(
    nomes_s == alvo_s
  )
  
  
  if (
    length(
      idx
    ) > 0
  ) {
    
    return(
      nomes[
        idx[1]
      ]
    )
  }
  
  
  NA_character_
}


# ============================================================
# 12. STANDARDIZE miRNA COLUMN NAMES INTERNALLY
# ============================================================

padronizar_mirnas <- function(df) {
  
  mapa <- list(
    
    miR29a = c(
      "miR29a",
      "miR29a-3p",
      "miR-29a-3p",
      "miR 29a"
    ),
    
    `miR132` = c(
      "miR132",
      "miR132-3p",
      "miR-132-3p",
      "miR 132"
    ),
    
    `miR150-5p` = c(
      "miR150-5p",
      "miR-150-5p",
      "miR 150-5p"
    ),
    
    `miR155-5p` = c(
      "miR155-5p",
      "miR-155-5p",
      "miR 155-5p"
    ),
    
    `miR222-3p` = c(
      "miR222-3p",
      "miR-222-3p",
      "miR 222-3p"
    )
  )
  
  
  for (
    nome_final in names(
      mapa
    )
  ) {
    
    candidatos <- mapa[[
      nome_final
    ]]
    
    
    nomes_s <- normalizar_texto(
      names(df)
    ) %>%
      
      stringr::str_replace_all(
        "[^a-z0-9]",
        ""
      )
    
    
    candidatos_s <- normalizar_texto(
      candidatos
    ) %>%
      
      stringr::str_replace_all(
        "[^a-z0-9]",
        ""
      )
    
    
    idx <- which(
      nomes_s %in%
        candidatos_s
    )
    
    
    if (
      length(
        idx
      ) > 0
    ) {
      
      names(df)[
        idx[1]
      ] <- nome_final
    }
  }
  
  
  df
}


lista_dados <- lapply(
  lista_dados,
  padronizar_mirnas
)


# ============================================================
# 13. ENSURE NUMERIC VARIABLES ARE NUMERIC
# ============================================================

converter_numerico <- function(x) {
  
  if (
    is.numeric(
      x
    )
  ) {
    return(
      x
    )
  }
  
  
  x <- as.character(
    x
  )
  
  
  x <- stringr::str_replace_all(
    x,
    ",",
    "."
  )
  
  
  suppressWarnings(
    as.numeric(
      x
    )
  )
}


lista_dados <- lapply(
  lista_dados,
  function(df) {
    
    df %>%
      
      dplyr::mutate(
        
        dplyr::across(
          
          -dplyr::all_of(
            c(
              "Code",
              "Group"
            )
          ),
          
          converter_numerico
        )
      )
  }
)


# ============================================================
# 14. DETERMINE CLINICAL VARIABLES
# ============================================================

variaveis_excluir <- unique(
  c(
    "Code",
    "Group",
    variaveis_mirna,
    variaveis_citometria
  )
)


variaveis_clinicas <- setdiff(
  names(
    lista_dados$Mother
  ),
  variaveis_excluir
)


# Keep only numeric clinical variables
variaveis_clinicas <- variaveis_clinicas[
  
  sapply(
    
    lista_dados$Mother[
      variaveis_clinicas
    ],
    
    is.numeric
  )
]


cat(
  "\nClinical variables identified:\n"
)

print(
  variaveis_clinicas
)


# ============================================================
# 15. SPEARMAN + BOOTSTRAP CI
# ============================================================

cor_spearman_boot <- function(
    x,
    y,
    R = 2000
) {
  
  
  validos <- stats::complete.cases(
    x,
    y
  )
  
  
  x <- x[
    validos
  ]
  
  y <- y[
    validos
  ]
  
  
  n <- length(
    x
  )
  
  
  if (
    n < 2
  ) {
    
    return(
      
      tibble::tibble(
        
        n = n,
        
        rho = NA_real_,
        
        CI_low = NA_real_,
        
        CI_high = NA_real_,
        
        p_value = NA_real_
      )
    )
  }
  
  
  teste <- suppressWarnings(
    
    stats::cor.test(
      
      x,
      y,
      
      method = "spearman",
      
      exact = FALSE
    )
  )
  
  
  rho <- unname(
    teste$estimate
  )
  
  
  p <- teste$p.value
  
  
  # ----------------------------------------------------------
  # Bootstrap percentile CI
  # ----------------------------------------------------------
  
  boots <- replicate(
    
    R,
    
    {
      
      idx <- sample(
        seq_len(
          n
        ),
        size = n,
        replace = TRUE
      )
      
      
      suppressWarnings(
        
        stats::cor(
          
          x[
            idx
          ],
          
          y[
            idx
          ],
          
          method = "spearman",
          
          use = "complete.obs"
        )
      )
    }
  )
  
  
  boots <- boots[
    is.finite(
      boots
    )
  ]
  
  
  if (
    length(
      boots
    ) >= 20
  ) {
    
    ci <- stats::quantile(
      
      boots,
      
      probs = c(
        0.025,
        0.975
      ),
      
      na.rm = TRUE,
      
      names = FALSE
    )
    
  } else {
    
    ci <- c(
      NA_real_,
      NA_real_
    )
  }
  
  
  tibble::tibble(
    
    n = n,
    
    rho = rho,
    
    CI_low = ci[1],
    
    CI_high = ci[2],
    
    p_value = p
  )
}


# ============================================================
# 16. FUNCTION TO ANALYZE ONE PAIR
# ============================================================

analisar_par <- function(
    df,
    var1,
    var2,
    grupo,
    compartimento,
    familia
) {
  
  
  dados_g <- df %>%
    
    dplyr::filter(
      Group == grupo
    )
  
  
  resultado <- cor_spearman_boot(
    
    dados_g[[
      var1
    ]],
    
    dados_g[[
      var2
    ]]
  )
  
  
  resultado %>%
    
    dplyr::mutate(
      
      Group = grupo,
      
      Compartment = compartimento,
      
      Family = familia,
      
      Variable_1 = var1,
      
      Variable_2 = var2,
      
      .before = 1
    )
}


# ============================================================
# 17. UNIQUE PAIRS
# ============================================================

criar_pares_unicos <- function(
    vars
) {
  
  
  vars <- unique(
    vars
  )
  
  
  if (
    length(
      vars
    ) < 2
  ) {
    
    return(
      list()
    )
  }
  
  
  combn(
    
    vars,
    
    2,
    
    simplify = FALSE
  )
}


# ============================================================
# 18. CROSS-DOMAIN PAIRS
# ============================================================

criar_pares_cruzados <- function(
    vars1,
    vars2
) {

  if (
    length(vars1) == 0 ||
    length(vars2) == 0
  ) {
    return(list())
  }

  grade <- expand.grid(
    Variable_1 = vars1,
    Variable_2 = vars2,
    stringsAsFactors = FALSE
  )

  lapply(
    seq_len(nrow(grade)),
    function(i) {
      c(
        grade$Variable_1[i],
        grade$Variable_2[i]
      )
    }
  )
}


# ============================================================
# 19. ANALYZE ONE FAMILY
# ============================================================

analisar_familia <- function(
    df,
    grupo,
    compartimento,
    familia,
    pares
) {
  
  
  if (
    length(
      pares
    ) == 0
  ) {
    
    return(
      tibble::tibble()
    )
  }
  
  
  purrr::map_dfr(
    
    pares,
    
    function(par) {
      
      var1 <- par[1]
      var2 <- par[2]
      
      
      if (
        !all(
          c(
            var1,
            var2
          ) %in%
          names(
            df
          )
        )
      ) {
        
        return(
          tibble::tibble()
        )
      }
      
      
      analisar_par(
        
        df = df,
        
        var1 = var1,
        
        var2 = var2,
        
        grupo = grupo,
        
        compartimento = compartimento,
        
        familia = familia
      )
    }
  )
}


# ============================================================
# 20. WITHIN-COMPARTMENT ANALYSES
# ============================================================

resultados_dentro <- tibble::tibble()


for (
  compartimento in names(
    lista_dados
  )
) {
  
  
  df <- lista_dados[[
    compartimento
  ]]
  
  
  mirnas_disponiveis <- intersect(
    variaveis_mirna,
    names(
      df
    )
  )
  
  
  cito_disponiveis <- intersect(
    variaveis_citometria,
    names(
      df
    )
  )
  
  
  clinicas_disponiveis <- intersect(
    variaveis_clinicas,
    names(
      df
    )
  )
  
  
  for (
    grupo in c(
      "CTR",
      "GDM"
    )
  ) {
    
    
    # --------------------------------------------------------
    # miRNA × miRNA
    # --------------------------------------------------------
    
    resultados_dentro <- dplyr::bind_rows(
      
      resultados_dentro,
      
      analisar_familia(
        
        df,
        
        grupo,
        
        compartimento,
        
        "miRNA x miRNA",
        
        criar_pares_unicos(
          mirnas_disponiveis
        )
      )
    )
    
    
    # --------------------------------------------------------
    # Cytometry × Cytometry
    # --------------------------------------------------------
    
    resultados_dentro <- dplyr::bind_rows(
      
      resultados_dentro,
      
      analisar_familia(
        
        df,
        
        grupo,
        
        compartimento,
        
        "Cytometry x Cytometry",
        
        criar_pares_unicos(
          cito_disponiveis
        )
      )
    )
    
    
    # --------------------------------------------------------
    # miRNA × Cytometry
    # --------------------------------------------------------
    
    resultados_dentro <- dplyr::bind_rows(
      
      resultados_dentro,
      
      analisar_familia(
        
        df,
        
        grupo,
        
        compartimento,
        
        "miRNA x Cytometry",
        
        criar_pares_cruzados(
          mirnas_disponiveis,
          cito_disponiveis
        )
      )
    )
    
    
    # --------------------------------------------------------
    # miRNA × Clinical
    # --------------------------------------------------------
    
    resultados_dentro <- dplyr::bind_rows(
      
      resultados_dentro,
      
      analisar_familia(
        
        df,
        
        grupo,
        
        compartimento,
        
        "miRNA x Clinical",
        
        criar_pares_cruzados(
          mirnas_disponiveis,
          clinicas_disponiveis
        )
      )
    )
    
    
    # --------------------------------------------------------
    # Cytometry × Clinical
    # --------------------------------------------------------
    
    resultados_dentro <- dplyr::bind_rows(
      
      resultados_dentro,
      
      analisar_familia(
        
        df,
        
        grupo,
        
        compartimento,
        
        "Cytometry x Clinical",
        
        criar_pares_cruzados(
          cito_disponiveis,
          clinicas_disponiveis
        )
      )
    )
  }
}


# ============================================================
# 21. CLINICAL × CLINICAL
#
# Clinical variables occur once per participant.
# Analyze only once, using Mother sheet as participant table.
# ============================================================

resultados_clinicos <- tibble::tibble()


pares_clinicos <- criar_pares_unicos(
  variaveis_clinicas
)


for (
  grupo in c(
    "CTR",
    "GDM"
  )
) {
  
  resultados_clinicos <- dplyr::bind_rows(
    
    resultados_clinicos,
    
    analisar_familia(
      
      df = lista_dados$Mother,
      
      grupo = grupo,
      
      compartimento = "Participant-level",
      
      familia = "Clinical x Clinical",
      
      pares = pares_clinicos
    )
  )
}


# ============================================================
# 22. COMBINE ALL WITHIN-DOMAIN RESULTS
# ============================================================

resultados <- dplyr::bind_rows(
  resultados_dentro,
  resultados_clinicos
)


# ============================================================
# 23. STATUS ACCORDING TO N
# ============================================================

resultados <- resultados %>%
  
  dplyr::mutate(
    
    Status = dplyr::case_when(
      
      n >= 8 ~ "Primary n>=8",
      
      n >= 5 & n <= 7 ~ "Exploratory n=5-7",
      
      TRUE ~ "Not analyzed n<5"
    )
  )


# ============================================================
# 24. BH FOR PRIMARY WITHIN-COMPARTMENT
# ============================================================

resultados <- resultados %>%
  
  dplyr::mutate(
    p_BH = NA_real_
  )


# ------------------------------------------------------------
# Non-clinical families:
# BH within Compartment + Group + Family
# ------------------------------------------------------------

tmp_nao_clinico <- resultados %>%
  
  dplyr::filter(
    Family != "Clinical x Clinical"
  ) %>%
  
  dplyr::group_by(
    Compartment,
    Group,
    Family,
    Status
  ) %>%
  
  dplyr::mutate(
    
    p_BH_temp = dplyr::if_else(
      
      Status %in%
        c(
          "Primary n>=8",
          "Exploratory n=5-7"
        ),
      
      stats::p.adjust(
        p_value,
        method = "BH"
      ),
      
      NA_real_
    )
  ) %>%
  
  dplyr::ungroup()


# ------------------------------------------------------------
# Clinical:
# BH within Group + Family
# ------------------------------------------------------------

tmp_clinico <- resultados %>%
  
  dplyr::filter(
    Family == "Clinical x Clinical"
  ) %>%
  
  dplyr::group_by(
    Group,
    Family,
    Status
  ) %>%
  
  dplyr::mutate(
    
    p_BH_temp = dplyr::if_else(
      
      Status %in%
        c(
          "Primary n>=8",
          "Exploratory n=5-7"
        ),
      
      stats::p.adjust(
        p_value,
        method = "BH"
      ),
      
      NA_real_
    )
  ) %>%
  
  dplyr::ungroup()


resultados <- dplyr::bind_rows(
  tmp_nao_clinico,
  tmp_clinico
) %>%
  
  dplyr::select(
    -p_BH,
    p_BH = p_BH_temp
  ) %>%
  
  dplyr::mutate(
    
    FDR_significant = dplyr::if_else(
      
      !is.na(
        p_BH
      ) &
        p_BH < 0.05,
      
      "Yes",
      
      "No"
    )
  )


# ============================================================
# 25. CROSS-COMPARTMENT ANALYSIS
# ============================================================

pares_compartimentos <- list(
  
  c(
    "Mother",
    "Cord"
  ),
  
  c(
    "Mother",
    "Decidua"
  ),
  
  c(
    "Cord",
    "Decidua"
  )
)


# Focused co-signalling markers
variaveis_cross_cito <- c(
  
  "CD45+CD3+CD4+CD28+",
  
  "CD45+CD3+CD4+CTLA4+",
  
  "CD45+CD3+CD8+CD28+",
  
  "CD45+CD3+CD8+CTLA4+"
)


variaveis_cross_mirna <- variaveis_mirna


# ============================================================
# 26. FUNCTION FOR CROSS-COMPARTMENT CORRELATION
# ============================================================

analisar_cross <- function(
    comp1,
    comp2,
    grupo,
    variavel,
    familia
) {
  
  
  df1 <- lista_dados[[
    comp1
  ]] %>%
    
    dplyr::filter(
      Group == grupo
    ) %>%
    
    dplyr::select(
      Code,
      dplyr::all_of(
        variavel
      )
    )
  
  
  names(
    df1
  )[2] <- "X"
  
  
  df2 <- lista_dados[[
    comp2
  ]] %>%
    
    dplyr::filter(
      Group == grupo
    ) %>%
    
    dplyr::select(
      Code,
      dplyr::all_of(
        variavel
      )
    )
  
  
  names(
    df2
  )[2] <- "Y"
  
  
  df <- dplyr::inner_join(
    df1,
    df2,
    by = "Code"
  )
  
  
  res <- cor_spearman_boot(
    df$X,
    df$Y
  )
  
  
  res %>%
    
    dplyr::mutate(
      
      Group = grupo,
      
      Family = familia,
      
      Compartment_1 = comp1,
      
      Compartment_2 = comp2,
      
      Variable = variavel,
      
      .before = 1
    )
}


# ============================================================
# 27. RUN CROSS-COMPARTMENT ANALYSES
# ============================================================

resultados_cross <- tibble::tibble()


for (
  grupo in c(
    "CTR",
    "GDM"
  )
) {
  
  
  for (
    par_comp in pares_compartimentos
  ) {
    
    
    comp1 <- par_comp[1]
    comp2 <- par_comp[2]
    
    
    # --------------------------------------------------------
    # miRNAs
    # --------------------------------------------------------
    
    for (
      variavel in variaveis_cross_mirna
    ) {
      
      
      if (
        variavel %in%
        names(
          lista_dados[[
            comp1
          ]]
        ) &&
        
        variavel %in%
        names(
          lista_dados[[
            comp2
          ]]
        )
      ) {
        
        resultados_cross <- dplyr::bind_rows(
          
          resultados_cross,
          
          analisar_cross(
            
            comp1,
            
            comp2,
            
            grupo,
            
            variavel,
            
            "miRNA cross-compartment"
          )
        )
      }
    }
    
    
    # --------------------------------------------------------
    # Cytometry
    # --------------------------------------------------------
    
    for (
      variavel in variaveis_cross_cito
    ) {
      
      
      if (
        variavel %in%
        names(
          lista_dados[[
            comp1
          ]]
        ) &&
        
        variavel %in%
        names(
          lista_dados[[
            comp2
          ]]
        )
      ) {
        
        resultados_cross <- dplyr::bind_rows(
          
          resultados_cross,
          
          analisar_cross(
            
            comp1,
            
            comp2,
            
            grupo,
            
            variavel,
            
            "T-cell cross-compartment"
          )
        )
      }
    }
  }
}


# ============================================================
# 28. CROSS-COMPARTMENT STATUS
# ============================================================

resultados_cross <- resultados_cross %>%
  
  dplyr::mutate(
    
    Status = dplyr::case_when(
      
      n >= 8 ~ "Primary n>=8",
      
      n >= 5 & n <= 7 ~ "Exploratory n=5-7",
      
      TRUE ~ "Not analyzed n<5"
    )
  )


# ============================================================
# 29. BH CROSS-COMPARTMENT
#
# BH within:
# Group + Family + Status
# ============================================================

resultados_cross <- resultados_cross %>%
  
  dplyr::group_by(
    Group,
    Family,
    Status
  ) %>%
  
  dplyr::mutate(
    
    p_BH = dplyr::if_else(
      
      Status %in%
        c(
          "Primary n>=8",
          "Exploratory n=5-7"
        ),
      
      stats::p.adjust(
        p_value,
        method = "BH"
      ),
      
      NA_real_
    )
  ) %>%
  
  dplyr::ungroup() %>%
  
  dplyr::mutate(
    
    FDR_significant = dplyr::if_else(
      
      !is.na(
        p_BH
      ) &
        p_BH < 0.05,
      
      "Yes",
      
      "No"
    )
  )


# ============================================================
# 30. PRESENTATION NAMES
# ============================================================

nome_compartimento <- function(x) {
  
  dplyr::case_when(
    
    x == "Mother" ~
      "Maternal blood",
    
    x == "Cord" ~
      "Umbilical cord blood",
    
    x == "Decidua" ~
      "Decidual tissue",
    
    x == "Participant-level" ~
      "Participant-level",
    
    TRUE ~ x
  )
}


nome_variavel <- function(x) {
  
  
  dplyr::case_when(
    
    
    # --------------------------------------------------------
    # miRNAs
    # --------------------------------------------------------
    
    x == "miR29a" ~
      "miR-29a-3p",
    
    x == "miR132" ~
      "miR-132-3p",
    
    x == "miR150-5p" ~
      "miR-150-5p",
    
    x == "miR155-5p" ~
      "miR-155-5p",
    
    x == "miR222-3p" ~
      "miR-222-3p",
    
    
    # --------------------------------------------------------
    # Cytometry – CD4+
    # --------------------------------------------------------
    
    x == "Lymphocytes" ~
      "Lymphocytes",
    
    x == "CD45+CD3+" ~
      "CD3+",
    
    x == "CD45+CD3+CD4+" ~
      "CD4+",
    
    x == "CD45+CD3+CD4+CD28+" ~
      "CD4+CD28+",
    
    x == "CD45+CD3+CD4+CTLA4+" ~
      "CD4+CTLA-4+",
    
    x == "CD45+CD3+CD4+IFNG+" ~
      "CD4+IFNγ+",
    
    x == "CD45+CD3+CD4+IL2+" ~
      "CD4+IL-2+",
    
    x == "CD45+CD3+CD4+IL17+" ~
      "CD4+IL-17+",
    
    x == "CD45+CD3+CD4+TGFB+" ~
      "CD4+TGFβ+",
    
    
    # --------------------------------------------------------
    # OLD CD8+ → DISPLAYED AS CD4−
    # --------------------------------------------------------
    
    x == "CD45+CD3+CD8+" ~
      "CD4−",
    
    x == "CD45+CD3+CD8+CD28+" ~
      "CD4−CD28+",
    
    x == "CD45+CD3+CD8+CTLA4+" ~
      "CD4−CTLA-4+",
    
    x == "CD45+CD3+CD8+IFNG+" ~
      "CD4−IFNγ+",
    
    x == "CD45+CD3+CD8+IL2+" ~
      "CD4−IL-2+",
    
    x == "CD45+CD3+CD8+IL17+" ~
      "CD4−IL-17+",
    
    x == "CD45+CD3+CD8+TGFB+" ~
      "CD4−TGFβ+",
    
    
    TRUE ~ x
  )
}


# ============================================================
# 31. CREATE EXPORT COPIES WITH CORRECTED NAMES
#
# Internal objects remain unchanged.
# ============================================================

resultados_export <- resultados %>%
  
  dplyr::mutate(
    
    Compartment = nome_compartimento(
      Compartment
    ),
    
    Variable_1 = nome_variavel(
      Variable_1
    ),
    
    Variable_2 = nome_variavel(
      Variable_2
    )
  ) %>%
  
  dplyr::arrange(
    
    Status,
    
    Compartment,
    
    Group,
    
    Family,
    
    p_BH,
    
    p_value
  )


resultados_cross_export <- resultados_cross %>%
  
  dplyr::mutate(
    
    Compartment_1 = nome_compartimento(
      Compartment_1
    ),
    
    Compartment_2 = nome_compartimento(
      Compartment_2
    ),
    
    Variable = nome_variavel(
      Variable
    )
  ) %>%
  
  dplyr::arrange(
    
    Status,
    
    Group,
    
    Family,
    
    p_BH,
    
    p_value
  )


# ============================================================
# 32. SEPARATE PRIMARY / EXPLORATORY / N<5
# ============================================================

resultado_primary <- resultados_export %>%
  
  dplyr::filter(
    Status == "Primary n>=8"
  )


resultado_exploratory <- resultados_export %>%
  
  dplyr::filter(
    Status == "Exploratory n=5-7"
  )


resultado_n_lt_5 <- resultados_export %>%
  
  dplyr::filter(
    Status == "Not analyzed n<5"
  )


cross_primary <- resultados_cross_export %>%
  
  dplyr::filter(
    Status == "Primary n>=8"
  )


cross_exploratory <- resultados_cross_export %>%
  
  dplyr::filter(
    Status == "Exploratory n=5-7"
  )


cross_n_lt_5 <- resultados_cross_export %>%
  
  dplyr::filter(
    Status == "Not analyzed n<5"
  )


# ============================================================
# 33. FDR-SIGNIFICANT RESULTS
# ============================================================

primary_fdr <- resultado_primary %>%
  
  dplyr::filter(
    FDR_significant == "Yes"
  )


exploratory_fdr <- resultado_exploratory %>%
  
  dplyr::filter(
    FDR_significant == "Yes"
  )


cross_primary_fdr <- cross_primary %>%
  
  dplyr::filter(
    FDR_significant == "Yes"
  )


cross_exploratory_fdr <- cross_exploratory %>%
  
  dplyr::filter(
    FDR_significant == "Yes"
  )


# ============================================================
# 34. ROUNDING FOR EXCEL
# ============================================================

arredondar_resultados <- function(df) {
  
  df %>%
    
    dplyr::mutate(
      
      rho = round(
        rho,
        4
      ),
      
      CI_low = round(
        CI_low,
        4
      ),
      
      CI_high = round(
        CI_high,
        4
      ),
      
      p_value = signif(
        p_value,
        5
      ),
      
      p_BH = signif(
        p_BH,
        5
      )
    )
}


resultado_primary <- arredondar_resultados(
  resultado_primary
)

resultado_exploratory <- arredondar_resultados(
  resultado_exploratory
)

resultado_n_lt_5 <- arredondar_resultados(
  resultado_n_lt_5
)

primary_fdr <- arredondar_resultados(
  primary_fdr
)

exploratory_fdr <- arredondar_resultados(
  exploratory_fdr
)

cross_primary <- arredondar_resultados(
  cross_primary
)

cross_exploratory <- arredondar_resultados(
  cross_exploratory
)

cross_n_lt_5 <- arredondar_resultados(
  cross_n_lt_5
)

cross_primary_fdr <- arredondar_resultados(
  cross_primary_fdr
)

cross_exploratory_fdr <- arredondar_resultados(
  cross_exploratory_fdr
)


# ============================================================
# 35. EXPORT EXCEL – COMPLETE FINAL WORKBOOK
# ============================================================

arquivo_excel <- file.path(
  pasta_resultados,
  "Resultados_Correlacoes_FINAL.xlsx"
)


wb <- openxlsx::createWorkbook()


# ------------------------------------------------------------
# Primary
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Primary_n8"
)

openxlsx::writeData(
  wb,
  "Primary_n8",
  resultado_primary
)


# ------------------------------------------------------------
# Primary significant
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Primary_FDR_significant"
)

openxlsx::writeData(
  wb,
  "Primary_FDR_significant",
  primary_fdr
)


# ------------------------------------------------------------
# Exploratory
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Exploratory_n5_7"
)

openxlsx::writeData(
  wb,
  "Exploratory_n5_7",
  resultado_exploratory
)


# ------------------------------------------------------------
# Exploratory significant
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Exploratory_FDR_sig"
)

openxlsx::writeData(
  wb,
  "Exploratory_FDR_sig",
  exploratory_fdr
)


# ------------------------------------------------------------
# n < 5
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Not_tested_n_lt_5"
)

openxlsx::writeData(
  wb,
  "Not_tested_n_lt_5",
  resultado_n_lt_5
)


# ------------------------------------------------------------
# Cross primary
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Cross_primary_n8"
)

openxlsx::writeData(
  wb,
  "Cross_primary_n8",
  cross_primary
)


# ------------------------------------------------------------
# Cross primary FDR
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Cross_primary_FDR"
)

openxlsx::writeData(
  wb,
  "Cross_primary_FDR",
  cross_primary_fdr
)


# ------------------------------------------------------------
# Cross exploratory
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Cross_exploratory_n5_7"
)

openxlsx::writeData(
  wb,
  "Cross_exploratory_n5_7",
  cross_exploratory
)


# ------------------------------------------------------------
# Cross exploratory FDR
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Cross_exploratory_FDR"
)

openxlsx::writeData(
  wb,
  "Cross_exploratory_FDR",
  cross_exploratory_fdr
)


# ------------------------------------------------------------
# Cross n < 5
# ------------------------------------------------------------

openxlsx::addWorksheet(
  wb,
  "Cross_not_tested_n_lt_5"
)

openxlsx::writeData(
  wb,
  "Cross_not_tested_n_lt_5",
  cross_n_lt_5
)


# ============================================================
# 36. EXCEL FORMATTING
# ============================================================

estilo_header <- openxlsx::createStyle(
  
  fontName = "Arial",
  
  fontSize = 11,
  
  textDecoration = "bold",
  
  halign = "center",
  
  valign = "center",
  
  border = "Bottom"
)


estilo_dados <- openxlsx::createStyle(
  
  fontName = "Arial",
  
  fontSize = 10,
  
  valign = "center"
)


for (
  aba in names(
    wb
  )
) {
  
  
  openxlsx::addStyle(
    
    wb,
    
    sheet = aba,
    
    style = estilo_header,
    
    rows = 1,
    
    cols = 1:ncol(
      openxlsx::readWorkbook(
        wb,
        sheet = aba
      )
    ),
    
    gridExpand = TRUE
  )
  
  
  openxlsx::setColWidths(
    
    wb,
    
    sheet = aba,
    
    cols = 1:ncol(
      openxlsx::readWorkbook(
        wb,
        sheet = aba
      )
    ),
    
    widths = "auto"
  )
  
  
  openxlsx::freezePane(
    
    wb,
    
    sheet = aba,
    
    firstRow = TRUE
  )
}


openxlsx::saveWorkbook(
  
  wb,
  
  arquivo_excel,
  
  overwrite = TRUE
)


# ============================================================
# 37. FIGURE FUNCTIONS
# ============================================================

escala_correlacao <- function() {
  
  ggplot2::scale_fill_gradient2(
    
    low = "#2761F5",
    
    mid = "white",
    
    high = "#F52727",
    
    midpoint = 0,
    
    limits = c(
      -1,
      1
    ),
    
    breaks = c(
      -1,
      -0.5,
      0,
      0.5,
      1
    ),
    
    name = expression(rho)
  )
}


# ============================================================
# 38. DECIDUAL miRNA HEATMAP
# ============================================================

criar_heatmap_mirna <- function(
    grupo
) {
  
  
  dados_heat <- resultados %>%
    
    dplyr::filter(
      
      Compartment == "Decidua",
      
      Group == grupo,
      
      Family == "miRNA x miRNA",
      
      Status == "Primary n>=8"
    ) %>%
    
    dplyr::mutate(
      
      Variable_1 = nome_variavel(
        Variable_1
      ),
      
      Variable_2 = nome_variavel(
        Variable_2
      ),
      
      estrela = dplyr::if_else(
        
        !is.na(
          p_BH
        ) &
          p_BH < 0.05,
        
        "*",
        
        ""
      )
    )
  
  
  dados_inv <- dados_heat %>%
    
    dplyr::transmute(
      
      Variable_1 = Variable_2,
      
      Variable_2 = Variable_1,
      
      rho = rho,
      
      p_BH = p_BH,
      
      estrela = estrela
    )
  
  
  dados_heat <- dplyr::bind_rows(
    
    dados_heat %>%
      dplyr::select(
        Variable_1,
        Variable_2,
        rho,
        p_BH,
        estrela
      ),
    
    dados_inv
  )
  
  
  ordem <- c(
    
    "miR-29a-3p",
    
    "miR-132-3p",
    
    "miR-150-5p",
    
    "miR-155-5p",
    
    "miR-222-3p"
  )
  
  
  diagonal <- tibble::tibble(
    
    Variable_1 = ordem,
    
    Variable_2 = ordem,
    
    rho = 1,
    
    p_BH = NA_real_,
    
    estrela = ""
  )
  
  
  dados_heat <- dplyr::bind_rows(
    dados_heat,
    diagonal
  ) %>%
    
    dplyr::mutate(
      
      Variable_1 = factor(
        Variable_1,
        levels = ordem
      ),
      
      Variable_2 = factor(
        Variable_2,
        levels = rev(
          ordem
        )
      )
    )
  
  
  ggplot2::ggplot(
    
    dados_heat,
    
    ggplot2::aes(
      x = Variable_1,
      y = Variable_2,
      fill = rho
    )
  ) +
    
    ggplot2::geom_tile(
      color = "white",
      linewidth = 0.7
    ) +
    
    ggplot2::geom_text(
      
      ggplot2::aes(
        label = estrela
      ),
      
      family = "Arial",
      
      size = 7,
      
      fontface = "bold"
    ) +
    
    escala_correlacao() +
    
    ggplot2::coord_fixed() +
    
    ggplot2::labs(
      
      title = grupo,
      
      x = NULL,
      
      y = NULL
    ) +
    
    ggplot2::theme_classic(
      base_family = "Arial"
    ) +
    
    ggplot2::theme(
      
      plot.title = ggplot2::element_text(
        
        family = "Arial",
        
        face = "bold",
        
        size = 18,
        
        hjust = 0.5,
        
        color = "black"
      ),
      
      axis.text.x = ggplot2::element_text(
        
        family = "Arial",
        
        size = 13,
        
        angle = 45,
        
        hjust = 1,
        
        color = "black"
      ),
      
      axis.text.y = ggplot2::element_text(
        
        family = "Arial",
        
        size = 13,
        
        color = "black"
      ),
      
      axis.line = ggplot2::element_blank(),
      
      axis.ticks = ggplot2::element_blank(),
      
      legend.title = ggplot2::element_text(
        family = "Arial",
        size = 14
      ),
      
      legend.text = ggplot2::element_text(
        family = "Arial",
        size = 12
      )
    )
}


# ============================================================
# 39. CYTOMETRY HEATMAP
# ============================================================

criar_heatmap_cito <- function(
    compartimento
) {
  
  
  dados_heat <- resultados %>%
    
    dplyr::filter(
      
      Compartment == compartimento,
      
      Family == "Cytometry x Cytometry",
      
      Status == "Primary n>=8"
    ) %>%
    
    dplyr::mutate(
      
      Variable_1 = nome_variavel(
        Variable_1
      ),
      
      Variable_2 = nome_variavel(
        Variable_2
      )
    )
  
  
  vars_relevantes <- dados_heat %>%
    
    dplyr::filter(
      
      !is.na(
        p_BH
      ),
      
      p_BH < 0.05
    ) %>%
    
    dplyr::select(
      Variable_1,
      Variable_2
    ) %>%
    
    unlist(
      use.names = FALSE
    ) %>%
    
    unique()
  
  
  dados_heat <- dados_heat %>%
    
    dplyr::filter(
      
      Variable_1 %in%
        vars_relevantes,
      
      Variable_2 %in%
        vars_relevantes
    ) %>%
    
    dplyr::mutate(
      
      estrela = dplyr::if_else(
        
        !is.na(
          p_BH
        ) &
          p_BH < 0.05,
        
        "*",
        
        ""
      )
    )
  
  
  dados_inv <- dados_heat %>%
    
    dplyr::transmute(
      
      Compartment,
      
      Group,
      
      Family,
      
      Variable_1 = Variable_2,
      
      Variable_2 = Variable_1,
      
      rho,
      
      p_BH,
      
      estrela
    )
  
  
  dados_heat <- dplyr::bind_rows(
    dados_heat,
    dados_inv
  )
  
  
  ordem_padrao <- c(
    
    "Lymphocytes",
    
    "CD3+",
    
    "CD4+",
    
    "CD4+CD28+",
    
    "CD4+CTLA-4+",
    
    "CD4+IFNγ+",
    
    "CD4+IL-2+",
    
    "CD4+IL-17+",
    
    "CD4+TGFβ+",
    
    "CD4−",
    
    "CD4−CD28+",
    
    "CD4−CTLA-4+",
    
    "CD4−IFNγ+",
    
    "CD4−IL-2+",
    
    "CD4−IL-17+",
    
    "CD4−TGFβ+"
  )
  
  
  ordem <- ordem_padrao[
    ordem_padrao %in%
      vars_relevantes
  ]
  
  
  dados_heat <- dados_heat %>%
    
    dplyr::mutate(
      
      Variable_1 = factor(
        Variable_1,
        levels = ordem
      ),
      
      Variable_2 = factor(
        Variable_2,
        levels = rev(
          ordem
        )
      )
    )
  
  
  ggplot2::ggplot(
    
    dados_heat,
    
    ggplot2::aes(
      x = Variable_1,
      y = Variable_2,
      fill = rho
    )
  ) +
    
    ggplot2::geom_tile(
      color = "white",
      linewidth = 0.5
    ) +
    
    ggplot2::geom_text(
      
      ggplot2::aes(
        label = estrela
      ),
      
      family = "Arial",
      
      size = 6,
      
      fontface = "bold"
    ) +
    
    escala_correlacao() +
    
    ggplot2::facet_wrap(
      ~ Group,
      nrow = 1
    ) +
    
    ggplot2::coord_fixed() +
    
    ggplot2::labs(
      
      title = nome_compartimento(
        compartimento
      ),
      
      x = NULL,
      
      y = NULL
    ) +
    
    ggplot2::theme_classic(
      base_family = "Arial"
    ) +
    
    ggplot2::theme(
      
      plot.title = ggplot2::element_text(
        
        family = "Arial",
        
        face = "bold",
        
        size = 18,
        
        hjust = 0.5,
        
        color = "black"
      ),
      
      strip.background = ggplot2::element_blank(),
      
      strip.text = ggplot2::element_text(
        
        family = "Arial",
        
        face = "bold",
        
        size = 15,
        
        color = "black"
      ),
      
      axis.text.x = ggplot2::element_text(
        
        family = "Arial",
        
        size = 11,
        
        angle = 45,
        
        hjust = 1,
        
        color = "black"
      ),
      
      axis.text.y = ggplot2::element_text(
        
        family = "Arial",
        
        size = 11,
        
        color = "black"
      ),
      
      axis.line = ggplot2::element_blank(),
      
      axis.ticks = ggplot2::element_blank(),
      
      legend.title = ggplot2::element_text(
        family = "Arial",
        size = 14
      ),
      
      legend.text = ggplot2::element_text(
        family = "Arial",
        size = 12
      )
    )
}


# ============================================================
# 40. CROSS-COMPARTMENT SCATTER
#
# Internal variable still uses original CD8+ column.
# Displayed name = CD4−CD28+
# ============================================================

marcador_cross <- "CD45+CD3+CD8+CD28+"


preparar_cross_plot <- function(
    comp1,
    comp2,
    grupo
) {
  
  
  df1 <- lista_dados[[
    comp1
  ]] %>%
    
    dplyr::filter(
      Group == grupo
    ) %>%
    
    dplyr::select(
      Code,
      dplyr::all_of(
        marcador_cross
      )
    )
  
  
  names(
    df1
  )[2] <- "X"
  
  
  df2 <- lista_dados[[
    comp2
  ]] %>%
    
    dplyr::filter(
      Group == grupo
    ) %>%
    
    dplyr::select(
      Code,
      dplyr::all_of(
        marcador_cross
      )
    )
  
  
  names(
    df2
  )[2] <- "Y"
  
  
  dplyr::inner_join(
    df1,
    df2,
    by = "Code"
  ) %>%
    
    dplyr::filter(
      !is.na(
        X
      ),
      !is.na(
        Y
      )
    )
}


criar_scatter_gdm <- function(
    comp1,
    comp2
) {
  
  
  df_plot <- preparar_cross_plot(
    
    comp1,
    
    comp2,
    
    "GDM"
  )
  
  
  stat <- resultados_cross %>%
    
    dplyr::filter(
      
      Compartment_1 == comp1,
      
      Compartment_2 == comp2,
      
      Group == "GDM",
      
      Variable == marcador_cross,
      
      Status == "Primary n>=8"
    )
  
  
  texto <- paste0(
    
    "n = ",
    stat$n,
    
    "\n\u03C1 = ",
    sprintf(
      "%.3f",
      stat$rho
    ),
    
    "\n95% CI ",
    sprintf(
      "%.3f",
      stat$CI_low
    ),
    "–",
    sprintf(
      "%.3f",
      stat$CI_high
    ),
    
    "\npBH = ",
    sprintf(
      "%.4f",
      stat$p_BH
    )
  )
  
  
  ggplot2::ggplot(
    
    df_plot,
    
    ggplot2::aes(
      x = X,
      y = Y
    )
  ) +
    
    ggplot2::geom_point(
      
      size = 4,
      
      shape = 21,
      
      fill = "#F52727",
      
      color = "black",
      
      stroke = 0.7
    ) +
    
    ggplot2::annotate(
      
      "text",
      
      x = -Inf,
      
      y = Inf,
      
      label = texto,
      
      hjust = -0.08,
      
      vjust = 1.10,
      
      family = "Arial",
      
      size = 4.8,
      
      color = "black"
    ) +
    
    ggplot2::labs(
      
      title = "GDM – CD4−CD28+",
      
      x = paste0(
        
        nome_compartimento(
          comp1
        ),
        
        "\nCD4−CD28+ frequency (%)"
      ),
      
      y = paste0(
        
        nome_compartimento(
          comp2
        ),
        
        "\nCD4−CD28+ frequency (%)"
      )
    ) +
    
    ggplot2::theme_classic(
      base_family = "Arial"
    ) +
    
    ggplot2::theme(
      
      plot.title = ggplot2::element_text(
        
        family = "Arial",
        
        face = "bold",
        
        size = 18,
        
        hjust = 0.5,
        
        color = "black"
      ),
      
      axis.title.x = ggplot2::element_text(
        
        family = "Arial",
        
        size = 15,
        
        color = "black"
      ),
      
      axis.title.y = ggplot2::element_text(
        
        family = "Arial",
        
        size = 15,
        
        color = "black"
      ),
      
      axis.text = ggplot2::element_text(
        
        family = "Arial",
        
        size = 13,
        
        color = "black"
      ),
      
      axis.line = ggplot2::element_line(
        
        color = "black",
        
        linewidth = 0.7
      )
    )
}


# ============================================================
# 41. CREATE FIGURES
# ============================================================

fig_A <- criar_heatmap_mirna(
  "CTR"
)

fig_B <- criar_heatmap_mirna(
  "GDM"
)

fig_C <- criar_heatmap_cito(
  "Cord"
)

fig_D <- criar_heatmap_cito(
  "Decidua"
)

fig_E <- criar_scatter_gdm(
  "Mother",
  "Decidua"
)

fig_F <- criar_scatter_gdm(
  "Cord",
  "Decidua"
)


# ============================================================
# 42. miRNA BLOCK
# ============================================================

bloco_mirna <- (
  
  fig_A |
    fig_B
  
) +
  
  patchwork::plot_annotation(
    
    title = "Decidual tissue – miRNA × miRNA"
    
  ) &
  
  ggplot2::theme(
    
    plot.title = ggplot2::element_text(
      
      family = "Arial",
      
      face = "bold",
      
      size = 18,
      
      hjust = 0.5,
      
      color = "black"
    )
  )


# ============================================================
# 43. FINAL SUPPLEMENTARY PANEL
# ============================================================

painel_correlacoes <- (
  
  bloco_mirna
  
) /
  
  (
    fig_C |
      fig_D
  ) /
  
  (
    fig_E |
      fig_F
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
    ),
    
    legend.position = "right"
  )


painel_correlacoes


# ============================================================
# 44. SAVE PNG
# ============================================================

ggplot2::ggsave(
  
  filename = file.path(
    pasta_png,
    "Supplementary_Correlation_Figure.png"
  ),
  
  plot = painel_correlacoes,
  
  width = 16,
  
  height = 20,
  
  units = "in",
  
  dpi = 600,
  
  bg = "white"
)


# ============================================================
# 45. SAVE TIFF
# ============================================================

ggplot2::ggsave(
  
  filename = file.path(
    pasta_tiff,
    "Supplementary_Correlation_Figure.tiff"
  ),
  
  plot = painel_correlacoes,
  
  width = 16,
  
  height = 20,
  
  units = "in",
  
  dpi = 600,
  
  compression = "lzw",
  
  bg = "white"
)


# ============================================================
# 46. SAVE EDITABLE SVG
# ============================================================

svglite::svglite(
  
  filename = file.path(
    pasta_svg,
    "Supplementary_Correlation_Figure.svg"
  ),
  
  width = 16,
  
  height = 20
)

print(
  painel_correlacoes
)

grDevices::dev.off()


# ============================================================
# 47. FINAL SUMMARY IN CONSOLE
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "CORRELATION ANALYSIS COMPLETED\n"
)

cat(
  "============================================================\n\n"
)


cat(
  "Primary n>=8 tests: ",
  nrow(
    resultado_primary
  ),
  "\n"
)

cat(
  "Primary FDR-significant: ",
  nrow(
    primary_fdr
  ),
  "\n\n"
)


cat(
  "Exploratory n=5-7 tests: ",
  nrow(
    resultado_exploratory
  ),
  "\n"
)

cat(
  "Exploratory FDR-significant: ",
  nrow(
    exploratory_fdr
  ),
  "\n\n"
)


cat(
  "Cross-compartment primary tests: ",
  nrow(
    cross_primary
  ),
  "\n"
)

cat(
  "Cross-compartment primary FDR-significant: ",
  nrow(
    cross_primary_fdr
  ),
  "\n\n"
)


cat(
  "Excel file:\n",
  arquivo_excel,
  "\n\n"
)


cat(
  "Figures:\n",
  pasta_figuras,
  "\n"
)


# ============================================================
# END
# ============================================================

# ============================================================
# 48. POOLED CORRELATION MATRICES — CTR + GDM TOGETHER
#
# Spearman correlations are calculated using all participants,
# but separately within each biological compartment.
# BH correction is applied within each compartment.
# ============================================================

analisar_par_pooled <- function(df, var1, var2, compartimento) {
  
  res <- cor_spearman_boot(
    df[[var1]],
    df[[var2]]
  )
  
  res %>%
    dplyr::mutate(
      Group = "ALL",
      Compartment = compartimento,
      Variable_1 = var1,
      Variable_2 = var2,
      .before = 1
    )
}


resultados_pooled <- tibble::tibble()

for (compartimento in names(lista_dados)) {
  
  df <- lista_dados[[compartimento]]
  
  vars_disponiveis <- unique(c(
    intersect(variaveis_mirna, names(df)),
    intersect(variaveis_citometria, names(df))
  ))
  
  pares <- criar_pares_unicos(vars_disponiveis)
  
  if (length(pares) > 0) {
    
    resultados_comp <- purrr::map_dfr(
      pares,
      function(par) {
        analisar_par_pooled(
          df = df,
          var1 = par[1],
          var2 = par[2],
          compartimento = compartimento
        )
      }
    )
    
    resultados_pooled <- dplyr::bind_rows(
      resultados_pooled,
      resultados_comp
    )
  }
}


resultados_pooled <- resultados_pooled %>%
  dplyr::mutate(
    Status = dplyr::if_else(
      n >= 8,
      "Included n>=8",
      "Excluded n<8"
    )
  ) %>%
  dplyr::group_by(
    Compartment
  ) %>%
  dplyr::mutate(
    p_BH = {
      p_adj <- rep(NA_real_, dplyr::n())
      idx <- n >= 8 & !is.na(p_value)
      p_adj[idx] <- stats::p.adjust(
        p_value[idx],
        method = "BH"
      )
      p_adj
    }
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    FDR_significant = dplyr::if_else(
      !is.na(p_BH) & p_BH < 0.05,
      "Yes",
      "No"
    )
  )


resultados_pooled_export <- resultados_pooled %>%
  dplyr::filter(
    n >= 8
  ) %>%
  dplyr::mutate(
    Compartment = nome_compartimento(Compartment),
    Variable_1 = nome_variavel(Variable_1),
    Variable_2 = nome_variavel(Variable_2),
    rho = round(rho, 4),
    CI_low = round(CI_low, 4),
    CI_high = round(CI_high, 4),
    p_value = signif(p_value, 5),
    p_BH = signif(p_BH, 5)
  ) %>%
  dplyr::arrange(
    Compartment,
    Status,
    p_BH,
    p_value
  )


# ============================================================
# 49. POOLED CORRELATION HEATMAP
# ============================================================

criar_heatmap_pooled <- function(compartimento) {
  
  dados_heat <- resultados_pooled %>%
    dplyr::filter(
      Compartment == compartimento,
      Status == "Included n>=8"
    ) %>%
    dplyr::mutate(
      Variable_1 = nome_variavel(Variable_1),
      Variable_2 = nome_variavel(Variable_2),
      estrela = dplyr::case_when(
        !is.na(p_BH) & p_BH < 0.001 ~ "***",
        !is.na(p_BH) & p_BH < 0.01  ~ "**",
        !is.na(p_BH) & p_BH < 0.05  ~ "*",
        TRUE ~ ""
      )
    )
  
  if (nrow(dados_heat) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  vars <- unique(c(dados_heat$Variable_1, dados_heat$Variable_2))
  
  ordem_padrao <- c(
    "miR-29a-3p", "miR-132-3p", "miR-150-5p",
    "miR-155-5p", "miR-222-3p",
    "Lymphocytes", "CD3+", "CD4+", "CD4+CD28+",
    "CD4+CTLA-4+", "CD4+IFNγ+", "CD4+IL-2+",
    "CD4+IL-17+", "CD4+TGFβ+", "CD4−",
    "CD4−CD28+", "CD4−CTLA-4+", "CD4−IFNγ+",
    "CD4−IL-2+", "CD4−IL-17+", "CD4−TGFβ+"
  )
  
  ordem <- c(
    ordem_padrao[ordem_padrao %in% vars],
    setdiff(vars, ordem_padrao)
  )
  
  dados_inv <- dados_heat %>%
    dplyr::transmute(
      Variable_1 = Variable_2,
      Variable_2 = Variable_1,
      rho = rho,
      p_BH = p_BH,
      estrela = estrela
    )
  
  diagonal <- tibble::tibble(
    Variable_1 = ordem,
    Variable_2 = ordem,
    rho = 1,
    p_BH = NA_real_,
    estrela = ""
  )
  
  dados_plot <- dplyr::bind_rows(
    dados_heat %>% dplyr::select(Variable_1, Variable_2, rho, p_BH, estrela),
    dados_inv,
    diagonal
  ) %>%
    dplyr::mutate(
      Variable_1 = factor(Variable_1, levels = ordem),
      Variable_2 = factor(Variable_2, levels = rev(ordem))
    )
  
  ggplot2::ggplot(
    dados_plot,
    ggplot2::aes(
      x = Variable_1,
      y = Variable_2,
      fill = rho
    )
  ) +
    ggplot2::geom_tile(
      color = "white",
      linewidth = 0.35
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = estrela),
      family = "Arial",
      size = 4.2,
      fontface = "bold"
    ) +
    escala_correlacao() +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = paste0(
        nome_compartimento(compartimento),
        " — CTR + GDM"
      ),
      subtitle = "Spearman correlation; * BH-adjusted p < 0.05",
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_classic(base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = "Arial",
        face = "bold",
        size = 16,
        hjust = 0.5,
        color = "black"
      ),
      plot.subtitle = ggplot2::element_text(
        family = "Arial",
        size = 11,
        hjust = 0.5,
        color = "black"
      ),
      axis.text.x = ggplot2::element_text(
        family = "Arial",
        size = 8.5,
        angle = 45,
        hjust = 1,
        color = "black"
      ),
      axis.text.y = ggplot2::element_text(
        family = "Arial",
        size = 8.5,
        color = "black"
      ),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(
        family = "Arial",
        size = 12
      ),
      legend.text = ggplot2::element_text(
        family = "Arial",
        size = 10
      )
    )
}


fig_matrix_mother <- criar_heatmap_pooled("Mother")
fig_matrix_cord <- criar_heatmap_pooled("Cord")
fig_matrix_decidua <- criar_heatmap_pooled("Decidua")

painel_matriz_pooled <-
  fig_matrix_mother /
  fig_matrix_cord /
  fig_matrix_decidua +
  patchwork::plot_annotation(
    title = "Overall correlation matrices — all participants"
  ) &
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = "Arial",
      face = "bold",
      size = 18,
      hjust = 0.5,
      color = "black"
    )
  )


# ============================================================
# 50. FORMAL GROUP COMPARISON BY LINEAR REGRESSION
#
# Question: does the relationship between compartments differ
# between CTR and GDM?
# Model: Y ~ X * Group
# The X:GroupGDM interaction tests whether slopes differ.
# CTR is the reference group.
# ============================================================

preparar_cross_regressao <- function(comp1, comp2) {
  
  df1 <- lista_dados[[comp1]] %>%
    dplyr::select(
      Code,
      Group,
      dplyr::all_of(marcador_cross)
    )
  
  names(df1)[3] <- "X"
  
  df2 <- lista_dados[[comp2]] %>%
    dplyr::select(
      Code,
      Group,
      dplyr::all_of(marcador_cross)
    )
  
  names(df2)[3] <- "Y"
  
  dplyr::inner_join(
    df1,
    df2,
    by = c("Code", "Group")
  ) %>%
    dplyr::filter(
      Group %in% c("CTR", "GDM"),
      !is.na(X),
      !is.na(Y)
    ) %>%
    dplyr::mutate(
      Group = factor(Group, levels = c("CTR", "GDM"))
    )
}


analisar_regressao_interacao <- function(comp1, comp2) {
  
  df <- preparar_cross_regressao(comp1, comp2)
  
  modelo <- stats::lm(
    Y ~ X * Group,
    data = df
  )
  
  coefs <- summary(modelo)$coefficients
  ci <- suppressMessages(stats::confint(modelo))
  
  termo_interacao <- grep(
    "^X:GroupGDM$|^GroupGDM:X$",
    rownames(coefs),
    value = TRUE
  )[1]
  
  beta_x <- unname(coefs["X", "Estimate"])
  beta_interacao <- unname(coefs[termo_interacao, "Estimate"])
  
  slope_ctr <- beta_x
  slope_gdm <- beta_x + beta_interacao
  
  tibble::tibble(
    Predictor_compartment = nome_compartimento(comp1),
    Outcome_compartment = nome_compartimento(comp2),
    Marker = "CD4−CD28+",
    n_total = nrow(df),
    n_CTR = sum(df$Group == "CTR"),
    n_GDM = sum(df$Group == "GDM"),
    slope_CTR = slope_ctr,
    slope_GDM = slope_gdm,
    beta_interaction = beta_interacao,
    CI_low_interaction = unname(ci[termo_interacao, 1]),
    CI_high_interaction = unname(ci[termo_interacao, 2]),
    p_interaction = unname(coefs[termo_interacao, "Pr(>|t|)"]),
    R2 = summary(modelo)$r.squared,
    adjusted_R2 = summary(modelo)$adj.r.squared
  )
}


regressao_mother_decidua <- analisar_regressao_interacao(
  "Mother",
  "Decidua"
)

regressao_cord_decidua <- analisar_regressao_interacao(
  "Cord",
  "Decidua"
)

resultados_regressao <- dplyr::bind_rows(
  regressao_mother_decidua,
  regressao_cord_decidua
) %>%
  dplyr::mutate(
    dplyr::across(
      c(
        slope_CTR,
        slope_GDM,
        beta_interaction,
        CI_low_interaction,
        CI_high_interaction,
        R2,
        adjusted_R2
      ),
      ~ round(.x, 4)
    ),
    p_interaction = signif(p_interaction, 5)
  )


# ============================================================
# 51. REGRESSION FIGURES — CTR AND GDM IN THE SAME PLOT
# ============================================================

criar_scatter_regressao <- function(comp1, comp2) {
  
  df <- preparar_cross_regressao(comp1, comp2)
  res <- analisar_regressao_interacao(comp1, comp2)
  
  p_int <- res$p_interaction[1]
  
  p_texto <- ifelse(
    is.na(p_int),
    "NA",
    ifelse(
      p_int < 0.0001,
      "< 0.0001",
      sprintf("%.4f", p_int)
    )
  )
  
  texto <- paste0(
    "Interaction p = ",
    p_texto,
    "\nβinteraction = ",
    sprintf("%.3f", res$beta_interaction[1]),
    "\n95% CI ",
    sprintf("%.3f", res$CI_low_interaction[1]),
    " to ",
    sprintf("%.3f", res$CI_high_interaction[1])
  )
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = X,
      y = Y,
      color = Group,
      fill = Group
    )
  ) +
    ggplot2::geom_point(
      size = 3.8,
      shape = 21,
      color = "black",
      stroke = 0.65,
      ggplot2::aes(fill = Group)
    ) +
    ggplot2::geom_smooth(
      ggplot2::aes(color = Group, fill = Group),
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      linewidth = 1,
      alpha = 0.15
    ) +
    ggplot2::scale_color_manual(
      values = c(
        CTR = "#2761F5",
        GDM = "#F52727"
      )
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        CTR = "#2761F5",
        GDM = "#F52727"
      )
    ) +
    ggplot2::annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = texto,
      hjust = -0.05,
      vjust = 1.10,
      family = "Arial",
      size = 4.5,
      color = "black"
    ) +
    ggplot2::labs(
      title = "CD4−CD28+",
      x = paste0(
        nome_compartimento(comp1),
        "\nCD4−CD28+ frequency (%)"
      ),
      y = paste0(
        nome_compartimento(comp2),
        "\nCD4−CD28+ frequency (%)"
      ),
      color = NULL,
      fill = NULL
    ) +
    ggplot2::theme_classic(base_family = "Arial") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        family = "Arial",
        face = "bold",
        size = 18,
        hjust = 0.5,
        color = "black"
      ),
      axis.title = ggplot2::element_text(
        family = "Arial",
        size = 14,
        color = "black"
      ),
      axis.text = ggplot2::element_text(
        family = "Arial",
        size = 12,
        color = "black"
      ),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(
        family = "Arial",
        size = 12,
        color = "black"
      ),
      axis.line = ggplot2::element_line(
        color = "black",
        linewidth = 0.7
      )
    )
}


fig_reg_mother_decidua <- criar_scatter_regressao(
  "Mother",
  "Decidua"
)

fig_reg_cord_decidua <- criar_scatter_regressao(
  "Cord",
  "Decidua"
)

painel_regressoes <-
  fig_reg_mother_decidua |
  fig_reg_cord_decidua


# ============================================================
# 52. EXPORT NEW RESULTS TO EXCEL
# ============================================================

arquivo_extra <- file.path(
  pasta_resultados,
  "Overall_Matrix_and_Regression_Results.xlsx"
)

wb_extra <- openxlsx::createWorkbook()

openxlsx::addWorksheet(
  wb_extra,
  "Pooled_correlations"
)

openxlsx::writeData(
  wb_extra,
  "Pooled_correlations",
  resultados_pooled_export
)

openxlsx::addWorksheet(
  wb_extra,
  "Regression_interactions"
)

openxlsx::writeData(
  wb_extra,
  "Regression_interactions",
  resultados_regressao
)

openxlsx::addWorksheet(
  wb_extra,
  "README"
)

readme_extra <- data.frame(
  Item = c(
    "Pooled correlations",
    "Correlation method",
    "Multiple-testing correction",
    "Regression model",
    "Interaction term",
    "Interpretation"
  ),
  Description = c(
    "CTR and GDM participants analyzed together, separately within each compartment.",
    "Spearman rank correlation with bootstrap 95% confidence interval.",
    "Benjamini-Hochberg correction within each compartment.",
    "Linear regression: Y ~ X * Group, with CTR as reference.",
    "X:GroupGDM tests whether the slope differs between GDM and CTR.",
    "A significant interaction indicates evidence that the X-Y relationship differs between groups."
  )
)

openxlsx::writeData(
  wb_extra,
  "README",
  readme_extra
)

for (aba in names(wb_extra)) {
  openxlsx::freezePane(
    wb_extra,
    sheet = aba,
    firstRow = TRUE
  )
  openxlsx::setColWidths(
    wb_extra,
    sheet = aba,
    cols = 1:ncol(openxlsx::readWorkbook(wb_extra, sheet = aba)),
    widths = "auto"
  )
}

openxlsx::saveWorkbook(
  wb_extra,
  arquivo_extra,
  overwrite = TRUE
)


# ============================================================
# 53. SAVE POOLED CORRELATION MATRIX FIGURE
# ============================================================

ggplot2::ggsave(
  filename = file.path(
    pasta_png,
    "Overall_Correlation_Matrices_ALL_participants.png"
  ),
  plot = painel_matriz_pooled,
  width = 16,
  height = 28,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_tiff,
    "Overall_Correlation_Matrices_ALL_participants.tiff"
  ),
  plot = painel_matriz_pooled,
  width = 16,
  height = 28,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

svglite::svglite(
  filename = file.path(
    pasta_svg,
    "Overall_Correlation_Matrices_ALL_participants.svg"
  ),
  width = 16,
  height = 28
)
print(painel_matriz_pooled)
grDevices::dev.off()


# ============================================================
# 54. SAVE REGRESSION FIGURE
# ============================================================

ggplot2::ggsave(
  filename = file.path(
    pasta_png,
    "CD4negCD28_Regression_Group_Interaction.png"
  ),
  plot = painel_regressoes,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = file.path(
    pasta_tiff,
    "CD4negCD28_Regression_Group_Interaction.tiff"
  ),
  plot = painel_regressoes,
  width = 14,
  height = 6.5,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)

svglite::svglite(
  filename = file.path(
    pasta_svg,
    "CD4negCD28_Regression_Group_Interaction.svg"
  ),
  width = 14,
  height = 6.5
)
print(painel_regressoes)
grDevices::dev.off()


# ============================================================
# 55. FINAL EXTRA SUMMARY
# ============================================================

cat("\n============================================================\n")
cat("POOLED MATRICES + REGRESSION ANALYSES COMPLETED\n")
cat("============================================================\n\n")

cat("Regression interaction results:\n")
print(resultados_regressao)

cat("\nAdditional Excel file:\n", arquivo_extra, "\n")
cat("\nAdditional figures saved in:\n", pasta_figuras, "\n")


# ============================================================
# END — FINAL VERSION WITH POOLED MATRIX + REGRESSION
# ============================================================


# ============================================================
# 56. FINAL MANUSCRIPT OUTPUT
#
# Reviewer-requested version:
# - only correlations with n >= 8 are exported/presented
# - exploratory correlations with n < 8 are not included
# - Maternal blood / Umbilical cord blood terminology
# - regression plots replace the previous GDM-only correlation
#   scatterplots in the main supplementary figure
# - one final Excel workbook in English
# ============================================================


# ------------------------------------------------------------
# 56.1 Final tables: n >= 8 only
# ------------------------------------------------------------

within_correlations_n8 <- resultado_primary %>%
  dplyr::arrange(
    Compartment,
    Group,
    Family,
    p_BH,
    p_value
  )

within_significant_n8 <- within_correlations_n8 %>%
  dplyr::filter(
    !is.na(p_BH),
    p_BH < 0.05
  )

cross_correlations_n8 <- cross_primary %>%
  dplyr::arrange(
    Group,
    Family,
    p_BH,
    p_value
  )

cross_significant_n8 <- cross_correlations_n8 %>%
  dplyr::filter(
    !is.na(p_BH),
    p_BH < 0.05
  )

pooled_correlations_n8 <- resultados_pooled_export %>%
  dplyr::filter(
    n >= 8
  ) %>%
  dplyr::arrange(
    Compartment,
    p_BH,
    p_value
  )

pooled_significant_n8 <- pooled_correlations_n8 %>%
  dplyr::filter(
    !is.na(p_BH),
    p_BH < 0.05
  )


# ------------------------------------------------------------
# 56.2 One final English Excel workbook
# ------------------------------------------------------------

final_excel_file <- file.path(
  pasta_resultados,
  "Final_Correlation_Analysis_n8_English.xlsx"
)

wb_final <- openxlsx::createWorkbook()


openxlsx::addWorksheet(
  wb_final,
  "Within_correlations_n8"
)

openxlsx::writeData(
  wb_final,
  "Within_correlations_n8",
  within_correlations_n8
)


openxlsx::addWorksheet(
  wb_final,
  "Within_FDR_significant"
)

openxlsx::writeData(
  wb_final,
  "Within_FDR_significant",
  within_significant_n8
)


openxlsx::addWorksheet(
  wb_final,
  "Cross_correlations_n8"
)

openxlsx::writeData(
  wb_final,
  "Cross_correlations_n8",
  cross_correlations_n8
)


openxlsx::addWorksheet(
  wb_final,
  "Cross_FDR_significant"
)

openxlsx::writeData(
  wb_final,
  "Cross_FDR_significant",
  cross_significant_n8
)


openxlsx::addWorksheet(
  wb_final,
  "Pooled_correlations_n8"
)

openxlsx::writeData(
  wb_final,
  "Pooled_correlations_n8",
  pooled_correlations_n8
)


openxlsx::addWorksheet(
  wb_final,
  "Pooled_FDR_significant"
)

openxlsx::writeData(
  wb_final,
  "Pooled_FDR_significant",
  pooled_significant_n8
)


openxlsx::addWorksheet(
  wb_final,
  "Regression_interactions"
)

openxlsx::writeData(
  wb_final,
  "Regression_interactions",
  resultados_regressao
)


openxlsx::addWorksheet(
  wb_final,
  "README"
)

readme_final <- data.frame(
  Section = c(
    "Inclusion criterion",
    "Within-compartment correlations",
    "Cross-compartment correlations",
    "Pooled correlations",
    "Correlation method",
    "Confidence intervals",
    "Multiple-testing correction",
    "Regression model",
    "Regression interpretation",
    "Terminology"
  ),
  Description = c(
    "Only correlations with at least 8 paired observations (n >= 8) are included in this final workbook. Exploratory analyses with n < 8 are excluded from manuscript tables and figures.",
    "Spearman correlations analyzed separately by group (CTR and GDM) within each compartment.",
    "Spearman correlations between matched compartments, analyzed separately by group.",
    "CTR and GDM participants analyzed together within each compartment; only pairs with n >= 8 are retained.",
    "Spearman rank correlation.",
    "Bootstrap 95% confidence intervals for Spearman rho.",
    "Benjamini-Hochberg false discovery rate correction. For pooled analyses, BH correction is applied only among correlations meeting n >= 8 within each compartment.",
    "Linear regression: Y ~ X * Group, with CTR as the reference group.",
    "The X:GroupGDM interaction tests whether the relationship (slope) differs between GDM and CTR.",
    "Mother is displayed as Maternal blood; Cord is displayed as Umbilical cord blood; Decidua is displayed as Decidual tissue."
  ),
  stringsAsFactors = FALSE
)

openxlsx::writeData(
  wb_final,
  "README",
  readme_final
)


# English workbook formatting
final_header_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 11,
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom"
)

for (sheet_name in names(wb_final)) {

  sheet_data <- openxlsx::readWorkbook(
    wb_final,
    sheet = sheet_name
  )

  if (ncol(sheet_data) > 0) {

    openxlsx::addStyle(
      wb_final,
      sheet = sheet_name,
      style = final_header_style,
      rows = 1,
      cols = seq_len(ncol(sheet_data)),
      gridExpand = TRUE
    )

    openxlsx::setColWidths(
      wb_final,
      sheet = sheet_name,
      cols = seq_len(ncol(sheet_data)),
      widths = "auto"
    )
  }

  openxlsx::freezePane(
    wb_final,
    sheet = sheet_name,
    firstRow = TRUE
  )
}


openxlsx::saveWorkbook(
  wb_final,
  final_excel_file,
  overwrite = TRUE
)


# ------------------------------------------------------------
# 56.3 FINAL MAIN SUPPLEMENTARY FIGURE
#
# Panels E/F are now the formal regression comparison
# between CTR and GDM instead of GDM-only scatterplots.
# ------------------------------------------------------------

final_supplementary_panel <- (

  bloco_mirna

) /

  (
    fig_C |
      fig_D
  ) /

  (
    fig_reg_mother_decidua |
      fig_reg_cord_decidua
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
    ),

    legend.position = "right"
  )


final_supplementary_panel


# ------------------------------------------------------------
# 56.4 Save final main figure
# ------------------------------------------------------------

ggplot2::ggsave(
  filename = file.path(
    pasta_png,
    "Supplementary_Correlation_Figure_FINAL_n8.png"
  ),
  plot = final_supplementary_panel,
  width = 16,
  height = 20,
  units = "in",
  dpi = 600,
  bg = "white"
)


ggplot2::ggsave(
  filename = file.path(
    pasta_tiff,
    "Supplementary_Correlation_Figure_FINAL_n8.tiff"
  ),
  plot = final_supplementary_panel,
  width = 16,
  height = 20,
  units = "in",
  dpi = 600,
  compression = "lzw",
  bg = "white"
)


svglite::svglite(
  filename = file.path(
    pasta_svg,
    "Supplementary_Correlation_Figure_FINAL_n8.svg"
  ),
  width = 16,
  height = 20
)

print(
  final_supplementary_panel
)

grDevices::dev.off()


# ------------------------------------------------------------
# 56.5 Final console summary
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("FINAL MANUSCRIPT CORRELATION ANALYSIS COMPLETED\n")
cat("============================================================\n\n")

cat("Only correlations with n >= 8 were retained in the final workbook.\n")
cat("Exploratory correlations with n < 8 were excluded.\n\n")

cat(
  "Final English Excel workbook:\n",
  final_excel_file,
  "\n\n"
)

cat(
  "Final supplementary figure:\n",
  file.path(
    pasta_png,
    "Supplementary_Correlation_Figure_FINAL_n8.png"
  ),
  "\n\n"
)

cat("Regression interaction results:\n")
print(resultados_regressao)


# ============================================================
# END — MANUSCRIPT VERSION n >= 8
# ============================================================
