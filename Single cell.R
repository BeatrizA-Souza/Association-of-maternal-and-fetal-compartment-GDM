# ============================================================
 ## scRNA-seq REANALYSIS
# Gestational Diabetes Mellitus vs Control

# Final workflow:
# 01. Packages
# 02. Helper functions
# 03. Import + QC + doublet removal
# 04. QC figures
# 05. Merge + CCA integration
# 06. Resolution sensitivity
# 07. Final clustering + UMAP
# 08. Broad cell-type annotation
# 09. Cell composition
# 10. Global donor-level pseudobulk
# 11. Cell-type donor-level pseudobulk
# 12. multiMiR targets
# 13. Permutation analysis
# 14. miRNA enrichment figures
# 15. Manuscript figures
# 16. Final Excel workbook
# 17. Save objects
# 18. Session information
# 19. Final sanity checks
#


# ============================================================
# FINAL CONSOLIDATED VERSION — 2026-10-02
#
# Final choices incorporated below:
# - donor-level pseudobulk inference
# - DEGs defined by DESeq2 FDR < 0.05 for final target-DEG figures
# - experimentally validated miRNA targets only in the main workflow
# - cell-type GO BP for FDR-significant DEGs
# - permutation analysis for Global + all testable cell types
# - miRNA GO BP with miRNA-specific validated-target background
# - final manuscript Figure 2
# - final Supplementary Figure
# - robust final Excel workbook with post-write verification
# ============================================================

rm(list = ls())

gc()

options(
  stringsAsFactors = FALSE
)

options(
  contrasts = c(
    "contr.sum",
    "contr.poly"
  )
)

set.seed(4301)



# ============================================================
# 00. CONFIGURATION
# EDIT ONLY THIS SECTION IF NECESSARY
# ============================================================

base_dir <- paste0(
  "C:/Users/beatr/Dropbox/Beatriz/",
  "Resultados miRs/Analise micros/",
  "figuras singel cell"
)


sample_dirs <- c(
  
  C1 = file.path(
    base_dir,
    "Amostras/GSM5261695_C1"
  ),
  
  C2 = file.path(
    base_dir,
    "Amostras/GSM5261696_C2"
  ),
  
  G1 = file.path(
    base_dir,
    "Amostras/GSM5261697_G1"
  ),
  
  G2 = file.path(
    base_dir,
    "Amostras/GSM5261698_G2"
  )
  
)


sample_group <- c(
  
  C1 = "Control",
  
  C2 = "Control",
  
  G1 = "GDM",
  
  G2 = "GDM"
  
)


out_dir <- file.path(
  base_dir,
  "Resultados_singlecell_FINAL"
)


output_dirs <- list(
  
  qc = file.path(
    out_dir,
    "01_QC"
  ),
  
  umap = file.path(
    out_dir,
    "02_UMAP"
  ),
  
  annotation = file.path(
    out_dir,
    "03_Anotacao"
  ),
  
  composition = file.path(
    out_dir,
    "04_Composicao"
  ),
  
  pseudobulk_global = file.path(
    out_dir,
    "05_Pseudobulk_Global"
  ),
  
  pseudobulk_celltypes = file.path(
    out_dir,
    "06_Pseudobulk_CellTypes"
  ),
  
  tnk = file.path(
    out_dir,
    "07_TNK"
  ),
  
  mirna = file.path(
    out_dir,
    "08_miRNA_Permutacao"
  ),
  
  figures = file.path(
    out_dir,
    "09_Figuras_Manuscrito"
  ),
  
  excel = file.path(
    out_dir,
    "10_Tabelas_Excel"
  ),
  
  rds = file.path(
    out_dir,
    "11_Objetos_RDS"
  ),
  
  session = file.path(
    out_dir,
    "12_SessionInfo"
  )
  
)


dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


invisible(
  
  lapply(
    
    output_dirs,
    
    dir.create,
    
    recursive = TRUE,
    
    showWarnings = FALSE
    
  )
  
)


# ------------------------------------------------------------
# QC settings
# ------------------------------------------------------------

min_features <- 200

max_percent_mt <- 10

mad_cutoff <- 5


# ------------------------------------------------------------
# Integration
# ------------------------------------------------------------

n_hvg <- 2000

n_pcs <- 20


# ------------------------------------------------------------
# Resolution
#
# The script will test several resolutions.
# resolution_main is the one used for the FINAL analysis.
#
# If after inspecting the figures you prefer 0.6,
# simply change this value and rerun.
# ------------------------------------------------------------

resolutions_test <- c(
  0.4,
  0.6,
  0.8,
  1.0
)

resolution_main <- 0.8


# ------------------------------------------------------------
# Pseudobulk
#
# Avoid testing a cell type represented by only a handful
# of cells in one donor.
# ------------------------------------------------------------

min_cells_per_donor_pb <- 10


# ------------------------------------------------------------
# Permutations
# ------------------------------------------------------------

n_perm <- 10000


# ------------------------------------------------------------
# Five prespecified miRNAs
# ------------------------------------------------------------

mirnas <- c(
  
  "hsa-miR-29a-3p",
  
  "hsa-miR-132-3p",
  
  "hsa-miR-150-5p",
  
  "hsa-miR-155-5p",
  
  "hsa-miR-222-3p"
  
)



# ============================================================
# 01. PACKAGES
# ============================================================

cran_pkgs <- c(
  
  "dplyr",
  
  "tidyr",
  
  "purrr",
  
  "tibble",
  
  "stringr",
  
  "ggplot2",
  
  "ggrepel",
  "svglite",
  
  "patchwork",
  
  "openxlsx",
  
  "scales"
  
)


bioc_pkgs <- c(
  
  "Seurat",
  
  "SeuratObject",
  
  "SingleCellExperiment",
  
  "SummarizedExperiment",
  
  "scDblFinder",
  
  "DESeq2",
  
  "Matrix",
  
  "multiMiR",
  
  "clusterProfiler",
  
  "org.Hs.eg.db"
  
)


missing_cran <- cran_pkgs[
  !vapply(
    cran_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(missing_cran) > 0) {
  
  install.packages(
    missing_cran
  )
  
}


if (!requireNamespace(
  "BiocManager",
  quietly = TRUE
)) {
  
  install.packages(
    "BiocManager"
  )
  
}


missing_bioc <- bioc_pkgs[
  !vapply(
    bioc_pkgs,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]


if (length(missing_bioc) > 0) {
  
  BiocManager::install(
    missing_bioc,
    ask = FALSE,
    update = FALSE
  )
  
}


suppressPackageStartupMessages({
  
  library(Seurat)
  
  library(SeuratObject)
  
  library(SingleCellExperiment)
  
  library(SummarizedExperiment)
  
  library(scDblFinder)
  
  library(DESeq2)
  
  library(Matrix)
  
  library(dplyr)
  
  library(tidyr)
  
  library(purrr)
  
  library(tibble)
  
  library(stringr)
  
  library(ggplot2)
  
  library(ggrepel)
  
  library(patchwork)
  
  library(openxlsx)
  
  library(scales)
  
  library(multiMiR)
  
  library(clusterProfiler)
  
  library(org.Hs.eg.db)
  
})



# ============================================================
# 02. HELPER FUNCTIONS
# ============================================================


# ------------------------------------------------------------
# Save plot as PNG, TIFF and SVG
# ------------------------------------------------------------

save_plot_all <- function(
    plot,
    filename,
    width,
    height,
    dpi = 600
) {
  
  ggplot2::ggsave(
    
    paste0(
      filename,
      ".png"
    ),
    
    plot = plot,
    
    width = width,
    
    height = height,
    
    units = "in",
    
    dpi = dpi,
    
    bg = "white"
    
  )
  
  
  ggplot2::ggsave(
    
    paste0(
      filename,
      ".tiff"
    ),
    
    plot = plot,
    
    width = width,
    
    height = height,
    
    units = "in",
    
    dpi = dpi,
    
    compression = "lzw",
    
    bg = "white"
    
  )
  
  
  ggplot2::ggsave(
    
    paste0(
      filename,
      ".svg"
    ),
    
    plot = plot,
    
    width = width,
    
    height = height,
    
    units = "in",
    
    bg = "white"
    
  )
  
}



# ------------------------------------------------------------
# Publication theme
# ------------------------------------------------------------

publication_theme <- function(
    base_size = 14
) {
  
  ggplot2::theme_classic(
    
    base_size = base_size,
    
    base_family = "Arial"
    
  ) +
    
    ggplot2::theme(
      
      axis.title =
        ggplot2::element_text(
          size = base_size + 1
        ),
      
      axis.text =
        ggplot2::element_text(
          size = base_size
        ),
      
      plot.title =
        ggplot2::element_text(
          size = base_size + 3,
          face = "bold",
          hjust = 0.5
        ),
      
      plot.subtitle =
        ggplot2::element_text(
          size = base_size
        ),
      
      legend.title =
        ggplot2::element_text(
          size = base_size
        ),
      
      legend.text =
        ggplot2::element_text(
          size = base_size
        ),
      
      strip.text =
        ggplot2::element_text(
          size = base_size,
          face = "bold"
        ),
      
      plot.tag =
        ggplot2::element_text(
          size = base_size + 4,
          face = "bold"
        )
      
    )
  
}



# ------------------------------------------------------------
# Read 10x
# ------------------------------------------------------------

safe_read10x <- function(
    path
) {
  
  x <- Seurat::Read10X(
    data.dir = path)
  
  
  if (is.list(x)) {
    
    if ("Gene Expression" %in% names(x)) {
      
      x <- x[["Gene Expression"]]
      
   } else {
      
      x <- x[[1]]
      
    }
    
  }
  
  
  x
  
}



# ------------------------------------------------------------
# MAD filtering
# ------------------------------------------------------------

calc_mad_keep <- function(
    obj,
    nmads = 5
) {
  
  lc <- log1p(
    obj$nCount_RNA
  )
  
  lf <- log1p(
    obj$nFeature_RNA
  )
  
  
  med_c <- stats::median(
    lc,
    na.rm = TRUE
  )
  
  mad_c <- stats::mad(
    lc,
    center = med_c,
    constant = 1,
    na.rm = TRUE
  )
  
  
  med_f <- stats::median(
    lf,
    na.rm = TRUE
  )
  
  mad_f <- stats::mad(
    lf,
    center = med_f,
    constant = 1,
    na.rm = TRUE
  )
  
  
  keep_c <-
    
    lc >= (
      med_c -
        nmads * mad_c
    ) &
    
    lc <= (
      med_c +
        nmads * mad_c
    )
  
  
  keep_f <-
    
    lf >= (
      med_f -
        nmads * mad_f
    ) &
    
    lf <= (
      med_f +
        nmads * mad_f
    )
  
  
  keep_c & keep_f
  
}



# ------------------------------------------------------------
# Volcano plot
# ------------------------------------------------------------

make_volcano <- function(
    
  res,
  
  title,
  
  lfc_cut = 1,
  
  padj_cut = 0.05,
  
  label_n = 12
  
) {
  
  df <- res %>%
    
    dplyr::mutate(
      
      padj_plot =
        ifelse(
          is.na(padj),
          1,
          pmax(
            padj,
            1e-300
          )
        ),
      
      Status =
        dplyr::case_when(
          
          padj < padj_cut &
            log2FoldChange >= lfc_cut ~
            "Up in GDM",
          
          padj < padj_cut &
            log2FoldChange <= -lfc_cut ~
            "Down in GDM",
          
          TRUE ~
            "Not significant"
          
        ),
      
      neglog10 =
        -log10(
          padj_plot
        )
      
    )
  
  
  lab <- df %>%
    
    dplyr::filter(
      Status !=
        "Not significant"
    ) %>%
    
    dplyr::arrange(
      padj_plot
    ) %>%
    
    dplyr::slice_head(
      n = label_n
    )
  
  
  ggplot2::ggplot(
    
    df,
    
    ggplot2::aes(
      x = log2FoldChange,
      y = neglog10
    )
    
  ) +
    
    ggplot2::geom_point(
      
      ggplot2::aes(
        shape = Status
      ),
      
      alpha = 0.75,
      
      size = 1.7
      
    ) +
    
    ggplot2::geom_vline(
      
      xintercept = c(
        -lfc_cut,
        lfc_cut
      ),
      
      linetype = 2,
      
      linewidth = 0.4
      
    ) +
    
    ggplot2::geom_hline(
      
      yintercept =
        -log10(
          padj_cut
        ),
      
      linetype = 2,
      
      linewidth = 0.4
      
    ) +
    
    ggrepel::geom_text_repel(
      
      data = lab,
      
      ggplot2::aes(
        label = gene
      ),
      
      family = "Arial",
      
      size = 3.5,
      
      max.overlaps = Inf,
      
      box.padding = 0.4
      
    ) +
    
    ggplot2::labs(
      
      x =
        expression(
          log[2]~
            fold~
            change~
            "(GDM / Control)"
        ),
      
      y =
        expression(
          -log[10]~
            FDR
        ),
      
      title = title,
      
      shape = NULL
      
    ) +
    
    publication_theme(
      13
    ) +
    
    ggplot2::theme(
      legend.position =
        "bottom"
    )
  
}



# ------------------------------------------------------------
# DESeq2 donor-level pseudobulk
# ------------------------------------------------------------

run_deseq <- function(
    
  count_matrix,
  
  sample_group,
  
  min_total_count = 10
  
) {
  
  count_matrix <- as.matrix(
    count_matrix
  )
  
  
  if (is.null(
    colnames(
      count_matrix
    )
  )) {
    
    stop(
      "Count matrix does not contain column names."
    )
    
  }
  
  
  groups <- sample_group[
    colnames(
      count_matrix
    )
  ]
  
  
  if (any(
    is.na(
      groups
    )
  )) {
    
    stop(
      "Group information missing for at least one donor."
    )
    
  }
  
  
  coldata <- data.frame(
    
    row.names =
      colnames(
        count_matrix
      ),
    
    group =
      unname(
        groups
      ),
    
    stringsAsFactors =
      FALSE
    
  )
  
  
  coldata$group <-
    
    factor(
      
      coldata$group,
      
      levels = c(
        "Control",
        "GDM"
      )
      
    )
  
  
  count_matrix <- round(
    count_matrix
  )
  
  
  storage.mode(
    count_matrix
  ) <- "integer"
  
  
  keep <-
    
    rowSums(
      count_matrix
    ) >= min_total_count
  
  
  count_filtered <-
    
    count_matrix[
      keep,
      ,
      drop = FALSE
    ]
  
  
  if (
    nrow(
      count_filtered
    ) == 0
  ) {
    
    stop(
      "No genes remained after count filtering."
    )
    
  }
  
  
  dds <-
    
    DESeq2::DESeqDataSetFromMatrix(
      
      countData =
        count_filtered,
      
      colData =
        coldata,
      
      design =
        ~ 0 + group
      
    )
  
  
  dds <-
    
    DESeq2::DESeq(
      dds,
      quiet = TRUE
    )
  
  
  coef_names <-
    
    DESeq2::resultsNames(
      dds
    )
  
  
  if (
    !"groupControl" %in%
    coef_names ||
    !"groupGDM" %in%
    coef_names
  ) {
    
    stop(
      
      paste(
        
        "Expected DESeq2 coefficients were not found.",
        
        paste(
          coef_names,
          collapse = ", "
        )
        
      )
      
    )
    
  }
  
  
  res <-
    
    DESeq2::results(
      
      dds,
      
      contrast =
        list(
          "groupGDM",
          "groupControl"
        ),
      
      alpha = 0.05
      
    )
  
  
  res_df <-
    
    as.data.frame(
      res
    ) %>%
    
    tibble::rownames_to_column(
      "gene"
    ) %>%
    
    dplyr::arrange(
      padj,
      pvalue
    )
  
  
  res_df$direction <-
    
    dplyr::case_when(
      
      !is.na(
        res_df$padj
      ) &
        res_df$padj < 0.05 &
        res_df$log2FoldChange > 0 ~
        "Up in GDM",
      
      !is.na(
        res_df$padj
      ) &
        res_df$padj < 0.05 &
        res_df$log2FoldChange < 0 ~
        "Down in GDM",
      
      TRUE ~
        "Not significant"
      
    )
  
  
  list(
    
    dds = dds,
    
    result = res_df,
    
    coldata = coldata,
    
    counts = count_filtered,
    
    genes_tested =
      rownames(
        count_filtered
      )
    
  )
  
}



# ------------------------------------------------------------
# multiMiR target extraction
# ------------------------------------------------------------

extract_multimir_targets <- function(
    mm_obj
) {
  
  dat <-
    
    tryCatch(
      
      mm_obj@data,
      
      error =
        function(e) NULL
      
    )
  
  
  if (is.null(dat)) {
    
    dat <-
      
      tryCatch(
        
        mm_obj$data,
        
        error =
          function(e) NULL
        
      )
    
  }
  
  
  if (is.null(dat)) {
    
    stop(
      "Could not extract multiMiR results."
    )
    
  }
  
  
  mir_col <-
    
    intersect(
      
      c(
        "mature_mirna_id",
        "mirna",
        "miRNA",
        "mature_mirna"
      ),
      
      colnames(
        dat
      )
      
    )[1]
  
  
  gene_col <-
    
    intersect(
      
      c(
        "target_symbol",
        "target_gene",
        "gene_symbol",
        "symbol"
      ),
      
      colnames(
        dat
      )
      
    )[1]
  
  
  db_col <-
    
    intersect(
      
      c(
        "database",
        "db"
      ),
      
      colnames(
        dat
      )
      
    )[1]
  
  
  if (
    is.na(
      mir_col
    ) ||
    is.na(
      gene_col
    )
  ) {
    
    stop(
      "Could not identify miRNA or target-gene column in multiMiR output."
    )
    
  }
  
  
  dat %>% 
    dplyr::transmute(
      
      miRNA =
        .data[[mir_col]],
      
      target =
        .data[[gene_col]],
      
      database =
        if (
          !is.na(
   db_col
          )
        ) {
          
          .data[[db_col]]
          
        } else {
          
          NA_character_
          
        }
      
    ) %>%
    
    dplyr::filter(
      
      !is.na(
        miRNA
      ),
      
      !is.na(
        target
      ),
      
      target != ""
      
    ) %>%
    
    dplyr::distinct()
  
}



# ------------------------------------------------------------
# Permutation test
# ------------------------------------------------------------

permutation_enrichment <- function(
    
  de_genes,
  
  universe,
  
  targets,
  
  nperm = 10000,
  
  seed = 12345
  
) {
  
  universe <-
    
    unique(
      universe
    )
  
  
  de_genes <-
    
    unique(
      intersect(
        de_genes,
        universe
      )
    )
  
  
  targets <-
    
    unique(
      intersect(
        targets,
        universe
      )
    )
  
  
  n_de <-
    length(
      de_genes
    )
  
  
  observed <-
    
    length(
      intersect(
        de_genes,
        targets
      )
    )
  
  
  if (
    n_de == 0 ||
    length(
      universe
    ) == 0 ||
    n_de >
    length(
      universe
    )
  ) {
    
    return(
      
      data.frame(
        
        n_universe =
          length(
            universe
          ),
        
        n_de =
          n_de,
        
        n_targets_universe =
          length(
            targets
          ),
        
        observed =
          observed,
        
        expected_mean =
          NA_real_,
        
        expected_median =
          NA_real_,
        
        perm_q025 =
          NA_real_,
        
        perm_q975 =
          NA_real_,
        
        fold_enrichment =
          NA_real_,
        
        p_perm =
          NA_real_
        
      )
      
    )
    
  }
  
  
  set.seed(
    seed
  )
  
  
  perm <-
    
    replicate(
      
      nperm,
      
      length(
        
        intersect(
          
          sample(
            universe,
            size = n_de,
            replace = FALSE
          ),
          
          targets
          
        )
        
      )
      
    )
  
  
  expected_mean <-
    mean(
      perm
    )
  
  
  p_perm <-
    
    (
      sum(
        perm >= observed
      ) + 1
    ) /
    (
      nperm + 1
    )
  
  
  data.frame(
    
    n_universe =
      length(
        universe
      ),
    
    n_de =
      n_de,
    
    n_targets_universe =
      length(
        targets
      ),
    
    observed =
      observed,
    
    expected_mean =
      expected_mean,
    
    expected_median =
      stats::median(
        perm
      ),
    
    perm_q025 =
      as.numeric(
        stats::quantile(
          perm,
          0.025
        )
      ),
    
    perm_q975 =
      as.numeric(
        stats::quantile(
          perm,
          0.975
        )
      ),
    
    fold_enrichment =
      ifelse(
        expected_mean > 0,
        observed /
          expected_mean,
        NA_real_
      ),
    
    p_perm =
      p_perm
    
  )
  
}



# ------------------------------------------------------------
# Run one permutation family
# ------------------------------------------------------------

run_perm_family <- function(
    
  res_de,
  
  genes_tested,
  
  target_df,
  
  analysis_name,
  
  target_type,
  
  threshold_type,
  
  nperm = 10000
  
) {
  
  if (
    is.null(
      target_df
    ) ||
    nrow(
      target_df
    ) == 0
  ) {
    
    return(
      NULL
    )
    
  }
  
  
  res_de <-
    
    res_de %>%
    
    dplyr::filter(
      gene %in%
        genes_tested
    )
  
  
  if (
    threshold_type ==
    "FDR005"
  ) {
    
    base_de <-
      
      res_de %>%
      
      dplyr::filter(
        !is.na(
          padj
        ),
        padj < 0.05
      )
    
  } else if (
    threshold_type ==
    "NOMINAL005"
  ) {
    
    base_de <-
      
      res_de %>%
      
      dplyr::filter(
        !is.na(
          pvalue
        ),
        pvalue < 0.05
      )
    
  } else {
    
    stop(
      "Unknown threshold type."
    )
    
  }
  
  
  deg_sets <- list(
    
    All =
      base_de$gene,
    
    Up =
      base_de %>%
      dplyr::filter(
        log2FoldChange > 0
      ) %>%
      dplyr::pull(
        gene
      ),
    
    Down =
      base_de %>%
      dplyr::filter(
        log2FoldChange < 0
      ) %>%
      dplyr::pull(
        gene
      )
    
  )
  
  
  rows <-
    list()
  
  
  overlaps <-
    list()
  
  
  for (
    direction in
    names(
      deg_sets
    )
  ) {
    
    de_genes <-
      
      unique(
        deg_sets[[direction]]
      )
    
    
    for (
      mir in
      mirnas
    ) {
      
      targets <-
        
        target_df %>%
        
        dplyr::filter(
          miRNA == mir
        ) %>%
        
        dplyr::pull(
          target
        ) %>%
        
        unique()
      
      
      st <-
        
        permutation_enrichment(
          
          de_genes =
            de_genes,
          
          universe =
            genes_tested,
          
          targets =
            targets,
          
          nperm =
            nperm,
          
          seed =
            12345 +
            match(
              mir,
              mirnas
            ) +
            match(
              direction,
              names(
                deg_sets
              )
            ) * 100
          
        )
      
      
      st$Analysis <-
        analysis_name
      
      
      st$Target_type <-
        target_type
      
      
      st$Threshold <-
        threshold_type
      
      
      st$Direction <-
        direction
      
      
      st$miRNA <-
        mir
      
      
      rows[[
          paste(
            direction,
            mir,
            sep = "__"
          )
        ]] <- st
      
      
      ov <-
        
        intersect(
          
          de_genes,
          
          intersect(
            targets,
            genes_tested
          )
          
        )
      
      
      if (
        length(
          ov
        ) > 0
      ) {
        
        overlaps[[
            paste(
              direction,
              mir,
              sep = "__"
            )
          ]] <-
          
          data.frame(
            
            Analysis =
              analysis_name,
            
            Target_type =
              target_type,
            
            Threshold =
              threshold_type,
            
            Direction =
              direction,
            
            miRNA =
              mir,
            
            gene =
              ov,
            
            stringsAsFactors =
              FALSE
            
          )
        
      }
      
    }
    
  }
  
  
  stats <-
    
    dplyr::bind_rows(
      rows
    ) %>%
    
    dplyr::group_by(
      
      Analysis,
      
      Target_type,
      
      Threshold,
      
      Direction
      
    ) %>%
    
    dplyr::mutate(
      
      p_BH =
        p.adjust(
          p_perm,
          method = "BH"
        )
      
    ) %>%
    
    dplyr::ungroup() %>%
    
    dplyr::select(
      
      Analysis,
      
      Target_type,
      
      Threshold,
      
      Direction,
      
      miRNA,
      
      n_universe,
      
      n_de,
      
      n_targets_universe,
      
      observed,
      
      expected_mean,
      
      expected_median,
      
      perm_q025,
      
      perm_q975,
      
      fold_enrichment,
      
      p_perm,
      
      p_BH
      
    )
  
  
  overlap_df <-
    
    if (
      length(
        overlaps
      ) > 0
    ) {
      
      dplyr::bind_rows(
        overlaps
      )
      
    } else {
      
      data.frame()
      
    }
  
  
  list(
    
    stats =
      stats,
    
    overlaps =
      overlap_df
    
  )
  
}



# ============================================================
# 03. IMPORT + QC + DOUBLETS PER SAMPLE
# ============================================================

objects <-
  list()


qc_rows <-
  list()


for (
  sid in
  names(
    sample_dirs
  )
) {
  
  message(
    "Processing sample: ",
    sid
  )
  
  
  counts <-
    
    safe_read10x(
      sample_dirs[[sid]]
    )
  
  
  obj <-
    
    Seurat::CreateSeuratObject(
      
      counts =
        counts,
      
      project =
        sid,
      
      min.cells =
        3,
      
      min.features =
        min_features
      
    )
  
  
  obj$orig.ident <-
    sid
  
  
  obj$group <-
    sample_group[[sid]]
  
  
  obj$percent.mt <-
    
    Seurat::PercentageFeatureSet(
      
      obj,
      
      pattern =
        "^MT-"
      
    )
  
  
  n_initial <-
    ncol(
      obj
    )
  
  
  obj <-
    
    subset(
      obj,
      subset =
        percent.mt <
        max_percent_mt
    )
  
  
  n_after_mt <-
    ncol(
      obj
    )
  
  
  keep_mad <-
    
    calc_mad_keep(
      
      obj,
      
      nmads =
        mad_cutoff
      
    )
  
  
  obj <-
    obj[
      ,
      keep_mad
    ]
  
  
  n_after_mad <-
    ncol(
      obj
    )
  
  
  sce <-
    
    SingleCellExperiment::SingleCellExperiment(
      
      assays =
        list(
          
          counts =
            SeuratObject::LayerData(
              
              obj,
              
              assay =
                "RNA",
              
              layer =
                "counts"
              
            )
          
        )
      
    )
  
  
  colnames(
    sce
  ) <-
    colnames(
      obj
    )
  
  
  sce <-
    
    scDblFinder::scDblFinder(
      sce
    )
  
  
  obj$scDblFinder.class <-
    
    SummarizedExperiment::colData(
      sce
    )$scDblFinder.class
  
  
  obj$scDblFinder.score <-
    
    SummarizedExperiment::colData(
      sce
    )$scDblFinder.score
  
  
  n_doublets <-
    
    sum(
      obj$scDblFinder.class ==
        "doublet"
    )
  
  
  doublet_pct <-
    
    100 *
    n_doublets /
    n_after_mad
  
  
  obj <-
    
    subset(
      obj,
      subset =
        scDblFinder.class ==
        "singlet"
    )
  
  
  n_final <-
    ncol(
      obj
    )
  
  
  qc_rows[[sid]] <-
    
    data.frame(
      
      Sample =
        sid,
      
      Group =
        sample_group[[sid]],
      
      Initial_cells =
        n_initial,
      
      After_MT_filter =
        n_after_mt,
      
      After_MAD_filter =
        n_after_mad,
      
      Doublets =
        n_doublets,
      
      Final_singlets =
        n_final,
      
      Doublet_percent =
        doublet_pct
      
    )
  
  
  objects[[sid]] <-
    obj
  
  
  saveRDS(
    
    obj,
    
    file.path(
      
      output_dirs$rds,
      
      paste0(
        "Data_",
        sid,
        "_singlets.rds"
      )
      
    )
    
  )
  
}


qc_table <-
  
  dplyr::bind_rows(
    qc_rows
  )


utils::write.csv(
  
  qc_table,
  
  file.path(
    output_dirs$qc,
    "QC_summary.csv"
  ),
  
  row.names = FALSE
  
)



# ============================================================
# 04. QC FIGURES
# ============================================================

qc_long <-
  
  qc_table %>%
  
  dplyr::select(
    
    Sample,
    
    Group,
    
    Initial_cells,
    
    After_MT_filter,
    
    After_MAD_filter,
    
    Final_singlets
    
  ) %>%
  
  tidyr::pivot_longer(
    
    cols =
      -c(
        Sample,
        Group
      ),
    
    names_to =
      "Stage",
    
    values_to =
      "Cells"
    
  ) %>%
  
  dplyr::mutate(
    
    Stage =
      factor(
        
        Stage,
        
        levels =
          c(
            "Initial_cells",
            "After_MT_filter",
            "After_MAD_filter",
            "Final_singlets"
          ),
        
        labels =
          c(
            "Initial",
            "After MT filter",
            "After MAD filter",
            "Final singlets"
          )
        
      )
    
  )


p_qc_retention <-
  
  ggplot2::ggplot(
    
    qc_long,
    
    ggplot2::aes(
      x = Stage,
      y = Cells,
      group = Sample,
      shape = Sample
    )
    
  ) +
  
  ggplot2::geom_line(
    linewidth = 0.7
  ) +
  
  ggplot2::geom_point(
    size = 3
  ) +
  
  ggplot2::facet_wrap(
    ~ Sample,
    scales = "free_y"
  ) +
  
  ggplot2::labs(
    
    x = NULL,
    
    y =
      "Number of cells",
    
    title =
      "Cell retention during quality control"
    
  ) +
  
  publication_theme(
    12
  ) +
  
  ggplot2::theme(
    
    axis.text.x =
      ggplot2::element_text(
        angle = 35,
        hjust = 1
      ),
    
    legend.position =
      "none"
    
  )


save_plot_all(
  
  p_qc_retention,
  
  file.path(
    output_dirs$qc,
    "QC_cell_retention"
  ),
  
  10,
  
  7
  
)



# ============================================================
# 05. MERGE + NORMALIZATION + CCA INTEGRATION
# ============================================================

Merge <-
  
  merge(
    
    x =
      objects[[1]],
    
    y =
      objects[-1],
    
    add.cell.ids =
      names(
        objects
      ),
    
    project =
      "GSE173193"
    
  )


Merge[["RNA"]] <-
  
  SeuratObject::JoinLayers(
    Merge[["RNA"]]
  )


Merge[["RNA"]] <-
  
  split(
    
    Merge[["RNA"]],
    
    f =
      Merge$orig.ident
    
  )


Merge <-
  
  Seurat::NormalizeData(
    Merge,
    verbose = FALSE
  )


Merge <-
  
  Seurat::FindVariableFeatures(
    
    Merge,
    
    selection.method =
      "vst",
    
    nfeatures =
      n_hvg,
    
    verbose =
      FALSE
    
  )


Merge <-
  
  Seurat::ScaleData(
    Merge,
    verbose = FALSE
  )


Merge <-
  
  Seurat::RunPCA(
    
    Merge,
    
    npcs =
      50,
    
    verbose =
      FALSE
    
  )


p_elbow <-
  
  Seurat::ElbowPlot(
    
    Merge,
    
    ndims =
      50
    
  ) +
  
  ggplot2::ggtitle(
    "PCA elbow plot"
  ) +
  
  publication_theme(
    13
  )


save_plot_all(
  
  p_elbow,
  
  file.path(
    output_dirs$umap,
    "PCA_Elbow_50PC"
  ),
  
  7,
  
  5
  
)


Merge <-
  
  Seurat::IntegrateLayers(
    
    object =
      Merge,
    
    method =
      Seurat::CCAIntegration,
    
    orig.reduction =
      "pca",
    
    new.reduction =
      "integrated.cca",
    
    dims =
      1:n_pcs,
    
    verbose =
      FALSE
    
  )


Merge[["RNA"]] <-
  
  SeuratObject::JoinLayers(
    Merge[["RNA"]]
  )


Merge <-
  
  Seurat::FindNeighbors(
    
    Merge,
    
    reduction =
      "integrated.cca",
    
    dims =
      1:n_pcs,
    
    verbose =
      FALSE
    
  )


# UMAP does not depend on the selected clustering resolution

Merge <-
  
  Seurat::RunUMAP(
    
    Merge,
    
    reduction =
      "integrated.cca",
    
    dims =
      1:n_pcs,
    
    verbose =
      FALSE
    
  )



# ============================================================
# 06. RESOLUTION SENSITIVITY ANALYSIS
# ============================================================

resolution_summary <-
  list()


for (
  res in
  resolutions_test
) {
  
  message(
    "Testing clustering resolution: ",
    res
  )
  
  
  tmp <-
    
    Seurat::FindClusters(
      
      Merge,
      
      resolution =
        res,
      
      verbose =
        FALSE
      
    )
  
  
  col_name <-
    
    paste0(
      "clusters_res_",
      res
    )
  
  
  Merge[[col_name]] <-
    
    as.character(
      tmp$seurat_clusters
    )
  
  
  tab <-
    
    table(
      tmp$seurat_clusters
    )
  
  
  resolution_summary[[as.character(
      res
    )]] <-
    
    data.frame(
      
      Resolution =
        res,
      
      Number_of_clusters =
        length(
          tab
        ),
      
      Smallest_cluster =
        min(
          tab
        ),
      
      Largest_cluster =
        max(
          tab
        ),
      
      Median_cluster_size =
        median(
          tab
        ),
      
      Clusters_under_100_cells =
        sum(
          tab < 100
        ),
      
      Clusters_under_200_cells =
        sum(
          tab < 200
        )
      
    )
  
}


resolution_summary_df <-
  
  dplyr::bind_rows(
    resolution_summary
  )


print(
  resolution_summary_df
)


openxlsx::write.xlsx(
  
  resolution_summary_df,
  
  file =
    file.path(
      output_dirs$umap,
      "Clustering_resolution_summary.xlsx"
    ),
  
  overwrite =
    TRUE
  
)


plots_resolution <-
  list()


for (
  res in
  resolutions_test
) {
  
  col_res <-
    
    paste0(
      "clusters_res_",
      res
    )
  
  
  plots_resolution[[as.character(
      res
    )]] <-
    
    Seurat::DimPlot(
      
      Merge,
      
      reduction =
        "umap",
      
      group.by =
        col_res,
      
      label =
        TRUE,
      
      repel =
        TRUE,
      
      raster =
        FALSE
      
    ) +
    
    ggplot2::ggtitle(
      
      paste0(
        "Resolution ",
        res
      )
      
    ) +
    
    publication_theme(
      11
    ) +
    
    ggplot2::theme(
      legend.position =
        "none"
    )
  
}


p_resolution_comparison <-
  
  patchwork::wrap_plots(
    
    plots_resolution,
    
    ncol =
      2
    
  )


save_plot_all(
  
  p_resolution_comparison,
  
  file.path(
    output_dirs$umap,
    "UMAP_resolution_comparison"
  ),
  
  14,
  
  11
  
)



# ============================================================
# 07. FINAL CLUSTERING
# ============================================================

Merge <-
  
  Seurat::FindClusters(
    
    Merge,
    
    resolution =
      resolution_main,
    
    verbose =
      FALSE
    
  )


Merge$Final_cluster <-
  
  as.character(
    Merge$seurat_clusters
  )


saveRDS(
  
  Merge,
  
  file.path(
    
    output_dirs$rds,
    
    paste0(
      "GSE173193_CCA_",
      n_pcs,
      "PC_res",
      resolution_main,
      "_PRE_ANNOTATION.rds"
    )
    
  )
  
)



# ============================================================
# 07B. UMAP FIGURES
# ============================================================

p_umap_cluster <-
  
  Seurat::DimPlot(
    
    Merge,
    
    reduction =
      "umap",
    
    group.by =
      "seurat_clusters",
    
    label =
      TRUE,
    
    repel =
      TRUE,
    
    raster =
      FALSE
    
  ) +
  
  ggplot2::ggtitle(
    "Seurat clusters"
  ) +
  
  publication_theme(
    13
  ) +
  
  ggplot2::theme(
    legend.position =
      "right"
  )


p_umap_sample <-
  
  Seurat::DimPlot(
    
    Merge,
    
    reduction =
      "umap",
    
    group.by =
      "orig.ident",
    
    raster =
      FALSE
    
  ) +
  
  ggplot2::ggtitle(
    "Donors"
  ) +
  
  publication_theme(
    13
  ) +
  
  ggplot2::theme(
    legend.position =
      "bottom"
  )


p_umap_group <-
  
  Seurat::DimPlot(
    
    Merge,
    
    reduction =
      "umap",
    
    group.by =
      "group",
    
    raster =
      FALSE
    
  ) +
  
  ggplot2::ggtitle(
    "Clinical group"
  ) +
  
  publication_theme(
    13
  ) +
  
  ggplot2::theme(
    legend.position =
      "bottom"
  )


p_umap_split <-
  
  Seurat::DimPlot(
    
    Merge,
    
    reduction =
      "umap",
    
    split.by =
      "orig.ident",
    
    group.by =
      "seurat_clusters",
    
    ncol =
      2,
    
    raster =
      FALSE
    
  ) +
  
  publication_theme(
    11
  )


save_plot_all(
  
  p_umap_cluster,
  
  file.path(
    output_dirs$umap,
    "UMAP_clusters"
  ),
  
  9,
  
  7
  
)


save_plot_all(
  
  p_umap_sample,
  
  file.path(
    output_dirs$umap,
    "UMAP_samples"
  ),
  
  9,
  
  7
  
)


save_plot_all(
  
  p_umap_group,
  
  file.path(
    output_dirs$umap,
    "UMAP_groups"
  ),
  
  9,
  
  7
  
)


save_plot_all(
  
  p_umap_split,
  
  file.path(
    output_dirs$umap,
    "UMAP_split_samples"
  ),
  
  12,
  
  9
  
)



# ============================================================
# 08. BROAD CELL-TYPE ANNOTATION
# ============================================================

Seurat::DefaultAssay(
  Merge
) <- "RNA"


Merge[["RNA"]] <-
  
  SeuratObject::JoinLayers(
    Merge[["RNA"]]
  )


marker_sets <- list(
  
  VCT = c(
    "CDH1",
    "MET",
    "CCNB2",
    "NRP2",
    "PARP1",
    "INSL4"
  ),
  
  SCT = c(
    "CYP19A1",
    "CGA",
    "ERVFRD-1",
    "LGALS13",
    "EGFR"
  ),
  
  EVT = c(
    "HLA-G",
    "PAPPA2",
    "MMP2",
    "TGFB1",
    "CXCR6",
    "MMP11"
  ),
  
  Granulocyte = c(
    "FCGR3B",
    "CXCL8",
    "MNDA",
    "SELL",
    "CSF3R"
  ),
  
  Myelocyte = c(
    "TCN1",
    "CEACAM8",
    "S100A8",
    "MMP8",
    "DEFA4",
    "CAMP",
    "MPO"
  ),
  
  `T/NK cell` = c(
    "CD3D",
    "CD3E",
    "CD3G",
    "TRBC1",
    "TRBC2",
    "GZMA",
    "CCL5",
    "GZMK",
    "NKG7",
    "GNLY",
    "KLRD1"
  ),
  
  `B cell` = c(
    "CD79A",
    "CD79B",
    "CD19",
    "MS4A1",
    "CD37"
  ),
  
  Monocytes = c(
    "CD14",
    "CD300E",
    "CLEC12A",
    "FCN1",
    "LYZ"
  ),
  
  Macrophages = c(
    "CD14",
    "CD68",
    "AIF1",
    "CD163",
    "CD209",
    "CSF1R",
    "C1QA",
    "C1QB",
    "C1QC"
  ),
  
  Endothelial = c(
    "PECAM1",
    "VWF",
    "KDR",
    "EMCN",
    "ENG",
    "RAMP2"
  )
  
)


genes_markers <-
  
  unique(
    unlist(
      marker_sets
    )
  )


genes_present <-
  
  intersect(
    genes_markers,
    rownames(
      Merge
    )
  )


avg_cluster <-
  
  Seurat::AverageExpression(
    
    Merge,
    
    assays =
      "RNA",
    
    features =
      genes_present,
    
    group.by =
      "seurat_clusters",
    
    layer =
      "data",
    
    verbose =
      FALSE
    
  )$RNA


colnames(
  avg_cluster
) <-
  
  sub(
    "^g",
    "",
    colnames(
      avg_cluster
    )
  )


# ------------------------------------------------------------
# Marker expression table
# ------------------------------------------------------------

marker_table <-
  
  as.data.frame(
    
    t(
      avg_cluster
    )
    
  )


marker_table$Seurat_cluster <-
  
  rownames(
    marker_table
  )


marker_table <-
  
  marker_table %>%
  
  dplyr::relocate(
    Seurat_cluster
  )


openxlsx::write.xlsx(
  
  marker_table,
  
  file =
    file.path(
      output_dirs$annotation,
      "Average_expression_markers_by_cluster.xlsx"
    ),
  
  overwrite =
    TRUE
  
)



# ------------------------------------------------------------
# Z-score marker expression
# ------------------------------------------------------------

expr_z <-
  
  t(
    
    scale(
      
      t(
        as.matrix(
          avg_cluster
        )
      )
      
    )
    
  )


expr_z[
  is.na(
    expr_z
  )
] <- 0



# ------------------------------------------------------------
# Marker score
# ------------------------------------------------------------

calculate_score <- function(
    cell_type
) {
  
  genes_type <-
    
    intersect(
      
      marker_sets[[cell_type]],
      
      rownames(
        expr_z
      )
      
    )
  
  
  if (
    length(
      genes_type
    ) == 0
  ) {
    
    return(
      
      rep(
        NA_real_,
        ncol(
          expr_z
        )
      )
      
    )
    
  }
  
  
  colMeans(
    
    expr_z[
      genes_type,
      ,
      drop = FALSE
    ]
    
  )
  
}


score_matrix <-
  
  sapply(
    
    names(
      marker_sets
    ),
    
    calculate_score
    
  )


rownames(
  score_matrix
) <-
  
  colnames(
    expr_z
  )


predicted_type <-
  
  apply(
    
    score_matrix,
    
    1,
    
    function(
    x
    ) {
      
      if (
        all(
          is.na(
            x
          )
        )
      ) {
        
        return(
          "Unassigned"
        )
        
      }
      
      
      names(
        which.max(
          x
        )
      )
      
    }
    
  )


annotation_table <-
  
  data.frame(
    
    Seurat_cluster =
      rownames(
        score_matrix
      ),
    
    Cell_type =
      unname(
        predicted_type
      ),
    
    score_matrix,
    
    check.names =
      FALSE
    
  )


# ------------------------------------------------------------
# Conservative canonical-marker refinements
# ------------------------------------------------------------

getavg <- function(
    gene,
    cluster
) {
  
  if (
    !gene %in%
    rownames(
      avg_cluster
    ) ||
    !cluster %in%
    colnames(
      avg_cluster
    )
  ) {
    
    return(
      0
    )
    
  }
  
  
  avg_cluster[
    gene,
    cluster
  ]
  
}


final_annotation <-
  
  setNames(
    
    annotation_table$Cell_type,
    
    annotation_table$Seurat_cluster
    
  )


for (
  cl in
  names(
    final_annotation
  )
) {
  
  if (
    
    getavg(
      "PECAM1",
      cl
    ) > 2 ||
    
    getavg(
      "VWF",
      cl
    ) > 3
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "Endothelial"
    
    
  } else if (
    
    getavg(
      "CGA",
      cl
    ) > 10 ||
    
    getavg(
      "CYP19A1",
      cl
    ) > 2
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "SCT"
    
    
  } else if (
    
    getavg(
      "HLA-G",
      cl
    ) > 3 ||
    
    getavg(
      "PAPPA2",
      cl
    ) > 3
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "EVT"
    
    
  } else if (
    
    getavg(
      "FCGR3B",
      cl
    ) > 2
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "Granulocyte"
    
    
  } else if (
    
    getavg(
      "MPO",
      cl
    ) > 5 ||
    
    getavg(
      "TCN1",
      cl
    ) > 1
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "Myelocyte"
    
    
  } else if (
    
    getavg(
      "CD79A",
      cl
    ) > 1 ||
    
    getavg(
      "MS4A1",
      cl
    ) > 1
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "B cell"
    
    
  } else if (
    
    getavg(
      "CD3D",
      cl
    ) > 1 ||
    
    getavg(
      "NKG7",
      cl
    ) > 3 ||
    
    getavg(
      "GNLY",
      cl
    ) > 3
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "T/NK cell"
    
    
  } else if (
    
    getavg(
      "CD14",
      cl
    ) > 2 &&
    
    getavg(
      "CD68",
      cl
    ) > 2
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "Macrophages"
    
    
  } else if (
    
    getavg(
      "CD14",
      cl
    ) > 2 &&
    
    getavg(
      "FCN1",
      cl
    ) > 1
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "Monocytes"
    
    
  } else if (
    
    getavg(
      "CDH1",
      cl
    ) > 0.5
    
  ) {
    
    final_annotation[
      cl
    ] <-
      "VCT"
    
  }
  
}


annotation_table$Cell_type <-
  
  unname(
    
    final_annotation[
      annotation_table$Seurat_cluster
    ]
    
  )


Merge$Cell_type <-
  
  unname(
    
    final_annotation[
      as.character(
        Merge$seurat_clusters
      )
    ]
    
  )


Merge$Cell_type <-
  
  factor(
    
    Merge$Cell_type,
    
    levels =
      c(
        "VCT",
        "SCT",
        "EVT",
        "T/NK cell",
        "B cell",
        "Monocytes",
        "Macrophages",
        "Granulocyte",
        "Myelocyte",
        "Endothelial"
      )
    
  )


openxlsx::write.xlsx(
  
  annotation_table,
  
  file =
    file.path(
      output_dirs$annotation,
      "Cluster_annotation_scores.xlsx"
    ),
  
  overwrite =
    TRUE
  
)



# ------------------------------------------------------------
# Annotation counts
# ------------------------------------------------------------

cluster_celltype_counts <-
  
  as.data.frame(
    
    table(
      
      Merge$seurat_clusters,
      
      Merge$Cell_type
      
    )
    
  )


colnames(
  cluster_celltype_counts
) <-
  
  c(
    "Seurat_cluster",
    "Cell_type",
    "Cells"
  )


donor_celltype_counts <-
  
  as.data.frame(
    
    table(
      
      Merge$orig.ident,
      
      Merge$Cell_type
      
    )
    
  )


colnames(
  donor_celltype_counts
) <-
  
  c(
    "Sample",
    "Cell_type",
    "Cells"
  )


openxlsx::write.xlsx(
  
  list(
    
    Cluster_to_CellType =
      cluster_celltype_counts,
    
    Donor_to_CellType =
      donor_celltype_counts
    
  ),
  
  file =
    file.path(
      output_dirs$annotation,
      "Cell_type_counts.xlsx"
    ),
  
  overwrite =
    TRUE
  
)



# ------------------------------------------------------------
# DotPlot
# ------------------------------------------------------------

genes_dotplot <-
  
  c(
    
    "CDH1",
    "MET",
    
    "CYP19A1",
    "CGA",
    
    "HLA-G",
    "PAPPA2",
    
    "FCGR3B",
    "CSF3R",
    
    "MPO",
    "TCN1",
    
    "CD3D",
    "CD3E",
    "TRBC1",
    "NKG7",
    "GNLY",
    
    "CD79A",
    "MS4A1",
    
    "CD14",
    "FCN1",
    
    "CD68",
    "C1QA",
    
    "PECAM1",
    "VWF",
    "KDR"
    
  )


genes_dotplot <-
  
  intersect(
    
    genes_dotplot,
    
    rownames(
      Merge
    )
    
  )


p_dotplot_annotation <-
  
  Seurat::DotPlot(
    
    Merge,
    
    features =
      genes_dotplot,
    
    group.by =
      "seurat_clusters"
    
  ) +
  
  Seurat::RotatedAxis() +
  
  ggplot2::labs(
    
    x = NULL,
    
    y =
      "Seurat cluster",
    
    title =
      "Canonical markers used for cell-type annotation"
    
  ) +
  
  publication_theme(
    12
  )


save_plot_all(
  
  p_dotplot_annotation,
  
  file.path(
    output_dirs$annotation,
    "DotPlot_markers_clusters"
  ),
  
  14,
  
  8
  
)



# ------------------------------------------------------------
# UMAP cell type
# ------------------------------------------------------------

p_umap_celltype <-
  
  Seurat::DimPlot(
    
    Merge,
    
    reduction =
      "umap",
    
    group.by =
      "Cell_type",
    
    label =
      TRUE,
    
    repel =
      TRUE,
    
    raster =
      FALSE
    
  ) +
  
  ggplot2::ggtitle(
    "Broad placental cell types"
  ) +
  
  publication_theme(
    14
  ) +
  
  ggplot2::theme(
    legend.position =
      "right"
  )


save_plot_all(
  
  p_umap_celltype,
  
  file.path(
    output_dirs$annotation,
    "UMAP_celltypes"
  ),
  
  11,
  
  8
  
)


saveRDS(
  
  Merge,
  
  file =
    file.path(
      output_dirs$rds,
      "GSE173193_FINAL_annotated.rds"
    )
  
)



# ============================================================
# 09. CELL COUNTS + DESCRIPTIVE PROPORTIONS
# ============================================================

cell_counts <-
  
  as.data.frame(
    
    table(
      
      Merge$Cell_type,
      
      Merge$orig.ident
      
    )
    
  ) %>%
  
  dplyr::rename(
    
    Cell_type =
      Var1,
    
    Sample =
      Var2,
    
    Cells =
      Freq
    
  ) %>%
  
  dplyr::mutate(
    
    Group =
      sample_group[
        as.character(
          Sample
        )
      ]
    
  )


cell_props <-
  
  cell_counts %>%
  
  dplyr::group_by(
    Sample
  ) %>%
  
  dplyr::mutate(
    
    Proportion =
      Cells /
      sum(
        Cells
      ),
    
    Percent =
      100 *
      Proportion
    
  ) %>%
  
  dplyr::ungroup()


utils::write.csv(
  
  cell_counts,
  
  file.path(
    output_dirs$composition,
    "Cell_counts_by_sample.csv"
  ),
  
  row.names =
    FALSE
  
)


utils::write.csv(
  
  cell_props,
  
  file.path(
    output_dirs$composition,
    "Cell_proportions_by_sample.csv"
  ),
  
  row.names =
    FALSE
  
)


p_prop <-
  
  ggplot2::ggplot(
    
    cell_props,
    
    ggplot2::aes(
      
      x =
        Sample,
      
      y =
        Percent,
      
      fill =
        Cell_type
      
    )
    
  ) +
  
  ggplot2::geom_col(
    width = 0.8
  ) +
  
  ggplot2::labs(
    
    x = NULL,
    
    y =
      "Cells (%)",
    
    fill = NULL,
    
    title =
      "Cell-type composition by donor"
    
  ) +
  
  publication_theme(
    12
  ) +
  
  ggplot2::theme(
    legend.position =
      "right"
  )


save_plot_all(
  
  p_prop,
  
  file.path(
    output_dirs$composition,
    "Cell_type_composition_stacked"
  ),
  
  10,
  
  7
  
)



# ============================================================
# 10. GLOBAL DONOR-LEVEL PSEUDOBULK
# ============================================================

Seurat::DefaultAssay(
  Merge
) <-
  "RNA"


Merge[["RNA"]] <-
  
  SeuratObject::JoinLayers(
    Merge[["RNA"]]
  )


pb_global <-
  
  Seurat::AggregateExpression(
    
    Merge,
    
    assays =
      "RNA",
    
    group.by =
      "orig.ident",
    
    return.seurat =
      FALSE,
    
    verbose =
      FALSE
    
  )$RNA


donors_expected <-
  
  c(
    "C1",
    "C2",
    "G1",
    "G2"
  )


if (
  !all(
    donors_expected %in%
    colnames(
      pb_global
    )
  )
) {
  
  stop(
    "Not all four donors were found in the global pseudobulk matrix."
  )
  
}


pb_global <-
  
  pb_global[
    ,
    donors_expected,
    drop = FALSE
  ]


res_global_obj <-
  
  run_deseq(
    
    pb_global,
    
    sample_group
    
  )


res_global <-
  
  res_global_obj$result


res_global_sig <-
  
  res_global %>%
  
  dplyr::filter(
    
    !is.na(
      padj
    ),
    
    padj < 0.05
    
  )


utils::write.csv(
  
  res_global,
  
  file =
    file.path(
      output_dirs$pseudobulk_global,
      "DESeq2_Global_ALL_genes.csv"
    ),
  
  row.names =
    FALSE
  
)


utils::write.csv(
  
  res_global_sig,
  
  file =
    file.path(
      output_dirs$pseudobulk_global,
      "DESeq2_Global_FDR005.csv"
    ),
  
  row.names =
    FALSE
  
)


openxlsx::write.xlsx(
  
  list(
    
    All_genes =
      res_global,
    
    FDR_005 =
      res_global_sig
    
  ),
  
  file =
    file.path(
      output_dirs$pseudobulk_global,
      "DESeq2_Global.xlsx"
    ),
  
  overwrite =
    TRUE
  
)


p_vol_global <-
  
  make_volcano(
    
    res_global,
    
    "Whole-placenta pseudobulk"
    
  )


save_plot_all(
  
  p_vol_global,
  
  file.path(
    output_dirs$pseudobulk_global,
    "Volcano_Global"
  ),
  
  8,
  
  6.5
  
)


saveRDS(
  
  res_global_obj,
  
  file =
    file.path(
      output_dirs$pseudobulk_global,
      "DESeq2_Global_object.rds"
    )
  
)



# ============================================================
# 11. CELL-TYPE DONOR-LEVEL PSEUDOBULK
# ============================================================

celltypes_to_test <-
  
  levels(
    Merge$Cell_type
  )


pb_celltype_results <-
  list()


pb_celltype_genes_tested <-
  list()


pb_celltype_counts <-
  list()


pb_summary_rows <-
  list()


for (
  ct in
  celltypes_to_test
) {
  
  message(
    "\nPseudobulk: ",
    ct
  )
  
  
  sub_obj <-
    
    subset(
      
      Merge,
      
      subset =
        Cell_type ==
        ct
      
    )
  
  
  n_per_donor <-
    
    table(
      sub_obj$orig.ident
    )
  
  
  n_vec <-
    
    c(
      C1 = 0,
      C2 = 0,
      G1 = 0,
      G2 = 0
    )
  
  
  n_vec[
    names(
      n_per_donor
    )
  ] <-
    
    as.integer(
      n_per_donor
    )
  
  
  if (
    any(
      n_vec <
      min_cells_per_donor_pb
    )
  ) {
    
    pb_summary_rows[[ct]] <-
      
      data.frame(
        
        Cell_type =
          ct,
        
        C1 =
          unname(
            n_vec[
              "C1"
            ]
          ),
        
        C2 =
          unname(
            n_vec[
              "C2"
            ]
          ),
        
        G1 =
          unname(
            n_vec[
              "G1"
            ]
          ),
        
        G2 =
          unname(
            n_vec[
              "G2"
            ]
          ),
        
        Genes_tested =
          NA_integer_,
        
        Raw_P_005 =
          NA_integer_,
        
        FDR_005 =
          NA_integer_,
        
        Note =
          paste0(
            "Skipped: fewer than ",
            min_cells_per_donor_pb,
            " cells in at least one donor"
          )
        
      )
    
    
    message(
      "Skipped due to insufficient representation."
    )
    
    
    next
    
  }
  
  
  Seurat::DefaultAssay(
    sub_obj
  ) <-
    "RNA"
  
  
  sub_obj[["RNA"]] <-
    
    SeuratObject::JoinLayers(
      sub_obj[["RNA"]]
    )
  
  
  pb <-
    
    Seurat::AggregateExpression(
      
      sub_obj,
      
      assays =
        "RNA",
      
      group.by =
        "orig.ident",
      
      return.seurat =
        FALSE,
      
      verbose =
        FALSE
      
    )$RNA
  
  
  if (
    !all(
      donors_expected %in%
      colnames(
        pb
      )
    )
  ) {
    
    next
    
  }
  
  
  pb <-
    
    pb[
      ,
      donors_expected,
      drop = FALSE
    ]
  
  
  rr_obj <-
    
    run_deseq(
      
      pb,
      
      sample_group
      
    )
  
  
  rr <-
    rr_obj$result
  
  
  rr$Cell_type <-
    ct
  
  
  pb_celltype_results[[ct]] <-
    rr
  
  
  pb_celltype_genes_tested[[ct]] <-
    rr_obj$genes_tested
  
  
  pb_celltype_counts[[ct]] <-
    pb
  
  
  n_raw <-
    
    sum(
      
      rr$pvalue < 0.05,
      
      na.rm =
        TRUE
      
    )
  
  
  n_fdr <-
    
    sum(
      
      rr$padj < 0.05,
      
      na.rm =
        TRUE
      
    )
   pb_summary_rows[[ct]] <-
    
    data.frame(
      
      Cell_type =
        ct,
      
      C1 =
        unname(
          n_vec[
            "C1"
          ]
        ),
      
      C2 =
        unname(
          n_vec[
            "C2"
          ]
        ),
      
      G1 =
        unname(
          n_vec[
            "G1"
          ]
        ),
      
      G2 =
        unname(
          n_vec[
            "G2"
          ]
        ),
      
      Genes_tested =
        nrow(
          rr
        ),
      
      Raw_P_005 =
        n_raw,
      
      FDR_005 =
        n_fdr,
      
      Note =
        "Exploratory: n = 2 donors/group"
      
    )
  
  
  safe_ct <-
    
    gsub(
      "[^A-Za-z0-9]+",
      "_",
      ct
    )
  
  
  rr_sig <-
    
    rr %>%
    
    dplyr::filter(
      
      !is.na(
        padj
      ),
      
      padj < 0.05
      
    )
  
  
  utils::write.csv(
    
    rr,
    
    file =
      file.path(
        
        output_dirs$pseudobulk_celltypes,
        
        paste0(
          "DESeq2_",
          safe_ct,
          "_ALL_genes.csv"
        )
        
      ),
    
    row.names =
      FALSE
    
  )
  
  
  utils::write.csv(
    
    rr_sig,
    
    file =
      file.path(
        
        output_dirs$pseudobulk_celltypes,
        
        paste0(
          "DESeq2_",
          safe_ct,
          "_FDR005.csv"
        )
        
      ),
    
    row.names =
      FALSE
    
  )
  
  
  pv <-
    
    make_volcano(
      
      rr,
      
      paste0(
        ct,
        " pseudobulk"
      )
      
    )
  
  
  save_plot_all(
    
    pv,
    
    file.path(
      
      output_dirs$pseudobulk_celltypes,
      
      paste0(
        "Volcano_",
        safe_ct
      )
      
    ),
    
    8,
    
    6.5
    
  )
  
}


pb_summary <-
  
  dplyr::bind_rows(
    pb_summary_rows
  )


pb_celltype_all <-
  
  dplyr::bind_rows(
    pb_celltype_results
  )


utils::write.csv(
  
  pb_summary,
  
  file =
    file.path(
      output_dirs$pseudobulk_celltypes,
      "Pseudobulk_CellTypes_summary.csv"
    ),
  
  row.names =
    FALSE
  
)


openxlsx::write.xlsx(
  
  pb_summary,
  
  file =
    file.path(
      output_dirs$pseudobulk_celltypes,
      "Pseudobulk_CellTypes_summary.xlsx"
    ),
  
  overwrite =
    TRUE
  
)


utils::write.csv(
  
  pb_celltype_all,
  
  file =
    file.path(
      output_dirs$pseudobulk_celltypes,
      "DESeq2_ALL_CellTypes_combined.csv"
    ),
  
  row.names =
    FALSE
  
)



# ------------------------------------------------------------
# Number of DE genes by cell type
# ------------------------------------------------------------

pb_summary_plot <-
  
  pb_summary %>%
  
  dplyr::filter(
    !is.na(
      FDR_005
    )
  )


p_pb_summary <-
  
  ggplot2::ggplot(
    
    pb_summary_plot,
    
    ggplot2::aes(
      x =
        reorder(
          Cell_type,
          FDR_005
        ),
      y =
        FDR_005
    )
    
  ) +
  
  ggplot2::geom_col(
    width = 0.7
  ) +
  
  ggplot2::coord_flip() +
  
  ggplot2::labs(
    
    x = NULL,
    
    y =
      "Genes with FDR < 0.05",
    
    title =
      "Cell-type-specific pseudobulk differential expression"
    
  ) +
  
  publication_theme(
    12
  )


save_plot_all(
  
  p_pb_summary,
  
  file.path(
    output_dirs$pseudobulk_celltypes,
    "Pseudobulk_FDR_genes_by_celltype"
  ),
  
  8,
  
  6.5
  
)

# ============================================================
# 11B. GO BIOLOGICAL PROCESS BY CELL TYPE
# FDR-significant DEGs; Up and Down analyzed separately
# ============================================================

go_celltype_dir <- file.path(
  output_dirs$pseudobulk_celltypes,
  "GO_BP"
)

dir.create(
  go_celltype_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


go_bp_list <- list()

for (ct in names(pb_celltype_results)) {
  
  rr <- pb_celltype_results[[ct]]
  universe_ct <- unique(pb_celltype_genes_tested[[ct]])
  
  for (dir_name in c("Up", "Down")) {
    
    genes_dir <- rr %>%
      dplyr::filter(
        !is.na(padj),
        padj < 0.05,
        if (dir_name == "Up") log2FoldChange > 0 else log2FoldChange < 0
      ) %>%
      dplyr::pull(gene) %>%
      unique()
    
    if (length(genes_dir) < 3) next
    
    ego <- tryCatch(
      clusterProfiler::enrichGO(
        gene = genes_dir,
        universe = universe_ct,
        OrgDb = org.Hs.eg.db::org.Hs.eg.db,
        keyType = "SYMBOL",
        ont = "BP",
        pAdjustMethod = "BH",
        pvalueCutoff = 0.05,
        qvalueCutoff = 0.05,
        minGSSize = 10,
        maxGSSize = 500,
        readable = FALSE
      ),
      error = function(e) {
        message("GO failed for ", ct, " / ", dir_name, ": ", conditionMessage(e))
        NULL
      }
    )
    
    if (!is.null(ego) && nrow(as.data.frame(ego)) > 0) {
      tmp_go <- as.data.frame(ego) %>%
        dplyr::mutate(
          Cell_type = ct,
          Direction = dir_name,
          n_input_genes = length(genes_dir),
          n_universe = length(universe_ct),
          .before = 1
        )
      
      go_bp_list[[paste(ct, dir_name, sep = "__")]] <- tmp_go
    }
  }
}


go_bp_results <- if (length(go_bp_list) > 0) {
  dplyr::bind_rows(go_bp_list)
} else {
  data.frame()
}

if (nrow(go_bp_results) > 0) {
  go_bp_results <- go_bp_results %>%
    dplyr::arrange(Cell_type, Direction, p.adjust)
}

utils::write.csv(
  go_bp_results,
  file.path(go_celltype_dir, "GO_BP_ALL_significant_results.csv"),
  row.names = FALSE
)

if (nrow(go_bp_results) > 0) {
  go_bp_top <- go_bp_results %>%
    dplyr::group_by(Cell_type, Direction) %>%
    dplyr::slice_min(order_by = p.adjust, n = 3, with_ties = FALSE) %>%
    dplyr::ungroup()
} else {
  go_bp_top <- data.frame()
}


# ============================================================
# 12. EXPERIMENTALLY VALIDATED miRNA TARGETS ONLY
# ============================================================

validated_file <- file.path(
  output_dirs$mirna,
  "miRNA_targets_VALIDATED_multiMiR.csv"
)

message("Querying experimentally validated miRNA targets...")

mm_validated <- tryCatch(
  multiMiR::get_multimir(
    mirna = mirnas,
    table = "validated",
    summary = FALSE,
    legacy.out = FALSE
  ),
  error = function(e) {
    message("multiMiR query failed: ", conditionMessage(e))
    NULL
  }
)

validated_targets <- NULL

if (!is.null(mm_validated)) {
  validated_targets <- extract_multimir_targets(mm_validated) %>%
    dplyr::filter(miRNA %in% mirnas) %>%
    dplyr::distinct()
  
  utils::write.csv(
    validated_targets,
    validated_file,
    row.names = FALSE
  )
  
} else if (file.exists(validated_file)) {
  message("Using saved validated-target table: ", validated_file)
  validated_targets <- utils::read.csv(
    validated_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  
} else {
  stop(
    paste0(
      "Validated miRNA targets could not be retrieved and no cached file was found: ",
      validated_file
    )
  )
}

if (is.null(validated_targets) || nrow(validated_targets) == 0) {
  stop("No experimentally validated miRNA targets were available.")
}

validated_target_pairs <- validated_targets %>%
  dplyr::select(miRNA, target) %>%
  dplyr::distinct()


# ============================================================
# 13. PERMUTATION ANALYSIS
# GLOBAL + ALL TESTABLE CELL TYPES
# VALIDATED TARGETS ONLY
# ============================================================

perm_stats_list <- list()
perm_overlap_list <- list()

target_df <- validated_target_pairs
tt <- "Validated"

# Global
for (threshold in c("FDR005", "NOMINAL005")) {
  tmp <- run_perm_family(
    res_de = res_global,
    genes_tested = res_global_obj$genes_tested,
    target_df = target_df,
    analysis_name = "Global",
    target_type = tt,
    threshold_type = threshold,
    nperm = n_perm
  )
  
  if (!is.null(tmp)) {
    nm <- paste("Global", tt, threshold, sep = "__")
    perm_stats_list[[nm]] <- tmp$stats
    perm_overlap_list[[nm]] <- tmp$overlaps
  }
}

# Cell types
for (ct in names(pb_celltype_results)) {
  for (threshold in c("FDR005", "NOMINAL005")) {
    tmp <- run_perm_family(
      res_de = pb_celltype_results[[ct]],
      genes_tested = pb_celltype_genes_tested[[ct]],
      target_df = target_df,
      analysis_name = ct,
      target_type = tt,
      threshold_type = threshold,
      nperm = n_perm
    )
    
    if (!is.null(tmp)) {
      nm <- paste(ct, tt, threshold, sep = "__")
      perm_stats_list[[nm]] <- tmp$stats
      perm_overlap_list[[nm]] <- tmp$overlaps
    }
  }
}

perm_stats <- dplyr::bind_rows(perm_stats_list)
perm_overlaps <- dplyr::bind_rows(perm_overlap_list)

# More stringent correction across analyses + miRNAs within each
# Target_type x Threshold x Direction family.
if (nrow(perm_stats) > 0) {
  perm_stats <- perm_stats %>%
    dplyr::group_by(Target_type, Threshold, Direction) %>%
    dplyr::mutate(
      p_BH_across_celltypes = stats::p.adjust(p_perm, method = "BH")
    ) %>%
    dplyr::ungroup()
}

utils::write.csv(
  perm_stats,
  file.path(output_dirs$mirna, "Permutation_ALL_celltypes.csv"),
  row.names = FALSE
)

utils::write.csv(
  perm_overlaps,
  file.path(output_dirs$mirna, "Permutation_ALL_celltypes_overlap_genes.csv"),
  row.names = FALSE
)

# Save a global-only table too; p_BH here is the BH correction across
# the five miRNAs within each global Threshold x Direction family.
perm_global <- perm_stats %>%
  dplyr::filter(Analysis == "Global") %>%
  dplyr::arrange(Threshold, Direction, p_BH, p_perm)

utils::write.csv(
  perm_global,
  file.path(output_dirs$mirna, "Permutation_Global.csv"),
  row.names = FALSE
)

message(
  "Global permutation results with p_BH < 0.05: ",
  sum(perm_global$p_BH < 0.05, na.rm = TRUE)
)


# ============================================================
# 13A. VALIDATED miRNA TARGET-DEGs
# FINAL DEG CRITERION: DESeq2 FDR < 0.05
# ============================================================

validated_target_degs <- pb_celltype_all %>%
  dplyr::filter(
    !is.na(padj),
    padj < 0.05
  ) %>%
  dplyr::inner_join(
    validated_target_pairs,
    by = c("gene" = "target"),
    relationship = "many-to-many"
  ) %>%
  dplyr::select(
    miRNA,
    Cell_type,
    gene,
    baseMean,
    log2FoldChange,
    lfcSE,
    stat,
    pvalue,
    padj,
    direction
  ) %>%
  dplyr::arrange(miRNA, Cell_type, padj)

utils::write.csv(
  validated_target_degs,
  file.path(output_dirs$mirna, "Validated_miRNA_target_DEGs_FDR005.csv"),
  row.names = FALSE
)


# ============================================================
# 13B. GO BP OF VALIDATED miRNA TARGET-DEGs
# Up + Down combined per miRNA
# Background = validated targets of the SAME miRNA that were
# testable in at least one cell-type pseudobulk analysis.
# ============================================================

go_mirna_dir <- file.path(
  output_dirs$mirna,
  "GO_BP_validated_target_DEGs_FDR005"
)

dir.create(
  go_mirna_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

mirna_go_genes <- validated_target_degs %>%
  dplyr::select(miRNA, gene) %>%
  dplyr::distinct()

genes_tested_all <- unique(unlist(pb_celltype_genes_tested, use.names = FALSE))

go_mirna_list <- list()

for (mir in mirnas) {
  universe_mir <- validated_target_pairs %>%
    dplyr::filter(miRNA == mir) %>%
    dplyr::pull(target) %>%
    unique() %>%
    intersect(genes_tested_all)
  
  foreground_mir <- mirna_go_genes %>%
    dplyr::filter(miRNA == mir) %>%
    dplyr::pull(gene) %>%
    unique() %>%
    intersect(universe_mir)
  
  if (length(foreground_mir) < 3 || length(universe_mir) < 10) next
  
  ego <- tryCatch(
    clusterProfiler::enrichGO(
      gene = foreground_mir,
      universe = universe_mir,
      OrgDb = org.Hs.eg.db::org.Hs.eg.db,
      keyType = "SYMBOL",
      ont = "BP",
      pAdjustMethod = "BH",
      pvalueCutoff = 0.05,
      qvalueCutoff = 0.05,
      minGSSize = 10,
      maxGSSize = 500,
      readable = FALSE
    ),
    error = function(e) {
      message("miRNA GO failed for ", mir, ": ", conditionMessage(e))
      NULL
    }
  )
  
  if (!is.null(ego) && nrow(as.data.frame(ego)) > 0) {
    tmp <- as.data.frame(ego) %>%
      dplyr::mutate(
        miRNA = mir,
        n_target_DEGs = length(foreground_mir),
        n_testable_validated_targets = length(universe_mir),
        .before = 1
      )
    go_mirna_list[[mir]] <- tmp
  }
}


go_mirna_results <- if (length(go_mirna_list) > 0) {
  dplyr::bind_rows(go_mirna_list) %>%
    dplyr::arrange(miRNA, p.adjust)
} else {
  data.frame()
}

utils::write.csv(
  go_mirna_results,
  file.path(go_mirna_dir, "GO_BP_validated_target_DEGs_ALL.csv"),
  row.names = FALSE
)

if (nrow(go_mirna_results) > 0) {
  go_mirna_top <- go_mirna_results %>%
    dplyr::group_by(miRNA) %>%
    dplyr::slice_min(order_by = p.adjust, n = 3, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      miRNA_short = sub("^hsa-", "", miRNA),
      GeneRatio_numeric = vapply(
        strsplit(as.character(GeneRatio), "/", fixed = TRUE),
        function(x) as.numeric(x[1]) / as.numeric(x[2]),
        numeric(1)
      ),
      minus_log10_FDR = -log10(pmax(p.adjust, 1e-300))
    )
} else {
  go_mirna_top <- data.frame()
}

if (nrow(go_mirna_top) > 0) {
  p_go_mirna <- ggplot2::ggplot(
    go_mirna_top,
    ggplot2::aes(x = miRNA_short, y = Description)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = GeneRatio_numeric,
        fill = minus_log10_FDR
      ),
      shape = 21,
      color = "black",
      stroke = 0.45
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      name = expression(-log[10] ~ "FDR")
    ) +
    ggplot2::scale_size_continuous(
      name = "Gene ratio",
      range = c(3, 8)
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      title = "GO Biological Process enrichment"
    ) +
    publication_theme(14) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 40, hjust = 1),
      legend.position = "right"
    )
  
  save_plot_all(
    p_go_mirna,
    file.path(go_mirna_dir, "GO_BP_validated_target_DEGs_top"),
    11,
    7
  )
}


# ============================================================
# 14. FINAL MANUSCRIPT FIGURE 2
# A = broad placental cell types
# B-F = validated miRNA target-DEGs (FDR < 0.05)
# ============================================================

if (is.factor(Merge$Cell_type)) {
  celltype_levels_umap <- levels(Merge$Cell_type)
} else {
  celltype_levels_umap <- sort(unique(as.character(Merge$Cell_type)))
}

celltype_colors <- setNames(
  scales::hue_pal()(length(celltype_levels_umap)),
  celltype_levels_umap
)

p_umap_celltype_final <- Seurat::DimPlot(
  Merge,
  reduction = "umap",
  group.by = "Cell_type",
  label = TRUE,
  label.size = 6.5,
  repel = TRUE,
  raster = FALSE,
  cols = celltype_colors
) +
  ggplot2::ggtitle("Placental cell types") +
  publication_theme(15) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 21, face = "bold", hjust = 0.5),
    axis.title = ggplot2::element_text(size = 16),
    axis.text = ggplot2::element_text(size = 14, color = "black"),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 13),
    legend.position = "right"
  )

selected_labels <- tibble::tribble(
  ~miRNA, ~Cell_type, ~gene,
  "hsa-miR-29a-3p", "EVT", "GPX7",
  "hsa-miR-29a-3p", "EVT", "SIK1",
  "hsa-miR-29a-3p", "Granulocyte", "IFIT1",
  "hsa-miR-29a-3p", "Granulocyte", "HSPA1B",
  "hsa-miR-29a-3p", "Macrophages", "HBEGF",
  "hsa-miR-29a-3p", "Macrophages", "HSPB1",
  "hsa-miR-29a-3p", "Monocytes", "FKBP4",
  "hsa-miR-29a-3p", "Monocytes", "HSPA1A",
  "hsa-miR-29a-3p", "Myelocyte", "EGR1",
  "hsa-miR-29a-3p", "Myelocyte", "JUN",
  "hsa-miR-29a-3p", "SCT", "ARMCX3",
  "hsa-miR-29a-3p", "T/NK cell", "HSPA1A",
  "hsa-miR-29a-3p", "T/NK cell", "HSPA1B",
  "hsa-miR-29a-3p", "VCT", "IL6",
  "hsa-miR-29a-3p", "VCT", "IFIT1",
  
  "hsa-miR-132-3p", "EVT", "FOSB",
  "hsa-miR-132-3p", "EVT", "CA1",
  "hsa-miR-132-3p", "Granulocyte", "HSPA1B",
  "hsa-miR-132-3p", "Granulocyte", "HSPH1",
  "hsa-miR-132-3p", "Macrophages", "HBEGF",
  "hsa-miR-132-3p", "Macrophages", "HSPA1B",
  "hsa-miR-132-3p", "Monocytes", "SERPINH1",
  "hsa-miR-132-3p", "Monocytes", "TIPARP",
  "hsa-miR-132-3p", "Myelocyte", "HSPD1",
  "hsa-miR-132-3p", "Myelocyte", "HSPA8",
  "hsa-miR-132-3p", "T/NK cell", "HSPA1B",
  "hsa-miR-132-3p", "VCT", "SPARC",
  "hsa-miR-132-3p", "VCT", "NUDT19",
  
  "hsa-miR-150-5p", "Granulocyte", "HSPA1B",
  "hsa-miR-150-5p", "Granulocyte", "HSPA8",
  "hsa-miR-150-5p", "Macrophages", "SERPINH1",
  "hsa-miR-150-5p", "Macrophages", "HSPA1B",
  "hsa-miR-150-5p", "Monocytes", "TNFSF15",
  "hsa-miR-150-5p", "Monocytes", "CACYBP",
  "hsa-miR-150-5p", "Myelocyte", "JUN",
  "hsa-miR-150-5p", "Myelocyte", "HSPA8",
  "hsa-miR-150-5p", "T/NK cell", "HSPA1B",
  "hsa-miR-150-5p", "VCT", "DSEL",
  "hsa-miR-150-5p", "VCT", "HSPA1B",
  
  "hsa-miR-155-5p", "EVT", "IFIT2",
  "hsa-miR-155-5p", "EVT", "CXCL2",
  "hsa-miR-155-5p", "Granulocyte", "IFIT1",
  "hsa-miR-155-5p", "Granulocyte", "IFIT2",
  "hsa-miR-155-5p", "Macrophages", "CXCL1",
  "hsa-miR-155-5p", "Macrophages", "HSPB1",
  "hsa-miR-155-5p", "Monocytes", "FKBP4",
  "hsa-miR-155-5p", "Monocytes", "HSPB1",
  "hsa-miR-155-5p", "Myelocyte", "EGR1",
  "hsa-miR-155-5p", "Myelocyte", "ZC3HAV1",
  "hsa-miR-155-5p", "SCT", "HSPA6",
  "hsa-miR-155-5p", "SCT", "METRNL",
  "hsa-miR-155-5p", "T/NK cell", "HSPA6",
  "hsa-miR-155-5p", "T/NK cell", "FKBP4",
  "hsa-miR-155-5p", "VCT", "IL6",
  "hsa-miR-155-5p", "VCT", "IFIT2",
  
  "hsa-miR-222-3p", "EVT", "IFIT2",
  "hsa-miR-222-3p", "EVT", "ICAM1",
  "hsa-miR-222-3p", "Granulocyte", "IFIT2",
  "hsa-miR-222-3p", "Granulocyte", "TNFSF10",
  "hsa-miR-222-3p", "Macrophages", "CXCL1",
  "hsa-miR-222-3p", "Macrophages", "RHOB",
  "hsa-miR-222-3p", "Monocytes", "FKBP4",
  "hsa-miR-222-3p", "Monocytes", "MYADM",
  "hsa-miR-222-3p", "Myelocyte", "EGR1",
  "hsa-miR-222-3p", "Myelocyte", "JUN",
  "hsa-miR-222-3p", "T/NK cell", "FKBP4",
  "hsa-miR-222-3p", "VCT", "IL6",
  "hsa-miR-222-3p", "VCT", "STAT4"
)

celltype_order_targets <- c(
  "EVT",
  "Granulocyte",
  "Macrophages",
  "Monocytes",
  "Myelocyte",
  "SCT",
  "T/NK cell",
  "VCT",
  "Endothelial"
)

make_target_plot <- function(mir, title_text) {
  df <- validated_target_degs %>%
    dplyr::filter(miRNA == mir) %>%
    dplyr::mutate(
      Cell_type = factor(Cell_type, levels = celltype_order_targets)
    ) %>%
    dplyr::left_join(
      selected_labels %>%
        dplyr::mutate(label = gene),
      by = c("miRNA", "Cell_type", "gene")
    )
  
  ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = Cell_type,
      y = log2FoldChange,
      color = Cell_type
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = 2,
      linewidth = 0.5
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_jitter(
        width = 0.12,
        height = 0,
        seed = 123
      ),
      size = 3,
      alpha = 0.82
    ) +
    ggrepel::geom_text_repel(
      data = df %>% dplyr::filter(!is.na(label)),
      ggplot2::aes(label = label),
      family = "Arial",
      fontface = "italic",
      size = 5.2,
      color = "black",
      box.padding = 0.45,
      point.padding = 0.3,
      max.overlaps = Inf,
      seed = 123,
      show.legend = FALSE
    ) +
    ggplot2::scale_color_manual(
      values = celltype_colors,
      drop = FALSE
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(
      x = NULL,
      y = expression(log[2] ~ "fold change (GDM / CTR)"),
      title = title_text
    ) +
    publication_theme(15) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 19, face = "bold", hjust = 0.5),
      axis.title.y = ggplot2::element_text(size = 16, margin = ggplot2::margin(r = 3)),
      axis.text.y = ggplot2::element_text(size = 14, color = "black"),
      axis.text.x = ggplot2::element_text(size = 13, color = "black", angle = 45, hjust = 1),
      legend.position = "none"
    )
}

p_mir29 <- make_target_plot("hsa-miR-29a-3p", "miR-29a-3p")
p_mir132 <- make_target_plot("hsa-miR-132-3p", "miR-132-3p")
p_mir150 <- make_target_plot("hsa-miR-150-5p", "miR-150-5p")
p_mir155 <- make_target_plot("hsa-miR-155-5p", "miR-155-5p")
p_mir222 <- make_target_plot("hsa-miR-222-3p", "miR-222-3p")

fig_main <-
  (p_umap_celltype_final + ggplot2::labs(tag = "A") |
     p_mir29 + ggplot2::labs(tag = "B")) /
  (p_mir132 + ggplot2::labs(tag = "C") |
     p_mir150 + ggplot2::labs(tag = "D")) /
  (p_mir155 + ggplot2::labs(tag = "E") |
     p_mir222 + ggplot2::labs(tag = "F")) &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(size = 23, face = "bold", family = "Arial")
  )

save_plot_all(
  fig_main,
  file.path(output_dirs$figures, "Figure_2_FINAL"),
  width = 16,
  height = 20
)

saveRDS(
  fig_main,
  file.path(output_dirs$rds, "Figure_2_FINAL.rds")
)


# ============================================================
# 15. FINAL SUPPLEMENTARY FIGURE
# A QC retention
# B UMAP by donor
# C Cell-type composition
# D Global pseudobulk volcano
# E Canonical marker DotPlot
# F miRNA GO BP
# ============================================================

# 15A. QC retention
qc_table_supp <- utils::read.csv(
  file.path(output_dirs$qc, "QC_summary.csv"),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

if ("After_MT" %in% colnames(qc_table_supp) &&
    !"After_MT_filter" %in% colnames(qc_table_supp)) {
  qc_table_supp <- qc_table_supp %>%
    dplyr::rename(After_MT_filter = After_MT)
}

if ("After_MAD" %in% colnames(qc_table_supp) &&
    !"After_MAD_filter" %in% colnames(qc_table_supp)) {
  qc_table_supp <- qc_table_supp %>%
    dplyr::rename(After_MAD_filter = After_MAD)
}

qc_long_supp <- qc_table_supp %>%
  dplyr::select(
    Sample,
    Group,
    Initial_cells,
    After_MT_filter,
    After_MAD_filter,
    Final_singlets
  ) %>%
  tidyr::pivot_longer(
    cols = -c(Sample, Group),
    names_to = "Stage",
    values_to = "Cells"
  ) %>%
  dplyr::mutate(
    Stage = factor(
      Stage,
      levels = c(
        "Initial_cells",
        "After_MT_filter",
        "After_MAD_filter",
        "Final_singlets"
      ),
      labels = c(
        "Initial",
        "After MT filter",
        "After MAD filter",
        "After doublet removal"
      )
    )
  )

p_qc_supp <- ggplot2::ggplot(
  qc_long_supp,
  ggplot2::aes(
    x = Stage,
    y = Cells,
    group = Sample,
    shape = Sample
  )
) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 4) +
  ggplot2::facet_wrap(~ Sample, scales = "free_y") +
  ggplot2::labs(
    x = NULL,
    y = "Number of cells",
    title = "Cell retention during quality control"
  ) +
  publication_theme(15) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.y = ggplot2::element_text(size = 17, margin = ggplot2::margin(r = 3)),
    axis.text.y = ggplot2::element_text(size = 14, color = "black"),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1, size = 12.5, color = "black"),
    strip.text = ggplot2::element_text(size = 16, face = "bold"),
    legend.position = "none"
  )

# 15B. UMAP by donor; existing UMAP coordinates are used.
p_umap_sample_supp <- Seurat::DimPlot(
  Merge,
  reduction = "umap",
  group.by = "orig.ident",
  raster = FALSE
) +
  ggplot2::ggtitle("Samples") +
  publication_theme(15) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = ggplot2::element_text(size = 17, margin = ggplot2::margin(t = 3)),
    axis.title.y = ggplot2::element_text(size = 17, margin = ggplot2::margin(r = 3)),
    axis.text = ggplot2::element_text(size = 14, color = "black"),
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(size = 14),
    legend.position = "bottom"
  )

# 15C. Cell-type composition by donor
cell_counts_supp <- as.data.frame(
  table(Merge$Cell_type, Merge$orig.ident)
) %>%
  dplyr::rename(
    Cell_type = Var1,
    Sample = Var2,
    Cells = Freq
  )

cell_props_supp <- cell_counts_supp %>%
  dplyr::group_by(Sample) %>%
  dplyr::mutate(
    Percent = 100 * Cells / sum(Cells)
  ) %>%
  dplyr::ungroup()

p_prop_supp <- ggplot2::ggplot(
  cell_props_supp,
  ggplot2::aes(
    x = Sample,
    y = Percent,
    fill = Cell_type
  )
) +
  ggplot2::geom_col(width = 0.8) +
  ggplot2::scale_fill_manual(values = celltype_colors, drop = FALSE) +
  ggplot2::labs(
    x = NULL,
    y = "Cells (%)",
    fill = NULL,
    title = "Cell-type composition by donor"
  ) +
  publication_theme(15) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.y = ggplot2::element_text(size = 17, margin = ggplot2::margin(r = 3)),
    axis.text = ggplot2::element_text(size = 14, color = "black"),
    legend.text = ggplot2::element_text(size = 13),
    legend.position = "right"
  )

# 15D. Global pseudobulk volcano
volcano_df <- res_global %>%
  dplyr::mutate(
    padj_plot = ifelse(is.na(padj), 1, pmax(padj, 1e-300)),
    Status = dplyr::case_when(
      padj < 0.05 & log2FoldChange >= 1 ~ "Up in GDM",
      padj < 0.05 & log2FoldChange <= -1 ~ "Down in GDM",
      TRUE ~ "Not significant"
    ),
    neglog10 = -log10(padj_plot)
  )

volcano_labels <- volcano_df %>%
  dplyr::filter(Status != "Not significant") %>%
  dplyr::arrange(padj_plot) %>%
  dplyr::slice_head(n = 12)

p_vol_global_supp <- ggplot2::ggplot(
  volcano_df,
  ggplot2::aes(x = log2FoldChange, y = neglog10)
) +
  ggplot2::geom_point(
    ggplot2::aes(shape = Status),
    alpha = 0.75,
    size = 2.3
  ) +
  ggplot2::geom_vline(
    xintercept = c(-1, 1),
    linetype = 2,
    linewidth = 0.5
  ) +
  ggplot2::geom_hline(
    yintercept = -log10(0.05),
    linetype = 2,
    linewidth = 0.5
  ) +
  ggrepel::geom_text_repel(
    data = volcano_labels,
    ggplot2::aes(label = gene),
    size = 5,
    fontface = "italic",
    box.padding = 0.5,
    point.padding = 0.25,
    max.overlaps = Inf,
    seed = 123,
    show.legend = FALSE
  ) +
  ggplot2::labs(
    x = expression(log[2] ~ "fold change (GDM / CTR)"),
    y = expression(-log[10] ~ "FDR"),
    title = "Global pseudobulk differential expression",
    shape = NULL
  ) +
  publication_theme(15) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.x = ggplot2::element_text(size = 17, margin = ggplot2::margin(t = 3)),
    axis.title.y = ggplot2::element_text(size = 17, margin = ggplot2::margin(r = 3)),
    axis.text = ggplot2::element_text(size = 14, color = "black"),
    legend.text = ggplot2::element_text(size = 13),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(t = 12, r = 8, b = 8, l = 14)
  )

# 15E. Canonical marker DotPlot
genes_dotplot <- c(
  "CDH1", "MET",
  "CYP19A1", "CGA",
  "HLA-G", "PAPPA2",
  "FCGR3B", "CSF3R",
  "MPO", "TCN1",
  "CD3D", "CD3E", "TRBC1", "NKG7", "GNLY",
  "CD79A", "MS4A1",
  "CD14", "FCN1",
  "CD68", "C1QA",
  "PECAM1", "VWF", "KDR"
)

genes_dotplot <- intersect(genes_dotplot, rownames(Merge))

p_dotplot_supp <- Seurat::DotPlot(
  Merge,
  features = genes_dotplot,
  group.by = "seurat_clusters"
) +
  Seurat::RotatedAxis() +
  ggplot2::labs(
    x = NULL,
    y = "Seurat cluster",
    title = "Canonical markers used for cell-type annotation"
  ) +
  publication_theme(15) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title.y = ggplot2::element_text(size = 17, margin = ggplot2::margin(r = 3)),
    axis.text.x = ggplot2::element_text(size = 15, color = "black", angle = 45, hjust = 1),
    axis.text.y = ggplot2::element_text(size = 14, color = "black"),
    legend.title = ggplot2::element_text(size = 14),
    legend.text = ggplot2::element_text(size = 13)
  )

# 15F. GO BP of validated miRNA target-DEGs
if (!exists("p_go_mirna") || nrow(go_mirna_top) == 0) {
  stop("p_go_mirna was not available; check section 13B.")
}

go_order <- go_mirna_top %>%
  dplyr::group_by(Description) %>%
  dplyr::summarise(best_FDR = min(p.adjust, na.rm = TRUE), .groups = "drop") %>%
  dplyr::arrange(best_FDR) %>%
  dplyr::pull(Description)

go_mirna_top$Description <- factor(
  go_mirna_top$Description,
  levels = rev(go_order)
)

p_go_supp <- ggplot2::ggplot(
  go_mirna_top,
  ggplot2::aes(x = miRNA_short, y = Description)
) +
  ggplot2::geom_point(
    ggplot2::aes(
      size = GeneRatio_numeric,
      fill = minus_log10_FDR
    ),
    shape = 21,
    color = "black",
    stroke = 0.45
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "C",
    name = expression(-log[10] ~ "FDR")
  ) +
  ggplot2::scale_size_continuous(
    name = "Gene ratio",
    range = c(3, 8)
  ) +
  ggplot2::labs(
    x = NULL,
    y = NULL,
    title = "GO Biological Process enrichment"
  ) +
  publication_theme(15) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 20, face = "bold", hjust = 0.5),
    axis.text.x = ggplot2::element_text(size = 14, angle = 40, hjust = 1, color = "black"),
    axis.text.y = ggplot2::element_text(size = 14, color = "black", margin = ggplot2::margin(r = 2)),
    legend.title = ggplot2::element_text(size = 14),
    legend.text = ggplot2::element_text(size = 13),
    legend.position = "right"
  )

supp_A <- p_qc_supp + ggplot2::labs(tag = "A")
supp_B <- p_umap_sample_supp + ggplot2::labs(tag = "B")
supp_C <- p_prop_supp + ggplot2::labs(tag = "C")
supp_D <- p_vol_global_supp +
  ggplot2::labs(tag = "D") +
  ggplot2::theme(plot.tag.position = c(-0.02, 1.02))
supp_E <- p_dotplot_supp + ggplot2::labs(tag = "E")
supp_F <- p_go_supp + ggplot2::labs(tag = "F")

supp_top <- supp_A | supp_B
supp_middle <- supp_C | supp_D

fig_supplementary <-
  supp_top /
  supp_middle /
  supp_E /
  supp_F +
  patchwork::plot_layout(
    heights = c(1, 1, 1.10, 0.90)
  ) &
  ggplot2::theme(
    plot.tag = ggplot2::element_text(
      size = 24,
      face = "bold",
      family = "Arial"
    )
  )

save_plot_all(
  fig_supplementary,
  file.path(output_dirs$figures, "Supplementary_Figure_scRNA_FINAL"),
  width = 18,
  height = 24
)

saveRDS(
  fig_supplementary,
  file.path(output_dirs$rds, "Supplementary_Figure_scRNA_FINAL.rds")
)


# ============================================================
# 16. FINAL EXCEL WORKBOOK
# Robust export: all tables are assembled first, written in one
# operation, then reopened and checked for row counts.
# ============================================================

qc_excel <- qc_table_supp

cluster_annotation_excel <- as.data.frame(
  table(Merge$seurat_clusters, Merge$Cell_type)
) %>%
  dplyr::rename(
    Seurat_cluster = Var1,
    Cell_type = Var2,
    Cells = Freq
  ) %>%
  dplyr::filter(Cells > 0) %>%
  dplyr::arrange(
    suppressWarnings(as.numeric(as.character(Seurat_cluster))),
    dplyr::desc(Cells)
  )

cell_counts_excel <- as.data.frame(
  table(Merge$orig.ident, Merge$Cell_type)
) %>%
  dplyr::rename(
    Sample = Var1,
    Cell_type = Var2,
    Cells = Freq
  ) %>%
  dplyr::arrange(Sample, Cell_type)

cell_proportions_excel <- cell_counts_excel %>%
  dplyr::group_by(Sample) %>%
  dplyr::mutate(
    Percent = 100 * Cells / sum(Cells)
  ) %>%
  dplyr::ungroup()

pb_global_fdr_excel <- res_global %>%
  dplyr::filter(!is.na(padj), padj < 0.05) %>%
  dplyr::arrange(padj, pvalue)

pb_celltype_fdr_excel <- pb_celltype_all %>%
  dplyr::filter(!is.na(padj), padj < 0.05) %>%
  dplyr::arrange(Cell_type, padj, pvalue)

permutation_excel <- perm_stats %>%
  dplyr::filter(Target_type == "Validated") %>%
  dplyr::arrange(Analysis, Threshold, Direction, miRNA)

permutation_global_excel <- permutation_excel %>%
  dplyr::filter(Analysis == "Global") %>%
  dplyr::arrange(Threshold, Direction, p_BH, p_perm)

permutation_bh005_excel <- permutation_excel %>%
  dplyr::filter(
    !is.na(p_BH_across_celltypes),
    p_BH_across_celltypes < 0.05
  ) %>%
  dplyr::arrange(Threshold, p_BH_across_celltypes, p_perm)

permutation_fdr_bh005_excel <- permutation_excel %>%
  dplyr::filter(
    Threshold == "FDR005",
    !is.na(p_BH_across_celltypes),
    p_BH_across_celltypes < 0.05
  ) %>%
  dplyr::arrange(p_BH_across_celltypes, p_perm)

permutation_overlap_excel <- perm_overlaps %>%
  dplyr::filter(Target_type == "Validated") %>%
  dplyr::arrange(Analysis, Threshold, Direction, miRNA, gene)

# Make every Excel sheet explicitly non-empty and plain-data-frame safe.
excel_safe_table <- function(x, note_if_empty) {
  if (is.null(x) || ncol(x) == 0 || nrow(x) == 0) {
    return(data.frame(Note = note_if_empty, stringsAsFactors = FALSE))
  }
  
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  
  for (nm in names(x)) {
    if (is.factor(x[[nm]])) {
      x[[nm]] <- as.character(x[[nm]])
    }
    if (is.list(x[[nm]])) {
      x[[nm]] <- vapply(
        x[[nm]],
        function(z) paste(as.character(z), collapse = "; "),
        character(1)
      )
    }
  }
  
  x
}

n_global_perm_bh <- sum(permutation_global_excel$p_BH < 0.05, na.rm = TRUE)

analysis_summary <- data.frame(
  Sheet = c(
    "QC",
    "Cluster_annotation",
    "Cell_counts",
    "Cell_proportions",
    "PB_Global_FDR005",
    "PB_CellType_summary",
    "PB_CellType_FDR005",
    "GO_CellType_BP",
    "miRNA_validated",
    "miRNA_target_DEGs",
    "miRNA_permutation",
    "miRNA_perm_Global",
    "miRNA_perm_BH005",
    "miRNA_perm_FDR_BH005",
    "miRNA_perm_overlap",
    "miRNA_GO_BP"
  ),
  Contents = c(
    "Quality-control retention and doublet-removal statistics by donor.",
    "Relationship between Seurat clusters and final broad placental cell-type annotations.",
    "Number of cells in each cell type for each donor.",
    "Percentage of cells in each cell type for each donor.",
    "Significant genes from global donor-level pseudobulk differential-expression analysis.",
    "Summary of cell-type-specific donor-level pseudobulk analyses.",
    "Significant genes from cell-type-specific donor-level pseudobulk analyses.",
    "Significant GO Biological Process enrichment from cell-type-specific DEGs.",
    "Experimentally validated miRNA-target pairs retrieved through multiMiR.",
    "Experimentally validated miRNA targets that are cell-type-specific DEGs.",
    "Complete validated-target permutation statistics for Global + all testable cell types.",
    "Global-only validated-target permutation statistics.",
    "Permutation results significant after BH correction across analyses + miRNAs within each threshold/direction family.",
    "FDR005 permutation results significant after BH correction across analyses + miRNAs within each direction family.",
    "Genes contributing to observed overlaps in the permutation analysis.",
    "GO Biological Process enrichment of differentially expressed validated miRNA targets."
  ),
  Criterion = c(
    "MT filter; MAD filter; scDblFinder doublet removal.",
    "Broad annotation based on canonical placental and immune-cell markers.",
    "Post-QC final singlets.",
    "Post-QC final singlets; percentage calculated within each donor.",
    "DESeq2 BH-adjusted P value (FDR) < 0.05.",
    "Donor-level pseudobulk by cell type.",
    "DESeq2 BH-adjusted P value (FDR) < 0.05.",
    "Input DEGs: DESeq2 FDR < 0.05; GO BP BH-FDR < 0.05; Up and Down analyzed separately.",
    "Experimentally validated interactions only.",
    "Validated targets intersected with cell-type DEGs at DESeq2 FDR < 0.05.",
    "10,000 permutations; FDR005 and nominal P < 0.05 DEG sets retained as separate thresholds.",
    paste0(
      "10,000 permutations; p_BH corrects the five miRNAs within each global threshold/direction family. Global p_BH < 0.05 results: ",
      n_global_perm_bh,
      "."
    ),
    "p_BH_across_celltypes < 0.05 within Target_type x Threshold x Direction families.",
    "Threshold = FDR005 and p_BH_across_celltypes < 0.05.",
    "Observed overlap genes corresponding to the permutation analyses.",
    "Validated target-DEGs at DESeq2 FDR < 0.05; Up and Down pooled; miRNA-specific validated-target background; GO BP BH-FDR < 0.05."
  ),
  stringsAsFactors = FALSE
)

final_tables <- list(
  Analysis_summary = excel_safe_table(analysis_summary, "No summary available."),
  QC = excel_safe_table(qc_excel, "No QC results available."),
  Cluster_annotation = excel_safe_table(cluster_annotation_excel, "No cluster annotation available."),
  Cell_counts = excel_safe_table(cell_counts_excel, "No cell counts available."),
  Cell_proportions = excel_safe_table(cell_proportions_excel, "No cell proportions available."),
  PB_Global_FDR005 = excel_safe_table(pb_global_fdr_excel, "No global DEGs at FDR < 0.05."),
  PB_CellType_summary = excel_safe_table(pb_summary, "No cell-type pseudobulk summary available."),
  PB_CellType_FDR005 = excel_safe_table(pb_celltype_fdr_excel, "No cell-type DEGs at FDR < 0.05."),
  GO_CellType_BP = excel_safe_table(go_bp_results, "No significant cell-type GO BP terms."),
  miRNA_validated = excel_safe_table(validated_target_pairs, "No validated miRNA targets available."),
  miRNA_target_DEGs = excel_safe_table(validated_target_degs, "No validated target-DEGs at FDR < 0.05."),
  miRNA_permutation = excel_safe_table(permutation_excel, "No permutation results available."),
  miRNA_perm_Global = excel_safe_table(permutation_global_excel, "No global permutation results available."),
  miRNA_perm_BH005 = excel_safe_table(permutation_bh005_excel, "No permutation result passed p_BH_across_celltypes < 0.05."),
  miRNA_perm_FDR_BH005 = excel_safe_table(permutation_fdr_bh005_excel, "No FDR005 permutation result passed p_BH_across_celltypes < 0.05."),
  miRNA_perm_overlap = excel_safe_table(permutation_overlap_excel, "No permutation overlap genes available."),
  miRNA_GO_BP = excel_safe_table(go_mirna_results, "No significant miRNA GO BP terms.")
)

# Print row counts before writing so a blank object is immediately visible.
message("\nRows prepared for final Excel:")
print(vapply(final_tables, nrow, integer(1)))

header_style <- openxlsx::createStyle(
  fontName = "Arial",
  fontSize = 11,
  textDecoration = "bold",
  fontColour = "#FFFFFF",
  fgFill = "#4472C4",
  halign = "center",
  valign = "center",
  wrapText = TRUE,
  border = "Bottom",
  borderColour = "#1F1F1F"
)

excel_path <- file.path(
  output_dirs$excel,
  "GSE173193_ESSENTIAL_RESULTS_FINAL.xlsx"
)

# Write ALL data in one operation. This avoids the previous workbook state
# in which sheet names were created but data were not persisted correctly.
openxlsx::write.xlsx(
  final_tables,
  file = excel_path,
  asTable = TRUE,
  overwrite = TRUE,
  headerStyle = header_style
)

# Reopen only for formatting.
wb <- openxlsx::loadWorkbook(excel_path)

for (sh in names(final_tables)) {
  openxlsx::freezePane(wb, sh, firstRow = TRUE)
  openxlsx::setColWidths(
    wb,
    sh,
    cols = seq_len(ncol(final_tables[[sh]])),
    widths = "auto"
  )
}

# Prevent very wide text columns.
openxlsx::setColWidths(wb, "Analysis_summary", cols = 1, widths = 24)
openxlsx::setColWidths(wb, "Analysis_summary", cols = 2, widths = 55)
openxlsx::setColWidths(wb, "Analysis_summary", cols = 3, widths = 65)

for (sh in intersect(c("GO_CellType_BP", "miRNA_GO_BP"), names(final_tables))) {
  dat <- final_tables[[sh]]
  if ("Description" %in% colnames(dat)) {
    openxlsx::setColWidths(
      wb,
      sh,
      cols = which(colnames(dat) == "Description"),
      widths = 45
    )
  }
}

openxlsx::saveWorkbook(
  wb,
  excel_path,
  overwrite = TRUE
)

# Verify that the saved workbook contains rows in every sheet.
excel_rows_written <- vapply(
  names(final_tables),
  function(sh) {
    x <- openxlsx::read.xlsx(
      excel_path,
      sheet = sh,
      check.names = FALSE
    )
    nrow(x)
  },
  integer(1)
)

message("\nRows read back from saved Excel:")
print(excel_rows_written)

if (any(excel_rows_written == 0)) {
  stop(
    "At least one Excel sheet was saved with zero rows. Check the row-count output above."
  )
}

message("\nFinal Excel saved and verified: ", excel_path)


# ============================================================
# 17. SAVE FINAL R OBJECTS
# ============================================================

saveRDS(
  Merge,
  file.path(output_dirs$rds, "GSE173193_FINAL_anotado.rds")
)

saveRDS(
  pb_celltype_results,
  file.path(output_dirs$rds, "DESeq2_CellType_results_list.rds")
)

saveRDS(
  pb_celltype_genes_tested,
  file.path(output_dirs$rds, "DESeq2_CellType_genes_tested.rds")
)

saveRDS(
  validated_target_degs,
  file.path(output_dirs$rds, "Validated_miRNA_target_DEGs_FDR005.rds")
)

saveRDS(
  go_mirna_results,
  file.path(output_dirs$rds, "GO_miRNA_BP_FDR005.rds")
)

saveRDS(
  perm_stats,
  file.path(output_dirs$rds, "Permutation_ALL_celltypes.rds")
)


# ============================================================
# 18. SESSION INFO + PACKAGE VERSIONS
# ============================================================

writeLines(
  capture.output(sessionInfo()),
  file.path(output_dirs$session, "sessionInfo.txt")
)

pkg_names <- c(
  "Seurat",
  "SeuratObject",
  "DESeq2",
  "scDblFinder",
  "multiMiR",
  "clusterProfiler",
  "org.Hs.eg.db",
  "dplyr",
  "ggplot2",
  "openxlsx"
)

pkg_versions <- data.frame(
  Package = pkg_names,
  Version = vapply(
    pkg_names,
    function(p) {
      if (requireNamespace(p, quietly = TRUE)) {
        as.character(packageVersion(p))
      } else {
        NA_character_
      }
    },
    character(1)
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  pkg_versions,
  file.path(output_dirs$session, "package_versions.csv"),
  row.names = FALSE
)


# ============================================================
# 19. FINAL SANITY CHECKS
# ============================================================

message("\n============================================================")
message("ANALYSIS FINISHED")
message("Output folder: ", out_dir)
message("Final singlets: ", ncol(Merge))
message("Global genes tested: ", nrow(res_global))
message("Global DEGs FDR < 0.05: ", sum(res_global$padj < 0.05, na.rm = TRUE))
message("Validated target-DEG rows: ", nrow(validated_target_degs))
message("Global permutation BH < 0.05: ", sum(perm_global$p_BH < 0.05, na.rm = TRUE))
message("Excel workbook: ", excel_path)
message("============================================================\n")

# ============================================================
# END
# ============================================================
