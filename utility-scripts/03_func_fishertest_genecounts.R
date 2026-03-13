library(dplyr)
library(tidyr)
library(rlang)

### functions for fisher tests


### prepare gene count
prepare_gene_counts <- function(hmm,
                                phage_col = "Accession",
                                gene_col = "gene_name",
                                lifestyle_col = "lifestyle",
                                cog_col = NULL,                 # set to "COGlet" if present, else NULL
                                temp_label = "Temperate",
                                lytic_label = "Lytic") {
  # Input checks
  stopifnot(is.data.frame(hmm))
  stopifnot(is.character(phage_col) && is.character(gene_col) && is.character(lifestyle_col))
  if(!is.null(cog_col)) stopifnot(is.character(cog_col))
  
  # create symbols for tidy eval
  phage_sym     <- sym(phage_col)
  gene_sym      <- sym(gene_col)
  lifestyle_sym <- sym(lifestyle_col)
  cog_sym       <- if(!is.null(cog_col)) sym(cog_col) else NULL
  
  # 1) Basic cleaning + rename to canonical names
  df <- hmm %>%
    rename(
      phage_id = !!phage_sym,
      gene_name = !!gene_sym,
      lifestyle = !!lifestyle_sym,
      # optionally rename COG if provided
      !!!if(!is.null(cog_sym)) setNames(list(cog_sym), "COGlet") else NULL
    ) %>%
    filter(!is.na(phage_id) & !is.na(lifestyle) & !is.na(gene_name))
  
  # 2) deduplicate to genome x gene presence (optionally including COGlet)
  if(!is.null(cog_sym)) {
    df_genome_gene <- df %>%
      distinct(phage_id, gene_name, lifestyle, COGlet)
  } else {
    df_genome_gene <- df %>%
      distinct(phage_id, gene_name, lifestyle)
  }
  
  # 3) raw copy counts: how many times each gene appears in each phage (pre-dedup)
  raw_copy_counts <- df %>%
    count(phage_id, gene_name, name = "copies")
  
  # 4) totals: number of genomes per lifestyle (count unique genomes)
  totals <- df_genome_gene %>%
    distinct(phage_id, lifestyle) %>%
    count(lifestyle, name = "n_genomes")
  
  total_temp <- totals %>% filter(lifestyle == temp_label)  %>% pull(n_genomes) %>% { if(length(.)==0) 0L else as.integer(.) }
  total_lytic <- totals %>% filter(lifestyle == lytic_label) %>% pull(n_genomes) %>% { if(length(.)==0) 0L else as.integer(.) }
  
  if(total_temp == 0L) stop("No genomes with the Temp lifestyle labels found. Check 'temp_label' or your data.")
  if(total_lytic == 0L) stop("No genomes with the Lytic lifestyle labels found. Check 'lytic_label' or your data.")
  
  # 5) per-gene genome counts by lifestyle (genome-level presence)
  gene_counts <- df_genome_gene %>%
    count(gene_name, lifestyle, name = "n") %>%
    pivot_wider(names_from = lifestyle, values_from = n, values_fill = 0)
  
  # ensure explicit columns for Temperate and Lytic exist (use the labels provided)
  # create canonical columns Temperate and Lytic to match downstream functions
  # If the user used different labels, map them
  # If pivot created columns with the given labels already, copy/rename them
  # We'll try both possibilities
  if(!"Temperate" %in% colnames(gene_counts) && temp_label %in% colnames(gene_counts)) {
    gene_counts <- gene_counts %>% rename(Temperate = !!sym(temp_label))
  }
  if(!"Lytic" %in% colnames(gene_counts) && lytic_label %in% colnames(gene_counts)) {
    gene_counts <- gene_counts %>% rename(Lytic = !!sym(lytic_label))
  }
  
  # if still missing columns, add them as zeros
  if(!"Temperate" %in% colnames(gene_counts)) gene_counts <- gene_counts %>% mutate(Temperate = 0L)
  if(!"Lytic" %in% colnames(gene_counts))    gene_counts <- gene_counts %>% mutate(Lytic = 0L)
  
  # coerce to integer and ensure deterministic column order
  gene_counts <- gene_counts %>%
    mutate(
      Temperate = as.integer(Temperate),
      Lytic = as.integer(Lytic)
    ) %>%
    select(gene_name, Temperate, Lytic, everything())
  
  # return as list for downstream steps
  list(
    gene_counts = gene_counts,
    raw_copy_counts = raw_copy_counts,
    df_genome_gene = df_genome_gene,
    totals = totals,
    total_temp = total_temp,
    total_lytic = total_lytic
  )
}


##### fisher test 

compute_fisher_minimal <- function(df, total_temp, total_lytic) {
  
  df %>%
    rowwise() %>%
    mutate(
      a = as.integer(Temperate),
      c = as.integer(Lytic),
      b = total_temp - a,
      d = total_lytic - c,
      
      # fisher tests
      p_two_sided = fisher.test(
        matrix(c(a, b, c, d), nrow = 2, byrow = TRUE),
        alternative = "two.sided"
      )$p.value,
      
      OR = (a*d)/(b*c),
      # continuity-corrected OR
      OR_cc = ((a + 0.5) * (d + 0.5)) /
        ((b + 0.5) * (c + 0.5)),
      plot_OR = abs(log2(OR_cc))
    ) %>%
    ungroup() %>%
    mutate(
      direction = case_when(
        OR > 1  ~ "Temperate",
        OR < 1  ~ "Lytic",
        TRUE       ~ "Equal"
      ),
      prevalence = Temperate + Lytic
    )
}

