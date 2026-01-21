#### extract and clean daniels sporulation table and gtdb data

spore <- read.csv(here(raw_dir,"gtdb_families_sporulation.csv" ))
spore.fam <- spore[,c(5,6)]
spore.fam$gtdb_f<- gsub('f__', '', spore.fam$gtdb_f) 


search_terms <- unique(spore.fam[,c(1)])

# Function to find first matching family in the host column from dan's spore family list and make a new column with just that family
extract_match <- function(text, terms) {
  match <- str_extract(text, str_c("(?i)", str_c(terms, collapse = "|"))) 
  ifelse(is.na(match), NA, match)
}

gpd <- read.delim2(here(raw_dir, "GPD_metadata.tsv" )) 
gvd <- read.csv(here(raw_dir, "gvd_all_hosts.csv" )) 
gpd.tax <- gpd[,c(1,7)]
gvd.tax <- gvd[,c(1,5)]
## can use this to match to daniel's GTDB spore/nonspore and phagescope output

colnames(gpd.tax) <- c("ID", "GTDB_tax")
colnames(gvd.tax)<- c("ID", "GTDB_tax")

dan.tax <- rbind(gpd.tax, gvd.tax)

dan.tax <- dan.tax %>%
  mutate(matched_word = sapply(GTDB_tax, extract_match, terms = search_terms))

dan.tax$matched_word[is.na(dan.tax$matched_word)] <- "Unknown"

dan.tax <- merge(spore.fam, dan.tax, by.x="gtdb_f", by.y="matched_word")
#write.csv(dan.tax, here(data_dir, "dan_fam_spor.csv"))