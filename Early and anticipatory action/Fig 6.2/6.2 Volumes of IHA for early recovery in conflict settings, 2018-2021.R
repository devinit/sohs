suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

source("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R")

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

fts <- fts_curated_flows(years = 2018:2021, update = NA, dataset_path = "reference_datasets", base_year = 2020)

#Assign a sector from available fields
fts[, sector := destinationObjects_GlobalCluster.name]

#Split rows into individual sectors where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD_defl", split.col = "sector", split.pattern = "; ", remove.unsplit = T)

#Merge custom cluster mapping
cluster.map <- fread("reference_datasets/appeal_cluster_mapping.csv", encoding = "UTF-8")
cluster.map[, `:=` (caps_sector = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)
fts[, caps_sector := toupper(sector)]
fts <- merge(fts, cluster.map, by = "caps_sector", all.x = T, sort = F)[, caps_sector := NULL]

#Keyword searching
major.keywords <- c(
  "early recovery",
  "early response",
  "immediate recovery",
  "immediate response",
  "rapid recovery",
  "rapid response"
)

fts$relevance <- "None"

#Assign relevance for 'Early recovery' sector
fts[`Global cluster` == "Early Recovery"]$relevance <- "Major: Sector"

#We prevent keywords from identifying multi-sector projects for which a partial split has already been identified by the global sector
fts[!(id %in% fts[`Global cluster` == "Early Recovery"]$id) & grepl(tolower(paste(major.keywords, collapse = "|")), tolower(paste(fts$description, fts$keywords)))]$relevance <- "Major: Keyword"

fts[, keywordcount := unlist(lapply(description, function(x) sum(gregexpr(tolower(paste0(major.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]

fts_output <- fts[relevance != "None"]

fwrite(fts_output, "Early and anticipatory action\\Fig 6.2\\relevant_fts.csv")

#Conflict
hiik <- fread("reference_datasets/HIIK_CoBa_2020_dataset.csv", encoding = "UTF-8")
countrynames <- fread("reference_datasets/isos.csv", encoding = "UTF-8")
hiik$countries <- gsub(" [(].*| et al[.]| et[.] al|°","", hiik$conflict)
hiik$countries <- gsub(" – ", ", ", hiik$countries)
conflict <- hiik[, .(country = unlist(strsplit(countries, ", "))), by = .(type, intensity_2020, intensity_2019, intensity_2018)][, .(`2020` = max(intensity_2020, na.rm = T), `2019` = max(intensity_2019, na.rm = T), `2018` = max(intensity_2018, na.rm = T)), by = .(country, type)]
conflict <- merge(conflict, countrynames[,c("iso3", "countryname_hiik")], by.x = "country", by.y = "countryname_hiik", all.x = T)
conflict <- melt(conflict, id.vars = c("iso3", "country", "type"))
max.country.conflict <- conflict[, .SD[which.max(value)], by = .(country, iso3, variable)]
non.max.country.conflict <- conflict[, .SD[!which.max(value)], by = .(country, iso3, variable)]
max.country.conflict <- unique(max.country.conflict[value >= 4])
non.max.country.conflict <- unique(non.max.country.conflict[value >= 4])
conflict <- rbind(max.country.conflict, non.max.country.conflict, fill = T)
conflict_isos <- unique(conflict[, .(iso3, year = as.character(variable))])
conflict_isos_2021 <- conflict_isos[year == 2020]
conflict_isos <- rbind(conflict_isos, conflict_isos_2021[, year := 2021])
rm(list = c("hiik", "conflict"))
#

fts_output[, conflict := ifelse(paste0(iso3,year) %in% paste0(conflict_isos$iso3, conflict_isos$year), "Conflict", "Non-conflict")]

fts_agg <- fts_output[!is.na(iso3), .(ea_disb = sum(amountUSD_defl, na.rm = T)), by = .(year, recipient, conflict)][order(year)]

fwrite(fts_agg, "Early and anticipatory action\\Fig 6.2\\Early action conflict settings.csv")
