lapply(c("data.table", "rstudioapi"), require, character.only = T)

setwd(dirname(getActiveDocumentContext()$path))

lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_get_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R"), source)

fts <- fts_get_flows(year = c(2018,2019,2020,2021))

#Split rows into individual years where multiple are recorded
fts[, year := sourceObjects_UsageYear.name]
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = T)

#Assign a sector from available fields
fts[, sector := destinationObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(sourceObjects_GlobalCluster.name), sector := sourceObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(destinationObjects_Cluster.name), sector := destinationObjects_Cluster.name]

#Split rows into individual sectors where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "sector", split.pattern = "; ", remove.unsplit = T)

cluster.map <- fread("appeal_cluster_mapping.csv", encoding = "UTF-8")

cluster.map[, `:=` (caps_sector = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)

fts[, caps_sector := toupper(sector)]

fts <- merge(fts, cluster.map, by = "caps_sector", all.x = T)[, caps_sector := NULL]

major.keywords <- c(
  "early recovery",
  "early response",
  "immediate recovery",
  "immediate response"
)

disqualifying.keywords <- c(
  ""
)

fts$relevance <- "None"
fts[`Global cluster` == "Early Recovery"]$relevance <- "Major: Sector"

#We prevent keywords from identifying multi-sector projects for which a partial split has already been identified by the global sector
fts[!(id %in% fts[`Global cluster` == "Early Recovery"]$id) & grepl(tolower(paste(major.keywords, collapse = "|")), tolower(paste(fts$description)))]$relevance <- "Major: Keyword"

#fts$check <- "No"
#fts[relevance != "None"][grepl(tolower(paste(disqualifying.keywords, collapse = "|")), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$description, fts[relevance != "None"]$description)))]$check <- "potential false negative"

fts[, keywordcount := unlist(lapply(description, function(x) sum(gregexpr(tolower(paste0(major.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]
#fts[, disqkeywordcount := unlist(lapply(description, function(x) sum(gregexpr(tolower(paste0(disqualifying.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]

fts_output <- fts[relevance != "None"]


