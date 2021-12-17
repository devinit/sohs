lapply(c("data.table", "rstudioapi"), require, character.only = T)

setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

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

#Split rows into individual recipients where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "destinationObjects_Location.name", split.pattern = "; ", remove.unsplit = T)

cluster.map <- fread("reference_datasets/appeal_cluster_mapping.csv", encoding = "UTF-8")

cluster.map[, `:=` (caps_sector = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)

fts[, caps_sector := toupper(sector)]

fts <- merge(fts, cluster.map, by = "caps_sector", all.x = T)[, caps_sector := NULL]

major.keywords <- c(
  "early recovery",
  "early response",
  "immediate recovery",
  "immediate response",
  "rapid recovery",
  "rapid response"
)

# disqualifying.keywords <- c(
#   ""
# )

fts$relevance <- "None"
fts[`Global cluster` == "Early Recovery"]$relevance <- "Major: Sector"

#We prevent keywords from identifying multi-sector projects for which a partial split has already been identified by the global sector
fts[!(id %in% fts[`Global cluster` == "Early Recovery"]$id) & grepl(tolower(paste(major.keywords, collapse = "|")), tolower(paste(fts$description, fts$keywords)))]$relevance <- "Major: Keyword"

#fts$check <- "No"
#fts[relevance != "None"][grepl(tolower(paste(disqualifying.keywords, collapse = "|")), tolower(paste(fts[relevance != "None"]$ProjectTitle, fts[relevance != "None"]$description, fts[relevance != "None"]$description)))]$check <- "potential false negative"

fts[, keywordcount := unlist(lapply(description, function(x) sum(gregexpr(tolower(paste0(major.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]
#fts[, disqkeywordcount := unlist(lapply(description, function(x) sum(gregexpr(tolower(paste0(disqualifying.keywords, collapse = "|")), x)[[1]] > 0, na.rm = T)))]

fts_output <- fts[relevance != "None"]

#Split rows into individual donors where multiple are recorded
fts_output <- fts_split_rows(fts_output, value.cols = "amountUSD", split.col = "sourceObjects_Organization.id", split.pattern = "; ", remove.unsplit = T)

fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
fts_orgs[, `:=` (type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), location = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name)), by = id]

fts_orgs_gov <- fts_orgs[type == "Government", .(sourceObjects_Organization.id = id, donor_country = location)]

#Manual development agency locations
fts_orgs_gov <- rbind(fts_orgs_gov,
                      data.table(sourceObjects_Organization.id = c("9946", "10399", "4058", "2987", "30", "6547"),
                                 donor_country = c("France", "Qatar", "United States", "Germany", "United Arab Emirates", "Taiwan, Province of China"))
)

#Merge orgs types for deflators
fts_output <- merge(fts_output, fts_orgs_gov, by = "sourceObjects_Organization.id", all.x = T)
fts_output[is.na(donor_country), donor_country := "Total DAC"]

#Add year where none recorded at source
fts_output[is.na(year), year := budgetYear]
fts_output <- fts_output[year %in% c(2018:2021)]

deflators <- fread("reference_datasets/usd_deflators_2021WEO.csv", encoding = "UTF-8", header = T)
deflators <- melt.data.table(deflators, id.vars = c("name", "ISO3"))
deflators <- deflators[, .(donor_country = name, year = as.character(variable), deflator = value)]

fts_output <- merge(fts_output, deflators, by = c("donor_country", "year"), all.x = T)
fts_output[is.na(deflator)]$deflator <- merge(fts_output[is.na(deflator)][, -"deflator"], deflators[donor_country == "Total DAC", -"donor_country"], by = "year")$deflator
fts_output[, amountUSD_defl := amountUSD/deflator]

fts_output[, `:=` (reportDetails = NULL, childFlowIds = NULL, grandBargainEarmarkingType = NULL, keywords = NULL)]

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

fts_output <- merge(fts_output, countrynames[,c("iso3", "countryname_fts")], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all = T)
fts_output[, conflict := ifelse(paste0(iso3,year) %in% paste0(conflict_isos$iso3, conflict_isos$year), "Conflict", "Non-conflict")]

fts_agg <- fts_output[!is.na(iso3), .(ea_disb = sum(amountUSD_defl, na.rm = T), ea_count = length(id)), by = .(year, conflict)][order(year)]

fwrite(fts_agg, "Early and anticipatory action\\Fig 6.2\\Early action conflict settings.csv")
