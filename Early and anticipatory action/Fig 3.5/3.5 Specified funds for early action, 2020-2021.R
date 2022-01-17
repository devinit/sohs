suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

source("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R")

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

fts <- fts_curated_flows(years = 2020:2021, update = NA, dataset_path = "reference_datasets")

#Assign a sector from available fields
fts[, sector := destinationObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(sourceObjects_GlobalCluster.name), sector := sourceObjects_GlobalCluster.name]
fts[is.na(sector) & !is.na(destinationObjects_Cluster.name), sector := destinationObjects_Cluster.name]

#Split rows into individual sectors where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD_defl", split.col = "sector", split.pattern = "; ", remove.unsplit = T)

#Merge custom cluster mapping
cluster.map <- fread("reference_datasets/appeal_cluster_mapping.csv", encoding = "UTF-8")
cluster.map[, `:=` (caps_sector = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)
fts[, caps_sector := toupper(sector)]
fts <- merge(fts, cluster.map, by = "caps_sector", all.x = T, sort = F)[, caps_sector := NULL]

#Early Action funds
ea_funds <- c("Central Emergency Response Fund", "Start Network")

fts_ea <- fts[sourceObjects_Organization.name %in% ea_funds]

fts_agg <- fts_ea[, .(amount = sum(amountUSD_defl, na.rm = T)), by = .(year, sourceObjects_Organization.name)]
names(fts_agg)[names(fts_agg) == "sourceObjects_Organization.name"] <- "fund"

#DREF
ifrcgo_appeals <- as.data.table(fromJSON("https://prddsgocdnapi.azureedge.net/api/v2/appeal/?limit=10000")$results)

drefs <- ifrcgo_appeals[atype_display == "DREF" & year(as.Date(start_date)) %in% c(2020,2021)]

drefs_agg <- drefs[, .(fund = "DREF", amount = sum(as.numeric(amount_funded), na.rm = T)), by = year(as.Date(start_date))]

deflators <- fread("reference_datasets/usd_deflators_2021WEO.csv", encoding = "UTF-8", header = T)
deflators <- melt.data.table(deflators, id.vars = c("name", "ISO3"))
deflators <- deflators[, .(donor_country = name, year = as.character(variable), deflator = value)]

xr_defl <- merge(fts[originalCurrency == "CHF", .(xr = mean(exchangeRate, na.rm = T)), by = .(year)], deflators[donor_country == "Switzerland", .(year, deflator)], by = "year")

drefs_agg <- merge(drefs_agg, xr_defl[, year := as.numeric(year)], by = "year")
drefs_agg[, amount := (amount/xr)/deflator]

agg <- rbind(fts_agg, drefs_agg[, .(year, fund, amount)])[order(year)]

fwrite(agg, "Early and anticipatory action/Fig 3.5/Funds for early-ant action.csv")
