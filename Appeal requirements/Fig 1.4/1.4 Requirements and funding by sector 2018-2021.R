lapply(c("data.table", "rstudioapi"), require, character.only = T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R","https://raw.githubusercontent.com/devinit/gha_automation/main/general/deflators.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R"), source)

cluster.map <- fread("reference_datasets/appeal_cluster_mapping.csv", encoding = "UTF-8")
cluster.map[, `:=` (caps_cluster = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)

fts <- fts_curated_flows(years = 2018:2021, update = NA, dataset_path = "reference_datasets", base_year = 2020, dummy_intra_flows = T)
fts[, sector := destinationObjects_GlobalCluster.name]
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "sector", split.pattern = "; ", remove.unsplit = T)

fts[, caps_cluster := toupper(sector)]
fts <- merge(fts, cluster.map, by = "caps_cluster", all.x = T, sort = F)[, caps_cluster := NULL]

#fts[grepl(";", sector), `Global cluster` := "Multiple sectors"]

fts_clusters_agg <- fts[destinationObjects_Plan.id != "", .(Funding_fts = sum(amountUSD, na.rm = T)), by = .(year, `Global cluster`, destinationObjects_Plan.id)]
fts_clusters_agg[is.na(`Global cluster`), `Global cluster` := "Unspecified"]

appeals <- fts_get_appeals(2018:2021)

appeals_list <- list()
for(i in 1:nrow(appeals)){
  year <- appeals[i]$years[[1]]['year']
  id <- appeals[i]$id
  message(paste0(appeals[i]$planVersion.name))
  appeals_list[[i]] <- tryCatch(fts_get_appeal_clusters(id, year), error = function(e) NULL)
}

appeals_clusters <- rbindlist(appeals_list, fill = T)

appeals_clusters <- appeals_clusters[, lapply(.SD, function(x) as.numeric(gsub(",", "", x))), .SDcols = grep("US[$]", names(appeals_clusters), value = T), by = .(year, appeal_id, plan_name, Cluster)]

famine_covid <- data.table(year = 2020, appeal_id = 1006, plan_name = "Famine Prevention COVID-19 2020", Cluster = c("Food Security", "Not reported"), `Current requirements US$` = c(500000000,0), `Funding US$` = c(37000000, 0))
gos_covid <- data.table(year = 2020, appeal_id = 996, plan_name = "Global operational support COVID-19", Cluster = c("COVID-19", "Not reported"), `Current requirements US$` = c(376000000,0), `Funding US$` = c(286379700, 2580600))
usng_covid <- data.table(year = 2020, appeal_id = 1007, plan_name = "Unallocated supplementary NGO envelope COVID-19 2020", Cluster = c("Multi-sector", "Not reported", "Multiple Sectors (shared)"), `Current requirements US$` = c(300000000,0, 0), `Funding US$` = c(1469821, 3480124, 1000000))

ghrp_components <- rbind(famine_covid, gos_covid, usng_covid)
appeals_clusters <- rbind(appeals_clusters, ghrp_components, fill = T)

appeals_clusters[, coverage := `Funding US$`/`Current requirements US$`]

appeals_clusters[, caps_cluster := toupper(Cluster)]

appeals_clusters <- merge(appeals_clusters, cluster.map, by = "caps_cluster", all.x = T)[, caps_cluster := NULL][order(year, plan_name, `Global cluster`, Cluster)]

rrps <- appeals[, type := lapply(categories, function(x) x$name)][type == "Regional response plan"]$planVersion.planId

appeals_clusters_agg <- appeals_clusters[, .(`Current requirements US$` = sum(`Current requirements US$`, na.rm = T)), by = .(year, appeal_id, `Global cluster`, plan_name)]#plan_name != "COVID-19 Global Humanitarian Response Plan" & !(appeal_id %in% rrps), .(Requirements_overview = sum(`Current requirements US$`, na.rm = T), Funding_overview = sum(`Funding US$`, na.rm = T)), by = .(year, `Global cluster`)]
# deflators <- get_deflators(base_year = 2020, currency = "USD", weo_ver = "Oct2021", approximate_missing = T)[ISO == "DAC"]
# appeals_clusters_agg <- merge(appeals_clusters_agg, deflators[, .(year, gdp_defl)], by = "year")
# appeals_clusters_agg[, `:=` (Requirements_overview_defl = Requirements_overview/gdp_defl, Funding_overview_defl = Funding_overview/gdp_defl, gdp_defl = NULL)]

appeals_clusters_agg <- merge(appeals_clusters_agg[, year := as.character(year)], fts_clusters_agg, by.x = c("year", "appeal_id", "Global cluster"), by.y = c("year", "destinationObjects_Plan.id", "Global cluster"), all = T)

appeals_clusters_agg <- appeals_clusters_agg[plan_name != "COVID-19 Global Humanitarian Response Plan" & !(appeal_id %in% rrps)]

setwd(dirname(getActiveDocumentContext()$path))
fwrite(appeals_clusters_agg, "output_appeal_clusters_agg_norrps_ap.csv")
