lapply(c("data.table", "rstudioapi"), require, character.only = T)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R","https://raw.githubusercontent.com/devinit/gha_automation/main/general/deflators.R"), source)

cluster.map <- fread("reference_datasets/appeal_cluster_mapping.csv", encoding = "UTF-8")
cluster.map[, `:=` (caps_cluster = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)

fts <- fts_curated_flows(years = 2018:2021, update = NA, dataset_path = "reference_datasets", base_year = 2020, dummy_intra_flows = T)
fts[, sector := destinationObjects_GlobalCluster.name]
fts <- fts_split_rows(fts, value.cols = "amountUSD_defl", split.col = "sector", split.pattern = "; ", remove.unsplit = T)

fts[, caps_cluster := toupper(sector)]
fts <- merge(fts, cluster.map, by = "caps_cluster", all.x = T, sort = F)[, caps_cluster := NULL]

fts_clusters_agg <- fts[destinationObjects_Plan.id != "", .(Funding_fts_defl = sum(amountUSD_defl, na.rm = T)), by = .(year, `Global cluster`)]
fts_clusters_agg[is.na(`Global cluster`), `Global cluster` := "Unspecified"]

appeals <- fts_get_appeal_urls(2018:2021)

appeals_list <- list()
for(i in 1:nrow(appeals)){
  year <- appeals[i]$year
  id <- appeals[i]$id
  message(paste0(appeals[i]$plan_name))
  appeals_list[[i]] <- fts_get_appeal_clusters(id, year)
}

appeals_clusters <- rbindlist(appeals_list, fill = T)

appeals_clusters <- appeals_clusters[, lapply(.SD, function(x) as.numeric(gsub(",", "", x))), .SDcols = grep("US[$]", names(appeals_clusters), value = T), by = .(year, plan_name, Cluster)]
appeals_clusters[, coverage := `Funding US$`/`Current requirements US$`]

appeals_clusters[, caps_cluster := toupper(Cluster)]

appeals_clusters <- merge(appeals_clusters, cluster.map, by = "caps_cluster", all.x = T)[, caps_cluster := NULL][order(year, plan_name, `Global cluster`, Cluster)]

appeals_clusters_agg <- appeals_clusters[, .(Requirements_overview = sum(`Current requirements US$`, na.rm = T), Funding_overview = sum(`Funding US$`, na.rm = T)), by = .(year, `Global cluster`)]
deflators <- get_deflators(base_year = 2020, currency = "USD", weo_ver = "Oct2021", approximate_missing = T)[ISO == "DAC"]
appeals_clusters_agg <- merge(appeals_clusters_agg, deflators[, .(year, gdp_defl)], by = "year")
appeals_clusters_agg[, `:=` (Requirements_overview_defl = Requirements_overview/gdp_defl, Funding_overview_defl = Funding_overview/gdp_defl, gdp_defl = NULL)]

appeals_clusters_agg <- merge(appeals_clusters_agg[, year := as.character(year)], fts_clusters_agg, all = T)

setwd(dirname(getActiveDocumentContext()$path))
fwrite(appeals_clusters_agg, "output_appeal_clusters_agg.csv")
