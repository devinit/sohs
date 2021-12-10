lapply(c("data.table", "rstudioapi"), require, character.only = T)

setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R")

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

cluster.map <- fread("reference_datasets/appeal_cluster_mapping.csv", encoding = "UTF-8")

cluster.map[, `:=` (caps_cluster = toupper(Cluster), Cluster = NULL)]
cluster.map <- unique(cluster.map)
appeals_clusters[, caps_cluster := toupper(Cluster)]

appeals_clusters <- merge(appeals_clusters, cluster.map, by = "caps_cluster", all.x = T)[, caps_cluster := NULL][order(year, plan_name, `Global cluster`, Cluster)]

appeals_clusters_agg <- appeals_clusters[, .(Requirements = sum(`Current requirements US$`, na.rm = T), Funding = sum(`Funding US$`, na.rm = T)), by = .(year, `Global cluster`)]

setwd(dirname(getActiveDocumentContext()$path))
fwrite(appeals_clusters_agg, "output_appeal_clusters_agg.csv")
