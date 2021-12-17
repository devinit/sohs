lapply(c("data.table", "rstudioapi"), require, character.only = T)

setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_get_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R"), source)

fts <- fts_get_flows(year = c(2020,2021))

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

#Split rows into individual donors where multiple are recorded
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "sourceObjects_Organization.id", split.pattern = "; ", remove.unsplit = T)

fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
fts_orgs[, `:=` (type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), location = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name)), by = id]

fts_orgs_gov <- fts_orgs[type == "Government", .(sourceObjects_Organization.id = id, donor_country = location)]

#Manual development agency locations
fts_orgs_gov <- rbind(fts_orgs_gov,
                      data.table(sourceObjects_Organization.id = c("9946", "10399", "4058", "2987", "30", "6547"),
                                 donor_country = c("France", "Qatar", "United States", "Germany", "United Arab Emirates", "Taiwan, Province of China"))
)

#Merge orgs types for deflators
fts <- merge(fts, fts_orgs_gov, by = "sourceObjects_Organization.id", all.x = T)
fts[is.na(donor_country), donor_country := "Total DAC"]

#Add year where none recorded at source
fts[is.na(year), year := budgetYear]
fts <- fts[year %in% c(2020:2021)]

deflators <- fread("reference_datasets/usd_deflators_2021WEO.csv", encoding = "UTF-8", header = T)
deflators <- melt.data.table(deflators, id.vars = c("name", "ISO3"))
deflators <- deflators[, .(donor_country = name, year = as.character(variable), deflator = value)]

fts <- merge(fts, deflators, by = c("donor_country", "year"), all.x = T)
fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[donor_country == "Total DAC", -"donor_country"], by = "year")$deflator
fts[, amountUSD_defl := amountUSD/deflator]

ea_funds <- c("Central Emergency Response Fund", "Start Network")

fts_ea <- fts[sourceObjects_Organization.name %in% ea_funds]

fts_agg <- fts_ea[, .(amount = sum(amountUSD_defl, na.rm = T)), by = .(year, sourceObjects_Organization.name)]
names(fts_agg)[names(fts_agg) == "sourceObjects_Organization.name"] <- "fund"

#DREF
ifrcgo_appeals <- as.data.table(fromJSON("https://prddsgocdnapi.azureedge.net/api/v2/appeal/?limit=10000")$results)

drefs <- ifrcgo_appeals[atype_display == "DREF" & year(as.Date(start_date)) %in% c(2020,2021)]

drefs_agg <- drefs[, .(fund = "DREF", amount = sum(as.numeric(amount_funded), na.rm = T)), by = year(as.Date(start_date))]

xr_defl <- merge(fts[originalCurrency == "CHF", .(xr = mean(exchangeRate, na.rm = T)), by = .(year)], deflators[donor_country == "Switzerland", .(year, deflator)], by = "year")

drefs_agg <- merge(drefs_agg, xr_defl[, year := as.numeric(year)], by = "year")
drefs_agg[, amount := (amount/xr)/deflator]

agg <- rbind(fts_agg, drefs_agg[, .(year, fund, amount)])[order(year)]

fwrite(agg, "Early and anticipatory action/Fig 3.5/Funds for early-ant action.csv")
