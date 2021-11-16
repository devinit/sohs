lapply(c("data.table", "rstudioapi"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))

source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R")
source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R")

appeals <- data.table(fts_get_appeals())

appeals[, `:=` (year = years[[1]]$year, location = ifelse(is.null(locations[[1]]$name), NA, paste0(locations[[1]]$name[locations[[1]]$adminLevel == 0], collapse = ", ")), type = categories[[1]]$name), by = id]

appeals <- appeals[year %in% 2000:2021, .(id, plan_name = planVersion.name, year, location, type)]

appeals_list <- list()
for(i in 1:nrow(appeals)){
  message(appeals[i]$plan_name)
  appeals_list[[i]] <- fts_get_appeal_totals(appeals[i]$id, appeals[i]$year)
}

appeals_table <- rbindlist(lapply(appeals_list, data.table), fill = T)

appeals_table <- appeals_table[, lapply(.SD, function(x) as.numeric(gsub("US[$]|,", "", x))), by = .(year, plan_name)]
appeals_table[is.na(appeals_table)] <- 0

appeals_table <- merge(appeals, appeals_table, by = c("plan_name", "year"), all = T)

#Choose which appeal types to include
appeals_req <- appeals_table[!is.na(location) & type %in% c("CAP", "Humanitarian response plan", "Flash appeal", "Other"), .(total_funding = sum(`Funded through this plan` + `COVID.Funded through this plan`, na.rm = T), total_requirements = sum(`Total requirements` + `COVID.Total requirements`, na.rm = T)), by = .(location, year)][, requirements_met := total_funding/total_requirements]

#Fig 1.8 Trends in levels of requirements met for countries with 10 consecutive years of appeals, 2012-2021
appeals_req_10ycc_1120 <- appeals_req[year %in% c(2011:2020), .SD[all(2011:2020 %in% year)], by = location][order(location, year)]
appeals_req_10ycc_1220 <- appeals_req[year %in% c(2012:2021), .SD[all(2012:2021 %in% year)], by = location][order(location, year)]

#Fig 1.9 Trends in levels of requirements met over first 5 years of a crisis (by crisis type)
appeals_req_f5ycc <- appeals_req[order(location, year)][, crisis_run := cumsum(c(T, diff(as.numeric(year)) != 1)), by = location][, consecutive_crisis := cumsum(c(T, diff(as.numeric(year)) == 1)), by = .(location, crisis_run)]
appeals_req_f5ycc <- appeals_req_f5ycc[consecutive_crisis <= 5, .SD[5 %in% consecutive_crisis], by = .(location, crisis_run)]

