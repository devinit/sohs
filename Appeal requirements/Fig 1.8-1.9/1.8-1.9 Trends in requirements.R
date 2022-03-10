lapply(c("data.table", "rstudioapi"), require, character.only = T)
setwd(dirname(getActiveDocumentContext()$path))

lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R"
         ,"https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R"
         ,"https://raw.githubusercontent.com/devinit/gha_automation/main/general/deflators.R")
       , source)

appeals <- data.table(fts_get_appeals())

appeals[, `:=` (year = years[[1]]$year, location = ifelse(is.null(locations[[1]]$name), NA, paste0(locations[[1]]$name[locations[[1]]$adminLevel == 0], collapse = ", ")), type = categories[[1]]$name), by = id]

appeals <- appeals[year %in% 2012:2021, .(id, plan_name = planVersion.name, year, location, type)]

appeals_list <- list()
for(i in 1:nrow(appeals)){
  message(appeals[i]$plan_name)
  appeals_list[[i]] <- fts_get_appeal_totals(appeals[i]$id, appeals[i]$year)
}

appeals_table <- rbindlist(lapply(appeals_list, data.table), fill = T)

appeals_table <- appeals_table[, lapply(.SD, function(x) as.numeric(gsub("US[$]|,", "", x))), by = .(year, plan_name)]
appeals_table[is.na(appeals_table)] <- 0

appeals_table <- merge(appeals[, -"year"], appeals_table[, -"plan_name"], by.x = "id", by.y = "appeal_id", all = T)

#Choose which appeal types to include
appeals_req <- appeals_table[!is.na(location) & type %in% c("CAP", "Humanitarian response plan", "Flash appeal", "Other"), .(total_funding = sum(`Funded through this plan` + `COVID.Funded through this plan`, na.rm = T), total_requirements = sum(`Total requirements` + `COVID.Total requirements`, na.rm = T)), by = .(type, location, year)][, requirements_met := total_funding/total_requirements]
appeals_req[type == "CAP", type := "Humanitarian response plan"]

deflators <- get_deflators(base_year = 2020, currency = "USD", weo_ver = "Oct2021", approximate_missing = T)[ISO == "DAC"]
appeals_req <- merge(appeals_req, deflators[, .(year = as.character(year), gdp_defl)])
appeals_req[, `:=` (total_funding_defl = total_funding/gdp_defl, total_requirements_defl = total_requirements/gdp_defl, gdp_defl = NULL)]

#Fig 1.8 Trends in levels of requirements met for countries with 10 consecutive years of appeals, 2012-2021
appeals_req_10ycc_1221 <- appeals_req[year %in% c(2012:2021), .SD[all(2012:2021 %in% year)], by = .(type, location)][order(location, year)]
fwrite(appeals_req_10ycc_1221, "appeals_10yrcons.csv")

#Fig 1.9 Trends in levels of requirements met over first 5 years of a crisis (by crisis type)
appeals_req_f5ycc <- appeals_req[order(location, year)][, crisis_run := cumsum(c(T, diff(as.numeric(year)) != 1)), by = location][, consecutive_crisis := cumsum(c(T, diff(as.numeric(year)) == 1)), by = .(location, crisis_run)]
appeals_req_f5ycc <- appeals_req_f5ycc[consecutive_crisis <= 5, .SD[5 %in% consecutive_crisis], by = .(location, crisis_run)]

