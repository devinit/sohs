suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/PiN/hpc_caseload_api.R",
         "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/deflators.R")
       , source)

setwd(dirname(getActiveDocumentContext()$path))
setwd("..")
setwd("..")

plans <- data.table(fromJSON("https://api.hpc.tools/v2/public/plan")$data)
plans[, year := lapply(years, function(x) x$year), by = id]
plans[, type := lapply(categories, function(x) x$name), by = id]

plans <- plans[year %in% 2018:2021]

plans_list <- list()
for(i in 1:nrow(plans)){
  message(plans[i]$planVersion.name)
  plans_list[[i]] <- fts_get_appeal_totals(plans[i]$id, plans[i]$year)
}
plans_table <- rbindlist(lapply(plans_list, data.table), fill = T)

plans_table <- plans_table[, lapply(.SD, function(x) as.numeric(gsub("US[$]|,", "", x))), by = .(year, plan_name)]
plans_table[is.na(plans_table)] <- 0

plan_caseloads <- list()
pb <- txtProgressBar(0, nrow(plans), style = 3)
for(i in 1:nrow(plans)){
  plan_id <- plans$id[[i]]
  plan_name <- plans$planVersion.name[[i]]
  location <- paste0(plans$locations[[i]][["name"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  iso <- paste0(plans$locations[[i]][["iso3"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  year <- paste0(plans$years[[i]][["year"]], collapse = "; ")
  
  metadata <- cbind(plan_id, plan_name, location, iso, year)
  
  caseload_temp <- hpc_api_all(plan_id, data_type = "caseLoad", by_sector = F, disaggregations = F)
  
  if(!is.null(caseload_temp)) plan_caseloads[[i]] <- data.table(cbind(metadata, caseload_temp))
  rm(plan_id)
  setTxtProgressBar(pb, i)
}
plan_caseloads <- rbindlist(plan_caseloads, fill = T)

plan_caseloads <- plan_caseloads[metric_id %in% c("inNeed", "target", "expectedReach")]

plan_caseloads <- plan_caseloads[plan_caseloads[, .I[which.max(value)], by = .(plan_id, metric_id)]$V1]

plans_percap <- merge(plan_caseloads, plans_table[, .(plan_id = as.character(appeal_id), funding = `Funded through this plan` + `COVID.Funded through this plan`)], by = "plan_id")

plans_percap <- merge(plans_percap, plans[, .(plan_id = as.character(id), type, revisedRequirements)], by = "plan_id")

deflators <- get_deflators(base_year = 2020, currency = "USD", weo_ver = "Oct2021", approximate_missing = T)[ISO == "DAC"]
plans_percap <- merge(plans_percap, deflators[, .(year = as.character(year), gdp_defl)], by = "year")
plans_percap[, `:=` (requirements_defl = revisedRequirements/gdp_defl, funding_defl = funding/gdp_defl)]

fwrite(plans_percap, "People in need and targeted/Fig 1.7/plans_req_pin.csv")

#Sex disaggregations
sex_f <- c("women", "girl", "female", "fille", "femme", "féminin", "feminin", "niña", "nina", "mujere")
sex_m <- c("\\bmen\\b", "boy", "\\bmale\\b", "garçon", "garcon", "homme", "masculin", "niño", "\\bnino\\b", "hombre")

plan_caseloads[grepl(paste0(sex_f, collapse = "|"), category_id, ignore.case = T), sex := "F"]
plan_caseloads[grepl(paste0(sex_m, collapse = "|"), category_id, ignore.case = T), sex := "M"]

by_sex <- plan_caseloads[metric_id %in% c("inNeed", "target") & !is.na(sex) & sector == "Total" & !is.na(value), sum(value), by = .(year, iso, plan_name, metric_id, sex)]
