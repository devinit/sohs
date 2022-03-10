lapply(c("data.table", "rstudioapi", "readxl"), require, character.only = T)
setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))
lapply(c("https://raw.githubusercontent.com/devinit/gha_automation/main/general/emdatData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/hiikData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/informAPI.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/wupData.R",
         "https://raw.githubusercontent.com/devinit/gha_automation/main/general/acapsAPI.R"
         ), source)

isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8")

pop <- wup_get()

emdat <- emdat_get()
hiik <- hiik_get()

#INFORM
years <- 2018:2021
inform <- list()
for(i in 1:length(years)){
  message(paste("INFORM:", years[i]))
  inform[[i]] <- inform_get(years[i], "CC,VU")
}
inform <- rbindlist(inform)

lccs <- inform[IndicatorId == "CC" & IndicatorScore >= 4.8, .(iso3 = Iso3, year)]
vulns <- inform[IndicatorId == "VU" & IndicatorScore >= 6, .(iso3 = Iso3, year)]

#ACAPS
dates <- as.POSIXlt("2019-01-01")
dates$mon <- seq(0,length(seq(as.Date("2019-01-01"), Sys.Date(), by = 'month'))-1)
dates <- format.Date(dates, "%b%Y")

acaps <- data.table()
for(i in 1:length(dates)){
  message(paste("ACAPS:", dates[i]))
  acaps_temp <- acaps_get(database = "inform-severity-index", date = dates[i], token = token)
  acaps_temp[, `:=` (mon = substr(dates[i], 1, 3), year = substr(dates[i], 4, 8))]
  acaps <- rbind(acaps, acaps_temp, fill = T)
  rm(acaps_temp)
}

crisis_classes <- list(`Physical` = list("Drought", "Flood", "Earthquake", "Tropical cyclone", "Epidemic", "Industrial Accidents", "Hurricane", "Monsoon", "Measles", "Tsunami", "Volcano", "Typhoon", "Tropical storm"),
                       `Conflict` = list("Conflict", "Other type of violence"),
                       `Displacement` = list("International displacement", "Refugee", "Migration", "Displacement"),
                       `Complex` = list("Complex crisis", "Complex situation", "Complex", "Political and economic crisis", "Socioeconomic crisis", "Multiple crises", "Regional crisis", "Food security", "Food insecurity"))

acaps <- cbind(acaps, acaps[, sapply(crisis_classes, function(x) grepl(paste0(x, collapse = "|"), paste(crisis_name, type_of_crisis), ignore.case = T))])
acaps <- merge(acaps[, .(iso3 = unlist(lapply(iso3, function(x) strsplit(x, ", ")))), by = .(crisis_id, year, mon)], acaps[, -"iso3"], by = c("crisis_id", "year", "mon"))

acaps_flags <- acaps[, lapply(.SD, max), .SDcols = c("Physical", "Conflict", "Displacement", "Complex"), by = .(iso3, year)]

#Physical/Natural
emdat <- emdat[, .(affected = sum(as.numeric(`Total Affected`), na.rm = T)), by = .(ISO, Year)]
emdat <- merge(emdat[Year >= 1950], pop[area == "total"], by.x = c("ISO", "Year"), by.y = c("ISO3", "year"), all.x = T)
emdat[, share_affected := affected/population]

acaps_physical <- acaps_flags[Physical == 1 & year == 2019]
acaps_physical <- merge(acaps_physical[, year := 2018], emdat, by.x = c("iso3", "year"), by.y = c("ISO", "Year"))
acaps_physical <- merge(acaps_physical, emdat[Year == 2019], by.x = c("iso3"), by.y = c("ISO"), suffixes = c("_18", "_19"))
acaps_physical <- acaps_physical[share_affected_18 >= 0.9*share_affected_19, .(iso3, year)]

min_physical_share <- emdat[paste0(ISO,Year) %in% acaps_flags[acaps_flags[Physical == 1, .I[which.min(year)], by = iso3]$V1, paste0(iso3, year)] & affected > 100000, min(share_affected, na.rm = T)]

emdat[share_affected >= min_physical_share & paste0(ISO, Year) %in% lccs[, paste0(iso3, year)], physical_marker := T]

physical <- unique(rbind(unique(emdat[physical_marker == TRUE, .(iso3 = ISO, year = Year)]), acaps_flags[Physical == 1, .(iso3, year)], acaps_physical))

#Conflict
hiik[, countries := gsub(" [(].*| et al[.]| et[.] al|°","", conflict)]
hiik[, countries := gsub(" – ", ", ", countries)]

data_cols <- c("X", names(hiik)[grepl("intensity", names(hiik))])
hiik <- merge(hiik[, .(country = unlist(strsplit(countries, ", "))), by = .(X, type)], hiik[, ..data_cols])

hiik <- merge(hiik, isos[,c("iso3", "countryname_hiik")], by.x = "country", by.y = "countryname_hiik", all.x = T)

hiik <- melt(hiik, id.vars = c("X", "iso3", "country", "type"))
hiik[, year := gsub("\\D", "", variable)]

hiik <- unique(rbind(hiik, copy(hiik)[year == max(year), year := as.character(as.numeric(year) + 1)]))

acaps_conflict <- acaps_flags[Conflict == 1 & year == 2019]
acaps_conflict <- merge(acaps_conflict[, year := 2018], hiik, by = c("iso3", "year"))
acaps_conflict <- merge(acaps_conflict, hiik[year == 2019], by = c("iso3", "type", "X"), suffixes = c("_18", "_19"))
acaps_conflict <- acaps_conflict[value_18 >= value_19, .(iso3, year = year_18)]

hiik[value >= 4  & paste0(iso3, year) %in% lccs[, paste0(iso3, year)], conflict_marker := T]

conflict <- unique(rbind(unique(hiik[conflict_marker == TRUE, .(iso3, year)]), acaps_flags[Conflict == 1, .(iso3, year)], acaps_conflict))

#Displacement
displacement <- data.table(read_excel("S:/Projects/GHA/Phase IV/Projects/P0489 GHA Programme 2022/ALNAP SOHS/Project Content/Fig 0.4 Funding volumes to types of emergency/Supporting Data/Displacement Data.xlsx", sheet = "Displaced population by country", range = "A3:H204"))
displacement <- melt(displacement[, -"Country of asylum"], id.vars = "ISO 3")

displacement <- merge(displacement, pop[area == "total"], by.x = c("ISO 3", "variable"), by.y = c("ISO3", "year"), all.x = T)
displacement[, share_displaced := value/population]

acaps_displacement <- acaps_flags[Displacement == 1 & year == 2019]
acaps_displacement <- merge(acaps_displacement[, year := 2018], displacement, by.x = c("iso3", "year"), by.y = c("ISO 3", "variable"))
acaps_displacement <- merge(acaps_displacement, displacement[variable == 2019], by.x = c("iso3"), by.y = c("ISO 3"), suffixes = c("_18", "_19"))
acaps_displacement <- acaps_displacement[share_displaced_18 >= 0.9*share_displaced_19, .(iso3, year)]

min_displacement_share <- displacement[paste0(`ISO 3`, variable) %in% acaps_flags[acaps_flags[Displacement == 1, .I[which.min(year)], by = iso3]$V1, paste0(iso3, year)], min(share_displaced, na.rm = T)]

displacement[share_displaced >= min_displacement_share & paste0(`ISO 3`, variable) %in% lccs[, paste0(iso3, year)] & paste0(`ISO 3`, variable) %in% vulns[, paste0(iso3, year)], displacement_marker := T]

displacement <- unique(rbind(unique(displacement[displacement_marker == TRUE, .(iso3 = `ISO 3`, year = variable)]), acaps_flags[Displacement == 1, .(iso3, year)], acaps_displacement))

#Complex
complex <- acaps_flags[Complex == 1 & Conflict == 0 & Displacement == 0, .(iso3, year)]

##All crises
all_crises <- unique(rbind(physical, conflict, displacement, complex))
all_crises[paste0(iso3, year) %in% physical[, paste0(iso3, year)], Physical := 1]
all_crises[paste0(iso3, year) %in% conflict[, paste0(iso3, year)], Conflict := 1]
all_crises[paste0(iso3, year) %in% displacement[, paste0(iso3, year)], Displacement := 1]

all_crises[is.na(all_crises)] <- 0
all_crises[, Complex := 0]
all_crises[paste0(iso3, year) %in% acaps_flags[Complex == 1, paste0(iso3, year)] | Physical + Conflict + Displacement > 1 & paste0(iso3, year) %in% lccs[, paste0(iso3, year)], Complex := 1]

all_crises[Complex == 1 & Conflict == 0 & Displacement == 0 & paste0(iso3, year) %in% lccs[, paste0(iso3, year)], `:=` (Conflict = 1, Displacement = 1)]

fwrite(all_crises[order(iso3, year)], "Crisis types/Fig 0.4/crisis_types.csv")
