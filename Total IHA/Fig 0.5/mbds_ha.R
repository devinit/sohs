suppressPackageStartupMessages(lapply(c("data.table","rstudioapi", "readxl"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

countrynames <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8")

load_crs <- function(dataname="crs", path="reference_datasets", years = 2018:2020){
  require("data.table")
  files.gz <- list.files(path, pattern=paste0("^", dataname, "_\\d{4}[.]gz$"))
  data.years <- gsub("\\D", "", files.gz)
  files.gz <- files.gz[data.years %in% years]
  crs <- list()
  for(i in 1:length(files.gz)){
    print(paste0("Loading file ", i, " of ", length(files.gz)))
    filepath <- paste0(path, "/", files.gz[i])
    crs[[i]] <- fread(filepath, showProgress = F)
  }
  crs <- rbindlist(crs)
  return(crs)
}

crs_raw <- load_crs(dataname="crs", path="reference_datasets")

keep <- c(
  "CrsID",
  "Year",
  "FlowName",
  "Bi_Multi",
  "DonorName",
  "RecipientName",
  "USD_Disbursement_Defl",
  "SectorCode",
  "PurposeCode"
)

mdbs <- c(
  #RDBs
  "African Development Bank"
  ,"African Development Fund"
  ,"Asian Development Bank"
  ,"Asian Infrastructure Investment Bank"
  ,"Caribbean Development Bank"
  ,"Council of Europe Development Bank"
  ,"Development Bank of Latin America"
  ,"Inter-American Development Bank"
  ,"Islamic Development Bank"
  ,"European Bank for Reconstruction and Development"
  ,"International Investment Bank"
  ,"IDB Invest"
  
  #WB
  ,"International Development Association"
  ,"International Bank for Reconstruction and Development"
  ,"International Finance Corporation"
  
  #IMF
  #,"IMF (Concessional Trust Funds)"
  
  #Other IFIs
  #,"Arab Bank for Economic Development in Africa"
  #,"Arab Fund (AFESD)"
  #,"OPEC Fund for International Development"
  #,"Nordic Development Fund"
  )

crs <- crs_raw[, ..keep]
crs <- crs[
  FlowName == "ODA Loans" 
  |
    FlowName == "ODA Grants"
  | 
    FlowName == "Equity Investment"
  #| 
  #  FlowName == "Private Development Finance"
]

crs <- crs[Year >= 2018]

crs[, Humanitarian := F]
crs[grepl("^7", SectorCode), Humanitarian := T] #All sectors codes which begin with '7' (i.e. HA)

crs <- merge(crs, countrynames[, c("iso3", "countryname_oecd")], by.x = "RecipientName", by.y = "countryname_oecd", all.x = T)

crs_ha <- crs[Humanitarian == T]
crs_ha_rank <- crs_ha[!is.na(iso3), .(ha_total = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, iso3, RecipientName)][, ha_rank := frank(-ha_total), by = Year][]
fwrite(crs_ha_rank[ha_rank >= 20], "Total IHA/Fig 0.5/top_20_ha_recipients_list.csv")

crs <- merge(crs, crs_ha_rank[, .(Year, RecipientName, ha_rank)], by = c("RecipientName", "Year"), all.x = T)

crs_mdbs <- crs[DonorName %in% mdbs, .(ha_oda = sum(USD_Disbursement_Defl[Humanitarian == T], na.rm = T), total_oda = sum(USD_Disbursement_Defl, na.rm = T)), by = .(Year, DonorName, RecipientName, ha_rank)]

fwrite(crs_mdbs, "Total IHA/Fig 0.5/MDBs_ODA.csv")

