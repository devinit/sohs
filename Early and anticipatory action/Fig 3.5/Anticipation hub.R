lapply(c("data.table", "rstudioapi", "jsonlite"), require, character.only = T)

setwd(dirname(dirname(dirname(getActiveDocumentContext()$path))))

ahub <- data.table(fromJSON("https://www.anticipation-hub.org/fileadmin/countries.json"))

ahub <- ahub[, .(description = gsub('.*</h4>|.*class=\"aCOpRe\">|[|] </span>', "", description)), by = .(id, name)]
ahub <- cbind(ahub[, .(id, name)], ahub[, tstrsplit(description, "\r\n<p>|&nbsp;[|] |[|]&nbsp;|[|] |[|]</span> ")])

ahub[, gsub("^; |; NA", "", paste0(gsub("</span>| [(]<a href.*|&nbsp|</p>|</p>|\r\n\r\n", "", .SD), collapse = "; ")), .SDcols = grepl("^V.", names(ahub)), by = .(id, name)]
