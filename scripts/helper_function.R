read_RWS_recent_patch <- function(file, dir = "", 
                     attr = NULL, 
                     format = "wide", ...){
  
  RWS <- NULL
  for (fi in file){
    fn <- paste(dir, fi, sep = "/")
    fn <- gsub("//", replacement = "/", fn)
    RWS <- rbind(RWS, read.csv2(fn))  
  }
  
  
  ## create a mapper from old and new names
  select_column_name_schema <- list(
    MEETPUNT_IDENTIFICATIE = c("MEETPUNT_IDENTIFICATIE"), 
    LOCATIE_CODE = c("LOCATIE_CODE"), 
    WAARDEBEPALINGSMETHODE_OMSCHRIJVING = c("WAARDEBEPALINGSMETHODE_OMSCHRIJVING"),
    WAARNEMINGDATUM = c("WAARNEMINGDATUM"), 
    WAARNEMINGTIJD..MET.CET. = c("WAARNEMINGTIJD", "WAARNEMINGTIJD..MET.CET."), 
    PARAMETER_OMSCHRIJVING = c("PARAMETER_OMSCHRIJVING"), 
    GROOTHEID_OMSCHRIJVING = c("GROOTHEID_OMSCHRIJVING"), 
    HOEDANIGHEID_OMSCHRIJVING = c("HOEDANIGHEID_OMSCHRIJVING"), 
    EENHEID_CODE = c("EENHEID_CODE"), 
    NUMERIEKEWAARDE = c("NUMERIEKEWAARDE"),
    MEETAPPARAAT_OMSCHRIJVING = c("MEETAPPARAAT_OMSCHRIJVING"),
    # LON = c("LON", "X"), 
    # LAT = c("LAT", "Y") 
    X = c("LON", "X"), 
    Y = c("LAT", "Y") 
  )
  
  ## Interface to standardize the name 
  .standardize_columns <- function(df, maps = select_column_name_schema){
    actual_names <- names(df)
    new_names <- actual_names
    
    for (standard_name in names(maps)) {
      # Find which variant exists in the current dataframe
      match_idx <- which(actual_names %in% maps[[standard_name]])
      
      if (length(match_idx) == 1) {
        new_names[match_idx] <- standard_name
      } else if (length(match_idx) > 1) {
        warning(paste("Multiple matches found for", standard_name, "- using the first one."))
        new_names[match_idx[1]] <- standard_name
      } else {
        stop(paste("Required data field for", standard_name, "is missing from the recent RWS database."))
      }
    }
    
    names(df) <- new_names
    if("LON" %in% actual_names){
      attributes(df)$coord_type <- "LONLAT" 
    } else {
      attributes(df)$coord_type <- "XY"
    }
    return(df)
  }
  
  ## standardize the name 
  RWS <- .standardize_columns(RWS)
  
  
  RWS <- RWS[! is.na(RWS$X),]
  # RWS <- RWS[! is.na(RWS$LON),]
  RWS <- RWS[! is.na(RWS$WAARNEMINGDATUM), ]
  select <- c("MEETPUNT_IDENTIFICATIE", "LOCATIE_CODE",
              "WAARDEBEPALINGSMETHODE_OMSCHRIJVING",
              "WAARNEMINGDATUM", "WAARNEMINGTIJD..MET.CET.",
              "PARAMETER_OMSCHRIJVING","GROOTHEID_OMSCHRIJVING",
              "HOEDANIGHEID_OMSCHRIJVING",
              "EENHEID_CODE", "NUMERIEKEWAARDE",
              "MEETAPPARAAT_OMSCHRIJVING", "X" ,      "Y" )
  
  
  # select <- c("MEETPUNT_IDENTIFICATIE", "LOCATIE_CODE", 
  #             "WAARDEBEPALINGSMETHODE_OMSCHRIJVING",
  #             "WAARNEMINGDATUM", "WAARNEMINGTIJD",  
  #             "PARAMETER_OMSCHRIJVING","GROOTHEID_OMSCHRIJVING", 
  #             "HOEDANIGHEID_OMSCHRIJVING",
  #             "EENHEID_CODE", "NUMERIEKEWAARDE", 
  #             "MEETAPPARAAT_OMSCHRIJVING", "LON" ,      "LAT" )
  
  cn <- c("stationName", "station", 
          "method",
          "date", "time",  "parameter", "description", 
          "property",  "unit", "value", 
          "sensor", "X" ,      "Y" )
  

  RWSdat <- RWS[, select]
  colnames(RWSdat) <- cn
  RWSdat <- RWSdat[RWSdat$value < 1e10, ]
  RWSdat$variable <- NA
  Vars <- rbind(
    c("Verzadigingsgraad",         "", "O2_percent", "Oxygen saturation percent"),   # NEEDS TO BE FIRST
    c("Biochemisch zuurstofverbruik met allylthioureum", "BZV5a", "BOD", "Biochemical oxygen demand"),
    c("chlorofyl-a",               "",      "Chl",     "Chlorophyll"),
    c("Chemisch zuurstofverbruik", "CZV",   "COD",     "Chemical oxygen demand"),
    c("Doorzicht",                 "",      "Secchi",  "Extinction depth"),
    c("Extinctie",                 "",      "Ext",     "Extinction- dimensionless"),
    c("fosfor totaal",             "Ptot",  "Ptot",    "Total phosphor"),
    c("koolstof",                  "Ctot",  "Ctot",    "Total carbon"),
    c("koolstof anorganisch",      "Cinorg", "PIC",    "Particulate inorganic carbon"),
    c("koolstof organisch",        "Corg",   "POC",    "Particulate organic carbon"),
    c("nitraat",                   "NO3",    "NO3",    "Nitrate"),  # PARAMETER_OMSCHRIJVING
    c("nitriet",                   "NO2",    "NO2",    "Nitrite"),
    c("ammonium",                  "NH4",    "NH4",    "Ammonium"),
    c("Onopgeloste stoffen",       "OS",     "SPM",    "Suspended particulate matter"),
    c("orthofosfaat",              "PO4",    "PO4",    "Phosphate"),
    c("siliciumdioxide",           "SiO2",   "SiO2",   "Silicium dioxide"),
    c("som nitraat en nitriet",    "sNO3NO2", "NOx",   "Nitrate+nitrite"),
    c("stikstof Kjeldahl",         "NKj",    "KjN",    "Kjehldahl nitrogen"),
    c("stikstof totaal",           "Ntot",   "Ntot",   "Total nitrogen"),
    c("Zuurgraad",                 "pH",     "pH",     "pH"),   # GROOTHEID_OMSCHRIJVING
    c("zuurstof",                  "",       "O2",     "Oxygen"),   
    c("sulfaat",                   "SO4",    "SO4",    "Sulphate"),
    c("waterstofcarbonaat",        "HCO3",   "HCO3",   "Bicarbonate"),
    c("Temperatuur",               "T",      "T",   "Temperature"),          
    c("Waterhoogte",               "WATHTE", "Height", "Waterheight"), 
    c("Saliniteit",                "SALNTT", "S",      "Salinity"),
    c("Waterhoogte berekend",      "WATHTBRKD", "Height", "Waterheight_estimated"))
  
  # Chloride, Chlorophyl, Doorzicht, Extinctiecoefficient
  # Opgelost organisch koolstof, Particulair organisch koolstof, Percentage zuurstof,
  # Saliniteit, Stikstof
  Vars <- as.data.frame(Vars)
  
  names(Vars) <- c("dutch_name", "dutch_code", "variable", "description")
  convfac=c(BOD  = 1,        
            COD  = 1,       
            Ptot = 1e3/30.97376,    
            NO3  = 1e3/14.0067, NO2 = 1e3/14.0067, NH4 = 1e3/14.0067, 
            SPM  = 1, 
            PO4  = 1e3/30.97376, 
            SiO2 = 1e3/28.0855,       
            NOx  = 1e3/14.0067,  KjN = 1e3/14.0067, Ntot = 1e3/14.0067,
            pH   = 1, SO4 = 1e3/32.065, 
            HCO3 = 1e3/(1+12.0107+3*15.9994),  
            Temp = 1,      
            Height = 0.01,     height_est =0.01, 
            O2     = 15.9994*2, 
            Secchi = 0.1)  # was: dm
  newunits = c(BOD = NA, COD = NA, Ptot = "mmol/m3", 
               NO3 = "mmol/m3", NO2 = "mmol/m3", NH4 = "mmol/m3", 
               SPM = NA, PO4 = "mmol/m3",       
               SiO2 = "mmol/m3", NOx = "mmol/m3", 
               KjN = "mmol/m3", Ntot = "mmol/m3",  
               pH = "-",  
               SO4 = "mmol/m3", HCO3 = "mmol/m3",  # mg/L ???
               Temp = NA,      
               Height = "m", height_est ="m", 
               O2 = "mmol/m3", Secchi = "m")
  NN <- merge(data.frame(variable = names(convfac), 
                         convfac = convfac), 
              data.frame(variable = names(newunits), newunits = newunits), 
              all = TRUE)
  
  VARS <- merge(Vars, NN, all = TRUE)
  
  for(i in 1:nrow(VARS)){
    
    ii <- which(RWSdat$description  == VARS$dutch_name[i])
    
    if (! length(ii))
      ii <- which(RWSdat$parameter == VARS$dutch_name[i])
    
    if (length(ii)){
      pars <- unique(RWSdat$description[ii])
      if (length(pars) > 1) 
        ii <- ii[which(RWSdat$description[ii] == "(massa)Concentratie")]
    }       
    
    if (length(ii)){
      RWSdat$description[ii] <- VARS$description[i]
      RWSdat$variable[ii]    <- VARS$variable[i]
      
      if (! is.na(VARS$newunits[i])){
        # browser()
        RWSdat$value[ii]    <- as.numeric(RWSdat$value[ii])*VARS$convfac[i]
        RWSdat$unit[ii]     <- VARS$newunits[i]
      }
    }
  }
  
  # manually clean the salinity, which also contains chlorinity!
  ii <- which(RWSdat$description == "Salinity" & RWSdat$value > 100)
  
  if (length(ii)) {
    RWSdat$description[ii] <- "Chlorinity"
    RWSdat$variable[ii] <- "Cl"
  }
  
  dd <- as.POSIXct(paste(RWSdat$date[1], RWSdat$time[1]), 
                   format = "%d-%m-%Y %H:%M:%S")
  
  RWSdat$variable   [is.na(RWSdat$variable)]    <- "unknown"
  RWSdat$sensor     [is.na(RWSdat$sensor)]      <- "unknown"
  RWSdat$method     [is.na(RWSdat$method)]      <- "unknown"
  RWSdat$property   [is.na(RWSdat$property)]    <- "unknown"
  RWSdat$unit       [is.na(RWSdat$unit)]        <- "unknown"
  RWSdat$description[is.na(RWSdat$description)] <- "unknown"
  if (!is.na(dd))
    RWSdat$datetime <- as.POSIXct(paste(RWSdat$date, RWSdat$time), 
                                  format = "%d-%m-%Y %H:%M:%S")
  else 
    RWSdat$datetime <- as.POSIXct(paste(RWSdat$date, RWSdat$time), 
                                  format = "%d/%m/%Y %H:%M:%S")
  Tfrom <- aggregate(as.Date(RWSdat$datetime), 
                     by = as.list(RWSdat[,c("variable", "description", "property", 
                                            "method", "unit", "sensor")]),
                     FUN = min, na.rm = TRUE)
  names(Tfrom)[ncol(Tfrom)] <- "from"
  Tto <- aggregate(as.Date(RWSdat$datetime), 
                   by = as.list(RWSdat[,c("variable", "description", "property", 
                                          "method", "unit", "sensor")]),
                   FUN = max, na.rm = TRUE)
  names(Tto)[ncol(Tto)] <- "to"
  variables <- merge(Tfrom, Tto)
  variables <- data.frame(variables, 
                          vnr = 1:nrow(variables))
  
  if(attributes(RWS)$coord_type == "XY"){
    wgs84 <- xy_to_wgs84(RWSdat$X, RWSdat$Y) 
  } else {
    wgs84 <- wgs84_to_xy(RWSdat$X, RWSdat$Y)
  }
  
  RWSdat <- data.frame(wgs84, RWSdat)
  names(RWSdat)[names(RWSdat) %in% c("X.1", "Y.1")] <- c("longitude", "latitude")

  # browser()
  # RWSdat <- data.frame(wgs84, RWSdat)
  if (format == "wide")
    RWSdat <- data.frame(RWSdat, 
                         vnr = NA)  
  else
    RWSdat <- merge(RWSdat, variables)   ### THIS TAKES A VERY LONG TIME FOR BIG DATASETS
  
  stats <- unique(RWSdat[,c("stationName", "station", "X", "Y", "longitude", "latitude")])
  ID    <- stats[,c("station", "longitude", "latitude")]
  names(ID)[1] <- "ID"
  
  RWSdat <- RWSdat[,c("station", "longitude", "latitude", 
                      "datetime", "variable", "value", "unit", "vnr")]
  
  if (format == "wide"){
    RWSdat <- reshape(RWSdat[ , c(1:6)], 
                      direction = "wide", 
                      idvar     = c("station", "latitude", "longitude", "datetime"),   
                      timevar   = "variable")
    
    cn <- colnames(RWSdat)
    cn <- gsub("value.", "", cn)
    colnames(RWSdat) <- cn  
  } else format <- "long"
  
  attributes(RWSdat)$variables   <- variables
  attributes(RWSdat)$stations    <- stats
  attributes(RWSdat)$ID          <- ID
  attributes(RWSdat)$datasource  <- "RWS"
  attributes(RWSdat)$EPSG <- unique(na.omit(RWS$EPSG))
  attributes(RWSdat)$file <- file
  attributes(RWSdat)$processing <-  paste("Created at", Sys.time())
  attributes(RWSdat)$fun  <- "read_RWS" 
  attributes(RWSdat)$format <- format 
  if (length(attr))
    attributes(RWSdat) <- c(attributes(RWSdat), attr)
  class(RWSdat) <- c("dtLife", "data.frame")
  RWSdat
}



find_time_comparison_function <- function(forcing_list) {
  # Initialize variables to store the latest start time
  # and earliest end time
  latest_start_time <- -Inf
  earliest_end_time <- Inf
  latest_start_function <- NULL
  earliest_end_function <- NULL
  # Loop over the list of functions and compare start and
  # end times
  for (function_name in names(forcing_list)) {
    df <- forcing_list[[function_name]]
    # Get the first and last value of the 'Second'
    # column
    first_second <- min(df$second, na.rm = TRUE)
    last_second <- max(df$second, na.rm = TRUE)
    # Check if this function starts later than the
    # current latest start time
    if (first_second > latest_start_time) {
      latest_start_time <- first_second
      latest_start_function <- function_name
    }
    # Check if this function ends earlier than the
    # current earliest end time
    if (last_second < earliest_end_time) {
      earliest_end_time <- last_second
      earliest_end_function <- function_name
    }
  }
  # Return the result as a named list
  list(LatestStartFunction = latest_start_function, FirstSecond = latest_start_time,
       EarliestEndFunction = earliest_end_function, LastSecond = earliest_end_time)
}
