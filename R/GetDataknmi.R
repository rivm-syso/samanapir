#' GetKNMIAPI
#'
#' Deze functie haalt de uurgemiddelde gegevens op van de KNMI stations:
#' windrichting, windsnelheid, temperatuur en relatieve luchtvochtigheid
#'
#'
#'
#'
#' @param stations : lijstje met de stationnummers as character Bijv: c("235","280")
#' @param ymd_vanaf : string "yyyymmdd"
#' @param ymd_tot : string "yyyymmdd"
#'
#' @return named list met daarin:
#'   info: dataframe met het nummer, lon, lat, hoogte, naam van het station
#'   data:
#' Dataframe met de volgende kolommen
#' STNS     = stationnummer
#' YYYYMMDD = datum (YYYY=jaar,MM=maand,DD=dag)
#' H        = tijd eindtijd van het uur (HH=uur, UT.12 UT=13 MET, 14 MEZT. Uurvak 05 loopt van 04.00 UT tot 5.00 UT;
#' DD       = Windrichting (in graden) gemiddeld over de laatste 10 minuten van het afgelopen uur
#'             (360=noord, 90=oost, 180=zuid, 270=west, 0=windstil 990=veranderlijk.
#' FF       = Windsnelheid (in m/s) gemiddeld over de laatste 10 minuten van het afgelopen uur;
#' TEMP     = Temperatuur (in graden Celsius) op 1.50 m hoogte tijdens de waarneming ( T in vars knmi);
#' U        = Relatieve vochtigheid (in procenten) op 1.50 m hoogte tijdens de waarneming;
#' tijd     = combinatie van YYYYMMDD en HH: as.POSIXct en UTC, eindtijd van uurgemiddelde
#'LET OP: als er geen data beschikbaar was (of problemen met server) dan is de return:
#' een string met daarin: "error"
#'
#'
#' @export
#'
#' @examples
#' TEST_API_KNMI <- GetKNMIAPI('260','20191214','20191215')
#'
GetKNMIAPI <- function(stations, ymd_vanaf, ymd_tot){
  # Zet de tijd bij de datum
  ymdh_vanaf <- paste(ymd_vanaf,'01',sep='')
  ymdh_tot <- paste(ymd_tot,'24',sep='')

  # Zet de lijstje stations om in 1 string die dan in de wget kan
  for(ind in seq(1:length(stations))){
    if(ind==1){
      samengevoegd <- stations[ind]
    }else{
      samengevoegd <- paste(samengevoegd,stations[ind],sep=':')
    }
  }
  stns <- samengevoegd

  # Ophalen van de meetgegevens van de stations
  knmi_uur_wget_string <- paste("wget -O - --no-check-certificate --post-data='stns=", stns, "&start=",ymdh_vanaf,"&end=",ymdh_tot,
                                "&vars=DD:FF:T:U' https://www.daggegevens.knmi.nl/klimatologie/uurgegevens", sep="")
  print(knmi_uur_wget_string)

  # Ophalen van de gegevens van de URL
  knmi_uur_raw <- system(knmi_uur_wget_string,intern=T)

  # Check of er wel data is opgehaald
  if(is.null(dim(knmi_uur_raw))){
    print('KNMI: Geen data opgehaald. ERROR')
    return(knmi_uur_raw)
    return( "error")
  }else if(substring(knmi_uur_raw[[1]],1,1)!= "#"){
    print('KNMI: Geen data opgehaald. ERROR')
    return(knmi_uur_raw)
    return( "error")
  }

  print('KNMI: ruwe data opgehaald')

  # Bepaal aantal headers:
  aantal_headers <- length(grep("#",knmi_uur_raw))

  # Haal de kolomnamen op:
  # Dit gaat bijna goed: nog 2 schoonheidsfoutjes: '# STN' en 'TRUE' (de T van temperatuur wordt vertaald naar TRUE)
  header_names <- as.character(read.csv(textConnection(knmi_uur_raw[aantal_headers]), header=F, strip.white=T, stringsAsFactors=F))
  header_names[1] <- 'STNS'
  header_names[grep("TRUE",header_names)] <- 'TEMP'

  # Zet de ruwe tekst om in een dataframe
  knmi_uur_df <- utils::read.csv(textConnection(knmi_uur_raw), header=F, skip=aantal_headers, na.strings="-", col.names=header_names)

  print('KNMI: data omgezet naar dataframe')

  # Zet de eenheid Temp om van 0.1 graden C naar 1 graden C
  knmi_uur_df['TEMP'] <- knmi_uur_df['TEMP']/10

  # Zet de eenheid FF om van 0.1 m/s naar 1 m/s
  knmi_uur_df['FF'] <- knmi_uur_df['FF']/10

  # Combineer de datum en uur naar het tijdstip in POSIXct: nieuwe kolom 'tijd'
  knmi_uur_df['tijd'] <- as.POSIXct(as.character(knmi_uur_df$YYYYMMDD), tz='UTC', format='%Y%m%d')
  knmi_uur_df['tijd'] <- knmi_uur_df['tijd'] + knmi_uur_df$H*60*60

  # Haal de gegevens over de info van sensoren (naam en coordinaten)
  begin_info <- which(grepl("LON", knmi_uur_raw))+1
  regeleinde_stns <- which(grepl("# DD", knmi_uur_raw))-1
  # Het gedeelte uit de API waar die sensor info instaat
  stn_info_ruw <- knmi_uur_raw[begin_info:regeleinde_stns]
  # Zet wat waardes uit de tekst om
  stn_info_ruw <- sub(':','',stn_info_ruw)
  stn_info_ruw <- sub('#','',stn_info_ruw)
  stn_info_ruw <- str_squish(stn_info_ruw) # Verwijder al de overige spaties
  # Van tekst naar dataframe
  stn_info_df <- (utils::read.csv(textConnection(stn_info_ruw),header=F ,sep = " ",strip.white=T, stringsAsFactors = F))

  print('KNMI: metadata omgezet naar dataframe')

  # Voeg de naam kolommen samen, door de spaties zijn de DE BILT in 2 kolommen
  if(length(names(stn_info_df))==6){
    stn_info_df[5] <- paste(stn_info_df[,5], stn_info_df[,6])
    stn_info_df[6] <- NULL}
  else if(length(names(stn_info_df))==7){
    stn_info_df[5] <- paste(stn_info_df[,5], stn_info_df[,6], stn_info_df[,7])
    stn_info_df[6] <- NULL
    stn_info_df[7] <- NULL}

  # Geef mooie kolomnamen
  colnames(stn_info_df) <- c("STNS","LON", "LAT", "ALT", "NAME")

  # Maak een list waarin de metingen en de coordinaten avn de stations staan
  knmi_info_data <- list(info=stn_info_df, data=knmi_uur_df)

  print('KNMI: metadata en data gecombineerd')

  return(knmi_info_data)
}

