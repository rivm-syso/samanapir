#' GetSamenMetenAPI
#'
#' Functie die de sensordata van het samenmetenportaal haalt.
#'
#' @param projectnaam : string met projectnaam erin. Van dit project worden alle
#'                       sensoren opgehaald
#' @param ymd_vanaf : string met datum van start van de periode Bijv:"20190909"
#' @param ymd_tot : string met datum van eind van de periode Bijv:"20190912"
#' @param data_opslag
#' @param updateProgress : functie om de progress te tonen in bijv. shiny tool
#' @param debug : boolean, om aan te geven of er prints moeten worden gemaakt die kunnen helpen bij debuggen.
#'
#' @return named list met:
#'       sensordata: dataframe met de informatie over de sensor
#'              (sensor_id, kit_id, project, locatie_lat, locatie_lon, url)
#'       metingen: dataframe met de meetgegevens
#'               waarde = meet waarde
#'               tijd =  tijd van de meting in UTC
#'               name = naam van de grootheid (bijv. pm10)
#'               kit_id = naam van de sensor zoals in database samenmeten
#'               error = wanneer er geen metingen konden worden opgehaald staat hier de url die geprobeed is
#' LET OP: wanneer er geen metingen van een sensor zijn in de meegegeven periode
#' staat de sensor wel in de sensordata, maar heeft geen gegevens in metingen!
#' @export
#'
#' @examples
#' TEST <- GetSamenMetenAPI("project eq'Amersfoort'","20190909", "20190912")
GetSamenMetenAPI <- function(projectnaam, ymd_vanaf, ymd_tot, data_opslag = list(), updateProgress=NULL, debug=F){
  ##################### ----
  # Helperfuncties
  ##################### ----
  GetlocatieAPI <- function(ind, data_opslag_loc = data_opslag){
    # Deze functie haalt de gegevens op uit de url_loc.
    # Hierbij gaat het om de lat en de lon.
    # Wanneer er een fout is, wordt de waarde NA meegegeven
    # input:
    #     url_loc = string met de API waar de locatie staat
    #     kit_id = string, id zoals in de samenmetendatabase
    # output: dataframe met de kolommen $lat en $lon en $kit_id

    # Haal de betreffende url en kit_id op uit dataframe
    sensor_data_loc <- data_opslag_loc$sensor_data
    url_loc <- sensor_data_loc[sensor_data_loc$id==ind,'url_loc']
    kit_id <- sensor_data_loc[sensor_data_loc$id==ind, 'kit_id']

    # Ophalen van de informatie in API: LOCATIE
    content_loc_data <- GetAPIDataframe(url_loc)

    if (debug){print(content_loc_data)}

    # Voeg een error afvanging erin. Er kan iets mis gaan met de URL
    # of server waardoor er geen waarde terug komt, maar een string met error
    if(! is.character(content_loc_data)){
      content_loc_data_df <- content_loc_data$value
      # Extract de gegevens die je wilt gebruiken
      lon <- content_loc_data_df$location$coordinates[[1]][1]
      lat <- content_loc_data_df$location$coordinates[[1]][2]
      # Voeg ze toe aan output
      locatie_df <- data.frame('lat' = lat, 'lon' = lon, 'kit_id' = kit_id)
    }else{
      locatie_df <- data.frame('lat' = NA, 'lon' = NA, 'kit_id' = kit_id)
    }
    return(locatie_df)
  }

  GeturlsmeetAPI <- function(ind, data_opslag_urlsmeet = data_opslag){
    # Deze haalt per sensor de verschillende type grootheden op (naam zoals PM10).
    # Daarnaast ook de url waar de verdere meetgeveens (observations) van deze grootheid staan.
    # input:
    #     kit_id
    #     url_datastream
    # output: dataframe met de kolommen
    #     kit_id (1 unieke waarde)
    #     grootheid (meerdere waardes)
    #     url_meet (meerdere waardes)

    # Haal de betreffende url en kit_id op uit dataframe
    sensor_data_urlsmeet <- data_opslag_urlsmeet$sensor_data
    url_datastream <- sensor_data_urlsmeet[sensor_data_urlsmeet$id==ind,'url_datastream']
    kit_id <- sensor_data_urlsmeet[sensor_data_urlsmeet$id==ind, 'kit_id']

    if(debug){print(paste0("urlsdatastream ",url_datastream ))}

    # Ophalen van de informatie in API: Datastream
    content_datastream_data <- GetAPIDataframe(url_datastream)
    content_datastream_data_df <- content_datastream_data$value

    # Er zijn natuurlijk meerdere grootheden gemeten. Ga elke apart af.
    # Maak een lijst met de urls voor elke grootheid en een voor de meetgegevens
    overzicht_url_grootheid <- content_datastream_data_df[,'ObservedProperty@iot.navigationLink']
    overzicht_url_meetgegevens <- content_datastream_data_df['Observations@iot.navigationLink'] # Waarom is dit nu een dataframe geworden???

    # Haal de verschillende grootheden op (de naam dus pm10)
    grootheden <- unlist(lapply(overzicht_url_grootheid, GetgrootheidAPI))
    if(debug){print(paste0("grootheden ",grootheden ))}

    # Maak de output dataframe, met de url van de meetgevens per grootheid
    output_df <- data.frame('url_meet' = overzicht_url_meetgegevens[,1], 'grootheid' = grootheden, 'kit_id' = kit_id)

    return(output_df)
  }

  GetgrootheidAPI <- function(url_grootheid){
    # Haalt het type van de grootheid op.
    # Leest de naam uit van de url en returned deze
    content_grootheid_df <- GetAPIDataframe(url_grootheid)
    grootheid <- content_grootheid_df['name']
    return(grootheid)
  }

  GetmeetgegevensAPI <- function(ind, data_opslag_meet = data_opslag){
    # Deze functie haalt de meetgegevens op van desbetreffende sensor en grootheid
    # voor een bepaalde periode
    # input:
    #     url_meetgegevens = string;
    #     grootheid = string, naam van de stof;
    #     kit_id = string, id zoals in de samenmetendatabase
    #     ymd_vanaf = string met datum van start van de periode Bijv:"20190909"
    #     ymd_tot = string met datum van einde van de periode Bijv:"20190909"
    # output: dataframe met daaron de kolommen
    #     waarde = meetwaarde
    #     tijd = tijd van de meting in UTC
    #     grootheid = input grootheid
    #     kit_id = input kit_id
    #     error = als er een error was bij het ophalen van de data staat hierin de url.

    # Sys.sleep(runif(1,0,1))

    # Haal de betreffende url en kit_id op uit dataframe
    urls_meet_meet <- data_opslag_meet$urls_meet
    url_meetgegevens <- urls_meet_meet[urls_meet_meet$id==ind,'url_meet']
    kit_id <- urls_meet_meet[urls_meet_meet$id==ind, 'kit_id']
    grootheid <- urls_meet_meet[urls_meet_meet$id==ind, 'grootheid']

    # Voor de progres bar in de tool: aangeven welke sensor er nu is en hoeveel er nog zijn
    # If we were passed a progress update function, call it
    aantal_sensoren <- length(urls_meet_meet$kit_id)
    if (is.function(updateProgress)) {
      text <- paste0("Sensor: ", kit_id, " - stap (",ind,"/",aantal_sensoren ,")")
      updateProgress(value = (ind/aantal_sensoren)*0.8+0.1 ,detail = text)
    }

    # Voeg filter toe die op tijdstip kan selecteren
    url_meetgegevens <- paste(url_meetgegevens,"?$filter=phenomenonTime+gt+%27",ymd_vanaf,"%27+and+phenomenonTime+lt+%27",ymd_tot,"%27&$orderby=phenomenonTime",sep='') # Met Orderby

    # De api splitst de gegevens op in meerdere pagina's.
    # Alle pagina's wil je uitlezen, dus zolang er nog een nieuwe pagina is, pak die uit
    pagina_aanwezig_meetgegevens <- TRUE

    # Dataframe om alle metingen van de sensoren op te halen (id, grootheid, tijd, meetwaarde)
    # Dit is een longformat
    metingen_df <- data.frame()

    while(pagina_aanwezig_meetgegevens){
      # Uitvoeren van de API
      content_meetgegevens <-  GetAPIDataframe(url_meetgegevens)

      # Check of de URL klopte -> Als er geen data is meegegeven, dan is het een character
      if (! is.character(content_meetgegevens)){
        # Zet de data in een dataframe
        content_meetgegevens_df <- content_meetgegevens$value

        # Pak de meetwaardes en de desbetreffende tijd
        waardes <- content_meetgegevens_df$result
        tijd <- as.POSIXct(content_meetgegevens_df$phenomenonTime, format='%Y-%m-%dT%H:%M:%S', tz='UTC')

        # Voeg de meetwaarde met tijd toe aan de algemene metingen dataframe
        gegevens_toevoegen <- data.frame('waarde' = waardes, 'tijd' = tijd, 'grootheid' = grootheid, 'kit_id' = kit_id, 'error' = NA)

      } else{ # nu is er dus een error met het ophalen van de data
        gegevens_toevoegen <- data.frame('waarde' = NA, 'tijd' = as.POSIXct(NA, tz='UTC'), 'grootheid' = grootheid,
                                         'kit_id' = kit_id, 'error' = paste('error in: ', url_meetgegevens, sep=""))
      }

      # Voeg de opgehaalde gegevens toe aan het totaal
      metingen_df <- rbind(metingen_df, gegevens_toevoegen)

      # Als er nog een pagina is, pak die dan ook uit
      if(length(content_meetgegevens) > 1){
        url_meetgegevens <- content_meetgegevens[[1]]
        if (debug){print('nieuwe pagina: +20')}
      } else{ # Is er geen pagina meer? Stop dan met de while loop.
        pagina_aanwezig_meetgegevens <- FALSE
      }
    }
    return(metingen_df)
  }

  ##################### ----
  # Initialisatie ----
  ##################### ----
  # Zet uit dat strings als factor worden opgeslagen
  # Dat is nl heel onhandig bij het doorgeven van strings naar de API
  options(stringsAsFactors = FALSE)

  # EQ,EQUALS is uniek voor elke gemeente en project
  # URL van de sensoren binnen een project
  url_things <- paste("https://api-samenmeten.rivm.nl/v1.0/Things?$filter=(properties/",projectnaam,")", sep='')

  url_things <- gsub(' ','%20', url_things)

  print('daadwerkelijke hoofdurl')
  print(url_things)

  if(debug){print(paste0("URL things: ", url_things))}

  # Dataframe om de basisgegevens van de sensoren op te slaan (naam, locatie, project, url)
  sensor_data <- data.frame()

  # Dataframe om alle metingen van de sensoren op te halen (id, grootheid, tijd, meetwaarde)
  # Dit wordt een longformat
  metingen_df <- data.frame()

  ##################### ----
  # OPHALEN van de sensorgegevens ----
  ##################### ----
  # De api splitst de gegevens op in meerdere pagina's.
  # Alle pagina's wil je uitlezen, dus zolang er nog een nieuwe pagina is, pak die uit
  pagina_aanwezig_things <- TRUE
  # If we were passed a progress update function, call it
  if (is.function(updateProgress)) {
    text <- paste0("basisgegevens")
    updateProgress(value=0.1, detail = text)
  }
  # Ga alle sensoren binnen dit project af en haal basisgegevens ervan op
  # Daarna de locatie
  # Tot slot alle meetgegevens binnen de meegegevens tijdsperiode
  while(pagina_aanwezig_things){

    # Ophalen van de informatie in API: THINGS
    tryCatch({
      content_things <- GetAPIDataframe(url_things)
      content_things_df <- content_things$value
    }, error = function(e){
      stop("Error in URL things. Check of projectnaam bestaat.")
    })

    # bewaar de gegevens over de sensor
    sensor_data <- rbind(sensor_data, data.frame('sensor_id' = content_things_df[,'@iot.id'],
                                                 'kit_id' = content_things_df[,'name'],
                                                 'project' = content_things_df$properties['project'],
                                                 'url_loc' = content_things_df[,'Locations@iot.navigationLink'],
                                                 'url_datastream' = content_things_df[,'Datastreams@iot.navigationLink'],
                                                 'knmicode' = content_things_df$properties['knmicode'],
                                                 'pm10closecode' = content_things_df$properties['pm10closecode'],
                                                 'pm10regiocode' = content_things_df$properties['pm10regiocode'],
                                                 'pm10stadcode' = content_things_df$properties['pm10stadcode'],
                                                 'pm25closecode' = content_things_df$properties['pm25closecode'],
                                                 'pm25regiocode' = content_things_df$properties['pm25regiocode'],
                                                 'pm25stadcode' = content_things_df$properties['pm25stadcode']
    ))

    # Kijk of er nog een pagina is die je dan wilt uitlezen
    if (length(content_things)>1){
      url_things <- content_things[[1]]
      if(debug){print('nieuwe pagina')}
    } else{
      pagina_aanwezig_things <- FALSE
    }
  }

  # maak een index aan aan de sensordata voor de verdere apply
  sensor_data['id'] <- seq(1:nrow(sensor_data))
  # losse lijst met de index
  # ind <- sensor_data[,'id']
  ind <- seq(1:nrow(sensor_data))

  # Voeg de sensor_data toe aan de data opslag
  data_opslag[['sensor_data']] <- sensor_data

  print('Sensordata opgehaald')

  ##################### ----
  # OPHALEN van de locaties ----
  ##################### ----
  # haal locatie op voor alle sensors tegelijk met apply
  locaties <- lapply(ind, GetlocatieAPI)
  # Zet de list op naar 1 grote dataframe
  locaties <- do.call("rbind", locaties)

  # Voeg de locaties toe aan de sensor_data
  sensor_data <- merge(sensor_data, locaties, by = 'kit_id')

  # Overschrijf de sensor_data toe aan de data opslag
  data_opslag[['sensor_data']] <- sensor_data

  print('Locatiedata opgehaald')
  if(debug){print(paste0('aantal api calls: ', length(ind)))}

  ##################### ----
  # OPHALEN van alle meetgegevens urls per sensor ----
  ##################### ----

  # Zoek alle urls bij elkaar waar meetgegevens inzitten
  # Deze neemt ook de types van de grootheid mee
  # De functie GeturlsmeetAPI doet dit
  urls_meet <- lapply(ind, GeturlsmeetAPI)

  # urls_meet <- lapply(ind, GeturlsmeetAPI)
  urls_meet <- do.call("rbind", urls_meet)

  if(debug){print(paste0("urls_meet: ",urls_meet))}

  # Voeg index toe voor de apply functie in de volgende stap
  urls_meet['id'] <- seq(1:nrow(urls_meet))

  # maak list met de index voor apply
  ind_meet <- seq(1:nrow(urls_meet)) # houd dit zo, het werkt niet als het een tibble is

  # Voeg de sensor_data toe aan de data opslag
  data_opslag[['urls_meet']] <- urls_meet

  print('URLS meet opgehaald')
  if(debug){print(paste0('aantal api calls: ', length(ind)*4))}


  ##################### ----
  # OPHALEN van alle data per sensor ----
  ##################### ----

  # Ga alle urls af waar meeteggevens bevinden (met vorige stap opgehaald)
  # Haal de gegevens op en zet in dataframe met kit_id en grootheid.
  # Dit is wat de functie GetmeetgegevensAPI doet

  meetgegevens <- lapply(ind_meet, GetmeetgegevensAPI)
  meetgegevens <- do.call("rbind", meetgegevens)

  print('Meetgegevens opgehaald')
  if(debug){print(paste0('aantal api calls: ', length(ind)))}

  #################### ----
  # Combine output ----
  #################### ----
  # Om het progress aan te geven van de laatste stap
  # If we were passed a progress update function, call it
  if (is.function(updateProgress)) {
    text <- paste0("Verwerken naar Samen Analyseren Tool")
    updateProgress(value=0.95,detail = text)
  }
  # Maak een list van de sensordata en de metingen, zodat meegegeven kan worden als output
  all_data_list <- list('sensordata' = sensor_data, 'metingen' = meetgegevens,  'dataopslag'= data_opslag)
  return(all_data_list)
}
