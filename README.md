<!-- README.md is generated from README.Rmd. Please edit that file -->
samanapir
=========

<!-- badges: start -->
<!-- badges: end -->
Het doel van samanapir is om de gegevens voor Sensor-data analyse bij elkaar
te brengen. De verschillende functies kunnen de gegevens ophalen van de samen meten
database, luchtmeetnet en het KNMI. 

Deze functies worden gebruikt in de 
[Samen Analyseren Tool](https://github.com/rivm-syso/Samen-analyseren-tool) 
. Dat is een shiny-tool
met een interface voor het gebruik van deze functies. Daarnaast zijn er 
verschillende mogelijkheden voor datavisualisatie.

Installatie
------------

``` r
install.package("devtools")
url <- "https://gitl01-int-p.rivm.nl/tennere/samanapir"
devtools::install_git(url = url)
```

Voorbeeld
-------

Hier een voorbeeld hoe je gegevens van het KNMI kan downloaden:

``` r
library(samanapir)
TEST_API_KNMI <- GetKNMIAPI('260','20191214','20191215')
```
Hier een voorbeeld hoe je gegevens van het Luchtmeetnet kan downloaden:

``` r
library(samanapir)
TEST <- GetLMLAPI("NL01908", "20190505", "20190510")
```
Hier een voorbeeld hoe je gegevens van het Samen Meten Dataportaal kan downloaden:

``` r
library(samanapir)
TEST <- GetSamenMetenAPI("project eq'Amersfoort'","20190909", "20190912")
```

Dataverantwoording
-------
Voor het ophalen en downloaden van de gegevens wordt gebruik gemaakt van:
* [Samen Meten Dataportaal](https://www.samenmetenaanluchtkwaliteit.nl/dataportaal/api-application-programming-interface)
* [Luchtmeetnet](https://www.luchtmeetnet.nl/)
* [KNMI](http://projects.knmi.nl/klimatologie/uurgegevens/selectie.cgi)
