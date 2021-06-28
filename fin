---
title: "Global Situation Coronavirus 2020"
output: 
  flexdashboard::flex_dashboard:
    orientation: rows
    vertical_layout: fill
runtime: shiny
---

```{r setup, include=FALSE}
library(flexdashboard)
library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

dat<-read.csv("C:/Users/KL/Documents/kantana/finalexam.csv")

dat$date<-as.Date(dat$date)

names(dat)[3] <- "Region"
names(dat)[5] <- "Total"
names(dat)[6] <- "New cases"
names(dat)[7] <- "Total per 1 million population"
names(dat)[8] <- "New cases per 1 million population"
names(dat)[47] <- "Confirmed cases"
names(dat)[9] <- "Total deaths"
names(dat)[10] <- "New deaths"
names(dat)[11] <- "Total deaths per 1 million population"
names(dat)[12] <- "New deaths per 1 million population"
names(dat)[48] <- "Confirmed deaths"

computeStu = function(...) return(3435)
computeSecS = function(...) return(19)
computePriS = function(...) return(148)

Region<-selectInput("Region", label = "Region",
              choices = c("	Abruzzo	"	,
"	Afghanistan	"	,
"	Ain	"	,
"	Aisne	"	,
"	Alabama	"	,
"	Alaska	"	,
"	Albania	"	,
"	Algeria	"	,
"	Allier	"	,
"	Alpes-de-Haute-Provence	"	,
"	Alpes-Maritimes	"	,
"	American Samoa	"	,
"	Andalusia	"	,
"	Andorra	"	,
"	Angola	"	,
"	Antigua and Barbuda	"	,
"	Aragon	"	,
"	Ardennes	"	,
"	Ardรจche	"	,
"	Argentina	"	,
"	Arizona	"	,
"	Arkansas	"	,
"	Armenia	"	,
"	Asturias	"	,
"	Aube	"	,
"	Aude	"	,
"	Australia	"	,
"	Austria	"	,
"	Auvergne-Rhรดne-Alpes	"	,
"	Aveyron	"	,
"	Azerbaijan	"	,
"	Bahrain	"	,
"	Balearic Islands	"	,
"	Bangladesh	"	,
"	Barbados	"	,
"	Basilicata	"	,
"	Bas-Rhin	"	,
"	Belarus	"	,
"	Belgium	"	,
"	Belize	"	,
"	Benin	"	,
"	Bermuda	"	,
"	Bhutan	"	,
"	Bolivia	"	,
"	Bolzano	"	,
"	Bosnia and Herzegovina	"	,
"	Botswana	"	,
"	Bouches-du-Rhรดne	"	,
"	Bourgogne-Franche-Comtรฉ	"	,
"	Brazil	"	,
"	Bretagne	"	,
"	Brunei	"	,
"	Bulgaria	"	,
"	Burkina Faso	"	,
"	Burundi	"	,
"	Calabria	"	,
"	California	"	,
"	Calvados	"	,
"	Cambodia	"	,
"	Cameroon	"	,
"	Campania	"	,
"	Canada	"	,
"	Canarias	"	,
"	Cantabria	"	,
"	Cantal	"	,
"	Cape Verde	"	,
"	Castile and Leon	"	,
"	Castilla La Mancha	"	,
"	Catalonia	"	,
"	Central African Republic	"	,
"	Centre-Val de Loire	"	,
"	Ceuta	"	,
"	Chad	"	,
"	Charente	"	,
"	Charente-Maritime	"	,
"	Cher	"	,
"	Chile	"	,
"	Colombia	"	,
"	Colorado	"	,
"	Comoros	"	,
"	Connecticut	"	,
"	Corrรจze	"	,
"	Corse	"	,
"	Corse-du-Sud	"	,
"	Costa Rica	"	,
"	Creuse	"	,
"	Croatia	"	,
"	Cuba	"	,
"	Cyprus	"	,
"	Czech Republic	"	,
"	Cรดte-d'Or	"	,
"	Cรดtes-d'Armor	"	,
"	Delaware	"	,
"	Democratic Republic of the Congo	"	,
"	Denmark	"	,
"	District of Columbia	"	,
"	Djibouti	"	,
"	Dominica	"	,
"	Dominican Republic	"	,
"	Dordogne	"	,
"	Doubs	"	,
"	Drรดme	"	,
"	East of England	"	,
"	East Timor	"	,
"	Ecuador	"	,
"	Egypt	"	,
"	El Salvador	"	,
"	Emilia-Romagna	"	,
"	England	"	,
"	Equatorial Guinea	"	,
"	Eritrea	"	,
"	Essonne	"	,
"	Estonia	"	,
"	Eswatini	"	,
"	Ethiopia	"	,
"	Eure	"	,
"	Eure-et-Loir	"	,
"	Extremadura	"	,
"	Faroe Islands	"	,
"	Fiji	"	,
"	Finistรจre	"	,
"	Finland	"	,
"	Florida	"	,
"	France	"	,
"	Friuli Venezia Giulia	"	,
"	Gabon	"	,
"	Galicia	"	,
"	Gard	"	,
"	Georgia	"	,
"	Germany	"	,
"	Gers	"	,
"	Ghana	"	,
"	Gironde	"	,
"	Grand-Est	"	,
"	Greece	"	,
"	Greenland	"	,
"	Grenada	"	,
"	Guadeloupe	"	,
"	Guam	"	,
"	Guatemala	"	,
"	Guinea	"	,
"	Guinea-Bissau	"	,
"	Guyana	"	,
"	Guyane	"	,
"	Haiti	"	,
"	Haute-Corse	"	,
"	Haute-Garonne	"	,
"	Haute-Loire	"	,
"	Haute-Marne	"	,
"	Hautes-Alpes	"	,
"	Haute-Savoie	"	,
"	Haute-Saรดne	"	,
"	Haute-Vienne	"	,
"	Haut-Rhin	"	,
"	Hauts-de-France	"	,
"	Hauts-de-Seine	"	,
"	Hawaii	"	,
"	Honduras	"	,
"	Hong Kong	"	,
"	Hungary	"	,
"	Iceland	"	,
"	Idaho	"	,
"	Ille-et-Vilaine	"	,
"	Illinois	"	,
"	India	"	,
"	Indiana	"	,
"	Indonesia	"	,
"	Indre	"	,
"	Indre-et-Loire	"	,
"	Iowa	"	,
"	Iran	"	,
"	Iraq	"	,
"	Ireland	"	,
"	Israel	"	,
"	Isรจre	"	,
"	Italy	"	,
"	Ivory Coast	"	,
"	Jamaica	"	,
"	Japan	"	,
"	Jordan	"	,
"	Jura	"	,
"	Kansas	"	,
"	Kazakhstan	"	,
"	Kentucky	"	,
"	Kenya	"	,
"	Kingdom of the Netherlands	"	,
"	Kuwait	"	,
"	Kyrgyzstan	"	,
"	La Rioja	"	,
"	Landes	"	,
"	Laos	"	,
"	Latvia	"	,
"	Lazio	"	,
"	Lebanon	"	,
"	Lesotho	"	,
"	Liberia	"	,
"	Libya	"	,
"	Liechtenstein	"	,
"	Liguria	"	,
"	Lithuania	"	,
"	Loire	"	,
"	Loire-Atlantique	"	,
"	Loiret	"	,
"	Loir-et-Cher	"	,
"	Lombardia	"	,
"	London	"	,
"	Lot	"	,
"	Lot-et-Garonne	"	,
"	Louisiana	"	,
"	Luxembourg	"	,
"	Macau	"	,
"	Madagascar	"	,
"	Madrid	"	,
"	Maine	"	,
"	Maine-et-Loire	"	,
"	Malawi	"	,
"	Malaysia	"	,
"	Maldives	"	,
"	Mali	"	,
"	Malta	"	,
"	Manche	"	,
"	Marche	"	,
"	Marne	"	,
"	Marshall Islands	"	,
"	Martinique	"	,
"	Maryland	"	,
"	Massachusetts	"	,
"	Mauritania	"	,
"	Mauritius	"	,
"	Mayenne	"	,
"	Mayotte	"	,
"	Melilla	"	,
"	Meurthe-et-Moselle	"	,
"	Meuse	"	,
"	Mexico	"	,
"	Michigan	"	,
"	Midlands	"	,
"	Minnesota	"	,
"	Mississippi	"	,
"	Missouri	"	,
"	Moldova	"	,
"	Molise	"	,
"	Monaco	"	,
"	Mongolia	"	,
"	Montana	"	,
"	Montenegro	"	,
"	Morbihan	"	,
"	Morocco	"	,
"	Moselle	"	,
"	Mozambique	"	,
"	Murcia	"	,
"	Myanmar	"	,
"	Namibia	"	,
"	Navarre	"	,
"	Nebraska	"	,
"	Nepal	"	,
"	Nevada	"	,
"	New Hampshire	"	,
"	New Jersey	"	,
"	New Mexico	"	,
"	New York	"	,
"	New Zealand	"	,
"	Nicaragua	"	,
"	Niger	"	,
"	Nigeria	"	,
"	Niรจvre	"	,
"	Nord	"	,
"	Normandie	"	,
"	North Carolina	"	,
"	North Dakota	"	,
"	North East & Yorkshire	"	,
"	North Macedonia	"	,
"	North West	"	,
"	Northern Ireland	"	,
"	Northern Mariana Islands	"	,
"	Norway	"	,
"	Nouvelle-Aquitaine	"	,
"	Occitanie	"	,
"	Ohio	"	,
"	Oise	"	,
"	Oklahoma	"	,
"	Oman	"	,
"	Oregon	"	,
"	Orne	"	,
"	Pais Vasco	"	,
"	Pakistan	"	,
"	Panama	"	,
"	Papua New Guinea	"	,
"	Paraguay	"	,
"	Paris	"	,
"	Pas-de-Calais	"	,
"	Pays-de-la-Loire	"	,
"	Pennsylvania	"	,
"	People's Republic of China	"	,
"	Peru	"	,
"	Philippines	"	,
"	Piemonte	"	,
"	Poland	"	,
"	Portugal	"	,
"	Puerto Rico	"	,
"	Puglia	"	,
"	Qatar	"	,
"	Republic of the Congo	"	,
"	Rhode Island	"	,
"	Romania	"	,
"	Russia	"	,
"	Rwanda	"	,
"	Saint Kitts and Nevis	"	,
"	Saint Lucia	"	,
"	Saint Vincent and the Grenadines	"	,
"	Samoa	"	,
"	San Marino	"	,
"	Sardegna	"	,
"	Sarthe	"	,
"	Saudi Arabia	"	,
"	Savoie	"	,
"	Saรดne-et-Loire	"	,
"	Scotland	"	,
"	Seine-et-Marne	"	,
"	Seine-Maritime	"	,
"	Seine-Saint-Denis	"	,
"	Senegal	"	,
"	Serbia	"	,
"	Seychelles	"	,
"	Sicilia	"	,
"	Sierra Leone	"	,
"	Singapore	"	,
"	Slovakia	"	,
"	Slovenia	"	,
"	Solomon Islands	"	,
"	Somalia	"	,
"	Somme	"	,
"	South Africa	"	,
"	South Carolina	"	,
"	South Dakota	"	,
"	South East	"	,
"	South Korea	"	,
"	South Sudan	"	,
"	South West	"	,
"	Spain	"	,
"	Sri Lanka	"	,
"	State of Palestine	"	,
"	Sudan	"	,
"	Suriname",
"	Sweden	"	,
"	Switzerland	"	,
"	Syria	"	,
"	Taiwan	"	,
"	Tajikistan	"	,
"	Tanzania	"	,
"	Tarn	"	,
"	Tarn-et-Garonne	"	,
"	Tennessee	"	,
"	Territoire de Belfort	"	,
"	Texas	"	,
"	Thailand	"	,
"	The Bahamas	"	,
"	The Gambia	"	,
"	Togo	"	,
"	Tonga	"	,
"	Toscana	"	,
"	Trento	"	,
"	Trinidad and Tobago	"	,
"	Tunisia	"	,
"	Turkey	"	,
"	Turkmenistan	"	,
"	U.S. Virgin Islands	"	,
"	Uganda	"	,
"	Ukraine	"	,
"	Umbria	"	,
"	United Arab Emirates	"	,
"	United Kingdom	"	,
"	United States of America	"	,
"	Uruguay	"	,
"	Utah	"	,
"	Uzbekistan	"	,
"	Val-de-Marne	"	,
"	Val-d'Oise	"	,
"	Valencian Community	"	,
"	Valle d'Aosta	"	,
"	Vanuatu	"	,
"	Var	"	,
"	Vatican City	"	,
"	Vaucluse	"	,
"	Vendรฉe	"	,
"	Veneto	"	,
"	Venezuela	"	,
"	Vermont	"	,
"	Vienne	"	,
"	Vietnam	"	,
"	Virginia	"	,
"	Vosges	"	,
"	Washington	"	,
"	West Virginia	"	,
"	Wisconsin	"	,
"	Wyoming	"	,
"	Yemen	"	,
"	Yonne	"	,
"	Yvelines	"	,
"	Zambia	"	,
"	Zimbabwe	"	

))

Cases<-selectInput("Cases", label = "Cases",
              choices = c("Total","New cases","Total per 1 million population","New cases per 1 million population","Confirmed cases"))


Deaths<-selectInput("Deaths", label = "Deaths",
              choices = c("Total deaths","New deaths","Total deaths per 1 million population","New deaths per 1 million population","Confirmed deaths"))


time<-dateInput("date",
            label="Date",
            value = "2020-01-01",
            min = "2020-01-01",  
            max = "2020-12-31", 
            width = "100px", 
            format="mm/dd/yy")

index<-radioButtons("index", "Select the index", choices = c("strin","health","support")
            )

```

Row {data-height=150}
-----------------------------------------------------------------------

### Coronavirus Cases

```{r}
valueBox("16,253,219",color="#ffab45")

```

### Waiting for test result

```{r}
valueBox("776,212,252",color="#008ae6")

```

### Deaths

```{r}
valueBox("1,617,366",color="#fa1e1e")

```

### Recovered

```{r}
valueBox("752,329",color="#a8e827")

```


Row
-----------------------------------------------------------------------

### Cases


```{r}
inputPanel(Region,time,Cases)

renderPlotly({
  plot_ly(x = ~dat[[input$Cases]], type = "histogram", marker=list(color="#008ae6", line=list(color="darkgray",width=0.5))
  )
     
})
  

```



### Deaths


```{r}
inputPanel(Region,Deaths)

renderPlotly({
  plot_ly(x = ~dat[[input$Deaths]], type = "histogram", marker=list(color="#e60000", line=list(color="#darkgray",width=0.5))
  )
     
})
  

```

Row
-----------------------------------------------------------------------
### Index

```{r}
inputPanel(index,Region)

renderPlotly({
  plot_ly(x = ~dat[[input$index]], type = "histogram", marker=list(color="#e60000", line=list(color="#darkgray",width=0.5))
  )
     
})

```

### f


```{r}

  titlePanel("Demostration of sliderInput widget in shiny")
  sidebarLayout(
    sidebarPanel(
      sliderInput("slide", "Select the value from Slider", min = 0, max=5, value=2, step=0.2)
      
    ),
    mainPanel(
      
      textOutput("out")
    )
    
  )

```
