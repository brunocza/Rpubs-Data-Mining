#importar pacotes
{ 
  # Check if the packages that we need are installed
  want = c("tidyverse", "COVID19", "data.table", "dplyr", "ggplot2")
  have = want %in% rownames(installed.packages())
  # Install the packages that we miss
  if ( any(!have) ) { install.packages( want[!have] ) }
  # Load the packages
  junk <- lapply(want, library, character.only = T)
  # Remove the objects we created
  rm(have, want, junk)
} 

#importar Banco de dados
{
  dcovid19 = covid19(level = 1)
  id = c(	"AIA",	"ARG",	"ABW",	"BHS",	"BRB",	"BLZ",	"BMU",	"BOL",	"BRA",	"VGB",	"CAN",	"CYM",	"CHL",	"COL",	"CRI",	"CUB",	"CUW",	"DMA",	"DOM",	"ECU",	"SLV",	"FLK",	"GRL",	"GLP",	"GTM",	"GUY",	"HTI",	"HND",	"JAM",	"MEX",	"MSR",	"NIC",	"PAN",	"PRY",	"PER",	"PRI",	"BLM",	"KNA",	"LCA",	"MAF",	"SPM",	"VCT",	"SUR",	"TTO",	"USA",	"URY",	"VEN")
  name =c (	"Anguilla",	"Argentina",	"Aruba",	"Bahamas",	"Barbados",	"Belize",	"Bermuda",	"Bolivia",	"Brazil",	"British Virgin Islands",	"Canada",	"Cayman Islands",	"Chile",	"Colombia",	"Costa Rica",	"Cuba",	"Curacao",	"Dominica",	"Dominican Republic",	"Ecuador",	"El Salvador",	"Falkland Islands",	"Greenland",	"Guadeloupe",	"Guatemala",	"Guyana",	"Haiti",	"Honduras",	"Jamaica",	"Mexico",	"Montserrat",	"Nicaragua",	"Panama",	"Paraguay",	"Peru",	"Puerto Rico",	"Saint Barthélemy",	"Saint Kitts and Nevis",	"Saint Lucia",	"Saint Martin",	"Saint Pierre and Miquelon",	"Saint Vincent and the Grenadines",	"Suriname",	"Trinidad and Tobago",	"United States",	"Uruguay",	"Venezuela"
  )
  america = data.frame(id,name)
}

#Filtrar dados
{
  letalidade = dcovid19 %>% select(.,id,date,tests,confirmed,deaths,population,administrative_area_level_1)%>% 
    filter(date=="2020-06-30", confirmed > 0,deaths > 0, tests > 0, id %in% america$id) %>%
    ungroup() %>%
    mutate( let = deaths * 100 / confirmed ) %>%
    mutate(., med = summarise(., avg = mean(let))) %>%
    mutate(., total = summarise(.,sum(  population))) %>% 
    mutate(., percent_pop = population/ total) %>% 
    mutate(., g_2 = let * percent_pop  ) %>% 
    mutate(., med_pond = summarise(.,sum(  g_2))) %>% 
    mutate( test_pond = tests * 100 / population )
  
  letalidade = letalidade  %>% select(.,id,date,tests,test_pond,confirmed,deaths,population,administrative_area_level_1,let,med,med_pond)
  remove(dcovid19, america, name, id)
}

#plotar Grafico
{
  ggplot(letalidade) +
    aes(x = test_pond, y = let, colour = id) +
    geom_point(size = 3L) + 
    geom_text(aes(label=id),hjust=0,vjust=-0.3)+
    scale_color_hue() +
    theme_minimal()
}
