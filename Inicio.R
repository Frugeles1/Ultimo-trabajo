#setwd "es el espacio de trabajo"

library(xml2)
library(rvest)

#leer una pagina web

OtakusTV <- read_html("https://www.otakustv.com/animes")
print(html_text(OtakusTV))

#obteniendo clase "Nombre anime y N°de episodio nuevo.

Camino_Estrenos <- html_nodes(OtakusTV,xpath="/html/body/div[4]/div/div[1]")
print(html_text(Camino_Estrenos))

Estrenos_10_12_2021 <- html_nodes(Camino_Estrenos,css=".col-6")
print(html_text(Estrenos_10_12_2021))

Capitulos <- html_nodes(Camino_Estrenos, css=".h2,p")
print(html_text(Capitulos))

