#___________________ Projeto Fantasma ___________________#
#___________________ Lavinia ___________________#

#______ Pacotes ----

library(tidyverse)

#______ Diretorio de trabalho ----

setwd('D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/rdocs') #getwd() retorna o diretorio atual

#______ Banco de dados ----

banco_final <- read_csv("D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/banco/banco_final.csv")
View(banco_final)

#______ Analise 1 ----

#______ Organizacao do banco de dados

formato_data <- banco_final %>%
  select(format, date_aired) %>%
  rename("Formato" = format, "Decada"= date_aired) %>%
  mutate(
    Formato = case_when(
    Formato == ("CrossOver") ~ "CrossOver",
    Formato == ("Movie") ~ "Filme",
    Formato == ("Serie") ~ "Serie"))
  

formato_data$Decada <- formato_data$Decada %>%
  floor_date(unit = c("10 years")) %>%
  format("%Y") 

formato_data <- formato_data %>%
  group_by(Formato, Decada) %>%
  arrange(Formato, Decada, sort = T, .by_group = T) %>%
  summarise(Total = n())

# formatoData <- with(formato_data, table(Formato, Decada)) - Table com colunas logicas

#______ Grafico de linhas

ggplot(formato_data) +
  aes(x = Decada, y = Total, group = Formato, colour = Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Decada", y = "Frequencia") +
  estat_theme()
ggsave("graficoLinhasA1.pdf",  width = 158, height = 93, units = "mm")

#______ Analise 2 ----

#______ Organizacao do banco de dados

temp_nota <- banco_final %>%
  select(format, season, imdb) %>%
  filter(format == "Serie") %>%
  filter(season != "Special") %>%
  group_by(season) %>%
  arrange(season) %>%
  rename("Temporada" = season, "IMDB" = imdb) %>%
  select(Temporada, IMDB) %>%
  mutate(
    Media = round(mean(IMDB), digits = 2),
    Variancia = round(var(IMDB), digits = 2))

temp_nota$Media <- str_replace(temp_nota$Media, "\\.", ",")
temp_nota$IMDB <- str_replace(temp_nota$IMDB, "\\.", ",")

view(temp_nota)

fruits <- c("one apple", "two pears", "three bananas")
str_replace_all(fruits, "[aeiou]", "-")

#______ 



#______ Remove ----
  
rm()

#______ sei la ----

 # %>%
  mutate(
    freq_relativa = round(Total / sum(Total) * 100,1)
  )