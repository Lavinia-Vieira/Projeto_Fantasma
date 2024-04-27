#___________________ Projeto Fantasma ___________________#
#___________________ Lavinia ___________________#

#______ Pacotes ----

pacman::p_load(tidyverse, readr, readxl, lubridate, janitor)

#______ Diretorio de trabalho ----

setwd('D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/rdocs') #getwd() retorna o diretorio atual

#______ Banco de dados ----

banco_final <- read_csv("D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/banco/banco_final.csv")
View(banco_final)

#______ Analise 1 ----

#______ Organizacao do banco de dados ----

formato_data <- banco_final %>%
  select(format, date_aired) %>%
  rename("Decada" = date_aired, "Formato" = format) %>%
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
  arrange(Formato, Decada, sort = T, .by_group = F) %>%
  summarise(Total = n()) %>%
  mutate(
    freq_relativa = round(Total / sum(Total) * 100,1)
  )

# formatoData <- with(formato_data, table(Formato, Decada)) - Table com colunas logicas

#______ Graficos colunas freq relativa ----

porcentagens <- str_c(formato_data$freq_relativa, "%") %>%
  str_replace("\\.", ",")

legendas <- str_squish(str_c(porcentagens))

ggplot(formato_data) +
  aes(x = fct_reorder(Formato, Total, .desc = T), y = freq_relativa,
      fill = Decada, label = legendas) +
  geom_col(position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9),
            vjust = -0.5, hjust = 0.5, size = 3) +
  labs(x = "Formato", y = "Frequencia relativa") +
  estat_theme()
ggsave("graficoColFreqRelA1.pdf", plot = last_plot(), device = "pdf",
       path = "D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/resultados",
       width = 158, height = 93, units = "mm")


#_____ Boxplot ----

ggplot(formato_data) +
  aes(x = reorder(Formato, Total, FUN = median), y = Total) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Formato", y = "Total de lancamentos") +
  estat_theme()
ggsave("boxFormatoData.pdf", plot = last_plot(), device = "pdf",
       path = "D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/resultados",
       width = 158, height = 93, units = "mm")


#______ Remove ----
  
rm()
