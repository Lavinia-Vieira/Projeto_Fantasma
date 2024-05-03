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
    Variancia = round(var(IMDB), digits = 2))

view(temp_nota)

#______ Boxplot 

ggplot(temp_nota) +
  aes(x = reorder(Temporada, IMDB, FUN = median), y = IMDB) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDB") +
  estat_theme()
ggsave("boxA2.pdf", path = ("D:/documentos/lavinia/PS/PF/Projeto_Fantasma/resultados"), width = 158, height = 93, units = "mm")

#______ Quadro resumo

print_quadro_resumo <- function(data, title="Medidas resumo da(o) [nome da variável]", label="quad:quadro_resumo1")
{
  data <- temp_nota %>%
    summarize(`Média` = round(mean(IMDB),2),
              `Desvio Padrão` = round(sd(IMDB),2),
              `Variância` = round(var(IMDB),2),
              `Mínimo` = round(min(IMDB),2),
              `1º Quartil` = round(quantile(IMDB, probs = .25),2),
              `Mediana` = round(quantile(IMDB, probs = .5),2),
              `3º Quartil` = round(quantile(IMDB, probs = .75),2),
              `Máximo` = round(max(IMDB),2)) %>%
    t() %>% 
    as.data.frame() %>%
    rownames_to_column()
  
  latex <- str_c("\\begin{quadro}[H]
\t\\caption{", title, "}
\t\\centering
\t\\begin{adjustbox}{max width=\\textwidth}
\t\\begin{tabular}{", sep="")
  
  col_count <- ncol(data)
  row_count <- nrow(data)
  latex <- str_c(latex, "| l", "|", sep=" ")
  for (i in seq(2, col_count))
  {
    latex <- str_c(latex, "S", sep=" ")
  }
  
  latex <- str_c(latex, "|}\n\t\\toprule\n\t\t", sep="")
  
  if (col_count > 2)
  {
    for (i in seq(1,col_count))
    {
      if (i == 1)
        latex <- str_c(latex, "\\textbf{Estatística}", sep="")
      else
        latex <- str_c(latex, " \\textbf{", data[1, i], "}", sep="")
      
      if (i < col_count)
        latex <- str_c(latex, "&", sep=" ")
      else
        latex <- str_c(latex, "\\\\\n", sep=" ")
    }
  }
  else
  {
    latex <- str_c(latex, "\\textbf{Estatística} & \\textbf{Valor} \\\\\n", sep="")  
  }
  
  latex <- str_c(latex, "\t\t\\midrule\n", sep="")
  
  if (col_count > 2)
    starting_number <- 2
  else
    starting_number <- 1
  
  for (i in seq(starting_number, row_count))
  {
    latex <- str_c(latex, "\t\t", str_flatten(t(data[i,]), collapse = " & "), " \\\\\n")
  }
  latex <- str_c(latex, "\t\\bottomrule
\t\\end{tabular}
\t\\label{", label, "}
\t\\end{adjustbox}
\\end{quadro}", sep="")
  
  writeLines(latex)
}

temp_nota %>%
  group_by(Temporada) %>%
  print_quadro_resumo()

#______ Remove ----
  
rm()

#______ sei la ----

 # %>%
  mutate(
    freq_relativa = round(Total / sum(Total) * 100,1)
  )

# temp_nota$Variancia <- str_replace(temp_nota$Variancia, "\\.", ",")
