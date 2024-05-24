#___________________ Projeto Fantasma ___________________#
#___________________ Lavinia ___________________#

#______ Pacotes ----

library(tidyverse)
library(DescTools)

#______ Diretorio de trabalho ----

setwd('D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/rdocs') #getwd() retorna o diretorio atual

#______ Banco de dados ----

banco_final <- read_csv("D:/Documentos/lavinia/PS/PF/Projeto_Fantasma/banco/banco_final.csv")
View(banco_final)

#______ Analise 1 ----

#______ Organizacao do banco de dados

formato_data <- banco_final %>%
  select(format, date_aired) %>%
  rename("Formato" = format,
         "Década"= date_aired) %>%
  mutate(
    Formato = case_when(
    Formato == ("CrossOver") ~ "CrossOver",
    Formato == ("Movie") ~ "Filme",
    Formato == ("Serie") ~ "Série"))

formato_data$`Década` <- formato_data$`Década` %>%
  floor_date(unit = c("10 years")) %>%
  format("%Y") 

formato_data <- formato_data %>%
  group_by(Formato, `Década`) %>%
  arrange(Formato, `Década`, sort = T, .by_group = T) %>%
  summarise(Total = n())

#______ Grafico de linhas

ggplot(formato_data) +
  aes(x = `Década`, y = Total, group = Formato, colour = Formato) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Década", y = "Frequência") +
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
  rename("Temporada" = season,
         "IMDb" = imdb) %>%
  select(Temporada, IMDb)

#______ Boxplot 

ggplot(temp_nota) +
  aes(x = reorder(Temporada, IMDb), y = IMDb) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Temporada", y = "Nota IMDb") +
  estat_theme()
ggsave("boxA2.pdf", width = 158, height = 93, units = "mm")

#______ Quadro resumo

print_quadro_resumo <- function(data, title="Medidas resumo da nota IMDb por temporada dos episódios", label="quad:quadro_resumoA2")
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

#______ Analise 3 ----

#______ Organizacao do banco de dados

banco_final %>%
  select(setting_terrain) %>%
  count(setting_terrain, sort = TRUE) %>%
  head(setting_terrain, n = 3)

terreno_armadilha <- banco_final %>%
  select(setting_terrain, trap_work_first) %>%
  rename("Terreno" = setting_terrain,
         "Armadilha é ativada na primeira tentativa" = trap_work_first) %>%
  filter(Terreno %in% c("Urban","Rural", "Forest")) %>%
  na.omit() %>%
  mutate(Terreno = case_when(
    Terreno == "Urban" ~ "Urbano",
    Terreno == "Rural" ~ "Rural",
    Terreno == "Forest" ~ "Floresta")) %>%
  mutate(`Armadilha é ativada na primeira tentativa` = case_when(
    `Armadilha é ativada na primeira tentativa` == "FALSE" ~ "Não",
    `Armadilha é ativada na primeira tentativa` == "TRUE" ~ "Sim"
    )) %>%
  group_by(Terreno, `Armadilha é ativada na primeira tentativa`)

coef <- ftable(terreno_armadilha$Terreno, terreno_armadilha$`Armadilha é ativada na primeira tentativa`)
ContCoef(coef, correct = T)

terreno_armadilha <- terreno_armadilha %>%
  summarise(freq = n()) %>%
  mutate(
    freq_relativa = round(freq / sum(freq) * 100,2))

#______ Grafico colunas bivariado

porcentagens <- str_c(terreno_armadilha$freq_relativa, "%") %>% str_replace("\\.", ",")

legendas <- str_squish(str_c(terreno_armadilha$freq, " (", porcentagens, ")"))

ggplot(terreno_armadilha) +
  aes(
    x = fct_reorder(Terreno, freq, .desc = T), y = freq,
    fill = `Armadilha é ativada na primeira tentativa`, label = legendas
  ) +
  geom_col(position = position_dodge2(preserve = "single", padding =
                                        0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, hjust = 0.5,
    size = 3
  ) +
  labs(x = "Terreno", y = "Frequência") +
  estat_theme()
ggsave("colunasBiFreqA3.pdf", width = 158, height = 93, units = "mm")

#______ Analise 4 ----

#______ Organigacao do banco de dados

eng_imdb <- banco_final %>%
  select(imdb, engagement) %>%
  rename("IMDb" = imdb,
         "Engajamento" = engagement)

#______ coeficiente de relacao

cor.test(eng_imdb$IMDb, eng_imdb$Engajamento, method = c("pearson"))

#______ grafico dispersao

ggplot(eng_imdb) +
  aes(x = IMDb, y = Engajamento) +
  geom_point(alpha = 0.7, colour = "#A11D21", size = 3) +
  labs(
    x = "Nota IMDb",
    y = "Engajamento"
  ) +
  estat_theme()
ggsave("dispUniA4.pdf", width = 158, height = 93, units = "mm")

#______ quadro resumo do engajamento e medidas resumo das notas IMDB

print_quadro_resumo <- function(data, title="Medidas resumo do Engajamento do episódio ou filme", label="quad:quadro_resumoA4")
{
  data <- eng_imdb %>%
    summarize(`Média` = round(mean(Engajamento),2),
              `Desvio Padrão` = round(sd(Engajamento),2),
              `Variância` = round(var(Engajamento),2),
              `Mínimo` = round(min(Engajamento),2),
              `1º Quartil` = round(quantile(Engajamento, probs = .25),2),
              `Mediana` = round(quantile(Engajamento, probs = .5),2),
              `3º Quartil` = round(quantile(Engajamento, probs = .75),2),
              `Máximo` = round(max(Engajamento),2)) %>%
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

eng_imdb %>%
  print_quadro_resumo()

round(summary(eng_imdb$IMDb), 2)
round(sd(eng_imdb$IMDb), 2)
round(var(eng_imdb$IMDb), 2)

#______ Analise 5 ----

#______ Organizacao do banco de dados

eng_personagem <- banco_final %>%
  select(engagement, 20:24, 41) %>%
  rename("Engajamento" = engagement, "Fred" = caught_fred,
         "Daphnie" = caught_daphnie, "Velma" = caught_velma,
         "Salsicha" = caught_shaggy, "Scooby" = caught_scooby,
         "Outro" = caught_other) %>%
  pivot_longer(cols = 2:7, names_to = "Personagem", values_to = "captura") %>% 
  filter(captura == T) %>%
  select(-captura) %>%
  group_by(Personagem) %>%
  na.omit() %>%
  arrange(Engajamento)

#______ boxplot

ggplot(eng_personagem) +
  aes(x = reorder(Personagem, Engajamento, FUN = median), y = Engajamento) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "Personagem que capturou o monstro", y = "Engajamento") +
  estat_theme()
ggsave("boxA5.pdf", width = 158, height = 93, units = "mm")

#______ quadro resumo

print_quadro_resumo <- function(data, title="Medidas resumo do Engajamento por Personagem que capturou o monstro", label="quad:quadro_resumoA5")
{
  data <- eng_personagem %>%
    summarize(`Média` = round(mean(Engajamento),2),
              `Desvio Padrão` = round(sd(Engajamento),2),
              `Variância` = round(var(Engajamento),2),
              `Mínimo` = round(min(Engajamento),2),
              `1º Quartil` = round(quantile(Engajamento, probs = .25),2),
              `Mediana` = round(quantile(Engajamento, probs = .5),2),
              `3º Quartil` = round(quantile(Engajamento, probs = .75),2),
              `Máximo` = round(max(Engajamento),2)) %>%
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

eng_personagem %>%
  group_by(Personagem) %>%
  print_quadro_resumo()

#______ Remove ----

rm()
