library(tidyverse)

data <- readr::read_csv("https://raw.githubusercontent.com/mnaR99/DA-BEDU/main/MOD2/data/psw02.csv")

data <- mutate(data, across(FTHG:FTAG, factor))

goals <- prop.table(table(Home = data$FTHG, Away = data$FTAG))

quots <- goals / outer(rowSums(goals), colSums(goals))

bootstrap_goals <- function(){
  sdata <- sample_frac(data, size = 1, replace = T) # Muestra del mismo tamaño que la base original con remplazo
  goals <- prop.table(table(Home = sdata$FTHG, Away = sdata$FTAG))
  quots <- goals / outer(rowSums(goals), colSums(goals))
  return(quots)
}

set.seed(234)

# Bootstrap
bs <- replicate(1000, bootstrap_goals(), simplify = F)

# Combinación de columnas y filas
crossing(
  row = 1:nrow(goals),
  col = 1:ncol(goals)
) %>% 
  # Recolección de celdas en todas las muestras
  mutate(
    indices = map2(row, col, function(x, y) sapply(bs, function(m) m[x, y]))
  ) %>% 
  # Se transforman en factor las columnas row y col
  mutate(
    row = factor(str_glue("Casa: {row-1}")),
    col = factor(str_glue("Visitante: {col-1}"))
  ) %>% 
  # Se desprenden los indices
  unnest(indices) %>%
  # Se eliminan aquellos valores NaN (No son números)
  filter(!is.nan(indices)) %>% 
  # Se eliminan aquellas combinaciones donde solo hay un valor distinto
  group_by(row, col) %>%
  filter(n_distinct(indices) > 1) %>%
  ungroup() %>%
  # Gráfico
  ggplot(aes(indices)) +
  # Paneles por combinación de fila y columna
  # Número de columnas igual a la establecida en tablas anteriores
  # No se eliminan paneles vacíos
  facet_wrap(~ row + col, scales = "free", ncol = ncol(goals), drop = F) +
  geom_histogram(bins = 12) +
  geom_vline(xintercept = 1, color = "red") +
  labs(
    title = "Bootstrap: Probabilidades Conjuntas entre Producto de Probabilidades Marginales",
    subtitle = expression(bar(x)==1 ~ "(rojo) puede indicar independencia"),
    caption = "Paneles vacíos indican menos de un valor distinto",
    x = NULL, y = NULL
  ) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) 
#+
#  ggsave("img/pws04.png", width = 8.4, height = 12)

