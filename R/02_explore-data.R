library(tidyverse)
library(plotly)

theme_set(theme_light())

df <- read_csv(file = "PLs_1946-2018.csv") %>%
  select(
    ano,
    txtNomeAutor,
    txtSiglaPartido,
    txtSiglaUF,
    txtEmenta,
    situacao.descricao
  ) %>%
  mutate(
    Ano = ano,
    Deputado = str_to_title(txtNomeAutor),
    Partido = txtSiglaPartido,
    UF = txtSiglaUF,
    Ementa = txtEmenta,
    Status = situacao.descricao,
    Decada = Ano - (Ano %% 10)
  ) %>%
  select(
    -ano,
    -txtNomeAutor,
    -txtSiglaPartido,
    -txtSiglaUF,
    -txtEmenta,
    -situacao.descricao
  )

df <-
  df %>%
  filter(
    !grepl(pattern = "^Comissão*", x = df$Deputado),
    !grepl(pattern = "^Senado*", x = df$Deputado),
    !grepl(pattern = "^Poder*", x = df$Deputado),
    !grepl(pattern = "^Tribunal*", x = df$Deputado),
    !grepl(pattern = "^Supremo*", x = df$Deputado),
    !grepl(pattern = "^Superior*", x = df$Deputado),
    !grepl(pattern = "^Mesa*", x = df$Deputado),
    !grepl(pattern = "^Ministério*", x = df$Deputado),
    !grepl(pattern = "\n", x = df$Status),
    !grepl(pattern = "^.$", x = df$Status)
  )

############################################################
# #
# Deputados Federais com 20 anos ou mais no cargo      #
# #
############################################################

df %>%
  group_by(Deputado, Ano) %>%
  distinct(Deputado, Ano) %>%
  count() %>%
  group_by(Deputado) %>%
  summarise(TempoExercicio = sum(n)) %>%
  filter(TempoExercicio >= 20) %>%
  ggplot(aes(reorder(Deputado, TempoExercicio), TempoExercicio)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(
    title = "Deputados Federais com 20 anos ou mais no cargo",
    x = ""
  )

dep_com_20 <-
  df %>%
  group_by(Deputado, Ano) %>%
  distinct(Deputado, Ano) %>%
  count() %>%
  group_by(Deputado) %>%
  summarise(TempoExercicio = sum(n)) %>%
  arrange(desc(TempoExercicio)) %>%
  filter(TempoExercicio >= 20)

############################################################
# #
# Número de PLs por Deputado Federal            #
# #
############################################################

df %>%
  filter(Deputado %in% dep_com_20$Deputado) %>%
  group_by(Deputado) %>%
  tally() %>%
  ggplot(aes(reorder(Deputado, n), n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Projetos de Lei por Deputados Federais",
    x = "", y = "Quantidade de Projetos de Lei"
  )

############################################################
# #
# Status das PLs == "^Aguardando*"             #
# #
############################################################

aguard_igual <-
  df %>%
  filter(Deputado %in% dep_com_20$Deputado) %>%
  filter(grepl(pattern = "^Aguardando*", x = Status)) %>%
  group_by(Deputado, Status) %>%
  tally() %>%
  ggplot(aes(reorder(Deputado, n), n, fill = Status)) +
  geom_col() +
  coord_flip()

aguard_igual

ggplotly(aguard_igual)

############################################################
# #
# Status das PLs != "^Aguardando*"             #
# #
############################################################

aguard_diff <-
  df %>%
  filter(Deputado %in% dep_com_20$Deputado) %>%
  filter(!grepl(pattern = "^Aguardando*", x = Status)) %>%
  group_by(Deputado, Status) %>%
  tally() %>%
  ggplot(aes(reorder(Deputado, n), n, fill = Status)) +
  geom_col() +
  coord_flip()

aguard_diff

ggplotly(aguard_diff)

############################################################
# #
# Deputados com mais de 20 anos e em exercício atualmente  #
# #
############################################################

dep_hoje <-
  df %>%
  filter(Deputado %in% dep_com_20$Deputado) %>%
  filter(Ano %in% c(2014, 2016, 2017, 2018)) %>%
  distinct(Deputado)

dep_hoje

############################################################
# #
# Status dos PLs == "^Aguardando*"             #
# dos Dep. Fed. em exercício                #
# #
############################################################


hoje_aguard_igual <-
  df %>%
  filter(Deputado %in% dep_hoje$Deputado) %>%
  filter(grepl(pattern = "^Aguardando*", x = Status)) %>%
  group_by(Deputado, Status) %>%
  tally() %>%
  ggplot(aes(reorder(Deputado, n), n, fill = Status)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Projetos de Lei com Situação igual a 'Aguardando'",
    subtitle = "Projetos de Lei de Dep. Federais com 20 anos ou mais no cargo e que atualmente estão em exercício.",
    x = "", y = "Quantidade de Projetos de Lei"
  )

hoje_aguard_igual

ggplotly(hoje_aguard_igual)

############################################################
# #
# Status dos PLs != "^Aguardando*"             #
# dos Dep. Fed. em exercício                #
# #
############################################################

hoje_aguard_diff <-
  df %>%
  filter(Deputado %in% dep_hoje$Deputado) %>%
  filter(!grepl(pattern = "^Aguardando*", x = Status)) %>%
  group_by(Deputado, Status) %>%
  tally() %>%
  ggplot(aes(reorder(Deputado, n), n, fill = Status)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Projetos de Lei com Situação diferente de 'Aguardando'",
    subtitle = "Projetos de Lei de Dep. Federais com 20 anos ou mais no cargo e que atualmente estão em exercício.",
    x = "", y = "Quantidade de Projetos de Lei"
  )

hoje_aguard_diff

fggplotly(hoje_aguard_diff)




















############################################################
# #
# junk                           #
# #
############################################################

dep_com_20 <-
  df %>%
  group_by(Deputado, Ano) %>%
  distinct(Deputado, Ano) %>%
  count() %>%
  group_by(Deputado) %>%
  summarise(TempoExercicio = sum(n)) %>%
  arrange(desc(TempoExercicio)) %>%
  filter(TempoExercicio >= 20)

n_prop <-
  df %>%
  filter(Deputado %in% dep_com_20$Deputado) %>%
  group_by(Deputado) %>%
  tally()

dep_prop <- full_join(dep_com_20, n_prop)

dep_hoje <-
  df %>%
  filter(Deputado %in% dep_com_20$Deputado) %>%
  filter(Ano %in% c(2014, 2016, 2017, 2018)) %>%
  distinct(Deputado)

dep_prop %>%
  filter(Deputado %in% dep_hoje$Deputado) %>%
  mutate(prop = (n / TempoExercicio)) %>%
  ggplot(aes(reorder(Deputado, prop), prop)) +
  geom_col() +
  coord_flip()
