#! /usr/bin/env Rscript

# ipak function: install and load multiple R packages.
# Check to see if packages are installed.
# Install them if they are not, then load them into the R session.
# Forked from: https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(sapply(pkg, require, character.only = TRUE))
}

ipak(c(
  "tidyverse",
  "httr",
  "XML",
  "bRasilLegis",
  "purrr",
  "future",
  "furrr",
  "parallel",
  "doParallel"
))


siglas <- bRasilLegis::listarSiglasTipoProposicao() %>%
  filter(ativa == "True")

verifica_sigla <- function(sigla) {
  Sys.sleep(time = 5)
# Conferencia das siglas com base no ano de 2017
  tent <- try(bRasilLegis::listarProposicoes(
    sigla = trimws(sigla),
    ano = 2017
  ),
  silent = TRUE
  )
  if (is(tent, "try-error")) {
# retorne "erro" se as infos para a sigla não estiverem disponíveis
    print("erro")
  } else {
# retorne "ok" se as infos para a sigla estiverem disponíveis
    print("ok")
  }
}


future::plan(multiprocess)

res_disp_siglas <- siglas %>%
  mutate(
    disponivel_l = furrr::future_map(
      .x = tipoSigla,
      .f = verifica_sigla,
      .progress = TRUE
    )
  )

res_disp_siglas_dl <-
  res_disp_siglas %>%
  mutate(disponivel = unlist(disponivel_l)) %>%
  mutate(tipoSigla = str_trim(tipoSigla)) %>%
  select(-disponivel_l)

write.csv(x = res_disp_siglas_dl, file = "siglas_disponiveis.csv", row.names = FALSE)
