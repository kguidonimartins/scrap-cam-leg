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
  "httr",
  "XML",
  "bRasilLegis",
  "purrr",
  "future",
  "furrr",
  "parallel",
  "doParallel"
))

if (!require("devtools")) install.packages("devtools")

devtools::install_github("leobarone/bRasilLegis")

# test: get deputados
dep <- bRasilLegis::obterDeputados()

# test: get Projetos de Lei (PL) propostas (aprovadas, rejeitadas e em anadamento) no ano de 2018
pl_ano <- bRasilLegis::listarProposicoes(sigla = "PL", ano = 2018)

############################################################
# #
# get all PLs                        #
# #
############################################################

# PLs available since 1946
ano <- seq(1946, 2018, 1)

# create dir
if (!dir.exists("data")) {
  dir.create("data")
}

# check files in the directory for a download failure; see get_prop function below
(
  dl_pl <- list.files(path = "data/", pattern = "*.csv", all.files = FALSE)
)

# extract year from filenames
(
  aux <- as.numeric(stringr::str_extract(string = dl_pl, pattern = "\\d{4}"))
)

# delete the PLs years that have already been downloaded
(
  ano <- ano[!ano %in% aux]
)

# define a facility function to get all PLs in a multiprocess run
get_prop <- function(ano) {
  tryCatch({
    prop <- bRasilLegis::listarProposicoes(
      sigla = "PL",
      ano = ano
    )
    write.csv(
      x = prop,
      file = paste0(
        "data/",
        ano,
        "_proposicoes.csv"
      ),
      row.names = FALSE
    )
  },
  error = function(e) {
    cat("ERROR: ", conditionMessage(e), "\n")
  }
  )
}

# plan
plan(multiprocess)

# run
furrr::future_map(.x = ano, .f = get_prop, .progress = TRUE)

############################################################
# #
# merge data                        #
# #
############################################################



tmp <- list.files(path = "data", pattern = "*.csv", full.names = TRUE)

dfs <- do.call(
  rbind,
  lapply(
    tmp,
    function(x) read.csv(x,
        header = TRUE,
        stringsAsFactors = FALSE
      )
  )
)

write.csv(x = dfs, file = "PLs_1946-2018.csv", row.names = FALSE)

status_prop <- bRasilLegis::listarSituacoesProposicao()

sigla_prop <- bRasilLegis::listarSiglasTipoProposicao() %>%
  filter(ativa == "True")
