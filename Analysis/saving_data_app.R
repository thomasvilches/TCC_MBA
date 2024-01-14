#* pacotes e funcoes -------------------------------------------------------

source("packages.R", encoding = "UTF-8")
source("functions.R", encoding = "UTF-8")



#* leitura da DB -----------------------------------------------------------

db <- conecta_base()
odbc::dbListTables(db)
#* IBGE --------------------------------------------------------------------


##* correlation ibge -------------------------------------------------------------


# Primeira coisa é vermos se existe correlação entre as variáveis

idade <- read_any(db, "ibge_idade")
raca <- read_any(db, "ibge_raca")
alf <- read_any(db, "ibge_alfabetizacao")
renda <- read_any(db, "ibge_renda")

mun_int <- read_any(db, "mun_interesse")

idade <- idade %>%
  select(-1, -total) %>%
  pivot_longer(starts_with("ag"),
               names_to = "group_age",
               values_to = "population_age"
  ) %>%
  group_by(id_mun) %>%
  mutate(
    prop_age = population_age / sum(population_age)
  ) %>%
  select(-population_age) %>%
  ungroup()

raca <- raca %>%
  select(-1, -total) %>%
  pivot_longer(., 2:ncol(.), names_to = "group_race", values_to = "population_race") %>%
  mutate(population_race = replace_na(population_race, 0)) %>%
  filter(group_race != "total") %>%
  group_by(id_mun) %>%
  mutate(
    prop_race = population_race / sum(population_race)
  ) %>%
  select(-population_race) %>%
  ungroup()

alf <- alf %>%
  select(id_mun, total, alf_total) %>%
  mutate(prop_alf = alf_total / total) %>%
  select(id_mun, prop_alf)



ref_idade <- read_any(db, "ref_idades") %>%
  select(groups, idade_en)
idade_int <- idade %>%
  filter(id_mun %in% mun_int$id_mun) %>%
  left_join(ref_idade, by = c("group_age" = "groups")) %>%
  select(-group_age) %>%
  pivot_wider(names_from = idade_en, values_from = prop_age)



ref_raca <- read_any(db, "ref_raca") %>%
  select(groups, raca_en)
raca_int <- raca %>%
  filter(id_mun %in% mun_int$id_mun) %>%
  left_join(ref_raca, by = c("group_race" = "groups")) %>%
  select(-group_race) %>%
  pivot_wider(names_from = raca_en, values_from = prop_race)

alf_int <- alf %>%
  filter(id_mun %in% mun_int$id_mun)

ref_renda <- read_any(db, "ref_renda") %>% select(groups, renda_en)

renda_int <- renda %>%
  filter(id_mun %in% mun_int$id_mun) %>%
  pivot_longer(., 3:ncol(.), names_to = "Grupo", values_to = "Valor") %>%
  left_join(ref_renda, by = c("Grupo" = "groups")) %>%
  group_by(id_mun) %>%
  filter(Grupo != "total") %>%
  mutate(
    p = Valor / sum(Valor)
  ) %>%
  select(id_mun, renda_en, p) %>%
  pivot_wider(names_from = renda_en, values_from = p)




dados <- idade_int %>%
  left_join(raca_int) %>%
  left_join(alf_int) %>%
  left_join(renda_int) %>%
  rename(`Literated proportion` = prop_alf)

names(dados)

dados_ibge <- dados
save(dados, file = "outputs/ibge_dados.RData")


# salvar IBGE cidades -----------------------------------------------------


ibge_cidades <- read_any(db, "ibge_cidades")
save(ibge_cidades, file = "outputs/ibge_cidades.RData")

mun_int <- read_any(db, "mun_interesse")
save(mun_int, file = "outputs/mun_interesse.RData")


# CNES --------------------------------------------------------------------


estabelecimento <- read_any(db, "estabelecimento")
save(estabelecimento, file = "outputs/estabelecimento.RData")


cnes <- read_any(db, "cnes_data_year")

names(cnes) <- gsub("qtleitp", "qtleit0", names(cnes))
# 
# nomes <- c(
#   "Consultório Urg/Emerg", "Repouso Urg/Emerg",
#   "Outros Urg/Emerg", "Consultórios médicos Urg/Emerg",
#   "Clínica Ambulatorial", "Consultório não médico (Ambulat)",
#   "Repouso (Ambulat)", "Outros (Ambulat)", "Centro cirúrgico",
#   "Centro obstétrico", "Neonatal"
# )

nomes <- c(
  "Urg/Emerg room", "Urg/Emerg resting room",
  "Urg/Emerg others", "Urg/Emerg medical room",
  "Outpatient clinic", "Outpatient non-medical room",
  "Outpatient resting room", "Outpatient - others", "Surgical center",
  "Obstetric center", "Newborn center"
)

idx <- list(
  c("01", "02", "03", "04"), c("05", "06", "07", "08"),
  c("09", "10", "11", "12", "13"), c("14"), c("15", "16", "17"),
  c("18"), c("19", "20", "21", "22"), as.character(seq(23, 30)),
  c("31", "32", "33"), c("34", "35", "36", "37"), c("38", "39", "40")
)

vars_list_inst <- lapply(idx, function(x) paste0("qtinst", x))
vars_list_leito <- lapply(idx, function(x) paste0("qtleit", x))

somar <- function(lista, df) {
  lista1 <- lista[lista %in% names(df)]
  rowSums(df[lista1])
}

Newvars <- lapply(vars_list_inst, somar, df = cnes) %>%
  Reduce(cbind, .) %>%
  as.data.frame()


Newvars2 <- lapply(vars_list_leito, somar, df = cnes) %>%
  Reduce(cbind, .) %>%
  as.data.frame()

names(Newvars) <- paste("Rooms -", nomes)
names(Newvars2) <- paste("Beds -", nomes)


cnes <- cnes %>%
  select(seq(1, 15)) %>%
  bind_cols(Newvars) %>%
  bind_cols(Newvars2)


check_n <- function(df) {
  nn <- names(df)
  teste <- rep(FALSE, length(nn))
  for (i in 1:length(nn)) {
    x <- df[[nn[i]]]
    if (!is.numeric(x)) {
      teste[i] <- TRUE
    } else {
      if (sum(x) > 0) {
        teste[i] <- TRUE
      }
    }
  }
  return(teste)
}

sn <- check_n(cnes)
names(sn) <- names(cnes)
sn
cnes <- cnes %>% rename(year = ano)

save(cnes, file = "outputs/cnes_data_year.RData")


# save PCA mun ------------------------------------------------------------


dados_pca_mun <- read.csv("outputs/result_pca_mun.csv")
save(dados_pca_mun, file = "outputs/result_pca_mun.RData")

dados_pca_cnes <- read.csv("outputs/dados_pca_cnes.csv")
save(dados_pca_cnes, file = "outputs/dados_pca_cnes.RData")


# save population ---------------------------------------------------------


pop <- odbc::dbGetQuery(db, "select id_mun, total from ibge_idade")
save(pop, file = "outputs/population.RData")


# save datasus ------------------------------------------------------------


cid <- read_any(db, "cid")
save(cid, file = "outputs/cid.RData")

proc <- read_any(db, "procedimentos")
save(proc, file = "outputs/procedimentos.RData")

sia <- read_any(db, "sia_pa")


sia <- sia %>% 
  filter(id_proc %in% c(929)) %>% 
  group_by(idcnes, id_mun_pct, dt_realiz) %>% 
  summarise(
    produzido = sum(pa_qtdpro),
    aprovado = sum(pa_qtdaprov)
  ) %>% ungroup()


save(sia, file = "outputs/sia_col.RData")
