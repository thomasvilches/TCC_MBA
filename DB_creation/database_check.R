

db <- conecta_base()
dbListTables(db)


read_any(db, "procedimentos")
cidades <- read_any(db, "ibge_cidades")

Encoding(cidades[["cidade"]]) <- "ASCII"

declare_utf8 <- function(x) {
  Encoding(x) <- "latin1"
  x
}

cidades %>% mutate_if(is.character, declare_utf8) %>% pull() %>% 
  iconv(from = "latin1", to = "UTF-8") %>% 
  iconv(from = "UTF-8", to = "ASCII")

codes <- stringi::stri_enc_mark(cidades$cidade)

for(i in 1:length(codes))
  cidades$cidade[i] <- iconv(cidades$cidade[i], from = codes[i], to = "ASCII")

stringi::stri_enc_toutf8(cidades$cidade) %>% 
  iconv(from = "UTF-8", to = "latin1")
 
odbc::dbGetQuery(
  db, paste0("SELECT * FROM public.", "ibge_cidades",";")
)
 



dados_raw %>% 
  group_by(PA_PROC_ID) %>% 
  summarise(
    n = length(unique(PA_NIVCPL))
  ) %>% filter(n > 1)


dados_raw %>% pull(PA_PROC_ID) %>% unique %>% length

odbc::dbListTables(con)
read_any(con, "procedimentos") %>% head

read_any(con, "cnes_data") %>% 
  group_by(cnes) %>% 
  summarise(
    n = length(unique(cpf_cnpj)),
    ll = paste(unique(cpf_cnpj), collapse = "; ")
  ) %>% filter(n > 1)



