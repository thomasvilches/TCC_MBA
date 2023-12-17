
# pacotes e funcoes -------------------------------------------------------

source("packages.R", encoding = "UTF-8")
source("functions.R", encoding = "UTF-8")



# leitura da DB -----------------------------------------------------------

db <- conecta_base()


# IBGE --------------------------------------------------------------------


## correlation ibge -------------------------------------------------------------


# Primeira coisa é vermos se existe correlação entre as variáveis

idade <- read_any(db, "ibge_idade")
raca <- read_any(db, "ibge_raca")
alf <- read_any(db, "ibge_alfabetizacao")

mun_int <- read_any(db, "mun_interesse")

idade <- idade %>% 
  select(-1, -total) %>% 
  pivot_longer(starts_with("ag"), names_to = "group_age",
               values_to = "population_age") %>% 
  group_by(id_mun) %>% 
  mutate(
    prop_age = population_age/sum(population_age)
  ) %>% select(-population_age) %>% ungroup()

raca <- raca %>% 
  select(-1, -total) %>% 
  pivot_longer(., 2:ncol(.), names_to = "group_race", values_to = "population_race") %>% 
  mutate(population_race = replace_na(population_race, 0)) %>% 
  filter(group_race != "total") %>% 
  group_by(id_mun) %>% 
  mutate(
    prop_race = population_race/sum(population_race)
  ) %>% select(-population_race) %>% ungroup()

alf <- alf %>% 
  select(id_mun, total, alf_total) %>%
  mutate(prop_alf = alf_total/total) %>% 
  select(id_mun, prop_alf)



ref_idade <- read_any(db, "ref_idades") %>% 
  select(groups, idade_pt)
idade_int <- idade %>% 
  filter(id_mun %in% mun_int$id_mun) %>% 
  left_join(ref_idade, by = c("group_age" = "groups")) %>% 
  select(-group_age) %>% 
  pivot_wider(names_from = idade_pt, values_from = prop_age)
  

ref_raca <- read_any(db, "ref_raca") %>% 
  select(groups, raca_pt)
raca_int <- raca %>% 
  filter(id_mun %in% mun_int$id_mun) %>% 
  left_join(ref_raca, by = c("group_race" = "groups")) %>% 
  select(-group_race) %>% 
  pivot_wider(names_from = raca_pt, values_from = prop_race)

alf_int <- alf %>% 
  filter(id_mun %in% mun_int$id_mun)


file_path= "plots/correlation_ibge.png"
png(height=8, width=8, units = "in", file=file_path, type = "cairo", res = 300)

# Your function to plot image goes here

idade_int %>% 
  left_join(raca_int) %>% 
  left_join(alf_int) %>% 
  rename(`Proporção de alfabetizados` = prop_alf) %>% 
  select(-1) %>% 
  as.matrix() %>% 
  cor() %>% 
  corrplot("pie", type = "lower", tl.col = "black")


# Then
dev.off()



## K-means -----------------------------------------


dados <- idade_int %>% 
  left_join(raca_int) %>% 
  left_join(alf_int) %>% 
  rename(`Proporção de alfabetizados` = prop_alf)


df <- dados %>% 
  mutate_at(2:21, ~ as.vector(scale(.)))

df %>% 
  select(-1) %>% 
fviz_nbclust(kmeans, method = "wss")

df %>% 
  select(-1) %>% 
fviz_nbclust(kmeans, method = "silhouette")+
  labs(x = "Número de agrupamentos", y = "Largura média da silhueta",
       title = "Número ótimo de agrupamentos")+
  geom_vline(xintercept = 4, linetype = "dotted")+
  scale_y_continuous(labels = virgula)+
  tema+
  theme(
    plot.title = element_blank()
  )

ggsave("plots/silhueta.png", device = "png", dpi = 300, width = 4, height = 3)

# Teste com 2
k2 <- df %>% 
  select(-1) %>% 
  kmeans(2)
k2$cluster

# Teste com 4
k4 <- df %>% 
  select(-1) %>% 
  kmeans(4)
k4$cluster


## PCA ---------------------------------------------------------------------


pca <- dados %>% 
  select(-id_mun) %>% 
  psych::principal(
    r = .,
    nfactors = ncol(.),
    scores = TRUE,
    rotate = "none"
  )


pca$Vaccounted
pca$values

# 4 variáveis
df_f <- as.data.frame(pca$scores[, 1:4])
df_f$id_mun <- dados$id_mun
df_f$cluster <- as.factor(k4$cluster)


df_f %>% 
  as.data.frame() %>% 
  ggplot()+
  geom_point(aes(x = PC1, y = PC2, color = cluster, fill = cluster), shape = 21, size = 2, stroke = 1)+
  scale_color_manual(values = paleta)+
  scale_fill_manual(values = scales::alpha(c(paleta), 0.5))+
  labs(color = "Grupo", fill = "Grupo")+
  tema


ggsave("plots/PCA.png", device = "png", dpi = 300, width = 4.5, height = 4)



file_path= "plots/correlation_pca.png"
png(height=4, width=10, units = "in", file=file_path, type = "cairo", res = 300)

# Your function to plot image goes here

dados %>% 
  select(-id_mun) %>% 
  as.matrix() %>% 
  cor(pca$scores[, 1:4]) %>%
  t() %>% 
  corrplot("pie", tl.col = "black")



# Then
dev.off()


### Plotly ------------------------------------------------------------------


l <- list(
  font = list(
    family = "sans-serif",
    size = 20,
    color = "#000"),
  bgcolor = "white",
  bordercolor = "black",
  borderwidth = 2, x = 0.1, y = 0.1, title=list(text='<b> Grupo </b>'))

df_f %>% 
  left_join(mun_int) %>% 
plot_ly(
  x = ~PC1, y = ~PC2, color = ~cluster, type = "scatter", mode = "markers",
  alpha = 0.7, size = 2,
  text = ~cidade,
  hoverinfo = 'text'
) %>%
  # add_trace(
  # ) %>% 
layout( xaxis =  list(title = list(text = '<b>PC1</b>', font = list(size = 20))),
        yaxis = list(title = list(text = '<b>PC2</b>', font = list(size = 20))),
        legend=l)



# CNES --------------------------------------------------------------------

odbc::dbListTables(db)

cities <- read_any(db, "ibge_cidades")
estabelecimento <- read_any(db, "estabelecimento")
cnes <- read_any(db, "cnes_data_year")

pop <- odbc::dbGetQuery(db, "select id_mun, total from ibge_idade")



nrow(cnes)

names(cities)
names(estabelecimento)
names(cnes)

df <- cnes %>% 
  left_join(estabelecimento, by = c("id_cnes" = "idcnes")) %>% 
  left_join(cities, by = c("id_mun" = "idmun"))

df %>%
  group_by(id_mun, cidade, ano) %>% 
  summarise(
    n = length(unique(id_cnes))
  ) %>% 
  left_join(pop, by = "id_mun") %>% 
  ungroup() %>% 
  mutate(
    prop = n*100000/total,
    ano = factor(ano)
  ) %>% 
  ggplot()+
  geom_histogram(aes(x = prop, fill = ano), color = 'black')+
  labs(x = "Número de estabelecimentos por 100 mil habitantes", y = "Contagem")+
  facet_wrap(.~ano)+
  scale_fill_manual(values = scales::alpha(c(paleta), 1))+
  tema+
  theme(
    legend.position = "none"
  )


## GIS ---------------------------------------------------------------------

sp <- sf::read_sf("../Dados/GIS/cidades_SP.shp")

plot(sp$geometry)

br <- sf::read_sf("../Dados/GIS/BRMUE250GC_SIR.shp")
br

sp <- br %>% 
  filter(grepl('^35', CD_GEOCMU))

plot(sp$geometry)

names(df)

df %>%
  group_by(id_mun, cidade, codibge, ano) %>% 
  summarise(
    n = length(unique(id_cnes))
  ) %>% 
  left_join(pop, by = "id_mun") %>% 
  ungroup() %>% 
  left_join(sp, by = c("codibge" = "CD_GEOCMU")) %>% 
  mutate(
    prop = n*100000/total,
    ano = factor(ano)
  ) %>% 
  ggplot()+
  geom_sf(aes(geometry = geometry, fill = prop))+
  facet_wrap(.~ano)+
  scale_fill_viridis_c()+
  labs(fill = "Número de estabelecimentos por 100 mil habitantes")+
  # scale_fill_manual(values = scales::alpha(c(paleta), 1))+
  tema+
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5, barwidth = 10, barheight = 1))+
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.direction = "horizontal"
  )



ggsave("plots/map_cnes.png", device = "png", dpi = 300, width = 9, height = 6)




df %>%
  group_by(id_mun, cidade, codibge, ano) %>% 
  summarise(
    n = length(unique(id_cnes))
  ) %>% 
  left_join(pop, by = "id_mun") %>% 
  ungroup() %>% 
  left_join(sp, by = c("codibge" = "CD_GEOCMU")) %>% 
  mutate(
    prop = n*100000/total,
    ano = factor(ano)
  ) %>% 
  arrange(desc(prop))


## Correlacao --------------------------------------------------------------


