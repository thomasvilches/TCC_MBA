# Trabalho de conclusão de curso

Thomas Nogueira Vilches


# DB Creation
## TO DO

- CNES:
  - Organizar os índices do que já foi adicionado
  - COncatenar com os municipios IBGE
  - Criar tabela com webscraping do dicionario da Fiocruz - *Não necessário agora*
  - Pegar códigos de Regiões, micro regiões, etc - *Não necessário agora*
- Cidades
  - Dados IBGE
    - Idade OK
    - Sexo OK
    - Alfabetizacao OK
    - Cor/Raça OK
    - Renda OK
    - Outros?
- Dados SIA-PA
  - Substituir os índices com as tabelas consolidadas
  - arrumar as datas OK
- *GERAL*
  - Consolidar o diagrama com a modelagem da base relacional
  
  ## Workflow
  
  - Leitura_atb: tabelas auxiliares de atributos SIA-PA
  - IBGE: processa dados IBGE
  - CNES: processa os dados CNES
  - data_extraction: processa os dados SIA-PA
  
