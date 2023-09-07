library(inedata)
library(tidyverse); library(glue)
library(tictoc)
source('R/get_catalog.R')
source('R/get_columns.R')
source('R/get_data.R')

catalogo = get_catalog()
catalogo %>% filter(survey == 'esi') %>% view
esi_bind = catalogo %>% filter(survey == 'esi') %>%
  pull(version) %>% imap_dfr(~get_data('esi', version =.x) %>%
                               mutate(across(everything(), as.character)))

catalogo %>% filter(survey == 'esi')

e = get_data('esi', version ='2017-personas')

col_na = ene_bind %>% summarise(across(everything(), ~sum(is.na(.x)) )) %>% t
a = get_data(dataset = 'epf_personas', version = 'viii')

?get_catalog
catalogo %>% ungroup
catalogo_ene = get_catalog('esi')
get_columns('esi', '12')

a = get_data('esi', version = '2010-hogares-sin-becas')


version = 'a'

esi = get_data('esi', version = '2010-hogares-con-becas', col_list = c('tme', 'b19jh'))
esi = get_many_data('esi',versions = c('2013-personas', '2020-personas'),
                    col_list = c('idrph', 'conglomerado'))



esi[1]
