library(dplyr)
library(ggplot2)
library(haven)
# tabla de casen 2020 con selección de variables
casen2020 = read_dta("data/casen_2020_small.dta")
# construcción de variabe de resumen de pobreza
casen2020 = casen2020 %>% 
  mutate(pobreza2 = case_when(pobreza == 1 ~ 1,
                              pobreza == 2 ~ 1,
                              pobreza == 3 ~ 0,
                              is.na(pobreza) ~ 0)) %>% 
  select(region, pobreza2, expr)
# Preparar datos para visualización
tabla = casen2020 %>% 
  group_by(region) %>% 
  summarise(pob_tot = sum(expr),
            pobres = sum(pobreza2 * expr),
            por_pob = round((pobres / pob_tot) * 100,1)) %>% 
  select(-pob_tot, -pobres)
# Plot de personas pobres según región
ggplot(data = tabla, aes(x = as.factor(region), y = por_pob)) + 
  geom_bar(stat = "identity", fill = "orange") + 
  geom_text(aes(label= por_pob, vjust=0)) +
  geom_hline(aes(yintercept = mean(por_pob))) +
  labs(title="Porcentaje de personas pobres por región en Casen 2020",
       x ="Región numérica", y = "Porcentaje") +
  theme(plot.title = element_text(hjust = 0.5))