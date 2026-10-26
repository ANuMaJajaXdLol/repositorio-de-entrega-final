library(tidyverse)

# Aquí se leen los datos
datos2019 <- read_csv("Datos_molec_2019-1.csv")
datos2020 <- read_csv("Datos_molec_2020-1.csv")
datos2021 <- read_csv("Datos_molec_2021-1.csv")
datos2022 <- read_csv("Datos_molec_2022-1.csv")
datos2023 <- read_csv("Datos_molec_2023-1.csv")
datos2024 <- read_csv("Datos_molec_2024-1.csv")

#Acá hacemos corrección de formato del periodo que no coincide en todas las tablas, para poder hacer una tabla conjunta 
tabla_para_estudiar_proporciones2019 <- datos2019 |> 
  select(p7,p13,p19,periodo)
tab_est_prop2020 <- datos2020 |> 
  select(p7,p13,p19,periodo)
tab_est_prop2021 <- datos2021 |> 
  select(p7,p13,p19,periodo)
tab_est_prop2022 <- datos2022 |> 
  select(p7,p13,p19,periodo)
tab_est_prop2023 <- datos2023 |> 
  select(p7,p13,p19,periodo)
tab_est_prop2024 <- datos2024 |> 
  select(p7,p13,p19,periodo)

lista_tablas <- list(
  tabla_para_estudiar_proporciones2019,
  tab_est_prop2020,
  tab_est_prop2021,
  tab_est_prop2022,
  tab_est_prop2023,
  tab_est_prop2024
)

lista_tablas <- lapply(lista_tablas, function(i) {
  i |> mutate(periodo = as.numeric(periodo))
})


# Combinar todas las tablas
tabla_suprema <- bind_rows(lista_tablas)




# Graficooooos
tabla_suprema |> 
  ggplot(aes(p7, fill=factor(periodo))) + geom_bar(position = "dodge") + scale_y_continuous(
    name = "Frecuencia absoluta",
    sec.axis = sec_axis(
      ~ . / sum(table(tabla_suprema$p7)),
      name = "Proporción"
    ) 
  ) + labs(fill = "Periodo")
tabla_suprema |> 
  ggplot(aes(p13, fill=factor(periodo))) + geom_bar(position = "dodge") + scale_y_continuous(
    name = "Frecuencia absoluta",
    sec.axis = sec_axis(
      ~ . / sum(table(tabla_suprema$p13)),
      name = "Proporción"
    )
  ) + labs(fill = "Periodo")
tabla_suprema |> 
  ggplot(aes(p19, fill=factor(periodo))) + geom_bar(position = "dodge") + scale_y_continuous(
    name = "Frecuencia absoluta",
    sec.axis = sec_axis(
      ~ . / sum(table(tabla_suprema$p19)),
      name = "Proporción"
    )
  ) + labs(fill="Periodo")

# Análisis de estadísiticos

exitos2019 <- sum(tabla_para_estudiar_proporciones2019$p7>2)

prop.test(exitos2019,length(tabla_para_estudiar_proporciones2019$p7),conf.level = 0.96, correct = FALSE)

exitos2020 <- sum(tab_est_prop2020$p7>2)
  

prop.test(exitos2020,length(tab_est_prop2020$p7),conf.level = 0.96, correct = FALSE)

exitos2021 <- sum(tab_est_prop2021$p7>2)

prop.test(exitos2021,length(tab_est_prop2021$p7),conf.level = 0.96, correct = FALSE)

exitosglobales <- sum(tabla_suprema$p7>2)
prop.test(exitosglobales,length(tabla_suprema$p7),conf.level = 0.96, correct = FALSE)

exitos19v24 <- c(sum(tabla_para_estudiar_proporciones2019$p7>2),sum(tab_est_prop2024$p7>2))
total19v24 <- c(length(tabla_para_estudiar_proporciones2019$p7),length(tab_est_prop2024$p7))
prop.test(exitos19v24,total19v24, alternative = "two.sided",conf.level = 0.96, correct = FALSE)

exitos20v21 <- c(sum(tab_est_prop2020$p7>2),sum(tab_est_prop2021$p7>2))
total20v21 <- c(length(tab_est_prop2020$p7),length(tab_est_prop2021$p7))
prop.test(exitos20v21,total20v21,alternative = "two.sided",conf.level = 0.96,correct = FALSE)

p19exitos19v24 <- c(sum(tabla_para_estudiar_proporciones2019$p19>2),sum(tab_est_prop2020$p19>2))
p19total19v24 <- c(length(tabla_para_estudiar_proporciones2019$p19),length(tab_est_prop2020$p19))
prop.test(p19exitos19v24,p19total19v24,alternative = "two.sided", conf.level = 0.96,correct = FALSE)


