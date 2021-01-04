# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Proyecto: Taller Magallanes Entrena
# Organiza: Austral Fitness ®
# Autor:    Matías Castillo Aguilar

# La finalidad de este código es la de descargar los datos recopilados
# desde los formularios de inscripción, eliminar a aquellas observaciones
# que no cumplen con los requisitos y ajustar las variables al formato
# que usaremos más adelante. 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Objetivos de análisis ---------------------------------------------------

# 1. Generar análisis descriptivos de tendencia central y dispersión de las
# variables de actividad física, calidad de vida y variables demográficas.
#
# 2. Realizar pruebas correlacionales entre variables de actividad física, 
# calidad de vida y variables demográficas.
# 
# 3. Buscar diferencias significativas entre los niveles de las variables
# categóricas en las escalas de las variables de actividad física, calidad
# de vida y variables demográficas.
# 
# 4. Hallar asociaciones entre las variables categóricas de actividad física, 
# calidad de vida y variables demográficas.
# 
# 5. Escribir un reporte con los hallazgos de los análisis para ser entregado
# a los organizadores de la actividad.
# 
# 6. Escribir un artículo científico para ser publicado en una revista de
# carácter científico, luego de su aprobación por un comité de ética científico.

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# 0. Funciones auxiliares -------------------------------------------------

testing <- function(data, variable, by, ...) {
  
  hyp_testing <- function(data, variable, by, ...) {
    result <- list()
    test <- if(all(aggregate(data[[variable]], list(data[[by]] ), 
                  function(x) stats::shapiro.test(x)$p.value)$x > 0.05))
      { if(car::leveneTest(data[[variable]], data[[by]])[1,3] > 0.05)
          { test <- t.test(data[[variable]] ~ data[[by]], var.equal = TRUE)
            test$method <- "Prueba t de Student"; test } else
          { test <- t.test(data[[variable]] ~ data[[by]], var.equal = FALSE)
            test$method <- "Prueba t de Student"; test } } else
            { test <- wilcox.test(data[[variable]] ~ data[[by]], correct = F, exact = F, paired = F)
              test$method <- "Prueba de suma de rangos de Wilcoxon"; test }
    result$p <- test$p.value; result$test <- test$method
    result}
  
  hyp_mult_testing <- function(data, variable, by, ...) {
    result <- list()
    test <- if(all(aggregate(data[[variable]], list(data[[by]] ), 
                  function(x) stats::shapiro.test(x)$p.value)$x > 0.05))
      { if(car::leveneTest(data[[variable]], data[[by]])[1,3] > 0.05)
          { test <- oneway.test(data[[variable]] ~ data[[by]], var.equal = TRUE)
            test$method <- "ANOVA de un factor"; test } else
          { test <- oneway.test(data[[variable]] ~ data[[by]], var.equal = FALSE)
            test$method <- "ANOVA de un factor"; test } } else
            { test <- kruskal.test(data[[variable]] ~ data[[by]])
              test$method <- "Prueba de Kruskall-Wallis"; test }
    result$p <- test$p.value; result$test <- test$method
    result}
  
  if(length(unique(data[[by]]) ) == 2 ) 
    { hyp_testing(data, variable, by) } else
    { hyp_mult_testing(data, variable, by) }
}

# 1. Análisis descriptivos ------------------------------------------------

library(data.table);library(tidyverse)
data <- readRDS(file = "data/muestra.RDS") %>% 
  as.data.table() %>% droplevels()

library(gtsummary)
reset_gtsummary_theme()
theme_gtsummary_language(
  language = "es"
)

table1 <- tbl_summary(data, missing = "no", by = sexo,
            include = c("edad","sexo","met_total","actividad_fisica","sf.dolor_corporal",
                    "sf.funcion_fisica","sf.funcion_social","sf.rol_emocional",
                    "sf.rol_fisico","sf.salud_general","sf.salud_mental","sf.vitalidad",
                    "sf_2","sf.score"),
            label = list(edad ~ "Edad",
                         met_total ~ "MET's /semana",
                         sf.funcion_fisica ~ "Función física",
                         sf.rol_fisico ~ "Rol físico",
                         sf.dolor_corporal ~ "Dolor corporal",
                         sf.salud_general ~ "Salud general",
                         sf.vitalidad ~ "Vitalidad",
                         sf.funcion_social ~ "Función social",
                         sf.rol_emocional ~ "Rol emocional",
                         sf.salud_mental ~ "Salud mental",
                         sf_2 ~ "Evolución de salud",
                         sf.score ~ "SF-36 Score",
                         actividad_fisica ~ "Actividad física"),
          statistic = list(all_continuous() ~ "{mean} ± {sd}",
                           all_categorical() ~ "{n} ({p}%)"),
            type = list(sf.dolor_corporal ~ "continuous",
                        sf.funcion_social ~ "continuous",
                        sf.rol_emocional ~ "continuous",
                        sf.rol_fisico ~ "continuous")) %>% 
    add_p(pvalue_fun = function(x) {style_pvalue(x, digits = 3)},
          test = list(all_continuous() ~ "testing")) %>% 
    add_overall() %>% 
    modify_header(list(label ~ "**Variables**",
                     stat_0 ~ "**Total**", 
                     p.value ~ "**P-valor**")) %>%
    modify_spanning_header(list(paste("stat_",1:2,sep = "") ~ "**Sexo**")) %>% 
    bold_labels() %>% bold_p() 



table2 <- tbl_summary(data, missing = "no", by = actividad_fisica,
            include = c("edad","sexo","met_total","actividad_fisica","sf.dolor_corporal",
                    "sf.funcion_fisica","sf.funcion_social","sf.rol_emocional",
                    "sf.rol_fisico","sf.salud_general","sf.salud_mental","sf.vitalidad",
                    "sf_2","sf.score"),
            label = list(edad ~ "Edad",
                         met_total ~ "MET's /semana",
                         sf.funcion_fisica ~ "Función física",
                         sf.rol_fisico ~ "Rol físico",
                         sf.dolor_corporal ~ "Dolor corporal",
                         sf.salud_general ~ "Salud general",
                         sf.vitalidad ~ "Vitalidad",
                         sf.funcion_social ~ "Función social",
                         sf.rol_emocional ~ "Rol emocional",
                         sf.salud_mental ~ "Salud mental",
                         sf_2 ~ "Evolución de salud",
                         sf.score ~ "SF-36 Score",
                         sexo ~ "Sexo"),
            statistic = list(all_continuous() ~ "{mean} ± {sd}",
                             all_categorical() ~ "{n}/{N} ({p}%)"),
            type = list(sf.dolor_corporal ~ "continuous",
                        sf.funcion_social ~ "continuous",
                        sf.rol_emocional ~ "continuous",
                        sf.rol_fisico ~ "continuous")) %>% 
  add_p(pvalue_fun = function(x) {style_pvalue(x, digits = 3)},
        test = list(all_continuous() ~ "testing")) %>% 
  modify_header(list(label ~ "**Variables**", p.value ~ "**P-valor**")) %>%
  modify_spanning_header(list(paste("stat_",1:3,sep = "") ~ "**Actividad física**")) %>% 
  bold_labels() %>% bold_p() 

# 2. Diferencias de grupos ------------------------------------------------

library(ggstatsplot)
plot1 <- ggplot(data, aes(x = sexo, y = sf.dolor_corporal, fill = sexo)) +
  labs(x = "", y = "Dolor físico\n") +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_ci", width = 0.3) +
  ggpubr::stat_compare_means(label.y = 40, label.x = 0.75) +
  theme_pubr()
plot1 <- set_palette(plot1, "grey") + theme(legend.position = "none")

plot2 <- ggplot(data, aes(x = sexo, y = sf.funcion_social, fill = sexo)) +
  labs(x = "", y = "Función social\n") +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_ci", width = 0.3) +
  ggpubr::stat_compare_means(label.y = 45, label.x = 0.75) +
  theme_pubr()
plot2 <- set_palette(plot2, "grey") + theme(legend.position = "none")

plot3 <- ggplot(data, aes(x = sexo, y = sf.salud_general, fill = sexo)) +
  labs(x = "", y = "Salud general\n") +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_ci", width = 0.3) +
  ggpubr::stat_compare_means(label.y = 60, label.x = 0.75) +
  theme_pubr()
plot3 <- set_palette(plot3, "grey") + theme(legend.position = "none")

plot4 <- ggplot(data, aes(x = sexo, y = sf.score, fill = sexo)) +
  labs(x = "", y = "Promedio SF-36 por área\n") +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_ci", width = 0.3) +
  ggpubr::stat_compare_means(label.y = 60, label.x = 0.75) +
  theme_pubr()
plot4 <- set_palette(plot4, "grey") + theme(legend.position = "none")

library(ggpubr)
plot.sexo <- ggarrange(
  plot1, plot2, plot3, plot4, 
  labels = c("A.","B.","C.","D."),
  align = "hv"
)

plot5 <- ggplot(data, aes(x = actividad_fisica, y = met_total, 
                          fill = actividad_fisica)) +
  labs(x = "", y = "MET's /semana\n") +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_ci", width = 0.3) +
  ggsignif::geom_signif(comparisons = list(
    c("Bajo","Moderado"),c("Moderado","Alto"),c("Bajo","Alto")),
    test = "wilcox.test", test.args = list(var.equal = TRUE), step_increase = 1/20, 
    map_signif_level = function(p)sprintf("p = %.2g", p), 
    margin_top = -1/1.75) +
  theme_pubr()
plot5 <- set_palette(plot5, "grey") + theme(legend.position = "none")

plot6 <- ggplot(data, aes(x = actividad_fisica, y = sf.salud_general, 
                          fill = actividad_fisica)) +
  labs(x = "", y = "Salud general\n") +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_ci", width = 0.3) +
  ggsignif::geom_signif(comparisons = list(
    c("Bajo","Moderado"),c("Moderado","Alto"),c("Bajo","Alto")),
    test = "wilcox.test", test.args = list(var.equal = TRUE), step_increase = 1/18, 
    map_signif_level = function(p)sprintf("p = %.2g", p), 
    margin_top = -1/2.5) +
  theme_pubr()
plot6 <- set_palette(plot6, "grey") + theme(legend.position = "none")

plot7 <- ggplot(data, aes(x = actividad_fisica, y = sf.rol_emocional, 
                          fill = actividad_fisica)) +
  labs(x = "", y = "Rol emocional\n") +
  stat_summary(geom = "bar", fun = "mean", alpha = 0.75) +
  stat_summary(geom = "errorbar", fun.data = "mean_ci", width = 0.3) +
  ggsignif::geom_signif(comparisons = list(
    c("Bajo","Moderado"),c("Moderado","Alto"),c("Bajo","Alto")),
    test = "wilcox.test", test.args = list(var.equal = TRUE), step_increase = 1/14, 
    map_signif_level = function(p)sprintf("p = %.2g", p), 
    margin_top = 0) +
  theme_pubr()
plot7 <- set_palette(plot7, "grey") + theme(legend.position = "none")

plot.acfisica <- ggarrange(
  plot7, 
  ggarrange(plot5, plot6, labels = c("B.","C."),
            align = "hv"),
  nrow = 2,labels = "A."
)

save(list = ls(), file = "output/output.RData")



