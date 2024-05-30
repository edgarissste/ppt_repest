library(pacman)
p_load(here, tidyverse, readxl, 
       extrafont, grid, gtable,
       janitor, ggrepel, scales, 
       data.table)
# extrafont::font_import()

fecha_inicio_ant <- ymd("2024-04-15")
fecha_fin_ant <- ymd("2024-04-30")
fecha_inicio <- ymd("2024-05-01")
fecha_fin <- ymd("2024-05-15")

#cecum<-df_cecum

df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
         quirofanos, tipo, tipo_ordenado, clasificacion, rep, hospital_42)

# Cambio 42 hosp/rep
rep_vect <- unique(df_cum$rep)[!is.na(unique(df_cum$rep))]
#rep_vect <- "SI"


for (rep_actual in rep_vect){
  
  #### Consultas ####
  
  load(file=here("01 datos", "df_consultas.RData"))
  df_cons_abr24<- df_consultas %>% 
    filter(!tipologia %in% c("CMN", "HR")) #filter(hospital_42 == "SI")
  df_cons_abr24 <- df_cons_abr24 %>%
    rename("clave_pres" = clave,
           "consultas" = consultas_mes,
           "nombre_corto" = nombre_unidad,
           "tipo" = tipologia,
           "consultas_xconsult_abr24"= promedio_mes,
           "consultas_xconsult_23"= promedio23,
           "incremento"=cambio) 
  df_cons_abr24 <- df_cons_abr24 %>% 
    mutate(nombre_corto = paste(consultorios, " C. | ", nombre_corto, sep=""))
  
  prom_ent <- mean(df_cons_abr24$consultas_xconsult_abr24, na.rm = T) 
  sd_ent <- sd(df_cons_abr24$consultas_xconsult_abr24, na.rm = T)  
  
  df_res <- group_by(df_cons_abr24, tipo) %>% 
    summarise(consultas_xconsult_abr24 = mean(consultas_xconsult_abr24, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo, sep=""))
  
  df_res2 <- df_cons_abr24 %>%
    group_by(tipo) %>%
    summarise(incremento = mean(incremento, na.rm = TRUE)) %>%
    mutate(nombre_corto = paste("Prom. Nacional", tipo))
  
  df_cons_abr24 <- filter(df_cons_abr24, rep == rep_actual) #filter(df_cons_abr24, hospital_42 == rep_actual) 
  
  df_cons_abr24 <- df_cons_abr24 %>% 
    bind_rows(df_res %>% 
                filter(tipo %in% unique(df_cons_abr24$tipo))) %>% 
    arrange(desc(consultas_xconsult_abr24))
  
  order_nom_um <- df_cons_abr24 %>%
    pull(nombre_corto)
  
  df_cons_abr24$nombre_corto <- factor(df_cons_abr24$nombre_corto, levels = unique(order_nom_um))
  
  df_cons_abr24$tipo <- factor(df_cons_abr24$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                              "CMFEQ", "CE", "CMF", "UMF", 
                                                              "CAF", "CMCT"))
  
  
  df_cons_abr24 <- mutate(df_cons_abr24, 
                          compara_prom=case_when(
                            str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                            consultas_xconsult_abr24 > prom_ent + sd_ent ~ "Mucho mayor",
                            consultas_xconsult_abr24 > prom_ent + 0.5 * sd_ent & consultas_xconsult_abr24 <= prom_ent + sd_ent ~ "Mayor",
                            consultas_xconsult_abr24 < prom_ent - sd_ent ~ "Mucho menor",
                            consultas_xconsult_abr24 < prom_ent - 0.5 * sd_ent & consultas_xconsult_abr24 >= prom_ent - sd_ent ~ "Menor",
                            TRUE ~ "Igual"))
  
  total_bars <- nrow(df_cons_abr24)
  
  df_cons_abr24$pos_x <- as.numeric(df_cons_abr24$nombre_corto)
  
  ppgg <- abs(max(df_cons_abr24$consultas_xconsult_abr24, na.rm=T) - min(df_cons_abr24$consultas_xconsult_abr24, na.rm=T))
  
  
  plot_bars <- function(data, filename_suffix) {
    
    
    p <- ggplot(df_cons_abr24, aes(x = nombre_corto, 
                                   y = consultas_xconsult_abr24,
                                   fill = compara_prom)) +
      geom_bar(stat = "identity", position = position_identity()) +
      geom_text(x = df_cons_abr24$nombre_corto[length(unique(df_cons_abr24$nombre_corto))], 
                y = 35, 
                label= "Estándar: 36", color = "#f16913", fontface="bold", size=3, hjust=1)+
      geom_text(aes(x = nombre_corto, 
                    y = consultas_xconsult_abr24 + ppgg * 0.08,
                    color = compara_prom, label = comma(consultas_xconsult_abr24, accuracy = 0.1)), 
                size = 4, fontface = "bold", angle=90,
                family = "Montserrat") +
      geom_hline(yintercept = 36, color = "#f16913", linetype=2) +
      scale_fill_manual(values = c("Mucho mayor" = "#4EA72E", "Mayor" = "#a1d99b", "Promedio" = "#807dba",
                                   "Igual" = "#D9D9D9", "Menor" = "#fcc5c0", "Mucho menor" = "#FF0000")) +
      scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio" = "black",
                                    "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
      theme_minimal() +
      labs(x = "", y = "Consultas diarias por consultorio") +
      theme(
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(family = "Montserrat"),
        strip.text.x = element_text(size = 10, face = "bold")
      ) +
      facet_grid(.~ tipo, scales = "free_x", space ="free_x") +
      annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col = "#9F2241")))
    
    ggsave(plot = p, filename = here("02 graficas", paste("barra consulta_", rep_actual, "_", ".png", sep = "")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  }
  
  plot_bars(df_cons_abr24, "")
  
  df_cons_abr24 <- mutate(df_cons_abr24) %>%
    filter(compara_prom != "Promedio")
  
  df_cons_abr24 <- bind_rows(df_cons_abr24, df_res2 %>% 
                               filter(tipo %in% unique(df_cons_abr24$tipo))) %>% 
    mutate(variacion = case_when(str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                                 incremento>0.05 & incremento < 0.15~ "Aumenta",
                                 incremento>=0.15 ~ "Aumenta mucho",
                                 incremento< -0.05 & incremento > -0.15 ~ "Disminuye",
                                 incremento<= -0.15 ~ "Disminuye mucho",
                                 TRUE ~ "Se mantiene"),
           posicion_text = case_when(incremento>=0 ~ 0.01,
                                     TRUE ~ -0.01),
           posicion_text2 = case_when(incremento>=0 ~ 0,
                                      TRUE ~ 1),) %>%
    arrange(desc(incremento))
  
  order_nom_um <- df_cons_abr24 %>%
    pull(nombre_corto)
  
  df_cons_abr24$nombre_corto <- factor(df_cons_abr24$nombre_corto, levels = unique(order_nom_um))
  df_cons_abr24$tipo <- factor(df_cons_abr24$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                              "CMFEQ", "CE", "CMF", "UMF", 
                                                              "CAF", "CMCT"))
  
  
  df_cons_abr24 <- arrange(df_cons_abr24, desc(tipo_ordenado), nombre_corto)
  total_bars <- nrow(df_cons_abr24)
  
  ppgg <- abs(max(df_cons_abr24$incremento, na.rm=T) - min(df_cons_abr24$incremento, na.rm=T))
  
  plot_consulta_diff_bars <- function(data, filename_suffix) {
    ggplot(df_cons_abr24, aes(x = nombre_corto, 
                              y = incremento,
                              fill = variacion)) +
      geom_bar(stat = "identity") +
      geom_text(aes(x = nombre_corto, 
                    y = posicion_text,
                    hjust = posicion_text2,
                    color = variacion, label = paste0(comma(incremento, accuracy = 0.1), "%")), 
                size = 4, fontface = "bold", angle = 90,
                family = "Montserrat") +
      geom_text(df_cons_abr24 = df_cons_abr24 %>% filter(!is.na(nombre_ent)), aes(x = nombre_corto, 
                                                                                  y = posicion_text - ppgg*0.19,
                                                                                  hjust = posicion_text2,
                                                                                  color = variacion, label = ifelse(grepl("Prom. Na", nombre_corto), "", paste0("(", comma(consultas_xconsult_23, accuracy = 0.1), "-", 
                                                                                                                    comma(consultas_xconsult_abr24, accuracy = 0.1), ")"))), 
                size = 3, angle = 90,
                family = "Montserrat") +
      scale_y_continuous(labels = ~paste0(., "%"))+
      scale_fill_manual(values = c("Aumenta mucho" = "#4EA72E", "Aumenta" = "#a1d99b", "Promedio" = "#807dba",
                                   "Se mantiene" = "#D9D9D9", "Disminuye" = "#fcc5c0", "Disminuye mucho" = "#FF0000")) +
      scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black", "Promedio" = "black",
                                    "Se mantiene" = "black", "Disminuye" = "black",
                                    "Disminuye mucho" = "black")) +
      theme_minimal() +
      labs(x = "", y = "Variación porcentual") +
      theme(
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size = 14),
        axis.text = element_text(size = 8, color = "black"),
        axis.title = element_text(size = 10),
        plot.caption = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        text = element_text(family = "Montserrat"),
        strip.text.x = element_text(size = 10, face = "bold")
      ) +
      facet_grid(~ tipo, scales = "free_x", space = "free_x") +
      annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col = "#9F2241")))
    
    ggsave(plot = last_plot(), filename = here("02 graficas", paste("barra consulta diff_", rep_actual, "_", ".png", sep = "")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  }
  
  plot_consulta_diff_bars(df_cons_abr24, "")
  
  # View(df_cons_abr24 %>% select(tipo, nombre, consultas_xconsult_abr24))
  # View(df_cons_abr24 %>% select(tipo, nombre, incremento))
  # View(t(df_cons_abr24 %>% select(consultas_xconsult_abr24, consultas_xconsult_23)))
  
  #### Cirugias #### 
  
  load(here("01 datos", "df_cirugias.RData"))
  df_cirugias <- filter(df_cirugias, !is.infinite(promedio_mes)) %>% 
    mutate(tipo=tipo_ordenado %>% 
             str_remove_all("^\\d*\\.\\s")) %>% 
    filter(!tipo %in% c("CMN", "HR"))#%>% filter(hospital_42 == "SI")
  
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, ent_corta, consultorios, 
           tipo, tipo_ordenado, rep ) %>% #hospital_42
    filter(rep == rep_actual & tipo_ordenado > 8) 
  
  prom_ent <- mean(df_cirugias$promedio_mes, na.rm = T) 
  sd_ent <- sd(df_cirugias$promedio_mes, na.rm = T) 
  
  df_res <- group_by(df_cirugias, tipo) %>% 
    summarise(promedio_mes = mean(promedio_mes, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_res2 <- mutate(df_cirugias, 
                    incremento = (promedio_mes - promedio23) / 
                      promedio23) %>% 
    group_by(tipo) %>% 
    summarise(incremento = mean(incremento, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  tipos_hay <- unique(df_cum$tipo)
  
  df_cirugias <- left_join(df_cum, df_cirugias %>% 
                             select(-tipo_ordenado), by=c("cve_pres"="clave", "tipo")) %>% 
    mutate(promedio_mes = case_when(is.na(promedio_mes) ~ 0, 
                                    TRUE ~ promedio_mes),
           nombre_corto = paste(quirofanos, " Q. | ", nombre_corto, sep="")
    )
  
  df_cirugias <- df_cirugias %>% bind_rows(df_res %>% 
                                             filter(tipo %in% tipos_hay)) %>% 
    arrange(desc(promedio_mes))
  
  order_nom_um <- df_cirugias %>%
    pull(nombre_corto)
  
  df_cirugias$nombre_corto <- factor(df_cirugias$nombre_corto, levels = unique(order_nom_um))
  
  df_cirugias$tipo <- factor(df_cirugias$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                          "CMFEQ", "CE", "CMF", "UMF", 
                                                          "CAF", "CMCT"))
  
  
  
  df_cirugias <- mutate(df_cirugias, 
                        compara_prom=case_when(
                          str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                          promedio_mes > prom_ent + sd_ent ~ "Mucho mayor",
                          promedio_mes > prom_ent + 0.5 * sd_ent & promedio_mes <= prom_ent + sd_ent ~ "Mayor",
                          promedio_mes < prom_ent - sd_ent ~ "Mucho menor",
                          promedio_mes < prom_ent - 0.5 * sd_ent & promedio_mes >= prom_ent - sd_ent ~ "Menor",
                          TRUE ~ "Igual"))
  
  ppgg <- abs(max(df_cirugias$promedio_mes, na.rm=T) - min(df_cirugias$promedio_mes, na.rm=T))
  
  graf1 <- 
    ggplot(df_cirugias, aes(x = nombre_corto, 
                            y = promedio_mes,
                            fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = promedio_mes + 0.05 * ppgg,
                  color = compara_prom, label=comma(promedio_mes, accuracy = 0.1)), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    geom_text(x = df_cirugias$nombre_corto[length(unique(df_cirugias$nombre_corto))], 
              y = 3.8, 
              label= "Estándar: 4", color = "#f16913", fontface="bold", size=3, hjust=1)+
    geom_hline(yintercept=4, color="#f16913", linetype=2)+
    scale_fill_manual(values = c("Mucho mayor" = "#4EA72E", "Mayor" = "#a1d99b", "Promedio"="#807dba",
                                 "Igual" = "#D9D9D9", "Menor" = "#fcc5c0", "Mucho menor" = "#FF0000")) +
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    theme_minimal() +
    labs(x="", y="Cirugías diarias por quirófano")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    #coord_cartesian(ylim=c(min(df_cirugias$promedio_mes, na.rm=T), max(df_cirugias$promedio_mes, na.rm=T)+ppgg*0.06))+
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra cirugia_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  df_cirugias <- df_cirugias %>%
    filter(compara_prom != "Promedio") %>% 
    mutate(incremento = (promedio_mes - promedio23) / 
             promedio23) %>% 
    bind_rows(df_res2 %>% 
                filter(tipo %in% tipos_hay)) 
  
  df_cirugias <- mutate(df_cirugias, 
                        variacion = case_when(str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                                              incremento>0.05 & incremento < 0.15~ "Aumenta",
                                              incremento>=0.15 ~ "Aumenta mucho",
                                              incremento< -0.05 & incremento > -0.15 ~ "Disminuye",
                                              incremento<= -0.15 ~ "Disminuye mucho",
                                              TRUE ~ "Se mantiene"),
                        posicion_text = case_when(incremento>=0 ~ 0.01,
                                                  TRUE ~ -0.01),
                        posicion_text2 = case_when(incremento>=0 ~ 0,
                                                   TRUE ~ 1)) %>%
    arrange(desc(incremento))
  
  order_nom_um <- df_cirugias %>%
    pull(nombre_corto)
  
  df_cirugias$tipo <- factor(df_cirugias$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                          "CMFEQ", "CE", "CMF", "UMF", 
                                                          "CAF", "CMCT"))
  
  
  df_cirugias$nombre_corto <- factor(df_cirugias$nombre_corto, levels = unique(order_nom_um))
  df_cirugias <- arrange(df_cirugias, desc(tipo_ordenado), nombre_corto)
  
  ppgg <- abs(max(df_cirugias$incremento, na.rm=T) - min(df_cirugias$incremento, na.rm=T))
  
  graf1 <- ggplot(df_cirugias, aes(x = nombre_corto, 
                                   y = incremento,
                                   fill = variacion)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = variacion, label=paste(comma(incremento*100, accuracy = 0.1), "%", sep="")), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    geom_text(data = df_cirugias %>% filter(!is.na(nombre_ent)), 
              aes(x = nombre_corto, 
                  y = posicion_text - ppgg*0.17,
                  hjust = posicion_text2,
                  color = variacion,
                  label = paste0("(", comma(promedio23, accuracy = 0.1), "-", 
                                 comma(promedio_mes, accuracy = 0.1), ")")), 
              size = 3, angle = 90,
              family = "Montserrat") +
    scale_fill_manual(values = c("Aumenta mucho" = "#4EA72E", "Aumenta" = "#a1d99b",  "Promedio"="#807dba",
                                 "Se mantiene" = "#D9D9D9", "Disminuye" = "#fcc5c0", "Disminuye mucho" = "#FF0000")) +
    scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                  "Se mantiene" = "black", "Disminuye" = "black",
                                  "Disminuye mucho" = "black")) +
    scale_y_continuous(labels=scales::percent) +
    theme_minimal() +
    labs(x="", y="Variación porcentual")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra cirugia diff_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  #### Abasto ####
  load(here("01 datos", "df_existencias.RData"))
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, ent_corta, consultorios, 
           tipo, tipo_ordenado, rep) %>% #hospital_42
    filter(rep == rep_actual,
           !tipo %in% c("CMN", "HR"))
  
  df_existencias<-df_existencias %>% filter(!tipologia %in% c("CMN", "HR"))
  
  prom_ent <- mean(df_existencias$prom_abasto, na.rm = T) 
  sd_ent <- sd(df_existencias$prom_abasto, na.rm = T) 
  
  df_res <- group_by(df_existencias, tipologia) %>% 
    summarise(prom_abasto = mean(prom_abasto, na.rm=T)) %>% 
    rename(tipo=tipologia) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_existencias <- left_join(df_existencias, df_cum, by=c("clave"="cve_pres", "rep")) %>% #"hospital_42"
    mutate(prom_abasto = case_when(is.na(prom_abasto) ~ 0, 
                                   TRUE ~ prom_abasto)
    ) %>% 
    dplyr::filter(rep==rep_actual) #(hospital_42==rep_actual) 
  
  df_existencias <-df_existencias %>% 
    bind_rows(df_res %>% 
                filter(tipo %in% unique(df_existencias$tipo))) %>% 
    arrange(desc(prom_abasto))
  
  order_nom_um <- df_existencias %>%
    pull(nombre_corto)
  
  df_existencias$nombre_corto <- factor(df_existencias$nombre_corto, levels = unique(order_nom_um))
  
  df_existencias$tipo <- factor(df_existencias$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                                "CMFEQ", "CE", "CMF", "UMF", 
                                                                "CAF", "CMCT"))
  
  df_existencias <- mutate(df_existencias, 
                           compara_prom=case_when(
                             str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                             prom_abasto >= 98 ~ "Mucho mayor",
                             prom_abasto >= 95 ~ "Mayor",
                             prom_abasto >= 92 ~ "Menor",
                             prom_abasto >= 80 ~ "Mucho menor",
                             TRUE ~ "Mucho menor"))
  
  ppgg <- abs(max(df_existencias$prom_abasto, na.rm=T) - min(df_existencias$prom_abasto, na.rm=T))
  
  graf1 <- ggplot(df_existencias, aes(x = nombre_corto, 
                                      y = prom_abasto,
                                      fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = prom_abasto - ppgg*0.01 ,
                  color = compara_prom, label=paste0(round(prom_abasto,1), "%")), 
              size=4, fontface="bold", hjust = 1, angle = 90,
              family = "Montserrat")+
    scale_y_continuous(labels = ~paste0(., "%"))+
    scale_fill_manual(values = c("Mucho mayor" = "#4EA72E", "Mayor" = "#a1d99b", "Promedio"="#807dba",
                                 "Igual" = "#D9D9D9", "Menor" = "#fcc5c0", "Mucho menor" = "#FF0000")) + #http://127.0.0.1:44511/graphics/plot_zoom_png?width=1856&height=853
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    #scale_y_continuous(limits = c(min(df_existencias$prom_abasto) - 3, 100))+
    #scale_y_continuous(labels = paste0(round(scales, 0), "%"))+ 
    theme_minimal() +
    labs(x="", y="")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) +
    coord_cartesian(ylim = c(min(df_existencias$prom_abasto) - 5, 100))
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra abasto_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  #### Dpn en cero ####
  
  load(here("01 datos", "df_existencias.RData"))
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, ent_corta, consultorios, 
           tipo, tipo_ordenado, rep) %>% #hospital_42
    filter(rep == rep_actual,
           !tipo %in% c("CMN", "HR")) 
  
  df_existencias<-df_existencias %>% 
    filter(!tipologia %in% c("CMN", "HR"))
  
  prom_ent <- mean(df_existencias$proporcion0, na.rm = T) 
  sd_ent <- sd(df_existencias$proporcion0, na.rm = T) 
  
  df_res <- group_by(df_existencias, tipologia) %>% 
    summarise(proporcion0 = mean(proporcion0, na.rm=T)) %>% 
    rename(tipo=tipologia) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_existencias <- left_join(df_existencias, df_cum, by=c("clave"="cve_pres", "rep"))  %>% #"hospital_42"
    filter(rep==rep_actual) %>% #("hospital_42"==rep_actual)
    mutate(proporcion0 = case_when(is.na(proporcion0) ~ 0,
                                   TRUE ~ proporcion0))
  
  df_existencias <- df_existencias %>% 
    bind_rows(df_res %>% 
                filter(tipo %in% unique(df_existencias$tipo))) %>% 
    arrange(proporcion0)
  
  order_nom_um <- df_existencias %>%
    pull(nombre_corto)
  
  df_existencias$nombre_corto <- factor(df_existencias$nombre_corto, levels = unique(order_nom_um))
  
  df_existencias$tipo <- factor(df_existencias$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                                "CMFEQ", "CE", "CMF", "UMF", 
                                                                "CAF", "CMCT"))
  
  df_existencias <- mutate(df_existencias, 
                           compara_prom=case_when(
                             str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                             proporcion0 < 5 ~ "Mucho mayor",
                             proporcion0 < 10 ~ "Mayor",
                             proporcion0 < 15 ~ "Menor",
                             proporcion0 < 20 ~ "Mucho menor",
                             TRUE ~ "Mucho menor"))
  
  ppgg <- abs(max(df_existencias$proporcion0, na.rm=T) - min(df_existencias$proporcion0, na.rm=T))
  
  
  graf1 <- ggplot(df_existencias, aes(x = nombre_corto, 
                                      y = proporcion0,
                                      fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = proporcion0 + 0.1 ,
                  color = compara_prom, label=paste0(round(proporcion0,1), "%")), 
              size=4, fontface="bold", hjust = 0, angle = 90,
              family= "Montserrat")+
    scale_fill_manual(values = c("Mucho mayor" = "#4EA72E", "Mayor" = "#a1d99b", "Promedio"="#807dba",
                                 "Igual" = "#D9D9D9", "Menor" = "#fcc5c0", "Mucho menor" = "#FF0000")) +
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    #scale_y_continuous(limits = c(min(df_existencias$proporcion0) - 3, 100))+
    scale_y_continuous(labels = ~paste0(., "%"))+
    theme_minimal() +
    labs(x="", y="")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    coord_cartesian(ylim=c(min(df_existencias$proporcion0, na.rm=T),
                           max(df_existencias$proporcion0, na.rm=T) + ppgg*0.05))+
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra ceros_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  #### Error dpn ####
  
  load(here("01 datos", "df_existencias.RData"))
  
  df_existencias$exist_prom  <- df_existencias$exist_prom - 1
  df_existencias<-df_existencias %>% 
    filter(!tipologia %in% c("CMN", "HR"))
  
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, ent_corta, consultorios, 
           tipo, tipo_ordenado, rep) %>% #hospital_42
    filter(rep == rep_actual,
           !tipo %in% c("CMN", "HR"))#filter(hospital_42 == rep_actual)
  
  prom_ent <- mean(df_existencias$exist_prom, na.rm = T) 
  sd_ent <- sd(df_existencias$exist_prom, na.rm = T) 
  
  df_res <- group_by(df_existencias, tipologia) %>% 
    summarise(exist_prom = mean(exist_prom, na.rm=T)) %>% 
    rename(tipo=tipologia) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_existencias <- left_join(df_existencias, df_cum, by=c("clave"="cve_pres", "rep"))  %>% #"hospital_42"
    filter(rep==rep_actual) %>% #"hospital_42"
    mutate(exist_prom = case_when(is.na(exist_prom) ~ 0,
                                  TRUE ~ exist_prom))
  
  df_existencias <- df_existencias %>% 
    bind_rows(df_res %>% 
                filter(tipo %in% unique(df_existencias$tipo))) %>% 
    arrange(desc(exist_prom))
  
  order_nom_um <- df_existencias %>%
    pull(nombre_corto)
  
  df_existencias$nombre_corto <- factor(df_existencias$nombre_corto, levels = unique(order_nom_um))
  
  df_existencias$tipo <- factor(df_existencias$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                                "CMFEQ", "CE", "CMF", "UMF", 
                                                                "CAF", "CMCT"))
  
  df_existencias <- mutate(df_existencias, 
                           compara_prom=case_when(
                             str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                             exist_prom > 0.5 ~ "Mucho mayor",
                             exist_prom > 0.15 ~ "Mayor",
                             exist_prom > -0.15 ~ "Menor",
                             exist_prom > -0.5 ~ "Mucho menor",
                             TRUE ~ "Mucho menor"),
                           posicion_text = case_when(exist_prom>=0 ~ 0.01,
                                                     TRUE ~ -0.01),
                           posicion_text2 = case_when(exist_prom>=0 ~ 0,
                                                      TRUE ~ 1)) 
  ppgg <- abs(max(df_existencias$exist_prom, na.rm=T) - min(df_existencias$exist_prom, na.rm=T))
  
  graf1 <- ggplot(df_existencias, aes(x = nombre_corto, 
                                      y = exist_prom,
                                      fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = compara_prom, label=paste(round(exist_prom*100,1), "%", sep="")), 
              size=4, fontface="bold", angle = 90,
              family = "Montserrat")+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = c("Mucho mayor" = "#C14E15", "Mayor" = "#fee391", "Promedio"="#01665e",
                                 "Igual" = "#D9D9D9", "Menor" = "#dadaeb", "Mucho menor" = "#9A2B95")) +
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    #scale_y_continuous(limits = c(min(df_existencias$exist_prom) - 3, 100))+
    theme_minimal() +
    labs(x="", y="")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra error dpn_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  
  
  
  #### Uso de agenda ####
  
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
           quirofanos, tipo, tipo_ordenado, clasificacion, rep) %>% #hospital_42
    filter(!tipo %in% c("CMN", "HR"))
  
  
  df_cirugias_nom <- fread(here("01 datos", 
                                "Cirugias2024_al16May2024.csv")) %>% 
    filter(fec_ope >= fecha_inicio & fec_ope <= fecha_fin,
           tipo == "Planeada") %>% 
    mutate(fec_reg = as_date(fec_reg),
           fec_ope = as_date(fec_ope),
           fecha_reg_progqx = as_date(fecha_reg_progqx),
           agendada = case_when(!is.na(fecha_reg_progqx) ~ "Si",
                                TRUE ~ "No"),
           diff_fechas = fec_ope - fecha_reg_progqx) %>% 
    group_by(cve_pre_uni_med, agendada) %>% 
    summarise(total=n()) %>% 
    pivot_wider(names_from = agendada, values_from = total) %>% 
    mutate(Si = case_when(is.na(Si) ~ 0,
                          TRUE ~ Si),
           No = case_when(is.na(No) ~ 0,
                          TRUE ~ No), 
           promedio_mes = Si / (Si + No)) %>% 
    left_join(df_cum %>% select("cve_pre_uni_med"="cve_pres", tipo, rep,#hospital_42
                                nombre_corto))
  
  
  df_cirugias_nom_ant <- fread(here("01 datos", 
                                    "Cirugias2024_al16May2024.csv")) %>% 
    filter(fec_ope >= fecha_inicio_ant & fec_ope <= fecha_fin_ant,
           tipo == "Planeada") %>% 
    mutate(fec_reg = as_date(fec_reg),
           fec_ope = as_date(fec_ope),
           fecha_reg_progqx = as_date(fecha_reg_progqx),
           agendada = case_when(!is.na(fecha_reg_progqx) ~ "Si",
                                TRUE ~ "No"),
           diff_fechas = fec_ope - fecha_reg_progqx) %>% 
    group_by(cve_pre_uni_med, agendada) %>% 
    summarise(total=n()) %>% 
    pivot_wider(names_from = agendada, values_from = total) %>% 
    mutate(Si = case_when(is.na(Si) ~ 0,
                          TRUE ~ Si),
           No = case_when(is.na(No) ~ 0,
                          TRUE ~ No), 
           promedio_mes = Si / (Si + No))
  
  df_cirugias_nom <- left_join(df_cirugias_nom, df_cirugias_nom_ant %>% 
                                 select(promedio_mes_ant = promedio_mes))
  
  df_cirugias_nom<-df_cirugias_nom %>% 
    filter(!tipo %in% c("CMN", "HR"))
  
  prom_ent <- mean(df_cirugias_nom$promedio_mes, na.rm=T)
  sd_ent  <- sd(df_cirugias_nom$promedio_mes, na.rm=T)
  
  df_res <- group_by(df_cirugias_nom, tipo) %>% 
    summarise(promedio_mes = mean(promedio_mes, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_res2 <- group_by(df_cirugias_nom, tipo) %>% 
    summarise(promedio_mes_ant = mean(promedio_mes_ant, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))

  df_res<-df_res %>% left_join(df_res2)
  
  df_res3<-df_cirugias_nom %>% 
    mutate(incremento=(promedio_mes/promedio_mes_ant)-1,
           incremento=ifelse(is.infinite(incremento), NA, incremento)) %>% 
    group_by(tipo) %>% 
    summarise(incremento = mean(incremento, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  
  # df_res2 <- mutate(df_cirugias, 
  #                   incremento = (promedio_mes - promedio23) / 
  #                     promedio23) %>% 
  #   group_by(tipo) %>% 
  #   summarise(incremento = mean(incremento, na.rm = T)) %>%
  #   mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
  #                               str_remove_all("^\\d*\\.\\s"), sep=""))
  
  
  df_nosocom <- df_cirugias_nom %>% 
    #filter(hospital_42=="SI") %>% 
    filter(!is.na(rep)|!rep==0) %>% 
    filter(rep==rep_actual) %>%
    arrange(desc(promedio_mes))
  
  id_tipo<-unique(df_nosocom$tipo)
  
  df_nosocom<-df_nosocom %>% 
    bind_rows(df_res %>% filter(tipo %in% id_tipo)) %>% 
    arrange(desc(promedio_mes))
  
  order_nom_um <- df_nosocom %>%
    pull(nombre_corto)
  
  df_nosocom$nombre_corto <- factor(df_nosocom$nombre_corto, levels = unique(order_nom_um))
  
  df_nosocom$tipo <- factor(df_nosocom$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                        "CMFEQ", "CE", "CMF", "UMF", 
                                                        "CAF", "CMCT"))
  
  df_nosocom <- mutate(df_nosocom, 
                       compara_prom=case_when(
                         str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                         promedio_mes > .5 ~ "Mucho mayor",
                         promedio_mes > .25 ~ "Mayor",
                         promedio_mes < .1 ~ "Mucho menor",
                         promedio_mes < .2 ~ "Menor",
                         TRUE ~ "Igual"))
  
  ppgg <- abs(max(df_nosocom$promedio_mes, na.rm=T) - min(df_nosocom$promedio_mes, na.rm=T))
  
  graf1 <- ggplot(df_nosocom, aes(x = nombre_corto, 
                                  y = promedio_mes,
                                  fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = promedio_mes + 0.01 *ppgg,
                  color = compara_prom, label=percent(promedio_mes, accuracy=0.1)), 
              size=4, fontface="bold", hjust = 0, angle = 90)+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = c("Mucho mayor" = "#4EA72E", "Mayor" = "#a1d99b", "Promedio"="#807dba",
                                 "Igual" = "#D9D9D9", "Menor" = "#fcc5c0", "Mucho menor" = "#FF0000")) +
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    #scale_y_continuous(limits = c(min(df_nosocom$promedio_mes) - 3, 100))+
    theme_minimal() +
    labs(x="", y="")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    coord_cartesian(ylim=c(min(df_nosocom$promedio_mes, na.rm=T), max(df_nosocom$promedio_mes, na.rm=T)+ppgg*0.08))+
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra uso agenda_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  
  df_cirugias_nom <- df_cirugias_nom %>% 
    filter(rep==rep_actual) %>% 
    mutate(incremento = (promedio_mes - promedio_mes_ant) / 
             promedio_mes_ant) %>%
    bind_rows(df_res3 %>% filter(tipo %in% id_tipo)) %>% 
    #filter(hospital_42 == "SI")
    mutate(incremento=ifelse(is.infinite(incremento), NA, incremento)) 
  
  df_cirugias_nom <- mutate(df_cirugias_nom, 
                            variacion = case_when(
                              grepl("Prom. Na", nombre_corto) ~ "Promedio",
                              incremento>0.05 & incremento < 0.15~ "Aumenta",
                              incremento>=0.15 ~ "Aumenta mucho",
                              incremento< -0.05 & incremento > -0.15 ~ "Disminuye",
                              incremento<= -0.15 ~ "Disminuye mucho",
                              TRUE ~ "Se mantiene"),
                            posicion_text = case_when(incremento>=0 ~ 0.01, #para la posición del valor
                                                      TRUE ~ -0.01),
                            posicion_text2 = case_when(incremento>=0 ~ 0, #para ajustar
                                                       TRUE ~ 1)) %>%
    arrange(desc(incremento))
  
  order_nom_um <- df_cirugias_nom %>%
    pull(nombre_corto)
  
  df_cirugias_nom$tipo <- factor(df_cirugias_nom$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                                  "CMFEQ", "CE", "CMF", "UMF", 
                                                                  "CAF", "CMCT"))
  
  
  df_cirugias_nom$nombre_corto <- factor(df_cirugias_nom$nombre_corto, levels = unique(order_nom_um))
  
  ppgg <- abs(max(df_cirugias_nom$incremento, na.rm=T) - min(df_cirugias_nom$incremento, na.rm=T))
  
  graf1 <- ggplot(df_cirugias_nom, aes(x = nombre_corto, 
                                       y = incremento,
                                       fill = variacion)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = variacion, label=paste(comma(incremento*100, accuracy = 0.1), "%", sep="")), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    geom_text(data = df_cirugias_nom, 
              aes(x = nombre_corto, 
                  y = posicion_text + ppgg*0.15,
                  hjust = posicion_text2,
                  color = variacion,
                  label = ifelse(grepl("Prom. N", nombre_corto), "", paste0("(", comma(promedio_mes_ant*100, accuracy = 0.1), "-", 
                                 comma(promedio_mes*100, accuracy = 0.1), ")"))), 
              size = 3, angle = 90,
              family = "Montserrat") +
    scale_fill_manual(values = c("Aumenta mucho" = "#4EA72E", "Aumenta" = "#a1d99b",  "Promedio"="#807dba",
                                 "Se mantiene" = "#D9D9D9", "Disminuye" = "#fcc5c0", "Disminuye mucho" = "#FF0000")) +
    scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                  "Se mantiene" = "black", "Disminuye" = "black",
                                  "Disminuye mucho" = "black")) +
    scale_y_continuous(labels=scales::percent) +
    theme_minimal() +
    labs(x="", y="Variación porcentual")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra uso agenda diff_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  #### Nosocom ####
  
  df_egresos <- fread(here("01 datos", 
                           "ingreso_egreso Marzo 15 Mayo 2024.csv"))
  
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
           quirofanos, tipo, tipo_ordenado, clasificacion, rep)#hospital_42
  
  df_nosocom <- df_egresos %>% 
    mutate(ingreso = ymd_hms(paste(ing_fec_ing_hos, ing_hora_ing_hos)),
           egreso = ymd_hms(paste(egre_fec_egr_hos, egre_hora_egr_hos))) %>% 
    filter(egreso >= fecha_inicio & egreso <= fecha_fin) %>% 
    filter(egre_cve_mot_egr!=8) %>% 
    group_by(egre_cve_pre_uni_med, infeccion_intrahospitalaria) %>% 
    summarise(total=n()) %>% 
    pivot_wider(names_from = infeccion_intrahospitalaria, values_from = total) %>% 
    clean_names() %>% 
    mutate(si = case_when(is.na(si) ~ 0,
                          TRUE ~ si),
           no = case_when(is.na(no) ~ 0,
                          TRUE ~ no),
           promedio_mes = si / sum(si, no, sin_identificar, na.rm=T)) %>% 
    left_join(df_cum %>% select(cve_pres, nombre, nombre_corto,
                                ent_corta, consultorios, 
                                tipo, tipo_ordenado,
                                rep),
              by=c("egre_cve_pre_uni_med"="cve_pres")) 
  
  df_nosocom_ant <- df_egresos %>% 
    mutate(ingreso = ymd_hms(paste(ing_fec_ing_hos, ing_hora_ing_hos)),
           egreso = ymd_hms(paste(egre_fec_egr_hos, egre_hora_egr_hos))) %>% 
    filter(egreso >= fecha_inicio_ant & egreso <= fecha_fin_ant) %>% 
    filter(egre_cve_mot_egr!=8) %>% 
    group_by(egre_cve_pre_uni_med, infeccion_intrahospitalaria) %>% 
    summarise(total=n()) %>% 
    mutate(infeccion_intrahospitalaria=
             if_else(infeccion_intrahospitalaria %in% c("SI", "NO"), infeccion_intrahospitalaria, "Otro")) %>% 
    pivot_wider(names_from = infeccion_intrahospitalaria, values_from = total) %>% 
    clean_names() %>% 
    mutate(si = case_when(is.na(si) ~ 0,
                          TRUE ~ si),
           no = case_when(is.na(no) ~ 0,
                          TRUE ~ no),
           promedio_mes_ant = si / sum(si, no, na.rm=T)) 
  
  df_nosocom <- left_join(df_nosocom, df_nosocom_ant %>% select(egre_cve_pre_uni_med,
                                                                promedio_mes_ant))
  df_nosocom <- df_nosocom %>% 
    filter(!tipo %in% c("CMN", "HR"))
  
  prom_ent <- mean(df_nosocom$promedio_mes, na.rm=T)
  sd_ent  <- sd(df_nosocom$promedio_mes, na.rm=T)
  
  df_res <- group_by(df_nosocom, tipo) %>% 
    summarise(promedio_mes = mean(promedio_mes, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_res2 <- group_by(df_nosocom, tipo) %>% 
    summarise(promedio_mes_ant = mean(promedio_mes_ant, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_res<-df_res %>% 
    left_join(df_res2)
  
  df_res3<-df_nosocom %>% 
    mutate(incremento=(promedio_mes/promedio_mes_ant)-1,
           incremento=ifelse(is.infinite(incremento), NA, incremento)) %>% 
             group_by(tipo) %>% 
    summarise(incremento = mean(incremento, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  
  # df_res2 <- mutate(df_nosocom, 
  #                   incremento = (promedio_mes - promedio23) / 
  #                     promedio23) %>% 
  #   group_by(tipo) %>% 
  #   summarise(incremento = mean(incremento, na.rm = T)) %>%
  #   mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
  #                               str_remove_all("^\\d*\\.\\s"), sep=""))
  
  
  df_nosocom <- df_nosocom %>% 
    #filter(hospital_42=="SI") %>% 
    filter(rep==rep_actual)
  
  id_tipo<-unique(df_nosocom$tipo)  
  
  df_nosocom<-df_nosocom %>% 
    bind_rows(df_res %>% filter(tipo %in% id_tipo)) %>% 
  arrange(promedio_mes)
  
  order_nom_um <- df_nosocom %>%
    pull(nombre_corto)
  
  df_nosocom$nombre_corto <- factor(df_nosocom$nombre_corto, levels = unique(order_nom_um))
  
  df_nosocom$tipo <- factor(df_nosocom$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                        "CMFEQ", "CE", "CMF", "UMF", 
                                                        "CAF", "CMCT"))
  
  df_nosocom <- mutate(df_nosocom, 
                       compara_prom=case_when(
                         str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                         promedio_mes > .05 ~ "Mucho mayor",
                         promedio_mes > .04 ~ "Mayor",
                         promedio_mes < .01 ~ "Mucho menor",
                         promedio_mes < .02 ~ "Menor",
                         TRUE ~ "Igual"))
  
  ppgg <- abs(max(df_nosocom$promedio_mes, na.rm=T) - min(df_nosocom$promedio_mes, na.rm=T))
  
  graf1 <- ggplot(df_nosocom, aes(x = nombre_corto, 
                                  y = promedio_mes,
                                  fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = promedio_mes + 0.001 ,
                  color = compara_prom, label=percent(promedio_mes,0.1)), 
              size=4, fontface="bold", hjust = 0, angle = 90)+
    geom_hline(yintercept=0, color="#f16913")+
    scale_y_continuous(labels=scales::percent)+
    scale_fill_manual(values = c("Mucho mayor" = "#FF0000", "Mayor" = "#fcc5c0", "Promedio"="#807dba",
                                 "Igual" = "#D9D9D9", "Menor" = "#a1d99b", "Mucho menor" = "#4EA72E")) +
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    #scale_y_continuous(limits = c(min(df_nosocom$promedio_mes) - 3, 100))+
    theme_minimal() +
    labs(x="", y="")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    coord_cartesian(ylim=c(min(df_nosocom$promedio_mes, na.rm=T), max(df_nosocom$promedio_mes, na.rm=T)+ppgg*0.08))+
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra nosocom_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  df_nosocom <- df_nosocom %>% 
    mutate(incremento = (promedio_mes - promedio_mes_ant) / 
             promedio_mes_ant)
    #filter(hospital_42 == "SI")
    
  prom_ent <- mean(df_nosocom$incremento, na.rm=T)
  sd_ent  <- sd(df_nosocom$incremento, na.rm=T)
  
  df_nosocom<-df_nosocom %>% 
    filter(rep==rep_actual)
  
  df_nosocom<-df_nosocom %>% 
    bind_rows(df_res3 %>% filter(tipo %in% id_tipo))
  
  df_nosocom <- mutate(df_nosocom, 
                       variacion = case_when(
                         str_detect(nombre_corto, "Prom. Nac") ~ "Promedio",
                         incremento > prom_ent + sd_ent ~ "Aumenta mucho",
                         incremento > prom_ent + 0.5 * sd_ent ~ "Aumenta",
                         incremento < prom_ent - sd_ent ~ "Disminuye mucho",
                         incremento < prom_ent - 0.5 * sd_ent ~ "Disminuye",
                         TRUE ~ "Se mantiene"),
                       posicion_text = case_when(incremento>=0 ~ 0.01,
                                                 TRUE ~ -0.01),
                       posicion_text2 = case_when(incremento>=0 ~ 0,
                                                  TRUE ~ 1)) %>%
    arrange(incremento)
  
  order_nom_um <- df_nosocom %>%
    pull(nombre_corto)
  
  df_nosocom$tipo <- factor(df_nosocom$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                        "CMFEQ", "CE", "CMF", "UMF", 
                                                        "CAF", "CMCT")) #http://127.0.0.1:44511/graphics/plot_zoom_png?width=1792&height=697
  
  
  df_nosocom$nombre_corto <- factor(df_nosocom$nombre_corto, levels = unique(order_nom_um))
  
  ppgg <- abs(max(df_nosocom$incremento, na.rm=T))+abs(min(df_nosocom$incremento, na.rm=T))
  
  graf1 <- ggplot(df_nosocom, aes(x = nombre_corto, 
                                  y = incremento,
                                  fill = variacion)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = variacion, label=paste(comma(incremento*100, accuracy = 0.1), "%", sep="")), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    geom_text(data = df_nosocom, 
              aes(x = nombre_corto, 
                  y = posicion_text + ppgg*0.2,
                  hjust = posicion_text2,
                  color = variacion,
                  label = ifelse(grepl("Prom. N", nombre_corto), "", paste0("(", comma(promedio_mes_ant * 100, accuracy = 0.1), "-", 
                                 comma(promedio_mes* 100, accuracy = 0.1), "%)"))), 
              size = 3, angle = 90,
              family = "Montserrat") +
    scale_fill_manual(values = c("Aumenta mucho" = "#FF0000", "Aumenta" = "#fcc5c0",  "Promedio"="#807dba",
                                 "Se mantiene" = "#D9D9D9", "Disminuye" = "#a1d99b", "Disminuye mucho" = "#4EA72E")) +
    scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                  "Se mantiene" = "black", "Disminuye" = "black",
                                  "Disminuye mucho" = "black")) +
    scale_y_continuous(labels=scales::percent) +
    theme_minimal() +
    labs(x="", y="Variación porcentual")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra nosocom diff_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  
  #### Estancia promedio de egresos ####
  
  df_egresos <- fread(here("01 datos", 
                           "ingreso_egreso Marzo 15 Mayo 2024.csv"))
  
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
           quirofanos, tipo, tipo_ordenado, clasificacion, rep) #hospital_42
  
  df_estancia <- df_egresos %>% 
    rename(cve_pres=ing_cve_pre_uni_med) %>% 
    mutate(ingreso = ymd(ing_fec_ing_hos),
           egreso = ymd(egre_fec_egr_hos)) %>% 
    filter(egre_cve_mot_egr!=8) %>% 
    filter(egreso >= fecha_inicio & egreso <= fecha_fin) %>% 
    mutate(dura_estancia = (egreso - (ingreso-days(1)))) %>% 
    group_by(cve_pres) %>% 
    summarise(promedio_mes = mean(dura_estancia, na.rm=T)) %>% 
    left_join(df_cum %>% select(cve_pres, nombre, nombre_corto,
                                ent_corta, consultorios, 
                                tipo, tipo_ordenado,
                                rep),
              by=c("cve_pres")) 
  
  df_estancia_ant <- df_egresos %>% 
    rename(cve_pres=ing_cve_pre_uni_med) %>% 
    mutate(ingreso = ymd(ing_fec_ing_hos),
           egreso = ymd(egre_fec_egr_hos)) %>% 
    filter(egre_cve_mot_egr!=8) %>% 
    filter(egreso >= fecha_inicio_ant & egreso <= fecha_fin_ant) %>% 
    mutate(dura_estancia = (egreso - (ingreso-days(1)))) %>% 
    group_by(cve_pres) %>% 
    summarise(promedio_mes_ant = mean(dura_estancia, na.rm=T)) 
  
  df_estancia <- left_join(df_estancia, df_estancia_ant)
  
  df_estancia <- df_estancia %>% 
    #filter(hospital_42=="SI") %>% 
    filter(!tipo %in% c("CMN", "HR")) %>% 
    arrange(promedio_mes) 
  
  df_res <- group_by(df_estancia, tipo) %>% 
    summarise(promedio_mes = mean(promedio_mes, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo, sep=""))

  df_res3<-df_estancia %>% 
    mutate(promedio_mes=as.numeric(promedio_mes),
           promedio_mes_ant=as.numeric(promedio_mes_ant),
      incremento=(promedio_mes/promedio_mes_ant)-1,
           incremento=ifelse(is.infinite(incremento), NA, incremento)) %>% 
    group_by(tipo) %>% 
    summarise(incremento = mean(incremento, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  prom_ent <- mean(df_estancia$promedio_mes, na.rm=T)
  sd_ent  <- sd(df_estancia$promedio_mes, na.rm=T)
  
  
  df_estancia<-df_estancia %>% 
    filter(rep==rep_actual)
  
  id_tipo<-unique(df_estancia$tipo)
  
  df_estancia<-df_estancia %>% 
    bind_rows(df_res %>% filter(tipo %in% id_tipo))
  
  order_nom_um <- df_estancia %>%
    pull(nombre_corto)
  
  df_estancia$nombre_corto <- factor(df_estancia$nombre_corto, levels = unique(order_nom_um))
  
  df_estancia$tipo <- factor(df_estancia$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                          "CMFEQ", "CE", "CMF", "UMF", 
                                                          "CAF", "CMCT"))
  
  df_estancia <- mutate(df_estancia, 
                        compara_prom=case_when(
                          str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                          promedio_mes > prom_ent + sd_ent ~ "Mucho mayor",
                          promedio_mes > prom_ent + 0.5 * sd_ent & promedio_mes <= prom_ent + sd_ent ~ "Mayor",
                          promedio_mes < prom_ent - sd_ent ~ "Mucho menor",
                          promedio_mes < prom_ent - 0.5 * sd_ent & promedio_mes >= prom_ent - sd_ent ~ "Menor",
                          TRUE ~ "Igual"))
  
  ppgg <- abs(max(df_estancia$promedio_mes, na.rm=T)) # Para parámetro que inicia en cero
  
  graf1 <- ggplot(df_estancia, aes(x = nombre_corto, 
                                   y = promedio_mes,
                                   fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = promedio_mes + 0.01 * ppgg ,
                  color = compara_prom, label= round(as.numeric(promedio_mes), 1)), 
              size=4, fontface="bold", hjust = 0, angle = 90)+
    scale_y_continuous(labels=~paste0(., " días"))+
    scale_fill_manual(values = c("Mucho mayor" = "#FF0000", "Mayor" = "#fcc5c0", "Promedio"="#807dba",
                                 "Igual" = "#D9D9D9", "Menor" = "#a1d99b", "Mucho menor" = "#4EA72E")) +
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    #scale_y_continuous(limits = c(min(df_nosocom$promedio_mes) - 3, 100))+
    theme_minimal() +
    labs(x="", y="")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    coord_cartesian(ylim=c(min(df_estancia$promedio_mes, na.rm=T), max(df_estancia$promedio_mes, na.rm=T)+ppgg*0.03))+
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra estancia_", rep_actual,".png", sep="")), 
         width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  df_estancia <- df_estancia %>% 
    mutate(incremento = (as.numeric(promedio_mes) - as.numeric(promedio_mes_ant)) / 
             as.numeric(promedio_mes_ant)) %>% 
    filter(!grepl("Prom. N", nombre_corto)) %>% 
    bind_rows(df_res3 %>% filter(tipo %in% id_tipo))
  #filter(hospital_42 == "SI")
    
  
  prom_ent <- mean(df_estancia$incremento, na.rm=T)
  sd_ent  <- sd(df_estancia$incremento, na.rm=T)
  
  df_estancia <- mutate(df_estancia, 
                        variacion = case_when(
                          grepl("Prom. N", nombre_corto) ~ "Promedio",
                          incremento > prom_ent + sd_ent ~ "Aumenta mucho",
                          incremento > prom_ent + 0.5 * sd_ent ~ "Aumenta",
                          incremento < prom_ent - sd_ent ~ "Disminuye mucho",
                          incremento < prom_ent - 0.5 * sd_ent ~ "Disminuye",
                          TRUE ~ "Se mantiene"),
                        posicion_text = case_when(incremento>=0 ~ 0.01,
                                                  TRUE ~ -0.01),
                        posicion_text2 = case_when(incremento>=0 ~ 0,
                                                   TRUE ~ 1)) %>%
    arrange(incremento)
  
  order_nom_um <- df_estancia %>%
    pull(nombre_corto)
  
  df_estancia$tipo <- factor(df_estancia$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                          "CMFEQ", "CE", "CMF", "UMF", 
                                                          "CAF", "CMCT"))
  
  
  df_estancia$nombre_corto <- factor(df_estancia$nombre_corto, levels = unique(order_nom_um))
  
  ppgg <- abs(max(df_estancia$incremento, na.rm=T) - min(df_estancia$incremento, na.rm=T))
  
  graf1 <- ggplot(df_estancia, aes(x = nombre_corto, 
                                   y = incremento,
                                   fill = variacion)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = variacion, label=paste(comma(incremento*100, accuracy = 0.1), "%", sep="")), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    geom_text(aes(x = nombre_corto, 
                  y = posicion_text + ppgg*0.2,
                  hjust = posicion_text2,
                  color = variacion,
                  label = ifelse(grepl("Prom. N", nombre_corto), "", paste0("(", comma(as.numeric(promedio_mes_ant), accuracy = 0.1), "-", 
                                 comma(as.numeric(promedio_mes), accuracy = 0.1), "%)"))), 
              size = 3, angle = 90,
              family = "Montserrat") +
    scale_fill_manual(values = c("Aumenta mucho" = "#FF0000", "Aumenta" = "#fcc5c0",  "Promedio"="#807dba",
                                 "Se mantiene" = "#D9D9D9", "Disminuye" = "#a1d99b", "Disminuye mucho" = "#4EA72E")) +
    scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                  "Se mantiene" = "black", "Disminuye" = "black",
                                  "Disminuye mucho" = "black")) +
    scale_y_continuous(labels=scales::percent) +
    theme_minimal() +
    labs(x="", y="Variación porcentual")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra estancia diff_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  
  #### Ocupación hospitalaria ####
  # 
  # df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  #   select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
  #          quirofanos, tipo, tipo_ordenado, clasificacion, rep)#hospital_42
  # 
  # df_ocupacion <- df_egresos %>% 
  #   rename(cve_pres=ing_cve_pre_uni_med) %>% 
  #   mutate(ingreso_h = ymd_hms(paste(ing_fec_ing_hos, ing_hora_ing_hos, sep=" ")),
  #          ingreso = ymd(ing_fec_ing_hos),
  #          egreso_h = ymd_hms(paste(egre_fec_egr_hos, egre_hora_egr_hos, sep=" ")),
  #          egreso = ymd(egre_fec_egr_hos)) %>% 
  #   filter(egre_cve_mot_egr!=8 | is.na(egre_cve_mot_egr)) %>% 
  #   filter((!(egreso < as_date(fecha_inicio)) | is.na(egreso)) &
  #            (!(ingreso-days(1) >= as_date(fecha_fin)))) %>% 
  #   mutate(grupo = case_when(ingreso >= as_date(fecha_inicio) &
  #                              is.na(egreso) ~ "G1",
  #                            ingreso >= as_date(fecha_inicio) &
  #                              egreso > as_date(fecha_fin) ~ "G2",
  #                            ingreso >= as_date(fecha_inicio) &
  #                              egreso <= as_date(fecha_fin)  ~ "G3",
  #                            ingreso < as_date(fecha_inicio) &
  #                              is.na(egreso) ~ "G4",
  #                            ingreso < as_date(fecha_inicio) &
  #                              egreso > as_date(fecha_fin) ~ "G5",
  #                            ingreso < as_date(fecha_inicio) &
  #                              egreso <= as_date(fecha_fin) ~ "G6",
  #   )) %>% 
  #   mutate(aporte = case_when(grupo=="G1" ~ as_date(fecha_fin)+days(1) - ingreso,
  #                             grupo=="G2" ~ as_date(fecha_fin)+days(1) - ingreso,
  #                             grupo=="G3" ~ egreso+days(1) - ingreso,
  #                             grupo=="G4" ~ as_date(fecha_fin) + days(1) - as_date(fecha_inicio),
  #                             grupo=="G5" ~ as_date(fecha_fin) + days(1)  - as_date(fecha_inicio),
  #                             grupo=="G6" ~ egreso + days(1)  - as_date(fecha_inicio)))
  # 
  # df_ocupacion_ant <- df_egresos %>% 
  #   rename(cve_pres=ing_cve_pre_uni_med) %>% 
  #   mutate(ingreso_h = ymd_hms(paste(ing_fec_ing_hos, ing_hora_ing_hos, sep=" ")),
  #          ingreso = ymd(ing_fec_ing_hos),
  #          egreso_h = ymd_hms(paste(egre_fec_egr_hos, egre_hora_egr_hos, sep=" ")),
  #          egreso = ymd(egre_fec_egr_hos)) %>% 
  #   filter(egre_cve_mot_egr!=8 | is.na(egre_cve_mot_egr)) %>% 
  #   filter((!(egreso < as_date(fecha_inicio_ant)) | is.na(egreso)) &
  #            (!(ingreso-days(1) >= as_date(fecha_fin_ant)))) %>% 
  #   mutate(grupo = case_when(ingreso >= as_date(fecha_inicio_ant) &
  #                              is.na(egreso) ~ "G1",
  #                            ingreso >= as_date(fecha_inicio_ant) &
  #                              egreso > as_date(fecha_fin_ant) ~ "G2",
  #                            ingreso >= as_date(fecha_inicio_ant) &
  #                              egreso <= as_date(fecha_fin_ant)  ~ "G3",
  #                            ingreso < as_date(fecha_inicio_ant) &
  #                              is.na(egreso) ~ "G4",
  #                            ingreso < as_date(fecha_inicio_ant) &
  #                              egreso > as_date(fecha_fin_ant) ~ "G5",
  #                            ingreso < as_date(fecha_inicio_ant) &
  #                              egreso <= as_date(fecha_fin_ant) ~ "G6",
  #   )) %>% 
  #   mutate(aporte = case_when(grupo=="G1" ~ as_date(fecha_fin_ant)+days(1) - ingreso,
  #                             grupo=="G2" ~ as_date(fecha_fin_ant)+days(1) - ingreso,
  #                             grupo=="G3" ~ egreso+days(1) - ingreso,
  #                             grupo=="G4" ~ as_date(fecha_fin_ant) + days(1) - as_date(fecha_inicio_ant),
  #                             grupo=="G5" ~ as_date(fecha_fin_ant) + days(1)  - as_date(fecha_inicio_ant),
  #                             grupo=="G6" ~ egreso + days(1)  - as_date(fecha_inicio_ant)))
  # 
  # df_ocupacion <- group_by(df_ocupacion, cve_pres) %>% 
  #   summarise(dias_tot = sum(aporte, na.rm=T))
  # 
  # df_ocupacion_ant <- group_by(df_ocupacion_ant, cve_pres) %>% 
  #   summarise(dias_tot_ant = sum(aporte, na.rm=T))
  # 
  # df_camas <- read_excel(here("01 datos", "Camas Censables abr-may 2024.xlsx")) %>% 
  #   # filter(dia >= fecha_inicio & dia <= fecha_fin) %>% 
  #   rename(cve_pres=cve_pre_uni_med, troncal = "servicio troncal") %>% 
  #   group_by(cve_pres, troncal) %>% 
  #   summarise(camas_tot = mean(num_camas, na.rm=T)) %>% 
  #   ungroup() %>% 
  #   group_by(cve_pres) %>% 
  #   summarise(camas_tot = sum(camas_tot, na.rm=T)) %>% 
  #   mutate(camas_tot = camas_tot * as.numeric(as_date(fecha_fin) + days(1) - as_date(fecha_inicio)),
  #          camas_tot = round(camas_tot))
  # 
  # df_ocupacion <- left_join(df_ocupacion, df_camas) %>% 
  #   mutate(promedio_mes=as.numeric(dias_tot)/as.numeric(camas_tot)) %>% 
  #   left_join(df_cum) 
  # 
  # df_ocupacion <- left_join(df_ocupacion, df_ocupacion_ant) %>% 
  #   mutate(promedio_mes_ant=as.numeric(dias_tot_ant)/as.numeric(camas_tot)) 
  # 
  # df_ocupacion <- df_ocupacion %>% 
  #   #filter(hospital_42=="SI") %>% 
  #   filter(!tipo %in% c("CMN", "HR")) %>% 
  #   arrange(promedio_mes)
  # 
  # df_ocupacion<-df_ocupacion %>% 
  #   filter(rep==rep_actual)
  # 
  # prom_ent <- mean(df_ocupacion$promedio_mes, na.rm=T)
  # sd_ent  <- sd(df_ocupacion$promedio_mes, na.rm=T)
  # 
  # order_nom_um <- df_ocupacion %>%
  #   pull(nombre_corto)
  # 
  # df_ocupacion$nombre_corto <- factor(df_ocupacion$nombre_corto, levels = unique(order_nom_um))
  # 
  # df_ocupacion$tipo <- factor(df_ocupacion$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
  #                                                           "CMFEQ", "CE", "CMF", "UMF", 
  #                                                           "CAF", "CMCT"))
  # 
  # df_ocupacion <- mutate(df_ocupacion, 
  #                        compara_prom=case_when(
  #                          str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
  #                          promedio_mes > 0.95 ~ "Mucho mayor",
  #                          promedio_mes > 0.9 ~ "Mayor",
  #                          promedio_mes < 0.8 ~ "Mucho menor",
  #                          promedio_mes < 0.85 ~ "Menor",
  #                          TRUE ~ "Igual"))
  # 
  # graf1 <- ggplot(df_ocupacion, aes(x = nombre_corto, 
  #                                   y = promedio_mes,
  #                                   fill = compara_prom)) +
  #   geom_bar(stat = "identity") +
  #   geom_text(aes(x = nombre_corto, 
  #                 y = promedio_mes + 0.001 ,
  #                 color = compara_prom, label= percent(as.numeric(promedio_mes), 0.1)), 
  #             size=4, fontface="bold", hjust = 0, angle = 90)+
  #   scale_y_continuous(labels=scales::percent)+
  #   scale_fill_manual(values = c("Mucho mayor" = "#FF0000", "Mayor" = "#fcc5c0", "Promedio"="#807dba",
  #                                "Igual" = "#D9D9D9", "Menor" = "#a1d99b", "Mucho menor" = "#4EA72E")) +
  #   scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
  #                                 "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
  #   #scale_y_continuous(limits = c(min(df_nosocom$promedio_mes) - 3, 100))+
  #   theme_minimal() +
  #   labs(x="", y="")+
  #   theme(
  #     legend.title = element_blank(),
  #     legend.position = "none",
  #     legend.text = element_text(size = 14),
  #     axis.text = element_text(size=8, color="black"),
  #     axis.title = element_text(size=10),
  #     plot.caption = element_text(size=9),
  #     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #     text = element_text(family = "Montserrat"),
  #     strip.text.x = element_text(size=10, face = "bold"))+
  #   facet_grid(~ tipo, scales = "free_x", space = "free_x") +
  #   annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  # graf1
  # 
  # ggsave(plot = graf1, here("02 graficas", paste("barra ocupa hosp_", rep_actual,".png", sep="")), 
  #        width = 8.9527559055, height = 5.8740157, dpi = 300)
  # 
  # 
  # df_ocupacion <- df_ocupacion %>% 
  #   mutate(incremento = (as.numeric(promedio_mes) - as.numeric(promedio_mes_ant)) / 
  #            as.numeric(promedio_mes_ant)) %>% 
  #   filter(!tipo %in% c("CMN", "HR"))
  # #filter(hospital_42 == "SI")
  # 
  # df_ocupacion<-df_ocupacion %>% filter(rep==rep_actual)
  # 
  # prom_ent <- mean(df_ocupacion$incremento, na.rm=T)
  # sd_ent  <- sd(df_ocupacion$incremento, na.rm=T)
  # 
  # df_ocupacion <- mutate(df_ocupacion, 
  #                        variacion = case_when(
  #                          incremento>prom_ent + 0.5 * sd_ent~ "Aumenta",
  #                          incremento>=prom_ent + sd_ent ~ "Aumenta mucho",
  #                          incremento< prom_ent + 0.5 * sd_ent ~ "Disminuye",
  #                          incremento<= prom_ent + sd_ent ~ "Disminuye mucho",
  #                          TRUE ~ "Se mantiene"),
  #                        posicion_text = case_when(incremento>=0 ~ 0.01,
  #                                                  TRUE ~ -0.01),
  #                        posicion_text2 = case_when(incremento>=0 ~ 0,
  #                                                   TRUE ~ 1)) %>%
  #   arrange(incremento)
  # 
  # order_nom_um <- df_ocupacion %>%
  #   pull(nombre_corto)
  # 
  # df_ocupacion$tipo <- factor(df_ocupacion$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
  #                                                           "CMFEQ", "CE", "CMF", "UMF", 
  #                                                           "CAF", "CMCT"))
  # 
  # 
  # df_ocupacion$nombre_corto <- factor(df_ocupacion$nombre_corto, levels = unique(order_nom_um))
  # 
  # ppgg <- abs(max(df_ocupacion$incremento, na.rm=T) - min(df_ocupacion$incremento, na.rm=T))
  # 
  # graf1 <- ggplot(df_ocupacion, aes(x = nombre_corto, 
  #                                   y = incremento,
  #                                   fill = variacion)) +
  #   geom_bar(stat = "identity") +
  #   geom_text(aes(x = nombre_corto, 
  #                 y = posicion_text,
  #                 hjust = posicion_text2,
  #                 color = variacion, label=paste(comma(incremento*100, accuracy = 0.1), "%", sep="")), 
  #             size=4, fontface="bold", angle=90,
  #             family="Montserrat")+
  #   geom_text(aes(x = nombre_corto, 
  #                 y = posicion_text + ppgg*0.12,
  #                 hjust = posicion_text2,
  #                 color = variacion,
  #                 label = paste0("(", comma(as.numeric(promedio_mes_ant), accuracy = 0.1), "-", 
  #                                comma(as.numeric(promedio_mes), accuracy = 0.1), "%)")), 
  #             size = 3, angle = 90,
  #             family = "Montserrat") +
  #   scale_fill_manual(values = c("Aumenta mucho" = "#FF0000", "Aumenta" = "#fcc5c0",  "Promedio"="#807dba",
  #                                "Se mantiene" = "#D9D9D9", "Disminuye" = "#a1d99b", "Disminuye mucho" = "#4EA72E")) +
  #   scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
  #                                 "Se mantiene" = "black", "Disminuye" = "black",
  #                                 "Disminuye mucho" = "black")) +
  #   scale_y_continuous(labels=scales::percent) +
  #   theme_minimal() +
  #   labs(x="", y="Variación porcentual")+
  #   theme(
  #     legend.title = element_blank(),
  #     legend.position = "none",
  #     legend.text = element_text(size = 14),
  #     axis.text = element_text(size=8, color="black"),
  #     axis.title = element_text(size=10),
  #     plot.caption = element_text(size=9),
  #     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #     text = element_text(family = "Montserrat"),
  #     strip.text.x = element_text(size=10, face = "bold"))+
  #   facet_grid(~ tipo, scales = "free_x", space = "free_x") +
  #   annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  # graf1
  # 
  # ggsave(plot = graf1, here("02 graficas", paste("barra ocupacion diff_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  # 
  
  #### Ocupación hospitalaria hora ####
  
  # fecha_inicio <- ymd_hms(paste0(fecha_inicio, " 00:00:01"))
  # fecha_fin <- ymd_hms(paste0(fecha_fin, " 23:59:59"))
  # fecha_inicio_ant <- ymd_hms(paste0(fecha_inicio_ant, " 00:00:01"))
  # fecha_fin_ant <- ymd_hms(paste0(fecha_fin_ant, " 23:59:59"))
  # 
  # df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
  #   select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
  #          quirofanos, tipo, tipo_ordenado, clasificacion, hospital_42)
  # 
  # df_ocupacion <- df_egresos %>% 
  #   rename(cve_pres=ing_cve_pre_uni_med) %>% 
  #   mutate(ingreso = ymd_hms(paste(ing_fec_ing_hos, ing_hora_ing_hos, sep=" ")),
  #          egreso = ymd_hms(paste(egre_fec_egr_hos, egre_hora_egr_hos, sep=" "))) %>% 
  #   filter(egre_cve_mot_egr!=8 | is.na(egre_cve_mot_egr)) %>% 
  #   filter((!(egreso < fecha_inicio) | is.na(egreso)) &
  #            (!(ingreso-days(1) >= fecha_fin))) %>% 
  #   mutate(grupo = case_when(ingreso >= fecha_inicio &
  #                              is.na(egreso) ~ "G1",
  #                            ingreso >= fecha_inicio &
  #                              egreso > fecha_fin ~ "G2",
  #                            ingreso >= fecha_inicio &
  #                              egreso <= fecha_fin  ~ "G3",
  #                            ingreso < fecha_inicio &
  #                              is.na(egreso) ~ "G4",
  #                            ingreso < fecha_inicio &
  #                              egreso > fecha_fin ~ "G5",
  #                            ingreso < fecha_inicio &
  #                              egreso <= fecha_fin ~ "G6",
  #   )) %>% 
  #   mutate(aporte = case_when(grupo=="G1" ~ interval(fecha_fin, ingreso),
  #                             grupo=="G2" ~ interval(fecha_fin, ingreso),
  #                             grupo=="G3" ~ interval(egreso, ingreso),
  #                             grupo=="G4" ~ interval(fecha_fin, fecha_inicio),
  #                             grupo=="G5" ~ interval(fecha_fin, fecha_inicio),
  #                             grupo=="G6" ~ interval(egreso, fecha_inicio))) %>% 
  #   mutate(en_dias = aporte / ddays(1),
  #          en_horas = aporte / dhours(1),
  #          en_minutos = aporte /dminutes(1),
  #          aporte = abs(as.numeric(aporte / dhours(1))))
  # 
  # df_ocupacion_ant <- df_egresos %>% 
  #   rename(cve_pres=ing_cve_pre_uni_med) %>% 
  #   mutate(ingreso = ymd_hms(paste(ing_fec_ing_hos, ing_hora_ing_hos, sep=" ")),
  #          egreso = ymd_hms(paste(egre_fec_egr_hos, egre_hora_egr_hos, sep=" "))) %>% 
  #   filter(egre_cve_mot_egr!=8 | is.na(egre_cve_mot_egr)) %>% 
  #   filter((!(egreso < fecha_inicio_ant) | is.na(egreso)) &
  #            (!(ingreso-days(1) >= fecha_fin_ant))) %>% 
  #   mutate(grupo = case_when(ingreso >= fecha_inicio &
  #                              is.na(egreso) ~ "G1",
  #                            ingreso >= fecha_inicio &
  #                              egreso > fecha_fin ~ "G2",
  #                            ingreso >= fecha_inicio &
  #                              egreso <= fecha_fin  ~ "G3",
  #                            ingreso < fecha_inicio &
  #                              is.na(egreso) ~ "G4",
  #                            ingreso < fecha_inicio &
  #                              egreso > fecha_fin ~ "G5",
  #                            ingreso < fecha_inicio &
  #                              egreso <= fecha_fin ~ "G6",
  #   )) %>% 
  #   mutate(aporte = case_when(grupo=="G1" ~ interval(fecha_fin, ingreso),
  #                             grupo=="G2" ~ interval(fecha_fin, ingreso),
  #                             grupo=="G3" ~ interval(egreso, ingreso),
  #                             grupo=="G4" ~ interval(fecha_fin, fecha_inicio),
  #                             grupo=="G5" ~ interval(fecha_fin, fecha_inicio),
  #                             grupo=="G6" ~ interval(egreso, fecha_inicio))) %>% 
  #   mutate(en_dias = aporte / ddays(1),
  #          en_horas = aporte / dhours(1),
  #          en_minutos = aporte /dminutes(1),
  #          aporte = abs(as.numeric(aporte / dhours(1))))
  # 
  # prueba <- filter(df_ocupacion, cve_pres == "008-204-00" | cve_pres=="001-204-00")
  # ggplot(prueba, aes(x=abs(en_horas)))+
  #   geom_histogram() +
  #   facet_grid(~cve_pres)
  # 
  # df_ocupacion <- group_by(df_ocupacion, cve_pres) %>% 
  #   summarise(dias_tot = sum(aporte, na.rm=T))
  # 
  # df_ocupacion_ant <- group_by(df_ocupacion_ant, cve_pres) %>% 
  #   summarise(dias_tot_ant = sum(aporte, na.rm=T))
  # 
  # df_camas <- read_excel(here("01 datos", "Camas Censables abr-may 2024.xlsx")) %>% 
  #   # filter(dia >= fecha_inicio & dia <= fecha_fin) %>% 
  #   rename(cve_pres=cve_pre_uni_med, troncal = "servicio troncal") %>% 
  #   group_by(cve_pres, troncal) %>% 
  #   summarise(camas_tot = mean(num_camas, na.rm=T)) %>% 
  #   ungroup() %>% 
  #   group_by(cve_pres) %>% 
  #   summarise(camas_tot = sum(camas_tot, na.rm=T)) %>% 
  #   mutate(camas_tot = camas_tot * abs(interval(fecha_fin, fecha_inicio) / dhours(1)),
  #          camas_tot = round(camas_tot))
  # 
  # df_ocupacion <- left_join(df_ocupacion, df_camas) %>% 
  #   mutate(promedio_mes=as.numeric(dias_tot)/as.numeric(camas_tot)) %>% 
  #   left_join(df_cum) 
  # 
  # df_ocupacion <- left_join(df_ocupacion, df_ocupacion_ant) %>% 
  #   mutate(promedio_mes_ant=as.numeric(dias_tot_ant)/as.numeric(camas_tot)) 
  # 
  # df_ocupacion <- df_ocupacion %>% 
  #   filter(hospital_42=="SI") %>% 
  #   arrange(promedio_mes)
  # 
  # prom_ent <- mean(df_ocupacion$promedio_mes, na.rm=T)
  # sd_ent  <- sd(df_ocupacion$promedio_mes, na.rm=T)
  # 
  # order_nom_um <- df_ocupacion %>%
  #   pull(nombre_corto)
  # 
  # df_ocupacion$nombre_corto <- factor(df_ocupacion$nombre_corto, levels = unique(order_nom_um))
  # 
  # df_ocupacion$tipo <- factor(df_ocupacion$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
  #                                                           "CMFEQ", "CE", "CMF", "UMF", 
  #                                                           "CAF", "CMCT"))
  # 
  # df_ocupacion <- mutate(df_ocupacion, 
  #                        compara_prom=case_when(
  #                          str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
  #                          promedio_mes > 0.95 ~ "Mucho mayor",
  #                          promedio_mes > 0.9 ~ "Mayor",
  #                          promedio_mes < 0.8 ~ "Mucho menor",
  #                          promedio_mes < 0.85 ~ "Menor",
  #                          TRUE ~ "Igual"))
  # 
  # graf1 <- ggplot(df_ocupacion, aes(x = nombre_corto, 
  #                                   y = promedio_mes,
  #                                   fill = compara_prom)) +
  #   geom_bar(stat = "identity") +
  #   geom_text(aes(x = nombre_corto, 
  #                 y = promedio_mes + 0.001 ,
  #                 color = compara_prom, label= percent(as.numeric(promedio_mes), 0.1)), 
  #             size=4, fontface="bold", hjust = 0, angle = 90)+
  #   scale_y_continuous(labels=scales::percent)+
  #   scale_fill_manual(values = c("Mucho mayor" = "#FF0000", "Mayor" = "#fcc5c0", "Promedio"="#807dba",
  #                                "Igual" = "#D9D9D9", "Menor" = "#a1d99b", "Mucho menor" = "#4EA72E")) +
  #   scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
  #                                 "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
  #   #scale_y_continuous(limits = c(min(df_nosocom$promedio_mes) - 3, 100))+
  #   theme_minimal() +
  #   labs(x="", y="")+
  #   theme(
  #     legend.title = element_blank(),
  #     legend.position = "none",
  #     legend.text = element_text(size = 14),
  #     axis.text = element_text(size=8, color="black"),
  #     axis.title = element_text(size=10),
  #     plot.caption = element_text(size=9),
  #     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #     text = element_text(family = "Montserrat"),
  #     strip.text.x = element_text(size=10, face = "bold"))+
  #   facet_grid(~ tipo, scales = "free_x", space = "free_x") +
  #   annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  # graf1
  # 
  # ggsave(plot = graf1, here("02 graficas", paste("barra ocupa hosp_", rep_actual,".png", sep="")), 
  #        width = 8.9527559055, height = 5.8740157, dpi = 300)
  # 
  # 
  # df_ocupacion <- df_ocupacion %>% 
  #   mutate(incremento = (as.numeric(promedio_mes) - as.numeric(promedio_mes_ant)) / 
  #            as.numeric(promedio_mes_ant)) %>% 
  #   filter(hospital_42 == "SI")
  # 
  # prom_ent <- mean(df_ocupacion$incremento, na.rm=T)
  # sd_ent  <- sd(df_ocupacion$incremento, na.rm=T)
  # 
  # df_ocupacion <- mutate(df_ocupacion, 
  #                        variacion = case_when(
  #                          incremento>prom_ent + 0.5 * sd_ent~ "Aumenta",
  #                          incremento>=prom_ent + sd_ent ~ "Aumenta mucho",
  #                          incremento< prom_ent + 0.5 * sd_ent ~ "Disminuye",
  #                          incremento<= prom_ent + sd_ent ~ "Disminuye mucho",
  #                          TRUE ~ "Se mantiene"),
  #                        posicion_text = case_when(incremento>=0 ~ 0.01,
  #                                                  TRUE ~ -0.01),
  #                        posicion_text2 = case_when(incremento>=0 ~ 0,
  #                                                   TRUE ~ 1)) %>%
  #   arrange(incremento)
  # 
  # order_nom_um <- df_ocupacion %>%
  #   pull(nombre_corto)
  # 
  # df_ocupacion$tipo <- factor(df_ocupacion$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
  #                                                           "CMFEQ", "CE", "CMF", "UMF", 
  #                                                           "CAF", "CMCT"))
  # 
  # 
  # df_ocupacion$nombre_corto <- factor(df_ocupacion$nombre_corto, levels = unique(order_nom_um))
  # 
  # ppgg <- abs(max(df_ocupacion$incremento, na.rm=T) - min(df_ocupacion$incremento, na.rm=T))
  # 
  # graf1 <- ggplot(df_ocupacion, aes(x = nombre_corto, 
  #                                   y = incremento,
  #                                   fill = variacion)) +
  #   geom_bar(stat = "identity") +
  #   geom_text(aes(x = nombre_corto, 
  #                 y = posicion_text,
  #                 hjust = posicion_text2,
  #                 color = variacion, label=paste(comma(incremento*100, accuracy = 0.1), "%", sep="")), 
  #             size=4, fontface="bold", angle=90,
  #             family="Montserrat")+
  #   geom_text(aes(x = nombre_corto, 
  #                 y = posicion_text + ppgg*0.12,
  #                 hjust = posicion_text2,
  #                 color = variacion,
  #                 label = paste0("(", comma(as.numeric(promedio_mes_ant), accuracy = 0.1), "-", 
  #                                comma(as.numeric(promedio_mes), accuracy = 0.1), "%)")), 
  #             size = 3, angle = 90,
  #             family = "Montserrat") +
  #   scale_fill_manual(values = c("Aumenta mucho" = "#FF0000", "Aumenta" = "#fcc5c0",  "Promedio"="#807dba",
  #                                "Se mantiene" = "#D9D9D9", "Disminuye" = "#a1d99b", "Disminuye mucho" = "#4EA72E")) +
  #   scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
  #                                 "Se mantiene" = "black", "Disminuye" = "black",
  #                                 "Disminuye mucho" = "black")) +
  #   scale_y_continuous(labels=scales::percent) +
  #   theme_minimal() +
  #   labs(x="", y="Variación porcentual")+
  #   theme(
  #     legend.title = element_blank(),
  #     legend.position = "none",
  #     legend.text = element_text(size = 14),
  #     axis.text = element_text(size=8, color="black"),
  #     axis.title = element_text(size=10),
  #     plot.caption = element_text(size=9),
  #     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #     text = element_text(family = "Montserrat"),
  #     strip.text.x = element_text(size=10, face = "bold"))+
  #   facet_grid(~ tipo, scales = "free_x", space = "free_x") +
  #   annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  # graf1
  # 
  # ggsave(plot = graf1, here("02 graficas", paste("barra ocupacion diff_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  # 
  #### Quejas ####
  
  df_quejas <- read_excel(here("01 datos", "quejas_240524.xlsx"),
                          sheet=1)
  df_cum <- read_excel(here("01 datos", "CUMM 2023_limpio.xlsx")) %>% 
    select(cve_pres, nombre, nombre_corto, nombre_ent, ent_corta, consultorios, 
           quirofanos, tipo, tipo_ordenado, clasificacion, rep) #hospital_42
  
  df_quejas2 <- df_quejas %>% 
    mutate(periodo=case_when(fechahechos >= fecha_inicio & fechahechos <= fecha_fin ~ "Esta",
                             fechahechos >= fecha_inicio_ant & fechahechos <= fecha_fin_ant ~ "Anterior",
                             TRUE ~ "Otra")) %>% 
    filter(periodo != "Otra") %>% 
    group_by(cve_pres, periodo) %>%
    summarise(total = n()) %>% 
    mutate(total=if_else(is.na(total), 0, total)) %>% 
    filter(!is.na(cve_pres)) %>% 
    pivot_wider(names_from = periodo, values_from = total) %>% 
    rename("promedio_mes"=Esta, "promedio_mes_ant"=Anterior) %>% 
    mutate(promedio_mes = if_else(is.na(promedio_mes), 0 , promedio_mes),
           promedio_mes_ant = if_else(is.na(promedio_mes_ant), 0 , promedio_mes_ant))
  
  load(file=here("01 datos", "df_consultas.RData"))
  df_cons_abr24<- df_consultas %>% 
    filter(!tipologia %in% c("CMN", "HR")) #filter(hospital_42 == "SI")
  df_cons_abr24 <- df_cons_abr24 %>%
    rename("clave_pres" = clave,
           "consultas" = consultas_mes,
           "nombre_corto" = nombre_unidad,
           "tipo" = tipologia,
           "consultas_xconsult_abr24"= promedio_mes,
           "consultas_xconsult_23"= promedio23,
           "incremento"=cambio) 
  df_cons_abr24 <- df_cons_abr24 %>% 
    mutate(nombre_corto = paste(consultorios, " C. | ", nombre_corto, sep=""))
  
  df_quejas2 <- left_join(df_quejas2, df_cons_abr24 %>% ungroup() %>% 
                            select(clave_pres, consultas),
                          by=c("cve_pres"="clave_pres"))
  
  df_cum <- df_cum %>%  
    filter(!tipo %in% c("CMN", "HR"))
  #filter(hospital_42 == "SI")
  
  df_quejas2 <- left_join(df_cum, df_quejas2) %>% 
    mutate(promedio_mes = promedio_mes * 10000 / consultas,
           promedio_mes_ant = promedio_mes_ant * 10000 / consultas,
           incremento = (as.numeric(promedio_mes) - as.numeric(promedio_mes_ant)) / 
             as.numeric(promedio_mes_ant))
  
  prom_ent <- mean(df_quejas2$promedio_mes, na.rm=T)
  sd_ent  <- sd(df_quejas2$promedio_mes, na.rm=T)
  
  df_res <- group_by(df_quejas2, tipo) %>% 
    summarise(promedio_mes = mean(promedio_mes, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo, sep=""))
  
  df_res3<-df_quejas2 %>% 
    mutate(promedio_mes=as.numeric(promedio_mes),
           promedio_mes_ant=as.numeric(promedio_mes_ant),
           incremento=(promedio_mes/promedio_mes_ant)-1,
           incremento=ifelse(is.infinite(incremento), NA, incremento)) %>% 
    group_by(tipo) %>% 
    summarise(incremento = mean(incremento, na.rm = T)) %>%
    mutate(nombre_corto=paste("Prom. Nacional ", tipo %>% 
                                str_remove_all("^\\d*\\.\\s"), sep=""))
  
  df_quejas2 <- df_quejas2 %>% 
    #filter(hospital_42=="SI") %>% 
    filter(!tipo %in% c("CMN", "HR")) %>% 
    arrange(promedio_mes)
  
  df_quejas2<-df_quejas2 %>% 
    filter(rep==rep_actual) 
  
  id_tipo<-unique(df_quejas2$tipo)
  
  df_quejas2<-df_quejas2 %>% 
    bind_rows(df_res %>% filter(tipo %in% id_tipo))
  
  order_nom_um <- df_quejas2 %>%
    pull(nombre_corto)
  
  df_quejas2$nombre_corto <- factor(df_quejas2$nombre_corto, levels = unique(order_nom_um))
  
  df_quejas2$tipo <- factor(df_quejas2$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                        "CMFEQ", "CE", "CMF", "UMF", 
                                                        "CAF", "CMCT"))
  
  df_quejas2 <- mutate(df_quejas2, 
                       compara_prom=case_when(
                         str_detect(nombre_corto, "Prom. Nacional") ~ "Promedio",
                         promedio_mes > prom_ent + sd_ent ~ "Mucho mayor",
                         promedio_mes > prom_ent + 0.5 * sd_ent & promedio_mes <= prom_ent + sd_ent ~ "Mayor",
                         promedio_mes < prom_ent - sd_ent ~ "Mucho menor",
                         promedio_mes < prom_ent - 0.5 * sd_ent & promedio_mes >= prom_ent - sd_ent ~ "Menor",
                         TRUE ~ "Igual"))
  
  ppgg <- abs(max(df_quejas2$promedio_mes, na.rm=T) - min(df_quejas2$promedio_mes, na.rm=T))
  
  graf1 <- ggplot(df_quejas2, aes(x = nombre_corto, 
                                  y = promedio_mes,
                                  fill = compara_prom)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = promedio_mes + 0.01 * ppgg,
                  color = compara_prom, label=round(promedio_mes,1)), 
              size=4, fontface="bold", hjust = 0, angle = 90)+
    scale_fill_manual(values = c("Mucho mayor" = "#FF0000", "Mayor" = "#fcc5c0", "Promedio"="#807dba",
                                 "Igual" = "#D9D9D9", "Menor" = "#a1d99b", "Mucho menor" = "#4EA72E")) +
    scale_color_manual(values = c("Mucho mayor" = "black", "Mayor" = "black", "Promedio"="black",
                                  "Igual" = "black", "Menor" = "black", "Mucho menor" = "black")) +
    scale_y_continuous(name = "Incidencias por cada 10 mil consultas")+
    theme_minimal() +
    labs(x="", y="")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    coord_cartesian(ylim=c(min(df_quejas2$promedio_mes, na.rm=T), max(df_quejas2$promedio_mes, na.rm=T)+ppgg*0.08))+
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra quejas_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  df_quejas2 <- df_quejas %>% 
    mutate(periodo=case_when(fechahechos >= fecha_inicio & fechahechos <= fecha_fin ~ "Esta",
                             fechahechos >= fecha_inicio_ant & fechahechos <= fecha_fin_ant ~ "Anterior",
                             TRUE ~ "Otra")) %>% 
    filter(periodo != "Otra") %>% 
    group_by(cve_pres, periodo) %>%
    summarise(total = n()) %>% 
    mutate(total=if_else(is.na(total), 0, total)) %>% 
    filter(!is.na(cve_pres)) %>% 
    pivot_wider(names_from = periodo, values_from = total) %>% 
    rename("promedio_mes"=Esta, "promedio_mes_ant"=Anterior) %>% 
    mutate(promedio_mes = if_else(is.na(promedio_mes), 0 , promedio_mes),
           promedio_mes_ant = if_else(is.na(promedio_mes_ant), 0 , promedio_mes_ant))
  
  df_quejas2 <- left_join(df_quejas2, df_cons_abr24 %>% ungroup() %>% 
                            select(clave_pres, consultas),
                          by=c("cve_pres"="clave_pres"))
  
  df_cum <- df_cum %>%  
    filter(!tipo %in% c("CMN", "HR"))
  #filter(hospital_42 == "SI")
  
  df_quejas2 <- left_join(df_cum, df_quejas2) %>% 
    mutate(promedio_mes = promedio_mes * 10000 / consultas,
           promedio_mes_ant = promedio_mes_ant * 10000 / consultas,
           incremento = (as.numeric(promedio_mes) - as.numeric(promedio_mes_ant)) / 
             as.numeric(promedio_mes_ant))

  #filter(hospital_42 == "SI")
  
  prom_ent <- mean(df_quejas2$incremento, na.rm=T)
  sd_ent  <- sd(df_quejas2$incremento, na.rm=T)
  
  df_quejas2 <- filter(df_quejas2, rep == rep_actual) %>% 
    bind_rows(df_res3 %>% filter(tipo %in% id_tipo))
  
  df_quejas2 <- mutate(df_quejas2, 
                       variacion = case_when(
                         grepl("Prom. N", nombre_corto) ~"Promedio",
                         incremento > prom_ent + sd_ent ~ "Aumenta mucho",
                         incremento > prom_ent + 0.5 * sd_ent & incremento <= prom_ent + sd_ent ~ "Aumenta",
                         incremento < prom_ent - sd_ent ~ "Disminuye mucho",
                         incremento < prom_ent - 0.5 * sd_ent & incremento >= prom_ent - sd_ent ~ "Disminuye",
                         TRUE ~ "Se mantiene"),
                       posicion_text = case_when(incremento>=0 ~ 0.01,
                                                 TRUE ~ -0.01),
                       posicion_text2 = case_when(incremento>=0 ~ 0,
                                                  TRUE ~ 1)) %>%
    arrange(incremento) 
  

  
  order_nom_um <- df_quejas2 %>%
    pull(nombre_corto)
  
  df_quejas2$tipo <- factor(df_quejas2$tipo, levels = c("CMN", "HR", "HG", "CH", "CEQ",
                                                        "CMFEQ", "CE", "CMF", "UMF", 
                                                        "CAF", "CMCT"))
  
  
  df_quejas2$nombre_corto <- factor(df_quejas2$nombre_corto, levels = unique(order_nom_um))
  
  ppgg <- abs(max(df_quejas2$incremento, na.rm=T) - min(df_quejas2$incremento, na.rm=T))
  
  graf1 <- ggplot(df_quejas2, aes(x = nombre_corto, 
                                  y = incremento,
                                  fill = variacion)) +
    geom_bar(stat = "identity") +
    geom_text(aes(x = nombre_corto, 
                  y = posicion_text,
                  hjust = posicion_text2,
                  color = variacion, label= paste(comma(incremento*100, accuracy = 0.1), "%", sep="")), 
              size=4, fontface="bold", angle=90,
              family="Montserrat")+
    geom_text(data = df_quejas2, 
              aes(x = nombre_corto, 
                  hjust = posicion_text2,
                  y = posicion_text + ppgg*0.16,
                  color = variacion,
                  label = ifelse(grepl("Nacional", nombre_corto), "", paste0("(", comma(promedio_mes_ant, accuracy = 0.1), "-", 
                                 comma(promedio_mes, accuracy = 0.1), ")"))), 
              size = 3, angle = 90,
              family = "Montserrat") +
    scale_fill_manual(values = c("Aumenta mucho" = "#FF0000", "Aumenta" = "#fcc5c0",  "Promedio"="#807dba",
                                 "Se mantiene" = "#D9D9D9", "Disminuye" = "#a1d99b", "Disminuye mucho" = "#4EA72E")) +
    scale_color_manual(values = c("Aumenta mucho" = "black", "Aumenta" = "black",  "Promedio"="black",
                                  "Se mantiene" = "black", "Disminuye" = "black",
                                  "Disminuye mucho" = "black")) +
    scale_y_continuous(labels=scales::percent) +
    theme_minimal() +
    labs(x="", y="Variación porcentual")+
    theme(
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 14),
      axis.text = element_text(size=8, color="black"),
      axis.title = element_text(size=10),
      plot.caption = element_text(size=9),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      text = element_text(family = "Montserrat"),
      strip.text.x = element_text(size=10, face = "bold"))+
    facet_grid(~ tipo, scales = "free_x", space = "free_x") +
    annotation_custom(grid::linesGrob(x = c(0, 0), gp = grid::gpar(lwd = 4, col="#9F2241"))) 
  graf1
  
  ggsave(plot = graf1, here("02 graficas", paste("barra quejas diff_", rep_actual,".png", sep="")), width = 8.9527559055, height = 5.8740157, dpi = 300)
  
  print(rep_actual)
}
