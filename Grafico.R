
setwd(r"(C:\Users\gflor\Downloads)")
# Ingresamos los datos de los dos sitios 

sitio1 <- c(16,17,15,16,15,13,14,17,16,14)
sitio2 <- c(20,23,21,19,21,21,22,16,20,20)

mean_sitio1 <- mean(sitio1)
mean_sitio2 <- mean(sitio2)
sd_sitio1 = sd(sitio1)
sd_sitio2 = sd(sitio2)
# Pre analsis con shapiro para ver normalidad 

library(ggplot2)

df <- data.frame(sitio = c("Sitio 1", "Sitio 2"),
                 produccion_media = c(mean_sitio1, mean_sitio2),
                 Produccion_sd=c(sd_sitio1,  sd_sitio2))


# Pre analsis con shapiro para ver normalidad 
# Prueba de normalidad para sitio1
shapiro.test(sitio1)

# Prueba de normalidad para sitio2
shapiro.test(sitio2)

t.test(sitio1, sitio2)
# Prueba t de Student para datos con distribución normal y varianzas iguales
t.test(sitio1, sitio2, var.equal = TRUE)


library("ggsignif")  
Grafico= df %>% 
  ggplot(aes(sitio, produccion_media)) +
  geom_col(aes(fill = sitio, color = sitio), width = 0.5, show.legend = F,
           alpha=0.6) +
  geom_errorbar(aes(ymin = produccion_media - Produccion_sd,
                    ymax = produccion_media + Produccion_sd),
                color = "#22292F",
                width = .1)+
  geom_signif(comparisons = list(c("Sitio 1", "Sitio 2")),
              map_signif_level = TRUE,
              annotations = c("p= 0.00046"), size=0.1,
              textsize=2, family="serif")+
  scale_fill_manual(values = c("#6a994e", "#e85d04")) +
  scale_color_manual(values = c("#6a994e", "#e85d04")) +
  labs(x = "Sitio", y = "Medición de la variable (kg)",
       title = "",caption = "Pueba T Studens") +
  theme_bw()+
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0))+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5),
        axis.text.x  = element_text(face="bold", color="black", size=7,
                                    family="serif"),
        axis.text.y  = element_text(face="bold", color="black",
                                    family="serif",size=7),
        axis.title = element_text(face="bold", color="black",size = 9),
        plot.caption = element_text(size = 8, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = margin(t = 15)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    scale_x_discrete(limit = c("Sitio 1", "Sitio 2"),
                     labels = c("Grupo 1","Grupo 2"))

Grafico



datos <- data.frame(Sitio = c(rep("Sitio 1", 10), rep("Sitio 2", 10)), 
                    Produccion = c(sitio1, sitio2), 
                    Dif_Medias = c(rep(0, 10), rep(dif_medias, 10)))
library(ggpubr)
compare_means(produccion ~ sitio, data = datos)

Grafico_box= ggboxplot(datos, x = "sitio", y = "produccion",
               color = "sitio", palette = "jco",
               add = "jitter", width = 0.3)+
  scale_fill_manual(values = c("#6a994e", "#e85d04")) +
  scale_color_manual(values = c("#6a994e", "#e85d04")) +
  labs(x = "Sitio", y = "Medición de la variable (kg)",
       title = "",caption = "Pueba T Studens")+
  scale_y_continuous(limits = c(12, 24), expand = c(0, 0))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5),
        axis.text.x  = element_text(face="bold", color="black", size=7,
                                    family="serif"),
        axis.text.y  = element_text(face="bold", color="black",
                                    family="serif",size=7),
        axis.title = element_text(face="bold", color="black",size = 9),
        legend.position = "none",
        plot.caption = element_text(size = 8, 
                                    face = "italic",
                                    color = "#606F7B",
                                    margin = margin(t = 15)),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_x_discrete(limit = c("Sitio 1", "Sitio 2"),
                   labels = c("Grupo 1","Grupo 2"))+ 
  geom_signif(comparisons = list(c("Sitio 1", "Sitio 2")),
              map_signif_level = TRUE,
              annotations = c("p= 0.00046"), size=0.1,
              textsize=2, family="serif")


library(cowplot)
Expo = ggdraw() +
  coord_equal(xlim = c(0, 16), ylim = c(0, 10), expand = FALSE) +
  
  
  draw_plot(Grafico , width = 8, height = 10,x = 0, y = 0)+
  draw_plot(Grafico_box , width = 8, height = 10,x = 8, y = 0)+

  
  theme(panel.background = element_rect(fill = "white"))

Expo

ggsave(plot=Expo ,"Grafico.png",units = "cm",width = 16, #ancho
       height = 10, #alargo
       dpi=1200)






