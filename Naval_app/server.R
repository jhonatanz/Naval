# Aplicacion Demo

# Carga de modelos previamente entrenados

library(splines)
library(dplyr)
library(purrr)
mdl_df1 <- readRDS("mdl_df1.RDS")
mdl_df2 <- readRDS("mdl_df2.RDS")
mdl_df3 <- readRDS("mdl_df3.RDS")
mdl_df4 <- readRDS("mdl_df4.RDS")
mdl_df5 <- readRDS("mdl_df5.RDS")
mdl_df6 <- readRDS("mdl_df6.RDS")
mdl_df7 <- readRDS("mdl_df7.RDS")
mdl_df8 <- readRDS("mdl_df8.RDS")
mdl_df9 <- readRDS("mdl_df9.RDS")

# se combinan todos los modelos en una sola lista

modelos<-list(mdl_df1, mdl_df2, mdl_df3, mdl_df4, mdl_df5, mdl_df6, mdl_df7,
              mdl_df8, mdl_df9)

# Carga de los datos de prueba
NPM_te<-read.csv("test.csv")

comb1 <- function(v, t, mini, maxi){
  # Función para la construcción de un data frame con muestras de NPM_te
  # a partir de dos vectores que contienen las velocidades y el tiempo de
  # operación a dicha velocidad que forman una estrategia operativa
  temp1<-filter(NPM_te, Ship_spd == v & Turb_decay > mini & Turb_decay <= maxi)
  temp2<-temp1[sample(nrow(temp1), size = 60*t, replace = T),]
  pred<-predict(modelos[[v/3]], newdata = temp2)
  out<-mutate(temp2, pred = pred)
  return(out)
}

server <- function(input, output) {
  # inicialización y definición de datos
  datos <- reactiveValues(op = tibble(v=0, t=0), ent = tibble(v=0, t=0), 
                          sal = tibble(acum = 0, t1 = 0, v= 0))
  datos1 <- reactiveValues(sal = tibble(acum = 0, t1 = 0, v= 0))
  datos2 <- reactiveValues(sal = tibble(acum = 0, t1 = 0, v= 0))
  datos3 <- reactiveValues(sal = tibble(acum = 0, t1 = 0, v= 0))
  tabla <- reactiveVal(value = 0)
  
  # Funcionamiento del boton de reset
  observeEvent(input$reset, {
    
    datos$op = tibble(v=0, t=0); datos$ent = tibble(v=0, t=0)
    
    datos$sal = tibble(acum = 0, t1 = 0, v= 0)
    datos1$sal = tibble(acum = 0, t1 = 0, v= 0)
    datos2$sal = tibble(acum = 0, t1 = 0, v= 0)
    datos3$sal = tibble(acum = 0, t1 = 0, v= 0)
    
    tabla(0)
  })
  
  # codigo para guardar y graficar segmentos de operacion.
  observeEvent(input$save, {
    d_in <- tibble(v=input$vel, t=input$t+datos$op$t[length(datos$op$t)])
    d_m <- tibble(v=input$vel, t=datos$op$t[length(datos$op$t)])
    datos$op <- rbind(datos$op, d_m, d_in)
    datos$ent <- rbind(datos$ent, tibble(v=input$vel, t=input$t))
    print(length((datos$ent$v)))
  })
  
  # calculos de consumo de combustible
  observeEvent(input$load, {
    if(length(datos$ent$v)<2){
      showNotification("DEBE INTRODUCIR UN SEGMENTO OPERATIVO", type = "error", duration = 10)
    } else {
      x <- datos$ent[-1,]
      datos$sal<-map2(x$v, x$t, comb1, 0.975, 1)%>%
        bind_rows()%>% #convierte la lista de df a un solo df unido
        mutate(acum = cumsum(60*pred), t1=(1:length(pred))/60)
    }
    
  })
  
  # calculos para comparar desempeño versus decaimiento
  observeEvent(input$load2, {
    if(length(datos$ent$v)<2){
      showNotification("DEBE INTRODUCIR UN SEGMENTO OPERATIVO", type = "error", duration = 10)
    } else {
      x <- datos$ent[-1,]
      datos1$sal<-map2(x$v, x$t, comb1, 0.975, 0.981)%>%
        bind_rows()%>% 
        mutate(acum = cumsum(60*pred), t1=(1:length(pred))/60)
      
      x <- datos$ent[-1,]
      datos2$sal<-map2(x$v, x$t, comb1, 0.981, 0.994)%>%
        bind_rows()%>%
        mutate(acum = cumsum(60*pred), t1=(1:length(pred))/60)
      
      x <- datos$ent[-1,]
      datos3$sal<-map2(x$v, x$t, comb1, 0.994, 1)%>%
        bind_rows()%>%
        mutate(acum = cumsum(60*pred), t1=(1:length(pred))/60)
      
      tabla(rbind(summarize(datos1$sal, consumo = round(max(acum)), 
                            min_decay = min(Turb_decay), 
                            max_decay = max(Turb_decay)),
                  summarize(datos2$sal, consumo = round(max(acum)), 
                            min_decay = min(Turb_decay), 
                            max_decay = max(Turb_decay)),
                  summarize(datos3$sal, consumo = round(max(acum)), 
                            min_decay = min(Turb_decay), 
                            max_decay = max(Turb_decay))
      )
      )
    }
    
  })
  
  #ploteo de graficas
  
  # tab 1
  output$plot_spd <- renderPlot({
    plot(datos$op$t, datos$op$v, type = 'l', main = "Gráfica de operación", 
         xlab = "Tiempo [horas]", ylab = "Velocidad [nudos]")
  })
  output$plot_acum <- renderPlot({
    plot(datos$sal$t1, datos$sal$acum, type = "l", main = "Consumo Acumulado", 
         xlab = "Tiempo [horas]", ylab = "Combustible [Kg]")
  })
  # tab 2
  output$plot_spd_2 <- renderPlot({
    plot(datos$op$t, datos$op$v, type = 'l', main = "Gráfica de operación", 
         xlab = "Tiempo [horas]", ylab = "Velocidad [nudos]")
  })
  output$plot_fuel <- renderPlot({
    plot(datos1$sal$t1, datos1$sal$pred, type= "l", col = "red", main = "Grafica de Flujo de Combustible", 
         xlab = "Tiempo [horas]", ylab = "Flujo [Kg/s]")
    lines(datos2$sal$t1, datos2$sal$pred, col = "blue")
    lines(datos3$sal$t1, datos3$sal$pred, col = "green")
    legend("topleft", legend = c("mala", "media", "buena"), 
           col = c("red", "blue", "green"), pch = 1, title = "Condición")
  })
  output$plot_int <- renderPlot({
    plot(datos1$sal$t1, datos1$sal$acum, type = "l", col= "red",
         main = "Consumo Acumulado por coeficiente de decaimiento", 
         xlab = "Tiempo [horas]", ylab = "Combustible [Kg]")
    lines(datos2$sal$t1, datos2$sal$acum, col = "blue")
    lines(datos3$sal$t1, datos3$sal$acum, col = "green")
    legend("bottomright", legend = c("mala", "media", "buena"), 
           col = c("red", "blue", "green"), lty = 1, lwd = 1, 
           title = "Condición")
  })
  # impresion de valores
  output$f_acum <- renderText({
    print(c(round(datos$sal$acum[length(datos$sal$acum)]), "Kg"))
    })
  output$f_acum_2 <- renderTable(tabla(), digits = 3)
}