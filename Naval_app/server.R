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
NPM_te <- NPM_te %>%
  mutate(NPM_te, cond = as.factor(if_else(Turb_decay < 0.981, "mala", 
                                          if_else(Turb_decay >= 0.981 & Turb_decay <0.994, 
                                                  "normal", "optima"))))
NPM_te$Ship_spd <- as.factor(NPM_te$Ship_spd)

comb1 <- function(v, t, condicion){
  # Función para la construcción de un data frame con muestras de NPM_te
  # a partir de dos vectores que contienen las velocidades y el tiempo de
  # operación a dicha velocidad que forman una estrategia operativa
  temp1<-filter(NPM_te, Ship_spd == v & cond == condicion)
  temp2<-temp1[sample(nrow(temp1), size = 60*t, replace = T),]
  pred<-predict(modelos[[v/3]], newdata = temp2)
  temp3<-mutate(temp2, pred = pred)
  return(temp3)
}


server <- function(input, output) {
  # inicialización y definición de datos
  datos <- reactiveValues(op = tibble(v=0, t=0), ent = tibble(v=0, t=0), 
                          sal = tibble(acum = 0, t = 0, v= 0, cond = 0, 
                                       media = 0, pred = 0, Turbine_temp = 0,
                                       Ship_spd = 0)
                          )

  tabla <- reactiveVal(value = 0)
  
  # Funcionamiento del boton de reset
  observeEvent(input$reset, {
    
    datos$op = tibble(v=0, t=0); 
    datos$ent = tibble(v=0, t=0)
    datos$sal = tibble(acum = 0, t = 0, v= 0, cond = 0, media = 0, pred = 0)
    tabla(0)
    
  })
  
  # codigo para guardar y graficar segmentos de operacion.
  observeEvent(input$save, {
    d_in <- tibble(v=input$vel, t=input$t+datos$op$t[length(datos$op$t)])
    d_m <- tibble(v=input$vel, t=datos$op$t[length(datos$op$t)])
    datos$op <- rbind(datos$op, d_m, d_in)
    datos$ent <- rbind(datos$ent, tibble(v=input$vel, t=input$t))
  })
  
  # calculos de consumo de combustible
  observeEvent(input$load, {
    if(length(datos$ent$v)<2){
      showNotification("DEBE INTRODUCIR UN SEGMENTO OPERATIVO", type = "error", duration = 10)
    } else {
      
      # Construccion de data frame para las diferentes condiciones posibles (mala, normal y optima en la variable "cond")
      ent <- datos$ent[-1,]
      x <- list()
      for(i in levels(NPM_te$cond)){
        x[[i]]<-map2(ent$v, ent$t, comb1, i)%>%
          bind_rows()%>%
          mutate(acum = cumsum(60*pred), t=(1:length(pred))/60)%>%
          group_by(Ship_spd)%>%
          mutate(media = mean(pred))
      }
      df<-bind_rows(x)
      
      df1<-df%>%
        group_by("Condición" = cond)%>%
        summarize("Consumo" = round(max(acum)))%>%
        mutate("Costo COP" = round(Consumo*2191/0.850))
      
      datos$sal <- df
      tabla(df1)
    }
  })

  #ploteo de graficas
  
  # tab 1
  output$plot_spd <- renderPlot({
    ggplot(data = datos$op)+
      geom_line(mapping = aes(x=t, y=v))+
      labs(x = "Tiempo [horas]", y = "Velocidad [nudos]",
           title ="OPERACIÓN"
           #subtitle = "Add a subtitle below title",
           #caption = "Add a caption below plot"
      )
    #plot(datos$op$t, datos$op$v, type = 'l', main = "Gráfica de operación", 
         #xlab = "Tiempo [horas]", ylab = "Velocidad [nudos]")
  })
  output$plot_acum <- renderPlot({
    ggplot(data = datos$sal)+
      geom_line(mapping = aes(x=t, y=acum, color = cond))+
      labs(x = "Tiempo [horas]", y = "Combustible [Kg]",
           title ="CONSUMO ACUMULADO"
           #subtitle = "Add a subtitle below title",
           #caption = "Add a caption below plot"
      )
    #plot(datos$sal$t1, datos$sal$acum, type = "l", main = "Consumo Acumulado", 
    #     xlab = "Tiempo [horas]", ylab = "Combustible [Kg]")
  })
  
  # tab 2
  output$plot_spd_2 <- renderPlot({
    ggplot(data = datos$op)+
      geom_line(mapping = aes(x=t, y=v))+
      labs(x = "Tiempo [horas]", y = "Velocidad [nudos]",
           title ="OPERACIÓN")
  })
  output$plot_fuel <- renderPlot({
    ggplot(data = datos$sal)+
      geom_line(mapping = aes(x=t, y=pred, color = cond))+
      labs(x = "Tiempo [horas]", y = "Flujo de Combustible [Kg/s]",
           title ="FLUJO INSTANTANEO DE COMBUSTIBLE")
  })
  output$plot_media <- renderPlot({
    ggplot(data = datos$sal)+
      geom_line(mapping = aes(x=t, y=media, color = cond))+
      labs(x = "Tiempo [horas]", y = "Flujo de Combustible [Kg/s]",
           title ="MEDIA DE CONSUMO EN CADA VELOCIDAD")
  })
  
  #tab 3
  output$plot_spd_3 <- renderPlot({
    ggplot(data = datos$op)+
      geom_line(mapping = aes(x=t, y=v))+
      labs(x = "Tiempo [horas]", y = "Velocidad [nudos]",
           title ="OPERACIÓN")
  })
  
  output$plot_temp1 <- renderPlot({
    ggplot(data = datos$sal)+
      geom_point(mapping = aes(x=t, y=pred, color = Turbine_temp))+
      labs(x = "Tiempo [horas]", y = "Flujo de Combustible [Kg/s]",
           title ="FLUJO INSTANTANEO DE COMBUSTIBLE SEGÚN LA TEMPERATURA")
  })
  
  output$plot_temp2 <- renderPlot({
    ggplot(data = datos$sal)+
      geom_point(mapping = aes(x=Turbine_temp, y=pred))+
      facet_wrap(~Ship_spd, nrow = 2, scales = "free")+
      labs(x = "Temperatura de la turbina [F]", y = "Flujo de Combustible [Kg/s]",
           title ="COMPORTAMIENTO DEL FLUJO DE COMBUSTIBLE CON LA TEMPERATURA SEGÚN LA VELOCIDAD")
  })
  
  # impresion de valores
  
  output$f_acum <- renderText({
    print(c(round(datos$sal$acum[length(datos$sal$acum)]), "Kg"))
    })
  output$f_acum_2 <- renderTable(tabla(), digits = 0)
}