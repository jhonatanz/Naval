# Aplicacion Demo

# Carga de modelos previamente entrenados

library(splines)
library(dplyr)
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

modelos<-list(
  mdl_df1,
  mdl_df2,
  mdl_df3,
  mdl_df4,
  mdl_df5,
  mdl_df6,
  mdl_df7,
  mdl_df8,
  mdl_df9
)

# Carga de los datos de prueba
NPM_te<-read.csv("test.csv")

# funcion de calculo de consumo de combustible
comb <- function (b, mini, maxi){
  y <- data.frame(matrix(ncol = ncol(NPM_te)+1, nrow = 0))
  names(y)<-c(names(NPM_te), "pred")
  acum <- 0
  for (i in 1:length(b)) {
    temp1<-filter(NPM_te, Ship_spd == b[i] & Turb_decay > mini & Turb_decay <= maxi)
    temp2<-temp1[sample(nrow(temp1), size = 1),]
    ind<-temp2$Ship_spd/3
    temp3<-predict(modelos[[ind]], newdata = temp2)
    acum <- acum + temp3*60
    temp4<-mutate(temp2, "pred" = temp3, "f_acum" = acum, "t" = i)
    y<-rbind(y, temp4)
  }
  return(y)
}

server <- function(input, output) {
  datos <- reactiveValues(v_vel = 3, t1 = 0, t2 =0, l_t = 0, f_f = 0, f_a = 0)
  datos1 <- reactiveValues(v_vel = 3, t1 = 0, t2 =0, l_t = 0, f_f = 0, f_a = 0)
  datos2 <- reactiveValues(v_vel = 3, t1 = 0, t2 =0, l_t = 0, f_f = 0, f_a = 0)
  datos3 <- reactiveValues(v_vel = 3, t1 = 0, t2 =0, l_t = 0, f_f = 0, f_a = 0)
  tabla <- reactiveVal(value = 0)
  observeEvent(input$reset, {
    datos$v_vel <- 3
    datos$t1 <- 0
    datos$t2 <- 0
    datos$l_t <- 0
    datos$f_f <- 0
    datos$f_a <- 0
  })
  # codigo para guardar segmentos de operacion
  observeEvent(input$save, {
    datos$v_vel <- c(datos$v_vel, rep_len(input$vel, 60*input$t))
    datos$t1 <- c(datos$t1, seq(from = datos$l_t + 1/60, to = datos$l_t + input$t, by = 1/60))
    datos$l_t<-datos$t1[length(datos$t1)]
  })
  
  # calculos de consumo de combustible
  observeEvent(input$load, {
    y <- comb(datos$v_vel, 0.975, 1)
    datos$f_f<-y$pred
    datos$f_a<-y$f_acum
    datos$t2<-datos$t1
  })
  observeEvent(input$load2, {
    a <- comb(datos$v_vel, 0.975, 0.981)
    datos1$f_f<-a$pred
    datos1$f_a<-a$f_acum
    datos1$t2<-a$t
    b <- comb(datos$v_vel, 0.981, 0.994)
    datos2$f_f<-b$pred
    datos2$f_a<-b$f_acum
    datos2$t2<-b$t
    c <- comb(datos$v_vel, 0.994, 1)
    datos3$f_f<-c$pred
    datos3$f_a<-c$f_acum
    datos3$t2<-c$t
    tabla(rbind(summarize(a, consumo = round(max(f_acum)), min_decay = min(Turb_decay), max_decay = max(Turb_decay)),
              summarize(b, consumo = round(max(f_acum)), min_decay = min(Turb_decay), max_decay = max(Turb_decay)),
              summarize(c, consumo = round(max(f_acum)), min_decay = min(Turb_decay), max_decay = max(Turb_decay))
              )
    )
  })
  #ploteo de graficas
  
  # tab 1
  output$plot_spd <- renderPlot({
    plot(datos$t1, datos$v_vel, type = 'l', main = "Gráfica de operación", xlab = "Tiempo [horas]", ylab = "Velocidad [nudos]")
  })
  output$plot_acum <- renderPlot({
    plot(datos$t2, datos$f_a, type = "l", main = "Consumo Acumulado", 
         xlab = "Tiempo [horas]", ylab = "Combustible [Kg]")
  })
  # tab 2
  output$plot_spd_2 <- renderPlot({
    plot(datos$t1, datos$v_vel, type = 'l', main = "Gráfica de operación", 
         xlab = "Tiempo [horas]", ylab = "Velocidad [nudos]")
  })
  output$plot_fuel <- renderPlot({
    plot(datos1$t2, datos1$f_f, col = "red", main = "Grafica de Flujo de Combustible", 
         xlab = "Tiempo [horas]", ylab = "Flujo [Kg/s]")
    points(datos2$t2, datos2$f_f, col = "blue")
    points(datos3$t2, datos3$f_f, col = "green")
  })
  output$plot_int <- renderPlot({
    plot(datos1$t2, datos1$f_a, type = "l", col= "red", main = "Consumo Acumulado por coeficiente de decaimiento", 
         xlab = "Tiempo [horas]", ylab = "Combustible [Kg]")
    lines(datos2$t2, datos2$f_a, col = "blue")
    lines(datos3$t2, datos3$f_a, col = "green")
  })
  # impresion de valores
  output$f_acum <- renderText({
    print(c(round(datos$f_a[length(datos$f_a)]), "Kg"))
    })
  output$f_acum_2 <- renderTable(tabla(), digits = 3)
}