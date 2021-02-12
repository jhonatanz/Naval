library(shiny)

ui <- navbarPage("Consumo Combustible",
    tabPanel("Consumo",
             sidebarLayout(
                 sidebarPanel(
                   img(src = "logo_horizontal_azul.jpg", alt = "logo", width = "200", height ="80", style = "float:top"),
                   h3("Entradas"),
                   sliderInput(inputId = "vel", 
                               label = "Selector de Velocidad", 
                               value = 3, min = 3, max = 27, step = 3),
                   sliderInput(inputId = "t", 
                               label = "Tiempo en horas", 
                               value = 1, min = 1, max = 24, step = 1),
                   actionButton(inputId = "save", 
                                label = "Escribir Segmento"),
                   actionButton(inputId = "load", 
                                label = "Cargar operación"),
                   actionButton(inputId = "reset", 
                                label = "Reset"),
                   h3("Calculos"),
                   p("Consumo de combustible:"),
                   textOutput(outputId = "f_acum", container = h2),
                   h3("Instrucciones"),
                   p("Inicie introduciendo segmentos operativos, por ejemplo, 
                     si el barco deberá mantener una velocidad de 6 nudos durante 
                     2 horas, use las barras para indicar lo anterior y oprima 
                     el boton de 'Escribir segmento', tras lo cual el segmento 
                     introducido se mostrara en la gráfica. Continue hasta 
                     completar la grafica que representa la operación."),
                   p("Despues de completar la grafica de operación, oprima el 
                     boton 'cargar operacion' para iniciar el calculo de consumo
                     de combustible, espere hasta que haya sido efectuado."),
                   p("Al finalizar el calculo, la aplicacion mostrará la grafica 
                     de consumo acumulado y el calculo del consumo de combustible 
                     total para la operacion en la seccion de calculo en este 
                     panel lateral."),
                   p("Navegue en las pestañas para ver mas información disponible 
                     respecto de la operacion simulada.")
                   ),
                 mainPanel(
                   h1("Graficas"),
                   plotOutput(outputId = "plot_spd"),
                   plotOutput(outputId = "plot_acum")
                   )
                 )
             ),
    tabPanel("Desempeño",
             sidebarLayout(
               sidebarPanel(
                 img(src = "logo_horizontal_azul.jpg", alt = "logo", width = "200", height ="80", style = "float:top"),
                 h3("Operacion"),
                 plotOutput(outputId = "plot_spd_2"),
                 p(),
                 actionButton(inputId = "load2", 
                              label = "comparar consumos"),
                 h3("Calculos"),
                 p("Comparación de consumo de combustible según el coeficiente de decaimiento de la turbina:"),
                 tableOutput(outputId = "f_acum_2"),
               ),
               mainPanel(
                 h1("Graficas"),
                 plotOutput(outputId = "plot_fuel"),
                 plotOutput(outputId = "plot_int")
               )
             )
             )
)