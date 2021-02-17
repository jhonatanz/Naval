library(shiny)

ui <- navbarPage("Consumo Combustible", 
                 tabPanel("Consumo General",
                          sidebarLayout(
                            sidebarPanel(
                              img(src = "logo_horizontal_azul.jpg", alt = "logo", 
                                  width = "200", height ="80", style = "float:top"),
                              h3("Entradas"),
                              sliderInput(inputId = "vel", 
                                          label = "Selector de Velocidad", 
                                          value = 3, min = 3, max = 27, step = 3),
                              sliderInput(inputId = "t", 
                                          label = "Tiempo en horas", 
                                          value = 1, min = 1, max = 10, step = 1),
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
                              si el barco debe mantener una velocidad de 6 nudos durante 
                              2 horas, use las barras para indicar lo anterior y oprima 
                              el botón de 'Escribir segmento', tras lo cual el segmento 
                              introducido se mostrara en la gráfica. Continué hasta 
                                completar la grafica que representa la operación."),
                              p("Después de completar la grafica de operación, oprima el 
                              botón 'cargar operación' para iniciar el calculo de consumo
                              de combustible, espere hasta que haya sido efectuado."),
                              p("Al finalizar el calculo, la aplicación mostrará la grafica 
                              de consumo acumulado y el calculo del consumo de combustible 
                              total para la operación en la sección de calculo en este 
                              panel lateral."),
                              p("Navegue en las pestañas para ver mas información disponible 
                                respecto de la operación simulada.")
                            ),
                            mainPanel(
                              h1("Gráficas"),
                              plotOutput(outputId = "plot_spd"),
                              plotOutput(outputId = "plot_acum")
                            )
                          )
                 ),
                 tabPanel("Consumo Vs Condición",
                          sidebarLayout(
                            sidebarPanel(
                              img(src = "logo_horizontal_azul.jpg", alt = "logo", width = "200", height ="80", style = "float:top"),
                              h3("Operación"),
                              plotOutput(outputId = "plot_spd_2"),
                              p(),
                              #actionButton(inputId = "load2", 
                              #              label = "comparar consumos"),
                              h3("Cálculos"),
                              p("Comparación de consumo de combustible según el coeficiente de decaimiento de la turbina:"),
                              tableOutput(outputId = "f_acum_2")
                            ),
                            mainPanel(
                              h1("Gráficas"),
                              plotOutput(outputId = "plot_fuel"),
                              plotOutput(outputId = "plot_media")
                            )
                          )
                 ),
                 tabPanel("Consumo Vs Temperatura",
                          sidebarLayout(
                            sidebarPanel(
                              img(src = "logo_horizontal_azul.jpg", alt = "logo", width = "200", height ="80", style = "float:top"),
                              h3("Operación"),
                              plotOutput(outputId = "plot_spd_3"),
                              p(),
                              #actionButton(inputId = "load2", 
                              #              label = "comparar consumos"),
                              h3("Análisis"),
                              p("Comparación de consumo de combustible en función de la temperatura:"),
                              p("Se observa que en términos generales, el consumo de combustible
                              aumenta con el aumento de la temperatura, en la segunda grafica 
                              puede observarse este comportamiento, incluso disgregando para
                              cada velocidad operativa."),
                              p("Es de notar la gran variabilidad en la temperatura de operación 
                              para la velocidades mas bajas (3 y 6 nudos)."),
                              p("Como medio para economizar combustible se podría estudiar sistemas
                              para el control de temperatura de turbina de modo que se pueda 
                              operar en los niveles mas bajos posibles.")
                            ),
                            mainPanel(
                              h1("Gráficas"),
                              plotOutput(outputId = "plot_temp1"),
                              plotOutput(outputId = "plot_temp2")
                            )
                          )
                 )
)