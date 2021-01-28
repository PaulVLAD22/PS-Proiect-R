library(shiny)
#librarie pt js
library(shinyjs)

#Nr minim varfuri
nrMinVf=1
#Nr max Varfuri
nrMaxVf=10
# Lista de forme 
forma=list('Forma 1','Forma 2','Forma 3','Forma 4')

#numele noastre
numeElevi=list('Munteanu Vlad Paul','Alexandru Oana','Mihai Catalin','Alex')

#cod javascript 
jsCode <- '
//functia shinyjs care poate fi apelata din R
shinyjs.draw = function(params) {
  var defaultParams = {
    nrVfInput:5,
    razaInput:5,
    formaInput:"Forma 1"
  };
  
  params = shinyjs.getParams(params, defaultParams);
  
  document.getElementById("nrVfHistory").innerHTML+=params.nrVfInput+", ";
  document.getElementById("razaHistory").innerHTML+=params.razaInput+", ";
  document.getElementById("formaHistory").innerHTML+=params.formaInput+", ";
  noLoop();
  
}'


# aici e interfata
ui <- fluidPage(
    useShinyjs(),
    #aici apar functiile shinyjs din javascript(singurele functii care pot fi chemate din R)
    extendShinyjs(text = jsCode, functions = c("draw","setup")),
    headerPanel('Chaos Game'),
    h5(numeElevi),
    sidebarPanel(
        numericInput('nrVfInput', 'Numar Varfuri:', value=5,
                     min=nrMinVf,max=nrMaxVf),
        numericInput('razaInput', 'Raza Cercului:', value=5),
        selectInput('formaInput', 'Alege Forma:',
                    forma),
        # la apasarea butonului se executa js
        actionButton("button", "Go"),
        #istoricul datelor rulate (se updateaza din js)
        p(id="nrVfHistory",'Numar Varfuri:'),
        p(id="razaHistory",'Raze:'),
        p(id="formaHistory","Forma:"),
    ),
    mainPanel(
      #aici trebuie pus outputul
        plotOutput('plot1'),
        #in acest div putem pune ce creeaza js
        tags$div(id="chaosGame")
    ))
#aici e logica
server <- function(input,output
) {#observe event e pt js
  #cand apas pe butonul #button se executa functia shinyjs.draw cu parametrii dati
    observeEvent(input$button, {
        js$draw(input$nrVfInput, input$razaInput,
                input$formaInput)
    })
    # output$numeElementDinOutput = ce sa se puna in elementul de output
    output$plot1<-renderPlot({
        hist(rnorm(input$nrVfInput))
    })
}

#basic
shinyApp(ui = ui, server = server)
