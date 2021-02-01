library(shiny)
#librarie pt js
library(shinyjs)
library(shinydashboard)
library(rmarkdown)

#Nr minim varfuri
nrMinVf=1
#Nr max Varfuri
nrMaxVf=10

#numele noastre
numeElevi=list('Grupa 242 - ','Munteanu Vlad Paul','Alexandru Oana','Mihai Catalin','Alex Stefan')

#cod javascript 
jsCode <- '
//functia shinyjs care poate fi apelata din R
shinyjs.addHistory = function(params) {
  var defaultParams = {
    nrVfInput:5,
    razaInput:5,
    formaInput:"Forma 1",
    stepInput :2,
    nrRepetInput:5000
  };
  
  params = shinyjs.getParams(params, defaultParams);
  
  document.getElementById("nrVfHistory").innerHTML+=" "+params.nrVfInput+", ";
  document.getElementById("razaHistory").innerHTML+=" "+params.razaInput+", ";
  document.getElementById("formaHistory").innerHTML+=" "+params.formaInput+", ";
  document.getElementById("stepHistory").innerHTML+=" "+params.stepInput+", ";
  document.getElementById("nrRepetHistory").innerHTML+=" "+params.nrRepetInput+", ";
}'


# aici e interfata
ui <- fluidPage(
  #css
  tags$style(HTML("
      *{
        font-family:system-ui;
      }
      p{
      font-size:1.5rem;
      }
      }")),
  useShinyjs(),
  #aici apar functiile shinyjs din javascript(singurele functii care pot fi chemate din R)
  extendShinyjs(text = jsCode, functions = c("addHistory")),
  
  headerPanel('Chaos Game'),
  h5(numeElevi),
    sidebarPanel(
      #inputurile
      numericInput('nrVfInput', 'Numar Varfuri:', value=3,
                   min=nrMinVf,max=nrMaxVf),
      numericInput('razaInput', 'Raza Cercului:', value=175),
      numericInput('formaInput','Forma:',min =1,max=3,value=1),
      numericInput('stepInput','Step:',value=2,min=2,max=10),
      numericInput('nrRepetInput','Numar repetari:',value=5000,max=50000),
      # la apasarea butonului se executa programu
      actionButton("button", "Go"),
      #istoricul datelor rulate (se updateaza din js)
      p(id="nrVfHistory",'Numar Varfuri:'),
      p(id="razaHistory",'Raze:'),
      p(id="formaHistory","Forma:"),
      p(id="stepHistory","Step:"),
      p(id="nrRepetHistory","Nr Repetari:")
    ),
    #outputul (canvas)
      htmlOutput("chaosGame")
    )

#aici e logica
server <- function(input,output
) {
  #cand apas pe butonul #button se executa functia shinyjs.draw cu actualele inputuri si in output se pune canvas-ul cu chaos game 
  observeEvent(input$button, {
    js$addHistory(input$nrVfInput, input$razaInput,
                  input$formaInput,input$stepInput,input$nrRepetInput)
    #luam valorile
    nrVf=input$nrVfInput
    forma=input$formaInput
    raza=input$razaInput
    step=input$stepInput
    nrRepet=input$nrRepetInput
    output$chaosGame<- renderUI(tags$html(
      tags$style(HTML("
      canvas{
        position:absolute;
        right:20%;
        top:20%;
        padding:2rem;
      }
      }")),
      tags$body(
        singleton(tags$head(tags$script(src="https://cdn.jsdelivr.net/npm/p5@1.2.0/lib/p5.js")))
        ,singleton(tags$head(tags$script(paste('var xVf = [];
var yVf = [];
var pct = [];
var reps = ',nrRepet,';
var width=400;
var height=400
var pct = [width/2, height/2];

console.log(pct)
function setup() {
  createCanvas(400, 400);
  
  angleMode(DEGREES);
  width=400;
  height=400;
  pct = [width/2, height/2];
}
/* Functia shp1 creaza triunghiul Sierpinski in urmatorul mod: incepand din mijloc, 
se alege aleator un varf si punem un alt punct la jumatatea distantei dintre punctul
initial si varful ales. */
function shp1(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i!=k+1;i++){
    let x = width/2 + (r * cos(angle));
    let y = height/2 - (r * sin(angle));
    
    xVf[i] = x;
    yVf[i] = y;
    
    ellipse(x,y,16,16);
    angle += step;
  }
  
  fill(255);
  stroke(255);
  for(let i = 1; i != reps+1; i++){
    point(pct[0],pct[1]);
    
    let rVf = floor(random(1,k+1));
    
    pct[0] = ((s-1)*pct[0]+xVf[rVf])/s;
    pct[1] = ((s-1)*pct[1]+yVf[rVf])/s;
  }
  
}
/* Functia shp2 creaza un "fulg", astfel: Incepand din mijloc, se alege aleator un varf spre care sa 
mergem jumatate din distanta, insa varful ales trebuie sa fie diferit de cel precedent ales. */
function shp2(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i!=k+1;i++){
    let x = width/2 + (r * cos(angle));
    let y = height/2 - (r * sin(angle));
    
    xVf[i] = x;
    yVf[i] = y;
    
    ellipse(x,y,16,16);
    angle += step;
  }
  
  fill(255);
  stroke(255);
  let prevVf = 0;
  for(let i = 1; i != reps+1; i++){
    //ellipse(pct[0],pct[1], 2, 2);
    point(pct[0], pct[1]);
    
    let rVf = floor(random(1,k+1));
    while(rVf == prevVf){
      rVf = floor(random(1,k+1));
    }
    prevVf = rVf;
    
    pct[0] = ((s-1)*pct[0]+xVf[rVf])/s;
    pct[1] = ((s-1)*pct[1]+yVf[rVf])/s;
  }
}
/* Functia shp3 creaza un "fulg", astfel: Incepand din mijloc, se alege aleator un varf spre
care sa mergem jumatate din distanta, insa varful ales trebuie sa nu fie la stanga celui ales precedent. */
function shp3(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i!=k+1;i++){
    let x = width/2 + (r * cos(angle));
    let y = height/2 - (r * sin(angle));
    
    xVf[i] = x;
    yVf[i] = y;
    
    ellipse(x,y,16,16);
    angle += step;
  }
  
  fill(255);
  stroke(255);
  let prevVf = 0;
  for(let i = 1; i != reps+1; i++){
    //ellipse(pct[0],pct[1], 2, 2);
    point(pct[0], pct[1]);
    
    let rVf = floor(random(1,k+1));
    
    if(prevVf-1 == 0) leftVf = k;
    else leftVf = prevVf-1;
    
    while(rVf == leftVf){
      rVf = floor(random(1,k+1));
    }
    prevVf = rVf;
    
    pct[0] = ((s-1)*pct[0]+xVf[rVf])/s;
    pct[1] = ((s-1)*pct[1]+yVf[rVf])/s;
  }
}
function draw() {
  shp',forma,'(',nrVf,',',raza,',',step,');
  
}',sep=""))))
        ,singleton(tags$div(id = 'divCanvas2', style = 'width:auto; height:auto;')))))
  })
}

shinyApp(ui = ui, server = server)
