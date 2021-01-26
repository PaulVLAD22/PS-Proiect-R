library(shiny)
library(shinyjs)
library(Rcpp)

#Nr minim varfuri
nrMinVf=1
#Nr max Varfuri
nrMaxVf=10
# Lista de forme 
forma=list('Forma 1','Forma 2','Forma 3','Forma 4')


jsCode <- '
var xVf = [];
var yVf = [];
var pct = [];
var reps = 50000;

function setup() {
  createCanvas(600, 600);
  
  angleMode(DEGREES);
  
  pct = [width/2, height/2];
}

function shp1(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
    let x = width/2 + (r * cos(angle));
    let y = height/2 - (r * sin(angle));
    
    xVf[i] = x;
    yVf[i] = y;
    
    ellipse(x,y,16,16);
    angle += step;
  }
  
  fill(255);
  stroke(255);
  for(let i = 1; i <= reps; i++){
    point(pct[0],pct[1]);
    
    let rVf = floor(random(1,k+1));
    
    pct[0] = ((s-1)*pct[0]+xVf[rVf])/s;
    pct[1] = ((s-1)*pct[1]+yVf[rVf])/s;
  }
}

function shp2(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
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
  for(let i = 1; i <= reps; i++){
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

function shp3(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
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
  for(let i = 1; i <= reps; i++){
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

function shp4(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
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
  for(let i = 1; i <= reps; i++){
    //ellipse(pct[0],pct[1], 2, 2);
    point(pct[0], pct[1]);
    
    let rVf = floor(random(1,k+1));
    
    if(prevVf-2 == 0) leftVf = k;
    else if(prevVf-2 < 0) leftVf = k-1;
    else leftVf = prevVf-2;
    
    while(rVf == leftVf){
      rVf = floor(random(1,k+1));
    }
    prevVf = rVf;
    
    pct[0] = ((s-1)*pct[0]+xVf[rVf])/s;
    pct[1] = ((s-1)*pct[1]+yVf[rVf])/s;
  }
}
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
  //shp4(3, width/2-25, 2);
  // nu merge sh4 tre sa fac ceva conexiune cu output
  noLoop();
  
}'



ui <- fluidPage(
  theme="style.css",
  includeHTML("index.html"),
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("draw","setup")),
  headerPanel('Chaos Game'),
  sidebarPanel(
    numericInput('nrVfInput', 'Numar Varfuri:', value=5,
                min=nrMinVf,max=nrMaxVf),
    numericInput('razaInput', 'Raza Cercului:', value=5),
    selectInput('formaInput', 'Alege Forma:',
                forma),
    actionButton("button", "Go"),
    p(id="nrVfHistory",'Numar Varfuri:'),
    p(id="razaHistory",'Raze:'),
    p(id="formaHistory","Forma:"),
  ),
  mainPanel(
    plotOutput('plot1'),
    tags$div(id="chaosGame")
  ),
  sidebarLayout(
    sidebarPanel("Cod Javascript:"),
    mainPanel(
      code(
        '
  var xVf = [];
  var yVf = [];
  var pct = [];
  var reps = 50000;

shinyjs.setup= function (params) {
  
  document.getElementById("chaosGame").innerHTML=createCanvas(600, 600);
  background(0);
  document.getElementById("formaHistory").innerHTML+=", ";
  angleMode(DEGREES);
  
  pct = [width/2, height/2];
}

function shp1(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
    let x = width/2 + (r * cos(angle));
    let y = height/2 - (r * sin(angle));
    
    xVf[i] = x;
    yVf[i] = y;
    
    ellipse(x,y,16,16);
    angle += step;
  }
  
  fill(255);
  stroke(255);
  for(let i = 1; i <= reps; i++){
    point(pct[0],pct[1]);
    
    let rVf = floor(random(1,k+1));
    
    pct[0] = ((s-1)*pct[0]+xVf[rVf])/s;
    pct[1] = ((s-1)*pct[1]+yVf[rVf])/s;
  }
}

function shp2(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
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
  for(let i = 1; i <= reps; i++){
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

function shp3(k,r,s){
  let step = 360 / k;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
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
  for(let i = 1; i <= reps; i++){
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

function shp4(k,r,s){
  let step = 360 / k;
  document.getElementById("name").innerHTML+=params.nrVfInput;
  let angle = 90;
  
  background(0);
  fill(0,255,0);
  
  for(let i = 1; i<=k;i++){
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
  for(let i = 1; i <= reps; i++){
    //ellipse(pct[0],pct[1], 2, 2);
    point(pct[0], pct[1]);
    
    let rVf = floor(random(1,k+1));
    
    if(prevVf-2 == 0) leftVf = k;
    else if(prevVf-2 < 0) leftVf = k-1;
    else leftVf = prevVf-2;
    
    while(rVf == leftVf){
      rVf = floor(random(1,k+1));
    }
    prevVf = rVf;
    
    pct[0] = ((s-1)*pct[0]+xVf[rVf])/s;
    pct[1] = ((s-1)*pct[1]+yVf[rVf])/s;
  }
}
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
  //shp4(3, width/2-25, 2);
  // nu merge sh4 tre sa fac ceva conexiune cu output
  noLoop();
  
}'
      )
    ))
)

server <- function(input,output
) {
  observeEvent(input$button, {
    js$draw(input$nrVfInput, input$razaInput,
                     input$formaInput)
  })
  output$plot1<-renderPlot({
    hist(rnorm(input$nrVfInput))
  })
}

shinyApp(ui = ui, server = server)

