library(shiny)

buildDist <- function() {
  pcomm <<- sample(1:5,500,TRUE,c(.1,.1,.2,.3,.3))
  pprob <<- sample(1:5,500,TRUE,c(.1,.1,.1,.3,.4))
  pntgrty <<- sample(1:5,500,TRUE,c(.1,.1,.1,.3,.4))
  port <- .2*pcomm + .3*pprob + .5*pntgrty
  eval <- data.frame(cbind(port,pcomm,pprob,pntgrty))  
  fit <<- lm(port~.,data=eval)
  pval <<- quantile(port,.7)
}

predictPerf <- function(emp, hir) {
  predict(emp,hir)
}

hiringDecision <- function(cval,prd) {
  if (prd > cval) {
    "Hire"
  } else {
    "Do Not Hire"
  }
}

emp <- buildDist()

shinyServer(function(input, output) {
  values <- reactiveValues()
  values$df <- data.frame(Name=NA,Comm=NA,Problem=NA,Integrity=NA,Predict=NA,Decision=NA)
  newEntry <- observe({
    if (input$submit > 0) {
      isolate(hire <- data.frame(cbind(pcomm=as.integer(input$select1),
                                       pprob=as.integer(input$select2),
                                       pntgrty=as.integer(input$select3))))
      isolate(values$pprd <- sprintf("%.2f",predictPerf(fit,hire)))
      isolate(values$dec <- hiringDecision(pval,values$pprd))
      newLine <- isolate(c(input$text1,input$select1,input$select2,
                           input$select3,values$pprd,values$dec))
      isolate(values$df <- rbind(values$df, newLine))
      isolate(values$df$Comm <- as.integer(values$df$Comm))
      isolate(values$df$Problem <- as.integer(values$df$Problem))
      isolate(values$df$Integrity <- as.integer(values$df$Integrity))
      isolate(values$df$Predict <- as.numeric(values$df$Predict))
    }
    if (input$submit == 1) {
      isolate(values$df <- values$df[complete.cases(values$df),])
    }
  })
      
  output$select1 <- renderText({values$pprd})
  
  output$decision <- renderText({values$dec})
  
  output$empHist <- renderPlot({
    op <- par(mfrow = c(2,2))
    hist(pcomm,main="Communication",xlab="",col="Gray")
    lines(c(input$select1, input$select1), c(0, 200),col="red",lwd=5)
    hist(pprob,main="Problem Solving",xlab="",col="Gray")
    lines(c(input$select2, input$select2), c(0, 200),col="red",lwd=5)
    hist(pntgrty,main="Integrity",xlab="",col="Gray")
    lines(c(input$select3, input$select3), c(0, 200),col="red",lwd=5)
    hist(fitted(fit),main="Predicted Performance",xlab="",col="Gray")
    lines(c(values$pprd, values$pprd), c(0, 200),col="red",lwd=5)
    par(op)
  })
  
  output$candHist <- renderPlot({
    op <- par(mfrow = c(2,2))
    hist(values$df$Comm,main="Communication",xlab="",col="Gray")
    lines(c(input$select1, input$select1), c(0, 200),col="red",lwd=5)
    hist(values$df$Problem,main="Problem Solving",xlab="",col="Gray")
    lines(c(input$select2, input$select2), c(0, 200),col="red",lwd=5)
    hist(values$df$Integrity,main="Integrity",xlab="",col="Gray")
    lines(c(input$select3, input$select3), c(0, 200),col="red",lwd=5)
    hist(values$df$Predict,main="Predicted Performance",xlab="",col="Gray")
    lines(c(values$pprd, values$pprd), c(0, 200),col="red",lwd=5)
    par(op)
  })
  
  output$table <- renderTable({tail(values$df,5)})
})
