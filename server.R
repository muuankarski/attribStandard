library(shiny)
library(ggplot2)
library(grid)
library(gridExtra)

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  load("data/datPlot.RData")
  
  #   output$caption <- renderText({
  #     datPlot[1:4,1:4]
  #     #textInput$varx
  #   })
  
  
  
  variableInput <- reactive({
    switch(input$predictor)
  })
  
  
  
  
  # Scatterplot
  output$scatter <- renderPlot({
    cbPalette <- c("#E69F00","#56B4E9","#999999","#009E73", "#F0E442", "#0072B2","#D55E00", "#CC79A7")
    
    #varx <- datPlot$perGini
    varx <- datPlot[, input$predictor]
    ivA <- datPlot$socialBlame
    ivB <- datPlot$individualBlame
    group_general <- datPlot$group_general
    cntry <- as.character(datPlot$cntry)
    dat <- data.frame(ivA,ivB,varx,cntry,group_general)
    dat1 <- dat[dat$group_general != "Western Europe",]
    dat2 <- dat[dat$group_general == "Western Europe",]
    plot1 <- ggplot(dat1, aes(x=varx, y=ivA, 
                              label=cntry,group=1)) +
      geom_point(data=dat1, aes(color=group_general), size=3) +
      geom_smooth(method=lm, se=FALSE, alpha=.5, linetype="dashed", size=0.5) +  
      geom_text(size=4, vjust=-0.8, hjust=0.5) +
      # west reference
      geom_point(data=dat2, aes(x=varx, 
                                y=ivA,
                                color=group_general), 
                 alpha=.5, size=3) +
      geom_text(data=dat2, aes(x=varx, 
                               y=ivA, 
                               label=cntry),
                alpha=.5, size=4, vjust=-0.8, hjust=0.5) +
      # west reference
      labs(x = paste("Variable ", input$predictor,sep=""),
           y = "Support for social blame type of explanation (%)") + 
      scale_colour_manual(values=cbPalette) +
      #       coord_cartesian(xlim=c(-25,100)) +
      theme_minimal() +
      theme(legend.title=element_blank()) +
      theme(legend.text=element_text(size=12)) +
      theme(legend.position="top") +
      theme(axis.title.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=12)) +
      theme(axis.text.y = element_text(size=12)) +
      theme(axis.text.x = element_text(size=12)) +
      #guides(color = guide_legend(nrow = 2)) +
      theme(legend.key.size = unit(3, "mm"))
    ## individual blame
    plot2 <- ggplot(dat1, aes(x=varx, y=ivB, 
                              label=cntry,group=1)) +
      geom_point(data=dat1, aes(color=group_general), size=3) +
      geom_smooth(method=lm, se=FALSE, alpha=.5, linetype="dashed", size=0.5) +  
      geom_text(size=4, vjust=-0.8, hjust=0.5) +
      # west reference
      geom_point(data=dat2, aes(x=varx, 
                                y=ivB,
                                color=group_general), 
                 alpha=.5, size=3) +
      geom_text(data=dat2, aes(x=varx, 
                               y=ivB, 
                               label=cntry),
                alpha=.5, size=4, vjust=-0.8, hjust=0.5) +
      # west reference
      labs(x = paste("Variable ", input$predictor,sep=""),
           y = "Support for individual blame type of explanation (%)") + 
      scale_colour_manual(values=cbPalette) +
      #       coord_cartesian(xlim=c(-25,100)) +
      theme_minimal() +
      theme(legend.title=element_blank()) +
      theme(legend.text=element_text(size=12)) +
      theme(legend.position="top") +
      theme(axis.title.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=12)) +
      theme(axis.text.y = element_text(size=12)) +
      theme(axis.text.x = element_text(size=12)) +
      #guides(color = guide_legend(nrow = 2)) +
      theme(legend.key.size = unit(3, "mm"))
    
    grid.arrange(plot1,plot2, ncol=2)
  })
  
  output$correlation <- renderPrint({
    varx <- datPlot[, input$predictor]
    ivA <- datPlot$socialBlame
    ivB <- datPlot$individualBlame
    group_general <- datPlot$group_general
    cntry <- as.character(datPlot$cntry)
    dat <- data.frame(ivA,ivB,varx,cntry,group_general)
    dat1 <- dat[dat$group_general != "Western Europe",]
    CdvA <- cor(dat1$varx,dat1$ivA)
    CdvB <- cor(dat1$varx,dat1$ivB)
    title <- c("Social Blame",
               "Individual Blame")
    corr <- c(CdvA,CdvB)
    cordat <- data.frame(title,corr)
    cordatT <- as.data.frame(t(cordat))
    library(xtable)
    print.xtable(xtable(cordatT), type="html",width=600,
                 include.rownames=FALSE,
                 include.colnames=FALSE, )
  })
  
  
  
})