library(shiny)
library(ggplot2)
library(grid) 

# Declare colors
power_fill_color      = "#00ff33"
power_text_color      = power_fill_color
H0_fill_color         = "#0000ff"
beta_fill_color       = "#003300"
alpha_fill_color      = "#00ffff"
HA_color              = "#00ff33"
H0_line_color         = "#0d6374"
background_color      = "#f9f0ea"
background_color      = "#ffffcc"
arrow_colors          = "#7D7F7F"

legend_labels <- list("Null Dist.  ", 
                      expression(paste(alpha,"=","Prob. of type I error  ")),
                      expression(paste(beta,"=","Prob. of type II error  ")),
                      expression(Power~"="~1~"-"~beta))

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  output$SPICPlot <- renderPlot({makePlot()})
  
  makePlot <- function(){
    
    # Read input values
    m1  <- input$m1
    m2  <- input$m2
    sd1 <- input$sd1
    sd2 <- input$sd2
    alpha  <- input$alpha
    # don't forget to divide alpha by 2 and to add the mean of the null distribution 
    # when calculating z_crit
    z_crit <- -(qnorm(input$alpha/2., mean=m1, sd=sd1))+m1 
    Show_Arrows <- input$show_arrows
    
    # Do the math and so fourth
    
    # set length of tails
    min1 <- m1-sd1*6
    max1 <- m1+sd1*6
    min2 <- m2-sd2*6
    max2 <- m2+sd2*6  
    
    increment = 0.01
    # create x sequence
    x <- seq(min(min1,min2), max(max1, max2), increment)
    # generate normal dist #1
    y1 <- dnorm(x, m1, sd1)
    # put in data frame
    df1 <- data.frame("x" = x, "y" = y1)
    
    # generate normal dist #2
    y2 <- dnorm(x, m2, sd2)
    # put in data frame
    df2 <- data.frame("x" = x, "y" = y2)
    
    # Positions for labels
    Power_pos = c(x = max2*0.8, y = max(y1,y2))
    m1_pos    = c(x = min1*0.65, -0.1)#y = max(y1,y2))
    
    # Define vertical lines
    vline_y  <- seq(0, 1.1*max(y1, y2), by=increment)
    vline_df <- data.frame(xm=m1-z_crit, xp=m1+z_crit, y=vline_y, id="alpha") 
    
    # Alpha polygon
    poly1  <- data.frame(x=x, y=y1)
    poly11 <- poly1[poly1$x >= m1+z_crit, ] 
    poly11 <- rbind(c(m1+z_crit, 0), poly11)  # add lower-left corner
    poly22 <- poly1[poly1$x <= m1-z_crit, ] 
    poly22 <- rbind(poly22, c(m1-z_crit, 0))  # add lower-left corner
    poly1  <- rbind(poly22, poly11)
    
    # Beta polygon
    poly2 <- df2
    poly2 <- poly2[poly2$x <= m1+z_crit,] 
    poly2 <- rbind(poly2, c(m1+z_crit, 0))  # add lower-left corner
    
    # power polygon; 1-beta
    poly3 <- df2
    poly3 <- poly3[poly3$x >= m1+z_crit,] 
    poly3 <-rbind(poly3, c(m1+z_crit, 0))  # add lower-left corner
    
    # H0 polygon
    poly4 <- df1
    #poly3 <- poly3[poly3$x >= z_crit,] 
    #poly3 <-rbind(poly3, c(z_crit, 0))  # add lower-left corner
    
    # combine polygons. 
    poly1$id <- 2 # alpha
    poly2$id <- 3 # beta
    poly3$id <- 4 # power; 1 - beta, give it the highest number to make it the top layer
    poly4$id <- 1
    
    poly <- rbind(poly1, poly2, poly3, poly4)
    poly$id <- factor(poly$id,  labels=c("H0", "alpha","beta","power"))
    
    alpha_label   <- paste("alpha == ", alpha)
    m1_label      <- paste("mu[1] == ", m1)
    m2_label      <- paste("mu[2] == ", m2)
    m1_label      <- expression(paste("Value is ", mu[2],",", R^{2},'=',m2))
    sd1_label     <- paste("sigma[1] == ", sd1)
    sd2_label     <- paste("sigma[2] == ", sd2)
    beta_label    <- sprintf("%.5f", (1-pnorm(z_crit, mean=m2, sd=sd2)))
    power_label   <- sprintf("Power = %.5f", (1-pnorm(z_crit, mean=m2, sd=sd2)))
    
    eqn <- as.character(as.expression(
      substitute(italic(alpha) == a * "    ," 
                 ~~ italic(mu[1]) == M1 *"," ~~italic(sigma[1]) == SD1*"   ,"
                 ~~ italic(mu[2]) == M2 *"," ~~italic(sigma[2]) == SD2,
                 list(a  = format(alpha, digits=3),
                      M1  = format(m1, digits=3, nsmall=2),
                      SD1 = format(sd1, digits=2, nsmall=2),
                      M2  = format(m2, digits=3, nsmall=2),
                      SD2 = format(sd2, digits=2, nsmall=2)
                 ))))
    
    # Make figure
    fig1 <- ggplot(poly, aes(x,y, fill=id, group=id)) +
      geom_polygon(alpha=I(8/10)) +
      # add line for treatment group
      geom_line(data=df1, aes(x,y, color="H0", group=NULL, fill=NULL), size=1.5, show_guide=F) + 
      # add line for treatment group. These lines could be combined into one dataframe.
      geom_line(data=df2, aes(color="HA", group=NULL, fill=NULL),size=1.5, show_guide=F) +
      # add vlines for z_crit 
      geom_line(data=vline_df, aes(x=xm, y=y), color="black", size=1, linetype="dashed") +
      geom_line(data=vline_df, aes(x=xp, y=y), color="black", size=1, linetype="dashed") +
      
      # Labels for power
      annotate("text", label=power_label, x=Power_pos[1], y=Power_pos[2], parse=F,#fontface="italic",
               family="Times", size=10, col=power_text_color) +
      
      # Labels for distribution properties
      annotate("text", label=eqn, x=m1_pos[1], y=-0.1, hjust=0.1, vjust=0.6,
               family="Times", size=10, col="black", parse=T) +
      
      # change colors 
      # Line colors
      scale_color_manual("Group", 
                         values= c("HA" = "#981e0b","H0" = "black")) +
      # Fill colors
      scale_fill_manual(values=c("H0"=H0_fill_color, "alpha" = alpha_fill_color,
                                 "beta" = beta_fill_color, "power"=power_fill_color), 
                        name="",
                        labels=legend_labels) +  
      # H_0 title
      annotate("text", label="H[0]", x=m1, y=max(y1)*1.1, fontface="italic",
               family="Times", parse=T, size=8) +
      # H_a title
      annotate("text", label="H[A]", x=m2, y=max(y2)*1.1, fontface="italic",
               family="Times", parse=T, size=8) +
      ggtitle("Statistical-Power Interactive Calculator\n") +
      # remove some elements
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            plot.background = element_rect(fill=background_color),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size=22),
            legend.position="bottom",
            legend.text = element_text(size=14)) 
    
    if (Show_Arrows) {
      fig1 <- fig1 +   
        # add lines
        annotate("segment", x=m1, y=-0.03, xend=(m1+z_crit)*0.95, yend=-0.03, arrow = 
                   arrow(length = unit(0.3, "cm")), size=1, color=arrow_colors) +
        annotate("segment", x=m1, y=-0.03, xend=(m1-z_crit)*0.95, yend=-0.03, arrow = 
                   arrow(length = unit(0.3, "cm")), size=1, color=arrow_colors) +
        annotate("text", label="Do~not~reject~H[0]", x=m1, y=-0.06, 
                 family="Times", parse=T, size=6, color=arrow_colors) +
        
        annotate("segment", x=(m1+z_crit)*1.05, y=-0.03, xend=max2, yend=-0.03, arrow = 
                   arrow(length = unit(0.3, "cm")), size=1, color=arrow_colors) +
        annotate("segment", x=max2, y=-0.03, xend=(m1+z_crit)*1.05, yend=-0.03, arrow = 
                   arrow(length = unit(0.3, "cm")), size=1, color=arrow_colors) +
        annotate("text", label="Reject~H[0]", x=(max2+m1+z_crit)/2., y=-0.06, 
                 family="Times", parse=T, size=6, color=power_fill_color) +
        
        annotate("segment", x=(m1-z_crit)*1.05, y=-0.03, xend=min1, yend=-0.03, arrow = 
                   arrow(length = unit(0.3, "cm")), size=1, color=arrow_colors) +
        annotate("segment", x=min1, y=-0.03, xend=(m1-z_crit)*1.05, yend=-0.03, arrow = 
                   arrow(length = unit(0.3, "cm")), size=1, color=arrow_colors) +
        annotate("text", label="Reject~H[0]", x=(min1-m1-z_crit)/2., y=-0.06, 
                 family="Times", parse=T, size=6, color=power_fill_color) 
    }
    print(fig1)
  }
  output$savePlot <- downloadHandler(
    filename = function() { paste('Power_Analysis_StatStudio.pdf') },
    content = function(file){
      pdf(file = file, width=11, height=8.5)
      makePlot()
      dev.off()
    }
  )
  
  output$savePNGPlot <- downloadHandler(
    filename = function() { paste('Power_Analysis_StatStudio.png') },
    content = function(file){
      png(file = file, width=1200, height=800, units="px")
      makePlot()
      dev.off()
    }
  )
  foo <- reactive({      
    makePlot()
    png(file="./www/foo2.png", width=1200, height=800, units="px")
    dev.off()})
  
})