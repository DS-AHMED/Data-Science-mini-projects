library(shiny) #For dashboard
library(reshape) #For data manipulation
library(dplyr) #For data manipulation
library(tidyr) #For data manipulation
library(corrplot) #For data plotting
library(ggplot2) #For data plotting
library(ggpubr) #For sub plotting
library(waffle) #For plotting waffle chart
library(wordcloud) #For plotting word cloud
df = read.csv("C:/ahmed/UM/University Of Malaya/YEAR 1 # FIRST SEMESTER/Principle of Data Science/Project/Dataset/canadian_immigration_data.csv")
DF = read.csv("C:/ahmed/UM/University Of Malaya/YEAR 1 # FIRST SEMESTER/Principle of Data Science/Project/Dataset/UpdatedDF_1.csv")
names(df)[4:37] <- c("1980", "1981", "1982", "1983", "1984",
                     "1985", "1986", "1987", "1988", "1989",
                     "1990", "1991", "1992", "1993", "1994",
                     "1995", "1996", "1997", "1998", "1999",
                     "2000", "2001", "2002", "2003", "2004",
                     "2005", "2006", "2007", "2008", "2009",
                     "2010", "2011", "2012", "2013")
df_top <- df[1:195,] %>% 
  arrange(desc(Total)) %>%
  select(1,4:37)



## Dashboard user interface
ui <- fluidPage(
  
  navbarPage(title = "Dashboard for Immigration to Canada",
             tabPanel("Dashboard", 
                      
                      tabsetPanel(
                        tabPanel("Q1",  column(12,
                                               h3("Q1 - How Many Total Immigrants to Canada From 1980 to 2013?")),   
                                 plotOutput("Q1")
                        ),
                        tabPanel("Q2",column(12,
                                             h3("Q2 - How Many Total immigrants to Canada by Continent From 1980 to 2013?")),
                                 plotOutput("Q2_1"), plotOutput("Q2_2")
                        ),
                        tabPanel("Q3",column(12,
                                             h3("Q3 - How Many Total Immigrants to Canada by Country From 1980-2013?")),
                                 selectInput("country1","country",
                                             na.omit(unique(df$Country))),
                                 plotOutput("Q3")
                        ),
                        tabPanel("Q4",column(12,
                                             h3("Q4 - What Are The Immigrants Counts for The Top-5 Immigrants Countries From 1980 to 2013?")),
                                 selectInput("country2","country",
                                             c("all",as.character(df_top$Country[1:5]))),
                                 plotOutput("Q4")
                        ),
                        tabPanel("Q5",column(12,
                                             h3("Q5 - What Are The Minimum, Median, Maximum, Inter Quartile Ranges, and Outlier Values
for Immigrants to Canada From 1980 to 2013 Per Decade?")),
                                 plotOutput("Q5")
                        )
                        ,
                        tabPanel("Q6",column(12,
                                             h3("Q6 - Which Countries in The Future Will Have More Immigrants,
and Which Will Have Less?")),
                                 selectInput("country3","country",
                                             na.omit(unique(df$Country))),
                                 plotOutput("Q6")
                        )
                        ,
                        tabPanel("Q7",column(12,
                                             h3("Q7 - Which Countries Would Have a Dominant Immigrants (Population)?")),
                                 
                                 plotOutput("Q7_1"),
                                 column(textInput("a1", "country1:", ""),width = 4),
                                 column(textInput("a2", "country2:", ""),width = 4),
                                 column(textInput("a3", "country3:", ""),width = 4),
                                 plotOutput("Q7_2"),
                                 plotOutput("Q7_3")
                        )
                        ,
                        tabPanel("Q8",column(12,
                                             h3("Q8 - What Are The Immigration Trends of The Continents From 1980 to 2013?")),
                                 
                                 plotOutput("Q8")
                        )
                      )
                      
                      
                      
             ),
             
             tabPanel("Summary",
                      fluidPage(
                        
                        fluidRow(
                          #Personal information
                          column(4,
                                 h2("Summery"),
                                 p("Group C"),
                                 p("Instructions for use")
                          ), 
                          column(2),
                        ),
                        #reflection
                        fluidRow(
                          column(12,h2("In the process of answering the question, through the search of the shiny development document, the trend in the countries is searched according to the requirements of the question"))
                        )
                      )
             )
  )
  
)



server <- function(input, output) {
  
  #title reactive
  plot_title <- eventReactive(input$but, {input$titl})
  
  output$Q1 <- renderPlot({
    Q1Aplot <- df %>%
      filter(Country=="Total") %>% 
      select(1,4:37) %>%
      gather(,key = "year", value = "number", 2:35) %>%
      ggplot(aes(x=year, y=number))+
      geom_line(aes(group=Country, color=Country))+
      geom_point(aes(color=Country))+
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom",
            axis.text.x = element_text(angle=60, hjust=1))+
      labs(x="YEAR",y="TOTAL IMMIGRANTS",title="CURVE CHART")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_discrete(breaks = seq(1980, 2013, by = 2))+ #A jump of 2 years
      scale_y_continuous(breaks = seq(10000, 300000, by = 10000)) #Scale y axis
    Q1Bplot <- df %>%
      filter(Country=="Total") %>% 
      select(1,4:37) %>%
      gather(key = "year",value = "number", 2:35) %>%
      ggplot(aes(x=year,y=number))+
      geom_col(fill="lightblue")+
      # scale_y_continuous(expand = c(0,0))+
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom",
            axis.text.x = element_text(angle=60, hjust=1))+
      labs(x="YEAR",y="TOTAL IMMIGRANTS",title="BAR CHART")+
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_discrete(breaks = seq(1980, 2013, by = 2))+ #A jump of 2 years
      scale_y_continuous(breaks = seq(10000, 300000, by = 10000)) #Scale y axis
    A <- ggarrange(Q1Aplot, Q1Bplot,
                   labels = c("A", "B"),
                   ncol = 2, nrow = 1)
    A
    
  })
  output$Q2_1<- renderPlot({
    
    Q2Aplot <- ggplot(data = df[196:201,], aes(x=Continent,y=Total, fill=Continent))+
      geom_col()+
      scale_y_continuous(expand = c(0,0))+
      geom_text(aes(label=Total),
                position = position_stack(vjust = 0.5))+
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom",
            axis.text.x = element_text(angle=30, hjust=1))+
      labs(x="Continent",y="Total immigration",title="BAR CHART WITH COUNTS") +
      theme(plot.title = element_text(hjust = 0.5)) 
    Q2Aplot
  })
  output$Q2_2<- renderPlot({
    
    Q2DF <- DF[-c(196,197),] #Remove last two rows as they do not have a continent
    Q2DF <- tapply(Q2DF$Total,Q2DF$Continent,sum) #Sum total immigrants per continent.
    Q2DF <- as.data.frame.table(Q2DF) #Convert it to a data frame
    names <- c("Africa", "Asia", "Europe", "Latin America and the Caribbean","Northern America","Oceania") 
    percentage <- round(Q2DF$Freq/sum(Q2DF$Freq)*100,2) #Count the percentage of each Continent
    lebals <- paste(names, percentage) # add percents to labels 
    lebals <- paste(lebals,"%",sep="") # add % to labels 
    Q2DF$Continent <- lebals #Add the labels as a new column "Continent"
    Q2DF<- Q2DF[,-1] #Remove the old column of names
    names(Q2DF)[1] <- "Total" 
    # Visualization.
    Q2Bplot <- ggplot(Q2DF, aes(x="", y=Total, fill=Continent)) +
      geom_bar(stat="identity", width=1, color="White") +
      coord_polar("y", start=0) + 
      ggtitle("PIE CHART WITH PERCENTAGES") +
      theme(plot.title = element_text(hjust = 0.5))+
      theme_void() # remove background, grid, numeric labels
    Q2Bplot
  })
  output$Q3<- renderPlot({
    if(nrow(df %>%filter(Country==input$country1))!=0){
      Q3Aplot <- df %>%
        filter(Country==input$country1) %>% #Here the country needs to be automated.
        select(1,4:37) %>%
        gather(,key = "year", value = "number", 2:35) %>%
        ggplot(aes(x=year, y=number))+
        geom_line(aes(group=Country, color=Country))+
        geom_point(aes(color=Country))+
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "bottom",
              axis.text.x = element_text(angle=60, hjust=1))+
        labs(x="YEAR",y="NUMBER OF IMMIGRANTS",title="CURVE CHART") +
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_discrete(breaks = seq(1980, 2013, by = 2)) #A jump of 2 years
      Q3Bplot <- df %>%
        filter(Country==input$country1) %>%
        select(1,4:37) %>%
        gather(,key = "year",value = "number", 2:35) %>%
        ggplot(aes(x=year,y=number))+
        geom_col(fill="lightblue")+
        scale_y_continuous(expand = c(0,0))+
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "bottom",
              axis.text.x = element_text(angle=60, hjust=1))+
        labs(x="YEAR",y="NUMBER OF IMMIGRANTS",title="BAR CHART")+
        theme(plot.title = element_text(hjust = 0.5)) +
        scale_x_discrete(breaks = seq(1980, 2013, by = 2)) #A jump of 2 years
      # Creating a sub data set 
      Q3DF <- DF[DF$Country == input$country1,] #This must be automated (User input)
      Q3DF <- Q3DF[,-c(1:4, 39)]
      Q3DF <- data.frame(Q3DF=as.numeric(Q3DF))
      
      # Visualization
      Q3Cplot_box <- ggplot(data = Q3DF, aes(x = Q3DF, y = '')) + 
        geom_boxplot(fill="green") +
        # coord_cartesian(ylim = c(0,10000)) + # I set the y axis scale so the plot looks better.
        ggtitle("CENTERAL TENDENCIES") +
        theme(plot.title = element_text(hjust = 0.5))+
        xlab("NUMBER OF IMMIGRANTS") + ylab("BOX CHART")
      
      A <- ggarrange(Q3Aplot, Q3Bplot,Q3Cplot_box,
                     ncol = 2, nrow = 2)
      A
      
    }
    
    
  })
  output$Q4<- renderPlot({
    # Visualization
    if(input$country2=="all"){
      Q4plot <- df_top[1:5,] %>%
        gather(,key = "year", value = "number", 2:35) %>%
        ggplot(aes(x=year, y=number))+
        geom_line(aes(group=Country, color=Country))+
        geom_point(aes(color=Country))+
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "bottom",
              axis.text.x = element_text(angle=60, hjust=1))+
        labs(x="YEAR",y="NUMBER OF IMMIGRANTS",title="TOP-5 CURVE CHART")
      Q4plot
    }else{
      Q4plot <- df_top[df_top$Country==input$country2,] %>%
        gather(,key = "year", value = "number", 2:35) %>%
        ggplot(aes(x=year, y=number))+
        geom_line(aes(group=Country, color=Country))+
        geom_point(aes(color=Country))+
        theme(panel.background = element_blank(),
              axis.line = element_line(),
              legend.position = "bottom",
              axis.text.x = element_text(angle=60, hjust=1))+
        labs(x="YEAR",y="NUMBER OF IMMIGRANTS",title="TOP-5 CURVE CHART")
      Q4plot
    }
    
  })
  output$Q5<- renderPlot({
    
    # creating empty lists to append the values for each year using for loop
    total_decade80s = vector(mode = "list") 
    total_decade90s = vector(mode = "list")
    total_decade2000s = vector(mode = "list")
    Years20s <- for (country in DF$Country) {
      a <- select(filter(DF, Country == country), X2000:X2009 )
      total <- apply(a, 1, sum)
      total_decade2000s <- append(total_decade2000s, total)
    }
    
    Years90s <- for (country in DF$Country) {
      a <- select(filter(DF, Country == country), X1990:X1999 )
      total <- apply(a, 1, sum)
      total_decade90s <- append(total_decade90s, total)
    }
    
    Years80s <- for (country in DF$Country) {
      a <- select(filter(DF, Country == country), X1980:X1989 )
      total <- apply(a, 1, sum)
      total_decade80s <- append(total_decade80s, total)
    }
    
    # making a data frame using the appended lists
    total_decade <- as.data.frame(cbind(DF$Country, total_decade80s,total_decade90s,total_decade2000s))
    total_decade <- total_decade[-nrow(total_decade),] #deleting the total row
    names(total_decade) <- c("Country", "1980s", "1990s", "2000s") 
    total_decade$`1980s` <- as.integer(total_decade$`1980s`) #changing the values of each column to integer 
    total_decade$`1990s` <- as.integer(total_decade$`1990s`)
    total_decade$`2000s` <- as.integer(total_decade$`2000s`)
    

    #reshaping the data to a from suitable for box plotting
    total_decade <- gather(total_decade, Decades, value, 2:4) 
    
    #Visualization.
    Q5plot_box <- ggplot(total_decade, aes(x = Decades, y = value, fill = Decades)) +
      geom_boxplot(aes(x=country, y=value)) +
      coord_cartesian(ylim = c(0,50000)) +
      ggtitle("NUMBER OF IMMIGRANTS FOR EACH DECADE") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("DECADES") + ylab("NUMBER OF IMMIGRANTS") 
    Q5plot_box 
  })
  output$Q6<- renderPlot({
    Q6DF <- DF[DF$Country==input$country3,5:38] #Here "China" is the user input, the purpose is to automate this option
    Q6DF <- gather(Q6DF,Year,Total,1:34) #Transposing the dataset 
    Q6DF$Year<-gsub("X","",as.character(Q6DF$Year)) #Removing X from years
    Q6DF$Year <- as.integer(Q6DF$Year)
    
    
    Q6Regplot <- ggplot(data = Q6DF, aes(x= Year, y=Total)) +
      geom_point() +
      geom_smooth(method="lm") +
      # scale_x_discrete(breaks = seq(1980, 2013, by = 2)) + #A jump of 2 years
      # scale_y_continuous(breaks = seq(10000, 50000, by = 5000)) +
      ggtitle("LINEAR REGRESSION PLOT") +
      theme(plot.title = element_text(hjust = 0.5))+
      xlab("YEAR") + ylab("NUMBER OF IMMIGRANTS")
    Q6Regplot
  })
  output$Q7_1<- renderPlot({
    Q7DFA <- DF[c("Country", "Total")] #Choosing these two columns only
    Q7DFA <- Q7DFA[-c(nrow(Q7DFA), nrow(Q7DFA)-1),] # removing the last two rows ["Unknown" "Total"]
    
    Q7Word_plot <- wordcloud(words = Q7DFA$Country, freq = Q7DFA$Total, min.freq = 1,
                             max.words=2000, random.order=FALSE, rot.per=0.35,
                             colors=brewer.pal(8, "Dark2"))
    Q7Word_plot
  })
  output$Q7_2<- renderPlot({
    Q7DFA <- DF[c("Country", "Total")] #Choosing these two columns only
    Q7DFA <- Q7DFA[-c(nrow(Q7DFA), nrow(Q7DFA)-1),] # removing the last two rows ["Unknown" "Total"]
    Q7DFB <-arrange(Q7DFA, desc(Total))
    Q7DFB <- Q7DFB[1:8,] #Choosing the first 8 rows >> top 8
    Q7DFB$Total <- Q7DFB$Total%/%10000 # scaling down the values by a factor of 10000 to be plottable 
    # used split() to make a list that contains countries names and their values which is the total number of immigrants
    waffle_list <- split(Q7DFB$Total, Q7DFB$Country) 
    waffle_list <- unlist(waffle_list) # un-list the data because waffle() doesn't support lists
    
    
    #Automated visualization 
    Q7DFC <- filter(Q7DFA, Total >100) 
    Q7DFC <- filter(Q7DFC, Country %in% c(input$a1 ,input$a2, input$a3)) # the chosen countries ## To be automated
    if(min(Q7DFC$Total)< (max(Q7DFC$Total)-min(Q7DFC$Total))/3){
      Q7DFC$Total <- Q7DFC$Total%/%min(Q7DFC$Total)
    }else{
      Q7DFC$Total <- Q7DFC$Total%/%(min(Q7DFC$Total)/5)
      
    }    
    
    
    #used split() to make a list that contains countries names and their values which is the total number of immigrants
    waffle_list2 <- split(Q7DFC$Total, Q7DFC$Country ) 
    waffle_list2 <- unlist(waffle_list2) # un-list the data because waffle() doesn't support lists
    Q7waffle <- waffle(waffle_list2)
    
    A <- ggarrange( waffle(waffle_list), Q7waffle ,
                    ncol = 2, nrow = 1,widths = c(2,1))
    A
    
  })
  output$Q8<- renderPlot({
    # Visualized by line chart
    df[196:201,] %>%
      select(2,4:37) %>%
      gather(,key = "year", value = "number", 2:35) %>%
      ggplot(aes(x=year, y=number))+
      geom_line(aes(group=Continent, color=Continent))+
      geom_point(aes(color=Continent))+
      scale_y_continuous(breaks = seq(5000, 170000, by = 10000)) +
      theme(panel.background = element_blank(),
            axis.line = element_line(),
            legend.position = "bottom",
            axis.text.x = element_text(angle=60, hjust=1))+
      labs(x="YEAR",y="NUMBER OF IMMIGRANTS",title=" IMMIGRATION TREND OF THE CONTINENTS FROM 1980 to 2013")
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
