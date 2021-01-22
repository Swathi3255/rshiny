library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(dplyr)
library(openintro)
library(plyr)
library(ggplot2)
library(plotly)
library(shinythemes)

#dataset for TAB 1
df = read.csv("new df.csv")
df = df%>%mutate(popup_info = paste("Name", name, "<br/>", "State: ",state,"<br/>","City: ",city, "<br/>","Status: ", status))
df$funding_total_usd = as.integer(df$funding_total_usd)

#line graph
new_df = data.frame("x" = '', "freq" = '', "years" = '')
for (i in 2000:2014){
  tmp_df = df%>%filter(founded_year == i)
  tmp_df = count(tmp_df$market)%>%filter(x!="")%>%arrange(desc(freq))%>%
    head(ifelse(nrow(tmp_df)>=5,5,nrow(tmp_df)))
  tmp_df$years = rep(i, times = nrow(tmp_df))
  new_df = rbind(new_df, tmp_df)
}
new_df = new_df[-1,]
new_df$freq = as.integer(new_df$freq)
names(new_df)[1] = "Market"
names(new_df)[2] = "New_Companies_Introduced"

#dataset fot TAB 2
DATA = read.csv("investments_VC.csv", stringsAsFactors = FALSE)
DATA$total_funding = as.integer(DATA$funding_total_usd)
DATA = filter(DATA, country_code != "USA")
DATA = DATA %>%
  mutate(status = replace(status, status == "", "unknown"))

countrydropdown = DATA %>% filter(country!='' & country!='UNITED STATES OF AMERICA') %>% group_by(country)%>% 
  summarise(count(country)) %>% 
  arrange(desc(freq)) %>%head(15)

MDATA = filter(DATA, founded_year>1990) %>% 
  group_by(founded_year) %>%
  summarise(avgseed=mean(seed),avgventure=mean(venture), avgdebt=mean(debt_financing))

HDATA = DATA %>% filter(founded_year>1990)

#violin plot
t2top10_funding=DATA %>% filter(total_funding!="-" & market!="") %>%
  group_by(market)%>%
  summarise(total=sum(total_funding), avg=mean(total_funding)) %>%
  arrange(-total) %>% head(10)
t2top10 = inner_join(DATA, t2top10_funding,
                     by = c("market"))

#shape file
shape = readOGR("cb_2016_us_state_500k.shp")

#ui
ui = navbarPage("TABS ",
                tabPanel("USA",
                         fluidPage(theme = shinytheme("cerulean"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("stateInput", "State",
                                                   choice = c("Select State",sort(unique(df$state)))
                                       ),
                                       fluidRow(
                                         textOutput("textInput")
                                       ),
                                       width = 2),
                                     mainPanel(
                                       fluidRow(
                                         leafletOutput(outputId = "mymap")
                                       ),
                                       fluidRow(
                                         splitLayout(cellWidths = c("50%", "50%"),fluidRow(plotlyOutput(outputId = "boxplot")),
                                                     fluidRow(plotlyOutput(outputId = "linegraph")))
                                       )
                                     )
                                   )
                         )
                ),
                tabPanel("GLOBAL",
                         fluidPage(theme = shinytheme("cerulean"),
                                   sidebarLayout(
                                     sidebarPanel(
                                       selectInput("countryInput", "County",
                                                   choice = c("Select Country",sort(unique(countrydropdown$country)))
                                       ),
                                       fluidRow(
                                         textOutput("textInput1")
                                       ),
                                       width = 2),
                                     mainPanel(
                                       fluidRow(
                                         plotlyOutput(outputId = "histoplot")
                                       ),
                                       fluidRow(
                                         splitLayout(cellWidths = c("50%", "50%"),fluidRow(plotlyOutput(outputId = "mixplot")),
                                                     fluidRow(plotlyOutput(outputId = "violinplot")))
                                       )
                                     )
                                   )
                         )
                )
)

# server
server <- shinyServer(function(input, output){
  #TAB 1
  #mymap    
  output$mymap <- renderLeaflet({
    leaflet()%>%addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
      addCircleMarkers(lat = df$latitude, lng = df$longitude,popup = df$popup_info,
                       label = df$name, clusterOptions = markerClusterOptions())
    
  })
  #filtering
  observe({
    filtered  = req({
      df%>%filter(state == input$stateInput)
    })
    clearShapes(leafletProxy("mymap"))
    #mymap
    if(input$stateInput == "Select State")
      leafletProxy('mymap') %>%
      setView(lng = -98.48425, lat = 39.01190, zoom = 3.5)%>%
      addPolygons(data = shape, color = "black", weight = 0.3, smoothFactor = 0.2)
    else
      leafletProxy('mymap') %>%
      setView(lng = filtered$longitude[1], lat = filtered$latitude[1], zoom = 5.5)%>%
      addPolygons(data = shape[shape$NAME == input$stateInput, ],
                  color = "black", weight = 0.3, smoothFactor = 0.2)
    
    #linegraph filtering        
    new_df1 = data.frame("x" = '', "freq" = '', "years" = '')
    for (i in 2000:2014){
      tmp_df1 = filtered%>%filter(founded_year == i)
      tmp_df1 = count(tmp_df1$market)%>%filter(x!="")%>%arrange(desc(freq))%>%
        head(ifelse(nrow(tmp_df1)>=5,5,nrow(tmp_df1)))
      tmp_df1$years = rep(i, times = nrow(tmp_df1))
      new_df1 = rbind(new_df1, tmp_df1)
    }
    
    new_df1 = new_df1[-1,]
    new_df1$freq = as.integer(new_df1$freq)
    names(new_df1)[1] = "Market"
    names(new_df1)[2] = "New_Companies_Introduced"
    
    #linegraph ploting
    if(input$stateInput == "Select State")
      output$linegraph = renderPlotly({
        a = ggplot(data = new_df, aes(x = years, y = New_Companies_Introduced, group = Market, colour = Market)) + 
          geom_line() +
          geom_point() +
          labs(x = "years", y = "No. of new companies introduced",title = paste("Top Start-ups in USA","\n2000 - 2014"))+
          theme_minimal()
        ggplotly(a, dynamicTicks = FALSE, tooltip = c("x", "y", "group")) %>%
          rangeslider() %>%
          layout(hovermode = "years")
      })
    else
      output$linegraph = renderPlotly({
        a = ggplot(data = new_df1, aes(x = years, y = New_Companies_Introduced, group = Market, colour = Market)) + 
          geom_line() +
          geom_point() +
          labs(x = "years", y = "No. of new companies introduced",
               title = paste("Top Start-ups in" , input$stateInput, "\n",min(new_df1$years), "-", max(new_df1$years)))+
          theme_minimal()
        ggplotly(a, dynamicTicks = FALSE, tooltip = c("x", "y", "group")) %>%
          rangeslider() %>%
          layout(hovermode = "years")
      })
    
    #boxplot
    if(input$stateInput == "Select State")
      output$boxplot = renderPlotly({
        top10_funding=df %>% filter(funding_total_usd!="-") %>%
          group_by(market)%>%
          summarise(total=sum(funding_total_usd), avg=mean(funding_total_usd)) %>%
          arrange(-total) %>% head(10)
        
        top10 = inner_join(df, top10_funding,
                           by = c("market"))
        
        ggplotly(ggplot(data=top10,aes(x=market, y=log10(funding_total_usd), fill=market)) +
                   geom_boxplot()+
                   theme(legend.position="none") +
                   ggtitle("Top 10 highest funded markets in USA") + xlab("Market") + ylab("funding")+
                   theme(axis.text.x=element_text(angle=35,vjust = 0.5)))
      })
    else
      output$boxplot = renderPlotly({
        top10_funding=filtered %>% filter(funding_total_usd!="-") %>%
          group_by(market)%>%
          summarise(total=sum(funding_total_usd), avg=mean(funding_total_usd)) %>%
          arrange(-total) %>% head(10)
        
        top10 = inner_join(filtered, top10_funding,
                           by = c("market"))
        
        ggplotly(ggplot(data=top10,aes(x=market, y=log10(funding_total_usd), fill=market)) +
                   geom_boxplot()+
                   theme(legend.position="none") +
                   ggtitle(paste("Top 10 highest funded markets in", input$stateInput)) + xlab("Market") + ylab("funding")+
                   theme(axis.text.x=element_text(angle=35,vjust = 0.5)))
      })
    #text
    if(input$stateInput == "Select State")
      output$textInput <- renderText({
        paste("Total No. of data in USA is", nrow(df))
      })
    else
      output$textInput <- renderText({
        paste("Total No. of data in ", input$stateInput, "is" , nrow(filtered))
      })
  })
  #TAB 2
  observe({
    tab2filtered  = req({
      DATA%>%filter(country == input$countryInput & founded_year>1990)
    })
    
    #mixplot
    MDATA1 = tab2filtered %>%  group_by(founded_year) %>%
      summarise(avgseed=mean(seed),avgventure=mean(venture), avgdebt=mean(debt_financing))
    
    if(input$countryInput == "Select Country")
      output$mixplot <- renderPlotly({
        ggplotly(ggplot(MDATA, aes(x = founded_year,))+
                   theme_minimal()+
                   geom_col(aes(y = avgventure), size = 1, fill = "light blue")+
                   geom_line(aes(y = avgseed), size = 1, color = "red")+
                   geom_line(aes(y = avgdebt), size = 1, color="yellow")+
                   geom_point(aes(y = avgdebt), size = 1, color="orange")+
                   labs(title = "Top 10 Market Leaders",x = "Years",y = "funding")+
                   scale_y_continuous(labels = scales::comma))
      })
    else
      output$mixplot <- renderPlotly({
        ggplotly(ggplot(MDATA1,aes(x = founded_year,))+
                   theme_minimal()+
                   geom_col(aes(y = avgventure), size = 1, fill = "light blue")+
                   geom_line(aes(y = avgseed), size = 1, color = "red")+
                   geom_line(aes(y = avgdebt), size = 1, color="yellow")+
                   geom_point(aes(y = avgdebt), size = 1, color="orange")+
                   labs(title = "Top 10 Market Leaders",x = "Years",y = "funding")+
                   scale_y_continuous(labels = scales::comma))
      })
    #histogramplot
    if(input$countryInput == "Select Country")
      output$histoplot = renderPlotly({
        ggplotly(ggplot(data=HDATA, aes(x = founded_year, fill=status))+
                   geom_histogram(binwidth=1) + facet_wrap( ~ status))
      })
    else
      output$histoplot = renderPlotly({
        ggplotly(ggplot(data=tab2filtered, aes(x = founded_year, fill=status))+
                   geom_histogram(binwidth=1) + facet_wrap( ~ status))
      })
    
    #voilinplot
    t2top10_funding1=tab2filtered %>% filter(total_funding!="-" & market!="") %>%
      group_by(market)%>%
      summarise(total=sum(total_funding), avg=mean(total_funding)) %>%
      arrange(-total) %>% head(10)
    t2top101 = inner_join(tab2filtered, t2top10_funding1,
                          by = c("market"))
    
    if(input$countryInput == "Select Country")
      output$violinplot = renderPlotly({
        ggplotly(ggplot(data=t2top10,aes(x=market, y=log10(total_funding), fill=market)) +
                   geom_violin()+
                   theme(legend.position="none") +
                   ggtitle("Top 10 highest funded markets") + xlab("Market") + ylab("funding")+
                   theme(axis.text.x=element_text(angle=35,vjust = 0.5)))
      })
    else
      output$violinplot = renderPlotly({
        ggplotly(ggplot(data=t2top101,aes(x=market, y=log10(total_funding), fill=market)) +
                   geom_violin()+
                   theme(legend.position="none") +
                   ggtitle("Top 10 highest funded markets") + xlab("Market") + ylab("funding")+
                   theme(axis.text.x=element_text(angle=35,vjust = 0.5)))
      })
    
    #Text 1
    if(input$countryInput == "Select Country")
      output$textInput1 <- renderText({
        paste("Total No. of data except USA is", nrow(HDATA))
      })
    else
      output$textInput1 <- renderText({
        paste("Total No. of data in ", input$countryInput, "is" , nrow(tab2filtered))
      })
  })
})
# Run the application 
shinyApp(ui = ui, server = server)

