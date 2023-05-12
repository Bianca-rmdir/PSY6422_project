#open libraries
library(here)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotly)
library(shiny)
library(shinydashboard)
library(fresh)
library(maps)

#open datasets
ds_pop_by_country <- read.csv("data/pop_by_country.csv")
ds_plastic_pol_2019 <- read.csv("data/plastic_pol.csv")
ds_annual_co2 <- read.csv("data/annual_co2.csv")
ds_milk_prod_t <- read.csv("data/milk_prod_t.csv")
ds_aviation_km <- read.csv("data/aviation_pc_km.csv")
ds_meat_prod_t <- read.csv("data/meat_prod_t.csv")

###cleaning the population dataset

#as this dataset contains population for multiple years, the following code filters only year 2019
ds_pop_by_country_2019 <- subset(ds_pop_by_country, 
                                 Year == 2019)

#change variable name
colnames(ds_pop_by_country_2019)[1] <- "country"

#remove unnecessary columns
ds_pop_by_country_2019 <- ds_pop_by_country_2019[, c("country", 
                                                     "Population")]


###cleaning the plastic pollution dataset

#the values are given per capita, so they need to be multipled by 1000 to be standardized

ds_plastic_pol_2019$plastic_pol_per1000 <- ds_plastic_pol_2019$Mismanaged.plastic.waste.to.ocean.per.capita..kg.per.year. *1000

#change variable name
colnames(ds_plastic_pol_2019)[1] <- "country"

#remove unnecessary columns
ds_plastic_pol_2019 <- ds_plastic_pol_2019[, c("country", 
                                               "plastic_pol_per1000")]

###cleaning the co2 dataset

#the annual co2 data set contains separate rows for different years, only 2019 is retained 

ds_annual_co2_2019 <- subset(ds_annual_co2, 
                             Year == 2019)

#change variables names
colnames(ds_annual_co2_2019)[1] <- "country"
colnames(ds_annual_co2_2019)[4] <- "co2"

#remove unnecessary columns
ds_annual_co2_2019 <- ds_annual_co2_2019[, c("country", 
                                             "co2")]

###cleaning the milk production dataset

#as this dataset contains population for multiple years, the following code filters only year 2019
ds_milk_prod_t_2019 <- subset(ds_milk_prod_t, Year == 2019)

#change variable name
colnames(ds_milk_prod_t_2019)[1] <- "country"
colnames(ds_milk_prod_t_2019)[4] <- "milk"

#remove unnecessary columns
ds_milk_prod_t_2019 <- ds_milk_prod_t_2019[, c("country", 
                                               "milk")]

###cleaning the aviation dataset

#change variable name
colnames(ds_aviation_km)[1] <- "country"
colnames(ds_aviation_km)[4] <- "airtravel_percap"

##the values are given per capita, so they need to be multipled by 1000 to be standardized

ds_aviation_km$airtravel_per1000 <- ds_aviation_km$airtravel_percap *1000

#remove unnecessary columns
ds_aviation_km <- ds_aviation_km[, c("country", 
                                     "airtravel_per1000")]


###cleaning the meat production dataset
#change variable name
colnames(ds_meat_prod_t)[1] <- "country"
colnames(ds_meat_prod_t)[4] <- "meat"

#as this dataset contains population for multiple years, the following code filters only year 2019
ds_meat_prod_t_2019 <- subset(ds_meat_prod_t, 
                              Year == 2019)

#remove unnecessary columns
ds_meat_prod_t_2019 <- ds_meat_prod_t_2019[, c("country", 
                                               "meat")]

#merge the cleaned datasets
combined <- list(ds_annual_co2_2019, ds_pop_by_country_2019, ds_aviation_km, ds_meat_prod_t_2019, ds_milk_prod_t_2019, ds_plastic_pol_2019)
df_full <- reduce(combined, merge, by = "country", all = TRUE)

#remove rows where data on co2 or on population is missing
df_full2 <- df_full[!is.na(df_full$co2), ]
df_full3 <- df_full2[!is.na(df_full2$Population), ]

#make function to calculate the rest of the variables per 1000 people
calculate_value_per1000 <- function(df_full4, Population) {
  variables_per_1000 <- df_full4 
  for (col in c(2,5,6)) {
    variables_per_1000[[col]] <- variables_per_1000 [[col]]/variables_per_1000[[Population]] * 1000
  }
  return(variables_per_1000)
}

#save final clean dataset
df_final <- calculate_value_per1000(df_full3, "Population")

#change variable names for consistency so they all specify that they are calculated per 1000

names(df_final) <- setNames(c("country", 
                              "co2_per1000", 
                              "population", 
                              "airtravel_per1000", 
                              "meat_per1000", 
                              "milk_per1000", 
                              "plastic_pol_per1000"), 
                            names(df_final))

coordinates <- map_data("world")

#rename variable in coordinates to match dataset
colnames(coordinates)[5] <- "country"

#rename country names so they match up
df_final$country[df_final$country == "United States"] <- "USA"
df_final$country[df_final$country == "United Kingdom"] <- "UK"
df_final$country[df_final$country == "Czechia"] <- "Czech Republic"
df_final$country[df_final$country == "Democratic Republic of Congo"] <- "Democratic Republic of the Congo"
df_final$country[df_final$country == "Congo"] <- "Republic of Congo"
df_final$country[df_final$country == "Cote d'Ivoire"] <- "Ivory Coast"
df_final$country[df_final$country == "Timor"] <- "Timor-Leste"
df_final$country[df_final$country == "Eswatini"] <- "Swaziland"




#combine coordinates dataset with my dataset
combinedcoord <- left_join(coordinates, df_final, 
                           by = "country")

server <- function(input, output) {
  output$firsttab <- renderText({
    "<div style='font-size: 24px; width: 85%; margin:0 auto; display: inline-block; '> <h1> Introduction 
    </h1> <p>The current project investigates three of the leading causes of CO2 emissions and their relationships: air travel, dairy and meat production, and plastic waste. Although most of the CO2 emissions are generated by the fossil fuels used in industry, the three behaviours where individual action can be taken have been selected for the purpose of this project. </p>

<p>Plastics have been found to emit 3.4% of global greenhouse gas emissions in 2019 and the emissions are projected to double by 2060 (www.oecd.org).</p>

<p>Aviation has been found to emit around 2.5% of CO2 emissions in 2022. </p>

<p>Meat and dairy account for around 14% of global CO2 emissions. </p>

<br> 

The variables of interest have been filtered, modified and combined into a new dataset. This will be detailed in the Data Preparation section. 

<h1> Data Origins </h1>

Six datasets have been retrieved from Our World in Data (https://ourworldindata.org/): 
<ul>
  <li>Annual CO2 emissions measured in tonnes. Published by Global Carbon Project</li>
  <li>Population by country. Published by United Nations, Department of Economic and Social Affairs, Population Division (2022)</li>
  <li>Air travel km per capita. Published by the International Council on Clean Transportation (ICCT)</li>
  <li>Meat production. Published by Food and Agriculture Organization of the United Nations</li>
  <li>Milk production. Published by Food and Agriculture Organization of the United Nations</li>
  <li>Plastic pollution. Published by Meijer et al. (2021). More than 1000 rivers account for 80% of global riverine plastic emissions into the ocean. Science Advances.</li>
  
  <br>


<h1> Codebook </h1>
    </div>" 
  }
  )
  
  
  output$codebooktable <- renderTable({
    data.frame(
      Variable_name = c("co2_per1000", "population", "airtravel", "meat_per1000", "milk_per1000", "plastic_pol_per1000"),
      Description = c("Annual CO2 emissions measured in tonnes, for 1000 people",
                      "The population of a country",
                      "The number of kilometers travelled by 1000 paying passengers, includes both domestic and international travel. International flights are considered for the country of departure.",
                      "Total production of meat in a country measured in tonnes per 1000 people",
                      "Total production of milk in a country measured in tonnes per 1000 people",
                      "Plastic waste discarded into the ocean per 1000 people measured in kilograms per year")
    ) 
  })
  
  output$secondtab <- renderText({"<div style='font-size: 24px; width: 85%; margin:0 auto; display: inline-block; '> 
The aims of the project are:
  <ol>
  <br>
  <li>To present global levels of CO2 emissions and of three of the leading causes of CO2 emissions: air travel, dairy and meat production, and plastic waste thrown into the ocean. This will be shown by creating world maps.</li>
  <br>
  <li>To investigate the impact of air travel, dairy and meat production, and plastic waste thrown into the ocean on CO2 emissions globally and in each country. This will be shown by creating scatterplots. </li> 
  <br>
  <li>To investigate the relationships between the three leadings causes of CO2 emissions. This will be shown by creating scatterplots.</li> 
  
      </ol>
      </div>"
    
  }
  )
  #create map for co2
  map_co2 <- ggplot(combinedcoord) + 
    geom_polygon(aes(long,lat, 
                     group=group, 
                     fill=co2_per1000,
                     text = paste("Country: ", 
                                  country, "<br>Co2 Emissions: ", co2_per1000)),
                 color = "black",
                 size = 0.1)+
    scale_fill_gradient(low = "white", high = "red") +
    theme_void() + 
    labs (title = "Annual CO2 emissions measured in tonnes, for 1000 people") +
    coord_equal()
  
  #convert map to plotly object
  co2_plotly <- ggplotly(map_co2, tooltip = "text")
  
  #create map for airtravel
  map_airtravel <- ggplot(combinedcoord) + 
    geom_polygon(aes(long,lat, 
                     group=group, 
                     fill=airtravel_per1000, 
                     text = paste("Country: ", 
                                  country, "<br> Air Travel: ",
                                  airtravel_per1000)),
                 color = "black",
                 size = 0.1)+
    scale_fill_gradient(low = "white", high = "red") +
    theme_void() + 
    labs(title = "The number of kilometers travelled by 1000 paying passengers, includes both domestic and international travel. International flights are considered for the country of departure") +
    coord_equal()
  
  #convert map to plotly objec
  airtravel_plotly <- ggplotly(map_airtravel, tooltip = "text")
  
  
  #create map for meat productio
  map_meatprod <- ggplot(combinedcoord) + 
    geom_polygon(aes(long,lat, 
                     group=group, 
                     fill=meat_per1000,
                     text = paste("Country: ", 
                                  country, "<br> Meat Production: ",
                                  meat_per1000)),
                 color = "black",
                 size = 0.1)+
    scale_fill_gradient(low = "white", high = "red") +
    theme_void() + 
    labs (title = "Total production of meat in a country measured in tonnes per 1000 people") +
    coord_equal()
  
  #convert map to plotly object
  meatprod_plotly <- ggplotly(map_meatprod, tooltip = "text")
  
  
  #create map for milk production
  map_milkprod <- ggplot(combinedcoord) + 
    geom_polygon(aes(long,lat, 
                     group=group, 
                     fill=milk_per1000,
                     text = paste("Country: ", 
                                  country, "<br> Milk Production: ",
                                  milk_per1000)),
                 color = "black",
                 size = 0.1)+
    scale_fill_gradient(low = "white", high = "red") +
    theme_void() + 
    labs (title = "Total production of milk in a country measured in tonnes per 1000 people") +
    coord_equal()
  
  
  #convert map to plotly object
  milkprod_plotly <- ggplotly(map_milkprod, tooltip = "text")
  
  
  #create map for plastic waste
  map_plasticpol <- ggplot(combinedcoord) + 
    geom_polygon(aes(long,lat, 
                     group=group, 
                     fill=plastic_pol_per1000,
                     text = paste("Country: ", 
                                  country, "<br> Plastic Pollution: ",
                                  plastic_pol_per1000)),
                 color = "black",
                 size = 0.1)+
    scale_fill_gradient(low = "white", high = "red") +
    theme_void() + 
    labs (title = "Plastic waste discarded into the ocean per 1000 people measured in kilograms per year") +
    coord_equal()
  
  #convert map to plotly object
  plasticpol_plotly <- ggplotly(map_plasticpol, tooltip = "text")
  
  #tell it which map to open based on button selected
  
  output$output_map <- renderPlotly({
    print(input$map)
    if (input$map == "CO2 Emissions") {
      co2_plotly
    } else if (input$map == "Air Travel") {
      airtravel_plotly
    } else if (input$map == "Meat Production") {
      meatprod_plotly
    } else if (input$map == "Milk Production") {
      milkprod_plotly
    } else if (input$map == "Plastic Discarded") {
      plasticpol_plotly
    }
  })
  #create reactive data frame
  df_reactive <- reactive ({combinedcoord
  })
  
  #add scatterplots to the same tab
  
  #define scatterplot
  output$scatterplot <- renderPlotly({
    scat_data <- df_reactive()
    scatterplot <- plot_ly(data = scat_data, 
                           x = ~get(input$x_var), 
                           y = ~get(input$y_var), 
                           type = "scatter",
                           mode = "markers",
                           marker = list(color = "orange"),
                           text = ~country)
    scatterplot
    #update x-axis and y-axis labels
    scatterplot <- layout(scatterplot, 
                          xaxis = list(title = input$x_var),
                          yaxis = list(title = input$y_var))
    
    scatterplot <- scatterplot %>% 
      add_markers(hoverinfo = "text")
  })
  
  
  output$summary <- renderText({"<div style='font-size: 24px; width: 85%; margin:0 auto; display: inline-block; '> 
  The current project aimed to:  
    <ol>
  <br>
  <li>Present global levels of CO2 emissions and of three of the leading causes of CO2 emissions: air travel, dairy and meat production, and plastic waste thrown into the ocean. </li>
  Global levels were displayed by creating world maps of the selected behaviour per 1000 people, in 2019. The air travel variable was not available for 2019, so 2018 was used. The maps showed that CO2 emissions were highest in North America, and lowest in Africa, and this seems to also translate to air travel and meat production. The plastic waste discarded into the ocean had many missing values due to many countries being landlocked, with highest levels found in the Philippines, Malaysia and Suriname. 
  Investigating these unsustainable behaviours per 1000 people gives a better picture of the general sustainability of the population of a country since it would be expected, for example, for a country with one billion people to have more plastic waste than a country with 20 million people.
  <br>
  <li>Investigate the impact of air travel, dairy and meat production, and plastic waste thrown into the ocean on CO2 emissions globally and in each country. This will be shown by creating scatterplots. </li> 
  The scatterplots revealed no clear relationship between CO2 and air travel, plastic waste and milk production. However, CO2 seems to increase for countries with higher meat production, but this trend is not clear and requires further statistical testing.
  <br>
  <li>Investigate the relationships between the three leadings causes of CO2 emissions.</li> 
  Scatterplots were created between all the variables and the behaviours do not seem to be related. 
      </ol>
      <h1> Further directions 
    </h1>
<p> If data was available, I would have liked to also look at changes in the chosen variables over the last few years, in light of the recent pandemic. To show the impact of each country better, a bar chart could have been created as it would be easier to visualise the top contributors to unsustainable behaviours. The project could also be extended by adding more unsustainable behaviours and looking at individual impact and relationship between them. </p>
  <h1> Resources used:
    </h1>
  <ul>
<li><a href='https://www.geeksforgeeks.org/how-to-make-world-map-with-ggplot2-in-r/' >GeeoksforGeeks</a></li>
<li><a href='https://rstudio.github.io/shinydashboard/structure.html'</a>shinydashboard</li>
<li><a href='https://epirhandbook.com/en/dashboards-with-shiny.html'</a>dashboardswithshiny</li>
<li><a href='https://www.youtube.com/watch?v=Zm4nvNF824s&ab_channel=TechKnowHow'</a>worldmaps</li>
  </ul>"
    
  })
  
}

theme <- create_theme(adminlte_color(
  light_blue = "#4898a8"
))

#make ui and set names for each tab
ui <- dashboardPage(
  header = dashboardHeader(title = "PSY6422 Project"),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "tab1"),
      menuItem("Research Questions", tabName = "tab2"),
      menuItem("Visualisations", tabName = "tab3"),
      menuItem("Summary", tabName = "tab4")
    )
  ),
  body = dashboardBody(
    tabItems(
      #First tab content
      tabItem(tabName = "tab1",
              h2("Introduction, Data Origins and Codebook"),
              fluidRow(style = "padding-left: 20px;",
                       htmlOutput("firsttab"),
                       htmlOutput("codebooktable")
              ),
      ),
      
      
      #Second tab content
      tabItem(tabName = "tab2", 
              h2("Research Questions"),
              fluidRow(style = "padding-left: 20px;",
                       htmlOutput("secondtab")
              )
      ),
      
      
      #Third tab content
      tabItem(tabName = "tab3",
              h2("World Maps",
                 selectInput("map", "Select a world map for:",
                             choices = c("CO2 Emissions",
                                         "Air Travel",
                                         "Meat Production",
                                         "Milk Production",
                                         "Plastic Discarded")),
                 plotlyOutput("output_map", height = "800px", width = "100%")),
              h2 ("Scatterplots"),
              fluidRow(
                column(width = 4, selectInput("x_var", "X-Axis Variable",
                                              choices = c("co2_per1000",
                                                          "airtravel_per1000",
                                                          "meat_per1000",
                                                          "milk_per1000",
                                                          "plastic_pol_per1000")
                )
                ),
                column(width = 4, selectInput("y_var", "Y-Axis Variable",
                                              choices = c("co2_per1000",
                                                          "airtravel_per1000",
                                                          "meat_per1000",
                                                          "milk_per1000",
                                                          "plastic_pol_per1000")
                )
                ),
                column(width = 12, style = "margin-top: 20px",
                       plotlyOutput("scatterplot", height = "800px", width = "100%")
                )
              )
              
              
      ),
      
      
      #Fourth tab content
      tabItem(tabName = "tab4", 
              h2("Summary"),
              fluidRow(style = "padding-left: 20px;",
                       htmlOutput("summary")
              )
      ),
      
      
      #Sixth tab content 
      tabItem(tabName = "tab6", 
              h2("Full Code"))
    ),
    use_theme(theme)
  )
)

shinyApp(ui, server)