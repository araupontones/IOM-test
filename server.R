#server 


#setwd("C:/Users/andre/Dropbox/Andres/03.Dashboards/11.MozambiqueRisk/app")



                               
                               

##start server ----------------------------------------------------------------------------------
server <- function(input, output, session) {
 
  ##read and clean data
  source("clean.R")
  

###### Data manipulation & Descriptive stats --------------------------------------------------------------------------------------
  ##Summary table
  output$summary_table <- DT::renderDT({
    
    DT::datatable(summary_table,
                  options = list( pageLength =4,
                                  lengthChange = FALSE,
                                 columnDefs = list( list(width="10%", targets=c(0:7)), #set col width
                                                   list(className="dt-center", targets=c(1:7)) #center columns
                                 ),
                                 
                                 ##hide search, and pagination
                                 dom = 'ltipr', 
                                 autoWidth = T,
                                 bInfo=F,
                                 bPaginate=F
                                 
                  ),
                  ##hide rownames and set colnames
                  rownames = F,
                  colnames = c('Faculty','% of non-Germans', 'Average life satisfaction score', '% in a relationship', 
                               '% Women', '% of age over 30', 'Average gpa (2010)', 'Average number of terms'))

    
    
  })
  
  ##life satistaction output
  
  output$chart_lifesat <- renderPlot({
    
    chart_lifesat
  })
  
  ##cost vs job status
  
  output$chart_cost <- renderPlot({
    
    chart_cost
  })
 
  ##chart age 
  
  output$chart_age <- renderPlot({
    
    chart_age
  })
  

##### Viusalization and user experience  -----------------------------------------------------------------------------------
 
  
  faculties = c("All faculties", levels(clean_students$faculty))
  ##reactive values 
  react = reactiveValues(
    
    data_app = clean_students,
    courses = c("All courses",sort(levels(clean_students$course)))
    
    
  )
  
  ###reactivity for filters
  observeEvent(input$faculties,{
    
    ##data
    select_faculty = faculties[as.numeric(input$faculties)]
    print(select_faculty)
   
    
    if(input$faculties== 1){
      
      react$courses <-  c("All courses",sort(levels(clean_students$course)))
    } else {
    
    
      c = as.character(unique(clean_students$course[clean_students$faculty==select_faculty]))
     
      react$courses = c("All courses",c)
    }
    
    
    ##reactivity to create data 
    
    observeEvent(input$input_course,{
      
      ##get selected faculty
      select_faculty = faculties[as.numeric(input$faculties)]
      print(select_faculty)
      print(input$input_course)
      
      
      ##If All courses
     if(input$input_course=="All courses"){
       ##and all faculties
        if(input$faculties == 1){
          print("HOLA")
          react$data_app <- clean_students ##complete data set
          
        } else {
          ##filter by faculty
          react$data_app <- clean_students %>%
            filter(faculty == select_faculty)
          
          
        }
       
       ##if a course is selected 
     } 
      
     ##if course is selected
       else {
         
         react$data_app <- clean_students %>%
           filter(course == input$input_course) ##only data of the course
       }
       
       
     
      
    })
    
    
    
    
  })
  
  
  
  
 
  
  ##selection of courses 
  output$input_courses <- renderUI({
    
    selectInput(label = h3("Select course"),
                inputId = "input_course",
                choices = react$courses
                )
  })
  
  
  output$boxes <- renderUI({
    
    
    age = round(mean(react$data_app$age),digits = 1)
    lifesat = paste(round(mean(react$data_app$lifesat),digits = 1), "/ 100")
    like = paste(round(mean(react$data_app$like),digits = 1), "/ 5")
    women = paste0(round(mean(react$data_app$sex=='Female'), digits = 3)*100,"%")
    
   HTML(paste0('

<div class="fila">
  <div class="columna">
    <h2>Age</h2>
    <p class= "text2">',age,'</p>
  </div>
  
  <div class="columna">
    <h2>Life satisfaction</h2>
    <p class= "text2">',lifesat,'</p>
  </div>
  
  <div class="columna">
    <h2>Liked</h2>
    <p class= "text2">',like,'</p>
  </div>
  
  <div class="columna">
    <h2>Women</h2>
    <p class= "text2">',women,'</p>
  </div>
  
</div>'
               ))
    
    
  })
  
  output$Map_vis <- renderLeaflet({
    
    
    data_cob2 = react$data_app %>%
      group_by(cob) %>%
      summarise(Students = n()) %>%
      full_join(clean_countries)
    
    labels_cob2 <- paste0(
      '<img src = "', data_cob2$flag,'"style="
  margin-left: auto;
  margin-right: auto;
  width: 100%;">',
      '<h4 style="color:',color_text,';
      font-weight: bold;
  margin-top:1px;">', data_cob2$Students, '</h4>'
    )%>% 
      lapply(htmltools::HTML)
    
    
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addMarkers(data = University_berlin,
                 lng = ~lon,
                 lat = ~ lat,
                 label = labelU,
                 labelOptions = labelOptions(noHide = TRUE, offset=c(100,30), textOnly = TRUE),
                 icon = logo) %>%
      addCircleMarkers(data = data_cob2,
                       lng = ~centroid.lon,
                       lat = ~centroid.lat,
                       color = color_text,
                       fillOpacity = .2,
                       radius = ~4*Students^.5,
                       label = labels_cob2,
                       labelOptions = labelOptions(noHide = TRUE, offset=c(-1,0), textOnly = TRUE)) %>%
      
      
      addLegendCustom(colors = color_text,
                      sizes = c(10,20,40),
                      labels = c("","",""))
    
    
  })
  
  output$gpa_chart <- renderPlot({
    
    gpa_vars = names(react$data_app)[str_detect(names(react$data_app), "gpa")]
    
    t = react$data_app %>%
      select(gpa_vars, faculty, course, sex) %>%
      gather(key = "Year", value = gpa, - c(sex, faculty, course)) %>%
      mutate(Year = str_remove_all(Year, "gpa_"))
    
    
    t2 = t %>% 
      group_by(Year, sex) %>%
      summarise(gpa = mean(gpa)) %>%
      ungroup()
    
    ggplot(data = t2, 
           aes(x = as.numeric(Year),
               y = gpa,
               color = sex
           ) ) +
      geom_point(size = 3) +
      geom_line() +
      ylim(0,5) +
      ylab("Average gpa") +
      xlab("Year") +
      scale_color_manual(values = c(color1, color2)) +
      scale_x_continuous(breaks =  seq(2010, 2020, by = 2)) +
      labs(caption = "Data from Berlin University") +
      tema
    
    
    
    
    
    
  })
  
#### Modeling -----------------------------------------------------------------------------------------------------
  
  output$coefficients_chart <- renderPlot({
    
   
    chart_coefficients
    
    
  })
  
  
  output$chart_job <- renderPlot({
    
    
    chart_job
    
    
  })
  
  output$chart_prediction <- renderPlot({
    
    chart_prediction
  })

##### Map ------------------------------------------------------------------------------------------------------------------- 
  
  output$map <- renderLeaflet({
    
    map
  })
  
}


