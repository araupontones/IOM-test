
library(shiny)
library(shinycssloaders)
library(markdown)
library(leaflet)

color_text= '#0033A0'
sub_color ='#FFB81C'
color2 = '#CB641D'

#start ui

ui <- navbarPage("R/stats test: Andrés Arau", id = "nav",
           
  
           tabPanel("Introduction",
                    includeMarkdown("intro.Rmd")
                    ),
                 
           tabPanel("Data manipulation & Descriptive stats", heigth="100%", id="Tr",
                    
                    tags$head(
                      tags$style(
                        HTML(
                          '                          
               body{
               
                padding:40px;
                
               }
                               
                      
                      .container-fluid{
                         background-color: white !important;
                         border-style: none;
                      }
                      
                      .navbar-default {
    background-color: #f8f8f8;
    border-style: none;
}
                    .navbar-default .navbar-nav{
                        color:',color_text,';
                        background-color: white;
                        font-size: large;
                    }
                    
                     .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover{
                        color:', color_text,';
                        background-color: white;
                        
                        font-weight: bold;
                        border-bottom: 2px solid',sub_color,';
                    }
                          
                          
                          .navbar-brand{
                            color:',color_text,'!important;
                             font-weight: bold;
                             background-color:white;
                             font-size: xx-large;
                          
                          }
                          
                          /*text*/
                          
                          .textblue{
                          
                          color:',color_text,' !important;
                          }
                          
                         
                          
                          /*edit table*/
                          
                          
                         #summary_table thead{
        
         background-color:',color_text,'!important;
         color :white;
          font-size:12px;
        
      }
      
     #summary_table td{
        font-size:12px;
        
      }
      
      /*edit map */
       #map {height: calc(100vh - 150px) !important;}
       
       /* Create three equal columns that floats next to each other */
       
.columna {
  float: left;
  width: 25%;
  padding: 10px;
  height: 100px; /* Should be removed. Only for demonstration */
  text-align: center;
  background-color: white;
  color:',color_text,'!important;
  
}


.text2{
color:',color2,' !important;
font-size: 24px;

}


/* Clear floats after the columns */

.fila:after {
  content: "";
  display: table;
  clear: both;
}

/* title of the map*/

#title_map{

text-align:center;
 color:',color_text,'!important;
}

#title_gpa{

text-align:center;
 color:',color_text,'!important;
}

.control-label {
color:',color_text,'!important;

}  
                          
                
                        '
                        )
                      )
                    ),
                    
                    fluidRow(column(6, 
                                    h4("1. Summary table", id ="table_title", class= "textblue"),
                                    DT::DTOutput("summary_table", width = "80%")
                                    ),
                             column(6, 
                                    h4("2.Differences in average life satisfaction by faculty and relationship status",
                                       class= "textblue"),
                                    wellPanel(plotOutput("chart_lifesat")))
                             ),
                    fluidRow(column(6, 
                                    h4("3. Differences in the average cost of the career by faculty and job status",  class= "textblue"),
                                    wellPanel(plotOutput("chart_cost"))),
                             column(6,
                                    h4("4. Relationship between life satisfaction and age",  class= "textblue"),
                                    wellPanel(plotOutput("chart_age"))))
           ),
                    #withSpinner(leafletOutput("map2")),
                    
                    
           
##Modelling ----- ----------------------------------------------------------------------------------------
           tabPanel("Modelling", heigth="100%",
                    #includeMarkdown("test.Rmd")
                    h4("1.	Check whether the relationship status has an effect on life satisfaction regardless of the number of terms, age, sex and the expected entry salary after university. If possible, visualize effects.", class= "textblue"),
                    br(),
                    h5("The relationship status of a student does not have a statistically significant effect at 5%. This can be seen on the coefficient of the variable (and confidence interval) ranging over 0"),
                    plotOutput("coefficients_chart", width = "50%"),
                    br(),
                    hr(),
                    h4("2.	Test whether having a job has a negative effect on the average grade point average over the last 10 years. If possible, visualize effects.", class= "textblue"),
                    br(),
                    h5("having a job does not have a statistically effect on the average gpa over the last 10 years, holding sex and age constant"),
                    plotOutput("chart_job", width = "50%"),
                    hr(),
                    h5("3.	Forecast the grade point average for next two year (i.e. 2021 and 2022).", class = "textblue"),
                    br(),
                    plotOutput("chart_prediction", width = "50%")
                    
                    ),
          
##Visualization and user experience --------------------------------------------------------------------------
           tabPanel("User experience and presentation skills",heigth="100%",
                   
                    ##Column with selections
                    column(2,
                    ##Options to select
                    radioButtons("faculties",
                                 label = h3("Select a faculty"),
                                 choices = list("All faculties"=1,
                                                "Business" =2,
                                                "Economics" = 3,
                                                "Political Science" = 4,
                                                "Sociology" =5),
                                 selected = 1),
                    ##reactive selection of courses based on selection of faculties
                    htmlOutput("input_courses")
                    ),
                    
                    ##Column with visualizaitons 
                    column(10, 
                           ##sumary of age, life sat, liked, women 
                           htmlOutput("boxes"),
                           
                           hr(),
                           fluidRow(
                             ##Map by selected group
                             
                             column(6, 
                                    h2("Student's country of birth", id = "title_map"),
                                    leafletOutput("Map_vis")
                                    ),
                             column(6, h2("Average gpa by year and sex", id = "title_gpa"),
                                    plotOutput("gpa_chart")
                                    )
                                    )
                           
                           
                           
                           )
                    ),
                    
                    
                   
           
           
           ##Map ----------------------------------------------------------------------------------------------------
           tabPanel("Map",heigth="100%",
                    
                    h4("Number of students by country of birth", class = "textblue"),
                    leafletOutput("map")
                    )
           
           
                    
                 
  
)
