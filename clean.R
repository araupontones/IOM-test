###Script to join, clean, and reshape the data

##libraries
library(readxl)  ## to read .xlsx files
library(tidyverse) ##to clean, manipulate, create indicators, and visualize
library(ggalt) ##for dumbbell chart
library(scales) ##to scale the labels of the axis
library(CoordinateCleaner) ##for countries' centroids
library(leaflet) ##for mapping
library(htmltools) ##for labels in the map
library(jtools) ##for visualizing coefficients of lm
library(lubridate) ## for manipulating dates
##1. set paths and input files --------------------------------------------------------------------

getwd()

students_excel = ("data_students.xlsx")
faculties_excel = ("data_faculty.xlsx")



##2. read files -----------------------------------------------------------------------------------


raw_students = read_xlsx(students_excel)
raw_faculties = read_xlsx(faculties_excel)
raw_countries = countryref



##2. clean students data base -----------------------------------------------------------------------------

clean_students = raw_students %>% 
  mutate_if(is.character, list(~na_if(., "NA"))) %>% ##drop rows missing all variables
  filter(apply(., MARGIN = 1, function(x) sum(is.na(x))) < ncol(.)) %>% ##format variables correctly :
  mutate_at(vars(matches("gpa")), as.double) %>%  ##all gpa scores to double
  mutate_at(c("age","lifesat", "term", "like"), as.integer) %>% ##age and lifesat to integer
  mutate(job = job=="yes") %>% ##job as logical
  mutate(course = str_to_title(course)) %>% ##Capitals to course for better vis in dashboard
  mutate_if(is.character, as.factor) ## all characters as factors and 'like' and 'term' as ordered factors
  
         
  


##2.1 clean faculties

  clean_faculties = raw_faculties %>%
    mutate(faculty = factor(faculty),
           costs = prettyNum(costs, big.mark = ","))
  
##2.3 clean countries 
  clean_countries = raw_countries %>%
    filter(type == "country") %>%
    group_by(name) %>%
    slice(1)%>%
    ungroup() %>%
    rename(cob = name) %>% ##clean name of the country for consistency with students data
    mutate(cob = if_else(cob=="United Kingdom", "UK", cob),
           iso2 = if_else(cob=="Austria", "AU", iso2), ##Clean iso2 to fetch the flags from the internet
           iso2 = if_else(cob=="Germany", "GM", iso2),
           iso2 = if_else(cob=="Spain", "SP", iso2),
           iso2 = if_else(cob=="UK", "UK", iso2),
           flag = paste0("https://www.cia.gov/library/publications/the-world-factbook/attachments/flags/",iso2,"-flag.gif")) %>%
    select(cob, iso2, centroid.lon, centroid.lat, flag) %>%
    filter(cob %in% unique(clean_students$cob))%>%
    mutate(cob = factor(cob))
  

##University of Berlin: data frame with lat and long (from google maps), and the Uni's logo
  
  icon = "https://w7.pngwing.com/pngs/7/6/png-transparent-computer-icons-university-academic-degree-bank-angle-people-logo.png"
  University_berlin = data.frame("lat" = 52.4537052,
                                 "lon" = 13.283312,
                                 'logo' = icon)
    
 
  


####DATA MANIPULATION & DESCRIPTIVE STATS ------------------------------------------------------------------
  
  
##1. summary table ------------------------------------------------------------------------------------------
summary_table = clean_students %>%
  group_by(faculty) %>%
  summarise(
    ##Percentage of non-German:
    non_german = paste0(round(mean(cob!="Germany"),digits = 3)*100,"%"),
    
    ##average life satisfaction:
    lifesat = round(mean(lifesat), digits = 1),
    
    ##percentage of students in a relationship:
    relationship = paste0(round(mean(relationship=='In a relationship'), digits = 3)*100,"%"),
    
    ##sex ratio:
    women = paste0(round(mean(sex=='Female'), digits = 3)*100,"%"),
    #men = paste0(round(mean(sex=='Male'), digits = 3)*100,"%"),
    
    ##percentage of students over 30:
    over30 = paste0(round(mean(age > 30), digits = 3)*100,"%"),
    
    ##average gpa in 2010
    
    gpa_2010 = round(mean(gpa_2010), digits = 1),
    
    ##average number of terms  !!! ask marti
    
    terms = round(mean(term), digits = 3)
    
    
    )



### 2. Differences in life satistfaction by faculty and relationship status ----------------------------


##prepare data for points 
data_points = clean_students %>%
  group_by(faculty, relationship) %>%
  summarise(lifesat = round(mean(lifesat), digits = 1)) %>% ##life satisfaction by sex and faculty
  ungroup %>%
  group_by(faculty) %>% ##sort faculties by life satisfaction for a better visualization (sorting by lifesat)
  mutate(mean_ls = round(mean(lifesat), digits = 1)) %>%
  arrange(mean_ls) %>%
  ungroup() %>%
  mutate(faculty = factor(faculty,
                          levels = unique(.$faculty))
  ) %>%
  select(-mean_ls)


##data for dumbbells (the lines that connect the points)
data_dumbbells = data_points %>%
  spread(key = "relationship", value=lifesat) %>% ##create a column for each relationship status
  rename(relationship = `In a relationship`) %>%
  mutate(diff = relationship - Single) ##to create a column in the chart showing the differences (if I have time)



##theme of the chart
gray = "#b2b2b2"
color_text = '#0033A0' #2F617C'
light_gray = '#ECECEC'
color1 = '#5890C1'
color2 = '#CB641D'
  
  tema = theme(
    ##legend
    legend.position = "top",
    legend.title = element_blank(),
    legend.key = element_rect(fill = "white"),
    legend.text = element_text(size = 12, color = color_text),
    ##background
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = light_gray),
    ##Axis
    axis.ticks = element_blank(),
    axis.text = element_text(hjust = 0, size = 12, face = "bold", colour = color_text),
    axis.title = element_text(size = 12, face = 'bold', colour = color_text),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
    ##Title
    plot.title = element_text(hjust = 0, size = 24, colour = color_text),
    ##Caption 
    plot.caption = element_text(color = color_text)
    
    
  )
  
 chart_lifesat = ggplot() +
    ##vertical line with average life satistaction
    geom_vline(xintercept = mean(data_points$lifesat),
               linetype = "dotted",
               size =1,
               color = gray) +
    ##connect points
    geom_dumbbell(data=data_dumbbells, aes(y=faculty, x=relationship, xend=Single),
                           size=1.5, color=gray, point.size.l=3, point.size.r=3) +
    
    ##points by relationship and faculty
    geom_point(data = data_points, 
           aes(x = lifesat,
               y = faculty,
               color = relationship
           ), size =4) +
    
    ##labels with values of lifesatisfactoin 
    geom_text(data = data_points, 
               aes(x = lifesat,
                   y = faculty,
                   color = relationship,
                   label = lifesat
               ),
              nudge_y = -.15,
              size = 3.5) +
    xlim(45, 55) +
    scale_color_manual(values = c(color1, color2))+
    labs(
         caption = "Data from Berlin University")+
    ylab("")+
    xlab("Average life satisfaction") +
    tema +
   theme(axis.text.x = element_blank())  ##delete text in x Axis
    
    
#chart_lifesat
  
###3.Differences in the average cost by faculty and job status ---------------------------------------------- 

##prepare data: group by faculty
 
 data_cost = clean_students %>%
   group_by(faculty) %>% ##proportion with a student job
   summarise(job_status = round(mean(job), digits = 1)) %>% ##combine with faculty data
   left_join(clean_faculties)
 
 chart_cost = ggplot(data = data_cost,
        aes(x=job_status,
            y = costs))+
   
   ##points by faculty
   geom_point(size = 4,
              color = color1) +
   ##labels
   geom_text(aes(label=faculty),
             hjust= 0,
             nudge_x = .02,
             color = color2) +
   scale_x_continuous(limits= c(0,1),
                      breaks = seq(0,1,.5),
                      labels = percent) +
   ##labs and titles
   ylab("Average cost of the 5 year programme")+
   xlab("Proportion of students with a job") +
   labs(caption = "Data from Berlin University") +
   tema 
 
 #chart_cost
 
### 4. Visual showing relationship between life satisfaction and age ------------------------------------------
 
 ##prepare data: group by age

  data_age = clean_students %>%
    group_by(age) %>%
    summarise(lifesat = mean(lifesat))
  
  chart_age = ggplot(data = data_age,
         aes(x = age,
             y = lifesat)
             )+
    geom_point(color = color2,
               size = 2) +
    geom_line(color = color2) +
    ylim(0,75) +
    ylab("Average life satisfaction score (out of 100)")+
    xlab("Age") +
    labs(caption = "Data from Berlin University") +
    tema
 
  

  
##### MMODELLIG -----------------------------------------------------------------------------------------------
  
##1 relationship status  effect on life satisfaction ----------------
  ##prepare data for model
  model_data = clean_students %>%
    left_join(clean_faculties) ##to get salaries
  
  
  ##run a linear model controling for term, age,sex, and salary
  linearMod <- lm(lifesat ~ relationship + term + age + sex + salary , data=model_data)
  
  ##visualize the results
  chart_coefficients= plot_summs(linearMod, linearMod, robust = list(FALSE, "HC0"),
             model.names = c("OLS", "HC0")) +
    scale_y_discrete(labels= c("Salary","Male", 'Age', 'Term','Single')) +
    ylab("") +
    labs(title = "Linear Model coefficients") +
    tema
  
##2 Job on average grade point over the last 10 years 
  
  
  ##Prepare Data: calculate average gpa
  gpa_vars = names(clean_students)[str_detect(names(clean_students), "gpa")]
  #gpa_vars= str_remove(gpa_vars,"gpa_2010")
  gpa_vars1120 = gpa_vars[-which(gpa_vars=="gpa_2010")]
 
  ##averages of the last 3 years
  gpa_vars20 = paste0("gpa_",seq(2017,2019,1))
  gpa_vars21 = paste0("gpa_",seq(2018,2020,1))
  gpa_vars22 = paste0("gpa_",seq(2019,2021,1))
 
  data_model = clean_students %>%
    mutate(avg20 = rowSums(select(., gpa_vars1120))/10,
           ##calculate average gpa over the last 10 years
           avg320 = rowSums(select(., gpa_vars20))/3,
           ##calculate average gpa over the last 3 years for 2020
           avg321 = rowSums(select(.,gpa_vars21))/3
           ##calculate average gpa over the last 3 years for 2021
           ) 
    
  ## Linear model controlling for sex, age
  model <- lm(avg20 ~ job + sex + age, data = data_model)
  
  chart_job = plot_summs(model, model, robust = list(FALSE, "HC0"),
             model.names = c("OLS", "HC0")) +
    scale_y_discrete(labels= c("Age","Male", 'Job')) +
    ylab("") +
    labs(title = "Linear Model coefficients") +
    tema
  
##3	Forecast the grade point average for next two years (i.e. 2021 and 2022)
  
  ##generate a quick prediction model based on job status and 10 years average
  predict_ols = lm(gpa_2020 ~ avg320 + job, data = data_model)
  
  ##check prediction works, predict, and prepare data for vis

  data_predicted = data_model %>%
    mutate(gpa_2020_hat = predict(predict_ols, data = .), ##check prediction works
           gpa_2021 = predict_ols$coefficients[1] + predict_ols$coefficients[2]*avg321 + predict_ols$coefficients[3]*job) %>%
    mutate(avg322 = rowSums(select(.,gpa_vars22))/3, ##calculate 10 years average for 2021
           gpa_2022 = predict_ols$coefficients[1] + predict_ols$coefficients[2]*avg322 + predict_ols$coefficients[3]*job) %>%
    select(.,contains("gpa"), -gpa_2020_hat) %>%
    gather(key = "Year", value = gpa)   %>%
    mutate(Year = str_remove_all(Year, "gpa_")) %>% ##reshape the data for vis
    group_by(Year) %>%
    summarise(gpa = mean(gpa)) %>%
    ungroup() %>%
    mutate(predicted = Year %in% c(2021, 2022),
           Year = year(as.Date(as.character(Year), format = "%Y"))
    )
  
  
  
##Create chart to vis prediction 
 chart_prediction =  ggplot(data = data_predicted %>%
           filter(predicted ==F), 
         aes(x = Year,
             y = gpa
         )) +
   ##Real gpa
    geom_point(size = 3,
               color = color1) +
    geom_line(color = color1)+
    
    ##predicted gpa
    geom_line(data = data_predicted %>%
                 filter(Year >2019),
               aes(x = Year,
                   y=gpa),
              linetype = "dashed",
              color = color2,
              size =1) +
    scale_x_continuous(breaks = seq(2010, 2022, 2)) +
    ylim(0,5) +
    ylab("Average gpa") +
    xlab("Year") +
    labs(title = "Prediction of average GPA (2021-2022)",
         caption = "Data from Berlin University") +
    tema

  



  
#####MAPS -----------------------------------------------------------------------------------------------------

  ##prepare data: group by country, count students, and join with centroinds
 data_cob = clean_students %>%
  group_by(cob) %>%
  summarise(Students = n()) %>%
  full_join(clean_countries)

  ##Create icon for the university of berlin
  
logo <- makeIcon(
  iconUrl = University_berlin$logo[1],
  iconWidth = 25, iconHeight = 25,
  iconAnchorX = 0, iconAnchorY = 0)

##labels for map
labels_cob <- paste0(
  '<img src = "', data_cob$flag,'"style="
  margin-left: auto;
  margin-right: auto;
  width: 100%;">',
  '<h4 style="color:',color_text,';
  margin-top:15px;">', data_cob$Students, '</h4>'
)%>% 
  lapply(htmltools::HTML)

##label of berlin university
labelU = HTML(paste0('<h5 style="color:',color_text,';
  margin-top: 0px;"> University of Berlin </h5>'))


##function for legend
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity,
                   title = "# students by<br>country of birth",
                   position = "topleft"))
}


  ##create map

  map = leaflet() %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addMarkers(data = University_berlin,
                     lng = ~lon,
                     lat = ~ lat,
               label = labelU,
               labelOptions = labelOptions(noHide = TRUE, offset=c(60,30), textOnly = TRUE),
               icon = logo) %>%
    addCircleMarkers(data = data_cob,
                     lng = ~centroid.lon,
                     lat = ~centroid.lat,
                     color = color_text,
                     fillOpacity = .2,
                     radius = ~4*Students^.5,
                     label = ~labels_cob,
                     labelOptions = labelOptions(noHide = TRUE, offset=c(-1,0), textOnly = TRUE)) %>%
  
  
    addLegendCustom(colors = color_text,
                    sizes = c(10,20,40),
                    labels = c("","",""))
  

    
  unique(clean_students$faculty)

