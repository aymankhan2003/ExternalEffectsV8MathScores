library(tidyverse)
library(shiny)
library(ggplot2)
library(readxl)

#Reading in all the datasets necessary for the app
disability <- as_tibble(read_xlsx("disability.xlsx", range = "A9:E75"))
esl <- as_tibble(read_xlsx("esl.xlsx", range = "A9:E75"))
lunch <- as_tibble(read_xlsx("lunch.xlsx", range = "A9:E108"))
parentsedu <- as_tibble(read_xlsx("parentseducation.xlsx", range = "A9:E174"))
allstudents <- as_tibble(read_xlsx("allstudents.xlsx", range = "A9:D53"))


#Converting the values to numbers so there are no errors
lunch$`Average scale score` <- as.numeric(lunch$`Average scale score`)
lunch$`Standard deviation` <- as.numeric(lunch$`Standard deviation`)

#Grouping average scores by it's representing jurisdiction
allstudents <- allstudents %>%
  pivot_wider(names_from = `Jurisdiction`, values_from = `Average scale score`)

#Removing the national section as I am only working with three states
allstudents <- allstudents %>%
  select(-`National`)

#The codes below are basically just grouping the fellow variables with it's corresponding average score
avg_disability <- disability %>%
  select(-`Standard deviation`) %>%
  pivot_wider(names_from = `Disability status of student, including those with 504 plan`, values_from = `Average scale score`)

avg_esl <- esl %>%
  select(-`Standard deviation`) %>%
  pivot_wider(names_from = `Status as English learner, 2 categories`, values_from = `Average scale score`)

#Converting all missing values to 0 so it doesn't mess with the app functionality
avg_esl <- avg_esl %>%
  mutate_at(vars(`ELL`, `Not ELL`), ~ifelse(. == '#', '0', .)) %>%
  mutate_at(vars(`ELL`, `Not ELL`), as.numeric)

#All NA's also become 0
avg_esl[is.na(avg_esl)] <- 0
  
avg_lunch <- lunch %>%
  select(-`Standard deviation`) %>%
  pivot_wider(names_from = `National School Lunch Program eligibility, 3 categories`, values_from = `Average scale score`)

avg_lunch <- avg_lunch %>%
  select(-`Information not available`)

avg_parents <- parentsedu %>%
  select(-`Standard deviation`) %>%
  pivot_wider(names_from = `Parental education level, from 2 questions`, values_from = `Average scale score`)

avg_parents <- avg_parents %>%
  select(-`Unknown`)



#Both codes down below are variable place holders which will be used as select options for the user interface
variableChoices <- c('Disability',
                     'English as Second Language',
                     'National Subsidized Lunch',
                     'Parents Education Level')

regionChoices <- c('New York',
                   'Illinois',
                   'California')


# create the ui for the app
ui <- fluidPage(
  
  # Add Title Panel
  titlePanel('Impacts of External Variables on Grade 8 Math scores'),
  
  sidebarLayout(
    sidebarPanel(
      # add a way for Users to choose which Variable they are viewing
      selectInput(
        inputId = 'selected_variable',
        label = 'Select Which Variable to Display',
        choices = variableChoices),
      selectInput(
        inputId = 'selected_region',
        label = 'Select Which Region to Display',
        choices = regionChoices,
        selected = regionChoices
      )
    ),
    #Adding tabs so I can have one page for my individual graphs and one for a overall conclusion
    mainPanel(tabsetPanel(
      tabPanel('Graphs',plotOutput('figurePlot'), textOutput('textBox')),
      tabPanel('Conclusion',plotOutput('mapPlot'), textOutput('concBox'))
    ))
  ))

server <- function(input, output) {
  
  # make the figure renders. This output will work with the Graphs tab that looks at each states and variable individually.
  output$figurePlot <- renderPlot({
    if (input$selected_variable == 'Disability') {
      avg_disability %>%
        filter(`Jurisdiction` %in% input$selected_region) %>%
        ggplot() +
        geom_line(mapping = aes(x=Year, y = `Identified as students with disabilities`, color="Identified")) +
        geom_line(mapping = aes(x=Year, y = `Not identified as students with disabilities`, color="NotIdentified")) +
        labs(
          title = "Trend in Students Identified and Not Identified for Disability",
          subtitle = "Main NAEP: 2000 - 2022",
          y = "Identified/Not Identifed"
        ) +
        geom_line(mapping = aes(x=Year, y = 262, color = "Basic"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 299, color = "Proficient"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 333, color = "Advanced"), linetype=2) +
        theme_gray() +
        theme(legend.position = "right") +
        scale_color_manual(name="", breaks=c("Identified", "NotIdentified", "Advanced", "Proficient", "Basic"), labels=c("Identified as students with disabilities", "Not identified as students with disabilities", "Advanced", "Proficient", "Basic"), values=c("purple", "brown", "red", "green", "blue"))
    } else if (input$selected_variable == 'English as Second Language'){
      avg_esl %>% 
        filter(`Jurisdiction` %in% input$selected_region) %>%
        ggplot() +
        geom_line(mapping = aes(x=Year, y = `ELL`, color="ELL")) +
        geom_line(mapping = aes(x=Year, y = `Not ELL`, color="NotELL")) +
        labs(
          title = "Trend in Students ELL and Not ELL",
          subtitle = "Main NAEP: 2000 - 2022",
          y = "ELL/Not ELL"
        ) +
        geom_line(mapping = aes(x=Year, y = 262, color = "Basic"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 299, color = "Proficient"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 333, color = "Advanced"), linetype=2) +
        theme_gray() +
        theme(legend.position = "right") +
        scale_color_manual(name="", breaks=c("ELL", "NotELL", "Advanced", "Proficient", "Basic"), labels=c("ELL", "Not ELL", "Advanced", "Proficient", "Basic"), values=c("purple", "brown", "red", "green", "blue"))
    } else if (input$selected_variable == 'National Subsidized Lunch'){
      avg_lunch %>% 
        filter(`Jurisdiction` %in% input$selected_region) %>%
        ggplot() +
        geom_line(mapping = aes(x=Year, y = `Eligible`, color="Eligible", group= 1)) +
        geom_line(mapping = aes(x=Year, y = `Not eligible`, color="NotEligible", group= 1)) +
        labs(
          title = "Trend in Students Eligible and Not Eligible for NSLP",
          subtitle = "Main NAEP: 2000 - 2022",
          y = "Eligible/Not Eligible"
        ) +
        geom_line(mapping = aes(x=Year, y = 262, color = "Basic"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 299, color = "Proficient"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 333, color = "Advanced"), linetype=2) +
        theme_gray() +
        theme(legend.position = "right") +
        scale_color_manual(name="", breaks=c("Eligible", "NotEligible", "Advanced", "Proficient", "Basic"), labels=c("Eligible for subsidized lunch", "Not eligible for subsidized lunch", "Advanced", "Proficient", "Basic"), values=c("purple", "brown", "red", "green", "blue"))
    } else if (input$selected_variable == 'Parents Education Level'){
      avg_parents %>% 
        filter(`Jurisdiction` %in% input$selected_region) %>%
        ggplot() +
        geom_line(mapping = aes(x=Year, y = `Did not finish high school`, color="nohighschool")) +
        geom_line(mapping = aes(x=Year, y = `Graduated high school`, color="gradhighschool")) +
        geom_line(mapping = aes(x=Year, y = `Some education after high school`, color="nocollege")) +
        geom_line(mapping = aes(x=Year, y = `Graduated college`, color="gradcollege")) +
        labs(
          title = "Trend in Students Parents Education Levels",
          subtitle = "Main NAEP: 2000 - 2022",
          y = "Education Levels"
        ) +
        geom_line(mapping = aes(x=Year, y = 262, color = "Basic"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 299, color = "Proficient"), linetype=2) +
        geom_line(mapping = aes(x=Year, y = 333, color = "Advanced"), linetype=2) +
        theme_gray() +
        theme(legend.position = "right") +
        scale_color_manual(name="", breaks=c("nohighschool", "gradhighschool", "nocollege", "gradcollege", "Advanced", "Proficient", "Basic"), labels=c("Did no finish high school", "Graduated High School", "Some education after high school", "Finished College", "Advanced", "Proficient", "Basic"), values=c("purple", "brown", "yellow", "pink", "red", "green", "blue"))
      }
    })
  
  #This output will also work with the Graphs tab but will place text under each variable selected within the above functionality.
  output$textBox <- renderText({
    if (input$selected_variable == 'Disability' & input$selected_region == 'New York'){
      'The graph shows the huge disparity of scores between students with disability and students with no disability in New York. This graph is kind
      of shocking thinking about the diversity New York has and yet it fails to close this big score gap, and yet somehow students with disabilities fall under the Basic Threshold which
      is concerning for this state.'
    } else if (input$selected_variable == 'Disability' & input$selected_region == 'California'){
      "The graph displays the limitations students with disability face when compared with students that do not have disbility in California. As seen through the
      graph, students with disability tend to have way lower scores then those that do not have disability. This tells that California might not be 
      doing enough to support equality within education as students with disbaility are way below the Basic threshold."
    } else if (input$selected_variable == 'Disability' & input$selected_region == 'Illinois'){
      "The graph displays the difference of scores between students with disbaility and students that don't have disability in Illinois. We can see a huge 
      disparity as it shows the state is not putting much effort into giving support to students that identify with disabilities. It is questioning knowing Illinois
      to be one of the largest school districts in USA and yet students with disabilities fall below the Basic Threshold."
    } else if (input$selected_variable == 'English as Second Language' & input$selected_region == 'New York'){
      "The graph shows the difference between scores of students whose first language is English and those whose first language is not English in New York. For New York we can
      see a difference and this gap is kind of big which is understanding considering the diverse population but yet it shows the state is not working to making
      proper accomodations for those students as scores for Not ELL learners are above basic while ELL students are below basic.."
    } else if (input$selected_variable == 'English as Second Language' & input$selected_region == 'Illinois'){
      "The graph shows the difference between scores of English first language students and those whose English is not first language in Illinois. This graph does show a disparity
      and considering how big of a school district Illinois is  we would expect a smaller gap and this graph does tell that Illinois still needs to work on 
      improving its method to making education accomodable for all students."
    } else if (input$selected_variable == 'English as Second Language' & input$selected_region == 'California'){
      "The graph displays a difference in scores of ELL and Not ELL students in California. We can see a gap where ELL students fall way under the basic threshold,
      and this does raise questions whether ELL students are getting proper accomodations in California that can help set an equal educational base with others."
    }  else if (input$selected_variable == 'National Subsidized Lunch' & input$selected_region == 'New York'){
      "The graph shows difference betweens scores for students eligible for subsidized lunch and for those not in New York. This variable tends to display financial
      capability also. We can see both variables tend to be above the Basic threshold which shows that performance level is almost not much inflated but 
      there is a huge gap in scores which could tell that students not eligible tend to have more financial capability where that could play a role in higher grades
      as more accomodations in terms of money."
    } else if (input$selected_variable == 'National Subsidized Lunch' & input$selected_region == 'Illinois'){
      "The graph displays a huge gap between students eligible for subsidized lunch and those who are not eligible in Illinois. Illinois is a huge school district
      where there could be many students with more financial capabilities then others as this graph does show this difference, where students who are not eligible
      are scoring more, meaning they have more access to more materials then others."
    } else if (input$selected_variable == 'National Subsidized Lunch' & input$selected_region == 'California'){
      "The graph shows a small disparity between eligible and not eligible students in California. This means even though students not eligible have higher scores,
      that gap is not that wide with those who are eligible but overall we can say financial abilities might play a role here as they have more access to external 
      stuff that some students don't have."
    } else if (input$selected_variable == 'Parents Education Level' & input$selected_region == 'New York'){
      "The graph shows how different educational levels of parents impact scores in New York. We can see that parents that graduated college have children whose
      average scores are higher then others. This means these parents tend to have good jobs which means good financial capabiltiy, or their education gives them the 
      ability to help their own children. But we can see there is not much of a gap in New York within the education levels that shows education levels are almost accomodable
      for every student no matter their parents background."
    } else if (input$selected_variable == 'Parents Education Level' & input$selected_region == 'Illinois'){
      "This graph shows how parents education level differ in students scores within Illinois. We can see that students whose parents finished college tend to score higher.
      This does make sense, as these students get to have people who can help them who went through the same system as them. But knowing how big of a school
      district Illinois is the gap does not make sense, as parents with only high school and no high school their students are kind of way below then those who had education after high school.
      This is kind of unexpected as Illinois should work harder to make this gap lower and provide access to materials and accomodations to everyone equally."
    } else if (input$selected_variable == 'Parents Education Level' & input$selected_region == 'California'){
      "This graph shows the scores of students depending on their parents education level in California. We can see a huge disparity between 
      parents that finished college and parents that either did or did not do high school. California is a place where almost all big time professionals move to
      which makes sense for this gap but again this shouldn't create a disparity if the state would be working hard to make sure that every students get equal access
      to materials that would help them improve their scores."
    }
  })
  
  # make the final conclusion graph within the conclusion tab.
  output$mapPlot <- renderPlot({
    allstudents %>% 
      ggplot() +
      geom_line(mapping = aes(x=Year, y = `California`, color="california")) +
      geom_line(mapping = aes(x=Year, y = `Illinois`, color="illinois")) +
      geom_line(mapping = aes(x=Year, y = `New York`, color="newyork")) +
      labs(
        title = "Trend in All Students Scores across all Three States",
        subtitle = "Main NAEP: 2000 - 2022",
        y = "Jurisdiction"
      ) +
      geom_line(mapping = aes(x=Year, y = 262, color = "Basic"), linetype=2) +
      geom_line(mapping = aes(x=Year, y = 299, color = "Proficient"), linetype=2) +
      geom_line(mapping = aes(x=Year, y = 333, color = "Advanced"), linetype=2) +
      theme_gray() +
      theme(legend.position = "right") +
      scale_color_manual(name="", breaks=c("california", "illinois", "newyork", "Advanced", "Proficient", "Basic"), labels=c("California", "Illinois", "New York", "Advanced", "Proficient", "Basic"), values=c("purple", "brown", "yellow", "red", "green", "blue"))
  })
  
  #Overall analysis text in the conclusion tab that explains our overall understanding. 
  output$concBox <- renderText({
    "After examining all the variables within each state individually, we can see many disparities between the privilege and non privilege students. Overall, these three big states
    need to work harder to make education accomodable for every student of every background. This big graph shows an overall plot and we can see New York
    and Illinois in the same range, whereas California is almost near, but the main point is that all these three states are below Proficient and moderately above Basic 
    which tells us that the emphasis on improving students education level is not prioritized. The individual graphs did show that students with more financial abilities 
    tend to have more higher scores, and it is mostly because of their access to external materials, and these big states need to provide external materials
    to other students who don't have the financial freedom like others. If education was prioritized then these lines would be able to go above proficient and every
    kid could have a chance at a better education. In simple words, there should be more accomodations for students with disabilities, there should be more access to
    external materials for every student no matter their financial background, and there should be more effort put into students whose first language is not English.
    If these can be put into place, then the huge disparity in grades can shrink and grow smaller."
  })
}

# run the app  
shinyApp(ui = ui, server = server)