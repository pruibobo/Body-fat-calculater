# Shiny App

library(shiny)
library(ggplot2)
library(shinythemes)

bodyfat=read.csv("data/cleandata.csv")
BodyFat=bodyfat[,-c(1,3)]
m3 = lm(BODYFAT ~ AGE + ADIPOSITY + CHEST + ABDOMEN + WRIST +
          AGE*WRIST, data = BodyFat)

ui = fluidPage(theme = shinytheme("yeti"),
  shinyUI(
    navbarPage("Body Fat Calculator",
               
               tabPanel("Calculator",
                          titlePanel("Estimate percentage of body fat using clinically available measurements."),
                          
                          sidebarLayout(
                            sidebarPanel(
                              helpText("Please provide these clinically measurements list below. 
                                       Notice the sample of our model only contains males, 
                                       this 'calculator' only works for males."),
                              
                              radioButtons("Gender",
                                          label = "Gender",
                                          choices = c("male","female"),
                                          selected = "male",
                                          inline = T
                                          ),
                              sliderInput("Age",
                                           label = "Age (years)",
                                           min = 0, max = 85,
                                           value = 35),
                              numericInput("Height",
                                           label = "Height (inches)",
                                           min = 0,
                                           value = 66,
                                           verbatimTextOutput("value")),
                              numericInput("Weight",
                                           label = "Weight (lbs)",
                                           min = 0,
                                           value = 162.75,
                                           verbatimTextOutput("value")),
                              numericInput("Abdomen",
                                           label = "Abdomen 2 circumference (cm)",
                                           min = 0,
                                           value = 92.8,
                                           verbatimTextOutput("value")),
                              numericInput("Wrist",
                                           label = "Wrist circumference (cm)",
                                           min = 0,
                                           value = 16.9,
                                           verbatimTextOutput("value")),
                              numericInput("Chest",
                                           label = "Chest circumference (cm)",
                                           min = 0,
                                           value = 99.1,
                                           verbatimTextOutput("value")),
                              actionButton("click", "Calculate")
                              
                            ),
                            
                          
                          mainPanel(
                            conditionalPanel(
                              condition = "input.Gender=='female'",
                              h3("Sorry, this calculator only works for men.")
                            ),
                            conditionalPanel(
                              condition = "input.Gender=='male'",
                              h5("Notice: If your age is under 20, this calculator may give 
                                 an inaccurate estimation."),
                              br(),
                              br(),
                              h5("Here is a plot of different body fat levels:"),
                              plotOutput("levels", width = 1000, height = 150),
                              br(),
                              br(),
                              textOutput("bodyfat_estimation"),
                              textOutput("bodyfat_level")
                             
                              
                              
                            )
                          
                            
                          )
                        )
                ),
               
               tabPanel("Instruction",
                        h3("Tips when you measure your Weight:"),
                        h4("1. Weigh yourself at the same time every day (morning is best, after using the restroom)."),
                        h4("2. Use a quality weighing device that's set up properly."),
                        h4("3. Take care of the scale, you should provide your weight in lbs 
                           when using this calculator."),
                        h4("4. Weigh yourself naked or wear the same thing for every weight measurement."),
                        br(),
                        br(),
                        h3("Tips when you measure your Height:"),
                        h4("1. Find a flat, uncarpeted section of floor. Better to measure with your shoes off."),
                        h4("2. Remove anything on your head that may get in the way of an accurate measurement."),
                        h4("3. Remove any bulky clothing that may make it difficult to stand flat."),
                        h4("4. Stand up straight with your eyes looking straight ahead. 
                           Your line of sight and chin should be parallel to the floor."),
                        br(),
                        br(),
                        h3("Tips when you measure circumference of your body part:"),
                        h4("1. When taking these measurements, use a cloth tape measure, not a metal one."),
                        h4("2. Make sure that, when you circle your chest, wrist, 
                           or abdomen, the tape is level and neither too tight nor too loose."),
                        h4("3. Measure yourself on your bare skin, not over clothes."),
                        h4("4. Don't trust your memory - be sure to write the measurements down!")
                        ),
                          
              
               tabPanel("More Information",
                        h3("What is body fat percentage?"),
                        h4("The body fat percentage (BFP) of a human or other living being 
                           is the total mass of fat divided by total body mass, multiplied 
                           by 100; body fat includes essential body fat and storage body fat. 
                           Essential body fat is necessary to maintain life and reproductive functions. 
                           Storage body fat consists of fat accumulation in adipose tissue, 
                           part of which protects internal organs in the chest and abdomen."),
                        br(),
                        h3("What does body fat percentage indicate?"),
                        h4("The body fat percentage is a measure of fitness level. 
                           No matter what you weigh, the higher percentage of body 
                           fat you have, the more likely you are to develop obesity-related 
                           diseases, including heart disease, high blood pressure, stroke, and type 2 diabetes."),
                        br(),
                        h3("How to lose your weight in a safe way?"),
                        h4("When it comes to losing weight, the key is to eat fewer calories 
                           than you expend. If you do this, AND exercise, you will lose body fat.
                           Also, take your time-if you eat too few calories or cut out all 
                           carbohydrates, the weight you lose will likely be fluids and muscle, not fat.
                           In this case the scale will go down, but your body fat percentage will go up, 
                           rendering you less healthy. The Academy of Nutrition and Dietetics recommends 
                           losing weight slowly-0.5 to 1 pound per week-and continue exercising to 
                           maximize fat loss and minimize muscle loss."),
                        br(),
                        br(),
                        br(),
                        div("Want more information? Check these pages:", style = "color:blue"),
                        uiOutput("page1"),
                        uiOutput("page2")

                        ),
               
               
               tabPanel("Model",
                        p("Our model is constructed from a real data set of 252 men 
                          with measurements of their percentage of body fat and various 
                          body circumference measurements. The formula of our calculator is:"),
                        uiOutput("formula"),
                        uiOutput("bmi")
                        ),
               
               
               tabPanel("About",
                        p('This Shiny App is designed by:
                          Ruyan Zhou
                          '),
                        p('All questions and suggestions are welcome. Contact these maintainers:'),
                        p('Ruyan Zhou (rzhou84@wisc.edu)'),
                        p('Peibin Rui (prui@wisc.edu)'),
                        p('Tianrun Wang (twang494@wisc.edu)')
                        )
  
    )
 )
)

server = function(input, output){
  
  input_data = eventReactive(input$click,{
    AGE = input$Age 
    ADIPOSITY = 703*input$Weight/(input$Height)^2 
    ABDOMEN = input$Abdomen 
    CHEST = input$Chest 
    WRIST = input$Wrist
    
    da = data.frame(AGE,ADIPOSITY,ABDOMEN,CHEST,WRIST)
    return(da)
  })
  

  output$bodyfat_estimation = renderText({
    pred = predict(m3,input_data(),interval='confidence')
    p1 = round(pred[1],2)
    p2 = round(pred[2],2)
    p3 = round(pred[3],2)
    
    if (0<=p1&p1<=100){
      paste0("The estimation of your body fat is: ", p1, "%", ", 
             and the 95% CI of your body fat is: [", p2, "%,", p3, "%].")
    } else {
      paste0("ERROR: It seems your inputs are abnormal.")
    }
    
  })
  
  output$bodyfat_level = renderText({
    pred = predict(m3,input_data(),interval='confidence')
    p1 = round(pred[1],2)
    
    if (0<=p1&p1<5) {
      level = "Essential Fat"
      sug = "Ask your health provider about how to safely increase your body fat."
    } else if (5<=p1&p1<13) {
      level = "Athletes"
      sug = "This body fat level is often found in most elite atheletes. You may 
      consider increase your body fat if you didn't work out regularly."
    } else if (13<=p1&p1<17) {
      level = "Fitness"
      sug = "Congratulations! This body fat level is excellent for health."
    } else if (17<=p1&p1<24) {
      level = "Average"
      sug = "This body fat level is generally acceptable for good health. 
      But it's better to lose a little weight."
    } else if (24<=p1&p1<=100){
      level = "Obese"
      sug = "Ask your health provider about how to safely decrease your body fat."
    }
    
    if (0<=p1&p1<=100) {
      paste("Your body fat is at the", level, "level. And our suggestion would be: ", sug)
    }
    
  })
  
  output$levels = renderPlot({
    data = data.frame("BodyFat" = c(0.76,0.07,0.04,0.08,0.05), 
                      "class" = c("Obese (>24%)", "Average (17%-24%)", "Fitness (13%-17%)", 
                                  "Athletes (5%-13%)", "Essential Fat (<5%)"))
    data$class <- factor(data$class, levels=unique(data$class))
    ggplot(data, aes(x="", y=BodyFat, fill=class)) +
      geom_bar(stat = "identity",width = .01) +
      scale_fill_manual(values = c("firebrick", "yellow", "chartreuse3", "chartreuse1", "tomato2")) +
      coord_flip() +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(limits = c(0,1), expand = c(0, 0))+
      theme(axis.title.y = element_blank(),
            axis.ticks.y = element_blank())
  })
  
  url1 <- a("Body Fat Percentage", href="https://en.wikipedia.org/wiki/Body_fat_percentage")
  output$page1 = renderUI({
    tagList("Wiki:", url1)
  })
  
  url2 <- a("Your Body Fat Percentage: What Does It Mean?", 
            herf = "https://www.winchesterhospital.org/health-library/article?id=41373")
  output$page2 = renderUI({
    tagList("BILH:", url2)
  })
  
  output$formula = renderUI({
    withMathJax(helpText('$$BODYFAT = 48.32-1.09AGE+0.52ADIPOSITY-0.22CHEST+
                         0.71ABDOMEN-4.88WRIST+0.06AGE*WRIST$$'))
  })
  
  output$bmi = renderUI({
    withMathJax(helpText('where \\(ADIPOSITY\\) is calculated by $$ADIPOSITY(kg/m^2) 
                         = \\frac{703*WEIGHT(lbs)}{HEIGHT(inches)^2}$$.'
    ))
  })
}

shinyApp(ui = ui, server = server)
