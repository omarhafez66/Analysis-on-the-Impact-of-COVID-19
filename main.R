library(xlsx)
library(dplyr)

data<-read.csv("C:/Users/Fares/Documents/university/datascianceproject/COVID-19 Survey Student Responses.csv")

data_no_duplicates<-distinct(data)

sum(duplicated(data_no_duplicates))
sum(is.na(data_no_duplicates))

data_no_nulls<-na.omit(data_no_duplicates)

data_no_none1 <- data_no_nulls[data_no_nulls$"Prefered.social.media.platform" != "None", ]
data_no_none2 <- data_no_none1[data_no_none1$"Prefered.social.media.platform" != "None ", ]
data_no_none3 <- data_no_none2[data_no_none2$"What.you.miss.the.most" != ".", ]
data_no_redun<-data_no_none3

fix_blank<-function(x) {
  if (x=="") {
    x <- 0
  }
  return(x)
}

data_no_redun <- data_no_redun[complete.cases( data_no_redun$Time.spent.on.TV), ]



tv_fix<-function(x) {
  result <- tryCatch(as.numeric(x), error = function(e) 0)
  if (is.na(result)) {
    result <- 0
  }
  return(result)
}
data_no_redun$Time.spent.on.TV<-apply(data_no_redun["Time.spent.on.TV"],1,FUN = tv_fix)
fix_phys<-function(x) {
  if (x=="Exercise"||x=="Exercising"||x=="Cardio"||x=="Workout "||x=="Workout"||x=="workout"||x=="Dancing"||x=="I run"||x=="working out and some physical activity"||x=="Workout and listening music"||x=="Sports"||x=="Gym"||x=="Cricket"||x=="Football"||x=="Running") {
    x <- "Physical Activity"
  }
  return(x)
}
fix_watchmedia<-function(x) {
  if (x=="Watching web series"||x=="Anime Manga"||x=="live stream watching"||x=="Netflix, Friends and Books"||x=="watching movies,reading books,games,listening to music,sleep,dancing"||x=="Watching ted talks and music and books"||x=="Watching YouTube "||x=="Youtube"||x=="web series"||x=="Web Series") {
    x <- "Watching Media"
  }
  return(x)
}
fix_readstudy<-function(x) {
  if (x=="All reading books watching web series listening to music and talking to friends"||x=="Coding and studying for exams"||x=="Reading"||x=="Reading books"||x=="Reading books, music, exercise") {
    x <- "Reading/Studying"
  }
  return(x)
}
fix_social<-function(x) {
  if (x=="Calling friends"||x=="Taking with parents"||x=="Talk with childhood friends."||x=="Talking"||x=="Talking to your relatives"||x=="Talking with friends "||x=="With a friend") {
    x <- "Socializing"
  }
  return(x)
}
fix_creative<-function(x) {
  if (x=="Drawing"||x=="drawing"||x=="drawing "||x=="Drawing and painting and sketching."||x=="Drawing, painting"||x=="Painting"||x=="Singing"||x=="singing"||x=="Poetry, writing books and novels , listening to music too"||x=="Sketching"||x=="Sketching and writing"||x=="sketching,reading books,meditation,songs"||x=="Writing"||x=="Writing my own Comics & novels"||x=="Painting "||x=="Drawing and painting and sketching") {
    x <- "Creative Work"
  }
  return(x)
}
fix_game<-function(x) {
  if (x=="Playing"||x=="playing"||x=="Online gaming"||x=="I play Rubiks cube"||x=="Online gaming , surfing and listening to music "||x=="pubg"||x=="Indoor Games") {
    x <- "Play Games"
  }
  return(x)
}
fix_chores<-function(x) {
  if (x=="Cooking"||x=="gardening cartoon"||x=="Do some home related stuff") {
    x <- "House Chores"
  }
  return(x)
}
fix_music<-function(x) {
  if (x==" listening music, motion design, graphic design, sleeping."||x=="Both listining music and scrolling down social media"||x=="Listening to music"||x=="Listening to music and reading books both . "||x=="listening to music,reading books and dancing.") {
    x <- "Listen to Music"
  }
  return(x)
}
fix_socialmedia<-function(x) {
  if (x=="Scrolling through social media"||x=="Social Media"||x=="Online surfing") {
    x <- "Consume Social Media"
  }
  return(x)
}
fix_sleep<-function(x) {
  if (x=="Sleep"||x=="Sleeping"||x=="Sleeping, Online games") {
    x <- "Sleep"
  }
  return(x)
}
fix_meditate<-function(x) {
  if (x=="Meditation") {
    x <- "Meditate"
  }
  return(x)
}
fix_others<-function(x) {
  if (x=="Crying"||x=="Many of these"||x=="Whatever want "||x=="Dont get distreessed"||x=="Business"||x=="Many among these "||x=="Driving"||x=="I cant de-stress myslef"||x=="I have no problem of stress "||x=="No able to reduce the stress "||x=="By engaging in my work."||x=="Forming "||x=="Work"||x=="Forming"||x=="no stress"||x=="Whatever want") {
    x <- "Others"
  }
  return(x)
}




fix_all<-function(x) {
  if (x=="All "||x=="All"||x=="all"||x=="ALL "||x=="All the above"||x=="All above"||x=="all of the above"||x=="everything"||x=="All of them"||x=="ALL"||x=="All of the above ") {
    x <- "All of the above"
  }
  return(x)
}
fix_human<-function(x) {
  if (x=="Friends , relatives"||x=="Friends and roaming around freely"||x=="Friends and School"||x=="Friends, relatives & travelling"||x=="Friends,Romaing and traveling"||x=="Only friends"||x=="Being social "||x=="Family "||x=="Family"||x=="The idea of being around fun loving people but this time has certainly made us all to reconnect (and fill the gap if any) with our families and relatives so it is fun but certainly we do miss hanging out with friends") {
    x <- "Human Interaction"
  }
  return(x)
}
fix_outside<-function(x) {
  if (x=="Badminton in court"||x=="Football"||x=="Going to the movies"||x=="Gym"||x=="Metro"||x=="Playing"||x=="Roaming around freely"||x=="Taking kids to park"||x=="Travelling"||x=="Travelling & Friends"||x=="Eating outside"||x=="Eating outside and friends.") {
    x <- "Going Outside/Travel"
  }
  return(x)
}
fix_school<-function(x) {
  if (x=="School/college"||x=="Job"||x=="Colleagues"||x=="School and friends."||x=="School and my school friends"||x=="school, relatives and friends"||x=="My normal routine") {
    x <- "School/Work"
  }
  return(x)
}
fix_nothing<-function(x) {
  if (x=="Nothing"||x=="Nah, this is my usual lifestyle anyway, just being lazy...."||x=="NOTHING"||x=="nothing"||x=="I have missed nothing "||x=="To stay alone. "||x=="Nothing ") {
    x <- "None of the above/Prefer Lockdown"
  }
  return(x)
}
fix_sother<-function(x) {
  if (x=="Elyment"||x=="Quora"||x=="Reddit"||x=="Snapchat"||x=="Talklife"||x=="Telegram"||x=="Twitter") {
    x <- "Other"
  }
  return(x)
}


data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_phys)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_watchmedia)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_readstudy)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_social)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_creative)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_game)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_chores)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_music)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_socialmedia)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_sleep)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_meditate)
data_no_redun$Stress.busters<-apply(data_no_redun["Stress.busters"],1,FUN = fix_others)


data_no_redun$What.you.miss.the.most<-apply(data_no_redun["What.you.miss.the.most"],1,FUN = fix_all)
data_no_redun$What.you.miss.the.most<-apply(data_no_redun["What.you.miss.the.most"],1,FUN = fix_human)
data_no_redun$What.you.miss.the.most<-apply(data_no_redun["What.you.miss.the.most"],1,FUN = fix_outside)
data_no_redun$What.you.miss.the.most<-apply(data_no_redun["What.you.miss.the.most"],1,FUN = fix_school)
data_no_redun$What.you.miss.the.most<-apply(data_no_redun["What.you.miss.the.most"],1,FUN = fix_nothing)

data_no_redun$Prefered.social.media.platform<-apply(data_no_redun["Prefered.social.media.platform"],1,FUN = fix_sother)


library(ggplot2)

piepercent=round(100*table(data_no_redun$Region.of.residence)/sum(table(data_no_redun$Region.of.residence)), 2)
region_counts <- table(data_no_redun$Region.of.residence)
pie(region_counts, labels = paste0(piepercent,"%"), main = "Region of Residence", col = rainbow(length(names(region_counts))))
legend("topright",c(names(region_counts)), fill = rainbow(length(names(region_counts))))

stress_busters <- (table(data_no_redun$Stress.busters))[c(names((table(data_no_redun$Stress.busters)))[names((table(data_no_redun$Stress.busters))) != "Others"], "Others")]
barplot(stress_busters,
        main = "Stress Busters",
        xlab = "Activity",
        ylab = "Frequency",
        col = "skyblue",
        names.arg = gsub(" ", "\n", names(stress_busters)),
        cex.names = 0.7)
piepercent_stress=round(100*stress_busters/sum(stress_busters), 2)
pie(stress_busters, labels = paste0(piepercent_stress,"%"), main = "Stress Busters", col = rainbow(length(names(stress_busters))))
legend("topright",c(names(stress_busters)), fill = rainbow(length(names(stress_busters))), cex=0.49)

what_you_miss <- (table(data_no_redun$What.you.miss.the.most))[c(names((table(data_no_redun$What.you.miss.the.most)))[names((table(data_no_redun$What.you.miss.the.most))) != "All of the above"], "All of the above")]
what_you_miss <- (what_you_miss)[c(names((what_you_miss))[names((what_you_miss)) != "None of the above/Prefer Lockdown"], "None of the above/Prefer Lockdown")]
barplot(what_you_miss,
        main = "What You Miss the Most",
        xlab = "Activity",
        ylab = "Frequency",
        col = "skyblue",
        cex.names = 0.7,
        ylim = c(0, 500),
        yaxp = c(0, 500, 10))
piepercent_miss=round(100*what_you_miss/sum(what_you_miss), 2)
pie(what_you_miss, labels = paste0(piepercent_miss,"%"), main = "What You Miss the Most", col = rainbow(length(names(what_you_miss))))
legend("topright",c(names(what_you_miss)), fill = rainbow(length(names(what_you_miss))), cex=0.8)

#loading needed packages for clustering
library(xlsx)
library(dplyr)
library(rpart)
library(rpart.plot)
#get csv
data<-data_no_redun
#prepare the cleaned data for clustering and remove unnecessary columns for clustering

data_kmeans<-data[,-c(1,5,6,11,14,16,17,18,19)]

fix_region_for_clusting<-function(x) {
  if (x == "Delhi-NCR") {
    return(0)
  }
  return(1)
}
data_kmeans$Region.of.residence<-apply(data_kmeans["Region.of.residence"],1,FUN = fix_region_for_clusting)


fix_issue_for_clustring<- function(x){
  if (x == "NO") {
    return(0)
  }
  return(1)
}
fix_issue_for_clustring("YES")
data_kmeans$Health.issue.during.lockdown<-apply(data_kmeans["Health.issue.during.lockdown"],1,FUN = fix_issue_for_clustring)
#check columns to moke sure they are all numeric
is.numeric(data_kmeans$Region.of.residence)
is.numeric(data_kmeans$Age.of.Subject)
is.numeric(data_kmeans$Time.spent.on.Online.Class)


is.numeric(data_kmeans$Time.spent.on.self.study)
is.numeric(data_kmeans$Time.spent.on.fitness)


is.numeric(data_kmeans$Time.spent.on.sleep)

is.numeric(data_kmeans$Time.spent.on.social.media)

is.numeric(data_kmeans$Time.spent.on.TV)

is.numeric(data_kmeans$Number.of.meals.per.day)

is.numeric(data_kmeans$Health.issue.during.lockdown)
#apply kmeans clustering on the data and choose three centorids
kmeans_clustring<-kmeans(data_kmeans,centers = 3)
data$cluster <- kmeans_clustring$cluster
data$actual<- data$Change.in.your.weight

data$actual <- factor(data$actual, levels = c("Increased", "Decreased", "Remain Constant"))
data$actual_numeric <- as.numeric(data$actual)
data$compare_cluster <- ifelse(data$cluster == data$actual_numeric, "Equal", "Not Equal")
result <- table(data$compare_cluster)
print(result)
data$actual <- factor(data$actual, levels = c("Increased", "Remain Constant", "Decreased"))
data$actual_numeric <- as.numeric(data$actual)
data$compare_cluster <- ifelse(data$cluster == data$actual_numeric, "Equal", "Not Equal")
result <- table(data$compare_cluster)
print(result)

data$actual <- factor(data$actual, levels = c("Remain Constant", "Decreased", "Increased"))
data$actual_numeric <- as.numeric(data$actual)
data$compare_cluster <- ifelse(data$cluster == data$actual_numeric, "Equal", "Not Equal")
result <- table(data$compare_cluster)
print(result)

data$actual <- factor(data$actual, levels = c("Decreased", "Remain Constant", "Increased"))
data$actual_numeric <- as.numeric(data$actual)
data$compare_cluster <- ifelse(data$cluster == data$actual_numeric, "Equal", "Not Equal")
result <- table(data$compare_cluster)
print(result)

data$actual <- factor(data$actual, levels = c("Decreased", "Increased", "Remain Constant"))
data$actual_numeric <- as.numeric(data$actual)
data$compare_cluster <- ifelse(data$cluster == data$actual_numeric, "Equal", "Not Equal")
result <- table(data$compare_cluster)
print(result)


data$actual <- factor(data$actual, levels = c("Remain Constant", "Increased", "Decreased"))
data$actual_numeric <- as.numeric(data$actual)
data$compare_cluster <- ifelse(data$cluster == data$actual_numeric, "Equal", "Not Equal")
result <- table(data$compare_cluster)
print(result)


data$clustring.most.accurate.names <- factor(data$cluster, levels = 1:3, labels = c("Remain Constant", "Increased", "Decreased"))
data$actual<- data$Change.in.your.weight


library(caret)

set.seed(123)
data_classification<-data[,-c(1,5,6,11,16,17,18,19,20,21,22,23,24)]


#----------------------------------------------------------------> train test
# Create a random sample of row indices for the training set
train_indices <- sample(1:nrow(data_classification), 0.7 * nrow(data_classification))

# Create training and testing sets
train_data <- data_classification[train_indices, ]
test_data <- data_classification[-train_indices, ]


T_Project <- rpart(Change.in.your.weight~
                     Age.of.Subject+
                     Time.spent.on.fitness+
                     Time.spent.on.sleep+
                     Time.spent.on.social.media+
                     Time.spent.on.TV+
                     Number.of.meals.per.day+
                     Health.issue.during.lockdown,
                   data = train_data)
rpart.plot(T_Project)
test_data$classification.names<- predict(T_Project,test_data[,-c(10)],type = "class")
test_data$compare_classification <- ifelse(test_data$classification.names == test_data$Change.in.your.weight, "Equal", "Not Equal")
result <- table(test_data$compare_classification )
print(paste((as.numeric(result['Equal'])/as.numeric(result['Not Equal']))*100 ,'%'))

#-------------------------------------------------------->end of train test
#--------------------------------------------------------->start of all data training and testing

T_Project <- rpart(Change.in.your.weight~
                     Age.of.Subject+
                     Time.spent.on.fitness+
                     Time.spent.on.sleep+
                     Time.spent.on.social.media+
                     Time.spent.on.TV+
                     Number.of.meals.per.day+
                     Health.issue.during.lockdown,
                   data = data_classification)
rpart.plot(T_Project)
data$classification.names<- predict(T_Project,data[,-c(1,5,6,14,11,16,17,18,19,20,21,22,23,24)],type = "class")
data$compare_classification <- ifelse(data$classification.names == data$Change.in.your.weight, "Equal", "Not Equal")
result <- table(data$compare_classification )
print(paste((as.numeric(result['Equal'])/as.numeric(result['Not Equal']))*100 ,'%'))



#prepare data for gui
gui_data<-data[,-c(26,23,22,21,20,1)]

write.csv(gui_data,file = "C:/Users/Fares/Documents/university/datascianceproject/gui_data.csv",row.names = F)
library(xlsx)
library(dplyr)
library(ggplot2)
library(ggplotgui)
library(shiny)


ui <- fluidPage(
  titlePanel("GGPlot GUI"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File", accept = ".csv",),
      selectInput("x", "X-axis:", choices = NULL),
      conditionalPanel(
        condition = 'input.plot_type != "Bar Plot"  && input.plot_type != "Pie chart" ',
        
        
        selectInput("y", "Y-axis:", choices = NULL)
      ),
      selectInput("plot_type", "Select Plot Type:",
                  choices = c("Scatter Plot","Pie chart", "Line Plot", "Bar Plot"), selected = "Scatter Plot"),
      actionButton("plot", "Plot")
    ),
    mainPanel(
      plotOutput("ggplot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath, header = TRUE)
  })
  
  
  observe({
    
    if (!is.null(data())) {
      updateSelectInput(session, "x", choices = names(data()))
      
      
      updateSelectInput(session, "y", choices = names(data()))
      
    }
  })
  
  output$ggplot <- renderPlot({
    req(input$plot)
    
    if (input$plot_type == "Bar Plot") {
  
      top_10_data <- data() %>% 
        count(!!sym(input$x)) %>%
        arrange(desc(n)) %>%
        head(10)
      
      ggplot(top_10_data, aes_string(x = input$x, y = "n")) +
        geom_bar(stat = "identity") +
        labs(x = input$x, y = "Frequency", title = "Bar Plot (Top 10)"
             ,names.arg = gsub(" ", "\n", names(top_10_data)))
      
    } else if(input$plot_type == "Pie chart"){
      
      top_values <- data() %>%
        group_by(!!sym(input$x)) %>%
        summarize(Frequency = n()) %>%
        arrange(desc(Frequency)) %>%
        top_n(12)  # Adjust the number of top values as needed
      
      ggplot(top_values, aes(x = "", y = Frequency, fill = !!sym(input$x))) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        labs(title = "Top Values Pie Chart", fill = input$x)
    }else {
      # For Scatter Plot and Line Plot
      plot_type <- switch(input$plot_type,
                          "Scatter Plot" = geom_point(),
                          "Line Plot" = geom_line())
      
      ggplot(data(), aes_string(x = input$x, y = input$y)) +
        plot_type
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)



#write.csv(data_kinda_clean,file = "C:/Users/Fares/Documents/university/datascianceproject/final.csv",row.names = F)
#write.csv(data_kinda_clean,file = "C:/Users/Fares/Documents/university/datascianceproject/letsneshelmuhmmedmenalgroup.csv",row.names = F)

