set.seed(123)
setwd("E:/data visualization/IPL Data 2008-16")
l
library(readxl)
library(dplyr)
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)

ball_by_ball <- read_excel("ball_by_ball.xlsx",sheet = "ball_by_ball")
batsman_scored <- read_excel("batsman_scored.xlsx", sheet = "batsman_scored")
player <- read_excel("player.xlsx", sheet = "player")


#========================= Over all Top N Batsman: ================================#

player2 <- player %>% select(c(player_id, player_name))
ball2 <- ball_by_ball %>% select(c(match_id, over_id, ball_id, innings_no, striker))

newdf <- merge.data.frame(player2, ball2, by.x = "player_id", by.y = "striker", all = T)

newdf2 <- merge.data.frame(newdf, batsman_scored, by = c("match_id","over_id","ball_id","innings_no"), all = T)


# Top 10 Batsman based on runs scored
topruns <- newdf2 %>% group_by(player_name) %>%
  summarise("Total.Runs" = sum(runs_scored, na.rm = T))

top10runs <- topruns %>% arrange(-Total.Runs) %>% head(10)

# Top 10 Batsman based on batting average
numMatch <- newdf2 %>% group_by(player_name, match_id) %>% 
  summarise("Match" = n())
numMatch$Match <- NULL
head(numMatch)

matchPlayed <- numMatch %>% group_by(player_name) %>% 
  summarise(Count = length(unique(match_id)))
batavg <- round(topruns$Total.Runs / matchPlayed$Count,2)

topruns$batavg <- batavg

top10avg <- topruns %>% arrange(-batavg) %>% head(10) %>% select(c(player_name, batavg))

# Top 10 Batsman based on batting strike rate
bowlFaced <- newdf2 %>% group_by(player_name) %>% summarise(BowlsFaced = n())

batstrike <- round(topruns$Total.Runs*100/bowlFaced$BowlsFaced,2)

topruns$batstrike <- batstrike

top10batstrike <- topruns %>% arrange(-batstrike) %>% head(10) %>% 
  select(c(player_name, batstrike))

# Top 10 batsman based on highest score
Score <- newdf2 %>% group_by(player_name, match_id) %>% 
  summarise("Score" = sum(runs_scored, na.rm = T))
highestScore <- Score %>% group_by(player_name) %>% 
  summarise(HighestScore = max(Score))

topruns$highesScore = highestScore$HighestScore

top10highscore <- Score %>% arrange(-Score) %>% head(10) %>% select(-match_id)

write.csv(topruns, "E:/Tableau/IPL/TopRuns.csv")

#======================= Season wise ============================#
season <- read_excel("season.xlsx",sheet = "season")

# Orange Cap Holder
season2 <- season %>% select(orange_cap,season_year)

playerid <- season2$orange_cap

orangeplayer <- player %>% filter(player_id %in% playerid) %>% select(player_id,player_name) %>% unique()

orangecap <- merge.data.frame(orangeplayer, season2, by.x = "player_id", by.y = "orange_cap", all = T)

orangecap <- orangecap %>% arrange(season_year)

#Purple Cap Holder
season3 <- season %>% select(purple_cap,season_year)

playerid <- season3$purple_cap

purpleplayer <- player %>% filter(player_id %in% playerid) %>% select(player_id,player_name) %>% unique()

purplecap <- merge.data.frame(purpleplayer, season3, by.x = "player_id", by.y = "purple_cap", all = T)

purplecap <- purplecap %>% arrange(season_year)

write.csv(orangecap, "E:/Tableau/IPL/OrangeCap.csv")

write.csv(orangecap, "E:/Tableau/IPL/PurpleCap.csv")

#======================= Total Number ===========================#
wicket <- read_excel("wicket_taken.xlsx", sheet = "wicket_taken")
match <- read_excel("match.xlsx", sheet = "match")

match2 <- match %>% select(match_id, season_id)

wickdf <- merge.data.frame(wicket, match2,by = "match_id", all = T)
wickdf <- merge.data.frame(wickdf, season %>% select(season_id, season_year), by = "season_id", all = T)

# Matches

numMatch <- match2 %>% group_by(season_id) %>% summarise(matches = n())
numMatch <- merge.data.frame(numMatch, season %>% select(season_id, season_year), by = "season_id", all = T)
numMatch <- numMatch %>% select(season_year, matches)

# Wickets
wickets <- wickdf %>% group_by(season_year) %>% summarise(Wickets = n())

# 6s
rundf <- merge.data.frame(newdf2, match2, by = "match_id", all = T)
rundf <- merge.data.frame(rundf, season %>% select(season_id, season_year), by = "season_id", all = T)

no6 <- rundf %>% filter(runs_scored == 6) %>% group_by(season_year) %>% summarise(Count6 = n())

# 4s
no4 <- rundf %>% filter(runs_scored == 4) %>% group_by(season_year) %>% 
  summarise(Count4 = n())

totalNumber <- data.frame(numMatch, wickets, no6, no4)
totalNumber <- totalNumber %>% select(-c(season_year.1,season_year.2,season_year.3))

write.csv(totalNumber, "E:/Tableau/IPL/totalNumber.csv")
#======================== Title won by Teams ===========================#
team <- read_excel("team.xlsx", sheet = "team")

match3 <- match %>% select(match_id, season_id, match_winner, match_date)

teamtitle <- merge.data.frame(match3,team, by.y = "team_id", by.x = "match_winner", all = T)
teamtitle <- teamtitle %>% arrange(match_date)

seaid <- unique(teamtitle$season_id)
final <- c()
for(i in seaid){
  final <- c(final, max(which(teamtitle$season_id == i)))
}

winnerteam <- teamtitle[final,]

winnerteam <- merge.data.frame(winnerteam, season %>% select(season_id, season_year), by = "season_id", all = T)

winners <- winnerteam %>% select(season_year, team_name) %>% group_by(team_name) %>% 
  summarise(Titles = n()) %>% arrange(Titles)

write.csv(winners, "E:/Tableau/IPL/winners.csv")


header=dashboardHeader(title = "IPL Dashboard")


sidebar=dashboardSidebar(selectInput(inputId = "Year",label = "Select the year",
                                     choices = c("All",unique(wickets$season_year)),
                                     selected = "All"),
                         sidebarMenu(
                           menuItem("Batsman",tabName = "KPI1",icon=icon("Dashboard")),
                           menuItem("Cap holder",tabName = "KPI2",icon=icon("Dashboard")),
                           menuItem("Numbers",tabName = "KPI3",icon=icon("Dashboard")),
                           menuItem("Title Wins",tabName = "KPI4",icon=icon("Dashboard")),
                           menuItem("Visit us",href = "http://google.com")))



#Body
body = dashboardBody(
  tabItems(
    tabItem(tabName = "KPI1",
            fluidRow(
              valueBoxOutput("value1"),
              valueBoxOutput("value2"),
              valueBoxOutput("value3")),
            
            #Fluid Row of two charts
            
            fluidRow(
              box(title = "Top 10 Runs Scorer",width = 6, collapsible = TRUE,plotOutput("TOP10Runs")),
              box(title = "Top 10 Batting Average",width = 6, collapsible = TRUE,plotOutput("TOP10Avg")),
              box(title="Top 10 Highest Score",width = 6,collapsible = TRUE,plotOutput("TOPScores")),
              box(title = "Top 10 Batting Strike Rate",width = 6,collapsible = TRUE,plotOutput("TOP10Strikers")))),
    #2nd page  
    tabItem(tabName = "KPI2",
            fluidRow(
              valueBoxOutput("value11"),
              valueBoxOutput("value22")),
              
              #Fluid Row of two charts
              
            fluidRow(
                box(title = "Orange Cap Holder",width = 6, collapsible = TRUE,dataTableOutput("Orange")),
                box(title = "Purple Cap Holder",width = 6, collapsible = TRUE,dataTableOutput("Purple")))),
            #3rd page  
    tabItem(tabName = "KPI3",
            fluidRow(
              valueBoxOutput("value111"),
              valueBoxOutput("value222"),
              valueBoxOutput("value333"),
              valueBoxOutput("value444")),
            
            #Fluid Row of two charts
            
            fluidRow(
              box(title = "Total no of matches",width = 12, collapsible = TRUE,dataTableOutput("Wkts")))),
    
                    
            
            #3nd page  
    tabItem(tabName = "KPI4",
            
                    #Fluid Row of two charts
            fluidRow(
              box(title = "Team Wise Count Wins",width = 12, collapsible = TRUE,dataTableOutput("Wins"))))
    
                    
            
    )
  )

#Lets Create a UI
UI=dashboardPage(header = header,sidebar = sidebar, body = body)

############################################################################

server = function(input,output){
  
  output$value1=renderValueBox({
    valueBox(top10runs%>%head(1),"Top Runs Scorer")
  })
  
  # Highest run scorer
  #2nd value box
  
  output$value2=renderValueBox({
    valueBox(top10avg%>%head(1),"Best Average")
  })
  
  # 
  #3nd value box Highest Indiviual score
  
  output$value3=renderValueBox({
    valueBox(top10highscore%>%head(1),"Highest Indiviual Score")
  })
  
  
  # Top 10 Batsman based on Runs
  
  output$TOP10Runs=renderPlot({
    ggplot(top10runs,aes(x=reorder(player_name,-Total.Runs),y=Total.Runs))+
      geom_bar(stat="Identity",width = .5,fill="#5DA74C")+
      geom_text(aes(label=Total.Runs),position = position_dodge(width = .9),vjust=-0.25)+
      theme(axis.text.x = element_text(angle = 45))+
      labs(y="Total.Runs",x=" ")+
      theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
    
  })
  
  #Top 10 Average
  output$TOP10Avg=renderPlot({
    ggplot(top10avg,aes(x=reorder(player_name,-batavg),y=batavg))+
      geom_bar(stat="Identity",width = .5,fill="#27917c")+
      geom_text(aes(label=batavg),position = position_dodge(width = .9),vjust=-0.25)+
      theme(axis.text.x = element_text(angle = 45))+
      labs(y="Batting Average",x=" ")+
      theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
    
  })
  
  #Top 10 Scores
  
  output$TOPScores=renderPlot({
    ggplot(top10highscore,aes(x=reorder(player_name,-Score),y=Score))+
      geom_bar(stat="Identity",width = .5,fill="#27917c")+
      geom_text(aes(label=Score),position = position_dodge(width = .9),vjust=-0.25)+
      theme(axis.text.x = element_text(angle = 45))+
      labs(y="Highest Scores",x=" ")+
      theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
    
  })
  
  #
  output$TOP10Strikers=renderPlot({
    ggplot(top10batstrike,aes(x=reorder(player_name,-batstrike),y=batstrike))+
      geom_bar(stat="Identity",width = .5,fill="#27917c")+
      geom_text(aes(label=batstrike),position = position_dodge(width = .9),vjust=-0.25)+
      theme(axis.text.x = element_text(angle = 45))+
      labs(y="Strike Rate",x=" ")+
      theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())
    
  })
  #___________________________________________________________________________________________   
  #2nd Page Dash Board
  output$value11=renderValueBox({
    data = {
      if(input$Year =="All"){
        data1=orangecap
      }else {
        data1=filter(orangecap,season_year==input$Year)
      }
      data1
    }
    valueBox(data$player_name,"Orange Cap")
  })
  
  
  #2nd value box
  
  output$value22=renderValueBox({
    data = {
      if(input$Year =="All"){
        data1=purplecap
      }else {
        data1=filter(purplecap,season_year==input$Year)
      }
      data1
    }
    valueBox(data$player_name,"Purple Cap")
  })
  
  
  #Orange Cap  
  output$Orange=renderDataTable({
    datatable(orangecap)
  })  
  
  #Purple Cap 
  output$Purple=renderDataTable({
    datatable(purplecap,width = 12)
  })
  ?datatable
  #________________________________________________________________________________________
  #Third Dashboard 
  
  output$value111=renderValueBox({
    data = {
      if(input$Year =="All"){
        data1=numMatch
      }else {
        data1=filter(numMatch,season_year==input$Year)
      }
      data1
    }
    valueBox(data$matches,"Number of Matches")
  }) 
  
  #wickets
  
  output$value222=renderValueBox({
    data = {
      if(input$Year =="All"){
        data1=wickets
      }else {
        data1=filter(wickets,season_year==input$Year)
      }
      data1
    }
    
    valueBox(data$Wickets,"Number of Wickets")
  })  
  #6's
  
  output$value333=renderValueBox({
    data = {
      if(input$Year =="All"){
        data1=no6
      }else {
        data1=filter(no6,season_year==input$Year)
      }
      data1
    }
    
    valueBox(data$Count6,"Number of sixes")
  })   
  
  # 4's
  output$value444=renderValueBox({
    data = {
      if(input$Year =="All"){
        data1=no4
      }else {
        data1=filter(no4,season_year==input$Year)
      }
      data1
    }
    
    valueBox(data$Count4,"Number of fours")
  })   
  
  output$Wkts=renderDataTable({
    datatable(totalNumber)
  })
  
  
  #Forth page Plot
  
  
  output$Wins=renderDataTable({
    winners
  })
}

##Create my app
shinyApp(UI,server)














