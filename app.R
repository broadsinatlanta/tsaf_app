source("./helper.R", local = TRUE) ; is.function(re_exp)

ui <- tagList(
        useShinyjs(),
        navbarPage(fluid = TRUE, 
                   "TSAF",
                   
                   tabPanel("Configuration",
                            
                            tags$style(appCSS),
                            
                            sidebarLayout(
                                    
                                    sidebarPanel(
                                            
                                            h2("TSAF - Twitter Sentiment Analysis and Findings"),
                                            
                                            p("TSAF is a web app made in ", em("R"), "using the ", em("Shiny"),
                                              "package. Taking your keywords as a query, TSAF retrieves data from Twitter
                                           and then returns detailed sentiment analysis, word breakdowns and user information."),
                                            
                                            h3("Instructions"),
                                            
                                            p("1) Configure your options on the right side of the page."),
                                            
                                            p("2) Wait for the loading to finish - a green tick will appear next to the 'Start Analysing!' button when done."),
                                            
                                            p("3) Explore! Tweet data can be found in the second tab at the top of the page and sentiment data in the third."),
                                            
                                            h3("Tabs & Notes"),
                                            
                                            p("TSAF breaks down your analysis in 2 broad ways - firstly focusing on the tweets in
                                              the 'Tweet Analysis' tab and also the sentiments in the 'Sentiment Analysis' tab."),
                                            
                                            p(em("Tweet Analysis "), "covers how people interacted with the tweets and the contents of the tweets themselves. Here you can 
                                              find about the users tweeting, interaction stats, a tweet cloud and more!"),
                                            
                                            p(em("Sentiment Analysis "), "dives into the emotion behind the tweets. Here we can see breakdowns of the emotions
                                              and any difference in language and vocabulary used depending on the sentiment."),
                                            p("Sentiment score is a custom number designated by the ", em("syuzhet "), "package. The number is an indication of general sentiment,
                                              with 0 being a neutral score. 
                                              Positivity and negativity are indicated the further the number is from zero both up and down respectively."),
                                            
                                            p("Get in touch with any questions and comments but most of all, enjoy!")
                                            
                                    ),
                                    
                                    mainPanel(fluidRow(
                                            h1("Choose the keywords you want to explore:"),
                                            
                                            p("TSAF uses your query with the Twitter API and retrieves the", strong(" latest n "), 
                                              "tweets containing them."),
                                            
                                            p("Queries with more than one word will result in only tweets containing ", strong("all the words"),
                                              ". Multiple queries can be combined by seperating them with the 'OR' operator (e.g. 'Paul Walker OR Vin Diesel' to search for both). 
                                        Queries can also be hashtags."),
                                            
                                            div(
                                                    id = "plot-container",
                                                    tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                             id = "loading-spinner"),
                                                    wordcloud2Output("demo")
                                            ),
                                            p(em("Wordcloud of recent tweets relating to some famous people, can you guess who it is?.")),
                                            
                                            column(3, br(),
                                                   sliderInput("nTweets", label = h5("Number of tweets to sample: "),
                                                               min = 100, max = 1000, value = 250, step = 50),
                                                   helpText("NOTE: Larger scoops take longer to process - 250 is the recommended value.")
                                            ),
                                            column(4, offset = 1, br(),
                                                   textInput("query", h5("Keyword to analyse:"), value = "Paul Walker"),
                                                   br(),
                                                   
                                                   withBusyIndicatorUI(
                                                           actionButton("goButton", "Start analysing!", class = "btn-primary")
                                                   ),
                                                   
                                                   textOutput("errMsg")
                                                   
                                                   
                                            ),
                                            column(4, br(), br(),
                                                   checkboxInput("rts", "Check to include RTs", value = FALSE),
                                                   checkboxInput("htag", "Check to include hashtags in analysis", value = FALSE),
                                                   checkboxInput("rudeWords", "Check to include explicit words", value = TRUE),
                                                   helpText("NOTE: Excluding explicit words may affect sentiment values in analysis.")
                                            )
                                            
                                    ))
                            )
                   ),
                   
                   tabPanel("Tweet Analysis",
                            
                            tags$head(tags$style(HTML(mycss))),
                            
                            tabsetPanel(
                                    
                                    tabPanel("About",
                                             
                                             h1("Contents"),
                                             
                                             h3("Users"),
                                             
                                             h5("User Source"),
                                             p("This tab contains a graph and table showing the most 
                                            common channels through which people tweeted their respective tweets."),
                                             
                                             h5("User Descriptions (All, Positive, Neutral, Negative)"),
                                             p(em("Rtweet's "), em("search_tweets"), " function allows us to see certain aspects of user data of those whose tweets were pulled.
                                            Using ", em("syuzhet"), "'s ", em("get_nrc_sentiment"), " function, we can split the various users by the general sentiment of their tweets.
                                            we can see if any reoccuring elements come up in the people behind the tweets."),
                                             
                                             h3("Interaction"),
                                             
                                             h5("Distribution"),
                                             p("This tab will show you how the various measures of interactivity are distributed."),
                                             
                                             h5("Interaction vs. Sentiment Score"),
                                             p("Here you can see if any of the interactivity variables are affected by the sentiment of the tweet."),
                                             
                                             p(em("When setting axis limits:
                                                  1) Both boxes must be filled for the system to work.
                                                  2) Minimum values must be smaller than the maximum.")),
                                             
                                             h3("Time Series"),
                                             p("This tab contains a time series graph of the tweets and the configurations. By editing
                                            the time options you can see how the behaviour changes over time."),
                                             
                                             p("Twitter API only gives access to tweets from up to 6-8 days ago.", em("n "), "tweets are 
                                                   also taken from the most recent tweets so results may be more interpretable from sparser queries."),
                                             
                                             h3("Wordcloud"),
                                             p("This tab shows a wordcloud made from the most common words in the tweet pool. Please bear in mind that when people tweet, they 
                                               will use different spellings and acronyms to avoid the character limit.")
                                    ),
                                    
                                    tabPanel("Users",
                                             
                                             tabsetPanel(
                                                     
                                                     tabPanel("User Source",
                                                              
                                                              navlistPanel(
                                                                      
                                                                      tabPanel("Source Plot",
                                                                               div(
                                                                                       id = "plot-container",
                                                                                       tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                                id = "loading-spinner"),
                                                                                       plotOutput("userSource")),
                                                                               
                                                                               textInput("USTitle", "Enter plot title here:")
                                                                               
                                                                      ),
                                                                      
                                                                      tabPanel("Table", dataTableOutput("userSourceTbl"))
                                                                      
                                                                      )
                                                              
                                                     ),
                                                     
                                                     tabPanel("User Descriptions - All",
                                                     
                                                             navlistPanel(
                                                                     
                                                                     tabPanel("Wordcloud",
                                                                              div(
                                                                                      id = "plot-container",
                                                                                      tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                               id = "loading-spinner"),
                                                                                      wordcloud2Output("userDesc")
                                                                              )
                                                                              
                                                                     ),
                                                                     
                                                                     tabPanel("Table", dataTableOutput("userDescDT"))
                                                                     
                                                             )
                                                     
                                                     ),
                                                     
                                                     tabPanel("User Descriptions - Positive",
                                                              
                                                              navlistPanel(
                                                                      
                                                                      tabPanel("Wordcloud",
                                                                               div(
                                                                                       id = "plot-container",
                                                                                       tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                                id = "loading-spinner"),
                                                                                       wordcloud2Output("POSuserDesc")
                                                                               )
                                                                               ),
                                                                      tabPanel("Table",
                                                                               dataTableOutput("POSuserDescDT")
                                                                               )
                                                                      
                                                              )
                                                     ),
                                                     
                                                     tabPanel("User Descriptions - Neutral",
                                                              
                                                              navlistPanel(
                                                                      
                                                                      tabPanel("Wordcloud",
                                                                               div(
                                                                                       id = "plot-container",
                                                                                       tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                                id = "loading-spinner"),
                                                                                       wordcloud2Output("NEUuserDesc")
                                                                               )
                                                                      ),
                                                                      tabPanel("Table",
                                                                               dataTableOutput("NEUuserDescDT")
                                                                      )
                                                                      
                                                              )
                                                              
                                                     ),
                                                     
                                                     tabPanel("User Descriptions - Negative",
                                                              
                                                              navlistPanel(
                                                                      
                                                                      tabPanel("Wordcloud",
                                                                               div(
                                                                                       id = "plot-container",
                                                                                       tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                                id = "loading-spinner"),
                                                                                       wordcloud2Output("NEGuserDesc")
                                                                               )
                                                                      ),
                                                                      tabPanel("Table",
                                                                               dataTableOutput("NEGuserDescDT")
                                                                      )
                                                                      
                                                              )
                                                              
                                                     )
                                                     
                                                     
                                             )
                                             
                                    ),
                                    
                                    tabPanel("Interaction",
                                             
                                             tabsetPanel(
                                                     
                                                     tabPanel("Distribution",
                                                              
                                                              div(
                                                                      id = "plot-container",
                                                                      tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                               id = "loading-spinner"),
                                                                      plotOutput("rtFavDist")
                                                              ),
                                                              
                                                              splitLayout(
                                                                      textInput("distTitle", "Enter plot title here:"),
                                                                      sliderInput("bins", "Number of bins: ", min = 20, max = 150, value = 30, step = 10),
                                                                      htmlOutput("avgInt")
                                                              ),
                                                              
                                                              h4("Edit each axis below"),
                                                              
                                                              tabsetPanel(
                                                                      
                                                                      tabPanel("Retweets",
                                                                               
                                                                               column(5, 
                                                                                      splitLayout(
                                                                                              numericInput("RTdistCoordxMin", 
                                                                                                           "X: Minimum Value", NULL), 
                                                                                              numericInput("RTdistCoordxMax", 
                                                                                                           "X: Maximum Value", NULL)
                                                                                              )
                                                                                      ),
                                                                               
                                                                               column(5,
                                                                                      splitLayout(
                                                                                              numericInput("RTdistCoordyMin", 
                                                                                                           "Y: Minimum Value", NULL), 
                                                                                              numericInput("RTdistCoordyMax", 
                                                                                                           "Y: Maximum Value", NULL)
                                                                                              ), offset = 1
                                                                                      )
                                                                      ),
                                                                      
                                                                      tabPanel("Favorites",
                                                                               
                                                                               column(5, 
                                                                                      splitLayout(
                                                                                              numericInput("FAVdistCoordxMin", 
                                                                                                           "X: Minimum Value", NULL), 
                                                                                              numericInput("FAVdistCoordxMax", 
                                                                                                           "X: Maximum Value", NULL)
                                                                                      )
                                                                               ),
                                                                               
                                                                               column(5,
                                                                                      splitLayout(
                                                                                              numericInput("FAVdistCoordyMin", 
                                                                                                           "Y: Minimum Value", NULL), 
                                                                                              numericInput("FAVdistCoordyMax", 
                                                                                                           "Y: Maximum Value", NULL)
                                                                                      ), offset = 1
                                                                               )
                                                                      ),
                                                                      
                                                                      tabPanel("Followers",
                                                                               
                                                                               column(5, 
                                                                                      splitLayout(
                                                                                              numericInput("FLLWdistCoordxMin", 
                                                                                                           "X: Minimum Value", NULL), 
                                                                                              numericInput("FLLWdistCoordxMax", 
                                                                                                           "X: Maximum Value", NULL)
                                                                                      )
                                                                               ),
                                                                               
                                                                               column(5,
                                                                                      splitLayout(
                                                                                              numericInput("FLLWdistCoordyMin", 
                                                                                                           "Y: Minimum Value", NULL), 
                                                                                              numericInput("FLLWdistCoordyMax", 
                                                                                                           "Y: Maximum Value", NULL)
                                                                                      ), offset = 1
                                                                               )
                                                                               
                                                                      ),
                                                                      
                                                                      tabPanel("Total Interactions",
                                                                               
                                                                               column(5, 
                                                                                      splitLayout(
                                                                                              numericInput("TOTdistCoordxMin", 
                                                                                                           "X: Minimum Value", NULL), 
                                                                                              numericInput("TOTdistCoordxMax", 
                                                                                                           "X: Maximum Value", NULL)
                                                                                      )
                                                                               ),
                                                                               
                                                                               column(5,
                                                                                      splitLayout(
                                                                                              numericInput("TOTdistCoordyMin", 
                                                                                                           "Y: Minimum Value", NULL), 
                                                                                              numericInput("TOTdistCoordyMax", 
                                                                                                           "Y: Maximum Value", NULL)
                                                                                      ), offset = 1
                                                                               )
                                                                               
                                                                      )
                                                                      
                                                              ),
                                                              
                                                              column(5, actionButton("resetAxesDist", "Reset Axes"))
                                                              
                                                     ),
                                                     
                                                     tabPanel("Interaction vs. Sentiment Score",
                                                              
                                                              div(
                                                                      id = "plot-container",
                                                                      tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                               id = "loading-spinner"),
                                                                      plotOutput("intSent")
                                                              ),
                                                              
                                                              textInput("sentTitle", "Enter plot title here:"), br(),
                                                              
                                                              h4("Edit each axis below"),
                                                              
                                                              tabsetPanel(
                                                                      
                                                                      tabPanel("Retweets",
                                                                               
                                                                               column(5, 
                                                                                      splitLayout(
                                                                                              numericInput("RTminXsent", 
                                                                                                           "X: Minimum Value", NULL), 
                                                                                              numericInput("RTmaxXsent", 
                                                                                                           "X: Maximum Value", NULL)
                                                                                      )
                                                                               ),
                                                                               
                                                                               column(5,
                                                                                      splitLayout(
                                                                                              numericInput("RTminYsent", 
                                                                                                           "Y: Minimum Value", NULL), 
                                                                                              numericInput("RTmaxYsent", 
                                                                                                           "Y: Maximum Value", NULL)
                                                                                      ), offset = 1
                                                                               )
                                                                      ),
                                                                      
                                                                      tabPanel("Favorites",
                                                                               
                                                                               column(5, 
                                                                                      splitLayout(
                                                                                              numericInput("FAVminXsent", 
                                                                                                           "X: Minimum Value", NULL), 
                                                                                              numericInput("FAVmaxXsent", 
                                                                                                           "X: Maximum Value", NULL)
                                                                                      )
                                                                               ),
                                                                               
                                                                               column(5,
                                                                                      splitLayout(
                                                                                              numericInput("FAVminYsent", 
                                                                                                           "Y: Minimum Value", NULL), 
                                                                                              numericInput("FAVmaxYsent", 
                                                                                                           "Y: Maximum Value", NULL)
                                                                                      ), offset = 1
                                                                               )
                                                                               
                                                                      ),
                                                                      
                                                                      tabPanel("Total Interactions",
                                                                               
                                                                               column(5, 
                                                                                      splitLayout(
                                                                                              numericInput("TOTminXsent", 
                                                                                                           "X: Minimum Value", NULL), 
                                                                                              numericInput("TOTmaxXsent", 
                                                                                                           "X: Maximum Value", NULL)
                                                                                      )
                                                                               ),
                                                                               
                                                                               column(5,
                                                                                      splitLayout(
                                                                                              numericInput("TOTminYsent", 
                                                                                                           "Y: Minimum Value", NULL), 
                                                                                              numericInput("TOTmaxYsent", 
                                                                                                           "Y: Maximum Value", NULL)
                                                                                      ), offset = 1
                                                                               )
                                                                               
                                                                      )
                                                                      
                                                              ),
                                                              
                                                              column(5, actionButton("resetAxesSent", "Reset Axes"))
                                                              
                                                              
                                                     )
                                                     
                                             )
                                             
                                             
                                             
                                    ),
                                    
                                    tabPanel("Time Series",
                                             
                                             div(
                                                     id = "plot-container",
                                                     tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                              id = "loading-spinner"),
                                                     plotOutput("ts")
                                             ),
                                             
                                             column(5, 
                                                    textInput("TSTitle", "Enter plot title here:"),
                                                    p(h5("Choose sentiments to plot against: ")),
                                                    splitLayout(
                                                            checkboxInput("posSent", "Positive ( > 0 )", value = FALSE),
                                                            checkboxInput("neuSent", "Neutral ( = 0 )", value = FALSE),
                                                            checkboxInput("negSent", "Negative ( < 0 )", value = FALSE))
                                                    ),
                                             
                                             column(6, 
                                                    p(h5("Choose time settings: ")),
                                                    splitLayout(
                                                            radioButtons(
                                                                    "time",
                                                                    choices = c("Seconds", "Minutes", "Hours", "Days"),
                                                                    label = NULL),
                                                            textInput("number", value = c("30"), label = NULL)
                                                     )
                                    )),
                                    
                                    tabPanel("Wordcloud",
                                             
                                             navlistPanel(
                                                     
                                                     tabPanel("Wordcloud",
                                                              div(
                                                                      id = "plot-container",
                                                                      tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                               id = "loading-spinner"),
                                                                      wordcloud2Output("wordcloud")
                                                              )
                                                     ),
                                                     
                                                     tabPanel("Table",
                                                              dataTableOutput("wordcloudDT")
                                                     )
                                                     
                                             )
                                    )
                            )
                            
                   ),
                   
                   tabPanel("Sentiment Analysis",
                            
                            tags$head(tags$style(HTML(mycss))),
                            
                            tabsetPanel(
                                    
                                    tabPanel("About",
                                             
                                             h1("Contents"),
                                             
                                             h3("Emotions Breakdown"),
                                             
                                             p("This tab disects the general feeling that you can draw from the tweets. Seeing how 
                                               tweets are some of the latest regarding your query, we can use this to glimpse what
                                                people are thinking right now."),
                                             
                                             h5("Counts"),
                                             p("By generalising each tweet by its sentiment score, we can see how many tweets fall in each sentiment
                                               and see how individuals are feeling towards the topic."),
                                             
                                             h5("Point Tallies"),
                                             p("Point tallies gets further emotion data from the tweets and shows the sentiment of the sample as a whole.
                                               Values for a range of emotions are calculated using language analysis and then those values are assigned as points to the tweet.
                                               By tallying up these points, we can see how the sample feels specifically."),
                                             
                                             p("Please bear in mind points aren't limited per tweet - polarising tweets will have high point totals in certain emotions 
                                               which are then indiscriminately totaled. In smaller sample sizes, a small amount of highly charged tweets may unfairly skew results."),
                                             
                                             h3("Wordclouds by Sentiment - Positive, Negative, Neutral"),
                                             p("Here we can see the language used by our users, depending on the sentiment of the tweet.
                                               Do those who are tweeting negatively use different language to neutral tweets? These are the types of question
                                                you can answer here.")
                                             
                                             
                                             ),
                                    
                                    tabPanel("Emotions Breakdown",
                                             
                                             tabsetPanel(
                                                     
                                                     tabPanel("Counts",
                                                              
                                                              navlistPanel(
                                                                      
                                                                      tabPanel("Plot",
                                                                              div(id = "plot-container",
                                                                                  tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                           id = "loading-spinner"),
                                                                                  plotOutput("emotionCount")
                                                                                  ),
                                                                              
                                                                              column(5, 
                                                                                     # textOutput("sent_score"),
                                                                                     textInput("emotionCountTitle", "Enter plot title here:"))
                                                                              
                                                                      ),
                                                                      
                                                                      tabPanel("Table",
                                                                               
                                                                               tableOutput("emotionCountTbl")
                                                                               
                                                                               )
                                                                      
                                                              )
                                                              
                                                              ),
                                                     
                                                     tabPanel("Point Tallies",
                                                             
                                                             navlistPanel(
                                                                     
                                                                     tabPanel("Plot",
                                                                              div(id = "plot-container",
                                                                                  tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                           id = "loading-spinner"),
                                                                                  plotOutput("emotions")
                                                                              ),
                                                                              
                                                                              column(5, textInput("emotionsTitle", "Enter plot title here:"))
                                                                              
                                                                     ),
                                                                     
                                                                     tabPanel("Table",
                                                                              
                                                                              tableOutput("emotionTbl")
                                                                              
                                                                     )
                                                                     
                                                             )
                                                             
                                                             )
                                                     
                                                     )
                                    ),
                                    
                                    tabPanel("Wordclouds by Sentiment",
                                            
                                            tabsetPanel(
                                            
                                                    tabPanel("Positive Tweets",
                                                     
                                                             navlistPanel(
                                                                     
                                                                     tabPanel("Wordcloud",
                                                                              div(
                                                                                      id = "plot-container",
                                                                                      tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                               id = "loading-spinner"),
                                                                                      wordcloud2Output("POSwordcloud")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("Table",
                                                                              dataTableOutput("POSwordcloudDT")
                                                                     )
                                                                     
                                                             )
                                                             
                                                    ),
                                                    tabPanel("Neutral Tweets",
                                                             
                                                             navlistPanel(
                                                                     
                                                                     tabPanel("Wordcloud",
                                                                              div(
                                                                                      id = "plot-container",
                                                                                      tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                               id = "loading-spinner"),
                                                                                      wordcloud2Output("NEUwordcloud")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("Table",
                                                                              
                                                                              dataTableOutput("NEUwordcloudDT")
                                                                     )
                                                                     
                                                             )
                                                             
                                                    ),
                                                    tabPanel("Negative Tweets",
                                                             
                                                             navlistPanel(
                                                                     
                                                                     tabPanel("Wordcloud",
                                                                              div(
                                                                                      id = "plot-container",
                                                                                      tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                                                                               id = "loading-spinner"),
                                                                                      wordcloud2Output("NEGwordcloud")
                                                                              )
                                                                     ),
                                                                     
                                                                     tabPanel("Table",
                                                                              dataTableOutput("NEGwordcloudDT")
                                                                     )
                                                                     
                                                             )
                                                    )))
                            )
                            
                   ),
                   
                   tabPanel("Summary Panel", 
                            
                            tags$head(tags$style(HTML(mycss))),
                            
                            h2("Quick View of Tweets Regarding Your Query:"),
                            
                            div(
                                    id = "plot-container",
                                    tags$img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/plot-spinner/www/spinner.gif",
                                             id = "loading-spinner"),
                                    plotOutput("avgSentStats")
                            ),
                            
                            br(),
                            
                            h4("Average Figures: "),
                            
                            dataTableOutput("avgWPT")
                   ),
                   
                   tabPanel("Download the Data",
                            
                            downloadButton("dl_data", "Download the Data"),
                            
                            p(),
                            
                            dataTableOutput("data_pull")
                            
                            )
                   
                   )
                   
        
)



server <- function(input, output, session){
        
        #end session on quit
        session$onSessionEnded(stopApp)
        
        ##############################
        #### GET AND FORMAT DATA ####
        ##############################
        
        #Demo wordcloud generation
        output$demo <- renderWordcloud2({
                
                print("Start demo")
                
                #Potential wordcloud topics
                qChoices <- c("Donald Trump", "Mo Salah", "Jose Mourinho", "Kevin De Bruyne", "Tom Cruise",
                              "Conor McGregor", "Cristiano Ronaldo", "Theresa May", "Anne McElvoy", "Serena Williams",
                              "Barack Obama")
                
                #potential shapes
                shape <- c("circle", "diamond", "pentagon", "star")
                
                #font formatting
                fontW <- c("bold", "normal")
                
                #colors
                colour <- c("#1DA1F2", "#78C0E0", "#820933", "#820933", "#FDCA40", "#035E5B", "#9A95BF",
                            "#A0DDE6", "#EFA00B", "#D99AC5", "#B37BA4", "#DCCDE8", "#1DA1F2", "#1DA1F2")
                
                #fonts
                ff <- c("Arial", "Calibri", "Cambria", "Didot", "Garamond", "Georgia",
                        "Helvetica", "Century Gothic", "Helvetica Neue")
                
                #randomly choose variables
                
                #topic
                n <- sample(1:length(qChoices), 1, FALSE) 
                
                #font
                x <- sample(1:length(ff), 1, FALSE)
                
                #shape
                y <- sample(1:length(shape), 1, FALSE)
                
                #font
                z <- sample(1:length(fontW), 1, FALSE)
                
                #find tweets for demo wordcloud
                demoTweets <- search_tweets(qChoices[n], n = 150, lang = "en", token = twitter_token)
                
                #clean the tweets
                demoTweets <- clean_tweets(plain_tweets(demoTweets$text),
                                           re_exp(leave = ifelse(input$htag, c("hashtags"), ""))
                )
                
                # demoTweets <- clean_tweets(plain_tweets(demoTweets$text), 
                #                            re_exp("")
                # )
                
                #get color vector
                colour <- rep(colour, times = 100)
                
                print("End Demo")
                
                #produce wordcloud
                wcGen(demoTweets, 2, color = colour, fontWeight = fontW[z], size = 1, explicit = FALSE,
                      minRotation = -pi/2, maxRotation = -pi/2, fontFamily = ff, query = "")[[1]]
                
        })
        
        #When user presses go:
        observeEvent(input$goButton, {
                
                print("Go Button Pressed")
                ##############################
                ####### CONFIGURATIONS #######
                ##############################
                
                #show "Busy" indicator while working
                withBusyIndicatorServer("goButton", {
                        
                        #USE API TO GET TWEETS
                        
                        print("USE API TO GET TWEETS")
                        
                        #get tweet data
                        tweets <- search_tweets(input$query, n = input$nTweets, 
                                                include_rts = ifelse(input$rts == TRUE, TRUE, FALSE), 
                                                lang = "en", token = twitter_token)
                        
                        print("'Tweets' created")
                        
                        #EXTRACT TWEETS TEXT FOR SENTIMENT ANALYSIS
                        
                        print("EXTRACT TWEETS TEXT FOR SENTIMENT ANALYSIS")
                        
                        #clean the tweets & remove unwanted 
                        txt <- clean_tweets(
                                plain_tweets(tweets$text),
                                re_exp(leave = ifelse(input$htag, c("hashtags"), ""))
                        )
                        
                        print("'txt' created")
                        
                        #SENTIMENT ANALYSIS
                        
                        print("Start basic sentiment analysis")
                        
                        #get sentiment data and map with sentiment data
                        emo <- cbind(txt,
                                     get_nrc_sentiment(as.vector(txt)),
                                     get_sentiment(txt))
                        
                        print("'emo Made")
                        
                        #ensure data types
                        emo$txt <- as.character(emo$txt)
                        
                        #rename for clarity
                        names(emo)[1] <- "tweet.text" ; names(emo)[12] <- "sent.score"
                        
                        print("emo created")
                        
                        #drop unwanted columns
                        emotion.df <- emo[,-c(1, 12)]
                        
                        #put sentiment numbers in a vector
                        s <- unlist(lapply(emotion.df, sum))
                        
                        #put sentiment score totals in seperate df
                        scores <- data.frame(emotion = names(emotion.df),
                                             total = s)
                        
                        print("scores calculated")
                        
                        #take off row names
                        rownames(scores) <- NULL
                        
                        print("Preprocess done")
                })
                
                ##############################
                ######### TWEETS TAB #########
                ##############################
                
                #INTERACTION TAB
                print("Start wrangling")
                
                #retrieve relevant info
                int <- tweets %>% 
                        select(c(text, created_at, retweet_count, favorite_count, followers_count))
                
                #ensure text is formatted
                int$text <- txt
                
                #compute total interactions
                int$total_interactions <- int$retweet_count + int$favorite_count
                
                #get averages for interactions - RT, Fav, Total
                avgRT <- round(mean(int$retweet_count), 2) ; avgFav <- round(mean(int$favorite_count), 2)
                avgTot <- round(mean(int$total_interactions), 2)
                
                #reformat int
                int <- as.tibble(cbind(int, emo[,"sent.score"])) ; names(int)[7] <- "sent.score"
                print("Output int")
                
                #in case of reset button clicked - distributions
                observeEvent(input$resetAxesDist, {
                        
                        print("Distribution Axis reset start")
                        
                        #reset all of the axes limits
                        reset("RTdistCoordyMax") ; reset("RTdistCoordyMin") ; reset("RTdistCoordxMax") ; reset("RTdistCoordxMin")
                        reset("FAVdistCoordyMax") ; reset("FAVdistCoordyMin") ; reset("FAVdistCoordxMax") ; reset("FAVdistCoordxMin")
                        reset("TOTdistCoordyMax") ; reset("TOTdistCoordyMin") ; reset("TOTdistCoordxMax") ; reset("TOTdistCoordxMin")
                        
                        print("Distribution Axis reset done")
                        reset("FLLWdistCoordyMax") ; reset("FLLWdistCoordyMin") ; reset("FLLWdistCoordxMax") ; reset("FLLWdistCoordxMin")
                        
                })
                
                #distribution of retweets
                output$rtFavDist <- renderPlot({
                        
                        print("Start interaction dist plots using int and axes input")
                        print("End interaction dist plots using int and axes input")
                        
                        #see helper.R to see function
                        interaction_dist_plots(df = int, 
                                               RTmaxY = input$RTdistCoordyMax, RTminY = input$RTdistCoordyMin, 
                                               RTmaxX = input$RTdistCoordxMax, RTminX = input$RTdistCoordxMin,
                                               FAVmaxY = input$FAVdistCoordyMax, FAVminY = input$FAVdistCoordyMin, 
                                               FAVmaxX = input$FAVdistCoordxMax, FAVminX = input$FAVdistCoordxMin,
                                               TOTmaxY = input$TOTdistCoordyMax, TOTminY = input$TOTdistCoordyMin, 
                                               TOTmaxX = input$TOTdistCoordxMax, TOTminX = input$TOTdistCoordxMin,
                                               FLLWmaxY = input$FLLWdistCoordyMax, FLLWminY = input$FLLWdistCoordyMin, 
                                               FLLWmaxX = input$FLLWdistCoordxMax, FLLWminX = input$FLLWdistCoordxMin,
                                               plot.title = input$distTitle, bins = input$bins)
                })
                
                #display average interaction figures
                output$avgInt <- renderUI({
                        
                        print("Computing avg interaction values")
                        
                        #create titles
                        RT <- paste("Average number of retweets recieved", as.character(avgRT))
                        FAV <- paste("Average number of favorites recieved", as.character(avgFav))
                        TOT <- paste("Average number of interactions recieved", as.character(avgTot))
                        
                        print("Output avg interaction values")
                        
                        #return figures
                        HTML(paste(
                                strong("Average Figures:"), paste(RT, FAV, TOT, sep = "<br/>"),
                                sep = "<br/>")
                             )
                })
                
                #in case of reset button clicked - sentiment
                observeEvent(input$resetAxesSent, {
                        
                        print("Sentiment axes reset start")
                        
                        #reset all of the axes limits
                        reset("RTmaxYsent") ; reset("RTminYsent") ; reset("RTmaxXsent") ; reset("RTminXsent")
                        reset("FAVmaxYsent") ; reset("FAVminYsent") ; reset("FAVmaxXsent") ; reset("FAVminXsent")
                        print("Sentiment axes reset end")
                        reset("TOTmaxYsent") ; reset("TOTminYsent") ; reset("TOTmaxXsent") ; reset("TOTminXsent")
                })
                
                #interactions vs sentiment plot
                output$intSent <- renderPlot({
                        
                        print("Start interaction sent plots using int and axes input")
                        print("End interaction sent plots using int and axes input")
                        
                        #see helper.R
                        sent_int_plots(int,
                                       RTmaxY = input$RTmaxYsent, RTminY = input$RTminYsent,
                                       RTmaxX = input$RTmaxXsent, RTminX = input$RTminXsent,
                                       FAVmaxY = input$FAVmaxYsent, FAVminY = input$FAVminYsent, 
                                       FAVmaxX = input$FAVmaxXsent, FAVminX = input$FAVminXsent,
                                       TOTmaxY = input$TOTmaxYsent, TOTminY = input$TOTminYsent, 
                                       TOTmaxX = input$TOTmaxXsent, TOTminX = input$TOTminXsent,
                                       plot.title = input$sentTitle)
                        
                })
                
                #time series plot
                output$ts <- renderPlot({
                        
                        print("Start time series")
                        
                        #add sentiment score to the df
                        tweets2 <- cbind(tweets, get_sentiment(txt)) ; names(tweets2)[88] <- "sent.score"
                        
                        print("tweets2 finished, start plot")
                        
                        #see helper.R
                        ts_plt <- time_series_tweets(tweetData = tweets2,
                                                     time = input$time,
                                                     number = input$number,
                                                     query = input$query,
                                                     inc.sent = c(input$posSent,
                                                                  input$neuSent,
                                                                  input$negSent),
                                                     plot.title = input$TSTitle)
                        
                        print("output plot")
                        ts_plt
                })
                
                ##general wordcloud
                
                #extract text for wc
                WCtext <- emo %>% select(tweet.text) 
                
                print("WCtext done")
                
                #create gen wc & df
                wc <- wcGen(WCtext, 2, size = 1, explicit = input$rudeWords,
                            minRotation = -pi/2, maxRotation = -pi/2, minSize = (input$nTweets * 0.02), query = input$query)
                
                print("Wordcloud made")
                
                #tweet wordcloud
                output$wordcloud <- renderWordcloud2({
                        
                        print("General Wordcloud start")
                        
                        #get cloud
                        wc[[1]]

                })
                
                #return wordcloud as df
                output$wordcloudDT <- renderDataTable({
                        
                        print("Output DF")
                        
                        #get table
                        wc[[2]]
                        
                        
                }, options = list(pageLength = 5))
                
                #USERS TAB
                print("Create user df")
                
                #retrieve user info - source & user description
                user.df <- tweets %>% select(source, description)
                
                #add on sentiment score
                user.df <- cbind(user.df, emo$sent.score) ; names(user.df)[3] <- "sent.score"
                
                #clean description user helper functions
                user.df$description <- clean_tweets(user.df$description, re_exp(leave = ifelse(input$htag, c("hashtags"), "")))
                
                #tally up source counts
                s2 <- as.data.frame(table(user.df$source)) ; names(s2) <- c("Source", "Count")
                print("Output user df & s2")
                
                #start description wordcloud
                print("General description wordcloud")
                
                #extract description texts
                descWc <- user.df$description
                print("Description text made")
                
                #create table and worcloud
                Dwc <- wcGen(descWc, 2, size = 0.25, explicit = input$rudeWords, minSize = (input$nTweets * 0.02), query = input$query)
                print("Description wordcloud/table made")
                
                #description wordcloud
                output$userDesc <- renderWordcloud2({
                        
                        print("output description wc")
                        
                        #extract wordcloud
                        Dwc[[1]]
                        
                })
                
                #description table
                output$userDescDT <- renderDataTable({
                        
                        print("Get description table")
                        
                        #extract table
                        Dwc[[2]]
                        
                }, options = list(pageLength = 5))
                
                print("positive descriptions start")
                
                #extract positive descriptions using sent.score
                POSdescWc <- user.df[user.df$sent.score > 0, 2]
                
                print("start making positive wc")
                
                #make positive wordcloud & table
                POSDwc <- wcGen(POSdescWc, 2, size = 0.3, explicit = input$rudeWords, minSize = (input$nTweets * 0.02), query = input$query)
                
                #wordcloud for positive tweets' user descriptions
                output$POSuserDesc <- renderWordcloud2({
                        
                        print("output positivewc")
                        
                        #extract wordcloud
                        POSDwc[[1]]
                        
                })
                
                #df for positive tweets' user descriptions
                output$POSuserDescDT <- renderDataTable({
                        
                        print("pos user desc DF")
                        
                        #extract table
                        POSDwc[[2]]
                        
                        
                }, options = list(pageLength = 5))
                
                #Neutral desc
                print("NEU user desc")
                
                #extract neutral tweeets' user descriptions
                print("Get neutral descriptions")
                NEUdescWc <- user.df[user.df$sent.score == 0, 2]
                
                #create wordcloud & table
                NEUDwc <- wcGen(NEUdescWc, 2, size = 0.3, explicit = input$rudeWords,
                                minSize = (input$nTweets * 0.02), query = input$query)
                
                #extract wordcloud
                output$NEUuserDesc <- renderWordcloud2({
                        
                        print("Neutral wordcloud")
                        
                        NEUDwc[[1]]
                        
                })
                
                #extract tables
                output$NEUuserDescDT <- renderDataTable({
                        
                        print("Neutral table")
                        
                        NEUDwc[[2]]
                        
                }, options = list(pageLength = 5))
                
                #Negative tweets' user description
                
                NEGdescWc <- user.df[user.df$sent.score < 0, 2]
                
                #create wordcloud & table
                NEGDwc <- wcGen(NEGdescWc, 2, size = 0.3, explicit = input$rudeWords,
                                minSize = (input$nTweets * 0.02), query = input$query)
                
                #extract wordcloud
                output$NEGuserDesc <- renderWordcloud2({
                        
                        print("Negative wordcloud")
                        
                        #extract cloud
                        NEGDwc[[1]]
                        
                })
                
                #extract tables
                output$NEGuserDescDT <- renderDataTable({
                        
                        print("Negative table")
                        
                        #extract table
                        NEGDwc[[2]]
                        
                }, options = list(pageLength = 5))
                
                #user source graph
                output$userSource <- renderPlot({
                        
                        #compute title
                        print("User source df & graph start")
                        pltTitle <- ifelse(input$USTitle == "", "Which Clients are People Using to Tweet?",
                                           input$USTitle)
                        
                        #plot graph
                        print("User source df & graph output")
                        ggplot(s2[s2$Count > 5,], aes(x = reorder(Source, -Count), y = Count)) + 
                                geom_bar(stat = "identity", fill = randCol(), alpha = 0.9) +
                                labs(x = "Source", y = "Count", title = pltTitle) +
                                geom_hline(yintercept = 0) +
                                theme(axis.text.x = element_text(size = 10, angle = 20, vjust = 0.5))
                        
                })
                
                #user source table
                output$userSourceTbl <- renderDataTable({
                        
                        print("User source df & graph table only")
                        
                        #arrange table
                        s2 %>% arrange(desc(Count))
                        
                }, options = list(pageLength = 5))
                
                ##############################
                ########## SENT TAB ##########
                ##############################
                
                print("Starting Emo Count - DF done")
                
                #clone df for extra actions 
                emoCount <- emo 
                
                #nested ifelse to classify sentiment
                emoCount$gen_sent <- ifelse(emo$sent.score > 0, "Positive",
                                            ifelse(emo$sent.score == 0, "Neutral",
                                                   "Negative"))
                
                print("Starting Emo Count - DF done")
                
                #compute plot title
                pltTitle <- ifelse(input$emotionCountTitle == "", "Number of Tweets by General Sentiment",
                                   input$emotionCountTitle)
                
                #aggregate numbers into df
                tallies <- as.data.frame(table(emoCount$gen_sent)) %>% arrange(desc(Freq)) ; names(tallies) <- c("Sentiment", "Count")
                
                #Emotion tally plot
                output$emotionCount <- renderPlot({
                        
                        #make and return graph
                        tot <- ggplot(tallies, aes(reorder(Sentiment, -Count), Count)) + 
                                geom_bar(stat = "identity", fill = randCol()) +
                                labs(x = "General Sentiment", y = "Tweet Count", title = pltTitle) +
                                geom_hline(yintercept = 0) + 
                                scale_y_continuous(breaks = seq(0, roundUp(max(tallies$Count)), roundUp(max(tallies$Count)) / 10))
                        
                        print("Starting Emo Count - Plot done")
                        
                        tot
                        
                })
                
                #emotion tally table
                output$emotionCountTbl <- renderTable({
                        
                        print("Starting Emo CountTBL - tbl done")
                        tallies
                })
                
                #emotion breakdown
                
                print("Start emotions plot")
                print("Start emotions df")
                
                #extract needed sentiment & emotion scores
                emoCounts <- scores[1:8,] ; sentCounts <- scores[9:10,]
                
                #assign labels
                emoCounts$emotion <- as.character(emoCounts$emotion) ; sentCounts$emotion <- as.character(sentCounts$emotion)
                emoCounts$emotion <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust")
                sentCounts$emotion <- c("Negative", "Positive")
                names(emoCounts) <- c("Emotion", "Count") ; names(sentCounts) <- c("Emotion", "Count")
                
                #plot
                output$emotions <- renderPlot({
                        
                        print("start emotions graphs a & b")
                        
                        #emotion plot
                        a <- ggplot(emoCounts, aes(x = reorder(Emotion, -Count), y = Count)) + 
                                geom_bar(stat = "identity", fill = randCol()) +
                                labs(x = "Emotion", y = "Points") + 
                                geom_hline(yintercept = 0) + 
                                theme(axis.text.x = element_text(size = 10, angle = 20, vjust = 0.5))
                                
                        
                        print("a done")
                        
                        #sentiment plot
                        b <- ggplot(sentCounts, aes(x = reorder(Emotion, -Count), y = Count)) + 
                                geom_bar(stat = "identity", fill = randCol()) +
                                labs(x = "Emotion", y = "Points") + 
                                geom_hline(yintercept = 0)
                        
                        print("b done")
                        
                        print("plt title start")
                        
                        #compute title
                        pltTitle <- ifelse(input$emotionsTitle == "", "Point Tallies of each Emotion in the Tweets",
                                          input$emotionsTitle)
                        print("Output final graph")
                        
                        #output graph
                        grid.arrange(a, b, ncol = 2, top = pltTitle)
                        
                })
                
                #table
                output$emotionTbl <- renderTable({
                        
                        #aggregate numbers
                        fin <- rbind(
                                sentCounts %>% arrange(desc(Count)),
                                emoCounts %>% arrange(desc(Count))
                                )
                        
                        #ensure data type
                        fin$Count <- as.integer(fin$Count)
                        print("output emotions df")
                        
                        #output table
                        fin
                        
                        
                        
                })
                
                #positive tweets
                
                #extract tweets
                
                print("Positive wordcloud cloud start")
                pSentTweets <- emo %>% filter(sent.score > 0)
                
                print("Positive wordcloud cloud - get tweets & create cloud")
                
                #create cloud & table
                pwc <- wcGen(pSentTweets$tweet.text, 2, size = 0.3, explicit = input$rudeWords,
                             minRotation = -pi/2, maxRotation = -pi/2, minSize = (input$nTweets * 0.02), query = input$query)
                print("Positive wordcloud cloud end")
                
                #extract cloud
                output$POSwordcloud <- renderWordcloud2({
                        
                        #extract cloud
                        pwc[[1]]
                        
                })
                
                #table
                output$POSwordcloudDT <- renderDataTable({
                        
                        #extract table
                        pwc[[2]]
                        
                }, options = list(pageLength = 5))
                
                #neutral tweets
                
                print("Neutral wordcloud cloud start")
                print("Neutral wordcloud get tweets ")
                
                #extract neutral tweets
                neuSentTweets <- emo %>% filter(sent.score == 0)
                print("Neutral wordcloud cloud output")
                
                #create WC & table
                NEUwc <- wcGen(neuSentTweets$tweet.text, 2, size = 0.3, explicit = input$rudeWords,
                               minRotation = -pi/2, maxRotation = -pi/2, minSize = (input$nTweets * 0.02), query = input$query)
                
                #wordcloud
                output$NEUwordcloud <- renderWordcloud2({
                        
                        #extract wordcloud
                        NEUwc[[1]]
                        
                })
                
                #table
                output$NEUwordcloudDT <- renderDataTable({
                        
                        #extract table
                        NEUwc[[2]]
                        
                        
                }, options = list(pageLength = 5))
                
                #negative tweets
                print("Neutral wordcloud df start")
                
                #extract negative tweets
                negSentTweets <- emo %>% filter(sent.score < 0)
                print("Neutral wordcloud df get tweets")
                
                #create worldcloud & table
                NEGwc <- wcGen(negSentTweets$tweet.text, 2, size = 0.3, explicit = input$rudeWords,
                               minRotation = -pi/2, maxRotation = -pi/2, minSize = (input$nTweets * 0.02), query = input$query)
                print("Neutral wordcloud df end")
                
                #wordcloud
                output$NEGwordcloud <- renderWordcloud2({
                        
                        #extract wordcloud
                        NEGwc[[1]]
                        
                })
                
                #table
                output$NEGwordcloudDT <- renderDataTable({
                        
                        #extract table
                        NEGwc[[2]]
                        
                        
                }, options = list(pageLength = 5))
                
                ##############################
                ######## SUMMARY TAB #########
                ##############################
                
                #average number of words in a tweet (to nearest int)
                
                output$avgWPT <- renderDataTable({
                        
                        #get avg words of all tweets
                        avg <- round(mean(sapply(txt, wordcount)))
                        
                        #avg retweets
                        RT <- round(avgRT)
                        
                        #avg favs
                        FAV <- round(avgFav)
                        
                        #avg tot int
                        TOT <- round(avgTot)
                        
                        #avg sentiment
                        avgSent <- round(mean(emo$sent.score), 2)
                        
                        data.frame(Statistic = c("Words per Tweet", "Retweets", "Favourites", "Total Interactions", "Sentiment Score"),
                                   Average = c(avg, RT, FAV, TOT, avgSent))
                        
                }, options = list(pageLength = 5, dom = "t"))
                
                #avg number of points of each sent/emotion in a tweet
                output$avgSentStats <- renderPlot({
                        
                        ##AVG SCORES
                        # / all sums by n tweets to get average scores
                        tots <- data.frame(Emotion = Hmisc::capitalize(names(s)), score = s / input$nTweets) ; rownames(tots) <- NULL
                        
                        #emotion
                        a <- ggplot(tots[-c(9,10),], aes(x = reorder(Emotion, -score), y = score)) +
                                geom_bar(stat = "identity", fill = randCol(8)) +
                                labs(x = "Emotion", y = "Average Points per Tweet") +
                                geom_hline(yintercept = 0) + 
                                scale_y_continuous(breaks = seq(0, ceiling(max(tots$score)), by = ceiling(max(tots$score)) / 10)) +
                                theme(axis.text.x = element_text(size = 10, angle = 20, vjust = 0.5))
                                
                        
                        #sent
                        b <- ggplot(tots[c(9,10),], aes(x = reorder(Emotion, -score), y = score)) +
                                geom_bar(stat = "identity", fill = randCol(2)) +
                                labs(x = "Sentiment", y = "Average Points per Tweet") +
                                geom_hline(yintercept = 0) + 
                                scale_y_continuous(breaks = seq(0, ceiling(max(tots$score)), by = ceiling(max(tots$score)) / 10))
                        
                        ##TALLIES
                        
                        #initilise empty df
                        tallies <- data.frame(Emotion = capitalize(names(s)), tally = 0)
                        
                        #prepare base df
                        emo2 <- emo[,-1] ; emo2$sent <- ifelse(emo2$sent.score < 0, "Negative",
                                                               ifelse(emo2$sent.score == 0, "Neutral",
                                                                      "Positive"))
                        
                        #get df for second graph
                        tally2 <- transform(table(emo2$sent)) ; names(tally2) <- c("Sentiment", "Tally")
                        
                        #iterate through the rows and cols and add onto tally dependng on the score
                        #rows
                        for(i in 1:nrow(emo2)){
                                
                                #cols
                                for(n in 1:10){
                                        
                                        #check
                                        if(emo2[i,n] > 0){
                                                
                                                #add onto tally
                                                tallies[tallies$Emotion == capitalize(names(s)[n]), 2] = tallies[tallies$Emotion == capitalize(names(s)[n]), 2] + 1
                                        }
                                        
                                }
                                
                        }
                        
                        #Tallies graphs
                        #emotions
                        c <- ggplot(tallies[-c(9, 10),], aes(x = reorder(Emotion, -tally), y = tally)) +
                                geom_bar(stat = "identity", fill = randCol(8))+
                                labs(x = "Emotion", y = "Tally") +
                                geom_hline(yintercept = 0) + 
                                scale_y_continuous(breaks = seq(0, ceiling(max(tallies$tally)), by = ceiling(max(tallies$tally) / 10))) +
                                theme(axis.text.x = element_text(size = 10, angle = 20, vjust = 0.5))
                        
                        #sentiment
                        d <- ggplot(tally2, aes(x = reorder(Sentiment, -Tally), y = Tally)) +
                                geom_bar(stat = "identity", fill = randCol(3)) +
                                labs(x = "Sentiment", y = "Tally") +
                                geom_hline(yintercept = 0) + 
                                scale_y_continuous(breaks = seq(0, ceiling(max(tally2$Tally)), by = ceiling(max(tally2$Tally) / 10)))
                        
                        #avg
                        one <- grid.arrange(a, b, ncol = 2, top = "Average Points per Tweet for each Emotion and Sentiment")
                        
                        #tallies
                        two <- grid.arrange(c, d, ncol = 2, top = "Number of Tweets Demonstrating each Emotion and Sentiment")
                        
                        grid.arrange(one, two, nrow = 2)
                        
                })
                
                #Data Pull
                
                #clone df and clean text
                tweets_raw <- tweets ; tweets_raw[,5] <- txt
                
                #add on emo values
                tweets_raw <- cbind(tweets_raw[,5] ,emo[,-1], tweets_raw[,-5])
                
                output$data_pull <- renderDataTable({
                        
                        #clone df
                        tweets_raw
                        
                }, options = list(pageLength = 25, autoWidth = TRUE, columnDefs = list(list(className = 'dt-center', targets = 5))), server = FALSE)
                
                #need to alter the lists in the df to be able to download
                
                all_is <- which(names(tweets_raw) %in% names(tweets_raw)[sapply(tweets_raw, class) == "list"])
                
                #iterate over indicies and collapse lists into characters
                for(i in all_is){
                        
                        tweets_raw[,i] <- as.character(tweets_raw[,i])
                        tweets_raw[,i] <- gsub("c\\(|\\)", "", tweets_raw[,i])
                        
                }
                
                output$dl_data <- downloadHandler(
                        
                        #write the filename
                        filename = "tweetdata.csv",
                        
                        #write the file for DL
                        content = function(file) {
                                write.csv(tweets_raw, file, row.names = FALSE)
                        }
                )
        })
        
}

shinyApp(ui, server)
