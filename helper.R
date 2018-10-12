library(shiny) ; library(shinyjs); library(SnowballC) ; library(rtweet) ; library(tm) ; library(syuzhet)
library(tidyverse) ; library(wordcloud2) ; library(ggthemes) ; library(gridExtra) ; library(scales) ; library(DT)
library(stringr); library(Hmisc)
test_run <- function(){source("./spares.R", local = TRUE)}
###REUSING TOKEN

#upload saved token
twitter_token <- readRDS(file = "./twitter_token.rds") ; print(twitter_token)

#HELPERS

#used to remove common patterns not useful to us from every tweet
clean_tweets <- function(tweets, regex_expressions){
        
        library(stringr)
        
        #iterate through each reg ex expression and remove.
        for(i in c(regex_expressions, "#")){

                tweets <- str_replace_all(tweets, i, "")

        }
        
        #final formatting (lower case etc.)
        tweets  %>% 
                iconv(from = "latin1", to = "ASCII", sub="") %>%
                gsub("[ \t]{2,}", " ", .) %>%
                gsub("^\\s+|\\s+$", "", .) %>%
                tolower

}

#alternate cleaning function
cleanPosts <- function(text) {
        clean_texts <- text %>%
                gsub("<.*>", "", .) %>% # remove emojis
                gsub("&amp;", "", .) %>% # remove &
                gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", .) %>% # remove retweet entities
                gsub("@\\w+", "", .) %>% # remove at people
                hashgrep %>%
                gsub("[[:punct:]]", "", .) %>% # remove punctuation
                gsub("[[:digit:]]", "", .) %>% # remove digits
                gsub("http\\w+", "", .) %>% # remove html links
                iconv(from = "latin1", to = "ASCII", sub="") %>% # remove emoji and bizarre signs
                gsub("[ \t]{2,}", " ", .) %>% # remove unnecessary spaces
                gsub("^\\s+|\\s+$", "", .) %>% # remove unnecessary spaces
                tolower
        return(clean_texts)
}

#return all common reg ex expressions bar any specified (enter names of exp)
re_exp <- function(leave = ""){
        
        #common expressions
        exps <- c("\\[^\\s]+", "(RT|via)((?:\\b\\W*@\\w+)+)", "#[^\\s]+", "@([^\\s]+)", "http\\w+", "<.*>", "&amp;",
                  "@\\w+", "[[:punct:]]")
        
        #name the expressions
        names(exps) <- c("breaks", "retweets", "hashtag", "username", "links", "emojis", "and", "at", "punctuation")
        
        #filter out unwanted expressions
        exps <- exps[!(names(exps) %in% leave)]
        
        return(exps)
}

#short CSS code to apply loading spinner icon when loading from shinyJS github - https://github.com/daattali/advanced-shiny/tree/master/plot-spinner
mycss <- "
        #plot-container {
        position: relative;
        }
        #loading-spinner {
        position: absolute;
        left: 50%;
        top: 50%;
        z-index: -1;
        margin-top: -33px;  /* half of the spinner's height */
        margin-left: -33px; /* half of the spinner's width */
        }
        #plot.recalculating {
        z-index: -2;}

        #wcLabel { display: none;}
        
        $(document).on('click', '#canvas', function() {
        word = document.getElementById('wcSpan').innerHTML;
        Shiny.onInputChange('selected_word', word);
        });
        "

#create wordcloud
wcGen <- function(text, choice = 1, explicit = TRUE, fontFamily = "Helvetica Neue", query, colour = randCol(13), ...){
        
        #used libraries
        library(SnowballC) # stemming
        library(wordcloud2) # visualisation
        library(tm) #mapping functions (text mining)
        library(syuzhet) #sentiment analysis
        
        #seperate query into vector
        query <- strsplit(query, " ")[[1]]
        
        #depending on explicit option, remove explicit words
        if(explicit == FALSE){
                
                #read into environment csv containing explicit words
                ex <- read.csv("explicit.csv")
                
                #format as character vector
                ex <- as.vector(as.character(ex$X4r5e))
                
                #add to the list of words to remove
                remove_list <- c(stopwords("english"), ex, query)
                
        } else {
                
                #else just remove stopwords
                remove_list <- c(stopwords("english"), query)
                
        }
        
        
        #create corpus (Volatile Corpora)
        tweetCorp <- VCorpus(VectorSource(tolower(text)))
        
        
        #interface to apply mapping functions (transformations to corpora)
        #also fix and shave off unwanted elements (Cleaning and pre processing)
        
        #create plain text doccument
        tweetCorp <- tm_map(tweetCorp, PlainTextDocument)
        
        #remove punctuation
        tweetCorp <- tm_map(tweetCorp, removePunctuation) 
        
        #remove numbers
        tweetCorp <- tm_map(tweetCorp, removeNumbers)
        
        #remove words on list (stop words, if true, explicit words)
        tweetCorp <- tm_map(tweetCorp, removeWords, remove_list)
        
        #stem similar words (e.g. complicate, complicated)
        # tweetCorp <- tm_map(tweetCorp, stemDocument) 
        
        #create matrix mapping frequency to words
        myTDM <- as.matrix(TermDocumentMatrix(tweetCorp)) 
        
        # make into frequency matrix
        FreqMat <- data.frame(ST = rownames(myTDM), Freq = rowSums(myTDM), row.names = NULL) 
        
        #if condition is TRUE, return only the dataframe
                
        tbl <- FreqMat %>% arrange(desc(Freq))
        
        FreqMat[,"Freq"] <- log(FreqMat[,"Freq"], base = 2)
        
        # FreqMat$Freq <- sqrt(FreqMat$Freq)
        
        #fig path of shape (not working)
        figPath = "./t.png"
        
        #map of shapes to cycle
        shape <- c("circle", "diamond", "pentagon", "star")
        
        #fonts to be cycled
        ff <- c("Arial", "Calibri", "Cambria", "Didot", "Garamond", "Georgia",
                "Helvetica", "Times New Roman", "Century Gothic", "Helvetica Neue")
        
        #randomly select shape & font
        shape <- shape[sample(1:length(shape), 1)] ; ff <- ff[sample(1:length(ff), 1)]
        
        #create wordcloud of twitter logo (NOT WORKING)
        one <- wordcloud2(FreqMat, figPath = figPath, fontFamily = ff, ...)
        
        # , color = "#1DA1F2"
        
        #create usable wordcloud
        two <- wordcloud2(FreqMat, fontFamily = ff, shape = shape, ...)
        
        #return wc depending on input
        if(choice == 1){
                
                return(list(one, tbl))
                
        } else{
                
                return(list(two, tbl))
                
        }
        
        
}

#return time series graph with added function not included in rtweet
time_series_tweets <- function(tweetData, time, number, query, 
                               inc.sent = c(positive = FALSE,  neutral = FALSE, negative = FALSE),
                               plot.title = ""){
        
        #error checking
        for(i in c(tweetData[1,88], time, number, query, inc.sent)){print("break");print(i)}
        
        #colors to use
        intPal <- c("#6029AC", "#FF006E", "#0B4F6C", "#B95F89", "#B1740F", "#003554",
                    "#57A773", "#9F9AC5", "#58A100")
        
        #assign unit to variable depending on input
        unit <- ifelse(time == "Seconds", "seconds",
                       ifelse(time == "Minutes", "mins", 
                              ifelse(time == "Hours", "hours",
                                     ifelse(time == "Days", "days", NULL))))
        
        #create unit string
        unit <- paste(number, unit)
        
        #Assign sentiment relating on syuzhet score
        tweetData$sent.type <- ifelse(tweetData$sent.score > 0, "Positive",
                                      ifelse(tweetData$sent.score == 0, "Neutral", "Negative"))
        
        #ensure data structure
        tweetData$sent.type <- as.character(tweetData$sent.type)
        
        #use as base rtweet's function to create base
        dat <- ts_data(tweetData, unit) ; dat$sent.type <- "All"
        
        #get max y value for scaling
        maxY <- max(dat$n) ; print(maxY)
        
        #iterate through tweets and their sentiments to assign dat with correct sentiment labels
        for(i in 1:length(inc.sent)){
                
                #check if sentiment is wanted using input
                if(inc.sent[i]){
                        
                        #depending on location of input
                        if(i == 1){
                                
                                #assign as positive
                                sent <- tweetData[tweetData$sent.type == "Positive",]
                                
                        }else if(i == 2){
                                
                                #assign as neutral
                                sent <- tweetData[tweetData$sent.type == "Neutral",]
                                
                        }else{
                                
                                #assign as negative
                                sent <- tweetData[tweetData$sent.type == "Negative",]
                                
                        }
                        
                        #make new ts data to plot on the axis with assigned sentiment
                        sent <- ts_data(sent, unit) ; sent$sent.type <- ifelse(i == 1, "Positive", 
                                                                               ifelse(i == 2, "Neutral", "Negative"))
                        
                        
                        #add to the aggregation
                        dat <- rbind(dat, sent)
                        
                }
                
        }
        
        #format the plot title
        pltTitle <- ifelse(plot.title == "", paste("Time series of ", ' "', query, '"',  " related tweets", sep = ""),
                           plot.title)
        
        #replot and return time series with relevant lines and updates
        ggplot(dat, aes(x = time, y = n, color = sent.type)) + theme_minimal(base_family = "Helvetica Neue") +
                theme(plot.title = element_text(face = "bold")) + 
                labs(x = NULL, y = NULL, 
                     title = pltTitle) +
                geom_hline(yintercept = 0) + 
                scale_y_continuous(breaks = seq(0, maxY + 5, 
                                                ifelse(round(maxY / 10) == 0, ceiling(maxY / 10), round(maxY / 10)))) +
                geom_line() + scale_color_manual(values = sample(intPal, 4, FALSE), guide = guide_legend(title = "Sentiment Type: "))
        
}

#organise tweet pull data into usable formats and also apply relevant pre processing
basic_dfs <- function(tweets){
        
        library(syuzhet) #Get emotion related values
        library(dplyr) #wrangling
        library(rtweet) #use rtweet functions
        
        #use clean_tweets (defined above) to remove reg_ex expressions and also use rtweet plain_tweets to get pure text.
        txt <- clean_tweets(
                plain_tweets(tweets$text),
                re_exp())
        
        #add onto the above table the emotion and sentiment values
        emo <- cbind(txt,
                     get_nrc_sentiment(as.vector(txt)),
                     get_sentiment(txt))
        
        #ensure data type
        emo$txt <- as.character(emo$txt)
        
        #rename for clarity
        names(emo)[1] <- "tweet.text" ; names(emo)[12] <- "sent.score"
        
        #extract only the numbers relating to emotional values
        emotion.df <- emo[,-c(1, 12)]
        
        #tallies up scores and then unlists into vector
        s <- unlist(lapply(emotion.df, sum))
        
        #reformats s into table format
        scores <- data.frame(emotion = names(emotion.df),
                             total = s) ; rownames(scores) <- NULL
        
        #return list of three main dfs used in analysis
        return(
                list(txt = txt, emo = emo, scores = scores)
        )
        
}

#defines theme for the graphics used
def_theme <- function() {
        
        #set background color
        offwhite <- "white"
        
        #set font
        fontText <- 'Helvetica Neue'
        
        #theme minimal as base
        theme_minimal(base_size=9, base_family = fontText ) +
                
                theme(plot.title = element_text(size = 14, vjust = 1.25, hjust = 0.5)) + #plot title
                theme(axis.text.x = element_text(size = 11)) + #x axis text
                theme(axis.text.y = element_text(size = 11)) + #y axis text
                theme(axis.title.x = element_text(size = 12)) + #x axis title
                theme(axis.title.y = element_text(size = 12, vjust = 1.25)) + #y axis title
                theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm")) + #plot margin
                theme(plot.subtitle = element_text(size = 12, vjust = 1.25)) + #subtitle
                theme(axis.line = element_blank(), #axis line removes
                      plot.background = element_rect(fill = offwhite, color = offwhite), #background color set
                      panel.background = element_rect(fill = offwhite, color = offwhite), #panel background set
                      panel.border = element_blank(), panel.grid = element_blank() # panel border removed
                )
} ; 
theme_set(def_theme()) #set theme

#busy indicator while retrieving tweets from API - https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
withBusyIndicatorUI <- function(button) {
        
        id <- button[['attribs']][['id']]
        div(
                `data-for-btn` = id,
                button,
                span(
                        class = "btn-loading-container",
                        hidden(
                                img(src = "https://raw.githubusercontent.com/daattali/advanced-shiny/master/busy-indicator/www/ajax-loader-bar.gif", class = "btn-loading-indicator"),
                                icon("check", class = "btn-done-indicator")
                        )
                ),
                hidden(
                        div(class = "btn-err",
                            div(icon("exclamation-circle"),
                                tags$b("Error: "),
                                span(class = "btn-err-msg")
                            )
                        )
                )
        )
}

#busy indicator while retrieving tweets from API - https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
withBusyIndicatorServer <- function(buttonId, expr) {
        # UX stuff: show the "busy" message, hide the other messages, disable the button
        loadingEl <- sprintf("[data-for-btn=%s] .btn-loading-indicator", buttonId)
        doneEl <- sprintf("[data-for-btn=%s] .btn-done-indicator", buttonId)
        errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
        shinyjs::disable(buttonId)
        shinyjs::show(selector = loadingEl)
        shinyjs::hide(selector = doneEl)
        shinyjs::hide(selector = errEl)
        on.exit({
                shinyjs::enable(buttonId)
                shinyjs::hide(selector = loadingEl)
        })
        
        # Try to run the code when the button is clicked and show an error message if
        # an error occurs or a success message if it completes
        tryCatch({
                value <- expr
                shinyjs::show(selector = doneEl)
                shinyjs::delay(2000, shinyjs::hide(selector = doneEl, anim = TRUE, animType = "fade",
                                                   time = 0.5))
                value
        }, error = function(err) { errorFunc(err, buttonId) })
}

#busy indicator while retrieving tweets from API - https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
errorFunc <- function(err, buttonId) {
        errEl <- sprintf("[data-for-btn=%s] .btn-err", buttonId)
        errElMsg <- sprintf("[data-for-btn=%s] .btn-err-msg", buttonId)
        errMessage <- gsub("^ddpcr: (.*)", "\\1", err$message)
        shinyjs::html(html = errMessage, selector = errElMsg)
        shinyjs::show(selector = errEl, anim = TRUE, animType = "fade")
}

#CSS for busy indictor - https://github.com/daattali/advanced-shiny/tree/master/busy-indicator
appCSS <- "
         .btn-loading-container {
           margin-left: 10px;
           font-size: 1.2em;
         }
         .btn-done-indicator {
           color: green;
         }
         .btn-err {
           margin-top: 10px;
           color: red;
         }

         #sidebar {
            background-color: #FFFFFF;
        }

        body, label, input, button, select { 
          font-family: ; 'Helvetica Neue'
        }

        #wclabel {display: none;}

        $(document).on('click', '#canvas', function() {
        word = document.getElementById('wcSpan').innerHTML;
        Shiny.onInputChange('selected_word', word);
        });
        "

         
#accurate rounding up function - returns largest column of x's one step up e.g. 224 -> 300
roundUp <- function(x, nice = c(1,2,3,4,5,6,7,8,10)) {
        
        #args check
        if(length(x) != 1) stop("'x' must be of length 1")
        
        #rounding equation
        # left half ids column of number to round
        # right half finds int bigger than the one in column the farthest left of x
        10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

#returns random x number of colors from predetermined palette
randCol <- function(x = 1){
        
        #colors in pool
        colorPal <- c("#20453A", "#32A7A9", "#59D2FE", "#5C7AFF",
                      "#D7263D", "#448FA3", "#F0C808", "#DD1C1A",
                      "#086788", "#8A1C7C", "#F79824", "#031A6B",
                      "#004385")
        
        #get indicies of x colors
        n <- sample(1:length(colorPal), x, replace = FALSE)
        
        #return color codes
        return(colorPal[n])
        
}

#formats labels for large numbers of followers
k_format <- function(x){ 
        
        #get f000 where f = x / 1000
        trans = x / 1000 
        
        #retrieve the label
        paste0(trans, "K") 
        
}

#get frequency df for various cols
get_freq_df <- function(df, cls){
        
        #initialise empty DF
        SL <- data.frame(count = character(),
                         xMin = numeric(), xMax = numeric(), yMin = numeric(), yMax = numeric())
        
        #iterate through cls and get frequencies
        for(i in cls){
                
                #get figures for the columns
                vec <- df %>% pull(names(df[i]))
                
                #check how many unique values are in vec
                if(length(unique(vec)) > 1){
                        
                        #create bins (bin nums determined by nclass.sturges formula function) using cuts
                        counts <- factor(cut(vec, breaks = nclass.Sturges(vec)))
                        
                        #reformat counts as df
                        counts <- as.data.frame(table(counts))
                        
                        #use transform to compute new columns
                        counts <- transform(counts, cumFreq = cumsum(Freq), relative = prop.table(Freq))
                        
                } else{
                        
                        #create one row df with same figures
                        counts <- data.frame(counts = factor(0), Freq = length(vec), 
                                             cumFreq = length(vec), relative = 1)        
                        
                }
                
                #aggregate into the format of SL with real values
                SLTemp <- data.frame(count = names(df)[i],
                                     xMin = min(df[,i]), xMax = max(df[,i]), 
                                     yMin = min(counts$Freq), yMax = max(counts$Freq)
                                     )
                
                #roll up into parent df
                SL <- rbind(SL, SLTemp)
        }
        
        return(SL)
        
}

#get plots for sentiment vs interactions
sent_int_plots <- function(df, 
                           RTmaxY = NA, RTminY = NA, RTmaxX = NA, RTminX = NA,
                           FAVmaxY = NA, FAVminY = NA, FAVmaxX = NA, FAVminX = NA,
                           TOTmaxY = NA, TOTminY = NA, TOTmaxX = NA, TOTminX = NA, 
                           plot.title = ""){
        
        #store user inputted axes limits
        axeslims <- list(retweets = c(maxY = RTmaxY, minY = RTminY, maxX = RTmaxX, minX = RTminX), 
                         favorites = c(maxY = FAVmaxY, minY = FAVminY, maxX = FAVmaxX, minX = FAVminX),
                         total = c(maxY = TOTmaxY, minY = TOTminY, maxX = TOTmaxX, minX = TOTminX))
        
        #get palette
        intPal <- c("#6029AC", "#FF006E", "#0B4F6C", "#B95F89", "#B1740F", "#003554",
                    "#57A773", "#77685D", "#9F9AC5", "#9E7F96", "#58A100", "#9C7500")
        
        #base plot for RTs
        rt <- ggplot(df, aes(x = sent.score , y = retweet_count, size = followers_count)) + 
                geom_point(alpha = 0.75, colour = sample(intPal, 1)) + 
                labs(x = "Sentiment Score", y = "Retweets") +
                theme(legend.position = "none") +
                scale_x_continuous(labels = comma)
        
        #base plot for favourites
        fav <- ggplot(df, aes(x = sent.score , y = favorite_count, size = followers_count)) + 
                geom_point(alpha = 0.75, colour = sample(intPal, 1)) + 
                labs(x = "Sentiment Score", y = "Favorites") +
                theme(legend.position = "none") +
                scale_x_continuous(labels = comma)
        
        #base plot for all interactions
        tot <- ggplot(df, aes(x = sent.score , y = total_interactions, size = followers_count)) + 
                geom_point(alpha = 0.75, colour = sample(intPal, 1)) + 
                labs(x = "Sentiment Score", y = "Total Interactions") +
                theme(legend.position = "bottom", legend.direction = "horizontal") +
                scale_size_continuous(name = "Tweeter Follower Count" , labels = k_format)
        
        #list all the plots
        fin <- list(rt, fav, tot)
        
        #get the frequency df for the required cols
        freqs <- get_freq_df(df, c(3,4,6,7))
        
        #iterate through interaction types
        for(i in 1:3){
                
                #retrieve relative axis limits
                ylims <- c(axeslims[[i]]["minY"], axeslims[[i]]["maxY"]) ; xlims <- c(axeslims[[i]]["minX"], axeslims[[i]]["maxX"])
                
                #set flag for axes lims if conditions aren't met
                if(is.na(ylims[1]) || is.na(ylims[2])){ ylims <- NULL } ; if(is.na(xlims[1]) || is.na(xlims[2])){ xlims <- NULL }
                
                #add axes lims to each plot
                fin[[i]] <- fin[[i]] + coord_cartesian(xlim = xlims, ylim = ylims)
                
                #alter y labels depending on axes limits
                if(!(is.null(ylims))){
                        
                        #based on ylims
                        fin[[i]] <- fin[[i]] + scale_y_continuous(breaks = seq(ylims[1], ylims[2], 
                                                                               roundUp(ylims[2] / 10)),
                                                                  labels = comma)
                        
                } else{
                        
                        #based on figures
                        fin[[i]] <- fin[[i]] + scale_y_continuous(breaks = seq(0, roundUp(freqs[i, 3]), 
                                                                               roundUp(freqs[i, 3] / 10)), labels = comma)
                        
                }
                
                #alter x labels depending on axes limits
                if(!(is.null(xlims))){
                        
                        #based on xlims
                        fin[[i]] <- fin[[i]] + scale_x_continuous(breaks = seq(xlims[1], xlims[2], 
                                                                               abs(roundUp(xlims[2] / 10))),
                                                                  labels = comma)
                        
                } else{
                        
                        #based on figres
                        fin[[i]] <- fin[[i]] + scale_x_continuous(breaks = seq(-5, 7, 1), 
                                                                  labels = comma)
                        
                }
        }
        
        #compile the plot title based on input
        pltTop <- ifelse(plot.title == "", "Sentiment Score vs. Retweets, Favorites and Total Interactions",
                         plot.title)
        
        #arrange rt and fav plots
        a <- grid.arrange(fin[[1]], fin[[2]], ncol = 2)
        
        #arrange with total plots and return
        grid.arrange(fin[[3]], a, top = pltTop)
        
}

#get plots for distribution of interactions
interaction_dist_plots <- function(df, 
                                   RTmaxY = NA, RTminY = NA, RTmaxX = NA, RTminX = NA,
                                   FAVmaxY = NA, FAVminY = NA, FAVmaxX = NA, FAVminX = NA,
                                   TOTmaxY = NA, TOTminY = NA, TOTmaxX = NA, TOTminX = NA,
                                   FLLWmaxY = NA, FLLWminY = NA, FLLWmaxX = NA, FLLWminX = NA,
                                   plot.title = "", bins = 50){
        
        #set axes limits according to inputs
        axeslims <- list(retweets = c(RTmaxY, RTminY, RTmaxX, RTminX), 
                         favorites = c(FAVmaxY, FAVminY, FAVmaxX, FAVminX),
                         followers = c(FLLWmaxY, FLLWminY, FLLWmaxX, FLLWminX),
                         total = c(TOTmaxY, TOTminY, TOTmaxX, TOTminX))
        
        #create palette
        p <- randCol(4)
        
        #base plots
        
        #retweets
        rtPLT <- ggplot(df, aes(retweet_count)) + 
                labs(x = "Retweets", y = "Count") +
                geom_hline(yintercept = 0)
        
        #favourites
        favPLT <- ggplot(df, aes(favorite_count)) + 
                labs(x = "Favorites", y = "Count") +
                geom_hline(yintercept = 0)
        
        #total interactions
        totPLT <- ggplot(df, aes(total_interactions)) + 
                labs(x = "Total Interactions", y = "Count") +
                geom_hline(yintercept = 0)
        
        #followers
        fllwPLT <- ggplot(df, aes(followers_count)) + 
                labs(x = "Followers", y = "Count") +
                geom_hline(yintercept = 0)
        
        #list of plots
        fin <- list(rtPLT, favPLT, fllwPLT, totPLT)
        
        #make a frequency df for the stats
        freqs <- get_freq_df(df, 3:6)
        
        #iterate through plots to adjust to settings
        for(i in 1:4){
                
                #set axes lims
                ylims <- c(axeslims[[i]][2], axeslims[[i]][1]) ; xlims <- c(axeslims[[i]][4], axeslims[[i]][3])
                
                #raise flags from user inputs
                if(is.na(ylims[1]) | is.na(ylims[2])){ylims = NULL} ; if(is.na(xlims[1]) | is.na(xlims[2])){xlims = NULL}
                
                #add settings and histograms
                fin[[i]] <- fin[[i]] + 
                        coord_cartesian(xlim = xlims, ylim = ylims) +
                        geom_histogram(fill = p[i], alpha = 0.8, bins = bins)
                
                #assign y breaks according to user input
                if(!(is.null(ylims))){
                        
                        #according to ylims
                        fin[[i]] <- fin[[i]] + scale_y_continuous(breaks = seq(ylims[1], ylims[2], 
                                                                               roundUp(ylims[2] / 10)
                        ))
                        
                } else{
                        
                        #according to figures
                        fin[[i]] <- fin[[i]] + scale_y_continuous(breaks = seq(0, roundUp(freqs[i,5]),
                                                                               roundUp(freqs[i,5] / 10)), labels = comma)
                        
                }
                
                #assign x breaks according to user input
                if(!(is.null(xlims))){
                        
                        #according to xlims
                        fin[[i]] <- fin[[i]] + scale_x_continuous(breaks = seq(xlims[1], xlims[2], 
                                                                               roundUp(xlims[2] / 10)),
                                                                  labels = ifelse(i == 3, k_format, comma))
                        
                } else{
                        
                        #according to figures
                        fin[[i]] <- fin[[i]] + scale_x_continuous(breaks = seq(0, roundUp(max(freqs[i,3])),
                                                                               roundUp(freqs[i,3] / 10)), 
                                                                  labels = ifelse(i == 3, k_format, comma))
                        
                }
                
        }
        
        #compile plot title from to input
        pltTop <- ifelse(plot.title == "", "Histograms of Retweets, Favorites, Total Interactions and Followers of the Tweet/Tweeter",
                         plot.title)
        
        #arrange plots and return
        return(grid.arrange(fin[[1]], fin[[2]], fin[[3]], fin[[4]], 
                            top = pltTop))
        
}

#counts words in a string
wordcount <- function(txt){
        #assumes all words - useful in twitter as that is probably the case
        
        #split string, count elems
        length(strsplit(txt, " ")[[1]])
        
}
