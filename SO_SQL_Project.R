library(rvest)
library(tidyr)
library(dplyr)
library(httr)
library(stringr)

base_url <- "https://stackoverflow.com/questions/tagged/r"
url <- paste0(base_url, "?tab=newest&page=", page_number, "&pagesize=50")
print(url) # page url
SO_page <- read_html(url)

# lists that will be added as columns to the final data frame
all_question_links <- c()
all_question_texts <- c()
allVotes <- c()
allViews <- c()
allTitles <- c()
all_question_dates <- c()
allTags <- c()
all_user_names <- c()
all_reps <- c()
q_badge_info <- c()
q_editors <- c()
q_edit_dates <- c()
all_ans <- c()


# https://www.youtube.com/watch?v=qyGYItbMKkM
# https://datascience.stackexchange.com/questions/8922/removing-strings-after-a-certain-character-in-a-given-text

# http://www.endmemo.com/r/regexpr.php
# https://jtr13.github.io/cc19/web-scraping-using-rvest.html
# getting question links from a page

get_questionLinks <- function(pg){
  question_part_link <- pg %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    grep("^/questions/\\d+", ., value=TRUE)
  website_url = "https://stackoverflow.com"
  question_link <- paste0(website_url, question_part_link)
  return(question_link)
}

# getting number of votes for question
get_num_votes <- function(q_page){
  numVotes <- q_page %>%
    html_elements(xpath="//div[starts-with(@class, 'question ')]") %>%
    html_elements(xpath=".//div[contains(@class, 'post-layout ')]") %>%
    html_elements(xpath=".//div[starts-with(@class, 'votecell')]") %>%
    html_elements(xpath=".//div[starts-with(@class, 'js-voting-container')]") %>%
    html_elements(xpath=".//div[starts-with(@class, 'js-vote-count ')]") %>%
    html_attr('data-value') %>%
    as.numeric()
  return(numVotes)
}

# getting number of views for each question
get_num_views <- function(q_page){
  numViews <- q_page %>% 
    html_elements(xpath="//div[starts-with(@class,'d-flex ')]") %>%
    html_elements(xpath=".//div[starts-with(@title, 'Viewed ')]") %>%
    html_attr('title') %>%
    str_remove_all("Viewed ") %>%
    str_remove_all(" times") %>%
    as.numeric()
  return(numViews)
}

# getting date when a question was posted
get_question_dates <- function(q_page){
  question_date <- q_page %>%
    html_elements(xpath="//*[@id='content']/div/div[1]/div[2]/div[1]/time") %>%
    html_attr("datetime") %>%
    str_remove_all("T.*")
  return(question_date)
}

# getting the text for each question (not including code blocks)
# each section of text is stored in a list
get_ques_text <- function(q_page){
  qtext <- q_page %>%
    html_elements(xpath="//*[@id='question']") %>%
    html_elements(xpath=".//div[contains(@class, 'postcell post-layout--right')]") %>%
    html_elements(xpath=".//div[starts-with(@class, 's-prose')]") %>%
    html_text(trim=TRUE)
  return(qtext)
}

# getting the title of each question post
get_ques_title <- function(q_page){
  question_title <- q_page %>%
    html_elements(xpath="//*[@id='question-header']/h1/a") %>%
    html_text()
  return(question_title)
}

# getting the tags for each question posted
get_tags_per_question_link <- function(q_page){
  tags <- q_page %>% 
    html_elements(xpath = "//div[starts-with(@class, 'post-taglist')]") %>%
    html_element(xpath = ".//ul[contains(@class, 'js-post-tag-list-wrapper')]") %>%
    html_nodes("li") %>%
    html_element(xpath = ".//a[starts-with(@class, 'post-tag')]") %>%
    html_text()
  comma_separated_tags_list <- paste(tags, collapse = ",")
  comma_separated_tags_list
  return(comma_separated_tags_list)
}

# getting user name of the person posting the question 
get_question_user <- function(q_page){
  name <- q_page %>%
    html_elements(xpath = "//div[starts-with(@class, 'postcell')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'post-signature owner flex--item')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-info ')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-details')]") %>%
    html_nodes("a")%>%
    html_text()
  return(name)
}

# getting question post user's reputation
get_ques_user_rep <- function(q_page){
  reputation <- q_page %>%
    html_elements(xpath = "//div[starts-with(@class, 'postcell')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'post-signature owner flex--item')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-info ')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-details')]") %>%
    html_elements(xpath = ".//span[starts-with(@class, 'reputation-score')]")%>%
    html_text()
  return(reputation)
}

# getting number of gold, silver and bronze badges for the user who posted the question
# will return a comma separated list (one column of the data frame will contian badge information)
get_question_userBadges <- function(q_page){
  questioners_badges_earned <-  q_page %>%
    html_elements(xpath = "//div[starts-with(@class, 'postcell')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'post-signature owner flex--item')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-info ')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-details')]") %>%
    html_elements(xpath = ".//span[contains(@class, 'v-visible-sr')]")%>%
    html_text()
  questioners_badges_lst <- paste(questioners_badges_earned, collapse=",")
  return(questioners_badges_lst)
}

# getting the user name of who edited the question
get_question_editor <- function(q_page){
  editor <- q_page %>%
    html_elements(xpath = "//div[starts-with(@class, 'postcell')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'post-signature flex--item')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-info ')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-details')]") %>%
    html_element(xpath = ".//a")%>%
    html_text()
  if(length(editor)==0){
    editor="No Editor"
  }
  return(editor)
}

# getting the date that the question was edited
get_question_editDate <- function(q_page){
  edit_date <- q_page %>%
    html_elements(xpath = "//div[starts-with(@class, 'postcell')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'post-signature flex--item')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-info ')]") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'user-action-time')]") %>%
    html_element(xpath = ".//span")%>%
    html_attr('title')
  if (length(edit_date)==0){
    edit_date="No Edits"
  }
  return(edit_date)
}

# getting text of answers from each question page
get_answer_text <- function(q_page){
  all_ans_string <- c()
  ans_text <- q_page %>%
    html_elements(xpath = "//div[@id='answers']") %>%
    html_elements(xpath = ".//div[starts-with(@class, 'answercell post-layout--right')]") %>%
    html_elements(xpath = ".//div[starts-with(@itemprop, 'text')]") %>%
    html_text(trim=TRUE)
  all_ans_string <- append(all_ans_string, ans_text)
  return(all_ans_string)
}


for (i in 1:2){
  page_number <- i
  url <- paste0(base_url, "?tab=newest&page=", page_number, "&pagesize=50")
  page <- read_html(url)
  all_question_links <- append(all_question_links, get_questionLinks(page))
  for (j in all_question_links){
    q_page <- read_html(j)
    view_count <- get_num_views(q_page)
    allViews <- append(allViews, view_count)
    
    vote_count <- get_num_votes(q_page)
    allVotes <- append(allVotes, vote_count)
    
    title <- get_ques_title(q_page)
    allTitles <- append(allTitles, title)
    
    text <- get_ques_text(q_page)
    all_question_texts <- append(question_texts, text)
    
    date <- get_question_dates(q_page)
    all_question_dates <- append(question_dates, date)
    
    tags <- get_tags_per_question_link(q_page)
    allTags <- append(allTags, tags)
    
    user_name <- get_question_user(q_page)
    all_user_names <- append(user_names, user_name)
    
    rep <- get_ques_user_rep(q_page)
    all_reps <- append(reps, rep)
    
    q_badges <- get_question_userBadges(q_page)
    q_badge_info <- append(q_badge_info, q_badges)
    
    ques_editor <- get_question_editor(q_page)
    q_editors <- append(q_editors, ques_editor)
    
    q_edit_date <- get_question_editDate(q_page)
    q_edit_dates <- append(q_edit_dates, q_edit_date)
    
    ansText <- get_answer_text(q_page)
    all_ans <- append(all_ans, ansText)
    
    Sys.sleep(.25)
  }
}
print(allViews)
print(allTitles)
print(all_ans)
print(all_user_names)
question_info <- c()
for(x in 1:200){
  
  allViews[x] + allTitle[x]
  question_info <- append(question_info, allViews[x])
  question_info <- append(question_info, allTitles[x])
  
}

links_lst <- c("https://stackoverflow.com/questions/76400485/machine-learning-difference-between-test-and-train-features-imdb-movie-review", "https://stackoverflow.com/questions/76400159/adding-background-color-for-currentvalue-animated-plotly-graph-in-r")

page_number <- 1
url <- paste0(base_url, "?tab=newest&page=", page_number, "&pagesize=50")
page <- read_html(url)
all_question_links <- append(all_question_links, get_questionLinks(page))
for(i in links_lst){
  get_question_content(i)
  Sys.sleep(10)
}

df1 = data.frame('Views'=allViews, 
                'Votes'=allVotes,
                'Question' = all_question_texts,
                'Question_Title' = allTitles,
                'Date'=all_question_dates, 
                'Tags'=allTags, 
                'Question_Poster'=all_user_names,
                'Reputation'=all_reps,
                'Badge_Info' = q_badge_info,
                'Editor' = q_editors, 
                'Edit_Date' = q_edit_dates)

View(df1)



#https://stackoverflow.com/questions/64206837/scrape-web-page-pagination-with-rvest-pagination-path-does-not-appear-in-the-st


get_question_content <- function(q_page){
  view_count <- get_num_views(q_page)
  allViews <- append(allViews, view_count)
  
  vote_count <- get_num_votes(q_page)
  allVotes <- append(allVotes, vote_count)
  
  title <- get_ques_title(q_page)
  allTitles <- append(allTitles, title)
  
  text <- get_ques_text(q_page)
  all_question_texts <- append(question_texts, text)
  
  date <- get_question_dates(q_page)
  all_question_dates <- append(question_dates, date)
  
  tags <- get_tags_per_question_link(q_page)
  allTags <- append(allTags, tags)
  
  user_name <- get_question_user(q_page)
  all_user_names <- append(user_names, user_name)
  
  rep <- get_ques_user_rep(q_page)
  all_reps <- append(reps, rep)
  
  q_badges <- get_question_userBadges(q_page)
  q_badge_info <- append(q_badge_info, q_badges)
  
  ques_editor <- get_question_editor(q_page)
  q_editors <- append(q_editors, ques_editor)
  
  q_edit_date <- get_question_editDate(q_page)
  q_edit_dates <- append(q_edit_dates, q_edit_date)
  
  ansText <- get_answer_text(q_page)
  all_ans <- append(all_ans, ansText)
}

# getting question text, title, date, answer and comments from question page
columns <- c('Views', 'Votes', 'Question', 'Title')
empty_df = data.frame(matrix(ncol=length(columns)))
colnames(empty_df) <- columns
View(empty_df)
df <- data.frame('Views'=character(), 
                 'Votes'=character(), 
                 'Question'=character(), 
                 'Title'=character(), 
                 stringsAsFactors=FALSE)
final = data.frame()
View(df)
