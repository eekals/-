#rm(list = ls())
library(rvest)
library(tidyverse)
library(stringr)

##__1. url쪼개기

base_url <- 'https://finance.naver.com/sise/sise_market_sum.nhn'
sosok_url <- 'sosok='
page_url <- 'page='

cospi_url <- paste0(base_url,'?',sosok_url,'0&',page_url);cospi_url
cosdaq_url<- paste0(base_url,'?',sosok_url,'1&',page_url);cosdaq_url

##__2. 마지막페이지 
(cospi_html <- read_html(cospi_url, encoding = 'EUC-KR') %>%
  html_nodes('.pgRR a') %>% html_attr('href') %>% 
  str_extract("(?<=page=)\\d+") %>% as.integer()) 

(cosdaq_html <- read_html(cosdaq_url, encoding = 'EUC-KR') %>%
    html_nodes('.pgRR a') %>% html_attr('href') %>% 
    str_extract("(?<=page=)\\d+") %>% as.integer())
  

## __3. 페이지별 url 생성
# 함수생성
pages_url <- function(url, end) {
  pages <- NULL  
  for (i in 1 : end) {
    
    temp <- paste0(url,i)
    pages[i] <- temp
  }  
  return(pages)
}

(cospi_pages <- pages_url(cospi_url,cospi_end))
(cosdaq_pages<- pages_url(cosdaq_url,cosdaq_end))

## __4. 필요한 정보 크롤링
# 필요한정보 : 종목명(company), 코드(code), 액면가(value)
items <- function(pages) {
  
  result_tb <- NULL
  for (i in 1 : length(pages))
     {  
    
      # i번째 페이지에서 크롤링
      html <- read_html(pages[i], encoding = 'EUC-KR')
      company <- html %>% html_nodes('.tltle') %>% html_text()
      value <- html %>% html_nodes('.type_2 tr td:nth-of-type(6)') %>% html_text()  
      code <- html %>% html_nodes('.tltle') %>% html_attr('href') %>% lapply(function(x) {str_extract(x,"(?<=code=)\\d+")}) %>% unlist()
    
      temp <- tibble(
      코드 = code,
      종목명 = company,
      액면가 = value
      )
      
      result_tb <- rbind(result_tb, temp)
    }
  
    return(result_tb)
}

cospi_items <- items(cospi_pages)
cosdaq_items <- items(cosdaq_pages)

#__5. csv 파일로 내보내기
options(encoding = "UTF-8")
write_excel_csv(cospi_items,'C:/Users/PC/Desktop/금융통계/cospi.csv')
write_excel_csv(cosdaq_items,'C:/Users/PC/Desktop/금융통계/cosdaq.csv')

