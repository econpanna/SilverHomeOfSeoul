rm(list=ls())
library(tidyverse)
library(rvest)
library(stringr)

#### 데이터 불러오기 ####
# http://www.longtermcare.or.kr/npbs/r/a/201/selectLtcoSrch 
# 위의 링크에서 지역 조건을 '서울특별시'로 선택하여 검색한 결과를 크롤링..해야하는데 
# 문제 발생: get방식 요청일 때(url 주소로) 크롤링하는 방법만 공부한 상태인데 위의 사이트에서 검색 결과를 post방식으로 요청..
# 해결: 급한대로 임시(야매)로 사이트 html의 hidden값을 수정해서 전체 리스트(4864개)가 출력된 소스를 longtermcare.html 파일로 저장ㄷㄷ
getwd()
setwd('/Users/lee/gitProject/silverHomeOfSeoul/src/data')
list.files()
ltc_doc <- read_html('longtermcare.html')
# 반성: 알아보니 R에서 post방식으로 요청할 수도 있는 듯하고 셀레늄이란 것을 이용할 수도 있음.. 좀 더 공부해보기..

# 데이터 있는 html node 확인 결과: #ltco_info_list > tbody > tr
# 필요한 변수: td:nth-child(2)~(11). 급여종류, 평가결과, 기관명, 정원, 현원, 잔여, 대기, 전화번호, (방문목욕차량,) 주소  
select_tds <- c(2:9,11) # 방문목욕차량 여부는 제외
ltc_cols <- vector('list', length(select_tds))
for (i in 1:length(select_tds)) {
  node_loc <- paste0('#ltco_info_list > tbody > tr > td:nth-child(', select_tds[i], ')')
  ltc_cols[[i]] <- ltc_doc %>% 
                     html_nodes(node_loc) %>% 
                     html_text() %>% 
                     as.data.frame(stringsAsFactors=F) # 안해주면 이 다음에 bind_cols 안됨
}



#### 처리1 => ltc_dat_raw1 ####
ltc_dat_raw1 <- bind_cols(ltc_cols) # 하나로 병합
ltc_dat_raw1 %>% class # data.frame
ltc_dat_raw1 %>% View # 확인
ltc_dat_raw1 %>% str

# 컬럼명 지정
col_names <- c('type','rating','name','capacity','current','remain','waiting','phone','address')
colnames(ltc_dat_raw1) <- col_names
ltc_dat_raw1 %>% View
ltc_dat_raw1 %>% str
ltc_dat_raw1 %>% summary # 결측치?
# 결측치?
sum(is.na(ltc_dat_raw1)) # 0: 일단 없지만 모두 char타입이어서 전처리 후 재확인 필요



#### 처리2 => ltc_dat_raw2 ####
# 불필요한 문자(\t, \n), 공백 제거
ltc_dat_raw2 <- ltc_dat_raw1 %>% 
                 lapply(str_replace_all, '\\s+', ' ') %>% # 빈 칸을 안주면 띄어쓰기가 다 없어짐
                 lapply(trimws) %>% 
                 as.data.frame(stringsAsFactors=F)
ltc_dat_raw2 %>% class # data.frame
ltc_dat_raw2 %>% View
ltc_dat_raw2 %>% str 
ltc_dat_raw2 %>% summary

# 결측치, 공백, or 이상한 값 재확인
sum(is.na(ltc_dat_raw2)) # 0: 결측치 없음
for (i in 1:ncol(ltc_dat_raw2)){
  print(paste0('Blank in ', colnames(ltc_dat_raw2)[i], ': ',sum(ltc_dat_raw2[,i]==' ')))
} # 공백없음
# 데이터를 보니 - 가 있어서 확인
for (i in 1:ncol(ltc_dat_raw2)){
  print(paste0('- in ', colnames(ltc_dat_raw2)[i], ': ',sum(ltc_dat_raw2[,i]=='-')))
} # capacity: 3916개, current: 3916개 발견
# capacity, current 컬럼의 - 에 NA 대입
ltc_dat_raw2 <- ltc_dat_raw2 %>%
                 mutate(capacity = gsub('^-$', NA, capacity),
                        current  = gsub('^-$', NA, current))
ltc_dat_raw2 %>% View # 확인
ltc_dat_raw2 %>% str

# capacity, current, remain, waiting 컬럼 numeric 타입으로 변환
num_colnames <- c('capacity', 'current', 'remain', 'waiting')
ltc_dat_raw2[num_colnames] <- lapply(ltc_dat_raw2[num_colnames], as.numeric)
ltc_dat_raw2 %>% str

# type 컬럼 factor 변환 전에 이상한 값 없는지 확인
ltc_dat_raw2 %>%
  select(type) %>% 
  unique  # ok
# type 컬럼 factor 변환
ltc_dat_raw2$type <- as.factor(ltc_dat_raw2$type)



#### 처리3 => ltc_dat_raw3 ####
# rating에 이상한 값 없는지 확인
ltc_dat_raw2 %>%
  select(rating) %>% 
  unique  # ok.. separate 필요
# separate rating 컬럼 into 'rating','rat_detail','rat_year','rat_type'
ltc_dat_raw3 <- ltc_dat_raw2 %>%
  separate(col=rating, into=c('rating','rat_detail','rat_year','rat_type')) # 자동으로 해줌><
ltc_dat_raw3 %>% View

# name에 '상세보기' 및 홑따옴표, 쌍따옴표, 역슬래시 등 제거 후 공백trim
ltc_dat_raw3$name <- gsub('상세보기', '', ltc_dat_raw3$name)
ltc_dat_raw3$name <- trimws(gsub('[\'\"\\]', '', ltc_dat_raw3$name))
# 확인
ltc_dat_raw3$name
ltc_dat_raw3$phone
ltc_dat_raw3$address



#### 처리4 => ltc_dat_raw4 ####
# address 시, 구, 도로명, 구주소 구분
sum(grepl('^서울특별시', ltc_dat_raw3$address)) # 4864 : 전부 서울특별시로 시작
# test 결과 grep 는 문자열(패턴)이 속하는 index 반환, grepl 는 문자열이 속하는지 여부 T/F 반환
# separate rating 컬럼 into 'addr_si','addr_gu','addr_st','addr_old'
ltc_dat_raw4 <- ltc_dat_raw3 %>% 
  separate(col=address, into=c('addr_si', 'addr_gu', 'addr_rest'), extra='merge') %>% 
  separate(col=addr_rest, into=c('addr_st', 'addr_old'), sep='\\(') %>% # 왜 여긴 \\두 개일까. 위에는 한 개..
  mutate(addr_old = str_replace_all(addr_old, '\\)', ''))
# trim
new_addr_colnames<- c('addr_si','addr_gu','addr_st','addr_old')
ltc_dat_raw4[new_addr_colnames] <- apply(ltc_dat_raw4[new_addr_colnames], 2, trimws)
ltc_dat_raw4 %>% View # 확인
ltc_dat_raw4 %>% class # data.frame
ltc_dat_raw4 %>% str



#### 경도, 위도 (도 표시하려고 했으나 값을 받아오지 못하는 데이터가 많아서 경도, 위도 시각화는 일단 보류..)####
library(ggmap)
unique(ltc_dat_raw4$type) # 이번 프로젝트는 재가 형태가 아닌 '노인요양시설(개정법)'과 '노인요양공동생활가정'에 한정할 것
# 두 type의 인덱스T/F 확인
type_y <- ltc_dat_raw4$type %in% c('노인요양시설(개정법)','노인요양공동생활가정')

# 경도 위도 받을 빈 데이터프레임 생성 
ltc_latlon <- data.frame(matrix(nrow=nrow(ltc_dat_raw4), ncol=2))
colnames(ltc_latlon) <- c('lat_y','lon_x')
# 경도 위도 받아오기
for (i in 1:nrow(ltc_dat_raw4)){
  if (type_y[i]){
    ltc_latlon[i,] <- geocode(enc2utf8(paste0(ltc_dat_raw4$addr_gu[i], ' ', ltc_dat_raw4$addr_st[i])))
  }
}
ltc_latlon %>% View
sum(is.na(ltc_latlon[type_y,])) # 총 실패 개수
# 재시도
fail_idx <- which(type_y & (rowSums(is.na(ltc_latlon)) > 0)) # type_y이면서 경도,위도 중 하나라도 NA이면 fail??
fail_idx %>% length
for (f_i in fail_idx){
  ltc_latlon[f_i,] <- geocode(enc2utf8(ltc_dat_raw4$addr_st[f_i]))
  # ltc_latlon[f_i,] <- geocode(enc2utf8(paste0(ltc_dat_raw4$addr_gu[f_i], ' ', ltc_dat_raw4$addr_st[f_i])))
  # ltc_latlon[f_i,] <- geocode(enc2utf8(paste0(ltc_dat_raw4$addr_si[f_i], ' ', ltc_dat_raw4$addr_gu[f_i], ' ', ltc_dat_raw4$addr_st[f_i])))
}
# 일단 41개 실패..query한도 초과..나중에 다시 시도 
ltc_latlon %>% View



#### ltc_dat_raw5 ####
# 'lat_y','lon_x' 컬럼 추가
ltc_dat_raw5 <- ltc_dat_raw4 %>% 
  bind_cols(lon_x = ltc_latlon['lon_x'],
            lat_y = ltc_latlon['lat_y'])
ltc_dat_raw5 %>% View
# 경도, 위도는 나중에 다시 해야할 듯..일단 저장..



##### write csv ####
ltc_dat_new <- ltc_dat_raw5
write.csv(ltc_dat_new, file='longtermcare_seoul.csv', row.names=F, fileEncoding = 'utf-8') # 전처리한 데이터 csv 파일로 저장

