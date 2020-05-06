library(mapspam2globiom)
param <- spam_par(spam_path = "C:/Users/dijk158/Dropbox/mapspam2globiom_mwi",
         iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
         solve_level = 1, model = "max_score")

create_spam_folders(param)

options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


# param <- spam_par(spam_path = "D:/temp/mapspam2globiom_mwi",
#                   iso3c = "MWI", year = 2010, res = "5min", adm_level = 2,
#                   solve_level = 1, model = "max_score")

create_spam_folders(param)


adm_list_wide <- dplyr::bind_rows(
  adm_list %>%
    dplyr::select_at(vars(contains("adm0"))) %>%
    setNames(c("adm_name", "adm_code")) %>%
    dplyr::mutate(adm_level = 0) %>%
    unique,
  adm_list %>%
    dplyr::select_at(vars(contains("adm1"))) %>%
    setNames(c("adm_name", "adm_code")) %>%
    dplyr::mutate(adm_level = 1) %>%
    unique,
  adm_list %>%
    dplyr::select_at(vars(contains("adm2"))) %>%
    setNames(c("adm_name", "adm_code")) %>%
    dplyr::mutate(adm_level = 2) %>%
    unique)

x <- purrr::map_dfc(crop$crop, function(x) {
  print(x)
  df <- data.frame()
  df[,x] <- "test"
  return(df)
})

adm_list_wide[,crop$crop] <- NA
x <- "maiz"

ss<- data.frame(x=c(1,NA,3,4),y=c(2,NA,4,NA))
ss
apply(ss,1,sum,na.rm=F)
sum(c(NA, NA), na.rm = T)
plus(c(NA, NA), na.rm = T)
