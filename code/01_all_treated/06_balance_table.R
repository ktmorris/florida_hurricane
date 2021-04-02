varnames <- c("white", "black", "latino", "asian",
              "female", "male", "dem", "rep", "age",
              "median_income", "some_college")


balance <- readRDS("./temp/balance_table_full_match.rds")

TrMean <- c()
PreMean <- c()
PreQQmed <- c()
PreQQmean <- c()
PreQQmax <- c()
PostMean <- c()
PostQQmed <- c()
PostQQmean <- c()
PostQQmax <- c()

for(i in c(1:length(balance$BeforeMatching))){
  TrMean <- unlist(c(TrMean, balance$BeforeMatching[[i]][3][1]))
  PreMean <- unlist(c(PreMean, balance$BeforeMatching[[i]][4][1]))
  PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[i]]$qqsummary[2]))
  PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[i]]$qqsummary[1]))
  PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[i]]$qqsummary[3]))
  
  PostMean <- unlist(c(PostMean, balance$AfterMatching[[i]][4][1]))
  PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[i]]$qqsummary[2]))
  PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[i]]$qqsummary[1]))
  PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[i]]$qqsummary[3]))
}



df <- data.frame("TrMean" = TrMean,
                 "TrMean2" = TrMean,
                 "PreMean" = PreMean,
                 "PreQQmed" = PreQQmed,
                 "PreQQmean" = PreQQmean,
                 "PreQQmax" = PreQQmax,
                 "PostMean" = PostMean,
                 "PostQQmed" = PostQQmed,
                 "PostQQmean" = PostQQmean,
                 "PostQQmax" = PostQQmax,
                 "names" = varnames) %>%
  mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
         change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
         change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
         change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), ~ comma(round(., 3), accuracy = .001)) %>%
  mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), ~ round(. * 100, 2)) %>% 
  filter(names != "voted_primary")


####

order <- fread("./raw_data/var_orders.csv") %>% 
  filter(variable != "reg_date")

df <- full_join(df, order, by = c("names" = "variable")) %>%
  arrange(order) %>%
  select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
  filter(!is.na(TrMean))


df <- df %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(name == "Median Income", dollar(round(as.numeric(gsub(",", "", .)))), .)) %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(name == "Age", round(as.numeric(.), 1), .)) %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(name == "Registration Date",
                     as.numeric(gsub(",", "", .)), .)) %>% 
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean),
            ~ ifelse(substring(name, 1, 1) == "%", percent(as.numeric(.), accuracy = .1), .))

colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")

saveRDS(df, "./temp/balance_table_full.rds")
