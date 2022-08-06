df %>% mutate(
  grades0 = `Total Score` ,
  grades = (grades0 - mean(grades0, na.rm = TRUE)) / sd(grades0, na.rm =
                                                        TRUE),
  grades1 = (grades) * 15 + 57,
  grades2 = ifelse(grades1 < 40, (40 - grades1) * 0.6 + grades1, grades1),
  grades2 = ifelse(grades2 >= 80, 80, grades2),
  diff_gr = grades2 - grades0 ) %>%  select(grades0, grades2, `First Name`, `Last Name`) %>% write_csv("midterm2_adj.csv")
  filter(!is.na(grades2)) %>% with(hist(grades2, breaks =20))
  
  
  %>%  select(grades0, grades1,grades2, diff_gr, `First Name`, `Last Name`) %>% View()
  summarise(sd(grades2, na.rm = TRUE),
                                        mean(grades2),
                                        min(grades2),
                                        max(grades2))

with(plot(grades0, diff_gr))
  
  select(grades0, grades1,grades2, diff_gr, `First Name`, `Last Name`) %>% View()

summarise(sd(grades2, na.rm = TRUE),
                                              mean(grades2),
                                              min(grades2),
                                              max(grades2))

  
  
  filter(!is.na(grades2)) %>% 
  
  
  
  
  with(plot(grades0, diff_gr))
  
  
  
  
  summarise(sd(grades2, na.rm = TRUE),
                                            mean(grades2),
                                            min(grades2),
                                            max(grades2))