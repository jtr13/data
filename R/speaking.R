# In order to obtain script generated values, speaking.html was manually saved as a
# complete web page from: https://www.nytimes.com/interactive/2019/10/15/us/elections/debate-speaking-time.html

library(tidyverse)
library(rvest)
robotstxt::paths_allowed("https://www.nytimes.com/interactive/2019/10/15/us/elections/debate-speaking-time.html")

heatmap <- read_html("speaking.html") %>%
  html_node("#palette")

issue_name <- heatmap %>%
  html_nodes(".minlabel") %>%
  html_nodes("text") %>%
  html_attr("class")

timelabel <- heatmap %>%
  html_nodes(".minlabel") %>%
  html_nodes("text") %>%
  html_text()

# ex. O'Rourke
names_human_readable <- heatmap %>%
  html_node(".yaxis") %>%
  html_nodes("g.tick") %>%
  html_text()

# ex. Gun control
issues_human_readable <- heatmap %>%
  html_node(".xaxis") %>%
  html_nodes("g.tick") %>%
  html_text()

# create strings for use in separating JavaScript classes # of the form
# "gun-controlorourke" into issues and candidates from issue_candidate

# ex. orourke
names_machine_readable <- names_human_readable %>%
  tolower() %>%
  str_replace_all("[:punct:]", "")

# ex. gun-control
issues_machine_readable <- issues_human_readable %>%
  tolower() %>%
  str_replace_all("[:punct:]", "") %>%
  str_replace(" ", "-")

dems <- tibble(issue_candidate, timelabel) %>%
  mutate(name = str_remove_all(issue_name, paste(issues_machine_readable, collapse = "|")),
         issue = str_remove_all(issue_name, paste(names_machine_readable, collapse = "|"))) %>%
  filter(name != "moderator") %>%
  separate(timelabel, into = c("minutes", "seconds"),
           sep = ":", remove = FALSE) %>%
  mutate(minutes = as.numeric(minutes),
         seconds = as.numeric(seconds),
         time = minutes*60 + seconds,
         candidate = map_chr(name, ~names_human_readable[names_machine_readable == .x]),
         topic = map_chr(issue, ~issues_human_readable[issues_machine_readable == .x])) %>%
  select(candidate, topic, time, timelabel)

write_csv(dems, "speakingtime.csv")
