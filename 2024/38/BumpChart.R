#----------------------------------
### Bump chart of ranks of top 7 characters in Romeo and Juliet by lines spoken

#----------------------------------



library(tidyverse)
library(ggtext)
library(ggbump)
tuesdata <- tidytuesdayR::tt_load(2024, week = 38)

romeo_juliet <- tuesdata$romeo_juliet

romeo_juliet <- romeo_juliet |> 
  filter(character != "[stage direction]")

Lines2<- romeo_juliet |> 
  group_by(character, act) |> 
  summarise(n=n()) |> 
  ungroup() |> 
  arrange(act, desc(n)) |> 
  group_by(act) |> 
  slice_max(n=7, order_by = n) |> 
  ungroup() |> 
  mutate(Rank = rep(1:7, 5),
         Act = rep(1:5, each=7, length.out=35),
         color = case_when(character=="Romeo" ~ '#0057B8',
                           character== "Juliet" ~ '#FDBE11',
                           TRUE ~ "gray40"))

caption = "Chart by @DataAngler@vis.social | Tidy Tuesday Wk. 38"
ggplot(Lines2, aes(x = Act, y = desc(Rank) , group=character, color = I(color))) +
  geom_bump(size = 1.5) +
  geom_point(size = 3) +
  geom_text(data = Lines2 %>% filter(Act == min(Act)),
            aes(Act = Act - 0.05, label = character),
            size = 3, hjust = -.2, vjust=2) +
  geom_text(data = Lines2 %>% filter(Act == max(Act)),
            aes(Act = Act + 0.05, label = character),
            size = 3, hjust = .8, vjust=2) +
  labs(y = "Rank by Number of Lines Spoken", title = "Rank of Characters in <em>Romeo and Juliet</em> by Number of Lines",
       subtitle = "With labels for top 7 in Acts I and V", caption = caption) +
  scale_y_continuous(breaks = c(-7,-6,-5,-4,-3,-2,-1), labels  = c("7", "6", "5","4", "3", "2", "1"))+
  theme_minimal() +
  theme(legend.position = "none",
        plot.margin = unit(c(.25,2,0,2), "cm"),
        plot.caption  = element_text(hjust = 1, size = 9),
        plot.title = element_markdown())

ggsave("BumpRomeoJuliet.png", dpi=300, width=6, height=5, bg="beige")

