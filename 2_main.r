
library(tidyverse)
library(patchwork)


#figure 1

#This figure shows raw relationships, across three different ways of displaying it. 
#I reproduce largely the same figure three times, added together with patchwork

#first the row with non-standardized LOESS curves
#preparing data
wvs6_cut %>%
  select(V192, V193, V194, V195, V196, V197, extraversion, agreeableness, conscientiousness, neuroticism, openness) %>% 
  rename(
    "Making our lives better" = "V192",
    "Opportunities for next generation" = "V193",
    "Depend too much on science (rev)" = "V194",
    "Breaks down right/wrong (rev)" = "V195",
    "Important to know about" = "V196",
    "The world is better off" = "V197",
    "negative\nemotionality" = "neuroticism"
  ) %>% 
  pivot_longer(7:11, names_to = "trait", values_to = "tvalue") %>% 
  pivot_longer(1:6, names_to = "attitude", values_to = "avalue") %>%
  mutate(
    attitude = fct_inorder(attitude),
    trait = str_to_sentence(trait),
    trait = str_replace(trait, "Conscientiousness", "Conscien-\ntiousness")
  ) %>% 
#and plotting  
  ggplot() +
  geom_smooth(
    aes(x = tvalue, y = avalue, fill = attitude),
    color = aucolr::picker()
  ) +
  facet_grid(.~trait, scales = "free_x") +
  jtools::theme_nice(legend.pos = "top") +
  ylab("Attitude value\nloess") +
  xlab("") +
  theme(
    axis.title.y = element_text(size =13),
    strip.text.x = element_text(size =13)
  ) +

#Second the row with standardized LOESS curves
wvs6_cut %>%
  select(V192s, V193s, V194s, V195s, V196s, V197s, extraversion, agreeableness, conscientiousness, neuroticism, openness) %>% 
  rename(
    "Making our lives better" = "V192s",
    "Opportunities for next generation" = "V193s",
    "Depend too much on science (rev)" = "V194s",
    "Breaks down right/wrong (rev)" = "V195s",
    "Important to know about" = "V196s",
    "The world is better off" = "V197s",
    "negative\nemotionality" = "neuroticism"
  ) %>% 
  pivot_longer(7:11, names_to = "trait", values_to = "tvalue") %>% 
  pivot_longer(1:6, names_to = "attitude", values_to = "avalue") %>% 
  mutate(
    attitude = fct_inorder(attitude),
    trait = str_to_sentence(trait),
    trait = str_replace(trait, "Conscientiousness", "Conscien-\ntiousness")
  ) %>% 
  
  ggplot() +
  geom_smooth(
    aes(x = tvalue, y = avalue, fill = attitude),
    color = aucolr::picker()
  ) +
  facet_grid(.~trait, scales = "free_x") +
  jtools::theme_nice(legend.pos = "top") +
  ylab("Standardized\n loess") +
  xlab("") + 
  theme(
    axis.title.y = element_text(size =13),
    strip.text = element_blank()
  ) +

#Third the row with standardized linear curves
wvs6_cut %>%
  select(V192s, V193s, V194s, V195s, V196s, V197s, extraversion, agreeableness, conscientiousness, neuroticism, openness) %>% 
  rename(
    "Making our lives better" = "V192s",
    "Opportunities for next generation" = "V193s",
    "Depend too much on science (rev)" = "V194s",
    "Breaks down right/wrong (rev)" = "V195s",
    "Important to know about" = "V196s",
    "The world is better off" = "V197s",
    "negative\nemotionality" = "neuroticism"
  ) %>% 
  pivot_longer(7:11, names_to = "trait", values_to = "tvalue") %>% 
  pivot_longer(1:6, names_to = "attitude", values_to = "avalue") %>% 
  mutate(
    attitude = fct_inorder(attitude),
    trait = str_to_sentence(trait),
    trait = str_replace(trait, "Conscientiousness", "Conscien-\ntiousness")
  ) %>% 
  
  ggplot() +
  geom_smooth(
    aes(x = tvalue, y = avalue, fill = attitude),
    color = aucolr::picker(),
    method = "lm"
  ) +
  facet_grid(.~trait, scales = "free_x") +
  jtools::theme_nice(legend.pos = "top") +
  ylab("Standardized\nlinear") +
  xlab("Trait value (standardized)") + 
  theme(
    axis.title.y = element_text(size =13),
    strip.text = element_blank()
  ) +
  
#additional arguments for the collected plots
  plot_layout(ncol = 1, guides = "collect") & theme(legend.position = "top", legend.title = element_blank())

#saving the figure
ggsave("figures/fig1.tiff", width = 24, height = 16, units = "cm", dpi = 300)


#figure 2

#This figure shows regression coefficients for the additive as well as individual items

#Coefficients are extracted using broom
bind_rows(
  broom::tidy(lm(data = wvs6_cut, sciatts ~ country+sex+age+extraversion+agreeableness + conscientiousness + neuroticism + openness + educont)) %>% 
    mutate(model = "Additive measure"),
  broom::tidy(lm(data = wvs6_cut, V192s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont)) %>% 
    mutate(model = "Making our lives better"),
  broom::tidy(lm(data = wvs6_cut, V193s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont)) %>% 
    mutate(model = "Opportunities for next generation"),
  broom::tidy(lm(data = wvs6_cut, V194s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont)) %>% 
    mutate(model = "Depend too much on science (rev)"),
  broom::tidy(lm(data = wvs6_cut, V195s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont)) %>% 
    mutate(model = "Breaks down right/wrong (rev)"),
  broom::tidy(lm(data = wvs6_cut, V196s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont)) %>% 
    mutate(model = "Important to know about"),
  broom::tidy(lm(data = wvs6_cut, V197s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont)) %>% 
    mutate(model = "The world is better off")
) %>% 
  mutate(model = fct_inorder(model)) %>% 
  filter(term == "extraversion" | term == "agreeableness" | term == "conscientiousness" | term == "neuroticism" | term == "openness") %>% 
  mutate(
    term = str_replace(term, "neuroticism", "negative\nemotionality"),
    term = str_to_sentence(term)
  ) %>% 
#And then plotted
  ggplot(aes(x = estimate, y = model, fill = model, alpha = model)) +
  
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_pointrange(
    aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error),
    color = aucolr::picker(), 
    shape = 21
  ) +
  ylab("") +
  xlab("") +
  facet_wrap(term~., nrow = 2) +
  scale_fill_manual(values = c(aucolr::picker(), "white", "white", "white", "white", "white", "white")) +
  scale_alpha_manual(values = c(1, .6,.6,.6,.6,.6,.6)) +
  scale_y_discrete(limits=rev) +
  jtools::theme_nice(legend.pos = "none") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(hjust = 1, size =13),
    strip.text = element_text(size = 13)
  )

#saving the figure
ggsave("figures/fig2.tiff", width = 24, height = 12, units = "cm", dpi = 300)



