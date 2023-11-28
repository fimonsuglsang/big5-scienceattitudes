


# corelation matrix ------------------------------------------------------------------

corrtable::save_correlation_matrix(
  wvs6_cut %>% select(extraversion, agreeableness, conscientiousness, neuroticism, openness, V192:V197, sciatts),
  digits = 2,
  "tables/a2.csv"
) 


# regression tables -------------------------------------------------------

modelsummary::modelsummary(
  list(
    lm(data = wvs6_cut, sciatts ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    lm(data = wvs6_cut, V192s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    lm(data = wvs6_cut, V193s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    lm(data = wvs6_cut, V194s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    lm(data = wvs6_cut, V195s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    lm(data = wvs6_cut, V196s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    lm(data = wvs6_cut, V197s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness)
  ),
  fmt = 2,
  stars = T,
  output = "flextable"
) %>% flextable::save_as_docx(path = "tables/a4.docx")



# regressions with latent science attitude measures -----------------------------------------------------------------

modelsummary::modelsummary(
  list(
    "additive" = lm(data = wvs6_cut, sciatts ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    "factor scores" = lm(data = fadata, sciatts ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness),
    "principal component" = lm(data = pcadata, sciatts ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness)
  ),
  fmt = 2,
  stars = T,
  output = "flextable"
) %>% flextable::save_as_docx(path = "tables/a5.docx")



# regression with world-views -------------------------------------------------------

#first producing the regression table
modelsummary::modelsummary(
  list(
    lm(data = wvs6_cut, sciatts ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness +pol+rel),
    lm(data = wvs6_cut, V192s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness +pol+rel),
    lm(data = wvs6_cut, V193s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness +pol+rel),
    lm(data = wvs6_cut, V194s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness +pol+rel),
    lm(data = wvs6_cut, V195s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness +pol+rel),
    lm(data = wvs6_cut, V196s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness +pol+rel),
    lm(data = wvs6_cut, V197s ~ country+sex+age+educont+extraversion + agreeableness + conscientiousness + neuroticism + openness +pol+rel)
  ),
  fmt = 2,
  stars = T,
  output = "flextable"
) %>% flextable::save_as_docx(path = "tables/a6.docx")

#then a seperate table showing differences
left_join(
  
  #extracting coefficients with no world-view controls
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
      term = str_to_sentence(term),
      v = "raw"
    ) %>% 
    select(term, model, raw_e = estimate),
  
  #extracting coefficients with world-view controls
  bind_rows(
    broom::tidy(lm(data = wvs6_cut, sciatts ~ country+sex+age+extraversion+agreeableness + conscientiousness + neuroticism + openness + educont+pol+rel)) %>% 
      mutate(model = "Additive measure"),
    broom::tidy(lm(data = wvs6_cut, V192s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont+pol+rel)) %>% 
      mutate(model = "Making our lives better"),
    broom::tidy(lm(data = wvs6_cut, V193s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont+pol+rel)) %>% 
      mutate(model = "Opportunities for next generation"),
    broom::tidy(lm(data = wvs6_cut, V194s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont+pol+rel)) %>% 
      mutate(model = "Depend too much on science (rev)"),
    broom::tidy(lm(data = wvs6_cut, V195s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont+pol+rel)) %>% 
      mutate(model = "Breaks down right/wrong (rev)"),
    broom::tidy(lm(data = wvs6_cut, V196s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont+pol+rel)) %>% 
      mutate(model = "Important to know about"),
    broom::tidy(lm(data = wvs6_cut, V197s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + educont+pol+rel)) %>% 
      mutate(model = "The world is better off")
  ) %>% 
    mutate(model = fct_inorder(model)) %>% 
    filter(term == "extraversion" | term == "agreeableness" | term == "conscientiousness" | term == "neuroticism" | term == "openness") %>% 
    mutate(
      term = str_replace(term, "neuroticism", "negative\nemotionality"),
      term = str_to_sentence(term),
      v = "ww"
    ) %>% 
    select(term, model, ww_e = estimate)
) %>% 
  #getting the difference
  mutate(d = raw_e-ww_e) %>% 
  select(-raw_e, -ww_e) %>% 
  #restructuring and saving as table
  pivot_wider(names_from = term, values_from = d) %>% 
  modelsummary::datasummary_df(output = "flextable") %>% 
  flextable::save_as_docx(path = "tables/a7.docx")

# education splits --------------------------------------------------------

#getting coefficients for no uni respondents 
bind_rows(
  bind_rows(
    broom::tidy(lm(data = wvs6_cut %>% filter(edu<= 7), sciatts ~ country+sex+age+extraversion+agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Additive measure"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu<= 7), V192s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Making our lives better"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu<= 7), V193s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Opportunities for next generation"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu<= 7), V194s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Depend too much on science (rev)"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu<= 7), V195s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Breaks down right/wrong (rev)"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu<= 7), V196s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Important to know about"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu<= 7), V197s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "The world is better off")
  ) %>% 
    mutate(model = fct_inorder(model)) %>% 
    mutate(cut = "No uni"),
  
  
  #getting coefficients for at least som uni respondents 
  bind_rows(
    broom::tidy(lm(data = wvs6_cut %>% filter(edu>= 8), sciatts ~ country+sex+age+extraversion+agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Additive measure"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu>= 8), V192s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Making our lives better"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu>= 8), V193s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Opportunities for next generation"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu>= 8), V194s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Depend too much on science (rev)"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu>= 8), V195s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Breaks down right/wrong (rev)"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu>= 8), V196s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "Important to know about"),
    broom::tidy(lm(data = wvs6_cut %>% filter(edu>= 8), V197s ~ country+sex+age+extraversion + agreeableness + conscientiousness + neuroticism + openness + rel + pol + educont)) %>% 
      mutate(model = "The world is better off")
  ) %>% 
    mutate(model = fct_inorder(model)) %>% 
    mutate(cut = "At least some uni")
) %>% 
  
  #extracting relevant data
filter(term == "extraversion" | term == "agreeableness" | term == "conscientiousness" | term == "neuroticism" | term == "openness") %>% 
  mutate(
    term = str_replace(term, "neuroticism", "negative\nemotionality"),
    term = str_to_sentence(term)
  ) %>% 
  
  #plotting
  ggplot(aes(x = estimate, y = model, shape = model, alpha = model, color = cut)) +
  
  geom_vline(xintercept = 0, linetype = "longdash") +

  geom_pointrange(
    aes(xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error),
    fill = "white",
    position = position_dodge(.6)
  ) +
  
  ylab("") +
  xlab("") +
  
  guides(alpha = "none", shape = "none") +
  
  facet_wrap(term~., nrow = 2) +
  scale_shape_manual(values = c(19, rep(21,6))) +
  scale_alpha_manual(values = c(1,.6,.6,.6,.6,.6,.6)) +
  scale_color_manual(values = aucolr::picker(c("blue", "orange"))) +
  scale_y_discrete(limits=rev) +
  jtools::theme_nice(legend.pos = "bottomright") +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(hjust = 1, size =13),
    legend.title = element_blank(),
    strip.text = element_text(size = 13)
  )
  
#saving figure
ggsave("figures/figA1.tiff", width = 24, height = 15, units = "cm", dpi = 300)
  
  
  