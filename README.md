shit happens index
================

Estimating coefficients for measuring psychological distress resulting
from various life events

<br>

## Extension and validation of stressful life events schedule

Instruments for measuring the severity of psychological distress
resulting from recent life experiences were originally developed in the
1960s, and are used by clinicians today to identify and predict sources
of patient stress. The instrument consists of a checklist containing a
list of major life events (e.g., divorce, bankruptcy, death of a loved
one), and a weighting is assigned to each item that was determined by
asking research volunteers to anticipate the severity of stress that was
expected from its occurrence. However people are notoriously poor at
anticipating their emotional reaction to significant events, and large
population-based surveys can now provide data which tracks self-reported
distress levels after survey respondants have experienced the life
event. This provides an opportunity to calculate weights based on
experienced distress levels rather than anticipated distress, and so can
provide a way to base clinical predictions on empirical evidence. Our
other work (`shit-happens`) has determined the causal effect of major
life events on clinically-relevant distress measures in large
population-based survey data. We have also developed methods and
resources to interrogate large datasets and calculate the necessary
regression weights in new data. Ethics permission to access two large
candidate public datasets are in place. This project requires requesting
access to the data, cross-validation of the regression weights, and the
results will form the basis of a publication to update current clinical
instruments to improve care in Australia.

Holmes and Rahe’s Social Readjustment Scale (Holmes and Rahe, 1967)
weights marriage as the sixth most stressful event yet we found no
negative impact on affective wellbeing and a profound anticipatory and
subsequent positive effect on life satisfaction.

Epidemiological research often assesses the impact of life-events
through summed checklists that treat events as equal or just evaluates
the impact of one event (Dohrenwend, 2006; Gray et al., 2004; Wethington
et al., 1997).

``` r
mhi5 <- read_rds("data/mhi5_by_all_lifeevents.rds")

# This includes all events, but we may want to select events with MHi5 observations
# before and after the event (i.e., balanced)
```

``` r
unconditional_fit <- function(.df) {
  
  .df %>%
    group_by(xwaveid) %>%
    mutate(y = c(scale(ghmh))) %>%
    ungroup() %>%
    mutate(
      time = fct_relevel(time, "pre36", "pre24", "pre12"),
      y = replace_na(y, 0)
    ) -> df
  
  ghmh_m <- select(df, xwaveid, year, ghmh) %>%
    distinct() %>%
    group_by(xwaveid) %>%
    summarise(m = mean(ghmh)) %>%
    mutate(sm = c(scale(m)))
  
  df <- left_join(df, ghmh_m)
  
  lmer(y ~ 0 + time + m + (1|xwaveid) + (1|wave), data = df)
  
}

fits <- map(mhi5, 
            .f = ~unconditional_fit(.))

write_rds(fits, "results/lmm_fits.rds")
```

``` r
fits <- read_rds("results/lmm_fits.rds")
```

``` r
map(fits, ~fixef(.) %>% as_tibble(rownames = "coef")) %>%
  bind_rows(.id = "event") %>%
  filter(coef != "m") %>%
  mutate(time = str_remove(coef, "time"),
         time = fct_relevel(time, "pre36", "pre24", "pre12")) %>% 
  group_by(event) %>%
  mutate(worst = min(value),
         `pre` = value[time == "pre12"],
         `3mo` = value[time == "post03"],
         `12mo` = value[time == "post12"]) %>% 
  ungroup() %>%
  mutate(event = fct_reorder(event, `3mo`)) %>%
  ggplot(aes(x = time, y = value)) +
    geom_line(group = 1) +
    facet_wrap(~event) +
    guides(x =  guide_axis(angle = 33)) +
    labs(subtitle = "Ordered by 3 mo effect", x = "") +
    theme_minimal()
```

![](results/plot_fits-1.png)<!-- -->

<br><br>

#### Table 1. Weights for life events

``` r
map(fits, ~tidy(., effects = "fixed", conf.int = T)) %>%
  bind_rows(.id = "event") %>%
  filter(term %in% c("timepre12", "timepost03", "timepost12")) %>%
  transmute(
    event,
    term = str_remove(term, "time"),
    estimate = if_else(0 > conf.low & 0 < conf.high, 
                       as.character(round(-1*estimate, 2)), 
                       paste0(round(-1*estimate, 2), "*"))
  ) %>%
  spread(term, estimate) %>%
  mutate(val = parse_number(post03),
         event = fct_reorder(event, val)) %>%
  arrange(desc(event)) %>%
  select(`Life event` = event, pre12, post03, post12) %>%
  flextable() %>%
  footnote(
    i = 1, j = 1,
    ref_symbols = "",
    value = as_paragraph("*p < .05")
  ) %>%
  autofit() %>%
  save_as_image(path = "results/weight_table_1.png")
```

![table 1](results/weight_table_1.png "Table 1")

<br><br>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-dohrenwend2006inventorying" class="csl-entry">

Dohrenwend, B.P., 2006. Inventorying stressful life events as risk
factors for psychopathology: Toward resolution of the problem of
intracategory variability. Psychological bulletin 132, 477.

</div>

<div id="ref-gray2004psychometric" class="csl-entry">

Gray, M.J., Litz, B.T., Hsu, J.L., Lombardo, T.W., 2004. [Psychometric
properties of the life events
checklist](https://www.ncbi.nlm.nih.gov/pubmed/15486169). Assessment 11,
330–341.

</div>

<div id="ref-holmes1967social" class="csl-entry">

Holmes, T.H., Rahe, R.H., 1967. The social readjustment rating scale.
Journal of psychosomatic research.

</div>

<div id="ref-wethington1995interview" class="csl-entry">

Wethington, E., Brown, G.W., Kessler, R.C., 1997. Interview measurement
of stressful life events, in: Cohen, S., Kessler, R.C., Gordon, L.U.
(Eds.), Measuring Stress: A Guide for Health and Social Scientists.
Oxford University Press, Oxford, UK, pp. 59–79.

</div>

</div>
