names(respicar_socio) %>%
    grep('filled_year', ., value = T) %>%
    c("id", "Year started") %>%
    select(.data = respicar_socio, all_of(.)) %>%
    rename(year_study = `Year started`) %>%
    distinct %>%
    gather(variable, year_covar, -id, -year_study) %>%
    mutate(staleness = year_study - year_covar) %>%
    ggplot(data = . , aes(x = staleness))  +
    geom_histogram(binwidth = 1, center = 0) +
    facet_wrap(~variable)

%>%
    nest(data = -variable) %>%
    mutate(variable = sub('_filled_year', '', variable)) %>%
    # mutate(S   = map(data, ~summarise_at(.x, 
    #                                      .vars = vars(staleness),
    #                                      .funs = list(Min    = min,
    #                                                   Mean   = mean,
    #                                                   Median = median,
    #                                                   Max    = max)))) %>%
    mutate(S   = map(data, ~boxplot.stats(.x$staleness, coef = 0))) %>%
    mutate(S = map(S, ~setNames(.x$stats, c('lower', 'Q2', 'median', 'Q4', 'upper')))) %>%
    unnest_wider(S) %>%
    select(-data) %>%
    ggplot(data = ., aes(x = variable, y = median)) +
    geom_pointrange(aes(ymin = lower, ymax = upper)) +
    geom_linerange(aes(ymin = Q2, ymax = Q4),
                   size = 2) +
    coord_flip() +
    theme_minimal(base_family = 'Lato') +
    xlab("Variable") + 
    ylab("Staleness of imputed variable\n(years since study that variable was measured)")
