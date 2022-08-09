
make_appendix_summary(dta_full) %>%
    filter(!is.na(Total) & !is.na(Positive)) %>%
    group_by(Country, `ISO 3166-1`) %>%
    summarise(Min = min(`Year started`),
              Max = max(`Year ended`)) %>%
    filter(grepl(
        pattern = "(Sudan|slav|Israel|Palest|Croatia|Serbia|Montenegro|Bosnia|Herz|Kosov|FYR|Macedonia|Ethiopia|Eritrea|Somal|Namibia|Hong|Yemen|Kuwait|Iraq|Macau|Czech|Slovak|Russia|Soviet|Latvia|Lithuania|Estonia|Belarus|Ukraine|Moldova|Georgia|Armenia|Azerbaijan|Germany|Palau)",
        x = Country)) %>%
    knitr::kable(format = 'markdown')
