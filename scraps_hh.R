# load Sweden

# census

list(# Swedish census data
    `752` = read_csv('data/BE0101C¤_20220729-160046.csv', skip = 1) %>%
        mutate(mean_hh = `00 Sweden Number of persons`/`00 Sweden Number of households`),
    
    # Danish census data 
    `208` = read_csv("data/denmark_pop_thousands.csv", skip = 3,
                     col_names = c("year", "pop")) %>%
        full_join(read_csv("data/denmark_households.csv",
                           skip = 2, col_names = c("year", "hh"))) %>%
        arrange(year) %>%
        group_by(year) %>%
        transmute(mean_hh = pop/hh * 1000),
    
    # CEIC data
    `352` = data.frame(year     = seq(2004, 2016),
                       mean_hh  = c(2.6, 2.5, 2.5, rep(2.4, 6), NA, 2.7, 2.7, 2.9)),
    
    # Statista
    `158` = data.frame(year     = seq(1990, 2020),
                       mean_hh  = c(4.19, 4.16, 4.1, 4.1, 4.02, 3.94, 3.92, 3.84, 3.77,
                                    3.63, 3.62, 3.58, 3.65, 3.53, 3.5, 3.42, 3.41,
                                    3.38, 3.35, 3.34, 3.25, 3.29, 3.23, 3.21, 3.15,
                                    3.1, 3.07, 3.07, 3.05, 3.02, 2.92)),
    
    # CEIC
    `144` = data.frame(year     = c(1981, 1986, 1991, 1996, 2001, 2019,
                                    2016, 2013, 2010, 2007, 2004 ),
                       mean_hh  = c(4.9,  5.1,  4.9,  4.5,  4.2,  3.7,
                                    3.8,  3.9,  4.0,  4.1, 4.1)),
    
    # Survey and census https://journals.sagepub.com/doi/10.1177/2158244020914556
    `682` = data.frame(year = c(1992, 2004, 2010, 2000, 2007, 2016),
                       mean_hh = c(6.6, 6, 6.3, 6.8, 6.0, 5.9))
) %>%
    map(~select(.x, year, mean_hh)) %>%
    bind_rows(.id = "iso_code") %>%
    mutate(iso_code = parse_integer(iso_code)) %>%
    arrange(iso_code, year)

hh_Sweden <- 
    read_csv('data/BE0101C¤_20220729-160046.csv', skip = 1) %>%
    mutate(mean_hh = `00 Sweden Number of persons`/`00 Sweden Number of households`) %>%
    select(year, mean_hh) %>%
    mutate(iso_code = 752L)

# census
hh_Denmark <-
    read_csv("data/denmark_pop_thousands.csv", skip = 3,
             col_names = c("year", "pop")) %>%
    full_join(read_csv("data/denmark_households.csv", skip = 2, col_names = c("year", "hh"))) %>%
    arrange(year) %>%
    group_by(year) %>%
    transmute(mean_hh = pop/hh * 1000) %>%
    mutate(iso_code = 208L)

# CEIC data
hh_Iceland <-
    data.frame(year     = seq(2004, 2016),
               mean_hh  = c(2.6, 2.5, 2.5, rep(2.4, 6), NA, 2.7, 2.7, 2.9),
               iso_code = 352L)

# statista 
hh_Taiwan <- 
    data.frame(year     = seq(1990, 2020),
               mean_hh  = c(4.19, 4.16, 4.1, 4.1, 4.02, 3.94, 3.92, 3.84, 3.77,
                            3.63, 3.62, 3.58, 3.65, 3.53, 3.5, 3.42, 3.41,
                            3.38, 3.35, 3.34, 3.25, 3.29, 3.23, 3.21, 3.15,
                            3.1, 3.07, 3.07, 3.05, 3.02, 2.92),
               iso_code = 158L)

# CEIC data
hh_SriLanka <-
    data.frame(year     = c(1981, 1986, 1991, 1996, 2001, 2019, 2016, 2013, 2010, 2007, 2004 ),
               mean_hh  = c(4.9,  5.1,  4.9,  4.5,  4.2,  3.7,  3.8,  3.9,  4.0,  4.1, 4.1),
               iso_code = 144L) %>%
    arrange(year)

# https://journals.sagepub.com/doi/10.1177/2158244020914556
hh_SaudiArabia <-
    data.frame(year = c(1992, 2004, 2010, 2000, 2007, 2016),
               mean_hh = c(6.6, 6, 6.3, 6.8, 6.0, 5.9),
               iso_code = 682L) %>%
    arrange(year)



cutoffs    <- c(1, 2, 4, 6, 1000)
group_prob <- c(17.45, 38.67, 28.23, 15.65)/100


get.KL.div.nb = function(x, cutoffs, group_prob)
{
    exp_freq = diff( pnbinom(cutoffs, size = x[1], mu = x[2]))
    KL = sum(group_prob * log(group_prob / exp_freq))
    return(KL)
}

library(extraDistr)

get.KL.div.pois = function(x, cutoffs, group_prob)
{
    exp_freq = diff( ptpois(cutoffs + 0.1, lambda = x, a = 0))
    KL = sum(group_prob * log(group_prob / exp_freq))
    return(KL)
}

result_nb = optim(par = c(size = 1, mu = 0.5), 
                     fn = get.KL.div.nb, method = "Nelder-Mead",
                     cutoffs = cutoffs, group_prob=group_prob)

q <- seq(0, 10)
p_nb <- pnbinom(q = q, size = result_nb$par[1], mu = result_nb$par[2])

p_nb <- (p_nb - p_nb[1])/(1 - p_nb[1])

plot(q, p_nb)
points(tail(cutoffs,-1), cumsum(group_prob), pch = 3)



result_pois = optim(par = c(lambda = 3), 
                  fn = get.KL.div.pois, method = "Brent", lower = 1, upper = 10,
                  cutoffs = cutoffs, group_prob=group_prob)

p_pois <- ptpois(q = q, lambda = result_pois$par[1], a = 0)
# p_pois <- (p_pois - p_pois[1])/(1 - p_pois[1])

points(q, p_pois, col = 'red')
points(tail(cutoffs,-1), cumsum(group_prob), pch = 3)



cutoffs    <- c(0, 1, 2, 4, 6)
group_prob <- c(0, 17.45, 38.67, 28.23, 15.65)/100
