library(editrules)


f <- function(x, tuning = 1){
    
    x <- matrix(x, ncol = 1)
    
    A <- matrix(c(1, 1, 0, 0,
                  0, 0, 1, 1,
                  1, 0, 1, 0,
                  0, 1, 0, 1),
                byrow = T, nrow = 4)
    
    b <- matrix(c(203, 738, 55, 886), ncol = 1)
    
    
    SS_linear <- sum( (A %*% x - b)^2 )
    
    SS_quadratic <- sum( (x[1]*(b[4] - x[2]) - 2.6*x[2]*(b[3] - x[1]))^2 )
    
    SS_linear + tuning*SS_quadratic
}


sol <- optim(par = c(30, 173, 25, 713), fn = f, tuning = 1)

f(sol$par)

tuning <- 10^{seq(-1,1,by=0.01)}

sols <- lapply(tuning, FUN = function(x){optim(par = c(30, 173, 25, 713), fn = f, tuning = x)})

map(sols, "par") %>% unlist %>% matrix(byrow = T, ncol = 4) %>%
    as.data.frame %>%
    mutate(value = map_dbl(sols, "value")) %>%
    filter(value < 1) %>%
    gather(key, x, -value) %>%
    ggplot( data = ., aes(x = value, y = round(x))) +
    geom_point() +
    facet_wrap(~key, scales = 'free_y') +
    scale_x_log10(limits = c(NA, 1))
