allmap <- function(v1 = "vf1", v2 = "bc", v1min = 10, v1max = 80, v2min = 10, v2max = 80) { ## add code to check validity of argument names
        dat <- read.table("vaspfix_data_spaepi.csv", sep = "," ,skip = 2, na.strings = "")
        col.names <- c("id", "plt_edta", "plt_hi", "lta_max", "lta_4m", "mu_auc", "mu_ma", "mu_vel", "vf1", "vf5", "vf_ave", "bc", "dose", "spa_wba", "epi_wba", "epi_lta_max", "epi_lta_4m")
        colnames(dat) <- col.names
        v1v2.cc <- complete.cases(dat[[v1]], dat[[v2]]) ## Logical vector complete cases bc + vf1
        v1v <- dat[[v1]][v1v2.cc]
        v2v <- dat[[v2]][v1v2.cc]      ## Read in data and create v1v and v2v
        
        cohenk2 <- function(r1 = v1v, r2 = v2v) {
                agm <- matrix(data = 0, nrow = 2, ncol = 2, dimnames = c(list(c("v1+", "v1-")), list(c("v2+", "v2-"))))
                for (k in seq_along(r1)) {
                        if(r1[k] && r2[k]) agm[1,1] <- agm[1,1] + 1
                        if(!r1[k] && !r2[k]) agm[2,2] <- agm[2,2] + 1
                        if(r1[k] && !r2[k]) agm[1,2] <- agm[1,2] + 1
                        if(!r1[k] && r2[k]) agm[2,1] <- agm[2,1] + 1
                }
                p1res <- sum(agm[1,])/sum(agm)
                p2res <- sum(agm[,1])/sum(agm)
                p.res <- p1res*p2res
                p1nres <- sum(agm[2,])/sum(agm)
                p2nres <- sum(agm[,2])/sum(agm)
                p.nres <- p1nres*p2nres
                pag <- p.res+p.nres
                rate.ag <- (agm[1,1] + agm[2,2])/sum(agm)
                c.k <- (rate.ag - pag)/(1-pag)
                return(c.k)
        }
        
                v1.co.range <- seq(v1min, v1max, length = 36) ## Cut off range for BC
                v2.co.range <- seq(v2min, v2max, length = 36) ## Cut off range for VF
                v1.L <- length(v1.co.range)
                v2.L <- length(v2.co.range)
                output <- matrix(nrow = v1.L, ncol = v2.L, dimnames = c(list(v1.co.range), list(v2.co.range)))
                
                for (i in seq_along(v1.co.range)) {
                        v1.co <- v1.co.range[i]
                        
                        for (j in seq_along(v2.co.range)) {
                                v2.co <- v2.co.range[j]
                                r1 <- v1v < v1.co
                                r2 <- v2v < v2.co
                                output[i,j] <- cohenk2(r1 = r1, r2 = r2)                        
                        }
                }
                x <<- v1.co.range
                y <<- v2.co.range
                return(output) 
}

