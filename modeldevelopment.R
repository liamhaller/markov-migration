#model devlopment


# Libraries ---------------------------------------------------------------

library(mHMMbayes)

##General model properities
m <- 2 #number of hidden states
ndep <- 1 #number of dependent variables
q_emiss <- unique(syria_sequence$country)


# specifying starting values
start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM <- list(matrix(c(0.05, 0.90, 0.05, 
                          0.90, 0.05, 0.05), byrow = TRUE,
                        nrow = m, ncol = q_emiss[1]), # vocalizing patient
                 matrix(c(0.1, 0.9, 
                          0.1, 0.9), byrow = TRUE, nrow = m,
                        ncol = q_emiss[2]), # looking patient
                 matrix(c(0.90, 0.05, 0.05, 
                          0.05, 0.90, 0.05), byrow = TRUE,
                        nrow = m, ncol = q_emiss[3]), # vocalizing therapist
                 matrix(c(0.1, 0.9, 
                          0.1, 0.9), byrow = TRUE, nrow = m,
                        ncol = q_emiss[4])) # looking therapist



# Run a model without covariate(s) and default priors:
set.seed(514)
out_2st <- mHMM(s_data = syria_mhmm, 
                gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss), 
                start_val = c(list(start_TM), start_EM),
                mcmc = list(J = 1000, burn_in = 200))



nonverbal
nonverbal_cov
