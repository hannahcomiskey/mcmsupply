#' Writes the JAGS code for the national and subnational models
#' @name write_jags_model
#' @param model_type String. Two options, "subnational" for subnational-level JAGS code or "national" for national-level JAGS code.
#' @param local TRUE/FALSE. Default is FALSE. local=FALSE retrieves the data for all subnational provinces across all countries. local=TRUE retrieves data for only one country.
#' @param spatial TRUE/FALSE. Default is FALSE. spatial=FALSE retrieves the data for all subnational provinces across all countries without GPS information. spatial=TRUE retrieves for data for countries with GPS information as well as FP source data.
#' @return returns a list ready for input into the JAGS model
#' @export

write_jags_model <- function(model_type, local=FALSE, spatial=FALSE) {
  if(model_type == "national") {
    if(local==FALSE) { # global national model
      cat(
        "model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for rho
    for(i in 1:M_count){ # Method loop i
      mu_delta[g,i] <- 0
      sd_delta[g,i] ~ dunif(0,1)
      sigma_delta[i,i,g] <- pow(sd_delta[g,i],2)
    }

    sigma_delta[1,2,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[1,3,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[1,4,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[1,5,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[2,1,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[2,3,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[2,4,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[2,5,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[3,1,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[3,2,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[3,4,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[3,5,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[4,1,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[4,2,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[4,3,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[4,5,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
    sigma_delta[5,1,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[5,2,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[5,3,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[5,4,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
  } # end G loop

  inv.sigma_delta[1:M_count,1:M_count,1] <- inverse(sigma_delta[,,1])
  inv.sigma_delta[1:M_count,1:M_count,2] <- inverse(sigma_delta[,,2])


## Model Estimates
  for(c in 1:C_count) { # country loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) {
        z[m,c,t] <- alpha_cms[1,m,c] + inprod(B.ik[c,t,],beta.k[1,m,c,])
        r[m,c,t] <- alpha_cms[2,m,c] + inprod(B.ik[c,t,],beta.k[2,m,c,])
      } # end time loop
    } # end M loop
  } # end C loop

## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(c in 1:C_count) { # country loop
      for(m in 1:M_count){ # method loop
        alpha_cms[s,m,c] ~ dnorm(beta_r[s,m,matchregion[c]],tau_alpha[s]) # sharing info across methods within a country so each country public/private sector has an intercept. Tau-alpha is the cross-method variance.
        beta.k[s,m,c,kstar[c]] <- 0 # kstar[c] is the knot at tstar for country c. Just the value of the intercept
        for(j in 1:(kstar[c]-1)){ # before kstar[c]
          beta.k[s,m,c,(kstar[c] - j)] <- beta.k[s,m,c,(kstar[c] - j)+1] - delta.k[s,m,c,(kstar[c] - j)]
        } # end K1 loop
        for(j in (kstar[c]+1):K){ # after kstar[c]
          beta.k[s,m,c,j] <- beta.k[s,m,c,(j-1)] + delta.k[s,m,c,(j-1)]
        } # end K2 loop
      } # end m loop

      for(j in 1:H){
        delta.k[s,c(1:M_count),c,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s]) # delta are the slopes for country c and method m
      } # end H loop
    } # end C loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(c in 1:C_count){ # country loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,c,t] <- 1/(1+exp(-(z[m,c,t]))) # modelling this as before assuming that z[m,c,t] is log(pi_public/pi_private)

         Q[m,c,t] <- 1/(1+exp(-(r[m,c,t]))) # logit-inverse of ratio

         U[m,c,t] <- 1-P[1,m,c,t] # this then gives you the total private sector

         P[2,m,c,t] <- Q[m,c,t]*U[m,c,t] # this is assuming that the logit(P[2,m,c,t]/U[m,c,t]) = r[m,c,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,c,t] <- U[m,c,t] - P[2,m,c,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchcountry[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchcountry[k],matchyears[k]], tau_y[k,2])T(0,1)

    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    for(m in 1:M_count){ # method loop
      for(r in 1:R_count){ # regional
        beta_r[s,m,r] ~ dnorm(beta_world[s,m],tau_beta[s])
      }
       # world intercept
       beta_world[s,m] ~ dnorm(0,0.1)
    }
    tau_alpha[s] <- sigma_alpha[s]^-2
    sigma_alpha[s] ~ dt(0,1,1)T(0,)

    tau_beta[s] <- sigma_beta[s]^-2
    sigma_beta[s] ~ dt(0,1,1)T(0,)
  }
}", file="model.txt")
    } else { # national local
      cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,crivate) loop for covariance
    mu_delta[g,1:M_count] <- rep(0,M_count)
    inv.sigma_delta[1:M_count,1:M_count,g] ~ dwish(natdf*natRmat[1:M_count,1:M_count,g],natdf) # S~dwish(R,c) => E(S) = p * solve(R)
  }

## Model Estimates
for(m in 1:M_count){ # method loop
  for (t in 1:n_years) {
    z[m,t] <- alpha_cms[1,m] + inprod(B.ik[t,],beta.k[1,m,])
    r[m,t] <- alpha_cms[2,m] + inprod(B.ik[t,],beta.k[2,m,])
  } # end **after tstar** loop
} # end M loop

## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(m in 1:M_count){ # method loop
      alpha_cms[s,m] ~ dnorm(alphahat_region[s,m], tau_alphahat_cms[s]) # sharing info across methods within a subnat area so each subnat public/private sector has an intercept. Tau-alpha is the cross-method sector variance.
      beta.k[s,m,kstar] <- 0 # kstar is the knot at tstar for country c. Just the value of the intercept
      for(j in 1:(kstar-1)){ # before kstar
        beta.k[s,m,(kstar - j)] <- beta.k[s,m,(kstar - j)+1] - delta.k[s,m,(kstar - j)]
      } # end K1 loop
      for(j in (kstar+1):K){ # after kstar
        beta.k[s,m,j] <- beta.k[s,m,(j-1)] + delta.k[s,m,(j-1)]
      } # end K2 loop
    } # end m loop
    for(j in 1:H){
      delta.k[s,c(1:M_count),j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s]) # delta are the slopes for country c and method m
    } # end H loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(m in 1:M_count){ # method loop
    for (t in 1:n_years) { # years loop
      P[1,m,t] <- 1/(1+exp(-(z[m,t]))) # modelling this as before assuming that z[m,c,t] is log(pi_public/pi_private)
      Q[m,t] <- 1/(1+exp(-(r[m,t]))) # logit-inverse of ratio
      U[m,t] <- 1-P[1,m,t] # this then gives you the total private sector
      P[2,m,t] <- Q[m,t]*U[m,t] # this is assuming that the logit(P[2,m,c,t]/U[m,c,t]) = r[m,c,t] i.e., we are modelling the ratio of private medical to private
      P[3,m,t] <- U[m,t] - P[2,m,t] # other = private - private medical
      }
    }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchyears[k]], tau_y[k,2])T(0,1)
    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
  }
}", file="model.txt")
    }
  } else {
    if(model_type=="subnational") {
      if(local==FALSE) {
        if(spatial==FALSE){ # subnational global nonspatial
          cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for rho
    for(i in 1:M_count){ # Method loop i
      mu_delta[g,i] <- 0
      sd_delta[g,i] ~ dt(0,1,1)T(0,)
      sigma_delta[i,i,g] <- pow(sd_delta[g,i],2)
    }

    sigma_delta[1,2,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[1,3,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[1,4,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[1,5,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[2,1,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[2,3,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[2,4,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[2,5,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[3,1,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[3,2,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[3,4,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[3,5,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[4,1,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[4,2,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[4,3,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[4,5,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
    sigma_delta[5,1,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[5,2,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[5,3,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[5,4,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
  } # end G loop

  inv.sigma_delta[1:M_count,1:M_count,1] <- inverse(sigma_delta[,,1])
  inv.sigma_delta[1:M_count,1:M_count,2] <- inverse(sigma_delta[,,2])

## Model Estimates
for(p in 1:P_count) { # province loop matched to C
  for(m in 1:M_count){ # method loop
    for (t in 1:n_years) {
      z[m,p,t] <- alpha_pms[1,m,p] + inprod(B.ik[p,t,],beta.k[1,m,p,])
      r[m,p,t] <- alpha_pms[2,m,p] + inprod(B.ik[p,t,],beta.k[2,m,p,])
    } # end time loop
  } # end M loop
} # end P loop


## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(p in 1:P_count) { # province loop
      for(m in 1:M_count){ # method loop
        alpha_pms[s,m,p] ~ dnorm(alpha_cms[s,m,matchcountry[p]],tau_alpha[s])
        beta.k[s,m,p,kstar[p]] <- 0
        for(j in 1:(kstar[p]-1)){ # before kstar[p]
          beta.k[s,m,p,(kstar[p] - j)] <- beta.k[s,m,p,(kstar[p] - j)+1] - delta.k[s,m,p,(kstar[p] - j)]
        } # end K1 loop
        for(j in (kstar[p]+1):K){ # after kstar[p]
          beta.k[s,m,p,j] <- beta.k[s,m,p,(j-1)] + delta.k[s,m,p,(j-1)]
        } # end K2 loop
      } # end m loop
      for(j in 1:H){
        delta.k[s,c(1:M_count),p,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s])
      } # end H loop
    } # end P loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(p in 1:P_count){ # province loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,p,t] <- 1/(1+exp(-(z[m,p,t]))) # modelling this as before assuming that z[m,p,t] is log(pi_public/pi_private)

         Q[m,p,t] <- 1/(1+exp(-(r[m,p,t]))) # logit-inverse of ratio

         U[m,p,t] <- 1-P[1,m,p,t] # this then gives you the total private sector

         P[2,m,p,t] <- Q[m,p,t]*U[m,p,t] # this is assuming that the logit(P[2,m,p,t]/U[m,p,t]) = r[m,p,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,p,t] <- U[m,p,t] - P[2,m,p,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,2])T(0,1)
    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    for(m in 1:M_count){ # method loop
      for(c in 1:C_count) { # country loop
        alpha_cms[s,m,c] ~ dnorm(beta_r[s,m,matchregion[c]],tau_alpha[s])
      }
      for(r in 1:R_count){ # regional
        beta_r[s,m,r] ~ dnorm(beta_world[s,m],tau_beta[s])
      }
       # world intercept
       beta_world[s,m] ~ dnorm(0,0.1)
    }
    tau_alpha[s] <- sigma_alpha[s]^-2
    sigma_alpha[s] ~ dt(0,1,1)T(0,)

    tau_beta[s] <- sigma_beta[s]^-2
    sigma_beta[s] ~ dt(0,1,1)T(0,)
  }
              }", file="model.txt")
        } else { # subnational global spatial model
            cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for rho
    for(i in 1:M_count){ # Method loop i
      mu_delta[g,i] <- 0
      sd_delta[g,i] ~ dt(0, 1/25, 1)T(0, )
      sigma_delta[i,i,g] <- pow(sd_delta[g,i],2)
    }

    sigma_delta[1,2,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[1,3,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[1,4,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[1,5,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[2,1,g] <- rho[1,g]*sd_delta[g,1]*sd_delta[g,2]
    sigma_delta[2,3,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[2,4,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[2,5,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[3,1,g] <- rho[2,g]*sd_delta[g,1]*sd_delta[g,3]
    sigma_delta[3,2,g] <- rho[5,g]*sd_delta[g,2]*sd_delta[g,3]
    sigma_delta[3,4,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[3,5,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[4,1,g] <- rho[3,g]*sd_delta[g,1]*sd_delta[g,4]
    sigma_delta[4,2,g] <- rho[6,g]*sd_delta[g,2]*sd_delta[g,4]
    sigma_delta[4,3,g] <- rho[8,g]*sd_delta[g,3]*sd_delta[g,4]
    sigma_delta[4,5,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
    sigma_delta[5,1,g] <- rho[4,g]*sd_delta[g,1]*sd_delta[g,5]
    sigma_delta[5,2,g] <- rho[7,g]*sd_delta[g,2]*sd_delta[g,5]
    sigma_delta[5,3,g] <- rho[9,g]*sd_delta[g,3]*sd_delta[g,5]
    sigma_delta[5,4,g] <- rho[10,g]*sd_delta[g,4]*sd_delta[g,5]
  } # end G loop

  inv.sigma_delta[1:M_count,1:M_count,1] <- inverse(sigma_delta[,,1])
  inv.sigma_delta[1:M_count,1:M_count,2] <- inverse(sigma_delta[,,2])

## Model Estimates
for(p in 1:P_count) { # province loop matched to C
  for(m in 1:M_count){ # method loop
    for (t in 1:n_years) {
      z[m,p,t] <- alpha_pms[1,m,p] + inprod(B.ik[p,t,],beta.k[1,m,p,]) + phi[1,p]
      r[m,p,t] <- alpha_pms[2,m,p] + inprod(B.ik[p,t,],beta.k[2,m,p,]) + phi[2,p]
    } # end time loop
  } # end M loop
} # end P loop


## Parameter Estimates
  for(s in 1:2){ # sector loop
    phi[s,c(1:P_count)] ~ dmnorm(P_zeroes, sigma[s]*(D-gamma[s]*W)) # spatial CAR term for continental patterns in residuals
    for(p in 1:P_count) { # province loop
      for(m in 1:M_count){ # method loop
        alpha_pms[s,m,p] ~ dnorm(alpha_cms[s,m,matchcountry[p]],tau_alpha[s])
        beta.k[s,m,p,kstar[p]] <- 0
        for(j in 1:(kstar[p]-1)){ # before kstar[p]
          beta.k[s,m,p,(kstar[p] - j)] <- beta.k[s,m,p,(kstar[p] - j)+1] - delta.k[s,m,p,(kstar[p] - j)]
        } # end K1 loop
        for(j in (kstar[p]+1):K){ # after kstar[p]
          beta.k[s,m,p,j] <- beta.k[s,m,p,(j-1)] + delta.k[s,m,p,(j-1)]
        } # end K2 loop
      } # end m loop
      for(j in 1:H){
        delta.k[s,c(1:M_count),p,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s])
      } # end H loop
    } # end P loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(p in 1:P_count){ # province loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,p,t] <- 1/(1+exp(-(z[m,p,t]))) # modelling this as before assuming that z[m,p,t] is log(pi_public/pi_private)

         Q[m,p,t] <- 1/(1+exp(-(r[m,p,t]))) # logit-inverse of ratio

         U[m,p,t] <- 1-P[1,m,p,t] # this then gives you the total private sector

         P[2,m,p,t] <- Q[m,p,t]*U[m,p,t] # this is assuming that the logit(P[2,m,p,t]/U[m,p,t]) = r[m,p,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,p,t] <- U[m,p,t] - P[2,m,p,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,2])T(0,1)
    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    for(m in 1:M_count){ # method loop
      for(c in 1:C_count) { # country loop
        alpha_cms[s,m,c] ~ dnorm(beta_r[s,m,matchregion[c]],tau_alpha[s])
      }
      for(r in 1:R_count){ # regional
        beta_r[s,m,r] ~ dnorm(beta_world[s,m],tau_beta[s])
      }
       # world intercept
       beta_world[s,m] ~ dnorm(0,0.1)
    }
    sigma[s] ~ dt(0,1,1)T(0,) # spatial variance parameter
    gamma[s] ~ dunif(0, 1) # spatial dependence parameter
    tau_alpha[s] <- sigma_alpha[s]^-2
    sigma_alpha[s] ~ dt(0,1,1)T(0,)
    tau_beta[s] <- sigma_beta[s]^-2
    sigma_beta[s] ~ dt(0,1,1)T(0,)
  }
            }", file="model.txt")
          }
      } else {
       if(spatial==TRUE) { # subnational local spatial
         cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for covariance
    mu_delta[g,1:M_count] <- rep(0,M_count)
    inv.sigma_delta[1:M_count,1:M_count,g] ~ dwish(natdf*natRmat[1:M_count,1:M_count,g],natdf) # S~dwish(R,p) => E(S) = p * solve(R)
  }

## Model Estimates
  for(p in 1:P_count) { # subnat loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) {
        z[m,p,t] <- alpha_pms[1,m,p] + inprod(B.ik[p,t,],beta.k[1,m,p,]) + phi[1,p]
        r[m,p,t] <- alpha_pms[2,m,p] + inprod(B.ik[p,t,],beta.k[2,m,p,]) + phi[2,p]
      } # end **after tstar** loop
    } # end M loop
  } # end P loop

## Parameter Estimates
  for(s in 1:2){ # sector loop
    phi[s,c(1:P_count)] ~ dmnorm(P_zeroes,  tau_phi[s]*(D-gamma[s]*W)) # spatial CAR term
    for(p in 1:P_count) { # subnat loop
      for(m in 1:M_count){ # method loop
        alpha_pms[s,m,p] ~ dnorm(alpha_cms_hat[s,m], tau_alpha_pms_hat[s]) # sharing info across methods within a subnat area so each subnat public/private sector has an intercept. Tau-alpha is the cross-method sector variance.
        beta.k[s,m,p,kstar[p]] <- 0 # kstar[p] is the knot at tstar for country c. Just the value of the intercept
        for(j in 1:(kstar[p]-1)){ # before kstar[p]
          beta.k[s,m,p,(kstar[p] - j)] <- beta.k[s,m,p,(kstar[p] - j)+1] - delta.k[s,m,p,(kstar[p] - j)]
        } # end K1 loop
        for(j in (kstar[p]+1):K){ # after kstar[p]
          beta.k[s,m,p,j] <- beta.k[s,m,p,(j-1)] + delta.k[s,m,p,(j-1)]
        } # end K2 loop
      } # end m loop

      for(j in 1:H){
        delta.k[s,c(1:M_count),p,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s]) # delta are the slopes for country c and method m
      } # end H loop
    } # end C loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(p in 1:P_count){ # country loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,p,t] <- 1/(1+exp(-(z[m,p,t]))) # modelling this as before assuming that z[m,p,t] is log(pi_public/pi_private)

         Q[m,p,t] <- 1/(1+exp(-(r[m,p,t]))) # logit-inverse of ratio

         U[m,p,t] <- 1-P[1,m,p,t] # this then gives you the total private sector

         P[2,m,p,t] <- Q[m,p,t]*U[m,p,t] # this is assuming that the logit(P[2,m,p,t]/U[m,p,t]) = r[m,p,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,p,t] <- U[m,p,t] - P[2,m,p,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,2])T(0,1)

    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    tau_phi[s] ~ dt(0,1,1)T(0,) # spatial variance parameter
    gamma[s] ~ dunif(0, 1) # spatial dependence parameter
    tau_alpha[s] ~ dt(0,1,1)T(0,)
  }
}", file="model.txt")
       } else { # subnational local nonspatial
         cat("model{
## Variance structure
  for(g in 1:2) { # Sector (public,private) loop for covariance
    mu_delta[g,1:M_count] <- rep(0,M_count)
    inv.sigma_delta[1:M_count,1:M_count,g] ~ dwish(natdf*natRmat[1:M_count,1:M_count,g],natdf) # S~dwish(R,p) => E(S) = p * solve(R)
  }

## Model Estimates
  for(p in 1:P_count) { # subnat loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) {
        z[m,p,t] <- alpha_pms[1,m,p] + inprod(B.ik[p,t,],beta.k[1,m,p,])
        r[m,p,t] <- alpha_pms[2,m,p] + inprod(B.ik[p,t,],beta.k[2,m,p,])
      } # end **after tstar** loop
    } # end M loop
  } # end P loop

## Parameter Estimates
  for(s in 1:2){ # sector loop
    for(p in 1:P_count) { # subnat loop
      for(m in 1:M_count){ # method loop
        alpha_pms[s,m,p] ~ dnorm(alpha_cms_hat[s,m], tau_alpha_pms_hat[s]) # sharing info across methods within a subnat area so each subnat public/private sector has an intercept. Tau-alpha is the cross-method sector variance.
        beta.k[s,m,p,kstar[p]] <- 0 # kstar[p] is the knot at tstar for country c. Just the value of the intercept
        for(j in 1:(kstar[p]-1)){ # before kstar[p]
          beta.k[s,m,p,(kstar[p] - j)] <- beta.k[s,m,p,(kstar[p] - j)+1] - delta.k[s,m,p,(kstar[p] - j)]
        } # end K1 loop
        for(j in (kstar[p]+1):K){ # after kstar[p]
          beta.k[s,m,p,j] <- beta.k[s,m,p,(j-1)] + delta.k[s,m,p,(j-1)]
        } # end K2 loop
      } # end m loop

      for(j in 1:H){
        delta.k[s,c(1:M_count),p,j] ~ dmnorm(mu_delta[s,],inv.sigma_delta[,,s]) # delta are the slopes for country c and method m
      } # end H loop

    } # end C loop
  } # end S loop

## Estimating all the Categories here (including total private)
  for(p in 1:P_count){ # country loop
    for(m in 1:M_count){ # method loop
      for (t in 1:n_years) { # years loop
         P[1,m,p,t] <- 1/(1+exp(-(z[m,p,t]))) # modelling this as before assuming that z[m,p,t] is log(pi_public/pi_private)

         Q[m,p,t] <- 1/(1+exp(-(r[m,p,t]))) # logit-inverse of ratio

         U[m,p,t] <- 1-P[1,m,p,t] # this then gives you the total private sector

         P[2,m,p,t] <- Q[m,p,t]*U[m,p,t] # this is assuming that the logit(P[2,m,p,t]/U[m,p,t]) = r[m,p,t] i.e., we are modelling the ratio of private medical to private

         P[3,m,p,t] <- U[m,p,t] - P[2,m,p,t] # other = private - private medical
      }
    }
  }

## Likelihood
  for (k in 1:n_obs) {
    y[k, 1] ~ dnorm(P[1,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,1])T(0,1)
    y[k, 2] ~ dnorm(P[2,matchmethod[k],matchsubnat[k],matchyears[k]], tau_y[k,2])T(0,1)

    tau_y[k,1:2] <- 1/(se_prop[k,1:2]^2)
    }

## Priors
  for(s in 1:2) { # cross method variance (within a country)
    tau_alpha[s] ~ dt(0,1,1)T(0,)
  }
}", file="model.txt")
       }
      }
    }
  }
}
