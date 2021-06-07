random_split_datasets = function(data=NULL, K=10, path=NULL) {
  "
  Input {
  data:   The dataset that is to be splited.
  K:      Number of subsets to be generated.
  path:   Path to save all K outputs.
  }
  
  Return {
  output: K .txt files containig K sampled subsets.
  }
  "
  n = dim(data)[1]
  n_each = ceiling(n / K)

  for (i in 1:(K-1)) {
    low_bd = n_each * (i - 1) + 1
    upp_bd = n_each * i
    output = data[low_bd:upp_bd, ]
    
    write.table(output, file = paste0(path, "/subset", i, ".txt"))
  }
  
  output = data[(upp_bd + 1):1369, ]
  write.table(output, file = paste0(path, "/subset", K, ".txt"))
}

rNIG = function(n=1, mu_beta=NULL, inv_V_beta=NULL, a=NULL, b=NULL){
  "
  Input {
  n:          number of samples
  mu_beta:    mean of beta
  inv_V_beta: precision matrix of beta divided by sigma^2
  a:          first parameter of inv gamma
  b:          second parameter of inv gamma
  }
  
  Return {
  output:     a nx(p+1) matrix containing generated samples
  }
  "
  p = length(mu_beta)
  output_beta = matrix(NA, nrow = n, ncol = p)
  output_sigma2 = rinvgamma(n, shape=a, scale=b)
  
  for (i in 1:n) {
    output_beta[i, ] = mvrnorm(1, mu=mu_beta, Sigma=output_sigma2[i]*solve(inv_V_beta))
  }
  
  data.frame(beta=output_beta, sigma2=output_sigma2)
}

rposNIG_comp = function(n=1, y=NULL, X=NULL, inv_V_y=NULL, mu_beta=NULL, 
                        inv_V_beta=NULL, a=0, b=0){
  "
  Input {
  n:          number of samples
  y:          a Nx1 vector of observation
  X:          a Nxp design matrix with intercept (a column with ones)
  mu_beta:    mean of beta
  inv_V_beta: precision matrix of beta divided by sigma^2
  a:          first parameter of inv gamma
  b:          second parameter of inv gamma
  }
  
  Return {
  output:     a nx2 matrix containing generated samples
  }
  "
  if(is.null(inv_V_y)) inv_V_y = diag(length(y))
  if(inv_V_beta==0) inv_V_beta = diag(dim(X)[2]) * 0
  
  inv_M = inv_V_beta + t(X) %*% inv_V_y %*% X
  M = solve(inv_M)
  m = inv_V_beta %*% mu_beta + t(X) %*% inv_V_y %*% y
  
  a_star = a + length(y) / 2
  b_star = b + 0.5 * (t(y) %*% inv_V_y %*% y + 
                        mu_beta %*% inv_V_beta %*% mu_beta - 
                        t(m) %*% M %*% m)
  mu_beta_star = M %*% m
  # inv_V_beta_star is indeed inv_M so no need to allocate memory
  
  output = rNIG(n=n, mu_beta = mu_beta_star, inv_V_beta = inv_M, 
                a = a_star, b = b_star)
  
  output
}

NIG_pos_para = function(y=NULL, X=NULL, inv_V_y=NULL, mu_beta=NULL, 
                        inv_V_beta=NULL, a=0, b=0){
  "
  Input {
  y:          a Nx1 vector of observation
  X:          a Nxp design matrix with intercept (a column with ones)
  mu_beta:    mean of beta
  inv_V_beta: precision matrix of beta divided by sigma^2
  a:          first parameter of inv gamma
  b:          second parameter of inv gamma
  }
  
  Return {
  output:     a list containing posterior parameters of the model.
  }
  "
  if(is.null(inv_V_y)) inv_V_y = diag(length(y))
  if(sum(inv_V_beta==0)==1) inv_V_beta = diag(dim(X)[2]) * 0
  
  inv_M = inv_V_beta + t(X) %*% inv_V_y %*% X
  M = solve(inv_M)
  m = inv_V_beta %*% mu_beta + t(X) %*% inv_V_y %*% y
  
  a_star = a + length(y) / 2
  b_star = b + 0.5 * (t(y) %*% inv_V_y %*% y + 
                        t(mu_beta) %*% inv_V_beta %*% mu_beta - 
                        t(m) %*% M %*% m)
  mu_beta_star = M %*% m
  # inv_V_beta_star is indeed inv_M so no need to allocate memory
  
  output = list("mu_beta_k" = mu_beta_star, "inv_V_beta_k" = inv_M, 
                "a_k" = a_star, "b_k" = as.numeric(b_star))
  output
}
