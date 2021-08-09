predictR <- function(m, f_var, linkinv = T, ci.level=0.95, offset=F,
                     Interaction = F, int_var=NULL, pct = NULL,
                     n=100){
  ## Preparation for a new data set---
  # model class
  CL <- class(m)[1]
  
  # data frame
  dat <- model.frame(m)
    
    # Random effect
    if(CL=="lmerMod"|CL=="glmerMod"){
      REID <- NULL
      for(i in 1:length(names(ranef(m)))){
        tmp <- which(colnames(dat)==names(ranef(m))[i])
        REID <- c(REID, tmp)
      }
      dat <- dat[,-REID]
    }
    
    # offset term
    fixID <- which(colnames(dat)!="(offset)")
    ofsID <- which(colnames(dat)=="(offset)")
    dat <- dat[,fixID]
    if(CL=="lmerMod"|CL=="glmerMod"){
      dat.check <- length(names(dat)[-1]) - length(names(fixef(m))[-1])
    }else{
      dat.check <- length(names(dat)[-1]) - length(names(coefficients(m))[-1])
    }
    
    if(offset==T){
      if(length(ofsID)==0|dat.check>0){ stop("Specify offset term as e.g., 'offset = log(x)' in the model object") }
    }else{
      if(length(ofsID)!=0|dat.check>0){ stop("Should be 'offset=T' and specify as e.g., 'offset = log(x)' in the model object") }
    }
    
  # turn "character" or "logical" into "factor"
  dat <- as.data.frame(lapply(dat,function(x){
    if(is.character(x)|is.logical(x)){
      as.factor(x)
    }
    else{x}
  }))
  
  # choose focal variable
  fID <- which(names(dat)==f_var)
  if(is.factor(dat[,fID])){
    stop("focal variable must be numeric")
  }else{
    X1 <- list(seq(min(dat[,fID]),max(dat[,fID]), length = n))
    names(X1) <- f_var
  }
      
  # Interction terms
  if(Interaction == T){
    iID <- which(names(dat)==int_var) # column ID of interction terms
    if(length(iID)>1){ stop("Only one interaction term is allowed") }
    if(is.null(int_var)==T){ stop("Specify interaction terms") }
    
    if(is.factor(dat[,iID])==1){#if factor
        if(length(iID)==1){ X2 <- list(unique(dat[,iID])); names(X2) <- int_var }
      }else{#if numeric
        if(length(iID)==1){ X2 <- list(quantile(dat[,iID],pct)); names(X2) <- int_var }
        if(is.null(pct)==T){ stop("Specify percentiles of interaction term (ex. pct=c(0.2,0.8))") }
    }
    
    X <- c(X1,X2)
    X <- expand.grid(X)
  }else{
    if(length(int_var)>0){ stop("Should be 'Interaction = T'") }
    X <- X1
  }
  
  # make new data set
  if(Interaction==T){
    dat_r <- as.data.frame(dat[,-c(1,fID,iID),drop=F])
    if(dim(dat_r)[2]==0){
      new_dat <- X
    }else{
      fixed <- rep(lapply(dat_r,function(x) if(is.numeric(x)) mean(x) else factor(levels(x)[1],levels = levels(x))))
      new_dat <- cbind(X,as.data.frame(fixed))
    }
  }else{
    dat_r <- as.data.frame(dat[,-c(1,fID),drop=F])
    if(dim(dat_r)[2]==0){
      new_dat <- X
    }else{
      fixed <- rep(lapply(dat_r,function(x) if(is.numeric(x)) mean(x) else factor(levels(x)[1],levels = levels(x))))
      new_dat <- cbind(X,as.data.frame(fixed))
    }
  }
  
  ## Get predicted values---
  # interval
  cr <- ci.level + (1-ci.level)/2
  CR <- qnorm(cr,0,1)
  
  # inverse link function
  ilink <- ifelse(linkinv==T, family(m)$linkinv, identity)
  
  if(Interaction==T){
    output <- list(NULL)
    Index <- ifelse(is.factor(new_dat[,int_var]), length(unique(new_dat[,int_var])), length(pct))
    for(i in 1:Index){
      st <- I(1+(i-1)*n); en <- i*n
      if(CL == "lm"|CL == "glm"|CL == "negbin"){
        fo <- as.formula(paste("~",as.character(formula(m,fixed.only=T)[3]))) # formula
        mm <- model.matrix(fo, new_dat[st:en,])
        us.fit <- mm%*%coefficients(m) 
        se <- sqrt(diag(mm%*%tcrossprod(vcov(m),mm)))
        fit <- ilink(us.fit)
        uci <- ilink(us.fit+CR*se)
        lci <- ilink(us.fit-CR*se)
        y.tmp <- data.frame(fit=fit,uci=uci,lci=lci)
      }
      
      if(CL == "lmerMod"| CL == "glmerMod"){
        fo <- as.formula(paste("~",as.character(formula(m,fixed.only=T)[3]))) # formula
        mm <- model.matrix(fo, new_dat[st:en,])
        us.fit <- mm%*%fixef(m) # unscaled fitted value
        se <- sqrt(diag(mm%*%tcrossprod(vcov(m),mm)))
        fit <- ilink(us.fit)
        uci <- ilink(us.fit + CR*se)
        lci <- ilink(us.fit - CR*se)
        y.tmp <- data.frame(fit=fit,uci=uci,lci=lci)
      }
      output[[i]] <- data.frame(y.tmp, new_dat[st:en,])
    }#i
    u_int_var <- unique(new_dat[,int_var])# unique values or name for interaction terms
    if(is.factor(new_dat[,int_var])==T){
      names(output) <- as.character(u_int_var)
    }else{
      names(output) <- as.factor(paste(pct*100,"%","_",int_var,sep=""))
    }
    return(output)
  }else{
    if(CL == "lm"|CL == "glm"|CL == "negbin"){
      fo <- as.formula(paste("~",as.character(formula(m,fixed.only=T)[3]))) # formula
      mm <- model.matrix(fo, new_dat)
      us.fit <- mm%*%coefficients(m) # unscaled fitted value
      se <- sqrt(diag(mm%*%tcrossprod(vcov(m),mm)))
      fit <- ilink(us.fit)
      uci <- ilink(us.fit + CR*se)
      lci <- ilink(us.fit - CR*se)
      Y <- data.frame(fit=fit,uci=uci,lci=lci)
    }
    
    if(CL == "lmerMod"| CL == "glmerMod"){
      fo <- as.formula(paste("~",as.character(formula(m,fixed.only=T)[3]))) # formula
      mm <- model.matrix(fo, new_dat)
      us.fit <- mm%*%fixef(m) # unscaled fitted value
      se <- sqrt(diag(mm%*%tcrossprod(vcov(m),mm)))
      fit <- ilink(us.fit)
      uci <- ilink(us.fit + CR*se)
      lci <- ilink(us.fit - CR*se)
      Y <- data.frame(fit=fit,uci=uci,lci=lci)
    }
    output <- data.frame(Y,new_dat)
    return(output)
  }
}
