function(x, nbins = 10) {
    # Create standard normal bins based off quantiles of 10%
    bins <- qnorm(1/nbins * (0:nbins), 0, 1);
    
    # Sort the observed values (from vector x) into the appropriate bins
    bincode <- cut(x, bins, labels = FALSE);
    
    #Now get the frequency of values in the normal bins
    observed <- table(bincode); 
    
    # Get the expected values from these bin sorts
    # There should be the same number of values (observations)
    expected <- length(x) / nbins;
    
    # Compute Chi-square statistic. 
    chisq <- sum((observed - expected)^2 / expected);
    
    # p-value of Chi-square statistic
    pval <- pchisq(chisq, df = nbins - 2, lower.tail = FALSE); 
    
    print("Chi-sq test statistic:")
    print(paste(chisq))
    
    print("p-value with df = {nbins - 2}:")
    print(paste(pval))
}
