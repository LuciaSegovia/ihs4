



test1 <- function(data.df, var1 = "var1", var2 = "var2", var3 = "var3", var4 = "var4"){  
  
  for(i in 1:nrow(data.df)){
    
    if(!is.na(data.df[i, var2]) & data.df[i, var2]>0 &
       data.df[i, var1] == data.df[i, var2] & 
       (is.na(data.df[i, var3])|data.df[i, var3]==0) &
       (is.na(data.df[i, var4])|data.df[i, var4]==0)){
      
      data.df[i, "test"] <-  data.df[i, var1]
      
    } else{
      
      data.df[i, "test"] <-  "no"
      
    }
  }
  
  return(data.df)
  
}    

test1(ihs4, "cons_quant", "purc_quant", "prod_quant", "gift_quant")
