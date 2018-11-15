###########################################################################################################

# ONE-HOT ENCODING FUNCTION FOR DNA SEQUENCES
# Author: Rubén Sánchez Fernández

############################################################################################################

one_hot <- function(data, column_name){
  
  #creating an empty list to save the encoded sequences
  ls <- list()
  #going through each sequence
  for (n_seq in 1:nrow(data)) {
    
    #converting to string
    x <- as.character(data[n_seq, column_name])
    
    #accessing to each character inside the sequence
    x_split <- strsplit(x, "")[[1]]
    
    #sequence length
    n <- length(nchar(x_split))
    
    #empty matrix
    m1 <- matrix(nrow = n, ncol = 4)
    
    #going through each character and assigning the binary vectors
    for (i in 1:n){
      if (x_split[i] == "a"){
        m1[i,] = c(1,0,0,0)}
      if (x_split[i] == "c"){
        m1[i,] = c(0,1,0,0)}
      if (x_split[i] == "g"){
        m1[i,] = c(0,0,1,0)}
      if(x_split[i] == "t"){
        m1[i,] = c(0,0,0,1)}
    }
    #adding each encoded sequence into the list
    ls[[n_seq]] <- m1
  }

  #we're going to save the sequences also as a dataframe to input them in the ML algorithm
  df <- matrix(nrow = length(ls), ncol = n*4)
  
  for (i in 1:length(ls)){
    df[i,] <- as.vector(t(ls[[i]])) #transposing and converting to vector each sequence
  }
  df <- as.data.frame(df) #coverting to df
  
  #storing list with enc. sequences and dataframe with enc.sequences
  results <- list(ls, df)
  names(results) <- c("List encoded sequences", "Dataframe encoded sequences")
  
  return(results)}