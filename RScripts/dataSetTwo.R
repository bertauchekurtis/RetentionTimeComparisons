# kurtis bertauche
# data set sorting

# libaries
library(tidyverse)
library(sjmisc)
library(stringr)
library(gtools)

# read data
modificationSpecificPeptides <- read.csv(file = "../data/modificationSpecificPeptides.txt", sep = "\t")
phospho <- read.csv(file = "../data/Phospho (STY)Sites.txt", sep = "\t")
oxi <- read.csv(file= "../data/Oxidation (M)Sites.txt", sep = "\t")

# reduce data to relevant columns
reduced <- modificationSpecificPeptides[, c("Sequence", "Modifications", "Oxidation..M.", "Phospho..STY.", "Calibrated.retention.time", "PEP", "Score", "Delta.score", "id","Peptide.ID", "Oxidation..M..site.IDs", "Phospho..STY..site.IDs")]
reducedPhospho <- phospho[, c("Number.of.Phospho..STY.","Amino.acid","Phospho..STY..Probabilities","Position.in.peptide","Peptide.IDs")]
reducedOxi <- oxi[,c("Number.of.Oxidation..M.","Amino.acid","Oxidation..M..Probabilities","Position.in.peptide","Peptide.IDs")]

# filter data
reduced <- reduced %>% filter(PEP < 0.01, Score > 40, Delta.score > 8)

# drop rows which indicate no modifications
reducedPhospho <- reducedPhospho %>% drop_na(Number.of.Phospho..STY.)
reducedOxi <- reducedOxi %>% drop_na(Number.of.Oxidation..M.)
reducedOxi<-reducedOxi[!(reducedOxi$Number.of.Oxidation..M.==""),]

# create column to directly compare sequences
reducedPhospho$plainSeq <- reducedPhospho$Phospho..STY..Probabilities
reducedOxi$plainSeq <- reducedOxi$Oxidation..M..Probabilities

# format new column to change fromat from AA(5)A to AA 5 A
reducedPhospho$plainSeq <- gsub("\\(", "", reducedPhospho$plainSeq)
reducedPhospho$plainSeq <- gsub("\\)", "", reducedPhospho$plainSeq)
reducedPhospho$plainSeq <- str_replace_all(reducedPhospho$plainSeq, "[[:punct:]]", "")

reducedOxi$plainSeq <- gsub("\\(", "", reducedOxi$plainSeq)
reducedOxi$plainSeq <- gsub("\\)", "", reducedOxi$plainSeq)
reducedOxi$plainSeq <- str_replace_all(reducedOxi$plainSeq, "[[:punct:]]", "")

for(num in 0:9)
{
  reducedPhospho$plainSeq <- gsub(as.character(num), "", reducedPhospho$plainSeq)
  reducedOxi$plainSeq <- gsub(as.character(num), "", reducedOxi$plainSeq)
}

# create potential sequence (for matching later)
reducedOxi$potentialSeq <- reducedOxi$Oxidation..M..Probabilities
reducedPhospho$potentialSeq <- reducedPhospho$Phospho..STY..Probabilities

# for phospho
reducedPhospho$potentialSeq <- gsub("\\(", " ", reducedPhospho$potentialSeq)
reducedPhospho$potentialSeq <- gsub("\\)", " ", reducedPhospho$potentialSeq)
reducedPhospho$potentialSeq <- strsplit(reducedPhospho$potentialSeq, " ")

# for oxi
reducedOxi$potentialSeq <- gsub("\\(", " ", reducedOxi$potentialSeq)
reducedOxi$potentialSeq <- gsub("\\)", " ", reducedOxi$potentialSeq)
reducedOxi$potentialSeq <- strsplit(reducedOxi$potentialSeq, " ")

condStrtoi = function(stringy)
{
  if(str_detect(stringy, "[ABDEFGHIJKLMNOPQRSTUVWXYZ]")){
    return(stringy)
  } else {
    return(as.double(stringy))
  }
  
}

applyToVec = function(vector, FUN)
{
  vector <- lapply(vector, FUN)
}

makeVec = function(list)
{
  as.vector(list, mode = "double")
}

getOrder = function(vector, length)
{
  return((order(vector, decreasing = TRUE)[1:length]))
}

reducedPhospho$potentialSeq <- sapply(reducedPhospho$potentialSeq, applyToVec, condStrtoi)
reducedOxi$potentialSeq <- sapply(reducedOxi$potentialSeq, applyToVec, condStrtoi)

reducedPhospho$withNA <- as.vector(reducedPhospho$potentialSeq, mode = "any")
reducedPhospho$withNA <- sapply(reducedPhospho$potentialSeq, makeVec)
reducedPhospho$largest <- lapply(reducedPhospho$withNA, getOrder, length(reducedPhospho$withNA))

reducedOxi$withNA <- as.vector(reducedOxi$potentialSeq, mode = "any")
reducedOxi$withNA <- sapply(reducedOxi$potentialSeq, makeVec)
reducedOxi$largest <- lapply(reducedOxi$withNA, getOrder, length(reducedOxi$withNA))


generatePhosphoSeq = function(numSTY, potentialSeq, withNA, largest)
{
  # first we'll want to check for "ties that matter". i.e. ties at the end of the selected
  if(is.na(withNA[largest[numSTY + 1]])){
    # no worrying about ties here
    for(val in 1:numSTY)
    {
      chunkLength <- nchar(potentialSeq[largest[val] - 1])
      chunk <- unlist(potentialSeq[largest[val] - 1])
      substr(chunk, chunkLength, chunkLength) <- tolower(str_sub(chunk, -1))
      potentialSeq[largest[val] - 1] <- chunk
    }
    potentialSeq <- potentialSeq[seq(1, length(potentialSeq), 2)]
    return(c(paste(potentialSeq, collapse = "")))
  }
  else
  {
    # possible tie
    # check values
    # we are checking if the probability of the peptide in the last spot is
    # the same as the peptide in the next spot. if it is, then we will need
    # to permute the modification throughout the sequence
    if(withNA[largest[numSTY + 1]] == withNA[largest[numSTY]]){
      
      # there is a tie here
      endPermute = numSTY + 1
      
      # we want to find how far we'll want to permute the sequence
      # here, we use a try block since we either going to run into a lower
      # probability or an NA value. The latter value produces an error, but
      # by using a try block we leverage this to use as our stopping point
      try({
        
        while(withNA[largest[endPermute]] == withNA[largest[endPermute + 1]])
          {
          endPermute = endPermute + 1
          }
      
      }, silent = TRUE)
      
      # now, get the permutation starting point
      # again, we will leverage a try block for when we reach the end of the vector
      startPermute <- endPermute
      try({
      
        while((!(identical(numeric(0), withNA[largest[startPermute - 1]])))&(withNA[largest[startPermute]] == withNA[largest[startPermute - 1]]))
        {
          startPermute = startPermute - 1
        }
      }, silent = TRUE)
      
      # how many spots are being permuted?
      # the number of modifications in the peptide minus the index of the first permutation spot plus one
      # For example: if numSTY = 4, we know a total of 4 peptides are modded, but we need to know of those 4, how many are we needing to permute due to ties
      numPermuteSpots <- numSTY - startPermute + 1 
      
      # calculate the actual locations of the "largest" vector that will be cycled through to create all the permutations
      permuteSpots <- combinations(endPermute - startPermute + 1 , numPermuteSpots, startPermute:endPermute, repeats.allowed=F)
     
      # where the finished sequences will go
      seqs <- vector()
      
      # this is the loop to generate all of the sequences for the set of permutations
      # thus, we run along the length of the permuteSpots, subsetting for the column since each row will be a permutation
      for(perm in 1:length(permuteSpots[,1]))
      {
        
        # empty vector to put sequences
        thisIter <- potentialSeq
        
        # this loop will need to run along all of the modification in each permutation
        # so, if two spots were being permuted each time, then this would run twice
        # this loop replaces the entries in the permute spots vector with their corresponding
        # values from the largest vector
        for(spot in 1:length(permuteSpots[perm,]))
        {
          permuteSpots[perm,][spot] <- largest[permuteSpots[perm,][spot]]
        }
    
        
        # now, we combine the permutation spot(s) for this permutation with the
        # fixed modification spots, into one vector to use
        # we include a case to check if all of the spots are tied (the else clause)
        # and the case where there are fixed spots and permuted spots
        if((startPermute - 1) != 0)
        {
          fixedModificationSpots <- largest[1:(startPermute -1)]
          largestToUse <- append(fixedModificationSpots, permuteSpots[perm,])
        }
        else
        {
          largestToUse <- permuteSpots[perm,]
        }
        
        # finally, we are ready to create the actual sequence
        # here, we change the uppercase letters to lowercase letters to denote modifications
        # and then add the sequence and it's rt to the data
        for(val in 1:numSTY)
        {
          chunkLength <- nchar(thisIter[largestToUse[val] - 1])
          chunk <- unlist(thisIter[largestToUse[val] - 1])
          substr(chunk, chunkLength, chunkLength) <- tolower(str_sub(chunk, -1))
          thisIter[largestToUse[val] - 1] <- chunk
        }
        
        thisIter <- thisIter[seq(1, length(thisIter), 2)]
        seqs <- rbind(seqs, c(paste(thisIter, collapse = "")))
      }
      return(seqs)
    }
    else
    {
      for(val in 1:numSTY)
      {
        chunkLength <- nchar(potentialSeq[largest[val] - 1])
        chunk <- unlist(potentialSeq[largest[val] - 1])
        substr(chunk, chunkLength, chunkLength) <- tolower(str_sub(chunk, -1))
        potentialSeq[largest[val] - 1] <- chunk
      }
      potentialSeq <- potentialSeq[seq(1, length(potentialSeq), 2)]
      return(c(paste(potentialSeq, collapse = "")))
    }
  }
  
} 

# essentially a wrapper function for the generatePhosphoSeq function
# very similar tasks so we can apply the other function for us here
generateOxiSeq = function(numOxi, potentialSeq, withNA, largest)
{
  if(str_contains(numOxi, ";"))
  {
    permuteNums <- str_split(numOxi, ";")
    permuteNums <- unlist(permuteNums)
    permuteNums <- strtoi(permuteNums)
    seqs <- vector()
    for(i in 1:length(permuteNums))
    {
      if(2 * i < length(potentialSeq))
      {
        seq <- generatePhosphoSeq(permuteNums[i], potentialSeq, unlist(withNA), largest)
        seqs <- rbind(seqs, seq)
      }
    }
    return(seqs)
  }
  else
  {
    numOxi <- strtoi(numOxi)
    return(generatePhosphoSeq(numOxi, potentialSeq, unlist(withNA), largest))
  }
}

# now, we need to know which rows to check in the modifications files
# although these are provided, they may not be accurate anymore due to filtering
# get places to check

# function for string manipulation
splitID = function(pepID)
{
  if(str_contains(pepID, ";"))
  {
    ls <- strsplit(pepID, split = ";")
  }
  else
  {
    list(c(pepID, pepID))
  }
}

# function for string manipulation
splitIDoxi = function(pepID)
{
  if(str_count(pepID, ";") == 0)
  {
    return(list(c(pepID, pepID, pepID)))
  }
  if(str_count(pepID, ";") == 1)
  {
    ls <- strsplit(pepID, split = ";")
    list(c(ls[[1]][1],ls[[1]][2],ls[[1]][1]))
  }
  else
  {
    ls <- strsplit(pepID, split = ";")
  }
}

# function for vector manipulation
getItem = function(vector)
{
  vector[1]
}

# function for vector manipulation
getItemTwo = function(vector)
{
  vector[2]
}

# function for vector manipulation
getItemThree = function(vector)
{
  vector[3]
}


# split peptide id's into two integer columns instead of one string
reducedPhospho$listID <- sapply(reducedPhospho$Peptide.IDs, splitID)
reducedPhospho$pepIDone <- sapply(reducedPhospho$listID, getItem)
reducedPhospho$pepIDtwo <- sapply(reducedPhospho$listID, getItemTwo)
reducedPhospho$pepIDone <- strtoi(reducedPhospho$pepIDone)
reducedPhospho$pepIDtwo <- strtoi(reducedPhospho$pepIDtwo)

reducedOxi$listID <- sapply(reducedOxi$Peptide.IDs, splitIDoxi)
reducedOxi$pepIDone <- sapply(reducedOxi$listID, getItem)
reducedOxi$pepIDtwo <- sapply(reducedOxi$listID, getItemTwo)
reducedOxi$pepIDthree <- sapply(reducedOxi$listID, getItemThree)
reducedOxi$pepIDone <- strtoi(reducedOxi$pepIDone)
reducedOxi$pepIDtwo <- strtoi(reducedOxi$pepIDtwo)
reducedOxi$pepIDthree <- strtoi(reducedOxi$pepIDthree)

# remove now redundant columns
reducedPhospho$listID <- NULL
reducedPhospho$Peptide.IDs <- NULL

reducedOxi$listID <- NULL
reducedOxi$Peptide.IDs <- NULL

# now, we will go through modification specific peptides.txt and generate all of the
# needed sequences

sequence <- vector()
retentionTime <- vector()
data <- data.frame(sequence, retentionTime)

for(row in 1:nrow(reduced))
{
  
  # there are four cases here
  # 1. no modifications
  # 2. phospho modifciation
  # 3. oxidation modification
  # 4. both types of modifications
  
  if((reduced[row,]["Oxidation..M."] == 0) & (reduced[row,]["Phospho..STY."] == 0))
  {
    # no modifications
    data[nrow(data) + 1,] <- c(reduced[row,]["Sequence"], reduced[row,]["Calibrated.retention.time"])
  }
  
    # only phospho
  if((reduced[row,]["Oxidation..M."] == 0) & (reduced[row,]["Phospho..STY."] != 0))
  {
    seqs <- vector()
    phosphoRows <- reducedPhospho[reducedPhospho$pepIDone == (reduced[row,"Peptide.ID"]) | reducedPhospho$pepIDtwo == (reduced[row,"Peptide.ID"]),]
    
    # we did additionall filtering, check for this
    if(nrow(phosphoRows) == 0)
    {
      next
    }
    
    for(i in 1:nrow(phosphoRows))
    {
      if(reduced[row,"Sequence"] == phosphoRows[i,"plainSeq"])
      {
        numSTY <- phosphoRows[i,"Number.of.Phospho..STY."]
        
        potentialSeq <- phosphoRows[i, "potentialSeq"] # is a list of 1
        potentialSeq <- unlist(potentialSeq) # now is vector
        potentialSeq <- as.list(potentialSeq) # now is a proper length list
        
        withNA <- phosphoRows[i, "withNA"]
        withNA <- unlist(withNA)
        
        largest <- phosphoRows[i, "largest"]
        largest <- unlist(largest)
        
        seqs <- rbind(seqs, generateOxiSeq(numSTY, potentialSeq, withNA, largest))
      }
    }
    # we did additional filtering, check for this
    if(identical(seqs, logical(0)))
    {
      next
    }
    
    for(j in 1:nrow(seqs))
    {
      data[nrow(data) + 1,] <- c(seqs[j], reduced[row,]["Calibrated.retention.time"])
    }
  }
  
  # only oxi
  if((reduced[row,]["Oxidation..M."] != 0) & (reduced[row,]["Phospho..STY."] == 0))
  {
    seqs <- vector()
    oxiRows <- reducedOxi[reducedOxi$pepIDone == (reduced[row,"Peptide.ID"]) | reducedOxi$pepIDtwo == (reduced[row,"Peptide.ID"]) | reducedOxi$pepIDthree == (reduced[row,"Peptide.ID"]),]

    # we did additionall filtering, check for this
    if(nrow(oxiRows) == 0)
    {
      next
    }
    
    for(i in 1:nrow(oxiRows))
    {
      if(reduced[row,"Sequence"] == oxiRows[i,"plainSeq"])
        {
          numOxi <- oxiRows[i,"Number.of.Oxidation..M."]
          
          potentialSeq <- oxiRows[i, "potentialSeq"] # is a list of 1
          potentialSeq <- unlist(potentialSeq) # now is vector
          potentialSeq <- as.list(potentialSeq) # now is a proper length list
          
          withNA <- oxiRows[i, "withNA"]
          withNA <- unlist(withNA)
          
          largest <- oxiRows[i, "largest"]
          largest <- unlist(largest)
        
          seqs <- rbind(seqs, generateOxiSeq(numOxi, potentialSeq, withNA, largest))
        }
    }
    # we did additional filtering, check for this
    if(identical(seqs, logical(0)))
    {
      next
    }
    
    
    for(j in 1:nrow(seqs))
    {
      data[nrow(data) + 1,] <- c(seqs[j], reduced[row,]["Calibrated.retention.time"])
    }
  }
  else
  {
    # first, we'll get all of the oxi seqs
    seqs <- vector()
    oxiSeqs <- vector()
    oxiRows <- reducedOxi[reducedOxi$pepIDone == (reduced[row,"Peptide.ID"]) | reducedOxi$pepIDtwo == (reduced[row,"Peptide.ID"]) | reducedOxi$pepIDthree == (reduced[row,"Peptide.ID"]),]
    
    # if we don't have it
    if(nrow(oxiRows) == 0)
    {
      next
    }
    
    
    for(i in 1:nrow(oxiRows))
    {
      if(reduced[row,"Sequence"] == oxiRows[i,"plainSeq"])
      {
        numOxi <- oxiRows[i,"Number.of.Oxidation..M."]
        
        potentialSeq <- oxiRows[i, "potentialSeq"] # is a list of 1
        potentialSeq <- unlist(potentialSeq) # now is vector
        potentialSeq <- as.list(potentialSeq) # now is a proper length list
        
        withNA <- oxiRows[i, "withNA"]
        withNA <- unlist(withNA)
        
        largest <- oxiRows[i, "largest"]
        largest <- unlist(largest)
        
        oxiSeqs <- rbind(oxiSeqs, generateOxiSeq(numOxi, potentialSeq, withNA, largest))
      }
    }
    
    # then, we'll get all of the phospho seqs
    phosphoSeqs <- vector()
    phosphoRows <- reducedPhospho[reducedPhospho$pepIDone == (reduced[row,"Peptide.ID"]) | reducedPhospho$pepIDtwo == (reduced[row,"Peptide.ID"]),]
    
    # we did additionall filtering, check for this
    if(nrow(phosphoRows) == 0)
    {
      next
    }
    
    for(i in 1:nrow(phosphoRows))
    {
      if(reduced[row,"Sequence"] == phosphoRows[i,"plainSeq"])
      {
        numSTY <- phosphoRows[i,"Number.of.Phospho..STY."]
        
        potentialSeq <- phosphoRows[i, "potentialSeq"] # is a list of 1
        potentialSeq <- unlist(potentialSeq) # now is vector
        potentialSeq <- as.list(potentialSeq) # now is a proper length list
        
        withNA <- phosphoRows[i, "withNA"]
        withNA <- unlist(withNA)
        
        largest <- phosphoRows[i, "largest"]
        largest <- unlist(largest)
        
        phosphoSeqs <- rbind(phosphoSeqs, generateOxiSeq(numSTY, potentialSeq, withNA, largest))
      }
    }
    
    # finally, we'll need to overlay the oxi and phospho mods
    for(i in 1:nrow(phosphoSeqs))
    {
      
      for(j in 1:nrow(oxiSeqs))
      {
        phosphoSeq <- phosphoSeqs[i]
        oxiPositions <- str_locate_all(pattern = "m", oxiSeqs[j])
        
        for(k in 1:length(oxiPositions))
        {
          substr(phosphoSeq, oxiPositions[[k]][1], oxiPositions[[k]][1]) <- "m"
        }
        
        seqs <- rbind(seqs, phosphoSeq)
      }
      
 
      
      
    }
    # we did additional filtering, check for this
    if(identical(seqs, logical(0)))
    {
      next
    }
    for(j in 1:nrow(seqs))
    {
      data[nrow(data) + 1,] <- c(seqs[j], reduced[row,]["Calibrated.retention.time"])
      
    }
  }
}


# remove duplicates and negative retention time entries
data <- data[data$retentionTime > 0,]
data <- data[!duplicated(data$sequence),]

write.csv(data,"../data/dataSetTwoFiltered.csv", row.names = TRUE)

