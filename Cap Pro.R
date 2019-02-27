#library(dplyr)
#library(tidyr)
#library(data.table)

# Read the file Batting.csv
batting <- fread(paste(getwd(),"/Capstone Project/Batting.csv", sep = ""))

# Create a new column containing the number of singles here called "X1B"
X1B <- batting$H - batting$'2B' - batting$'3B' - batting$HR

# Add BA, OBP, and SLG columns to battinng data table
batting <- batting %>% mutate(BA = H/AB, OBP = (H+BB+HBP)/(AB+BB+HBP+SF), SLG = (X1B+(2*batting$'2B')+(3*batting$'3B')+(4*HR))/AB )

# Read the file Salaries.csv
sal <- fread(paste(getwd(),"/Capstone Project/Salaries.csv", sep = ""))

# Delete all rows from batting that contain information from before 1985 (this is where our salaries data table begins)
batting <- batting %>% filter(yearID >= 1985)

# Merge the sal data table to the batting data table by playerID and yearID
combo <- merge(batting, sal, by = c('playerID', 'yearID'))

# Create a data table containing the information about the players lost in 2001
lostPlayers <- combo %>% filter(playerID %in% c('giambja01','damonjo01','saenzol01'))
lostPlayers <- lostPlayers %>% filter(yearID == 2001)

# Create a data table containing information from only the year 2001, to compare with the lost players
combo2001 <- combo %>% filter(yearID == 2001)

# Delete irrelevant columns from lostPlayers data table.
lostPlayers <- lostPlayers %>% select(playerID, H, '2B', '3B', HR, OBP, SLG, BA, AB)

# Delete irrelevant columns from combo2001 data table.
reducedCombo2001 <- combo2001 %>% select(playerID, H, '2B', '3B', HR, OBP, SLG, BA, AB, salary)

# Delete all rows from reducedCombo2001 with missing data
reducedCombo2001 <- na.omit(reducedCombo2001)

# Delete all rows (players) from reducedCombo2001 that have repetition in playerID column. These player's salary information is ambiguous.
reducedCombo2001 <- distinct(reducedCombo2001, playerID, .keep_all = T)

# Add a column to reducedCombo2001 with the norm of the each players (AB, OBP) vector.
reducedCombo2001 <- reducedCombo2001 %>% mutate(score = sqrt(OBP*OBP + AB*AB))

# Arrange reducedCombo2001 in descending order by score.
reducedCombo2001 <- reducedCombo2001 %>% arrange(desc(score))

# Conditions that need to be satisfied regarding replacing lost players.
lpTotalSalary <- 15000000
lpTotalAB <- sum(lostPlayers$AB)
lpAvgOBP <- mean(lostPlayers$OBP)

# Variable that will be used in the following while loop
winner <- F

# Starting from the top, search for the first three players whose total salary is less that lpTotalSalary, whose AB average is at least lpTotalAB, and whose OBP average is at least lpAvgOBP.
# Since the data table was arranged in descending order by score, and since we are using score as our measure of desirability, we have chosen the best three players we can.
while(winner == F){
  i <- 1
  j <- 2
  k <- 3
  while(winner == F && i < (length(reducedCombo2001$playerID)-2)){
    while(winner ==F && j < (length(reducedCombo2001$playerID)-1)){
      while(winner == F && k < length(reducedCombo2001$playerID)){
        tempReducedCombo2001 <- reducedCombo2001 %>% select(playerID, salary, AB, OBP,score) %>% slice(c(i,j,k))
        if( sum(tempReducedCombo2001$salary) < lpTotalSalary && sum(tempReducedCombo2001$AB) >= lpTotalAB && mean(tempReducedCombo2001$OBP) >= lpAvgOBP){
          print(tempReducedCombo2001)
          winner <- T
          break
      }else{
        k <- k+1
      }
      j <- j+1
    }
    i <- i+1
  }
}
}

