library(dplyr)
library(skellam)
library(ggplot2)

set.seed(10) # for reproducability

##### Q1.1 and Q1.2

data <- read.csv('USA.csv',header=TRUE)
data <-  as.data.frame(data)

data$Home_Conference <- rep('dummy', nrow(data))
data$Away_Conference <- rep('dummy', nrow(data))
data$Home_GD <- rep(0, nrow(data)) # Home goal difference
data$Away_GD <- rep(0, nrow(data)) # Away goal difference

for (i in 1:nrow(data)){
  
  data$Home_GD[i] <- data$HG[i] - data$AG[i]
  data$Away_GD[i] <- data$AG[i] - data$HG[i]
  
  # Putting Atlanta Utd and Atlanta United as the same team
  if(data$Home[i] == 'Atlanta Utd'){
    data$Home[i] <- 'Atlanta United'
  }
  
  if(data$Home[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew',
                         'DC United', 'FC Cincinnati', 'Houston Dynamo',
                         'Inter Miami', 'Montreal Impact', 'Nashville SC',
                         'New England Revolution', 'New York City',
                         'New York Red Bulls', 'Orlando City',
                         'Philadelphia Union', 'Sporting Kansas City',
                         'Toronto FC'))
  {
    data$Home_Conference[i] <- 'East'
  }
  
  if(data$Home[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas',
                         'Los Angeles FC', 'Los Angeles Galaxy',
                         'Minnesota United', 'Portland Timbers',
                         'Real Salt Lake', 'San Jose Earthquakes',
                         'Seattle Sounders', 'Vancouver Whitecaps'))
  {
    data$Home_Conference[i] <- 'West'
  }
  
  if(data$Season[i] >2014 && (data$Home[i] == 'Sporting Kansas City' ||
                               data$Home[i] == 'Houston Dynamo')){
    data$Home_Conference[i] <- 'West'
  }
  
  if(data$Away[i] == 'Atlanta Utd'){
    data$Away[i] <- 'Atlanta United'
  }
  
  if(data$Away[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew',
                         'DC United', 'FC Cincinnati', 'Houston Dynamo',
                         'Inter Miami', 'Montreal Impact', 'Nashville SC',
                         'New England Revolution', 'New York City',
                         'New York Red Bulls', 'Orlando City',
                         'Philadelphia Union', 'Sporting Kansas City',
                         'Toronto FC'))
  {
    data$Away_Conference[i] <- 'East'
  }
  
  if(data$Away[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas',
                         'Los Angeles FC', 'Los Angeles Galaxy',
                         'Minnesota United', 'Portland Timbers',
                         'Real Salt Lake', 'San Jose Earthquakes',
                         'Seattle Sounders', 'Vancouver Whitecaps'))
  {
    data$Away_Conference[i] <- 'West'
  }
  
  if(data$Season[i] >2014 && (data$Away[i] == 'Sporting Kansas City' ||
                              data$Away[i] == 'Houston Dynamo')){
    data$Away_Conference[i] <- 'West'
  }
}

data$Date <- as.Date(data$Date, "%d/%m/%Y") # Putting date into date format
data$HomeNum <- factor(data$Home)
data$HomeNum <- as.numeric(data$HomeNum) # Giving home teams a number
data$AwayNum <- factor(data$Away)
data$AwayNum <- as.numeric(data$AwayNum) # Giving away teams a number

nt <- length(unique(data$Home)) # Number of teams in data

##### Q 2.1

fitset <- subset(data, Date<=as.Date("2015-12-06") )
simset <- subset(data, Date>as.Date("2015-12-06") & Date<=as.Date("2016-10-23"))

s <- 0 # The number of games between teams of the same conference in fitset
for (i in 1:nrow(fitset)){
  if(fitset$Home_Conference[i] == fitset$Away_Conference[i]){
    s <- s+1
  }
}

diff_prop <- 1 - (s/nrow(fitset))
diff_prop # Proportion of games between teams of different conferences in fitset

fit_east <- subset(fitset,Home_Conference == "East" & Away_Conference == "East")
fit_west <- subset(fitset,Home_Conference == "West" & Away_Conference == "West")
fit_same<-subset(fitset,(Home_Conference == "East" & Away_Conference == "East")
                    | (Home_Conference == "West" & Away_Conference == "West") )

##### Q2.2

# Using the home goal differences in each conference to see the difference
# in mean home ground advantage, and test it's significance
t.test(fit_east$Home_GD, fit_west$Home_GD,paired=FALSE,alternative="two.sided")

##### Q2.3

fit_same$TG_MA <- rep('N/A', nrow(fit_same)) # Moving Average Column

n <- 20 # Moving Average over this number of games

for (i in n:nrow(fit_same)){
  MA <- 0
  
  for (j in (i-n+1):i){
    if (fit_same$Season[i-n+1]==fit_same$Season[i]){
      MA <- MA + fit_same$HG[j]+ fit_same$AG[j]
    } else{
      MA <- 'N/A' # These NAs are ok. For periods at the start of seasons
    }
  }
  fit_same$TG_MA[i] <- MA
}
fit_same$TG_MA <- as.numeric(fit_same$TG_MA)

# Plotting total goals moving average for the fit_same dataset
MovingAverages <- data.frame(Date=fit_same$Date, Moving_Average=fit_same$TG_MA)
ggplot(data=MovingAverages, mapping = aes(x = Date, y = Moving_Average)) +
  geom_line(colour = 'blue') +
  ggtitle('20 Game Total Goals Moving Average Over Time') +
  xlab('Date') + ylab('20 Game Total Goals Moving Average') +
  theme_grey(base_size = 12) + theme(plot.title = element_text(hjust = 0.5))

##### Q3.2

InvLogit=function(x){
  "Inverse Logit function. To constrain inputs between 0 and 1."
  return(exp(x)/(1+exp(x)))
}

Model_LL <- function(P, data){
  "This gives the negative model log likelihood.
  P is the vector of model parameters, before being constrained via InvLogit.
  data is the fitted data being used, containing home and away goals."
  
  P=InvLogit(P) # Constraining the initial model parameters inputted
  
  A=P[1:nt] # Alpha values for each team
  B=-1*P[(nt+1):(2*nt)] # Beta values for each team
  G=P[(2*nt+1)] # Gamma value
  E=P[(2*nt+2)] # Eta value
  
  LL <- 0 # Log likelihood
  
  for (i in 1:nrow(data)){
    lambda <- exp(A[data$HomeNum[i]] + B[data$AwayNum[i]] + G + E/2)
    mu <- exp(A[data$AwayNum[i]] + B[data$HomeNum[i]] + G - E/2)
    
    LL <- LL + (-lambda-log(factorial(data$HG[i]))+data$HG[i]*log(lambda)) +
      (-mu-log(factorial(data$AG[i]))+data$AG[i]*log(mu))
  }
  
  return(-LL) # Neg. log lik. outputted. This should be minimised for best fit
}

# Fitting the model param.s by minimising the neg. log lik. using fit_same data
fit=optim(c(rep(2, nt), rep(2, nt), 2, 2),Model_LL, data=fit_same,
          method="BFGS",
          control=list(maxit=10000,REPORT=1, trace=1,fnscale=100),
          hessian=FALSE)
OUT=InvLogit(fit$par) # Vector of fitted model parameters

A=OUT[1:nt] # Fitted alpha values for each team
B=-1*OUT[(nt+1):(2*nt)] # Fitted beta values for each team
G=OUT[(2*nt+1)] # Fitted gamma value
E=OUT[(2*nt+2)] # Fitted eta value

Team <- unique(fit_same$Home)
nt_f <- length(Team) # Number of teams in fit_same

Alpha <- rep(0, nt_f) # Column of alpha values for each team in a table
Neg_Beta <- rep(0, nt_f) # Column of neg. beta values for each team in a table
Alpha_Rank <- rep(0, nt_f) # Alpha rankings for each team in a table
Neg_Beta_Rank <- rep(0, nt_f) # Neg. beta rankings for each team in a table

for (i in 1:nt_f){
  Alpha[i] <- A[subset(fit_same, Home==Team[i])$HomeNum[1]]
  Neg_Beta[i] <- -1*B[subset(fit_same, Home==Team[i])$HomeNum[1]]
}

for (i in 1:nt_f){
  h <- 0
  for (j in 1:nt_f){
    if (Alpha[j] > Alpha[i]){
      h <- h + 1
    }
  }
  Alpha_Rank[i] <- h + 1
}

for (i in 1:nt_f){
  h <- 0
  for (j in 1:nt_f){
    if (Neg_Beta[j] > Neg_Beta[i]){
      h <- h + 1
    }
  }
  Neg_Beta_Rank[i] <- h + 1
}


Params_Table <- data.frame(Team, Alpha, Alpha_Rank, Neg_Beta, Neg_Beta_Rank)
# ordered alpha and neg. beta table for each team, along with the rankings
Params_Table <- Params_Table %>% arrange(Team)

##### Q5.1 and Q5.2

simset$lambda <- rep(0, nrow(simset)) # lambdas for games in simset
simset$mu <- rep(0, nrow(simset)) # mus for games in simset
simset$phome <- rep(0, nrow(simset)) # prob.s of home wins for games in simset
simset$pdraw <- rep(0, nrow(simset)) # prob.s of draws for games in simset
simset$paway <- rep(0, nrow(simset)) # prob.s of away wins for games in simset

for (i in 1:nrow(simset)){
  simset$lambda[i] <- exp(A[simset$HomeNum[i]]+B[simset$AwayNum[i]]+G+E/2)
  simset$mu[i] <- exp(A[simset$AwayNum[i]]+B[simset$HomeNum[i]]+G-E/2)
  # Skellam distribution used to calculate probabilities
  simset$pdraw[i]=dskellam(0,simset$lambda[i],simset$mu[i])
  simset$paway[i]=dskellam(-1,simset$lambda[i],simset$mu[i])
  simset$phome[i]=1-simset$pdraw[i]-simset$paway[i]
}

##### Q5.3

Team <- unique(simset$Home)
nt_s <- length(Team) # Number of teams in simset
Conference <- rep('dummy', nt_s)

for(i in 1:nt_s){
  if(Team[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew',
                    'DC United', 'FC Cincinnati', 'Inter Miami',
                    'Montreal Impact', 'Nashville SC',
                    'New England Revolution', 'New York City',
                    'New York Red Bulls', 'Orlando City',
                    'Philadelphia Union', 'Toronto FC'))
  {
    Conference[i] <- 'East'
  }
  if(Team[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas',
                    'Los Angeles FC', 'Los Angeles Galaxy',
                    'Minnesota United', 'Portland Timbers',
                    'Real Salt Lake', 'San Jose Earthquakes',
                    'Seattle Sounders', 'Vancouver Whitecaps',
                    'Sporting Kansas City', 'Houston Dynamo'))
  {
    Conference[i] <- 'West'
  }
}
Points <- rep(0, nt_s)
Table <- data.frame(Conference, Team, Points) # Expected overall table

for (i in 1:nt_s){
  p <- 0
  
  for (j in 1:nrow(simset)){
    if (simset$Home[j]==Table$Team[i]){
      # Expected draw and win points (for home team) added
      p <- p + simset$pdraw[j] + simset$phome[j]*3
    }
    
    if (simset$Away[j]==Table$Team[i]){
      # Expected draw and win points (for away team) added
      p <- p + simset$pdraw[j] + simset$paway[j]*3
    }
  }
  
  Table$Points[i]=p
}

# Expected overall table arranged by points
Exp_Table_Overall <- Table %>% arrange(desc(Points))
# Expected east table
Exp_Table_East <- subset(Exp_Table_Overall, Conference=='East')
# Expected west table
Exp_Table_West <- subset(Exp_Table_Overall, Conference=='West')

##### Q5.4

# Model A data
A_data <- read.csv('mls_simset_predictions.csv',header=TRUE)
A_data <-  as.data.frame(A_data)

A_data$HG <- rep(0, nrow(A_data)) # Home goals added to A_data
A_data$AG <- rep(0, nrow(A_data)) # Away goals added to A_data
A_data$Res <- rep('dummy', nrow(A_data)) # Results added to A_data

# Adding HG, AG and Res columns in by matching vs simset.
# Both data sets include the same games, but the order is different.
for (i in 1:nrow(A_data)){
  for (j in 1:nrow(simset)){
    if (A_data$Date[i]==simset$Date[j] && (A_data$Home[i]==simset$Home[j]) &&
        (A_data$Away[i]==simset$Away[j])){
      A_data$HG[i] <- simset$HG[j]
      A_data$AG[i] <- simset$AG[j]
      A_data$Res[i] <- simset$Res[j]
    }
  }
}

RPS=function(X,phome, pdraw){
  "Ranked probability score function - for results
  X are the actual results
  phome and pdraw and the probabilities oh a home win and draw"
  n <- length(X) # number of games
  score <- 0
  
  for(i in 1:n){
    z1 <- 0; z2 <- 0 # identifiers for home win and draw
    
    if(X[i]=="H"){
      z1 <- 1
    }
    if(X[i]=="D"){
      z2 <- 1
    }
    
    score <- score + (z1-phome[i])^2 + ((z1-phome[i]) + (z2-pdraw[i]))^2
  }
  
  RES <- score/(2*n)
  
  return(RES)}

RPS_TG=function(HG, AG, EHG, EAG){
  "Ranked probability score function - for total goals
  HG and AG are the actual home and away goals
  EHG and EAG and the expected home and away goals"
  n <- length(HG) # number of games
  score <- 0
  
  for(i in 1:n){
    z <- rep(0,20) # identifiers for number of goals, up to 20 maximum
    p <- rep(0,20) # probabilities for number of goals, up to 20 maximum
    
    z[(HG[i]+AG[i])] <- 1
    
    for(j in 1:20){
      p[j]=dpois(j, (EHG[i]+EAG[i]))
    }
    
    for(j in 1:20){
      s <- 0
      for(k in 1:j){
        s <- s + (z[k]-p[k])
      }
      score <- score + s^2
    }
  }
  
  RES <- score/(20*n)
  
  return(RES)}

# RPSs for fitted model and model A for match results.
RPS_fit <- RPS(simset$Res, simset$phome, simset$pdraw)
RPS_A <- RPS(A_data$Res, A_data$expected_team1_win, A_data$expected_draw)

# RPSs for fitted model and model A for total goals.
RPS_TG_fit <- RPS_TG(simset$HG, simset$AG, simset$lambda, simset$mu)
RPS_TG_A <- RPS_TG(A_data$HG, A_data$AG, A_data$expected_team1_goals,
                   A_data$expected_team2_goals)

##### Q6.1

sim_table <- function(simset){
  "This creates a simulated table for the simset data.
  The fitted lamda and mu values are used."
  
  TableOut <- list()
  
  simset$sim_HG <- rep(0, nrow(simset)) # Similated home goals
  simset$sim_AG <- rep(0, nrow(simset)) # Similated away goals
  simset$sim_Res <- rep('dummy', nrow(simset)) # Similated results
  
  Team <- unique(simset$Home)
  Conference <- rep('dummy', length(Team))
  
  for(i in 1:length(Team)){
    if(Team[i] %in% c('Atlanta United', 'Chicago Fire', 'Columbus Crew',
                      'DC United', 'FC Cincinnati', 'Inter Miami',
                      'Montreal Impact', 'Nashville SC',
                      'New England Revolution', 'New York City',
                      'New York Red Bulls', 'Orlando City',
                      'Philadelphia Union', 'Toronto FC'))
    {
      Conference[i] <- 'East'
    }
    if(Team[i] %in% c('Chivas USA', 'Colorado Rapids', 'FC Dallas',
                      'Los Angeles FC', 'Los Angeles Galaxy',
                      'Minnesota United', 'Portland Timbers',
                      'Real Salt Lake', 'San Jose Earthquakes',
                      'Seattle Sounders', 'Vancouver Whitecaps',
                      'Sporting Kansas City', 'Houston Dynamo'))
    {
      Conference[i] <- 'West'
    }
  }
  
  Points <- rep(0, length(Team)) # Resulting points
  GD <- rep(0, length(Team)) # Resulting goal differences
  GS <- rep(0, length(Team)) # Resulting goals scored
  Table <- data.frame(Conference, Team, Points, GD, GS)
  
  # Match sumulations
  for (i in 1:nrow(simset)){
    
    simset$sim_HG[i] <- rpois(1, simset$lambda[i])
    simset$sim_AG[i] <- rpois(1, simset$mu[i])
    
    if (simset$sim_HG[i] > simset$sim_AG[i]){
      simset$sim_Res[i] <- "H"
    }
    
    if (simset$sim_HG[i] == simset$sim_AG[i]){
      simset$sim_Res[i] <- "D"
    }
    
    if (simset$sim_AG[i] > simset$sim_HG[i]){
      simset$sim_Res[i] <- "A"
    }
  }
  
  # Table columns populating
  for (i in 1:nrow(simset)){
    for (j in 1:nrow(Table)){
      if (simset$Home[i]==Table$Team[j]){
        if(simset$sim_Res[i]=="H"){
          Table$Points[j] <- Table$Points[j] + 3
        }
        if(simset$sim_Res[i]=="D"){
          Table$Points[j] <- Table$Points[j] + 1
        }
        Table$GD[j] <- Table$GD[j] + (simset$sim_HG[i]-simset$sim_AG[i])
        Table$GS[j] <- Table$GS[j] + simset$sim_HG[i]
      }
      if (simset$Away[i]==Table$Team[j]){
        if(simset$sim_Res[i]=="A"){
          Table$Points[j] <- Table$Points[j] + 3
        }
        if(simset$sim_Res[i]=="D"){
          Table$Points[j] <- Table$Points[j] + 1
        }
        Table$GD[j] <- Table$GD[j] + (simset$sim_AG[i]-simset$sim_HG[i])
        Table$GS[j] <- Table$GS[j] + simset$sim_AG[i]
      }
    }
  }
  
  TableOut$Overall <- Table %>% arrange(desc(Points), desc(GD), desc(GS), Team)
  TableOut$East <- subset(TableOut$Overall, Conference=='East')
  TableOut$West <- subset(TableOut$Overall, Conference=='West')
  
  return(TableOut)
}

# Single table simulation
sim_1 <- sim_table(simset)

##### Q6.2

n <- rep(0,10000) # Number of instances of LA Galaxy finishing in west top 2.
for (i in 1:10000){
  sim <- sim_table(simset)
  
  if(sim$West$Team[1]=='Los Angeles Galaxy' ||
     sim$West$Team[2]=='Los Angeles Galaxy'){
    n[i] <- 1
  }
}
p <- sum(n)/10000 # Probability of LA Galaxy finishing in west top 2.

##### Q6.3

gamma   <- 0.95 # For a 95% confidence interval
epsilon <- 0.001 # Monte carlo error of 0.1%
q <- qnorm((1+gamma)/2)
N <- ceiling((1-p)*p*q^2/epsilon^2) # Number of simulations required
