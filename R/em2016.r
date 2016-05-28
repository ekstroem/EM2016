groupsize    <- 4     # number of teams per group
ngroups      <- 4     # number of groups
normalgoals  <- 2.75  # Average number of goals in a match
winpoints    <- 3     # Number of point for winning a match
evenpoints   <- 1     # Number of point for an even match


normalgoals <- normalgoals - .5



##
## Read data. Includes country, group and any skill/background
##

indata <- read.table("uefa2016.txt", header=TRUE)
groupmatches <- read.table("uefa2016groupplan.txt")

groupmatches <- cbind(groupmatches, rep(NA, nrow(groupmatches)), rep(NA, nrow(groupmatches)))



#
# Danske spils odds er udregnet ved 91 / ssh%
# Det vil sige 40.5 ssh for at vinde giver odds 2
# Hvis man fraregner gevinsten til danskespil vil
# odds 2 svare til 50%
#
odds <- c(70, 45, 18, 70,
          70, 7.5, 18, 3.8,
          80, 15, 40, 3.5,
          13, 10, 60, 45
          )

# odds <- odds**sqrt(2)

#
                                        # Initial setup holds the initial data
#
Initial.setup <- data.frame(country, group, odds)

#
# Include model
#
source("skellam.r")



playgame <- function(team1, team2, data) {

    cbind(rep(1,length(team1)), rep(2, length(team2)))
}


nsim <- 100

simulateTournament <- function(n=100, groupmatches, FUN=playgame, data, points.win=3, points.even=1) {

    for (i in 1:n) {
        ##
        ## Simulate group round
        ##

        gmatch <- groupmatches
        d <- data

        ## Find matches to simulate
        findmatches <- apply(groupmatches, 1, FUN=function(x) {any(is.na(x))})
        gmatch[findmatches,3:4] <- playgame(groupmatches[findmatches, 1], groupmatches[findmatches, 2], data)

        gmatch$p1 <- ifelse(gmatch[,3]>gmatch[,4], points.win, 0)
        gmatch$p2 <- ifelse(gmatch[,4]>gmatch[,3], points.win, 0)
        gmatch$p1[gmatch[,3] == gmatch[,4]] <- 1
        gmatch$p2[gmatch[,3] == gmatch[,4]] <- 1

        ## Find / order winners within each group
        d$points <- sapply(1:nrow(d), function(x) { sum(gmatch$p1[gmatch[,1]==x]) + sum(gmatch$p2[gmatch[,2]==x]) } )

        ## order within group
        res <- d[order(rev(d$group), d$points, decreasing=TRUE),]

        ##
        ## Knockout phase
        ## Who designed this system? So obnoxious
        ##
        ## First figure out which 4 3rds that get a spot
        threeranks <- d[seq(3, 23, 4),]

        ## Sort the 3rd positions and get the 4 best groups form them

        get the groups. paste the group names into an ordered string
        thirdpos <- (switch(xxxx,
                           "ABCD"=c(3, 4, 1, 2),
                           "ABCE"=c(3, 1, 2, 5),
                           "ABCF"=c(3, 1, 2, 6),
                           "ABDE"=c(4, 1, 2, 5),
                           "ABDF"=c(4, 1, 2, 6),
                           "ABEF"=c(5, 1, 2, 6),
                           "ACDE"=c(3, 4, 1, 5),
                           "ACDF"=c(3, 4, 2, 6),
                           "ACEF"=c(3, 1, 6, 5),
                           "ADEF"=c(4, 1, 6, 5),
                           "BCDE"=c(3, 4, 2, 5)
                           "BCDF"=c(3, 4, 2, 6),
                           "BCEF"=c(5, 3, 2, 6),
                           "BDEF"=c(5, 4, 2, 6),
                           "CDEF"=c(3, 4, 6, 5))-1)*4+3

        komatches <- matrix(rep(0, 16), ncol=2)
        komatches[1,] <- d$id[2, 10]
        komatches[2,] <- d$id[13, thirdpos[4]]
        komatches[3,] <- d$id[5, thirdpos[2]]
        komatches[4,] <- d$id[21, 18]
        komatches[5,] <- d$id[9, thirdpos[3]]
        komatches[6,] <- d$id[17, 14]
        komatches[7,] <- d$id[1, thirdpos[1]]
        komatches[8,] <- d$id[6, 22]


    }

    res
}







#
# Initial setup
#
rating      <- odds
startpoints <- rep(0, length(country))
matches     <- rep(0, length(country))
wins        <- rep(0, length(country))
draws       <- rep(0, length(country))
goals.f     <- rep(0, length(country))
goals.a     <- rep(0, length(country))

start.match.init <- matrix(c(1,4,2,3),ncol=2, byrow=T)
start.games <- start.match.init
for (i in 2:ngroups) {
  start.games <- rbind(start.games, start.match.init+4*(i-1))
}
round2 <- matrix(c(1,2,3,4),ncol=2, byrow=T)
for (i in 1:ngroups) {
  start.games <- rbind(start.games, round2+4*(i-1))
}
round3 <- matrix(c(3, 1, 4, 2),ncol=2, byrow=T)
for (i in 1:ngroups) {
  start.games <- rbind(start.games, round3+4*(i-1))
}


# land land score
PlayedMatches <- cbind(start.games, rep(NA, 3*8), rep(NA,3*8))


PlayedMatches[ 9, c(3,4)] <- c(1, 1)
PlayedMatches[10, c(3,4)] <- c(4, 1)
PlayedMatches[11, c(3,4)] <- c(1, 0)
PlayedMatches[12, c(3,4)] <- c(0, 1)
PlayedMatches[22, c(3,4)] <- c(1, 1)   # Spanien Italien
PlayedMatches[21, c(3,4)] <- c(3, 1)   # Kroatien Irland
PlayedMatches[15, c(3,4)] <- c(1, 1)   #
PlayedMatches[16, c(3,4)] <- c(1, 2)   #

PlayedMatches[ 1, c(3,4)] <- c(1, 2)   # Grækenland Tjekkiet
PlayedMatches[ 2, c(3,4)] <- c(1, 1)   # Polen Rusland

PlayedMatches[19, c(3,4)] <- c(3, 2)   # Portugal DK
PlayedMatches[20, c(3,4)] <- c(2, 1)   # GER HOL

#
#  Omdanner resultaterne til resultattavle
#
MakeTables<- function(x, countries = seq(along=country)) {
  # Only look at results from played matches
  x <- x[complete.cases(x), seq(1, ncol(x)), drop=FALSE]

  #
  # Only look at the countries specified
  #
#  x <- x[x[,1] %in% countries | x[,2] %in% countries,]

  matches   <<- rep(0, length(country))
  matchtable<- table(x[,c(1,2)])
  matches[as.numeric(names(matchtable))] <<- matchtable

  draws     <<- rep(0, length(country))
  drawtable <- table(x[x[,3]==x[,4],c(1,2)])
  draws[as.numeric(names(drawtable))] <<- drawtable

  wins      <<- rep(0, length(country))
  wintable  <- table(c(x[x[,3]>x[,4],1], x[x[,3]<x[,4],2]))
  wins[as.numeric(names(wintable))] <<- wintable

  goals.f   <<- rep(0, length(country))
  goals.a   <<- rep(0, length(country))

  goals.f.table  <- table(c(rep(x[,1], x[,3]), rep(x[,2], x[,4])))
  goals.a.table  <- table(c(rep(x[,1], x[,4]), rep(x[,2], x[,3])))

  goals.f[as.numeric(names(goals.f.table))] <<- goals.f.table
  goals.a[as.numeric(names(goals.a.table))] <<- goals.a.table

  return(data.frame(matches, wins, draws, goals.f, goals.a))

}


fixture  <- MakeTables(PlayedMatches)
origdata <- data.frame(matches, wins, draws, goals.f, goals.a)

#
# Denne funktion tager to ratings, og returnerer antallet af scorede mål fra ordinær spilletid
#
PlayMatch <- function(team1, team2) {
  #
  # Start by estimating the parameter in the Poisson distributions
  #
  # Step 1: Find relative probability
  #
  prob <- 1/(rating[team1]+1) / (1/(rating[team1]+1) + 1/(rating[team2]+1))
  # CE update 8/6 2012
  p1 <- .91/rating[team1]
  p2 <- .91/rating[team2]
  prob <- p1 / (p1 + p2)
  lambdaA <- FindParameter(prob)
  Agoals <- rpois(1, lambdaA)
  Bgoals <- rpois(1, normalgoals-lambdaA)

  return(c(Agoals, Bgoals))
}

SimulateMatch <- function(team1, team2, debug=F) {
  # Check to see input
  if (length(team1)>1) {
    team2 <- team1[2]
    team1 <- team1[1]
  }

  if (debug) {
    cat("Simulating match between team ", team1, " and ", team2, "\n")
  }

  # Simulate the game and find the result
  result <- PlayMatch(team1, team2)

  invisible(result)
}



ShowCurrentScore <- function(obj, winpoints=3, drawpoints=1, print=T, game.list=initialround, groups=4, groupsize=4) {
  points <- obj$wins*winpoints+obj$draws*drawpoints
  diff.goals <- obj$goals.f-obj$goals.a
  randomsample <- sample(seq(1,length(country),1))
  sortby <- order(rev(group), points, diff.goals, obj$goals.f, randomsample)
  # Har nu fået en rækkefølge at grov-sortere efter:
  # Point, Differens, Scorede mål
  # Skal herefter vurdere dem, der ellers er ens indenfor grupper efter samme kriterier
  # som ovenfor men kun i lyset af kampene mellem holdene indbyrdes
#  MakeTables(initialround)

  res <- data.frame(obj, points, row.names=country)
#  ressort <- res[rev(sortby),]

  if (print)
    print(res[rev(sortby),])
  return(rev(sortby))
}

GetOriginalData <- function(dat) {
  data.frame(matches, wins, draws, goals.f, goals.a)
  matches <<- dat$matches
  wins    <<- dat$wins
  draws   <<- dat$draws
  goals.f <<- dat$goals.f
  goals.a <<- dat$goals.a
}


eigthgames <- matrix(c(1, 2,
                       2, 1,
                       3, 4,
                       4, 3,
                       5, 6,
                       6, 5,
                       7, 8,
                       8, 7
                       ), ncol=2, byrow=T)



quartergames <- matrix(c(
                      1, 2,
                      2, 1,
                      3, 4,
                      4, 3 ), ncol=2, byrow=T)


#
# This function decides the ranking / scores of each group
# Input needed - a table with matches. NA for scores will be estimated
GroupWinner <- function(nsim=20, initial.matches, ncountries=16) {

  result <- matrix(rep(0, ncountries*nsim), ncol=nsim)
  cupwinner <- numeric(nsim)
  finals <- numeric(nsim*2)
  semifinals <- numeric(nsim*4)

  # Identify which games are not played yet
  missing.games <- !complete.cases(initial.matches)
  if (length(missing.games)==0) {
    stop("All initial matches are already played")
  }

  for (i in 1:nsim) {
    if (i %% 10 == 0) # Print notification for each 10 simulations
      cat(".")

    initialround <- initial.matches
    res <- t(apply(initialround[missing.games,c(1,2)], 1, SimulateMatch))

    initialround[missing.games,3] <- res[,1]
    initialround[missing.games,4] <- res[,2]

    # Have now simulated all the initial missing games

    current.fixture <- MakeTables(initialround)
    placering <- ShowCurrentScore(current.fixture, print=F, game.list=initialround)

#    print(current.fixture)

#    print(placering)

    result[,i] <- placering

    groupscores <- matrix(placering, ncol=4, byrow=T)
    groupwinners <- groupscores[,c(1,2)]

#    print(groupscores)
    #
    # Now go on to quarter, semi and finals
    #

##    temp <- EstimateEigthRound(groupwinners[,1], groupwinners[,2])
##    temp <- matrix(temp[c(1,3,5,7,2,4,6,8)], ncol=2, byrow=TRUE)

    temp <- EstimateQuarterRound(groupwinners[,1], groupwinners[,2])
#    temp <- EstimateQuarterRound(temp[,1], temp[,2])
     semifinals[(4*i-3):(4*i)] <- temp
    temp <- matrix(temp[c(1, 3, 2, 4)], ncol=2)

    temps <- EstimateSemiRound(temp[,1], temp[,2])
    finals[(2*i-1):(2*i)] <- temps

    tempf <- EstimateFinalRound(temps[1], temps[2])
    cupwinner[i] <- tempf[1]
  }

  # Country
  ncountry <- ncountries
  y <- factor(rep(1:ncountry,nsim), labels=rep(c("1","2","3","4"), length(unique(group))))
  print(table(factor(result, labels=country), y, dnn=c("Country", "Position")))



  #
  #  Select the two positions in which Denmarks group wins
  #
#  denmark <- result[c(9,10),]

  # Probability of continuing
#  cat("Probability of Denmark continuing in tournament:\n")
#  print(length(denmark[denmark==10])/nsim)

  # Probability of winning group
#  cat("Probability of Denmark winning its group:\n")
#  denmark <- denmark[1,]
#  print(length(denmark[denmark==10])/nsim)

  # Probability of winning group
#  cat("Probability of Denmark winning EM:\n")
#  print(sum(cupwinner==10)/nsim)

   quarterwinners <- table(semifinals)
   names(quarterwinners) <- country[as.numeric(names(quarterwinners))]
   cat("Who will go to semifinals:\n")
   print(quarterwinners*100/nsim)


   semiwinners <- table(finals)
   names(semiwinners) <- country[as.numeric(names(semiwinners))]
   cat("Who will go to finals:\n")
   print(semiwinners*100/nsim)

   emwinners <- table(cupwinner)
   names(emwinners) <- country[as.numeric(names(emwinners))]

   par(mar=c(6,4,1,1)+.1)
   barplot(as.numeric(emwinners), names.arg=names(emwinners), las=2)

   cat("Who will win:\n")
   print(emwinners*100/nsim)


  return(emwinners)
}

EstimateEigthRound  <- function(winners, runups, games) {

  goals <- apply(cbind(winners[eigthgames[,1]],runups[eigthgames[,2]]), 1, SimulateMatch)

  ### Gemmer spillede kampe
#  goals[,1] <- c(0,10)   # England - Brasilien
#  goals[,2] <- c(10,0)   # Tyskland - Usa
#  goals[,3] <- c(0,10)   # Spanien - Korea
#  goals[,4] <- c(0,10)   # Senegal - Tyrkiet

  # Simulate penalty kick
  differens <- goals[1,]-goals[2,]

  goals[1,differens==0] <- goals[1,differens==0]+2*rbinom(length(differens[differens==0]),1,.5)-1

  winner <- 1.5 + sign(goals[2,]-goals[1,])/2

  return(diag(cbind(winners[eigthgames[,1]],runups[eigthgames[,2]])[,winner]))
}




EstimateQuarterRound  <- function(winners, runups, games) {

#  print(cbind(winners[quartergames[,1]],runups[quartergames[,2]]))
#  print(cbind(winners, runups))

#  goals <- apply(cbind(winners,runups), 1, SimulateMatch)
  goals <- apply(cbind(winners[quartergames[,1]],runups[quartergames[,2]]), 1, SimulateMatch)

  ### Gemmer spillede kampe
#  goals[,1] <- c(0,10)   # England - Brasilien
#  goals[,2] <- c(10,0)   # Tyskland - Usa
#  goals[,3] <- c(0,10)   # Spanien - Korea
#  goals[,4] <- c(0,10)   # Senegal - Tyrkiet

  # Simulate penalty kick
  differens <- goals[1,]-goals[2,]

  goals[1,differens==0] <- goals[1,differens==0]+2*rbinom(length(differens[differens==0]),1,.5)-1

  winner <- 1.5 + sign(goals[2,]-goals[1,])/2

  return(diag(cbind(winners[quartergames[,1]],runups[quartergames[,2]])[,winner]))
}

EstimateSemiRound  <- function(winners, runups) {
  goals <- apply(cbind(winners,runups), 1, SimulateMatch)

  # Simulate penalty kick
  differens <- goals[1,]-goals[2,]

  goals[1,differens==0] <- goals[1,differens==0]+2*rbinom(length(differens[differens==0]),1,.5)-1

  winner <- 1.5 + sign(goals[2,]-goals[1,])/2

  return(diag(cbind(winners,runups)[,winner]))
}

EstimateFinalRound  <- function(winners, runups) {
  goals <- apply(cbind(winners,runups), 1, SimulateMatch)

  # Simulate penalty kick
  differens <- goals[1,]-goals[2,]

  goals[1,differens==0] <- goals[1,differens==0]+2*rbinom(length(differens[differens==0]),1,.5)-1

  winner <- 1.5 + sign(goals[2,]-goals[1,])/2

  invisible(c(winners,runups)[c(winner, 3-winner)])
}



end.result <- GroupWinner(10000, PlayedMatches, ncountries=groupsize*ngroups)
