normalgoals  <- 2.75  # Average number of goals in a match
winpoints    <- 3     # Number of point for winning a match
evenpoints   <- 1     # Number of point for an even match


normalgoals <- normalgoals - .5

source("skellam.r")


##
## Read data. Includes country, group and any skill/background
##

indata <- read.table("uefa2016.txt", header=TRUE)
indata$id <- 1:24


groupmatches <- read.table("uefa2016groupplan.txt", header=TRUE)
groupmatches <- data.frame(groupmatches, goals1=rep(NA, nrow(groupmatches)), goals2=rep(NA, nrow(groupmatches)))


                                        #
# Include model
#
source("skellam.r")

## Initial rating (Danske spil, 31/5)
indata$rating <- c(400, 4, 200, 60, 9, 70, 150, 70, 4.25, 125, 23, 80, 30, 400, 6.5, 80, 12, 17, 100, 80, 35, 300, 100, 23)
indata$elo <- c(1591, 1948, 1725, 1744,
                1941, 1747, 1748, 1638,
                2009, 1589, 1762, 1805,
                1795, 1730, 1983, 1795,
                1905, 1850, 1752, 1729,
                1704, 1670, 1647, 1889)

## Boost Frankrigs rating pga hjemmebanefordel med 5%
indata$elo[2] <- indata$elo[2]*1.05

elo <- indata$elo

sankey <- list(nodes=data.frame(id=c(1:32),
                   name=c(as.character(indata$country), paste("KO", 1:8))),
               links=data.frame(source=rep(1:24, 8),
                   target=rep(25:32, times=rep(24,8)),
                   value=rep(0, 8*24))
               )

save(indata, file="mytab.rda")


playgame <- function(team1, team2, data, musthavewinner=FALSE, k=58) {
    res <- cbind(rpois(length(team1), lambda=2.75), rpois(length(team2), lambda=2.75))

    ## Old method
    prob <- 1/(data$rating[team1]+1) / (1/(data$rating[team1]+1) + 1/(data$rating[team2]+1))
    ## CE update 8/6 2012
    p1 <- .91/data$rating[team1]
    p2 <- .91/data$rating[team2]
    prob <- p1 / (p1 + p2)
    lambdaA <- FindParameter(prob)
    Agoals <- rpois(length(prob), lambdaA)
    Bgoals <- rpois(length(prob), normalgoals-lambdaA)

    res <- cbind(Agoals, Bgoals)

    ## ELO method
    expe <- sapply(seq_len(length(team1)), function(i) {
                        myprob <- 1/(1 + 10^((elo[team2[i]] - elo[team1[i]])/400))
                        myres <- rbinom(1, size=1, prob=myprob)
                        elo[team1[i]] <<- elo[team1[i]] + k*(myres-myprob)
                        elo[team2[i]] <<- elo[team2[i]] + k*(-myres+myprob)
                        myres })
#    expe <- rbinom(length(prob), size=1, prob=1/(1 + 10^((data$elo[team2] - data$elo[team1])/400)))

    res <- cbind(expe, 1-expe)

    ## Simulate penalty kick situation
    if (musthavewinner) {
        pick <- res[,1]==res[,2]
        ## Give a point randomly
        rassign <- sample(1:2, size=sum(pick), replace=TRUE)
        res[cbind(seq_len(length(pick))[pick], rassign)] <- res[cbind(seq_len(length(pick))[pick], rassign)] +1
    }
    res
}

findWinner <- function(matches, points.win=winpoints, points.even=evenpoints) {
    matches$p1 <- ifelse(matches$goals1>matches$goals2, points.win, 0)
    matches$p2 <- ifelse(matches$goals2>matches$goals1, points.win, 0)
    matches$p1[matches$goals1 == matches$goals2] <- points.even
    matches$p2[matches$goals1 == matches$goals2] <- points.even
    matches$winner <- rep(0, nrow(matches))
    matches$winner[matches$p1==points.win] <- matches$team1[matches$p1==points.win]
    matches$winner[matches$p2==points.win] <- matches$team2[matches$p2==points.win]
    matches
}


simulateMatches <- function(matches, data, musthavewinner=FALSE) {
    ## Find relevant matches to simulate
    findmatches <- apply(matches, 1, FUN=function(x) {any(is.na(x))})
    ## Simulate matches
    matches[findmatches,c("goals1", "goals2")] <- playgame(matches[findmatches, 1], matches[findmatches, 2], data, musthavewinner=musthavewinner)
    findWinner(matches)
}

library(data.table)


simulateTournament <- function(n=100, groupmatches, FUN=playgame, data, points.win=3, points.even=1) {

    data$wins <- rep(0, nrow(data))
    for (i in 1:n) {
        ##
        ## Simulate group stage
        ##
        gmatch <- groupmatches
        d <- data
        elo <<- data$elo

        ## Simulate the group stage
        gmatch <- simulateMatches(gmatch, d, musthavewinner=FALSE)
        # print(gmatch)
        individualmatrix <- diag(nrow(d))
        ## Fill in indiviual matches
        individualmatrix[cbind(pmin(gmatch$team1, gmatch$team2),pmax(gmatch$team1, gmatch$team2))] <- gmatch$goals1 - gmatch$goals2

        ## Find / order winners within each group
        d$points <- sapply(1:nrow(d), function(x) { sum(gmatch$p1[gmatch[,1]==x]) + sum(gmatch$p2[gmatch[,2]==x]) } )

        ## order within group
        res <- d[order(rev(d$group), d$points, decreasing=TRUE),]
#        print(gmatch)
#        print(res)

        dt <- data.table(res)

        ffp <- function(x) {
#            cat("Starting ff with x\n")
#            print(x)
            listofteams <- x$id
            partdf <- gmatch[gmatch$team1 %in% listofteams & gmatch$team2 %in% listofteams, ]
#            cat("Found these:\n")
#            print(partdf)
#            cat("\n\n")
               x$lpoints <- rep(0, nrow(x))
#            cat("and x is\n")
#            print(x)
            for (i in 1:nrow(partdf)) {
#                cat("Iteration ", i, "\n")
#                   cat("Now x is ----->\n")
#                   print(x)
#                   cat("=====\n")
#                   print(partdf$team2[i])
#                   print(x$id)
                                        #                   print(x$id==partdf$team2[i])

                   x$lpoints[x$id==partdf$team1[i]] <- x$lpoints[x$id==partdf$team1[i]] + partdf$p1[i]
                   x$lpoints[x$id==partdf$team2[i]] <- x$lpoints[x$id==partdf$team2[i]] + partdf$p2[i]

                   x$lgoals[x$id==partdf$team2[i]] <- x$lgoals[x$id==partdf$team2[i]] + partdf$goals1[i]
                   x$lgoals[x$id==partdf$team2[i]] <- x$lgoals[x$id==partdf$team2[i]] + partdf$goals2[i]

#                   print(x$lpoints)
               }
#               print("Returning: ")
#               print(x)
#               print(partdf)
#               print(x$lpoints)
               x$lpoints
        }

        ffg <- function(x) {
#            cat("Starting ff with x\n")
#            print(x)
            listofteams <- x$id
            partdf <- gmatch[gmatch$team1 %in% listofteams & gmatch$team2 %in% listofteams, ]
#            cat("Found these:\n")
#            print(partdf)
#            cat("\n\n")
            x$lpoints <- rep(0, nrow(x))
            x$lgoals <- rep(-20, nrow(x))
#            cat("and x is\n")
#            print(x)
            for (i in 1:nrow(partdf)) {
#                cat("Iteration ", i, "\n")
#                   cat("Now x is ----->\n")
#                   print(x)
#                   cat("=====\n")
#                   print(partdf$team2[i])
#                   print(x$id)
                                        #                   print(x$id==partdf$team2[i])

                   x$lgoals[x$id==partdf$team1[i]] <- max(x$lgoals[x$id==partdf$team1[i]], partdf$goals1[i] - partdf$goals2[i])
                   x$lgoals[x$id==partdf$team2[i]] <- max(x$lgoals[x$id==partdf$team2[i]], partdf$goals2[i] - partdf$goals1[i])

#                   print(x$lpoints)
               }
#               print("Returning: ")
#               print(x)
#               print(partdf)
#               print(x$lpoints)
               x$lgoals
        }

        flipacoin <- function(x) {rnorm(nrow(x))}


        dt[, `:=`(lpoints=ffp(.SD), lgoals=ffg(.SD), coinflip=flipacoin(.SD)), by="group,points"]

#        print(dt)

        res <- as.data.frame(dt)
        res <- res[order(rev(res$group), res$points, rev(res$lpoints), res$lgoals, res$coinflip, decreasing=TRUE),]

        ## Update ELO rating

        ## Now for each equal point score within each group find the order of the given participants


        ##
        ## Knockout phase
        ## Who designed this system? So obnoxious
        ##
        ## First figure out which four 3rds that get a spot
        threeranks <- res[seq(3, 23, 4),]
        ## Sort the 3rd positions and get the 4 best groups form them
        threeranks <- threeranks[order(threeranks$points, decreasing=TRUE),]
        thirdposcode <- paste0(sort(threeranks$group[1:4]),collapse="")
#        print(thirdposcode)

        thirdpos <- (switch(thirdposcode,
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
                            "BCDE"=c(3, 4, 2, 5),
                            "BCDF"=c(3, 4, 2, 6),
                            "BCEF"=c(5, 3, 2, 6),
                            "BDEF"=c(5, 4, 2, 6),
                            "CDEF"=c(3, 4, 6, 5))-1)*4+3

        komatches <- data.frame(team1=rep(0, 8),
                                team2=rep(0, 8),
                                goals1=rep(NA, 8),
                                goals2=rep(NA, 8))

        komatches[1,c("team1", "team2")] <- res$id[c(2, 10)]
        komatches[2,c("team1", "team2")] <- res$id[c(13, thirdpos[4])]
        komatches[3,c("team1", "team2")] <- res$id[c(5, thirdpos[2])]
        komatches[4,c("team1", "team2")] <- res$id[c(21, 18)]
        komatches[5,c("team1", "team2")] <- res$id[c(9, thirdpos[3])]
        komatches[6,c("team1", "team2")] <- res$id[c(17, 14)]
        komatches[7,c("team1", "team2")] <- res$id[c(1, thirdpos[1])]
        komatches[8,c("team1", "team2")] <- res$id[c(6, 22)]

        sankey$links$value[res$id[2]] <<- sankey$links$value[res$id[2]] +1
        sankey$links$value[res$id[10]] <<- sankey$links$value[res$id[10]] +1
        sankey$links$value[res$id[13]+24] <<- sankey$links$value[res$id[13]+24] +1
        sankey$links$value[res$id[thirdpos[4]]+24] <<- sankey$links$value[res$id[thirdpos[4]]+24] +1
        sankey$links$value[res$id[5]+48] <<- sankey$links$value[res$id[5]+48] +1
        sankey$links$value[res$id[thirdpos[2]]+48] <<- sankey$links$value[res$id[thirdpos[2]]+48] +1
        sankey$links$value[res$id[21]+72] <<- sankey$links$value[res$id[21]+72] +1
        sankey$links$value[res$id[18]+72] <<- sankey$links$value[res$id[18]+72] +1
        sankey$links$value[res$id[9]+96] <<- sankey$links$value[res$id[9]+96] +1
        sankey$links$value[res$id[thirdpos[3]]+96] <<- sankey$links$value[res$id[thirdpos[3]]+96] +1
        sankey$links$value[res$id[17]+120] <<- sankey$links$value[res$id[17]+120] +1
        sankey$links$value[res$id[14]+120] <<- sankey$links$value[res$id[14]+120] +1
        sankey$links$value[res$id[1]+144] <<- sankey$links$value[res$id[1]+144] +1
        sankey$links$value[res$id[thirdpos[1]]+144] <<- sankey$links$value[res$id[thirdpos[1]]+144] +1
        sankey$links$value[res$id[6]+168] <<- sankey$links$value[res$id[6]+168] +1
        sankey$links$value[res$id[22]+168] <<- sankey$links$value[res$id[22]+168] +1




        komatches <- simulateMatches(komatches, d, musthavewinner=TRUE)

#        print(d)
#        print(komatches)

        ## Quarter finals
        qmatches <- data.frame(team1=komatches$winner[c(1,3,5,7)],
                               team2=komatches$winner[c(2,4,6,8)],
                               goals1=rep(NA, 4), goals2=rep(NA,4))

        qmatches <- simulateMatches(qmatches, d, musthavewinner=TRUE)

#        print(qmatches)

        ## Semi finals
        smatches <- data.frame(team1=qmatches$winner[c(1,3)],
                               team2=qmatches$winner[c(2,4)],
                               goals1=rep(NA, 2), goals2=rep(NA, 2))

        smatches <- simulateMatches(smatches, d, musthavewinner=TRUE)

#        print(smatches)

        ## Finals
        fmatches <- data.frame(team1=smatches$winner[c(1)],
                               team2=smatches$winner[c(2)],
                               goals1=rep(NA, 1), goals2=rep(NA, 1))

        fmatches <- simulateMatches(fmatches, d, musthavewinner=TRUE)

        data$wins[fmatches$winner[1]] <- data$wins[fmatches$winner[1]] +1

#        print(komatches)
#        print(fmatches)

    }

#    res
    data
}

result <- simulateTournament(n=1000, groupmatches=groupmatches, data=indata)

save(result, file="soccer.rda")

sankey$links$source <- sankey$links$source-1
sankey$links$target <- sankey$links$target-1

save(sankey, file="sankey.rda")

#  sankeyNetwork(Links = sankey$links, Nodes = sankey$nodes, Source = "source",
#                 +  Target = "target", Value = "value", NodeID = "name",
#                 +  units = "", fontSize = 12, nodeWidth = 30)

library(ggplot2)
plot1 <- ggplot(result, aes(x=reorder(country, -wins), y=wins, fill=group)) +
#    stat_summary(fun.y=mean,geom="bar")+
    geom_bar(stat="identity") + geom_text(aes(label=wins), vjust=1.6, color="white", size=3.5) + xlab("Country") +
      theme(axis.text.x = element_text(angle=60, hjust = 1))

plot1
