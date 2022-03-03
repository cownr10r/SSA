#' ssa
#' @param a path to docx template file
#' @param b the first number of the row indicating the first minute of observation
#' @param c the last number of the row indicating the last minute of observation
#' @param d a number indicating the minimum number for transitions between sequences
#' @import docxtractr
#' @import TraMineR
#' @import dplyr
#' @import gridExtra
#' @export







ssa <- function(a,b,c,d){
  library(docxtractr)

  readtr <- function(a){
    a <- data.frame(a)
    names <-  a[,1]
    aT <- as.data.frame(as.matrix(t(a[,-1])))
    colnames(aT) <- names
    return(aT)
  }


  doc3 <- docxtractr::read_docx(a)


  p1 <- docxtractr::docx_extract_tbl(doc3, tbl_number = 1, header = TRUE,preserve = FALSE,trim = TRUE)


  p2 <- p1[,c(1,2)]

  data <- readtr(p2)


  st.alphabet <- c("I", "DIS",  "GrP", "BM","TR","GP","CL", "AL", "FA", "D", "IP","O", "SG", "H", "WU", "DY", "SA", "AN", "IN", "SD", "GA")

  st.labels <-c("Introduction","Discussion", "GroupWork", "Behavior", "Transition","Guided Practice", "Closure", "Algorithm",
                "Formative Assessment", "Demonstration", "Independent Practice", "Objectives", "Small Group", "Hook", "Warm UP",
                "Dyad", "Summative Assessment",  "Announcements", "Instructions", "Student Demonstration", "Game")

  st.scodes <- c("I", "DIS",  "GrP", "BM", "TR", "GP", "CL", "AL", "FA", "D", "IP", "O","SG", "H", "WU", "DY", "SA","AN", "IN", "SD", "GA")


  mvad.seq <- TraMineR::seqdef(data, b:c, alphabet = st.alphabet, states = st.scodes, labels = st.labels)
 seq<-  TraMineR::seqIplot(mvad.seq, sortv = "from.start", with.legend = T) # (all sequences)
 means <- TraMineR::seqmtplot(mvad.seq, with.legend = T)
 mvad.seqe <- TraMineR::seqecreate(mvad.seq, use.labels = F)

 fsubseq <- TraMineR::seqefsub(mvad.seqe, min.support = d)
 freq <- plot(fsubseq[1:10], col = "white")


 p1 <- p1[,c(2,3)]
 p1 <- p1[-1,] #this could change if you have more than one demographic variable

 p1$Quality <- as.numeric(p1$Quality)

 mean <- p1 %>%
   dplyr::group_by(Codes) %>%
   dplyr::summarize(mean(Quality))

 mean <- data.frame(mean)

 library(gridExtra)
 grid::grid.newpage()
 gridExtra::grid.table(mean)



 sum <- p1 %>%
   dplyr::group_by(Codes) %>%
   dplyr::summarize(sum(Quality))

 sum <- data.frame(sum)

 library(gridExtra)
 grid::grid.newpage()
 gridExtra::grid.table(sum)

 return(list(seq,means, freq, mean,sum))
}




