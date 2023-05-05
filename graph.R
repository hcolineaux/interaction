
################################################################################
### 6- out.int.fig()           pour obtenir une figure présentant les résultats
out.int.fig <- function(int.r = int.r) {
  int.r$A1 <- as.factor(int.r$A1)
  int.r$A2 <- as.factor(int.r$A2)
  
  int.r$RD.A1[which(is.na(int.r$RD.A1))] <- 0
  int.r$RD.A2[which(is.na(int.r$RD.A2))] <- 0
  
  int.r$RR.A1[which(is.na(int.r$RR.A1))] <- 1
  int.r$RR.A2[which(is.na(int.r$RR.A2))] <- 1
  
  # The errorbars overlapped, so use position_dodge to move them horizontally
  pd <- position_dodge(0.04) # move them .05 to the left and right
  
  
  g1 <- ggplot(int.r) +
    aes(x = A1, color = A2, y = p) +
    geom_line(aes(group = A2), linetype = 2, position = pd) +
    geom_errorbar(aes(group = A2, ymin = int.r$p.lo, ymax = int.r$p.up), 
                  width=.1, position = pd) +
    geom_point(position = pd) +
    labs(title = "Effect of A1 in groups of A2")
  
  g2 <- ggplot(int.r) +
    aes(x = A2, color = A1, y = p) +
    geom_line(aes(group = A1), linetype = 2, position = pd) +
    geom_errorbar(aes(group = A1, ymin = int.r$p.lo, ymax = int.r$p.up), 
                  width=.1, position = pd) +
    geom_point(position = pd) +
    labs(title = "Effect of A2 in groups of A1")
  
  g3 <- ggplot(int.r) +
    aes(x = A1, color = A2, y= RD.A1) + 
    geom_line(aes(group = A2, linetype = A2)) +
    geom_errorbar(aes(group = A2, ymin = int.r$RD.A1.lo, ymax = int.r$RD.A1.up), 
                  width=.1, position = pd) +
    geom_point() +
    labs(subtitle = "(RD scale)")
  
  g4 <- ggplot(int.r) +
    aes(x = A2, color = A1, y= RD.A2) + 
    geom_line(aes(group = A1, linetype = A1)) +
    geom_errorbar(aes(group = A1, ymin = int.r$RD.A2.lo, ymax = int.r$RD.A2.up), 
                  width=.1, position = pd) +
    geom_point() +
    labs(subtitle = "(RD scale)")
  
  g5 <- ggplot(int.r) +
    aes(x = A1, color = A2, y = RR.A1) +
    geom_line(aes(group = A2, linetype = A2)) +
    geom_errorbar(aes(group = A2, ymin = int.r$RR.A1.lo, ymax = int.r$RR.A1.up), 
                  width=.1, position = pd) +
    geom_point() +
    labs(subtitle = "(RR scale)")
  
  g6 <- ggplot(int.r) +
    aes(x = A2, color = A1, y = RR.A2) +
    geom_line(aes(group = A1, linetype = A1)) +
    geom_errorbar(aes(group = A1, ymin = int.r$RR.A2.lo, ymax = int.r$RR.A2.up), 
                  width=.1, position = pd) +
    geom_point() +
    labs(subtitle = "(RR scale)")
  
  g1 + g2 + g3 + g4 + g5 + g6 + plot_layout(ncol = 2, nrow = 3)
}

out.int.fig()
