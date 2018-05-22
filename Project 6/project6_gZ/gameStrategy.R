gameStrategy <- function(p_goal){
  
  goal = p_goal+1
  all_comb = expand.grid(seq(goal-2),seq(goal-1))     # Generating all combinations of each index
  all_comb$sum = all_comb[[1]] + all_comb[[2]]    # Finding max sum
  all_comb = all_comb[order(all_comb$sum, decreasing = TRUE),]
    
  V = array(NA, dim=c(goal+5,goal+5,goal+5))  # s,p,x
  U = array(NA, dim=c(goal+5,goal+5,goal+5))  # s,p,x
  
  # Initialize boundaries
  V[goal:(goal+5),,] = 1                 # If you have 100+ points then you win irregardless of the opponent's score
  V[goal-1,seq(1,goal-1),] = 1           # If it is your turn and you have goal-1 points, no matter what you will win
  V[seq(1,goal-1),goal:(goal+5),] = 0    # If the opponent reaches the goal first then you lose.
  V[,seq(1,goal-1),goal:(goal+5)] = 1    # If you accumulate the goal amount in one round you will win
  
  # Fill in remaining V and U matrix
  for(r in 1:length(all_comb$sum)){
      for(x in (goal):1){
        s = all_comb[r,1]
        p = all_comb[r,2]
        V[s,p,x] = max( ( (1/6)*(1-V[min(p,goal),s+1,1]) + (1/6)*V[min(s,goal),p,min(x+2,goal)] + (1/6)*V[min(s,goal),p,min(x+3,goal)]
                          + (1/6)*V[min(s,goal),p,min(x+4,goal)] + (1/6)*V[min(s,goal),p,min(x+5,goal)]
                          + (1/6)*V[min(s,goal),p,min(x+6,goal)] ), 1-V[min(p,goal),min(s+max(x-1,1),goal),1] ) #Roll/Hold
        U[s,p,x] = which.max( c( ( (1/6)*(1-V[min(p,goal),s+1,1]) + (1/6)*V[min(s,goal),p,min(x+2,goal)] + (1/6)*V[min(s,goal),p,min(x+3,goal)]
                          + (1/6)*V[min(s,goal),p,min(x+4,goal)] + (1/6)*V[min(s,goal),p,min(x+5,goal)]
                          + (1/6)*V[min(s,goal),p,min(x+6,goal)] ), 1-V[min(p,goal),min(s+max(1,x-1),goal),1] ) ) #Roll/Hold
      }
  }
  save(list = c('V','U'),file = 'VUfile.Rdata')
  
}



