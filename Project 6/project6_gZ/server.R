library(shiny)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  nG <- 0
  nH <- 0
  nR <- 0
  nD <- 0
  
  
  newgame <- function(){
    
    goal <<- 100
    score  <<- 0
    cScore <<- 0
    i <<- 0
    j <<- 0
    k <<- 0
    r <<- 0
    uTurn <<- 1
    msg <<- ""
   
    load(file="VUfile.Rdata")
    U <<- U
    
  }
  newgame()
  
  output$score <- renderUI({
  
    if(input$newGameBut > nG){
      newgame()
      nG <<- input$newGameBut 
      nD <<- input$playDPBut
      nR <<- input$rollBut
      nH <<- input$holdBut
    }
    else {
      if(input$rollBut > nR){
        nR <<- input$rollBut
        if(uTurn){
          r <<- ceiling(runif(1,0,6))
          if(r == 1){
            k <<- 0
            i <<- i + 1 
            uTurn <<- 0
          }
          else{
            k <<- k + r
          }
        }  
      }#if Roll
      if(input$holdBut > nH){
        nH <<- input$holdBut
        if(uTurn){
          i <<- i+k
          k <<- 0
          uTurn <<- 0
        }  
      }#if Hold
      if(k+i>=goal){
        msg <<- "YOU WIN!"
        uTurn <<- 3
      }
      else{
        msg <<- ""
      }
    }
   
    h3(i)
    
    
  })
  
  output$cScore <- renderUI({
    
    if(input$newGameBut > nG){
      newgame()
      nG <<- input$newGameBut 
      nD <<- input$playDPBut
      nR <<- input$rollBut
      nH <<- input$holdBut
    }
    else {
      if(input$playDPBut > nD){
        nD <<- input$playDPBut
        if(uTurn==0){
          cont=1
          ck <<- 0
          msg <<- ""
          while(cont){
            cr <<- ceiling(runif(1,0,6))
            
            
            if(cr>1){
              ck <<- ck + cr
              msg <<- paste(msg,"DP rolls ", cr ,". Turn Total ", ck,". ")
              if(U[j+1,i+1,ck+1]==2){
                msg <<- paste(msg, " Holds. End of Turn.")
                j <<- j+ck
                cont=0
              }else if(U[j+1,i+1,ck+1]==1){
                msg <<- paste(msg, " Rolls again. <br/>")
              }
              if(ck+j>=goal){
                msg <<- paste(msg,"DP WINs!")
                uTurn <<- 2
                cont =0
              }
              
              
            }
            else if(cr == 1){
              ck <<- 1
              j <<- j+1
              msg <<- paste(msg,"DP rolls ", cr ,". Turn Total ", ck,". End of Turn.")
              cont=0
              if(ck+j>=goal){
                msg <<- paste(msg,"<h3/>DP WINs! <//h3/>")
                uTurn <<- 2
                cont =0
              }
              
            }
  
            
            
          }#while
          
          if(uTurn==0){
            uTurn <<- 1
          }
        }#if turn
      }
    }  
    h3(j)    
    
    
  })
  
  output$dpPlayMsg <- renderUI({
    input$newGameBut
    input$playDPBut
    input$rollBut
    input$holdBut
    HTML(msg)
    
  })
  
  
  output$r <- renderUI({
    input$newGameBut
    input$rollBut
    input$holdBut
    h4(r)
    
  })
  output$k <- renderUI({
    input$newGameBut
    input$rollBut
    input$holdBut
    h4(k)
  })
  
  
  
  output$uTurn <- renderUI({
    input$newGameBut
    input$rollBut
    input$holdBut
    input$playDPBut
    if(uTurn==1){
      h3("Your Turn")
    }else if(uTurn==0){
      h3("---")
    }else if(uTurn==2){
      h3("DP WINS!")
    }else if(uTurn==3){
      h3("YOU WIN!")
    }
    
    
  })
  output$dTurn <- renderUI({
    input$newGameBut
    input$rollBut
    input$holdBut  
    input$playDPBut
    if(uTurn==0){
      h3("DP's Turn")
    }else if(uTurn==1){
      h3("---")
    }else if(uTurn==2){
      h3("DP WINS!")
    }else if(uTurn==3){
      h3("YOU WIN!")
    }
    
  })
  
  
  
})