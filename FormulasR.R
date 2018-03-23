healthyness <- function(df){
  Healthy <- logical(nrow(df))
  HDL.LEVEL <- character(nrow(df))
  ColesterolFaixa <- character(nrow(df))
  Displazia <- logical(nrow(df))
  for(i in 1:nrow(df)){
    Healthy[i] <- TRUE;
    if (df$HDL[i] != "NA" && df$HDL[i] < 40){
      HDL.LEVEL[i] <- "Baixo"
      Healthy[i] <- FALSE;
    } 
    else if (df$HDL[i] < 60){
      HDL.LEVEL[i] <- "Medio"
    }
    else if (df$HDL[i] >= 60){
      HDL.LEVEL[i] <- "Desejável"
    }
    if (df$Colesterol[i] < 200){
      ColesterolFaixa[i] <- "Desejável"
    } 
    else if (df$Colesterol[i] <= 239){
      ColesterolFaixa[i] <- "Limítrofe"
    }
    else if (df$Colesterol[i] > 239){
      ColesterolFaixa[i] <- "Alto"
      Healthy[i] <- FALSE;
    }
    if (((df$Sexo[i] == "female" && df$HDL[i] < 50)|| (df$Sexo[i] == "male" && df$HDL[i] < 40))  && df$Pressao.Sistolica[i] >= 130 && df$Pressao.Diast[i] >= 85 && df$Glicose >= 100){
      Displazia[i] <- TRUE
      Healthy[i] <- FALSE;
    }
    else{
      Displazia[i] <- FALSE
    }
  }

}

  