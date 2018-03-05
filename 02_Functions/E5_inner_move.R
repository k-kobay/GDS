inner_move <- function(Data, origin_mat, moved_mat, triangle_list, search_nearest=20){ # START DEF inner_move3
  point_matrix <- moved_mat
  res_L1 <- list()
  
  for(iter1 in 1:length(Data)){ # iter1 <-1
    res_L2 <- list()
    L1 <- Data[[iter1]]
    
    for(iter2 in 1:length(L1)){ # iter2 <- 1
      L2 <- L1[[iter2]]
      moved_points <- matrix(NA, nrow=nrow(L2), ncol=2)
      
      for(iter3 in 1:nrow(L2)){ # iter3 <- 1000
        v <- as.numeric(L2[iter3,])
        sort_near <- order(as.matrix(dist(rbind(v,origin_mat)))[-1,1])
        test_res1 <- TRUE
        while_iter1 <- 1
        while(test_res1){ # START WHILE test_res1
          n_ver <- sort_near[while_iter1]
          in_num <- which(apply(triangle_list, 1, function(x){ sum(x %in% n_ver) != 0 }))
          while_iter2 <- 1
          test_res2 <- TRUE
          while(test_res2){ # START WHILE test_res2
            xxx <- triangle_list[in_num[while_iter2] , ]
            vvv <- origin_mat[as.numeric(xxx), ]
            rr <- wn_test(p=v, v=vvv) != 0
            
            #====start origin======#
            # xxx <- triangle_list[in_num[while_iter2] , ]
            # p1 <- as.numeric(origin_mat[xxx[[1]], c(1,2)])
            # q1 <- as.numeric(origin_mat[xxx[[2]], c(1,2)])
            # r1 <- as.numeric(origin_mat[xxx[[3]], c(1,2)])
            # 
            # p2 <- as.numeric(point_matrix[xxx[[1]], c(1,2)])
            # q2 <- as.numeric(point_matrix[xxx[[2]], c(1,2)])
            # r2 <- as.numeric(point_matrix[xxx[[3]], c(1,2)])
            # 
            # yy <- triangle_move(p1=p1, q1=q1, r1=r1, p2=p2, q2=q2, r2=r2, v=v)
            # rr <- (sum(yy$st >= 0) == 2) & (sum(yy$st) <= 1)
            # 
            # test_res2 <- !rr
            #====end origin======#
            if(rr==TRUE){
              # print(rr)
              p1 <- as.numeric(origin_mat[xxx[[1]], c(1,2)])
              q1 <- as.numeric(origin_mat[xxx[[2]], c(1,2)])
              r1 <- as.numeric(origin_mat[xxx[[3]], c(1,2)])
              
              p2 <- as.numeric(point_matrix[xxx[[1]], c(1,2)])
              q2 <- as.numeric(point_matrix[xxx[[2]], c(1,2)])
              r2 <- as.numeric(point_matrix[xxx[[3]], c(1,2)])
              
              yy <- triangle_move(p1=p1, q1=q1, r1=r1, p2=p2, q2=q2, r2=r2, v=v)
              
              moved_points[iter3, ] <- yy$w
              }
            if(while_iter2 == length(in_num)){
              test_res2 <- FALSE
            }
            while_iter2 <- while_iter2 + 1
          } # END WHILE test_res2
          
          if(rr==TRUE){
            test_res1 <- FALSE
          }
          
          # if(while_iter1==round(length(sort_near)/3)){
          if(while_iter1==search_nearest){
            test_res1 <- FALSE
          }
          while_iter1 <- while_iter1 + 1
          
        }  # END WHILE test_res1
        if({iter3 %% 1000}==0){
          print(paste(iter3, nrow(L2), sep="/"))
        }
        
      } # END FOR iter3
      res_L2[[iter2]] <- moved_points
    } # END FOR iter2
    res_L1[[iter1]] <- res_L2
  } # END FOR iter3 
  return(res_L1)
} # END DEF inner_move3
