#==============================================================================
# 005. floydの定義
# floyd : グラフの最短経路の計算

# 関数定義

floyd <- function( n, a ) {
#  Finds the shortest distances between pairs of nodes in a directed graph.
#
#  Description:
#    We assume we are given the adjacency matrix a of the directed graph.
#    The adjacency matrix is NOT assumed to be symmetric.
#    If there is not a direct link from node i to node j, the distance
#    would formally be infinity.
#
#  Parameters:
#    Input, integer n, the order of the matrix.
#    Input, real A(i,j), the direct distance from node i to node j.
#    Output, real A(i,j), the shortest distance from node i to node j.
  for (k in 1 : n) {
    for (j in 1 : n) {
      for (i in 1 : n) {
        a[i,j] <- min( a[i,j], a[i,k] + a[k,j] )
      }
    }
  }
  return( a )
}
#===========end================================================================