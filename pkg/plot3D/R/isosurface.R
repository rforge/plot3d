## =============================================================================
## =============================================================================
## Creating isosurface triangulations, using the marching algorithm
## =============================================================================
## =============================================================================

# The marchin Algorithm as translated from:
# http://paulbourke.net/geometry/polygonise/



#        v5 ----- e5  ----- v6
#      / |                 / |
#     e8 |               e6  |
#   /    |              /    |
#  v8 ----- e7 ----- v7      |
#  |     |            |      |
#  |     e9           |      e10
#  |     |            |      |
#  |     |            |      |
#  e12   |           e11     |
#  |     v1 ----- e1  |----- v2
#  |   /              |    /
#  |  e4              |   e2
#  |/                 | /
#  v4 ----- e3 ----- v3

#   Given a grid cell and an isolevel, calculate the triangular
#   facets required to represent the isosurface through the cell.
#   at most 5 triangular facets.

# this not used, to see which edge is defined by a certain integer
as.binary <- function(n, base = 2 , r = FALSE) {
   out <- NULL
   while(n > 0) {
     if (r) {
       out <- c(out , n%%base)
     } else {
       out <- c(n%%base , out)
     }
     n <- n %/% base
   }
   lo <- length(out)
   
   return(c(out, rep(0, 12-lo)))
}

## =============================================================================
## the lookup tables 
## =============================================================================

edges <- as.hexmode(c(
 0x0  , 0x109, 0x203, 0x30a, 0x406, 0x50f, 0x605, 0x70c,
 0x80c, 0x905, 0xa0f, 0xb06, 0xc0a, 0xd03, 0xe09, 0xf00,
 0x190, 0x99 , 0x393, 0x29a, 0x596, 0x49f, 0x795, 0x69c,
 0x99c, 0x895, 0xb9f, 0xa96, 0xd9a, 0xc93, 0xf99, 0xe90,
 0x230, 0x339, 0x33 , 0x13a, 0x636, 0x73f, 0x435, 0x53c,
 0xa3c, 0xb35, 0x83f, 0x936, 0xe3a, 0xf33, 0xc39, 0xd30,
 0x3a0, 0x2a9, 0x1a3, 0xaa , 0x7a6, 0x6af, 0x5a5, 0x4ac,
 0xbac, 0xaa5, 0x9af, 0x8a6, 0xfaa, 0xea3, 0xda9, 0xca0,
 0x460, 0x569, 0x663, 0x76a, 0x66 , 0x16f, 0x265, 0x36c,
 0xc6c, 0xd65, 0xe6f, 0xf66, 0x86a, 0x963, 0xa69, 0xb60,
 0x5f0, 0x4f9, 0x7f3, 0x6fa, 0x1f6, 0xff , 0x3f5, 0x2fc,
 0xdfc, 0xcf5, 0xfff, 0xef6, 0x9fa, 0x8f3, 0xbf9, 0xaf0,
 0x650, 0x759, 0x453, 0x55a, 0x256, 0x35f, 0x55 , 0x15c,
 0xe5c, 0xf55, 0xc5f, 0xd56, 0xa5a, 0xb53, 0x859, 0x950,
 0x7c0, 0x6c9, 0x5c3, 0x4ca, 0x3c6, 0x2cf, 0x1c5, 0xcc ,
 0xfcc, 0xec5, 0xdcf, 0xcc6, 0xbca, 0xac3, 0x9c9, 0x8c0,
 0x8c0, 0x9c9, 0xac3, 0xbca, 0xcc6, 0xdcf, 0xec5, 0xfcc,
 0xcc , 0x1c5, 0x2cf, 0x3c6, 0x4ca, 0x5c3, 0x6c9, 0x7c0,
 0x950, 0x859, 0xb53, 0xa5a, 0xd56, 0xc5f, 0xf55, 0xe5c,
 0x15c, 0x55 , 0x35f, 0x256, 0x55a, 0x453, 0x759, 0x650,
 0xaf0, 0xbf9, 0x8f3, 0x9fa, 0xef6, 0xfff, 0xcf5, 0xdfc,
 0x2fc, 0x3f5, 0xff , 0x1f6, 0x6fa, 0x7f3, 0x4f9, 0x5f0,
 0xb60, 0xa69, 0x963, 0x86a, 0xf66, 0xe6f, 0xd65, 0xc6c,
 0x36c, 0x265, 0x16f, 0x66 , 0x76a, 0x663, 0x569, 0x460,
 0xca0, 0xda9, 0xea3, 0xfaa, 0x8a6, 0x9af, 0xaa5, 0xbac,
 0x4ac, 0x5a5, 0x6af, 0x7a6, 0xaa , 0x1a3, 0x2a9, 0x3a0,
 0xd30, 0xc39, 0xf33, 0xe3a, 0x936, 0x83f, 0xb35, 0xa3c,
 0x53c, 0x435, 0x73f, 0x636, 0x13a, 0x33 , 0x339, 0x230,
 0xe90, 0xf99, 0xc93, 0xd9a, 0xa96, 0xb9f, 0x895, 0x99c,
 0x69c, 0x795, 0x49f, 0x596, 0x29a, 0x393, 0x99 , 0x190,
 0xf00, 0xe09, 0xd03, 0xc0a, 0xb06, 0xa0f, 0x905, 0x80c,
 0x70c, 0x605, 0x50f, 0x406, 0x30a, 0x203, 0x109, 0x0))
 
 


triTable <- matrix(nrow = 256, ncol = 16, byrow = TRUE, data = c(
 NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  3,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  1,  9,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  8,  3,   9,  8,  1,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  10,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  3,   1,  2,  10,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  2,  10,  0,  2,  9,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 2,  8,  3,   2,  10, 8,   10, 9,  8,   NA, NA, NA,  NA, NA, NA,  NA,
 3,  11, 2,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  11, 2,   8,  11, 0,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  9,  0,   2,  3,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  11, 2,   1,  9,  11,  9,  8,  11,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  10, 1,   11, 10, 3,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  10, 1,   0,  8,  10,  8,  11, 10,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  9,  0,   3,  11, 9,   11, 10, 9,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  8,  10,  10, 8,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  7,  8,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  3,  0,   7,  3,  4,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  1,  9,   8,  4,  7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  1,  9,   4,  7,  1,   7,  3,  1,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  10,  8,  4,  7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  4,  7,   3,  0,  4,   1,  2,  10,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  2,  10,  9,  0,  2,   8,  4,  7,   NA, NA, NA,  NA, NA, NA,  NA,
 2,  10, 9,   2,  9,  7,   2,  7,  3,   7,  9,  4,   NA, NA, NA,  NA,
 8,  4,  7,   3,  11, 2,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 11, 4,  7,   11, 2,  4,   2,  0,  4,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  0,  1,   8,  4,  7,   2,  3,  11,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  7,  11,  9,  4,  11,  9,  11, 2,   9,  2,  1,   NA, NA, NA,  NA,
 3,  10, 1,   3,  11, 10,  7,  8,  4,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  11, 10,  1,  4,  11,  1,  0,  4,   7,  11, 4,   NA, NA, NA,  NA,
 4,  7,  8,   9,  0,  11,  9,  11, 10,  11, 0,  3,   NA, NA, NA,  NA,
 4,  7,  11,  4,  11, 9,   9,  11, 10,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  5,  4,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  5,  4,   0,  8,  3,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  5,  4,   1,  5,  0,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 8,  5,  4,   8,  3,  5,   3,  1,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  10,  9,  5,  4,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  0,  8,   1,  2,  10,  4,  9,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  2,  10,  5,  4,  2,   4,  0,  2,   NA, NA, NA,  NA, NA, NA,  NA,
 2,  10, 5,   3,  2,  5,   3,  5,  4,   3,  4,  8,   NA, NA, NA,  NA,
 9,  5,  4,   2,  3,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  11, 2,   0,  8,  11,  4,  9,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  5,  4,   0,  1,  5,   2,  3,  11,  NA, NA, NA,  NA, NA, NA,  NA,
 2,  1,  5,   2,  5,  8,   2,  8,  11,  4,  8,  5,   NA, NA, NA,  NA,
 10, 3,  11,  10, 1,  3,   9,  5,  4,   NA, NA, NA,  NA, NA, NA,  NA,
 4,  9,  5,   0,  8,  1,   8,  10, 1,   8,  11, 10,  NA, NA, NA,  NA,
 5,  4,  0,   5,  0,  11,  5,  11, 10,  11, 0,  3,   NA, NA, NA,  NA,
 5,  4,  8,   5,  8,  10,  10, 8,  11,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  7,  8,   5,  7,  9,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  3,  0,   9,  5,  3,   5,  7,  3,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  7,  8,   0,  1,  7,   1,  5,  7,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  5,  3,   3,  5,  7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  7,  8,   9,  5,  7,   10, 1,  2,   NA, NA, NA,  NA, NA, NA,  NA,
 10, 1,  2,   9,  5,  0,   5,  3,  0,   5,  7,  3,   NA, NA, NA,  NA,
 8,  0,  2,   8,  2,  5,   8,  5,  7,   10, 5,  2,   NA, NA, NA,  NA,
 2,  10, 5,   2,  5,  3,   3,  5,  7,   NA, NA, NA,  NA, NA, NA,  NA,
 7,  9,  5,   7,  8,  9,   3,  11, 2,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  5,  7,   9,  7,  2,   9,  2,  0,   2,  7,  11,  NA, NA, NA,  NA,
 2,  3,  11,  0,  1,  8,   1,  7,  8,   1,  5,  7,   NA, NA, NA,  NA,
 11, 2,  1,   11, 1,  7,   7,  1,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  5,  8,   8, 5,  7,   10,  1,  3,   10, 3,  11,  NA, NA, NA,  NA,
 5,  7,  0,   5,  0,  9,   7,  11, 0,   1,  0,  10,  11, 10, 0,   NA,
 11, 10, 0,   11, 0,  3,   10, 5,  0,   8,  0,  7,   5,  7,  0,   NA,
 11, 10, 5,   7,  11, 5,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 10, 6,  5,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  3,   5,  10, 6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 9,  0,  1,   5,  10, 6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  8,  3,   1,  9,  8,   5,  10, 6,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  6,  5,   2,  6,  1,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  6,  5,   1,  2,  6,   3,  0,  8,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  6,  5,   9,  0,  6,   0,  2,  6,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  9,  8,   5,  8,  2,   5,  2,  6,   3,  2,  8,   NA, NA, NA,  NA,
 2,  3,  11,  10, 6,  5,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 11, 0,  8,   11, 2,  0,   10, 6,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  1,  9,   2,  3,  11,  5,  10, 6,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  10, 6,   1,  9,  2,   9,  11, 2,   9,  8,  11,  NA, NA, NA,  NA,
 6,  3,  11,  6,  5,  3,   5,  1,  3,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  11,  0,  11, 5,   0,  5,  1,   5,  11, 6,   NA, NA, NA,  NA,
 3,  11, 6,   0,  3,  6,   0,  6,  5,   0,  5,  9,   NA, NA, NA,  NA,
 6,  5,  9,   6,  9,  11,  11, 9,  8,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  10, 6,   4,  7,  8,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  3,  0,   4,  7,  3,   6,  5,  10,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  9,  0,   5,  10, 6,   8,  4,  7,   NA, NA, NA,  NA, NA, NA,  NA,
 10, 6,  5,   1,  9,  7,   1,  7,  3,   7,  9,  4,   NA, NA, NA,  NA,
 6,  1,  2,   6,  5,  1,   4,  7,  8,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  5,   5,  2,  6,   3,  0,  4,   3,  4,  7,   NA, NA, NA,  NA,
 8,  4,  7,   9,  0,  5,   0,  6,  5,   0,  2,  6,   NA, NA, NA,  NA,
 7,  3,  9,   7,  9,  4,   3,  2,  9,   5,  9,  6,   2,  6,  9,   NA,
 3,  11, 2,   7,  8,  4,   10, 6,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  10, 6,   4,  7,  2,   4,  2,  0,   2,  7,  11,  NA, NA, NA,  NA,
 0,  1,  9,   4,  7,  8,   2,  3,  11,  5,  10, 6,   NA, NA, NA,  NA,
 9,  2,  1,   9,  11, 2,   9,  4,  11,  7,  11, 4,   5,  10, 6,   NA,
 8,  4,  7,   3,  11, 5,   3,  5,  1,   5,  11, 6,   NA, NA, NA,  NA,
 5,  1,  11,  5,  11, 6,   1,  0,  11,  7,  11, 4,   0,  4,  11,  NA,
 0,  5,  9,   0,  6,  5,   0,  3,  6,   11, 6,  3,   8,  4,  7,   NA,
 6,  5,  9,   6,  9,  11,  4,  7,  9,   7,  11, 9,   NA, NA, NA,  NA,
 10, 4,  9,   6,  4,  10,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  10, 6,   4,  9,  10,  0,  8,  3,   NA, NA, NA,  NA, NA, NA,  NA,
 10, 0,  1,   10, 6,  0,   6,  4,  0,   NA, NA, NA,  NA, NA, NA,  NA,
 8,  3,  1,   8,  1,  6,   8,  6,  4,   6,  1,  10,  NA, NA, NA,  NA,
 1,  4,  9,   1,  2,  4,   2,  6,  4,   NA, NA, NA,  NA, NA, NA,  NA,
 3,  0,  8,   1,  2,  9,   2,  4,  9,   2,  6,  4,   NA, NA, NA,  NA,
 0,  2,  4,   4,  2,  6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 8,  3,  2,   8,  2,  4,   4,  2,  6,   NA, NA, NA,  NA, NA, NA,  NA,
 10, 4,  9,   10, 6,  4,   11, 2,  3,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  2,   2,  8,  11,  4,  9,  10,  4,  10, 6,   NA, NA, NA,  NA,
 3,  11, 2,   0,  1,  6,   0,  6,  4,   6,  1,  10,  NA, NA, NA,  NA,
 6,  4,  1,   6,  1,  10,  4,  8,  1,   2,  1,  11,  8,  11, 1,   NA,
 9,  6,  4,   9,  3,  6,   9,  1,  3,   11, 6,  3,   NA, NA, NA,  NA,
 8,  11, 1,   8,  1,  0,   11, 6,  1,   9,  1,  4,   6,  4,  1,   NA,
 3,  11, 6,   3,  6,  0,   0,  6,  4,   NA, NA, NA,  NA, NA, NA,  NA,
 6,  4,  8,   11, 6,  8,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 7,  10, 6,   7,  8,  10,  8,  9,  10,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  7,  3,   0,  10, 7,   0,  9,  10,  6,  7,  10,  NA, NA, NA,  NA,
 10, 6,  7,   1,  10, 7,   1,  7,  8,   1,  8,  0,   NA, NA, NA,  NA,
 10, 6,  7,   10, 7,  1,   1,  7,  3,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  6,   1,  6,  8,   1,  8,  9,   8,  6,  7,   NA, NA, NA,  NA,
 2,  6,  9,   2,  9,  1,   6,  7,  9,   0,  9,  3,   7,  3,  9,   NA,
 7,  8,  0,   7,  0,  6,   6,  0,  2,   NA, NA, NA,  NA, NA, NA,  NA,
 7,  3,  2,   6,  7,  2,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 2,  3,  11,  10, 6,  8,   10, 8,  9,   8,  6,  7,   NA, NA, NA,  NA,
 2,  0,  7,   2,  7,  11,  0,  9,  7,   6,  7,  10,  9,  10, 7,   NA,
 1,  8,  0,   1,  7,  8,   1,  10, 7,   6,  7,  10,  2,  3,  11,  NA,
 11, 2,  1,   11, 1,  7,   10, 6,  1,   6,  7,  1,   NA, NA, NA,  NA,
 8,  9,  6,   8,  6,  7,   9,  1,  6,   11, 6,  3,   1,  3,  6,   NA,
 0,  9,  1,   11, 6,  7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 7,  8,  0,   7,  0,  6,   3,  11, 0,   11, 6,  0,   NA, NA, NA,  NA,
 7,  11, 6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 7,  6,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  0,  8,   11, 7,  6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  1,  9,   11, 7,  6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 8,  1,  9,   8,  3,  1,   11, 7,  6,   NA, NA, NA,  NA, NA, NA,  NA,
 10, 1,  2,   6,  11, 7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  10,  3,  0,  8,   6,  11, 7,   NA, NA, NA,  NA, NA, NA,  NA,
 2,  9,  0,   2,  10, 9,   6,  11, 7,   NA, NA, NA,  NA, NA, NA,  NA,
 6,  11, 7,   2,  10, 3,   10, 8,  3,   10, 9,  8,   NA, NA, NA,  NA,
 7,  2,  3,   6,  2,  7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 7,  0,  8,   7,  6,  0,   6,  2,  0,   NA, NA, NA,  NA, NA, NA,  NA,
 2,  7,  6,   2,  3,  7,   0,  1,  9,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  6,  2,   1,  8,  6,   1,  9,  8,   8,  7,  6,   NA, NA, NA,  NA,
 10, 7,  6,   10, 1,  7,   1,  3,  7,   NA, NA, NA,  NA, NA, NA,  NA,
 10, 7,  6,   1,  7,  10,  1,  8,  7,   1,  0,  8,   NA, NA, NA,  NA,
 0,  3,  7,   0,  7,  10,  0,  10, 9,   6,  10, 7,   NA, NA, NA,  NA,
 7,  6,  10,  7,  10, 8,   8,  10, 9,   NA, NA, NA,  NA, NA, NA,  NA,
 6,  8,  4,   11, 8,  6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  6,  11,  3,  0,  6,   0,  4,  6,   NA, NA, NA,  NA, NA, NA,  NA,
 8,  6,  11,  8,  4,  6,   9,  0,  1,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  4,  6,   9,  6,  3,   9,  3,  1,   11, 3,  6,   NA, NA, NA,  NA,
 6,  8,  4,   6,  11, 8,   2,  10, 1,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  10,  3,  0,  11,  0,  6,  11,  0,  4,  6,   NA, NA, NA,  NA,
 4,  11, 8,   4,  6,  11,  0,  2,  9,   2,  10, 9,   NA, NA, NA,  NA,
 10, 9,  3,   10, 3,  2,   9,  4,  3,   11, 3,  6,   4,  6,  3,   NA,
 8,  2,  3,   8,  4,  2,   4,  6,  2,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  4,  2,   4,  6,  2,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  9,  0,   2,  3,  4,   2,  4,  6,   4,  3,  8,   NA, NA, NA,  NA,
 1,  9,  4,   1,  4,  2,   2,  4,  6,   NA, NA, NA,  NA, NA, NA,  NA,
 8,  1,  3,   8,  6,  1,   8,  4,  6,   6,  10, 1,   NA, NA, NA,  NA,
 10, 1,  0,   10, 0,  6,   6,  0,  4,   NA, NA, NA,  NA, NA, NA,  NA,
 4,  6,  3,   4,  3,  8,   6,  10, 3,   0,  3,  9,   10, 9,  3,   NA,
 10, 9,  4,   6,  10, 4,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  9,  5,   7,  6,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  3,   4,  9,  5,   11, 7,  6,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  0,  1,   5,  4,  0,   7,  6,  11,  NA, NA, NA,  NA, NA, NA,  NA,
 11, 7,  6,   8,  3,  4,   3,  5,  4,   3,  1,  5,   NA, NA, NA,  NA,
 9,  5,  4,   10, 1,  2,   7,  6,  11,  NA, NA, NA,  NA, NA, NA,  NA,
 6,  11, 7,   1,  2,  10,  0,  8,  3,   4,  9,  5,   NA, NA, NA,  NA,
 7,  6,  11,  5,  4,  10,  4,  2,  10,  4,  0,  2,   NA, NA, NA,  NA,
 3,  4,  8,   3,  5,  4,   3,  2,  5,   10, 5,  2,   11, 7,  6,   NA,
 7,  2,  3,   7,  6,  2,   5,  4,  9,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  5,  4,   0,  8,  6,   0,  6,  2,   6,  8,  7,   NA, NA, NA,  NA,
 3,  6,  2,   3,  7,  6,   1,  5,  0,   5,  4,  0,   NA, NA, NA,  NA,
 6,  2,  8,   6,  8,  7,   2,  1,  8,   4,  8,  5,   1,  5,  8,   NA,
 9,  5,  4,   10, 1,  6,   1,  7,  6,   1,  3,  7,   NA, NA, NA,  NA,
 1,  6,  10,  1,  7,  6,   1,  0,  7,   8,  7,  0,   9,  5,  4,   NA,
 4,  0,  10,  4,  10, 5,   0,  3,  10,  6,  10, 7,   3,  7,  10,  NA,
 7,  6,  10,  7,  10, 8,   5,  4,  10,  4,  8,  10,  NA, NA, NA,  NA,
 6,  9,  5,   6,  11, 9,   11, 8,  9,   NA, NA, NA,  NA, NA, NA,  NA,
 3,  6,  11,  0,  6,  3,   0,  5,  6,   0,  9,  5,   NA, NA, NA,  NA,
 0,  11, 8,   0,  5,  11,  0,  1,  5,   5,  6,  11,  NA, NA, NA,  NA,
 6,  11, 3,   6,  3,  5,   5,  3,  1,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  10,  9,  5,  11,  9,  11, 8,   11, 5,  6,   NA, NA, NA,  NA,
 0,  11, 3,   0,  6,  11,  0,  9,  6,   5,  6,  9,   1,  2,  10,  NA,
 11, 8,  5,   11, 5,  6,   8,  0,  5,   10, 5,  2,   0,  2,  5,   NA,
 6,  11, 3,   6,  3,  5,   2,  10, 3,   10, 5,  3,   NA, NA, NA,  NA,
 5,  8,  9,   5,  2,  8,   5,  6,  2,   3,  8,  2,   NA, NA, NA,  NA,
 9,  5,  6,   9,  6,  0,   0,  6,  2,   NA, NA, NA,  NA, NA, NA,  NA,
 1,  5,  8,   1,  8,  0,   5,  6,  8,   3,  8,  2,   6,  2,  8,   NA,
 1,  5,  6,   2,  1,  6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  3,  6,   1,  6,  10,  3,  8,  6,   5,  6,  9,   8,  9,  6,   NA,
 10, 1,  0,   10, 0,  6,   9,  5,  0,   5,  6,  0,   NA, NA, NA,  NA,
 0,  3,  8,   5,  6,  10,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 10, 5,  6,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 11, 5,  10,  7,  5,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 11, 5,  10,  11, 7,  5,   8,  3,  0,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  11, 7,   5,  10, 11,  1,  9,  0,   NA, NA, NA,  NA, NA, NA,  NA,
 10, 7,  5,   10, 11, 7,   9,  8,  1,   8,  3,  1,   NA, NA, NA,  NA,
 11, 1,  2,   11, 7,  1,   7,  5,  1,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  3,   1,  2,  7,   1,  7,  5,   7,  2,  11,  NA, NA, NA,  NA,
 9,  7,  5,   9,  2,  7,   9,  0,  2,   2, 11,  7,   NA, NA, NA,  NA,
 7,  5,  2,   7,  2,  11,  5,  9,  2,   3,  2,  8,   9,  8,  2,   NA,
 2,  5,  10,  2,  3,  5,   3,  7,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 8,  2,  0,   8,  5,  2,   8,  7,  5,   10, 2,  5,   NA, NA, NA,  NA,
 9,  0,  1,   5,  10, 3,   5,  3,  7,   3,  10, 2,   NA, NA, NA,  NA,
 9,  8,  2,   9,  2,  1,   8,  7,  2,   10, 2,  5,   7,  5,  2,   NA,
 1,  3,  5,   3,  7,  5,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  7,   0,  7,  1,   1,  7,  5,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  0,  3,   9,  3,  5,   5,  3,  7,   NA, NA, NA,  NA, NA, NA,  NA,
 9,  8,  7,   5,  9,  7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 5,  8,  4,   5,  10, 8,   10, 11, 8,   NA, NA, NA,  NA, NA, NA,  NA,
 5,  0,  4,   5,  11, 0,   5,  10, 11,  11, 3,  0,   NA, NA, NA,  NA,
 0,  1,  9,   8,  4,  10,  8,  10, 11,  10, 4,  5,   NA, NA, NA,  NA,
 10, 11, 4,   10, 4,  5,   11, 3,  4,   9,  4,  1,   3,  1,  4,   NA,
 2,  5,  1,   2,  8,  5,   2,  11, 8,   4,  5,  8,   NA, NA, NA,  NA,
 0,  4,  11,  0,  11, 3,   4,  5,  11,  2,  11, 1,   5,  1,  11,  NA,
 0,  2,  5,   0,  5,  9,   2,  11, 5,   4,  5,  8,   11, 8,  5,   NA,
 9,  4,  5,   2,  11, 3,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 2,  5,  10,  3,  5,  2,   3,  4,  5,   3,  8,  4,   NA, NA, NA,  NA,
 5,  10, 2,   5,  2,  4,   4,  2,  0,   NA, NA, NA,  NA, NA, NA,  NA,
 3,  10, 2,   3,  5,  10,  3,  8,  5,   4,  5,  8,   0,  1,  9,   NA,
 5,  10, 2,   5,  2,  4,   1,  9,  2,   9,  4,  2,   NA, NA, NA,  NA,
 8,  4,  5,   8,  5,  3,   3,  5,  1,   NA, NA, NA,  NA, NA, NA,  NA,
 0,  4,  5,   1,  0,  5,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 8,  4,  5,   8,  5,  3,   9,  0,  5,   0,  3,  5,   NA, NA, NA,  NA,
 9,  4,  5,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  11, 7,   4,  9,  11,  9,  10, 11,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  8,  3,   4,  9,  7,   9,  11, 7,   9,  10, 11,  NA, NA, NA,  NA,
 1,  10, 11,  1,  11, 4,   1,  4,  0,   7,  4,  11,  NA, NA, NA,  NA,
 3,  1,  4,   3,  4,  8,   1,  10, 4,   7,  4,  11,  10, 11, 4,   NA,
 4,  11, 7,   9,  11, 4,   9,  2,  11,  9,  1,  2,   NA, NA, NA,  NA,
 9,  7,  4,   9,  11, 7,   9,  1,  11,  2,  11, 1,   0,  8,  3,   NA,
 11, 7,  4,   11, 4,  2,   2,  4,  0,   NA, NA, NA,  NA, NA, NA,  NA,
 11, 7,  4,   11, 4,  2,   8,  3,  4,   3,  2,  4,   NA, NA, NA,  NA,
 2,  9,  10,  2,  7,  9,   2,  3,  7,   7,  4,  9,   NA, NA, NA,  NA,
 9, 10,  7,   9,  7,  4,   10, 2,  7,   8,  7,  0,   2,  0,  7,   NA,
 3,  7,  10,  3,  10, 2,   7,  4,  10,  1,  10, 0,   4,  0,  10,  NA,
 1, 10,  2,   8,  7,  4,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  9,  1,   4,  1,  7,   7,  1,  3,   NA, NA, NA,  NA, NA, NA,  NA,
 4,  9,  1,   4,  1,  7,   0,  8,  1,   8,  7,  1,   NA, NA, NA,  NA,
 4,  0,  3,   7,  4,  3,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 4,  8,  7,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 9, 10,  8,   10, 11, 8,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  0,  9,   3,  9,  11,  11, 9,  10,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  1,  10,  0,  10, 8,   8,  10, 11,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  1,  10,  11, 3,  10,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  2,  11,  1,  11, 9,   9, 11,  8,   NA, NA, NA,  NA, NA, NA,  NA,
 3,  0,  9,   3,  9,  11,  1,  2,  9,   2,  11, 9,   NA, NA, NA,  NA,
 0,  2,  11,  8,  0,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 3,  2,  11,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 2,  3,  8,   2,  8,  10,  10, 8,  9,   NA, NA, NA,  NA, NA, NA,  NA,
 9, 10,  2,   0,  9,  2,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 2,  3,  8,   2,  8,  10,  0,  1,  8,   1,  10, 8,   NA, NA, NA,  NA,
 1, 10,  2,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 1,  3,  8,   9,  1,  8,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  9,  1,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 0,  3,  8,   NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA,
 NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA, NA, NA,  NA))  + 1
 ## note: +1 as table is from c-code



## =============================================================================
##  Function to create triangle in one grid cell
## =============================================================================

CreateTriangle <- function(grid.val, grid.p, level) {

 # grid.val : vector with 8 values
 # grid.p: matrix with 8 rows, 3 columns (x, y, z)

# ---------------------------------------------------------------
# Function to linearly interpolate the position where an isosurface cuts
# an edge between two vertices, each with their own scalar value
# ---------------------------------------------------------------

  VertexInterp <- function (level, i1, i2){

    p1    <- grid.p[i1, ]       # vertex 1 of cut edge
    p2    <- grid.p[i2, ]

    valp1 <- grid.val[i1]       # value 1 of cut edge
    valp2 <- grid.val[i2]

    if (abs(valp1 - valp2) < 0.00001)
      return(p1)

    return(p1 + (level - valp1) / (valp2 - valp1)*(p2 - p1))
  }

# ---------------------------------------------------------------
# Find the index into the edge table of vertices inside the surface
# Returns a 12 bit number, each bit is an edge,
# 1 if the edge is cut by the isosurface.
# ---------------------------------------------------------------

  cubeindex <- as.raw(0)
  if (grid.val[1] < level) cubeindex <- cubeindex | as.raw(1)    # bitwise OR
  if (grid.val[2] < level) cubeindex <- cubeindex | as.raw(2)
  if (grid.val[3] < level) cubeindex <- cubeindex | as.raw(4)
  if (grid.val[4] < level) cubeindex <- cubeindex | as.raw(8)
  if (grid.val[5] < level) cubeindex <- cubeindex | as.raw(16)
  if (grid.val[6] < level) cubeindex <- cubeindex | as.raw(32)
  if (grid.val[7] < level) cubeindex <- cubeindex | as.raw(64)
  if (grid.val[8] < level) cubeindex <- cubeindex | as.raw(128)
  cubeindex <- as.integer(cubeindex) +1

#  Edge <- as.hexmode(edges[cubeindex])
  Edge <- edges[cubeindex]

  if (Edge == 0)      # Cube is entirely in/out of the surface */
      return(0)

  vertlist <- matrix(nrow = 12, ncol = 3,  data = 0)
  
 # Find the vertices where the surface intersects the cube */

  if (Edge & 1)     vertlist[1,]  <- VertexInterp(level, 1,  2)
  if (Edge & 2)     vertlist[2,]  <- VertexInterp(level, 2,  3)
  if (Edge & 4)     vertlist[3,]  <- VertexInterp(level, 3,  4)
  if (Edge & 8)     vertlist[4,]  <- VertexInterp(level, 4,  1)
  if (Edge & 16)    vertlist[5,]  <- VertexInterp(level, 5,  6)
  if (Edge & 32)    vertlist[6,]  <- VertexInterp(level, 6,  7)
  if (Edge & 64)    vertlist[7,]  <- VertexInterp(level, 7,  8)  
  if (Edge & 128)   vertlist[8,]  <- VertexInterp(level, 8,  5)
  if (Edge & 256)   vertlist[9,]  <- VertexInterp(level, 1,  5)
  if (Edge & 512)   vertlist[10,] <- VertexInterp(level, 2,  6)
  if (Edge & 1024)  vertlist[11,] <- VertexInterp(level, 3,  7)
  if (Edge & 2048)  vertlist[12,] <- VertexInterp(level, 4,  8)

 # Form the correct facets from the positions that the isosurface
 # intersects the edges of the grid cell.
 # triTable uses the same cubeindex but allows the vertex sequence
 # to be looked up for as many triangular facets are necessary to represent
 # the isosurface within the grid cell. There are at most 5 triangular facets necessary.

  tritable  <- triTable[cubeindex, ]
  is        <- sum (! is.na(tritable))  # ! of elements not NA
  triangles <- vertlist[tritable[1:is],]

  return(triangles)
}


## =============================================================================
##  Function to create isosurface triangles of an array
## =============================================================================

createisosurf <- function(x, y, z, colvar, level = mean(colvar))  {
  Tri <- computeContour3d(vol = colvar, maxvol = max(colvar, na.rm = TRUE), 
     level = level, x = x, y = y, z = z, mask = NULL)
  colnames(Tri) <- c("x","y","z")
  invisible(Tri)
}  

### NOT USED ANYMORE                 
createisosurf.old <- function(x, y, z, colvar, level = mean(colvar)) {

  if (! ispresent(colvar))
    stop("'colvar' has to be defined and be an array of dimension 3")

 # check dimensionality 
  DD <- dim(colvar)
  if (length(DD) != 3)
    stop("'colvar' has to be an array of dimension 3")
  if (DD[1] !=  length(x))
    stop("dimension of 'colvar' not compatible with length of 'x'")
  if (DD[2] !=  length(y))
    stop("dimension of 'colvar' not compatible with length of 'y'")
  if (DD[3] !=  length(z))
    stop("dimension of 'colvar' not compatible with length of 'z'")

  Nx <- length(x)
  Ny <- length(y)
  Nz <- length(z)
  Tri <- NULL
  
 # find cells that contain level
  mm <- mesh(1:(Nx-1),1:(Ny-1),1:(Nz-1))
  ix <- as.vector(mm$x)
  iy <- as.vector(mm$y)
  iz <- as.vector(mm$z)

  MIN <- array(dim = c(Nx-1, Ny-1, Nz-1), data = 
       pmin(colvar[cbind(ix,  iy,  iz  )], colvar[cbind(ix,  iy+1,iz  )], 
            colvar[cbind(ix  ,iy,  iz+1)], colvar[cbind(ix+1,iy,  iz  )], 
            colvar[cbind(ix+1,iy+1,iz  )], colvar[cbind(ix+1,iy,  iz+1)],
            colvar[cbind(ix,  iy+1,iz+1)], colvar[cbind(ix+1,iy+1,iz+1)]))
        
  MAX <- array(dim = c(Nx-1, Ny-1, Nz-1), data = 
       pmax(colvar[cbind(ix,  iy,  iz  )], colvar[cbind(ix,  iy+1,iz  )], 
            colvar[cbind(ix  ,iy,  iz+1)], colvar[cbind(ix+1,iy,  iz  )], 
            colvar[cbind(ix+1,iy+1,iz  )], colvar[cbind(ix+1,iy,  iz+1)],
            colvar[cbind(ix,  iy+1,iz+1)], colvar[cbind(ix+1,iy+1,iz+1)]))

  iindex <- which(MIN <= level & MAX >= level, arr.ind = TRUE)        

  fun <- function(ii) {
        i <- ii[1]
        j <- ii[2]
        k <- ii[3]
        ix <- c(i,   i+1, i+1, i, i,   i+1, i+1, i)
        iy <- c(j+1, j+1, j,   j, j+1, j+1, j,   j)
        iz <- c(k,   k,   k,   k, k+1, k+1, k+1, k+1)
        grid.p   <- cbind(x[ix], y[iy], z[iz]) 
        grid.val <- colvar[cbind(ix, iy, iz)]

        TT <-  CreateTriangle(grid.val, grid.p, level)
        as.vector(t(TT))
  }

  TRI <- apply(iindex, MARGIN = 1, FUN = fun)
  Tri <- t(matrix(nrow = 3, data = unlist(TRI)))
  colnames(Tri) <- c("x","y","z")

  invisible(Tri)
}


