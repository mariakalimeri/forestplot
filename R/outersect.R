# Find the Outersect Among Two Sets
#
# This function receives two vectors, numeric or character, and returns their
# xor.
#
#
# @param x A vector, numeric or character
# @param y A vector, numeric or character
# @return A vector of the same class as the inputs
# @author Maria Kalimeri
# @note I couldn't find an R-ready xor function for characters.
# @importFrom dplyr setdiff
# @examples
# x <- c(4,6,7,8,9,10)
# y <- c(2,3,4,6,7,8,9)
# outersect(x, y)
# # [1] 2 3 10
# #
# x <- c("water", "sky", "baloon")
# y <- c("water", "sky", "fire", "baloon", "down")
# outersect(x, y)
# # [1] "fire" "down"

outersect <- function(x, y) {
  c(setdiff(y,x),
    setdiff(x,y))
}
