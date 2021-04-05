binary_search_in_100 <- function(number){
  guess = 50
  guess_sub = 25
  times = 0
  repeat{
    if (number>guess){
      guess = ceiling(guess + guess_sub)
    }
    else if (number<guess){
      guess = floor(guess - guess_sub)
    }
    
    times = times + 1
    guess_sub = guess_sub/2
    print(guess)
    if (number==guess){
      break
    }
  }
  print("Times:")
  print(times)
}