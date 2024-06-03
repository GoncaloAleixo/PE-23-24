set.seed(2336)

realizations <- 650

sistema <- replicate(realizations, {
  emissions <- sample(1:10, 9, replace = TRUE, prob = 1:10 / 45)
  sound <- any(emissions == 2)
  not_turn_off <- !any(emissions == 1)
  c(sound, not_turn_off)
})

prop <- sum(sistema[1,] & sistema[2,]) / sum(sistema[2,])
prop