length = 11
width = 8.5
width_calc = width - 2*x
leng_calc = length - 2*x

Volume <- function(x){
  vv = x * (leng_calc * width_calc)
  return(vv)
} 

Volume(x = 1.92)
Volume(x = 2.05)
Volume(x = 0.38)

volume <- function(h, l = 11, w = 8.5){
  vvv = h * (l - 2 * h) * (w - 2 * h)
  return(vvv)
}
volume(1.92)
volume(2.05)
volume(0.38)
plot(volume)


leng = 11
width = 8.5
part1 = (leng - 2 * width)



getStops <- function(n){
  s = 0
  while (n != 1){
    if (n %% 2 > 0){
      n <- ((3 * n) + 1)
      s <- s + 1
    }
    else{
      n <- (n/2)
      s<- s + 1
    }
  }
  return(s)
}

vecGetStops <- Vectorize(comp_1)
mems <- 1:2000
seq_stops <- sapply(mems, vecGetStops)
hist(seq_stops, main = "Stopping Time", xlab = "Sequence Length", ylab = "Frequency", col = "pink", border = "red")

getStops(35)
getStops(569)
getStops(276)
getStops(61)
getStops(4918)


install.packages('ggplot2')

library(ggplot2)

# Function to create a spinner plot
create_spinner <- function() {
  # Define sectors
  sectors <- data.frame(
    angle = seq(0, 360, length.out = 5)[-1],
    label = c("Player A", "Player B", "Player A", "Player B")
  )
  
  # Create the spinner
  ggplot(sectors, aes(x = factor(label), fill = label)) +
    geom_bar(width = 1, color = "black") +
    coord_polar(theta = "x") +
    theme_void() +
    labs(title = "Spinner Game: Player A vs Player B") +
    theme(legend.position = "none")
}

# Function to simulate the spinner game
spinner_game <- function() {
  player_a_wins <- 0
  player_b_wins <- 0
  rounds <- 1
  
  while (player_a_wins < 5 && player_b_wins < 5) {
    # Simulate a spin (randomly select player)
    spin_result <- sample(c("Player A", "Player B"), 1)
    
    if (spin_result == "Player A") {
      player_a_wins <- player_a_wins + 1
    } else {
      player_b_wins <- player_b_wins + 1
    }
    
    cat(sprintf("Round %d: %s wins! (Score: A: %d, B: %d)\n", 
                rounds, spin_result, player_a_wins, player_b_wins))
    rounds <- rounds + 1
  }
  
  if (player_a_wins == 5) {
    cat("Player A wins the game!\n")
  } else {
    cat("Player B wins the game!\n")
  }
}

# Create and display the spinner
create_spinner()

# Start the game
spinner_game()

simResults <- replicate(
  n = 1000,
  expr = spinner_game()
  
)
table(simResults)



