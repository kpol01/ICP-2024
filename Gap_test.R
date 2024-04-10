gap_frequency_distribution <- function(sequence, lower_bound, upper_bound) {
  # Sort the sequence
  sorted_sequence <- sequence
  
  # Initialize variables
  gap_lengths <- numeric()
  prev_num <- NULL
  
  # Iterate through the sorted sequence
  for (num in sorted_sequence) {
    # If previous number exists and the difference between current and previous exceeds bounds, count as gap
    if (!is.null(prev_num) && (num - prev_num) > upper_bound) {
      gap_lengths <- c(gap_lengths, num - prev_num)
    }
    prev_num <- num
  }
  
  # Calculate frequency distribution
  freq_table <- table(gap_lengths)
  
  return(freq_table)
}

# Example usage:
sequence <- c(1, 3, 5, 7, 10, 15, 18, 20, 25, 30)
lower_bound <- 5
upper_bound <- 15

frequency_distribution <- gap_frequency_distribution(sequence, lower_bound, upper_bound)
print(frequency_distribution)

gap.test(sequence, lower = 2, upper = 10)

