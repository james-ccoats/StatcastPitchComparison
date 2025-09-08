# Ask user for pitch movement input
pfx_x_input <- as.numeric(readline(prompt = "Enter the desired horizontal break: "))
pfx_z_input <- as.numeric(readline(prompt = "Enter the desired vertical break: "))
rpm_input <- as.numeric(readline(prompt = "Enter the desired minimum RPM: "))
pitch_type_input <- tolower(readline(prompt = "Enter the pitch type (e.g., 'fastball', 'slider', etc.): "))
handedness_input <- tolower(readline(prompt = "Are you right-handed or left-handed? (Enter 'right' or 'left'): "))

if (handedness_input == "right") {
  handedness_filter <- "R"  # Match right-handed pitchers
} else if (handedness_input == "left") {
  handedness_filter <- "L"  # Match left-handed pitchers
} else {
  stop("Invalid input. Please enter 'right' or 'left'.")
}

# Filter by pitcher handedness and pitch type
filtered_data <- savant_data %>%
  filter(pitch_hand == handedness_filter & 
           tolower(pitch_name) == pitch_type_input & 
           release_spin_rate >= rpm_input)  # Filter by RPM

if (nrow(filtered_data) == 0) {
  stop("No pitches found for the selected criteria.")
}

# Find the pitch closest to the input
closest_pitch <- filtered_data %>%
  mutate(
    pfx_x_diff = abs(pfx_x*-12 - pfx_x_input),
    pfx_z_diff = abs(pfx_z*12 - pfx_z_input),
    total_diff = pfx_x_diff + pfx_z_diff  # Calculate total difference
  ) %>%
  arrange(total_diff) %>%  # Sort by closest match
  select(play_id, pfx_x, pfx_z, pfx_x_diff, pfx_z_diff, pitcher_id, pitch_name, release_spin_rate) %>%
  slice(1)  # Get the closest pitch

# Display the closest match
if (nrow(closest_pitch) > 0) {
  message(sprintf(
    "Closest pitch found:\n  Pitcher ID: %s\n  Pitch Name: %s\n  RPM: %d\n  Horizontal Break (pfx_x): %.2f (%.2f away from input)\n  Vertical Break (pfx_z): %.2f (%.2f away from input)",
    closest_pitch$pitcher_id, closest_pitch$pitch_name, closest_pitch$release_spin_rate,
    closest_pitch$pfx_x*-12, closest_pitch$pfx_x_diff,
    closest_pitch$pfx_z*12, closest_pitch$pfx_z_diff
  ))
  
  # Open the video for the closest match
  httr::BROWSE(sabRmetrics::get_video_url(closest_pitch$play_id[1]))
} else {
  message("No matching pitches found.")
}
  
