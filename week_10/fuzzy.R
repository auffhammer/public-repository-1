rm(list = ls()) # clear memory
setwd("/Users/auffhammer/Library/CloudStorage/Dropbox/06_Teaching/MACSS/2024/code/public-repository-1/week_10")
set.seed(22092008) # set random number generator seed
library(pacman)
p_load(tidyverse,broom,rdrobust,estimatr,modelsummary)
# This is an excellent example from Andrew Weiss: 
#hypothetical example, students take an entrance exam 
#at the beginning of a school year. 
#Those who score 70 or below are automatically enrolled 
#in a free tutoring program and receive assistance throughout the year. 
#At the end of the school year, students take a final test, 
#or exit exam (with a maximum of 100 points) to measure how much 
#they learned overall. Remember, this is a hypothetical example 
#and tests like this don’t really exist

tutoring <- read_csv("tutoring_program_fuzzy.csv")

# Show that some people scored higher on the entrance exam and somehow used tutoring, or that some people scored below the threshold but didn’t participate in the program, either because they’re never-takers, or because they fell through bureaucratic cracks.
ggplot(tutoring, aes(x = entrance_exam, y = tutoring_text, color = entrance_exam <= 70)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(size = 1.5, alpha = 0.5,
             position = position_jitter(width = 0, height = 0.25, seed = 1234)) +
  # Add vertical line
  geom_vline(xintercept = 70) +
  # Add labels
  labs(x = "Entrance exam score", y = "Participated in tutoring program") +
  # Turn off the color legend, since it's redundant
  guides(color = "none")

# You can generate a table showing these numebrs
tutoring %>%
  group_by(tutoring, entrance_exam <= 70) %>%
  summarize(count = n()) %>%
  group_by(tutoring) %>%
  mutate(prop = count / sum(count))

# For example... 36 people who should have used tutoring who didn’t (never takers?) 116 people (!!!) who somehow snuck into the program????

# Visualize the gap:
ggplot(tutoring, aes(x = entrance_exam, y = exit_exam, color = tutoring)) +
  geom_point(size = 1, alpha = 0.5) +
  # Add a line based on a linear model for the people scoring less than 70
  geom_smooth(data = filter(tutoring, entrance_exam <= 70), method = "lm") +
  # Add a line based on a linear model for the people scoring 70 or more
  geom_smooth(data = filter(tutoring, entrance_exam > 70), method = "lm") +
  geom_vline(xintercept = 70) +
  labs(x = "Entrance exam score", y = "Exit exam score", color = "Used tutoring")


# This fun code uses cut() to split the entrance exam column into distinct
# categories (0-5, 5-10, 10-15, etc.). You'll see some strange syntax in the
# categories it creates: (70, 75]. These ranges start with ( and end with ] for
# a reason: ( means the range *does not* include the number, while ] means that
# the range *does* include the number. (70, 75] thus means 71-75. You can
# reverse that with an argument to cut() so taht it would do [70, 75), which
# means 70-74.
tutoring_with_bins <- tutoring %>%
  mutate(exam_binned = cut(entrance_exam, breaks = seq(0, 100, 5))) %>%
  # Group by each of the new bins and tutoring status
  group_by(exam_binned, tutoring) %>%
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n()) %>%
  # Make this summarized data wider so that there's a column for tutoring and no tutoring
  pivot_wider(names_from = "tutoring", values_from = "n", values_fill = 0) %>%
  rename(tutor_yes = `TRUE`, tutor_no = `FALSE`) %>%
  # Find the probability of tutoring in each bin by taking
  # the count of yes / count of yes + count of no
  mutate(prob_tutoring = tutor_yes / (tutor_yes + tutor_no))

# Plot this puppy
ggplot(tutoring_with_bins, aes(x = exam_binned, y = prob_tutoring)) +
  geom_col() +
  geom_vline(xintercept = 8.5) +
  labs(x = "Entrance exam score", y = "Proportion of people participating in program")

# If this were a sharp design, every single bar to the left of the cutpoint would be 100% and every single bar to the right would be 0%, but that’s not the case here. The probability of tutoring changes at the cutpoint, but it’s not 100% perfect.

# Let's make an instrument and center this puppy
tutoring_centered <- tutoring %>%
  mutate(entrance_centered = entrance_exam - 70,
         below_cutoff = entrance_exam <= 70)
tutoring_centered

# Now we have a new column named below_cutoff that we’ll use as an instrument. Most of the time this will be the same as the tutoring column, since most people are compliers. But some people didn’t comply, like person 8 here who was not below the cutoff but still used the tutoring program.
# Let's pretend this cutoff is sharp. 
model_sans_instrument <- lm(exit_exam ~ entrance_centered + tutoring,
                            data = filter(tutoring_centered,
                                          entrance_centered >= -10 &
                                            entrance_centered <= 10))
tidy(model_sans_instrument)

# Without realizing that this is a fuzzy design we would say this is a 11.5 point increase
# on the final because of the program. This is wrong of course. You need to run 2SLS
# Using the below cutoff as an instrument. 

model_fuzzy <- iv_robust(
  exit_exam ~ entrance_centered + tutoring | entrance_centered + below_cutoff,
  data = filter(tutoring_centered, entrance_centered >= -10 & entrance_centered <= 10)
)
tidy(model_fuzzy)

