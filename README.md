# Jersey Number Optimizer

This is a Shiny app to help assign jersey numbers to players based on their preferences

App available here: https://genomicepi.shinyapps.io/Jersey_Optimizer/

<img width="1499" alt="image" src="https://github.com/user-attachments/assets/417c0bee-6de6-4879-9fba-e390dc2db81c" />

## 🚀 Features

- Input available jersey numbers (including ranges like `1-5,8,10`)
- Add players and select their top 5 preferred jersey numbers
- Optimize assignments to minimize total ranking cost
- Download the final assignments
- Reset each step as needed

## 🧩 How It Works

1. **Enter Jersey Numbers**  
   Use comma-separated values or ranges like `1-3,5,10`. Click **Submit Jersey Numbers**.

2. **Add Players**  
   Enter each player's name and then specify their top 5 choices from the list.

3. **Optimize**  
   Click **Optimize Assignments** to assign jerseys based on ranked preferences using the Hungarian algorithm.

4. **View & Download Results**  
   See the optimized table, ranked scores, and optionally download as CSV.

## ▶️ Run the App Locally

To run this app locally, make sure you have R and the following packages installed:

```r
install.packages(c("shiny", "tidyverse", "DT", "clue"))
