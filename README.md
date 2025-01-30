# PSE-and-Slope-Computation
Extract the point of subjective equality and slope of logistic regressions

This script computes for each participant and level of a condition variable the logistic regression of Y on X
and then computes the X value for which Y = 0.5 (that is to say the point of subjective equality [PSE])

📂 Input Data Requirements
You must provide your data in a .xlsx or .csv file located in the same folder as the script. The dataset should have one row per trial, with the following columns:
- ID (Categorical) – Identifies the participant for each trial.
- Condition (Categorical) – Specifies the experimental condition for each trial.
- If multiple categorical variables exist (e.g., gender and hair color), combine them into a single string (e.g., "Female_Brown").
- X (Continuous) – The predictor variable used in the logistic regression.
- Y (Binary: 0 or 1) – The response variable, where 0 (e.g., "no") and 1 (e.g., "yes") indicate participant responses.

📊 Output Files
The script generates the following results:
- A CSV file containing:
  Intercept (a)
  Slope (b)
  Point of Subjective Equality (PSE)
  For each participant and condition.

- Three images for visualization:
  Scatter plots of individual responses (Y vs. X).
  Fitted logistic regression curves for each participant.
  A plot of the average PSE as a function of the condition.
