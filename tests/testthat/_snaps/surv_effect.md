# print method for surv_effect works as expected

    Code
      print(x)
    Output
      Model        :  Surv(time, status) ~ meal.cal + age 
      Randomization:  sex ~ strata  ( Simple )
      
      Contrast     :  Log Hazard ratio
      
                       Estimate Std.Err Z Value Pr(>|z|)   
      Male v.s. Female  0.55219 0.19133  2.8861   0.0039 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
      
      Test         :  Log-Rank
      
                       Test Stat. Pr(>|z|)   
      Male v.s. Female     2.9496 0.003181 **
      ---
      Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# table method for surv_effect works as expected

    Code
      result <- table(x)
    Output
      Number of patients and events per stratum and treatment arm:
        strata    sex Patients Events
      1      0 Female       20      6
      2      0   Male       30     24
      3      1 Female       29     21
      4      1   Male       57     43
      5      2 Female       18     14
      6      2   Male       25     24
      7      3   Male        1      1

# confint method for surv_effect works as expected

    Code
      result <- confint(x)

---

    Code
      expect_message(result <- confint(x, transform = exp),
      "The confidence interval is transformed.", fixed = TRUE)

