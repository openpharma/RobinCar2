# reporting function give info message when eff_measure is NULL

    Code
      report_vcov_robincar(vcovRobinCar.prediction_cf(predict_counterfactual(fit_lm,
        "treatment", dummy_data)), digits = 3)
    Message
      i When eff_measure is NULL, nothing to report

# reporting function works for gaussian fitted by lm

    Code
      report_vcov_robincar(vcovRobinCar.prediction_cf(predict_counterfactual(fit_lm,
        "treatment", dummy_data), eff_measure = "diff"), digits = 3)
    Output
        Total Arm_1    N_Arm_1 Arm_0    N_Arm_0 Eff_measure Estimate  S.E.
      1   600  trt1 198(33.0%)   pbo 202(33.7%)        diff    0.564 0.498
      2   600  trt2 200(33.3%)   pbo 202(33.7%)        diff    0.771 0.502
      3   600   pbo 202(33.7%)  trt1 198(33.0%)        diff   -0.564 0.498
      4   600  trt2 200(33.3%)  trt1 198(33.0%)        diff    0.207 0.529
      5   600   pbo 202(33.7%)  trt2 200(33.3%)        diff   -0.771 0.502
      6   600  trt1 198(33.0%)  trt2 200(33.3%)        diff   -0.207 0.529
        95%CI_lower 95%CI_upper
      1      -0.412       1.539
      2      -0.213       1.755
      3      -1.539       0.412
      4      -0.829       1.244
      5      -1.755       0.213
      6      -1.244       0.829

# reporting function works for guassian fitted by glm

    Code
      report_vcov_robincar(vcovRobinCar.prediction_cf(predict_counterfactual(fit_glm,
        "treatment", dummy_data), eff_measure = "diff"), digits = 3)
    Output
        Total Arm_1    N_Arm_1 Arm_0    N_Arm_0 Eff_measure Estimate  S.E.
      1   600  trt1 198(33.0%)   pbo 202(33.7%)        diff    0.564 0.498
      2   600  trt2 200(33.3%)   pbo 202(33.7%)        diff    0.771 0.502
      3   600   pbo 202(33.7%)  trt1 198(33.0%)        diff   -0.564 0.498
      4   600  trt2 200(33.3%)  trt1 198(33.0%)        diff    0.207 0.529
      5   600   pbo 202(33.7%)  trt2 200(33.3%)        diff   -0.771 0.502
      6   600  trt1 198(33.0%)  trt2 200(33.3%)        diff   -0.207 0.529
        95%CI_lower 95%CI_upper
      1      -0.412       1.539
      2      -0.213       1.755
      3      -1.539       0.412
      4      -0.829       1.244
      5      -1.755       0.213
      6      -1.244       0.829

# reporting function works for binomial fitted by glm

    Code
      report_vcov_robincar(vcovRobinCar.prediction_cf(predict_counterfactual(
        fit_binom, "treatment", dummy_data), eff_measure = "diff"), digits = 3)
    Output
        Total Arm_1    N_Arm_1 Arm_0    N_Arm_0 Eff_measure Estimate  S.E.
      1   600  trt1 198(33.0%)   pbo 202(33.7%)        diff    0.225 0.236
      2   600  trt2 200(33.3%)   pbo 202(33.7%)        diff    0.265 0.236
      3   600   pbo 202(33.7%)  trt1 198(33.0%)        diff   -0.225 0.236
      4   600  trt2 200(33.3%)  trt1 198(33.0%)        diff    0.041 0.237
      5   600   pbo 202(33.7%)  trt2 200(33.3%)        diff   -0.265 0.236
      6   600  trt1 198(33.0%)  trt2 200(33.3%)        diff   -0.041 0.237
        95%CI_lower 95%CI_upper
      1      -0.238       0.687
      2      -0.196       0.727
      3      -0.687       0.238
      4      -0.425       0.506
      5      -0.727       0.196
      6      -0.506       0.425

---

    Code
      report_vcov_robincar(vcovRobinCar.prediction_cf(predict_counterfactual(
        fit_binom, "treatment", dummy_data), eff_measure = "risk ratio"), digits = 3)
    Output
        Total Arm_1    N_Arm_1 Arm_0    N_Arm_0 Eff_measure Estimate  S.E.
      1   600  trt1 198(33.0%)   pbo 202(33.7%)  risk ratio    1.631 2.610
      2   600  trt2 200(33.3%)   pbo 202(33.7%)  risk ratio    1.745 2.890
      3   600   pbo 202(33.7%)  trt1 198(33.0%)  risk ratio    0.613 0.369
      4   600  trt2 200(33.3%)  trt1 198(33.0%)  risk ratio    1.070 0.650
      5   600   pbo 202(33.7%)  trt2 200(33.3%)  risk ratio    0.573 0.312
      6   600  trt1 198(33.0%)  trt2 200(33.3%)  risk ratio    0.935 0.496
        95%CI_lower 95%CI_upper
      1       0.558       4.762
      2       0.609       5.001
      3       0.210       1.791
      4       0.493       2.324
      5       0.200       1.642
      6       0.430       2.030

---

    Code
      report_vcov_robincar(vcovRobinCar.prediction_cf(predict_counterfactual(
        fit_binom, "treatment", dummy_data), eff_measure = "odds ratio"), digits = 3)
    Output
        Total Arm_1    N_Arm_1 Arm_0    N_Arm_0 Eff_measure Estimate   S.E.
      1   600  trt1 198(33.0%)   pbo 202(33.7%)  odds ratio    2.504 29.088
      2   600  trt2 200(33.3%)   pbo 202(33.7%)  odds ratio    2.968 43.252
      3   600   pbo 202(33.7%)  trt1 198(33.0%)  odds ratio    0.399  0.740
      4   600  trt2 200(33.3%)  trt1 198(33.0%)  odds ratio    1.185  6.372
      5   600   pbo 202(33.7%)  trt2 200(33.3%)  odds ratio    0.337  0.558
      6   600  trt1 198(33.0%)  trt2 200(33.3%)  odds ratio    0.844  3.231
        95%CI_lower 95%CI_upper
      1       0.354      17.710
      2       0.407      21.653
      3       0.056       2.824
      4       0.170       8.280
      5       0.046       2.459
      6       0.121       5.896

