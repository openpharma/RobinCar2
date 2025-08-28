# confint works correctly

    Code
      confint(robin_res$marginal_mean)
    Output
            Estimate    Std.Err     2.5 %    97.5 %
      pbo  0.3660066 0.03386416 0.2996341 0.4323791
      trt1 0.5809844 0.03502713 0.5123324 0.6496363
      trt2 0.6101537 0.03447222 0.5425894 0.6777180

---

    Code
      confint(robin_res$contrast)
    Message
      The confidence interval is transformed.
    Output
                     Estimate    2.5 %   97.5 %
      trt1 v.s. pbo  1.587360 1.278881 1.970249
      trt2 v.s. pbo  1.667056 1.348364 2.061073
      trt2 v.s. trt1 1.050207 0.893430 1.234494

# confint works with parm argument

    Code
      confint(robin_res$marginal_mean, parm = 1:2)
    Output
            Estimate    Std.Err     2.5 %    97.5 %
      pbo  0.3660066 0.03386416 0.2996341 0.4323791
      trt1 0.5809844 0.03502713 0.5123324 0.6496363

---

    Code
      confint(robin_res$marginal_mean, parm = c("pbo", "trt2"))
    Output
            Estimate    Std.Err     2.5 %    97.5 %
      pbo  0.3660066 0.03386416 0.2996341 0.4323791
      trt2 0.6101537 0.03447222 0.5425894 0.6777180

---

    Code
      confint(robin_res$contrast, parm = 1:2)
    Message
      The confidence interval is transformed.
    Output
                    Estimate    2.5 %   97.5 %
      trt1 v.s. pbo 1.587360 1.278881 1.970249
      trt2 v.s. pbo 1.667056 1.348364 2.061073

---

    Code
      confint(robin_res$contrast, parm = c("trt1 v.s. pbo"))
    Message
      The confidence interval is transformed.
    Output
                    Estimate    2.5 %   97.5 %
      trt1 v.s. pbo  1.58736 1.278881 1.970249

# confint works with level argument

    Code
      confint(robin_res$marginal_mean, level = 0.8)
    Output
            Estimate    Std.Err      10 %      90 %
      pbo  0.3660066 0.03386416 0.3226079 0.4094053
      trt1 0.5809844 0.03502713 0.5360953 0.6258734
      trt2 0.6101537 0.03447222 0.5659758 0.6543316

---

    Code
      confint(robin_res$contrast, level = 0.7)
    Message
      The confidence interval is transformed.
    Output
                     Estimate      15 %     85 %
      trt1 v.s. pbo  1.587360 1.4159561 1.779513
      trt2 v.s. pbo  1.667056 1.4901323 1.864987
      trt2 v.s. trt1 1.050207 0.9641516 1.143943

# confint works with transform argument

    Code
      confint(robin_res$contrast, transform = exp)
    Message
      The confidence interval is transformed.
    Output
                     Estimate    2.5 %   97.5 %
      trt1 v.s. pbo  1.587360 1.278881 1.970249
      trt2 v.s. pbo  1.667056 1.348364 2.061073
      trt2 v.s. trt1 1.050207 0.893430 1.234494

---

    Code
      confint(robin_res$contrast, transform = identity)
    Output
                       Estimate      2.5 %    97.5 %
      trt1 v.s. pbo  0.46207246  0.2459851 0.6781598
      trt2 v.s. pbo  0.51105950  0.2988922 0.7232268
      trt2 v.s. trt1 0.04898704 -0.1126873 0.2106614

