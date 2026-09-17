# Offsets, exposure time, and the rate estimand in `robin_glm`

**Status:** analysis only — no package code has been changed.
**Date:** 2026-09-16 (rev. 3 — bottom line sharpened; see §0.0)
**Context:** GitHub issue #117 (Tobias Mütze) — "Negative binomial model: Marginal means ignore the model offset".
**Primary reference read:** Bannick, Shao, Liu, Du, Yi, Ye (2024), *A General Form of Covariate
Adjustment in Randomized Clinical Trials*, arXiv:2306.10213v2 (25 Mar 2024, 69 pp.) — cited in
`DESCRIPTION:30` as the basis for nonlinear adjustment. Note Bannick and Ye are also authors of this
package.

---

## 0.0 Bottom line

Two statements govern everything below. They are the conclusions to carry into the issue response and
any implementation decision.

**1. There is no established method for a rate estimand under covariate adjustment in Prof. Ye's
work.** The theory RobinCar2 rests on (arXiv:2306.10213 and the companion papers cited in
`DESCRIPTION`) develops marginal means and their contrasts for a response measured on every subject.
It does not define a rate, does not treat exposure time, and does not state assumptions under which a
ratio-of-means rate estimand inherits the robustness or efficiency guarantees of Theorem 1. §0 and
§3.6 document this from the text. Everything in §4 of this note is therefore **our own construction,
proposed and not established** — it has no citation behind it, its variance is unimplemented and
unvalidated (§4 "Variance", §6 q2), and it must not be presented to users as the covariate-adjustment
literature's answer. Until Prof. Ye rules on §6 q1 and q2, RobinCar2 has no rate estimand it can
defend.

**2. Do not use offsets in any model handled by RobinCar2.** Not in the prediction step, and not in the
fitting step either. An offset is a coefficient fixed at 1 on a variable that, whenever follow-up is
treatment- or outcome-dependent, violates the "distribution of $X$ is not affected by treatments"
clause the entire robustness argument rests on (§3.2) — and even when it does not, own-exposure
prediction collapses the ratio contrast to $e^{\hat\beta}$ (§4, §5.2). The reference-exposure
workaround (§7 option 2) is *arithmetically* defensible but is a package feature with no literature
behind it and an interface that invites the invalid use, so it is not offered. `robin_glm` and
`robin_lm` should both **reject models carrying an offset**, via `offset()` in the formula or
`offset =` as an argument.

The rate framing above is the *log-link* case, and it is the one that motivated #117. It is not the
only way an offset reaches the package: under an identity link an offset is an additive shift and has
nothing to do with rates. The rejection covers both, but for different reasons and — since rev. 4 —
with different messages.

**What we do not offer as a substitute:** $\log T$ as an ordinary covariate with a **free**
coefficient. It is a legitimate working model under §2 of the paper ("can be arbitrary") and it does
keep every prediction a function of baseline covariates — but it is a *different model*, in which the
response is no longer proportional to exposure, and it estimates a marginal mean of the count rather
than a rate. Offering it as the answer to an offset user's question would conflate the two. Rev. 3 of
this note did exactly that; corrected here.

**The one exact substitute** is confined to a **gaussian identity link**, where the offset is a known
additive shift: fitting the shifted response $y - z$ gives bit-identical coefficients and standard
errors, identical treatment contrasts, and marginal means shifted by $\overline{z}$ (verified — §9).
It does not extend to an identity link with a non-gaussian family, where $y - z$ need not lie in the
response support. For any other link there is no restatement, and users needing a rate have no
supported route.

---

## 0. What reading the paper changed

Rev. 1 of this note reasoned from the structure of the estimator without consulting the source. The
paper confirms the core argument and adds one thing rev. 1 got wrong by omission.

**Confirmed, and stronger than rev. 1 claimed:**

- The regrouping rev. 1 called "the cleanest way to see it" is *literally the paper's definition*,
  equation (1). Not a re-derivation — the same formula.
- The influence function in rev. 1's §4 is *exactly* Theorem 1(i). No adjustment needed.
- `vcovG()` implements Theorem 1(ii) term for term (§3.4 below).
- The requirement that the prediction function use baseline covariates only — rev. 1's load-bearing
  claim — is the paper's own stated justification for why AIPW is robust, quoted in §3.2.

**Wrong by omission in rev. 1:** rev. 1 said the choice of prediction function "affects only
efficiency", implying that was benign. The paper (§3.3, Example 1) shows that when the working model
is misspecified *and* the link is nonlinear, the **guaranteed efficiency gain over the unadjusted
sample mean is lost** — the adjusted estimator can be *worse* than not adjusting. A negative
binomial rate model is exactly that case. The paper's remedy is **joint calibration**, which
**RobinCar2 does not implement** (§3.5). This is a separate, pre-existing gap that the offset work
touches.

**Confirmed on the original question:** the paper contains no treatment of exposure time whatsoever.
Term counts over the full 69-page text:

| term | count | term | count |
|---|---|---|---|
| `offset` | **0** | `at risk` | **0** |
| `exposure` | **0** | `window` | **0** |
| `person-time` | **0** | `censor` | **0** |
| `rate ratio` | **0** | `dropout` | **0** |
| `duration` | **0** | `recurrent` | **0** |
| `follow-up` | 2 (both descriptive, §7) | `time-varying` | **0** |

So the original instinct was right, and for a sharper reason than "the paper assumes constant
follow-up": the paper never raises the question. It also *designs around* it — see §3.6.

This is the evidence for §0.0 statement 1. The absence is not a gap in one paper's exposition that
could be closed by a careful reading; there is no definition of a rate estimand, no identification
argument for one, and no variance result covering one. Any rate method RobinCar2 ships would be new
methodology, authored by us, not an implementation of published theory.

---

## 1. Summary

1. **There are two bugs, not one.** Issue #117 correctly reports the offset is dropped from the
   counterfactual prediction. It does not report the more consequential problem: the residual used
   by the bias-correction step *does* include the offset, so prediction and residual are on
   different scales and the AIPW augmentation is silently disabled. The current output estimates
   nothing.

2. **The offset choice is an efficiency knob, not an estimand choice** — provided the residual is
   taken against the same prediction function. This follows from equation (1) of the paper and is
   confirmed numerically in §5.1. It makes issue #117's suggestion 2 ("respect the observed offset")
   the *less* robust of its two suggestions, and suggestion 1 (a reference exposure) the more robust.
   But see §0: "efficiency knob" does not mean "harmless" — and per §0.0 neither suggestion is a
   feature we should ship. The conclusion is to reject offsets, not to pick between them.

3. **Predicting at each subject's own exposure collapses the ratio contrast to the model coefficient
   $e^{\hat\beta}$**, defeating the purpose of g-computation, and is biased for the marginal rate
   ratio when follow-up is affected by treatment (~10% in §5.2).

4. **There is no established rate estimand to fall back on.** §4 sketches one — a ratio of two AIPW
   marginal means — but it is our proposal, absent from Prof. Ye's work, with an unimplemented and
   unvalidated variance. It is a research direction, not a fix for issue #117.

---

## 2. The two bugs

In `R/predict_couterfactual.R` (used for `lm`, `glm` and `MASS::glm.nb`):

```r
mm          <- model.matrix(fit, data = df)   # line 61 — no offset column
pred_linear <- mm %*% coefficients(fit)       # line 62 — offset omitted
preds       <- family(fit)$linkinv(pred_linear)

y            <- model.response(fit$model)     # line 66
residual_raw <- y - fitted(fit)               # line 68 — fitted() DOES include the offset
...
ret <- ret + bias(residual_raw, ...)          # lines 76-79 — mixed scales
```

**Bug 1 (reported).** `model.matrix()` never carries the offset, so every prediction is made at
exposure = 1.

**Bug 2 (not reported).** `fitted()` includes the offset, so `bias()` adds count-scale residual
means onto exposure = 1-scale predictions. Under `schema = "ps"` the per-stratum residual means are
not forced to zero and can be large, so the corruption is worse there than the arm-level case
suggests.

Reproduced on `main` (n = 400, `glm.nb` with `offset(log(expo))`):

| | pbo | trt |
|---|---|---|
| Correct g-computation at own exposure | 5.31 | 7.83 |
| **RobinCar2 today** | **3.18** | **4.46** |
| Observed mean count | 5.66 | 7.29 |

§3.3 explains exactly why 3.18/4.46 comes out where it does.

**Implementation note.** `predict()` handles *both* offset routes correctly with `newdata` —
`offset()` in the formula (`attr(terms(fit), "offset")` is set) and `offset =` as an argument
(`predict.lm` evaluates `object$call$offset` in `newdata`). Verified. A fix does not need bespoke
offset extraction.

---

## 3. What the paper establishes

### 3.1 The estimator is AIPW, and `bias()` is its augmentation term

Paper equation (1), §2:

$$\hat\theta_{\mathrm{AIPW},a} \;=\; \bar y_a \;-\; \frac{1}{n_a}\sum_{i:A_i=a}\hat\mu_a(X_i) \;+\; \frac1n\sum_{i=1}^n \hat\mu_a(X_i)$$

`predict_couterfactual.R:76-79` computes the sample analog: predictions plus the within-arm (or
within-stratum) mean residual. So RobinCar2's estimator *is* $\hat\theta_{\mathrm{AIPW}}$, and
`bias()` is the augmentation. Equation (2) defines **prediction unbiasedness**,
$\bar y_a = n_a^{-1}\sum_{i:A_i=a}\hat\mu_a(X_i)$; when it holds, AIPW and plain g-computation
coincide.

### 3.2 Why it is robust — and the exact sentence the offset argument turns on

The paper's justification, §2 immediately after (1), states that AIPW

> "is robust against working model misspecification, because $\bar y_a$ is asymptotically correct
> for estimating $\theta_a$ and $n_a^{-1}\sum_{i:A_i=a}\hat\mu_a(X_i) - n^{-1}\sum_{i=1}^n\hat\mu_a(X_i)$
> has asymptotic mean zero, **as the distribution of $X$ is not affected by treatments in randomized
> clinical trials.**"

That final clause is the whole offset question. An offset $\log T_i$ evaluated at each subject's own
exposure makes $\hat\mu_a$ a function of a variable whose distribution **is** affected by treatment
whenever follow-up is treatment-dependent. The robustness guarantee is then void — not weakened,
void, because the sentence it rests on no longer holds.

This is reinforced formally by Assumption 2 (Stability), which requires
$\|\hat\mu_a - \mu_a\|_{L^2} \to 0$ with the norm taken "with respect to the distribution of $X$" —
so $\hat\mu_a$ is a function of $X$ alone — and by Assumption 1(i), which makes $X$ pre-treatment by
construction (the assignment vector is conditionally independent of $\{Y_i, X_i\}$ given $Z$).

**Consequence.** If $T$ is baseline-determined, fold $\log T$ into $X$ and every result applies
unchanged. If $T$ is outcome-dependent, it cannot enter $\hat\mu_a$ at all. This is the Case A /
Case B split in §4, and it is a direct reading of the assumptions rather than an interpretation.

### 3.3 Reconciling Prof. Ye's two statements

Both are in the paper, one paragraph apart (§2, and §6/Figure 1):

> "If this condition is not met, estimators from g-computation may have large bias but the AIPW
> estimators are always asymptotically unbiased. In fact, the AIPW estimator can be viewed as a
> de-biased g-computation estimator (Firth and Bennett, 1998)"

Example 1 (§4) makes the link condition concrete: for a GLM with a **canonical** link,
$\hat\mu_a(X)=g^{-1}(\hat\alpha_a+\hat\beta_a^TX)$, the score equations (6) give prediction
unbiasedness for free — and the paper names $g^{-1}(t)=\exp(t)$ for non-negative integer responses
as the "Poisson working model" canonical case. Negative binomial with a log link and unknown
dispersion is *not* canonical; its score equations are a weighted residual sum,

$$\sum_i \frac{Y_i-\mu_i}{1+\mu_i/\theta}\,x_i = 0,$$

so (2) fails and plain g-computation is biased. §6/Figure 1 is exactly this demonstration: 1,000
two-arm trials, n = 500, Poisson data-generating process, **negative binomial working model with
treatment-by-covariate interactions and unknown dispersion**; g-computation is visibly positively
biased for $\theta_2-\theta_1=3.25$ and AIPW corrects it. So "NB leads to biased g-computation" and
"the method is robust" are the same statement from either side of the augmentation term.

**Why the current code gives 3.18/4.46.** It uses $g_a(X_i)=\exp(x_i'\hat\beta_a)$ for the
prediction but $Y_i-\hat\mu_i$ with $\hat\mu_i=T_i\exp(x_i'\hat\beta_a)$ for the residual. The
augmentation bracket becomes
$\frac1n\sum_i g_a(X_i) - \frac{1}{n_a}\sum_{A_i=a}\hat\mu_i$, which converges to
$E[g_a(X)]-\theta_a$ instead of to zero, so

$$\hat\theta_a \;\to\; E[g_a(X)] \;=\; E\big[\exp(X'\beta_a)\big],$$

the exposure = 1 mean. Matches both the reproduction in §2 and the issue reporter's observation that
the output equals `mean(predict(fit, newdata = <os = 0>, type = "response"))`.

### 3.4 `vcovG()` implements Theorem 1(ii)

Theorem 1(i) gives the influence function

$$\phi_a = \frac{I(A_i=a)}{\pi_a}\Big(y_{a,i}-\mu_a(X_i)-\theta_a+E\{\mu_a(X)\}\Big) + \mu_a(X_i)-E\{\mu_a(X)\}$$

and Theorem 1(ii) the variance

$$V = \mathrm{diag}\big\{\pi_a^{-1}\mathrm{Var}(y_a-\mu_a(X))\big\} + \mathrm{Cov}\{Y-\mu(X),\mu(X)\} + \mathrm{Cov}\{\mu(X),Y\} - E\big[(R_Y-R_X)(\Omega_{SR}-\Omega(Z))(R_Y-R_X)\big]$$

Mapping onto `R/variance_anhecova.R`:

| paper term | code |
|---|---|
| $\pi_a^{-1}\mathrm{Var}(y_a-\mu_a(X))$ | `vcov_sr <- (var(y[is]) + diag(var_preds) - 2*diag(cov_ymu))/pi_t` (line 28) |
| $\mathrm{Cov}\{Y-\mu,\mu\}+\mathrm{Cov}\{\mu,Y\}$ | `v <- diag(vcov_sr) + cov_ymu + t(cov_ymu) - var_preds` (line 33) |
| $E[(R_Y-R_X)(\Omega_{SR}-\Omega(Z))(R_Y-R_X)]$ | `h_get_erb()` (line 34, and lines 56-81) |

Consistent with the theory, `h_get_erb()` returns 0 for simple randomization ($\Omega(Z)=\Omega_{SR}$),
and the stratum-level `bias()` path enforces the sample analog of $E\{y_a-\mu_a(X)\mid Z\}=0$, which
per equation (6) is what makes $R_Y-R_X=0$ and the term vanish when strata are in the model.

**This is good news for §4's variance sketch:** the extension needed is $V$ for a stacked $2k$-vector,
i.e. structural rather than conceptual. But note Theorem 1 is stated for a **scalar** response with
$\theta\in\mathbb{R}^k$ indexed by arm. There is no multivariate-response version, so the covariance
*between* a $Y$-block and a $T$-block under CAR is not covered as stated. That is open question 2 in §6.

### 3.5 Joint calibration — the paper's remedy, absent from RobinCar2

§3.3 and Example 1 establish the limitation rev. 1 missed:

> "When the working model is misspecified and $g^{-1}$ is nonlinear, however, neither (G1) nor (G2)
> is satisfied. Thus, the AIPW estimator $\hat\theta_{\mathrm{AIPW}}$ that uses the GLM conditional
> mean estimate $\hat\mu_a(X)$ may not have guaranteed efficiency gain over the benchmark sample
> mean vector $\bar Y$."

Consistency is never at risk; *guaranteed efficiency gain* is. A negative binomial rate model is
squarely in this regime. §4 of the paper proposes joint calibration:

$$\hat\mu^*_a(X_i)=\hat\gamma_a^T\hat W_i, \qquad \hat W = \big(Z^T, \hat\mu(X)^T\big)^T$$

fitted by within-arm OLS of $y_a$ on $\hat W$ **plus an intercept**. Theorem 5 gives $\hat\theta_{JC}$
both guaranteed efficiency gain over $\bar Y$ and universal applicability. §7 adds that JC "is the
only method for which the naive standard errors developed under simple randomization are correct."

**`grep -rn -i "calibrat|aipw" R/ man/ NAMESPACE vignettes/` returns nothing** — JC is not in
RobinCar2. Two observations:

- Because JC is an OLS recalibration *with an intercept*, its residuals sum to zero within arm, so
  prediction unbiasedness holds automatically and AIPW = g-computation.
- A linear recalibration absorbs a common multiplicative scale factor in $\hat\mu$. So JC would make
  the estimator largely insensitive to the *reference-exposure* choice — but it does **not** rescue
  Case B, because $\hat\mu(X_i)$ must still be a function of baseline $X$ only.

### 3.6 The paper's own count endpoint sidesteps exposure time

§7 analyses a trial of insulin peglispro vs. glargine. The endpoint is telling:

> "an indicator of having an average of more than two hypoglycemic events per 30 days, during weeks
> 52-78 of follow-up. The estimand that we are interested in is the linear contrast of treatment
> group means."

A rate-type quantity, handled by **binarizing over a fixed window** — and a baseline count
("baseline hypoglycemic events count") used only as a *covariate*. Combined with the zero term
counts in §0, the reading is that the authors did not derive around variable follow-up; they
specified an endpoint that avoids it. That is a legitimate design choice, and it is also exactly why
issue #117 has no answer in the literature the package cites.

---

## 4. Defining the estimand when follow-up varies

> **Read this section as a proposal, not as method.** Nothing below is drawn from Prof. Ye's work or
> from any other source cited in `DESCRIPTION`; §0/§3.6 establish that no such source exists. The
> construction is ours. It reuses Theorem 1 componentwise, which is where its plausibility comes from,
> but the stacked-response variance it needs is outside what Theorem 1 states (§3.4, §6 q2) and has
> not been implemented or validated. It is not a basis for changing package behaviour today.

Let $T_i$ be exposure time and $Y_i$ the count.

### Candidate definition

$$\rho_a \;=\; \frac{E[Y(a)]}{E[T(a)]}, \qquad \mathrm{RR} \;=\; \frac{\rho_1}{\rho_0}$$

Both $E[Y(a)]$ and $E[T(a)]$ are marginal means of **observed** variables under randomization, so
both are identified without new assumptions and both are estimable by AIPW from baseline covariates
only. This is the ICH E9(R1) *while-on-treatment* estimand. The unequal-follow-up problem is not
assumed away; it moves into the denominator.

This is also consistent with how the paper handles derived estimands (§2): "To estimate $f(\theta)$,
such as a linear contrast of $\theta$ or risk ratio, we use the AIPW estimator $f(\hat\theta_{AIPW})$
… an **augment-then-contrast** approach." The rate ratio is such an $f$, once $\theta$ is the stacked
vector $(\theta_{Y,0},\theta_{Y,1},\theta_{T,0},\theta_{T,1})$.

### Two regimes

**Case A — $T$ is baseline-determined.** Planned window, staggered entry with a common data cutoff,
administrative censoring only. Then $T(1)=T(0)=T$, so $E[T(1)]=E[T(0)]$ and the denominator cancels:

$$\mathrm{RR} = E[Y(1)]/E[Y(0)]$$

— the ratio of marginal mean counts, which `robin_glm` already computes. Absolute rates are
$\hat\theta_a/\bar T$. **Case A is a special case of the general formula**, so implementing the
general formula gets it for free; no separate code path is needed.

**Case B — $T$ is outcome-dependent.** Follow-up terminated by death, dropout, or discontinuation,
so $T_i=T_i(A_i)$. Then $\log T_i$ cannot enter $\hat\mu_a$ (§3.2). Use the general formula:
AIPW-estimate $E[Y(a)]$ and $E[T(a)]$ separately from baseline $X$.

### Why own-exposure prediction is the wrong choice

With a multiplicative model, $\lambda_1=\lambda_0e^{\beta}$, predicting at own $T_i$ gives

$$\frac{\sum_i T_i\lambda_1(X_i)}{\sum_i T_i\lambda_0(X_i)} \;=\; e^{\hat\beta}$$

The $T_i$ cancel **exactly**. Including the offset at own exposure therefore collapses the ratio
contrast to the conditional model coefficient — an elaborate route to a number `summary(fit)` already
prints, and not a marginal quantity. Confirmed in §5.2.

Predicting at a **common reference** exposure $\tau$ is by contrast arithmetically valid in *both*
regimes, because $\tau\exp(x_i'\hat\beta_a)$ is a function of baseline covariates. The augmentation
absorbs the entire scale discrepancy, so $\hat\theta_a\to E[Y(a)]$ regardless of $\tau$. $\tau$ does
not change the estimand — but per §3.5 it does affect whether efficiency gain is guaranteed, and joint
calibration is the principled way to stop caring. Being arithmetically valid is not sufficient reason
to ship it: see §0.0 statement 2 and §7 option 2.

### Where an offset would belong — and why we still do not accept one

The working model "can be arbitrary" (§2), so an offset in the **fitting** step is not itself a
theoretical error. It is nevertheless not accepted, for interface reasons rather than algebraic ones:
a fitted object carrying an offset is exactly the input that makes the prediction step ambiguous, and
every choice available at that point is either invalid (own exposure, Case B), estimand-collapsing
(own exposure, ratio contrast), or unpublished (reference exposure). Rejecting the input removes the
ambiguity at its source.

There is no substitute that answers the same question. Two candidates and why neither is one:

- **$\log T$ as an ordinary covariate with a free coefficient.** Valid as a working model, and under
  Case B it keeps $\hat\mu_a$ a function of baseline covariates as §3.2 requires. But freeing the
  coefficient changes the model: the response is no longer proportional to exposure, which is the
  content of an offset. It targets a marginal mean of the count, not a rate. It is a legitimate thing
  to fit if that is what the analyst wants; it is not the offset model and must not be presented as
  the fix for one.
- **The shifted response $y - z$.** Exact, but only for a **gaussian identity link**, where the offset
  is a known additive shift. Then contrasts are identical and marginal means shift by $\overline{z}$
  (§9). Under a log link the offset is multiplicative and no response transformation reproduces it.

So under Case B, or under any non-identity link, a marginal *rate* still requires the unestablished §4
construction and there is no supported route to one. The package says so rather than substituting a
different estimand.

### Variance

Per-arm influence functions $\phi^Y_a$ and $\phi^T_a$, each exactly Theorem 1(i) with $Y_i$ or $T_i$
as the response. Stack the four, take the empirical covariance, and apply the delta method on the log
scale, where

$$\log\mathrm{RR} = \log\theta_{Y,1}-\log\theta_{T,1}-\log\theta_{Y,0}+\log\theta_{T,0}$$

has gradient $(\theta_{Y,1}^{-1},\,-\theta_{T,1}^{-1},\,-\theta_{Y,0}^{-1},\,\theta_{T,0}^{-1})$.
Under CAR the $(\Omega_{SR}-\Omega(Z))$ term applies blockwise. **Not verified** — see §6 q2.

---

## 5. Simulation evidence

Both scripts are self-contained and were run on 2026-09-16.

### 5.1 The estimand does not depend on the prediction function (n = 400,000, single replicate)

```r
set.seed(42); n <- 400000
X <- rnorm(n); A <- rbinom(n,1,.5)
Tt <- runif(n, .5, 3)                                  # baseline exposure
lam <- exp(-0.3 + 0.4*A + 0.8*X)
Y <- rnbinom(n, mu = Tt*lam, size = 2)
d <- data.frame(Y, X, A = factor(A), Tt, os = log(Tt))

truth <- c(mean(Tt*exp(-0.3+0.8*X)), mean(Tt*exp(-0.3+0.4+0.8*X)))
fit <- MASS::glm.nb(Y ~ A + X + offset(os), data = d)   # non-canonical link
aug <- function(g) sapply(1:2, function(k) {
  a <- k-1; mean(g[,k]) + mean(Y[A==a] - g[A==a,k]) })
lp <- function(a) cbind(1,a,X) %*% coef(fit)[1:3]

G_own <- cbind(Tt*exp(lp(0)), Tt*exp(lp(1)))            # own exposure
G_one <- cbind(exp(lp(0)),    exp(lp(1)))               # exposure = 1
G_bar <- mean(Tt)*cbind(exp(lp(0)), exp(lp(1)))         # mean exposure
fit_bad <- MASS::glm.nb(Y ~ A + offset(os), data = d)   # X omitted: misspecified
lpb <- function(a) cbind(1,a) %*% coef(fit_bad)[1:2]
G_mis <- cbind(Tt*as.vector(exp(lpb(0))), Tt*as.vector(exp(lpb(1))))
```

| prediction function | $\hat\theta_0$ | $\hat\theta_1$ |
|---|---|---|
| TRUTH $E[Y(0)], E[Y(1)]$ | 1.7824 | 2.6590 |
| AIPW, own exposure | 1.7976 | 2.6595 |
| AIPW, exposure = 1 | 1.7980 | 2.6588 |
| AIPW, mean exposure | 1.7981 | 2.6587 |
| AIPW, **misspecified** model | 1.7982 | 2.6587 |
| plain g-comp, own exposure (no augmentation) | 1.7934 | 2.6636 |
| **current RobinCar2 (mismatched prediction/residual)** | **1.0293** | **1.5185** |

The crisp result is that the four AIPW variants agree to four decimals — they share the anchor
$\bar y_a$ (§3.1), so the comparison against `TRUTH` is limited by common sampling noise in the arm
means and should not be read as bias. The current code sits 42% low.

### 5.2 Treatment-dependent follow-up: rate ratio estimators (400 reps, n = 1500)

Data-generating mechanism — follow-up depends on both baseline severity $X$ and treatment:

```r
Tf  <- function(a) pmin(pmax(1.5 + 0.6*X + 1.0*a + rnorm(n, 0, .5), .1), 5)
lam <- function(a) exp(-0.4 + 0.3*a + 0.7*X)
Y   <- rnbinom(n, mu = Tob*lam(A), size = 2)
```

True marginal rates $\rho_0=1.0890$, $\rho_1=1.3495$, so **true marginal RR = 1.2392**. True
*conditional* rate ratio $e^{\beta}=1.3499$.

| estimator | mean | bias | % bias |
|---|---|---|---|
| (1) offset g-comp at own observed $T$ | 1.3626 | +0.1235 | +10.0% |
| (2) $e^{\hat\beta}$ from the offset model | 1.3587 | +0.1196 | +9.6% |
| (3) ratio of two AIPW marginal means | 1.2518 | +0.0126 | +1.0% |

Estimators (1) and (2) agree with each other and with the true *conditional* RR (1.3499), not the
marginal one — the collapse predicted in §4. Estimator (2) is not broken; it answers the conditional
question. Estimator (1) is an elaborate route to the same place.

### 5.3 Estimator (3) is consistent (200–400 reps per n)

| n | mean | bias | % bias | MC SE |
|---|---|---|---|---|
| 1,500 | 1.2421 | +0.0029 | +0.23% | 0.0043 |
| 6,000 | 1.2395 | +0.0003 | +0.02% | 0.0021 |
| 24,000 | 1.2402 | +0.0010 | +0.08% | 0.0015 |

Within Monte Carlo error at every n. The +1.0% in §5.2 did not replicate under a different seed and
should be read as seed noise, not systematic finite-sample bias.

---

## 6. Questions to put to Prof. Ye

Framed against specific results in the paper. Question 0 is the one that matters most: the package's
position depends on whether the absence documented in §0 is real.

0. **Prior question.** Is there *any* established method for a rate estimand under covariate
   adjustment in this line of work — in the papers cited in `DESCRIPTION`, in later work, or in
   preparation? We could not find one (§0, §3.6) and are proceeding on the assumption that none
   exists. If that assumption is wrong, questions 1 and 2 are moot and the answer supersedes §4.

1. **Estimand.** Assuming it does not: is the augment-then-contrast rate ratio
   $\big(\theta_{Y,1}/\theta_{T,1}\big)\big/\big(\theta_{Y,0}/\theta_{T,0}\big)$ — treating exposure
   time as a second response and applying (1) to each — the recommended marginal rate estimand under
   variable follow-up? The motivation is that an offset evaluated at each subject's own $T_i$ makes
   $\hat\mu_a$ depend on a variable whose distribution *is* affected by treatment, which contradicts
   the justification given for (1) in §2, and in the multiplicative case collapses the ratio contrast
   to $e^{\beta}$ (§4, §5.2).

2. **Variance.** Theorem 1 is stated for a scalar response with $\theta\in\mathbb{R}^k$. For a
   stacked $(Y,T)$ response, does the $(\Omega_{SR}-\Omega(Z))$ term apply blockwise to the stacked
   influence contributions, or does the cross-block covariance require separate treatment under CAR?

3. **Secondary, worth raising.** Given §3.3/Example 1 — that a misspecified nonlinear working model
   loses guaranteed efficiency gain over $\bar Y$, which is precisely the NB rate-model case — should
   joint calibration (§4, Theorem 5) be considered a prerequisite for recommending `robin_glm` with
   negative binomial families at all? It is currently absent from RobinCar2.

---

## 7. Options for the package

**Decided — option 4, implemented on branch `117-reject-offsets`. See §9 for the decision record.**

| # | Option | Verdict |
|---|---|---|
| 1 | Respect the observed offset in g-computation (issue suggestion 2) | Valid in Case A only; collapses the ratio contrast to $e^\beta$. **Reject.** |
| 2 | Reference exposure `offset_value` (issue suggestion 1) | Arithmetically valid in **both** regimes provided residuals are matched — but no literature behind it, and the argument's presence invites option 1's usage. **Reject for 0.2.4.** |
| 3 | Rate estimand as ratio of two AIPW marginal means | Our own proposal (§4), not established methodology. Needs a spec + §4 variance + §6 q1/q2 answered before it is even a candidate. |
| 4 | Error when an offset is present | **Recommended.** The only option whose correctness does not depend on unpublished methodology. |
| 5 | Implement joint calibration | Pre-existing gap surfaced by §3.5. Independent of offsets, but most relevant precisely for NB. |

**Decision for 0.2.4: option 4 alone — error when an offset is present, with no opt-in.** The
current numbers are wrong under every reading and a warning would be swallowed in pipelines, so the
check must be an error. Rev. 2 of this note paired the error with a reference-exposure opt-in; that is
withdrawn. Shipping any offset pathway means shipping a rate method that Prof. Ye's work does not
establish (§0.0), and an `offset_value` argument reads to users as an endorsement that the theory does
not support. The error message should offer a substitute only where an exact one exists — the shifted
response under a gaussian identity link — and should otherwise say plainly that there is none, rather
than redirecting the user to a model that answers a different question.

Options 3 and 5 belong in their own design cycles, and option 3 is gated on §6 rather than on
engineering effort.

Note that "match the residual to the prediction function" remains the correct description of the
*algebraic* defect in §2 — it is just not something we act on, because the conclusion is to remove the
offset pathway rather than to repair it.

---

## 8. Caveats on this note

- §3 is now sourced from arXiv:2306.10213v2 with section/equation/theorem references. Quotations are
  short and marked. The other cited works in `DESCRIPTION` (Ye/Shao/Yi 2022 Biometrika; Ye/Shao/Yi/Zhao
  2023 JASA; Ye/Bannick/Yi/Shao 2023 STRF) were **not** read; if any treats exposure time, it would
  supersede §0's "no treatment of exposure time" conclusion for this package's literature as a whole.
- §4 in its entirety is our own construction, not established methodology (§0.0). Its variance has not
  been implemented or numerically validated; only the point estimators in §5 were checked. §5.3 shows
  the point estimator is consistent under one data-generating mechanism, which is not the same as the
  estimand being the right one or the inference being valid.
- The `schema = "ps"` claim in §2 (per-stratum residual means not forced to zero, so bug 2 is worse
  there) was reasoned from `bias()` and not separately simulated.
- The `vcovG()`-to-Theorem-1(ii) mapping in §3.4 was done by inspection, not by numerical comparison
  against an independent implementation.

---

## 9. Decision record

**Date:** 2026-09-17. **Branch:** `117-reject-offsets`. **Status:** implemented, tests passing.

### Decision

Reject offsets outright. Do not attempt a rate estimand until §6 q0/q1/q2 are answered.

The decision rests on two things that are logically independent, and both must be understood, because
fixing only the first would produce a package that computes something defensible-looking with no
methodology behind it:

1. **The algebraic defect (§2).** Offset models return numbers today, and those numbers estimate
   nothing — the offset is dropped from the prediction but retained in the residual. This is fixable.
2. **The methodological gap (§0.0, §0, §3.6).** Offsets exist almost solely to express a rate per unit
   exposure, and no covariate-adjusted rate estimand is established in Prof. Ye's work. This is not
   fixable by us in this cycle.

Because (2) holds, repairing (1) is not the right move: it would ship a rate method we authored,
presented to users as covariate adjustment theory. So the offset pathway is removed rather than
repaired.

### What was implemented

| item | location |
|---|---|
| The guard | [`R/predict_couterfactual.R`](../../R/predict_couterfactual.R), first statement of `predict_counterfactual.lm` |
| Unit tests, both supply routes × `lm`/`glm`/`glm.nb`, plus no-offset regression guard | `tests/testthat/test-predict_counterfactual.R` |
| Tests pinning which of the two messages each link gets, incl. Poisson identity link | `tests/testthat/test-predict_counterfactual.R` |
| End-to-end tests | `tests/testthat/test-robin_glm.R`, `tests/testthat/test-robin_lm.R` |
| User-facing rationale | `NEWS.md`, under **Breaking Changes** |

**One check covers every entry point.** `predict_counterfactual.lm` is the single implementation;
`predict_counterfactual.glm` delegates to it and `glm.nb` inherits from `glm`. `robin_glm`, `robin_lm`
and both `treatment_effect` routes all pass through it. Verified by execution, not inspection. The
survival code paths do not use it and are untouched.

**The message branches on the link, not the class.** Two messages, because the two situations are
mathematically different and a single message would have to be wrong about one of them:

- **Gaussian identity link** — the offset is a known additive shift, so the message gives the exact
  restatement (fit $y - z$) and never mentions rates, which are irrelevant here. Verified: coefficients
  and standard errors are bit-identical (max abs. difference 0), treatment contrasts are bit-identical,
  and marginal means differ by exactly $\overline{z}$.
- **Any other link** — the message states that prediction requires choosing an offset value, that the
  implied estimand where the offset is exposure time is a rate, that none is established, and
  explicitly that a free coefficient is *not* a substitute.

The condition is gaussian **and** identity, not identity alone: a Poisson or NB fit with an identity
link is legal, and there $y - z$ need not lie in the response support, so the shift advice would be
wrong. `family(fit)$link` and `$family` are available on plain `lm` too, so no class dispatch is needed.
A test pins the Poisson-identity case to the generic message.

Neither message points at this design note, because `design` is in `.Rbuildignore` and installed users
would not have it.

**Detection is `!is.null(fit$offset)`.** Established empirically across the six offset cases
(`offset()` in formula and `offset =` argument × `lm`/`glm`/`glm.nb`) and the three no-offset cases.
Neither `attr(terms(fit), "offset")` nor `fit$call$offset` alone is sufficient — the formula route sets
the terms attribute and leaves the call empty, the argument route does the reverse.

**The guard is family-agnostic, by design.** Issue #117 reports this against `glm.nb`, but both defects
live in the shared prediction method, so `lm` and Poisson with an offset are wrong in exactly the same
way. Restricting the guard to negative binomial would leave the same wrong numbers reachable.

**An all-zero offset is also rejected.** It is algebraically harmless, but special-casing it adds a
branch for a degenerate input in exchange for nothing.

### Consequences accepted

- **Breaking change.** Users with offset models get an error where they previously got output. This is
  intended: the previous output was wrong (§5.1 puts it 42% low), and a warning would be swallowed in
  pipelines. Flagged under **Breaking Changes** in `NEWS.md`, and to be stated plainly in the #117
  reply rather than left for users to discover — the reporter is one of the affected users.
- **No migration path to a rate, and we say so instead of offering one.** Under a non-identity link
  there is no substitute model, and the error message states that rather than redirecting the user.
  A free coefficient on $\log T$ is explicitly named as *not* a substitute, because it fits a different
  model and estimates a marginal mean of the count. Case A users can still recover absolute rates as
  $\hat\theta_a/\bar T$ by hand (§4). Gaussian identity-link users lose nothing: their restatement is
  exact.
- **The error message is longer than is conventional in R.** Accepted, because the reason for rejection
  is the part users need and a terse message would read as an unimplemented feature rather than a
  deliberate refusal.

### What this decision does not settle

Deliberately left open, each gated on §6 rather than on engineering effort:

- **Option 3 (rate estimand, §4).** Gated on §6 q0 first — whether any established method exists — then
  q1 (is the augment-then-contrast rate ratio the right target) and q2 (stacked-response variance under
  CAR). If q0 comes back positive, §4 is superseded wholesale.
- **Option 5 (joint calibration, §3.5).** Independent of offsets and pre-existing, but most relevant
  precisely for the NB models that motivated #117.
- **Reference-exposure opt-in (option 2).** Withdrawn, not deferred. Revisit only if a rate estimand is
  established, at which point it would be reconsidered as part of that design rather than on its own.
