# Simple test of nominal_test to understand its behavior

library(ordinal)

# Create simple test data
set.seed(123)
n <- 200
df <- data.frame(
  y = ordered(sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1))),
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = factor(sample(c("A", "B", "C"), n, replace = TRUE))
)

# Fit clm model
m1 <- clm(y ~ x1 + x2 + x3, data = df)

cat("Model summary:\n")
print(summary(m1))

cat("\n\nRunning nominal_test with default arguments:\n")
nt1 <- nominal_test(m1)
print(nt1)

cat("\n\nTrying with scope argument:\n")
nt2 <- nominal_test(m1, scope = ~ x1 + x2 + x3)
print(nt2)

cat("\n\nChecking if values are NA:\n")
cat("Row names:", rownames(nt1), "\n")
cat("LRT values:", nt1$LRT, "\n")
cat("P-values:", nt1$`Pr(>Chi)`, "\n")
