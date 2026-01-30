power.prop.test(
  p1 = 0.15,
  p2 = 0.09,
  sig.level = 0.05,
  power = 0.8,
  alternative = "two.sided"
)

n=459
attrition=0.2
n_inflated = n/(1-attrition)
