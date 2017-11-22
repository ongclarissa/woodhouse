devtools::document()
devtools::load_all()

df = data.frame(
  x = rep(c(0, 1, 2, 3, 4, 5, 6), 6),
  y = rep(c(0, 1, 2, 3, 4, 5), 7)
)


ladf$x_r = reverse(df$x)
df$y_r = reverse(df$y, highest = 5)


df$x2 = minus(df$x, 2)
df$x1 = add(df$x, 4)

df$x3 = recode(df$x, 2, 77)

df$x1 = dichotomize(df$x, 4, 1, 0)

df$x2 = categorize(df$x, 3, 5, 0, 1, 2)
