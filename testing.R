devtools::document()
devtools::load_all()
devtools::load_all("~/Box Sync/Resources/Statistics/R/woodhouse")

df = data.frame(
  x = rep(c(0, 1, 2, 3, 4, 5, 6), 6),
  y = rep(c(0, 1, 2, 3, 4, 5), 7),
  z = rnorm(42)
)

ivs = df[, c(1, 3)]
scatterplots(df, y, ivs)

reverse = function(x, highest = 6) {
  x_rev = highest - x
  return(x_rev)
}

df = df %>%
  mutate(x1 = reverse(x))

df$x_r = reverse(df$x)
df$y_r = reverse(df$y, highest = 5)


df$x2 = minus(df$x, 2)
df$x1 = add(df$x, 4)

df$x3 = recode(df$x, 2, 77)

df$x1 = dichotomize(df$x, 4, 1, 0)

df$x2 = categorize(df$x, 3, 5, 0, 1, 2)

# didn't work
histogram = function(data, bins = 1){
  x = c(1:(length(data)))

  data[, x] = lapply(data[, x], as.numeric)

  labels = names(data)
  label = labels[[i]]

  for(i in x){
    print(qplot(data[, i],
                geom = "histogram",
                binwidth = bins,
                xlab = label))
  }
}
