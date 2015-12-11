
box_smoother = function(L, ts = west){#x is time series, L is box length (*must be odd number)
  if (L%%2 == 0) stop('L must be an odd integer')
  mvspec(ts, kernel = kernel('daniell', (L-1)/2))
}
box_smoother(13, west_d1)
mvspec(log(west), span = 6)


