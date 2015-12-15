#freq_domain
load(file = '~/Github/Stat153FinalProject_WestCoastHousing/west_homes_ts.RData')

spec.pgram(west, spans = c(13,11))$kernel

##Spectrographs
par(mfrow = c(1,1))

nobs = length(west) # number of observations 
seg_size = 24
cut_west = west[-(1:(nobs%%seg_size))]
nseg = floor(nobs/seg_size) - 1 # size of nseg unique sections
wsize = seg_size*2 # window size = next segment and overlap of previous section
west.spec = matrix(0, wsize, nseg)
for (k in 1:nseg) {
  a = seg_size*(k-1)+1
  b = wsize+seg_size*(k-1)
  west.spec[,k] = spec.pgram(cut_west[a:b], 
                             spans = c(13,11))$spec} #spans 6 months on both sides
t = seq(1973 + 10/12, 
        1973 + 10/12 + (seg_size*nseg/12), 
        len = ncol(west.spec))
f = seq(0, 6,  len = nrow(west.spec)/2)
z = t(west.spec[1:(nrow(west.spec)/2),])


filled.contour(t, f, log(z), ylab="frequency", xlab="time\n(months from October 1973)",
               color.palette = function(x)rev(heat.colors(x)),
               main="West Coast House Sales,\nlog(spectral densities) estimated via\n
                    smoothed periodogram w/ span 13",
               cex.main = .9)

persp(t, f, z, zlab="Power", xlab="frequency", ylab="time",
      ticktype="detailed", theta=25,d=2, main="House Sales")


save.image(file = '~/Github/Stat153FinalProject_WestCoastHousing/west_homes_ts.RData')

