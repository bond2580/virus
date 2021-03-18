
plot.spectrum <- function(dat, lab="", main="", col="",y_max=1, tick = c(7,4), unit=1){
  
  #データの周波数領域変換
  dat_FFT <- abs(fft(as.vector(dat)))
  
  #グラフの横軸(周波数)の表示に関する準備
  data_len <- length(dat_FFT)
  freq_tick<- c(data_len, tick, 2)
  
  #周波数領域でのデータをプロット
  plot(dat_FFT/max(dat_FFT), type="l", main=main, 
       ylab = "|規格化された周波数スペクトル|", ylim = c(0, 0.20),
       xlab = sprintf("周波数[1/%s]", lab), xlim = c(1, data_len/2), xaxt = "n", col=col, lwd=2.0, cex.lab=1.25, cex.axis=1.5)
  axis(side=1, at = data_len/freq_tick * unit + 1,
       labels = sprintf("1/%d", freq_tick), cex.axis = 1.5, lty="dotted", tck=1.0)
  axis(side=2, cex.axis=1.5,lty="dotted", tck=1.0)
  
  returnValue(dat_FFT/max(dat_FFT))
  
}

points.spectrum <- function(dat, lab="", main="",col="", y_max=1, tick = c(7,4), unit=1){
  #データの周波数領域変換
  dat_FFT <- abs(fft(as.vector(dat)))
  
  #グラフの横軸(周波数)の表示に関する準備
  data_len <- length(dat_FFT)
  freq_tick<- c(data_len, tick, 2)
  
  #周波数領域でのデータをプロット
  points(dat_FFT/max(dat_FFT), type="l", main=main, 
       ylab = "|規格化された周波数スペクトル|", ylim = c(0, 0.20),
       xlab = sprintf("周波数[1/%s]", lab), xlim = c(1, data_len/2), xaxt = "n", col=col, lwd=2.0, cex.lab=1.25, cex.axis=1.5)
  axis(side=1, at = data_len/freq_tick * unit + 1,
       labels = sprintf("1/%d", freq_tick), cex.axis = 1.5, lty="dotted", tck=1.0)
  axis(side=2, cex.axis=1.5,lty="dotted", tck=1.0)
  
  returnValue(dat_FFT/max(dat_FFT))

  
}


