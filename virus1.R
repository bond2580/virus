#SEIRモデルの過去の最適パラメータを推測し
#それらの周波数スペクトルから周期性ｗ確認する

library(xts)
library(deSolve)
library(tictoc)

start <- 139 #139は2020年7月1日を表すインデックスに相当
span <- 28 #過去のデータの予測における予測期間
end <- 419-span #419は2021年4月7日を表すインデックスに相当


covid <- read.csv("COVID-19.csv")
tic() #処理時間計測
ans <- sim(start, end, span) #過去の最適パラメータを予測
toc() #処理時間表示
dates <- seq(as.Date("2020-07-01"), length=nrow(ans), by="days")
ans_xts <- as.xts(ans[, -1], order.by = dates)

png("parameter.png", width = 959, height = 605) #過去の最適パラメータをプロット
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2),par(mar=c(0,0,0,0)),par(mar=c(4,4,3,1))) #プロットを四分割
plot(ans_xts$R0, main="R0",ylim=c(0, 3.5),xlab="days",col="red", lwd=2.0)
plot(ans_xts$e, main="e",ylim=c(4, 14),xlab="days",col="darkgreen", lwd=2.0)
plot(ans_xts$l, main="l",ylim=c(6, 16),xlab="days",col="blue", lwd=2.0)
par(oldpar)
dev.off()


png("spectrum.png", width = 959, height = 605) #過去の最適パラメータの周波数スペクトルをプロット
oldpar <- par(no.readonly = TRUE)
par(mfrow = c(2,2),par(mar=c(0,0,0,0)),par(mar=c(4,4,3,1)))
plot.spectrum(ans_xts$R0, lab="days", main="R0", col="red")
plot.spectrum(ans_xts$e, lab="days", main="e", col="darkgreen")
plot.spectrum(ans_xts$l, lab="days", main="l", col="blue")
par(oldpar)
dev.off()


