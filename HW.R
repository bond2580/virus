

#ホルトウィンタース法にによるパラメータ推測モデル
HR0 <- HoltWinters(tmp$R0,
                   #gamma=FALSE
) #周期性が確認できなかったため, gammma=FALSEとする. 
He <- HoltWinters(tmp$e)
Hl <- HoltWinters(tmp$l)

#12月28時点の最適パラメータを推測
HR0 <- predict(HR0, n.ahead=span+1)
He <- predict(He, n.ahead=span+1)
Hl <- predict(Hl, n.ahead=span+1)