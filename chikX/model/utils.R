# hyperbolic funciton for outbreak shape 
# sinh curve used to simulate outbreak 
shin_curve = function(xs=xdates,
                      amplitude=params[1],
                      h_transl=xdates[which(ycases == max(ycases))],
                      s_l=params[2],
                      s_r=params[3]){
    # simulate curve 
    ys =  amplitude*2 / ( exp((xs-h_transl)*s_l) + exp((-xs+h_transl)*s_r) )
    # add noise 
    # ys = ys+rnorm(length(ys), mean(ys), mean(ys)*0.05)
    # # filter out <0
    # ys = ifelse(ys<0, 0, ys)
    return(ys)
}
