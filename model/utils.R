library('magick')

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


# make gifs of saved plots 
# gifs saved and returned by default 
gif_maker = function(
    vec_img_paths,  # vector of full paths to desired images in correct order
    file_name = 'plot_gif', # how to name the gif
    dest_dir = getwd(),     # directory to save gif in 
    .fps=2) {               # frames per second (multiple of 100)
    # read all images into a list  
    img_list <- lapply(vec_img_paths, image_read)
    # join images 
    img_joined <- image_join(img_list)
    # animate at 2 frames per second by default
    img_animated <- image_animate(img_joined, fps = .fps)
    ## save to disk
    img_path = paste0(dest_dir, '/', file_name, '.gif')
    image_write(image = img_animated, path = img_path)
    return(img_animated)
}


