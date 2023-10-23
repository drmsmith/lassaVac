outbreak_res = rnorm(30,1,1) %>% sort 
outbreak_res = ifelse(outbreak_res<0,0,outbreak_res)
plot(outbreak_res)


outresfun = function(.data=1:30) outbreak_res[.data]

cum_U = integrate(outresfun,1,30, subdivisions = 10e10, 
                  rel.tol = .Machine$double.eps^.05)


outbreak_res[1:30]


sum(outbreak_res)

pracma::trapz(1:30,outbreak_res)
MESS::auc(seq_along(outbreak_res),outbreak_res,type='spline')


# BENCHMARKING 

params_inc_per_capita['country']

benchmark(
    "df[]" = {
        params_inc_per_capita['country']
    },
    "df$" = {
        params_inc_per_capita$country
    },
    "list" = {
        out[[1]]$df_out$country
    },
    replications = 10000,
    columns = c("test",
                "replications",
                "elapsed",
                "relative",
                "user.self",
                "sys.self"))


mat = matrix(NA, ncol = 3, nrow=3)

rownames(mat) = c('a', 'b', 'c')
colnames(mat) = c('a1', 'b1', 'c1')
mat_l = map(1:3, function(.x) mat[.x, ])
names(mat_l) = c('a', 'b', 'c')

benchmark(
    "mat['a']" = {
        mat['a', ]
    },
    "mat_l$a" = {
        mat_l$a
    },
    replications = 100000,
    columns = c("test",
                "replications",
                "elapsed",
                "relative",
                "user.self",
                "sys.self"))





