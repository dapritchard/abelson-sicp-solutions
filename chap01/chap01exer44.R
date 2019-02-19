# smoothing functions ----------------------------------------------------------

compose <- function(f, g) {
  function(x) f(g(x))
}

repeated <- function(f, n) {
  `if`(n == 1L,
       f,
       compose(f, repeated(f, n - 1L)))
}

smooth <- function(f, k, dx) {
  function(x) rowMeans(sapply(-k:k, function(y) f(x + y*dx)))
}

n_smooth <- function(f, n, k, dx) {
  repeated(smooth(f, k, dx), n)
}




# create function to experiment with -------------------------------------------

poly_f <- function(x) {
  # (sin(x)) + (5 * sin((x + 1) * 10)) + (2 * sin((x + 3) * 4))
  sin(x) + (0.3 * cos(50 * x)) + (0.1 * cos(11 * x)) + (0.1 * sin(7 * x))
}

plot_smoothing <- function(n, k, dx) {
  x <- seq(-pi, pi, 0.001)
  smooth_poly <- n_smooth(poly_f, n, k, dx)
  curve(smooth_poly,
        from = -pi,
        to   = pi,
        n    = 1001,
        ylab = paste0(n, " smoothing", `if`(n == 1L, "", "s")),
        ylim = c(-1, 1),
        bg = "gray")
  lines(x, sin(x), col = "blue")
}




# create plots -----------------------------------------------------------------

default_par <- par()
x <- seq(-pi, pi, 0.001)

pdf("chap01/figures/sin-f.pdf", width = 7, height = 5, bg = "grey97")
par(las = 1, mar = c(5, 5, 0.5, 0.1))
curve(poly_f, from = -pi, to = pi, ylab = "f(x)")
lines(x, sin(x), col = "blue")
dev.off()

pdf("chap01/figures/smoothed-sin.pdf", width = 7, height = 7, bg = "grey97")
par(las = 1, mar = c(3, 5, 0.5, 0.1), mfrow = c(2L, 2L))
plot_smoothing(1, 7, 0.05)
plot_smoothing(2, 7, 0.05)
plot_smoothing(3, 7, 0.05)
plot_smoothing(35, 7, 0.05)
dev.off()

svg("chap01/figures/sin-f.svg", width = 7, height = 5, bg = "grey97")
par(las = 1, mar = c(5, 5, 0.5, 0.1))
curve(poly_f, from = -pi, to = pi, ylab = "f(x)")
lines(x, sin(x), col = "blue")
dev.off()

svg("chap01/figures/smoothed-sin.svg", width = 7, height = 7, bg = "grey97")
par(las = 1, mar = c(3, 5, 0.5, 0.1), mfrow = c(2L, 2L))
plot_smoothing(1, 7, 0.05)
plot_smoothing(2, 7, 0.05)
plot_smoothing(3, 7, 0.05)
plot_smoothing(35, 7, 0.05)
dev.off()
