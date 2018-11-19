type <- c(rep('a', 100), rep('b', 100), rep('c', 100))

matrix_a <- matrix(rnorm(500, 0, 1), nrow=100)
matrix_b <- matrix(rnorm(500, 1, 2), nrow=100)
matrix_c <- matrix(rnorm(500, 2, 3), nrow=100)

mydata <- data.frame(type=type, rbind(matrix_a, matrix_b, matrix_c))

mean_df <- function(data){
  # data <- as.data.frame(data)
  data <- select(data, -type)
  return(mean(as.matrix(data)))
}

dist_df <- function(data){
  # browser()
  # data <- as.data.frame(data)
  data <- select(data, -type)
  distdata <- t(data)
  return(mean(dist(distdata)))
}

testdata <- mydata %>% filter(type == 'c') %>% select(-type)

mean(as.matrix(testdata))
mean(dist(t(testdata)))

testdata <- mydata %>% filter(type == 'c')

mean_df(testdata)
dist_df(testdata)


mydata %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(
    mean = mean_df(.),
    dist = dist_df(.)
  )

x <- mydata %>%
  dplyr::group_by(type) %>%
  dplyr::do(
    data.frame(mean = mean_df(.),
    dist = dist_df(.))
  ) %>%
  ungroup()

x