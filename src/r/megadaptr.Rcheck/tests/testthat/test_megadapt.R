context('util')

library(megadaptr)
library(dplyr)
test_that('apply_data_changes updates original data with changed columns', {
  df <- data.frame(x=1:5, y=6:10, z=11:15, pk=1:5)
  changes <- data.frame(x=5:1, z=1:5, pk=c(1,3,2,4,5))

  new_df <- apply_data_changes(df, changes, join_columns=c('pk'='pk'))
  comp_df <- df %>%
    dplyr::select(-x, -z) %>%
    dplyr::inner_join(changes, by=c("pk"="pk")) %>%
    dplyr::arrange(pk)
  expect_equal(new_df$x, comp_df$x)
  expect_equal(new_df$z, comp_df$z)
  expect_equal(new_df$pk, comp_df$pk)
})
