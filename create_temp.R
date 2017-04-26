
start_year = 1970
end_year = 1989


# create temp input

juv_tmp <- get_temperature_data(start_year, end_year)
juv_tmp_delta <- get_temperature_data(start_year, end_year, 'delta')
