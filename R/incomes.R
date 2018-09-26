
ds10_income = c(1.6e5,
                1.65e5,
                1.7e5,
                1.75e5,
                1.8e5,
                1.85e5,
                1.9e5,
                1.95e5,
                2e5,
                2e5) %>% as.integer()

ds12_income = c(ds10_income, 1.6e5, 1.6e5) %>% as.integer()

doc10_income = c(.6e5,
                 .62e5,
                 .64e5,
                 .8e5,
                 2.4e5,
                 2.6e5,
                 2.8e5,
                 2.8e5,
                 2.8e5,
                 2.8e5) %>% as.integer()

doc12_income = c(doc10_income, 2.8e5, 2.8e5) %>% as.integer()

age_disc_income = ds10_income
age_disc_income[7:10] = as.integer(.8e5)

ds10 = data.frame(year = 1:10,
                  eric_income = ds10_income,
                  page_income = ds10_income)

ds12 = data.frame(year = 1:12,
                  eric_income = ds12_income,
                  page_income = ds12_income)

doc10 = data.frame(year = 1:10,
                   eric_income = doc10_income,
                   page_income = ds10_income)

doc12 = data.frame(year = 1:12,
                   eric_income = doc12_income,
                   page_income = ds12_income)

age_disc = data.frame(year = 1:10,
                      eric_income = age_disc_income,
                      page_income = age_disc_income)
