
ds10_income = c(1.3e5,
                1.35e5,
                1.4e5,
                1.45e5,
                1.5e5,
                1.6e5,
                1.6e5,
                1.6e5,
                1.6e5,
                1.6e5)

ds12_income = c(ds10_income, 1.6e5, 1.6e5)

doc10_income = c(.6e5,
                 .62e5,
                 .64e5,
                 .8e5,
                 2.4e5,
                 2.6e5,
                 2.8e5,
                 2.8e5,
                 2.8e5,
                 2.8e5)

doc12_income = c(doc10_income, 2.8e5, 2.8e5)

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
