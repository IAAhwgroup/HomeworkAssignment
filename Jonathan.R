load('LeukError.RData')

table(leuk$V5001)

class(leuk)

sum(!is.na(leuk$V5001))

# there are no duplicated nor NA variables
sum(duplicated(leuk))

# no na in data
any(is.na(leuk))
sum(is.na(leuk))

str(leuk$V5001)




