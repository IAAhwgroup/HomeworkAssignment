load('LeukError.RData')

table(leuk$V5001)

class(leuk)

# there are 38 labeled data not matching the email 37 
sum(!is.na(leuk$V5001))

# and they are not duplicated
sum(duplicated(leuk))
