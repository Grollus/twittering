library(ROAuth)
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "qKAAAGok2VJeRVMreQMfv4Obl"
consumerSecret <- "xfBDtr3tAqJ6K6GhVoNuSPBP5vxqIb8qr9i6DAgXqgJLUpxt2c"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret,
                             requestURL = requestURL, accessURL = accessURL,
                             authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

save(my_oauth, file = "my_oauth.Rdata")
