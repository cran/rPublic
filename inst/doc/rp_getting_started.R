## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

# step 1 - Paste your secret key into line 25. Afterwards, run the block to save them as 'rp_tokens.rds'. 
rp = new.env()
assign('secret_key','PASTE_YOUR_SECRET_KEY_HERE', envir = rp)
assign('access_token', NULL, envir = rp)
assign('expiresAt', Sys.time(), envir = rp)
saveRDS(rp, "rp_tokens.rds")

## ----include = FALSE----------------------------------------------------------
# 
# # step 2 - use the secret key to exchange for an access token
# rp_getAccToken(exp_in_mins = 120)
# 

## ----include = FALSE----------------------------------------------------------
# 
# # step 3: making your first request
# my_acc = rp_getAccts()
# 

