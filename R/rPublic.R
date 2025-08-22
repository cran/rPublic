require("httr");require("jsonlite");require("data.table");require("lubridate");require("stringr");require("uuid")
# ***************************************************************************
#                             Helper Functions
# ***************************************************************************

#' temporary working environment
#'@return An auto generated \code{environment} to store our tokens
#'@examples
#' \dontrun{
#'   .rp_env <- new.env(parent = emptyenv())
#' }
#'@export
.rp_env <- new.env(parent = emptyenv())

#' Order ID
#'@return An auto generated \code{character} string to use for placing orders
#'@examples
#' \dontrun{
#'   rp_getOrderId()
#' }
#'@export
rp_getOrderId = function (){
  id <- uuid::UUIDgenerate()
  id
}

#' Get New Access/Bearer Token From Secret Key
#'@return Update Bearer Token from secret key & returns working \code{environment} and saves updated tokens in 'rp_tokens.rds'
#'@param exp_in_mins = (int) The number of minutes that the bearer token will be valid for.
#'@examples
#' \dontrun{
#'   # Request New Bearer Token that expires in 120 minutes
#'   rp_getAccToken(exp_in_mins=120)
#' }
#'@export
rp_getAccToken = function(exp_in_mins){
  # assign token envir
  .rp_read_tokens()
  # assign body/payload
  dta = list('validityInMinutes' = exp_in_mins, 'secret' = .rp_env$rp$secret_key)
  # post request
  resp = httr::POST(url = 'https://api.public.com/userapiauthservice/personal/access-tokens',
                    httr::add_headers(`Content-Type` = 'application/json'),
                    body = jsonlite::toJSON(dta, pretty = T, auto_unbox = T)
  )
  req_time = Sys.time()
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract access token
    access_token = httr::content(resp)
    # assign access token
    assign('access_token', access_token$accessToken, envir = .rp_env$rp)
    assign('expiresAt', req_time+lubridate::minutes(exp_in_mins), envir = .rp_env$rp)
    saveRDS(.rp_env$rp, "rp_tokens.rds")
  }else{
    # get error code
    err = httr::content(resp)
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
}

#' Request token file (Internal)
#'@return Requests your token file 'rp_tokens.rds' from working directory & assigns a working \code{environment} if it exists
#'@examples
#' \dontrun{
#'   # For Internal Use (assigns tokens inside of the 'rp' environment)
#'   .rp_read_tokens()
#' }
#'@export
.rp_read_tokens <- function() {
  if (file.exists("rp_tokens.rds")) {
    .rp_env$rp <- readRDS("rp_tokens.rds")
  } else {
    stop("'rp_tokens.rds' file not found!")
  }
}

#' Check & Auto-Renew Bearer Tokens (Internal)
#'@return Checks validity of Bearer Token & auto-updates if needed. Assigns the new tokens in 'rp' \code{environment} and 'rp_tokens.rds' file
#'@param printMsg = (bool) Should outcome messages be printed out? defaults to FALSE
#'@param     mins = (int) The number of minutes that the bearer token will be valid for.
#'@examples
#' \dontrun{
#'   # For Internal Use Prior to Making API Requests
#'   .rp_checkAccessToken(printMsg=FALSE, mins=120)
#' }
#'@export
.rp_checkAccessToken = function(printMsg=FALSE, mins=120){
  # check if tokens are saved
  if(file.exists("rp_tokens.rds")){
    # read in token file if it exists
    tokens = readRDS("rp_tokens.rds")
    # check if it needs renewal
    getNewAccTkn = (as.POSIXct(tokens$expiresAt,tz = Sys.timezone()) < Sys.time())
  }
  # if token file does not exist then notify
  if(!file.exists('rp_tokens.rds')){
    getNewAccTkn = NULL
  }
  # *************************************************************************
  #                     Renew only if getNewAccTkn = TRUE 
  # *************************************************************************
  if(getNewAccTkn){  
    # get new access tokens... will save them in the RDS file and also in rp environment
    rp_getAccToken(exp_in_mins = mins)
    # print if output is enabled
    if(printMsg){
      message("\n",sprintf("%-30s : %-20s", 
                           paste0("Renewed ",mins,"-minute Access Token"),
                           "File Saved"))}
  }
  # *************************************************************************
  #  If getNewAccTkn, We don't have a file & need to generate one (manually)
  # *************************************************************************
  if(is.null(getNewAccTkn)){
    if(printMsg){
      warning("\n",sprintf("%-30s : %-20s", "Error", 
                           "Token File Does Not Exist"))
    }}
  # *************************************************************************
  #  If Access Token not expired, notify
  # *************************************************************************
  if(!getNewAccTkn){
    if(printMsg){
      cat("\n",message("%-30s : %-20s", "Access/Refresh Token Valid",
                       "No Action Required"))
    }}
}

#' Build Dynamic Payload For rp_getQuote (Internal)
#'@return Returns a \code{list} in the appropriate payload format in case the user needs multiple symbols for quotes
#'@param   symbols = (string) Equity/ETF/Option symbol(s)
#'@param     types = (string) The product type (ex. 'EQUITY' or 'OPTION')
#'@examples
#' \dontrun{
#'   # Create the correct quote payload for AAPL and a SPY 631 Call 8/15/25 Expiration
#'   .rp_make_qte_payload(symbols=c("AAPL","SPY250815C00631000"), types=c("EQUITY","OPTION"))
#' }
#'@export
.rp_make_qte_payload <- function(symbols, types) {
  # Ensure symbols and types have the same length
  if (length(symbols) != length(types)) {
    stop("The number of symbols and types must match.")
  }
  
  # Create the instruments list dynamically
  instruments <- lapply(seq_along(symbols), function(i) {
    list(symbol = symbols[i], type = types[i])
  })
  
  # Construct the payload
  payload <- list(instruments = instruments)
  
  return(payload)
}

#' Build Option Symbol (Internal)
#'@return Returns a valid symbol \code{string} for the option contract of interest
#'@param   under_sym = (string) Underlying symbol for the option: ex. 'SPY'
#'@param         exp = (string) The option expiration: ex. "2025-08-15"
#'@param        type = (string) The option type: 'C' for Call & 'P' for Put
#'@param      strike = (double/int) The option strike price: 631 or 631.00
#'@examples
#' \dontrun{
#'   # Return the proper option symbol of interest: "TSLA250808C00325000"
#'   .rp_make_opt_symbol(under_sym="TSLA", exp="2025-08-08", type="C", strike=325)
#' }
#'@export
.rp_make_opt_symbol = function(under_sym, exp, type, strike){
  sym = gsub(" ", "", stringr::str_pad(under_sym, width=6, side = "right"))
  exp = format(as.Date(exp),"%y%m%d")
  strike = as.character(as.integer(strike*100))
  if(stringr::str_length(strike) == 7){strike = paste0(strike,"0")}
  if(stringr::str_length(strike) == 6){strike = paste0("0",strike,"0")}
  if(stringr::str_length(strike) == 5){strike = paste0("00",strike,"0")}
  if(stringr::str_length(strike) == 4){strike = paste0("000",strike,"0")}
  if(stringr::str_length(strike) == 3){strike = paste0("0000",strike,"0")}
  if(stringr::str_length(strike) == 2){strike = paste0("00000",strike,"0")}
  paste0(sym,exp,type,strike)
}

#' Build Single-Leg Order Payload (Internal)
#'@return Returns an appropriate payload \code{list} for a single-leg order
#'@param         ticker = (string) Ticker symbol: ex. 'SPY'
#'@param        symType = (string) Symbol type: ex. 'EQUITY'
#'@param        orderId = (string) The order ID 
#'@param           side = (string) The Order Side BUY/SELL. For Options also include the openCloseIndicator
#'@param        ordType = (string) The Type of order: 'MARKET', 'LIMIT', 'STOP', 'STOP_LIMIT'
#'@param    timeInForce = (string) The time in for the order: 'DAY' or 'GTD"
#'@param expirationTime = (string) The expiration date. Only used when timeInForce is GTD, cannot be more than 90 days in the future
#'@param            qty = (string) The order quantity. Used when buying/selling whole shares and when selling fractional. Mutually exclusive with amount
#'@param            amt = (string) The order amount. Used when buying/selling shares for a specific notional value
#'@param         lmtPrc = (string) The limit price. Used when orderType = LIMIT or orderType = STOP_LIMIT
#'@param        stopPrc = (string) The stop price. Used when orderType = STOP or orderType = STOP_LIMIT
#'@param openCloseIndicator = (string) Used for options only. Indicates if this is BUY to OPEN/CLOSE
#'@examples
#' \dontrun{
#'   # Return the proper order payload for single-leg orders
#'   .rp_make_ord_payload(under_sym="IWM", symType = "EQUITY", orderId = rp_getOrderId(), 
#'                        side="BUY", orderType="LIMIT", timeInForce="GTD", 
#'                        expirationTime ="2023-11-07T05:31:56Z", qty=1.735, lmtPrc="200.00")
#' }
#'@export
.rp_make_ord_payload <- function(ticker, symType, orderId = NULL, side = NULL, ordType = NULL, 
                                 timeInForce = NULL, expirationTime = NULL, qty = NULL, 
                                 amt = NULL, lmtPrc = NULL, stopPrc = NULL, 
                                 openCloseIndicator = NULL) {
  # Create the instrument sublist
  instrument_fields <- list(
    'symbol' = ticker,
    'type' = symType
  )
  
  expiration <- list(
    'timeInForce' = ifelse(is.null(timeInForce), "", timeInForce),
    'expirationTime' = ifelse(is.null(expirationTime), "", expirationTime)
  )  
  
  # Create the main order payload
  order_fields <- list(
    'orderId' = if(!is.null(orderId)) paste(orderId),
    'instrument' = instrument_fields,
    'orderSide' = side,
    'orderType' = ordType,
    'expiration' = expiration,
    'quantity' = qty,
    'amount' = amt,
    'limitPrice' = lmtPrc,
    'stopPrice' = stopPrc,
    'openCloseIndicator' = openCloseIndicator
  )
  
  # Filter out NULL fields from the main payload
  order_fields <- order_fields[!sapply(order_fields, is.null)]
  
  return(order_fields)
}


#' Build Multi-Leg Order Payload (Internal)
#'@return Returns an appropriate payload \code{list} for a multiple-leg order
#'@param     orderType = (string) The Type of order: 'MARKET', 'LIMIT', 'STOP', 'STOP_LIMIT'
#'@param           qty = (string) leg_ratio multiple: ex. '2' multiples the leg_ratios by 2X
#'@param       orderId = (string) The order ID 
#'@param   leg_symbols = (string) Symbols: ex. c("SPY250815C00631000", "SPY250815C00631000")
#'@param     leg_types = (string) Symbol types: ex. c("OPTION", "OPTION")
#'@param     leg_sides = (string) The side for each leg: ex. c("BUY", "SELL")
#'@param leg_indicator = (string) Indicates if this is BUY to OPEN/CLOSE ex. c("OPEN", "OPEN")
#'@param    leg_ratios = (string) The number of contracts to BUY/SELL: ex. c('5','5')
#'@param        lmtPrc = (string) The limit price. Used when orderType = LIMIT or orderType = STOP_LIMIT
#'@param           tif = (string) The time in for the order: 'DAY' or 'GTD"
#'@param       expTime = (string) The expiration date. Only used when timeInForce is GTD, cannot be more than 90 days in the future
#'@examples
#' \dontrun{
#'   # Return the proper order payload for multiple-leg orders
#'   .rp_make_multileg_payload(orderType="LIMIT", qty="2", orderId=rp_getOrderId(), 
#'                             leg_symbols = c("SPY250815C00631000", "SPY250815C00631000"), 
#'                             leg_types = c("OPTION", "OPTION"), leg_sides = c("BUY", "SELL"), 
#'                             leg_indicator = c("OPEN", "OPEN"), leg_ratios=c('5','5'), 
#'                             lmtPrc='0.25', tif="DAY")
#' }
#'@export
.rp_make_multileg_payload <- function(orderType, qty, orderId=NULL, leg_symbols, leg_types, leg_sides, leg_indicator, leg_ratios, lmtPrc=NULL, tif, expTime = NULL){
  
  # Create the instrument sublist
  instrument_fields <-  lapply(seq_along(leg_symbols), function(i) {
    # build legs
    list('instrument' = list('symbol' = leg_symbols[i], type= leg_types[i]), 
         'side' = leg_sides[i],
         'openCloseIndicator' = leg_indicator[i],
         'ratioQuantity' = paste(leg_ratios[i])
    )
  })
  
  
  expiration <- list(
    'timeInForce' = tif,
    'expirationTime' = ifelse(is.null(expTime), "", expTime)
  )  
  
  # Create the main order payload
  if(!is.null(orderId)){
    order_fields <- list(
      'orderId' = if(!is.null(orderId)) paste(orderId),
      'type' = orderType, 
      'expiration' = expiration,
      'quantity' = paste(qty),
      'limitPrice' = if(!is.null(lmtPrc)) paste(lmtPrc),
      'legs' = instrument_fields
    )
  }else{
    order_fields <- list(
      'orderId' = if(!is.null(orderId)) paste(orderId),
      'orderType' = orderType, 
      'expiration' = expiration,
      'quantity' = paste(qty),
      'limitPrice' = if(!is.null(lmtPrc)) paste(lmtPrc),
      'legs' = instrument_fields
    ) 
    
  }
  # Filter out NULL fields from the main payload
  order_fields <- order_fields[!sapply(order_fields, is.null)]
  
  return(order_fields)
}
# ***************************************************************************
#                             Account Details
# ***************************************************************************

#' Get Public Account Info
#'@return Returns a \code{data.frame} for the user's Public Brokerage Account
#'@examples
#' \dontrun{
#'   # Return Public Brokerage Account Information
#'     rp_getAccts()
#' }
#'@export
rp_getAccts = function(){
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::GET(url = 'https://api.public.com/userapigateway/trading/account',
                   httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                     `Content-Type` = 'application/json'
                   )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format
    df = data.frame(data.table::rbindlist(dta$accounts, use.names = T, fill = T))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}


#' Get Account Portfolio V2
#'@return Returns a \code{data.frame} for the user's specific Public Brokerage Account
#'@param accountId = Public Brokerage Account Number
#'@examples
#' \dontrun{
#'   # Return Public Brokerage Account Information
#'      my_acc <- rp_getAccts()
#'     my_port <- rp_getAcctsPort(accountId = my_acc$accountId)
#' }
#'@export
rp_getAcctsPort = function(accountId){
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::GET(url = paste0('https://api.public.com/userapigateway/trading/',accountId,'/portfolio/v2'),
                   httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                     `Content-Type` = 'application/json'
                   )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}

#' Get History
#'@return Fetches a paginated \code{data.frame} of historical events for the specified account.  
#'@param accountId = Public Brokerage Account Number
#'@param     start = (Optional) Start timestamp in ISO 8601 format with timezone. Ex. "YYYY-MM-DDTHH:MM:SSZ"
#'@param       end = (Optional) End timestamp in ISO 8601 format with timezone. Ex. "YYYY-MM-DDTHH:MM:SSZ"
#'@param  pageSize = (Optional) Maximum number of records to return.
#'@examples
#' \dontrun{
#'   # Return Public Brokerage Account History
#'     my_acc <- rp_getAccts()
#'     
#'     # using only accountId
#'     my_hist <- rp_getAccHist(accountId = my_acc$accountId)
#'     
#'     # using some parameters
#'     my_hist <- rp_getAccHist(accountId = my_acc$accountId, 
#'                              start = format(Sys.time()-days(30), format="%Y-%m-%dT%H:%M:%SZ"),
#'                              pageSize = 20
#'                              )
#' }
#'@export
rp_getAccHist = function(accountId, start=NULL, end=NULL, pageSize=NULL){
  # Base URL
  base_url <- paste0("https://api.public.com/userapigateway/trading/", accountId, "/history")
  
  # Initialize query parameters list
  query_params <- list()
  
  # Add optional parameters if provided
  if(!is.null(start)){
    query_params <- c(query_params, paste0("start=", start))
  }
  if(!is.null(end)){
    query_params <- c(query_params, paste0("end=", end))
  }
  if(!is.null(pageSize)){
    query_params <- c(query_params, paste0("pageSize=", pageSize))
  }
  
  # Combine base URL and query parameters
  if(length(query_params) > 0) {
    # Join parameters with "&" and prepend with "?"
    query_string <- paste0("?", paste(query_params, collapse = "&"))
    URL = paste0(base_url, query_string)
  }else{
    # Return base URL without query parameters
    URL = base_url
  }
  
  
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::GET(url = URL,
                   httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                     `Content-Type` = 'application/json'
                   )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = suppressWarnings(data.frame(data.table::rbindlist(dta$transactions, use.names = T, fill = T)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}

# ***************************************************************************
#                    Instrument Details
# ***************************************************************************

#' Get All Instruments
#'@return Retrieves all available trading instruments with optional filtering capabilities as a \code{data.frame}.
#'@param                typeFilter = (Optional) Ex. "BOND","EQUITY","CRYPTO","INDEX","ALT"   
#'@param             tradingFilter = (Optional) Ex. "BUY_AND_SELL","DISABLED","LIQUIDATION_ONLY"
#'@param   fractionalTradingFilter = (Optional) Ex. "DISABLED","BUY_AND_SELL","LIQUIDATION_ONLY"
#'@param       optionTradingFilter = (Optional) Ex. "DISABLED","BUY_AND_SELL","LIQUIDATION_ONLY"
#'@param optionSpreadTradingFilter = (Optional) Ex. "DISABLED","BUY_AND_SELL","LIQUIDATION_ONLY"
#'@examples
#' \dontrun{
#'   # Fetches All Instruments From Public
#'     all_inst <- rp_getAllInstruments()
#'     
#'     # Fetches All equities enabled for trading fractional shares
#'     all_frac <- rp_getAllInstruments(typeFilter = "EQUITY", 
#'                                      tradingFilter = 'BUY_AND_SELL',
#'                                      fractionalTradingFilter = 'BUY_AND_SELL')
#' }
#'@export
rp_getAllInstruments = function(typeFilter=NULL, tradingFilter=NULL, fractionalTradingFilter=NULL, optionTradingFilter=NULL, optionSpreadTradingFilter=NULL){
  
  # Base URL
  base_url <- paste0("https://api.public.com/userapigateway/trading/instruments")
  
  # Initialize query parameters list
  query_params <- list()
  
  # Add optional parameters if provided
  if(!is.null(typeFilter)){
    query_params <- c(query_params, paste0("typeFilter=", typeFilter))
  }
  if(!is.null(tradingFilter)){
    query_params <- c(query_params, paste0("tradingFilter=", tradingFilter))
  }
  if(!is.null(fractionalTradingFilter)){
    query_params <- c(query_params, paste0("fractionalTradingFilter=", fractionalTradingFilter))
  }
  if(!is.null(optionTradingFilter)){
    query_params <- c(query_params, paste0("optionTradingFilter=", optionTradingFilter))
  }
  if(!is.null(optionSpreadTradingFilter)){
    query_params <- c(query_params, paste0("optionSpreadTradingFilter=", optionSpreadTradingFilter))
  }
  
  # Combine base URL and query parameters
  if(length(query_params) > 0) {
    # Join parameters with "&" and prepend with "?"
    query_string <- paste0("?", paste(query_params, collapse = "&"))
    URL = paste0(base_url, query_string)
  }else{
    # Return base URL without query parameters
    URL = base_url
  }
  
  
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::GET(url = URL,
                   httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                     `Content-Type` = 'application/json'
                   )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(data.table::rbindlist(lapply(dta$instruments, function(item) {
      list(
        symbol = item$instrument$symbol,
        type = item$instrument$type,
        trading = item$trading,
        fractionalTrading = item$fractionalTrading,
        optionTrading = item$optionTrading,
        optionSpreadTrading = item$optionSpreadTrading
      )
    })))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}


#' Get Specific Instrument Information
#'@return Retrieves specific trading instrument with optional filtering capabilities as a \code{data.frame}.
#'@param  symbol = Trading Symbol Type: Ex. "AAPL"
#'@param    type = Symbol Type Ex. "EQUITY", "OPTION", "MULTI_LEG_INSTRUMENT", "CRYPTO", "ALT", "TREASURY", "BOND", "INDEX"
#'@examples
#' \dontrun{
#'   # Fetches AAPL instrument trading information
#'     this_ins <- rp_getInstrument(symbol = "AAPL", type="EQUITY")
#' }
#'@export
rp_getInstrument = function(symbol, type){
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::GET(url = paste0('https://api.public.com/userapigateway/trading/instruments/',symbol,'/',type),
                   httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                     `Content-Type` = 'application/json'
                   )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(dta)
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}

# ***************************************************************************
#                    Market Data
# ***************************************************************************

#' Get Trading Quotes
#'@return Retrieve real-time quotes as a \code{data.frame}.
#'@param  accountId = Public Brokerage Account Number
#'@param     ticker = Ticker symbol: Ex. "SPY"
#'@param       type = Ticker Type: Ex. 'EQUITY','OPTION','MULTI_LEG_INSTRUMENT', 'CRYPTO', 'ALT','TREASURY', 'BOND', 'INDEX'
#'@examples
#' \dontrun{
#'  # Fetches Multiple Real-Time Quotes
#'  my_acc <- rp_getAccts()
#'  rp_getQuote(accountId = my_acc$accountId, ticker = "TSLA", type="EQUITY")
#'  rp_getQuote(accountId = my_acc$accountId, ticker = 'SPY250807C00633000', type = "OPTION") 
#'  rp_getQuote(accountId = my_acc$accountId, 
#'              ticker = c("AAPL", 'SPY250807C00633000'), 
#'              type = c("EQUITY", "OPTION"))
#' }
#'@export
rp_getQuote = function(accountId, ticker, type){
  # assign token envir
  .rp_read_tokens()
  # build data
  payload = .rp_make_qte_payload(symbols = ticker, types = type)    
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::POST(url = paste0('https://api.public.com/userapigateway/marketdata/',accountId,'/quotes'),
                    httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                      `Content-Type` = 'application/json'
                    ), 
                    body = jsonlite::toJSON(payload, pretty = T, auto_unbox = T)
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = suppressWarnings(data.frame(data.table::rbindlist(lapply(dta$quotes, function(item) {
      list(
        symbol = item$instrument$symbol,
        type = item$instrument$type,
        outcome = item$outcome,
        last = item$last,
        lastTimestamp = item$lastTimestamp,
        bid = item$bid,
        bidSize = item$bidSize,
        bidTimestamp = item$bidTimestamp,
        ask = item$ask,
        askSize = item$askSize,
        askTimestamp = item$askTimestamp,
        volume = item$volume,
        openInterest = item$openInterest
      )
    }))))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}



#' Get Option Expiration Dates
#'@return Retrieve option expiration dates for a specific ticker symbol as a \code{data.frame}.
#'@param  accountId = Public Brokerage Account Number
#'@param     ticker = Ticker symbol: Ex. "SPY"
#'@param       type = Ticker Type: Ex. 'EQUITY','OPTION','MULTI_LEG_INSTRUMENT', 'CRYPTO', 'ALT','TREASURY', 'BOND', 'INDEX'
#'@examples
#' \dontrun{
#'  # Fetches Option Expiry Dates Available
#'  my_acc <- rp_getAccts()
#'  rp_getOptExp(accountId = my_acc$accountId, ticker = "TSLA", type="EQUITY")
#' }
#'@export
rp_getOptExp = function(accountId, ticker, type){
  # assign token envir
  .rp_read_tokens()
  # build data
  payload = list('instrument' = list('symbol' = ticker, 'type' = type))
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::POST(url = paste0('https://api.public.com/userapigateway/marketdata/',accountId,'/option-expirations'),
                    httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                      `Content-Type` = 'application/json'
                    ), 
                    body = jsonlite::toJSON(payload, pretty = T, auto_unbox = T)
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(cbind(dta$expirations))
    colnames(df) = dta$baseSymbol
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}


#' Get Option Chains
#'@return Retrieve option chains by symbol and return as a \code{data.frame}.
#'@param  accountId = Public Brokerage Account Number
#'@param     ticker = Ticker symbol: Ex. "SPY"
#'@param       type = Ticker Type: Ex. 'EQUITY','OPTION','MULTI_LEG_INSTRUMENT', 'CRYPTO', 'ALT','TREASURY', 'BOND', 'INDEX'
#'@param        exp = Option Expiration Date: Ex. "2025-08-08"
#'@examples
#' \dontrun{
#'  # Fetches Option Chains for Ticker Symbol
#'  my_acc <- rp_getAccts()
#'  rp_getOptChains(accountId = my_acc$accountId, ticker = 'SPY', type = "EQUITY", exp="2025-08-15") 
#' }
#'@export
rp_getOptChains = function(accountId, ticker, type, exp){
  # assign token envir
  .rp_read_tokens()
  # build data
  payload = list('instrument' = list('symbol' = ticker, 'type' = type), 'expirationDate' = exp)
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::POST(url = paste0('https://api.public.com/userapigateway/marketdata/',accountId,'/option-chain'),
                    httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                      `Content-Type` = 'application/json'
                    ), 
                    body = jsonlite::toJSON(payload, pretty = T, auto_unbox = T)
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    calls = suppressWarnings(data.frame(data.table::rbindlist(lapply(dta$calls, function(item) {
      list(
        symbol = item$instrument$symbol,
        type = item$instrument$type,
        outcome = item$outcome,
        last = item$last,
        lastTimestamp = item$lastTimestamp,
        bid = item$bid,
        bidSize = item$bidSize,
        bidTimestamp = item$bidTimestamp,
        ask = item$ask,
        askSize = item$askSize,
        askTimestamp = item$askTimestamp,
        volume = item$volume,
        openInterest = item$openInterest
      )
    }))))
    calls$side = "C"
    puts = suppressWarnings(data.frame(data.table::rbindlist(lapply(dta$puts, function(item) {
      list(
        symbol = item$instrument$symbol,
        type = item$instrument$type,
        outcome = item$outcome,
        last = item$last,
        lastTimestamp = item$lastTimestamp,
        bid = item$bid,
        bidSize = item$bidSize,
        bidTimestamp = item$bidTimestamp,
        ask = item$ask,
        askSize = item$askSize,
        askTimestamp = item$askTimestamp,
        volume = item$volume,
        openInterest = item$openInterest
      )
    }))))
    puts$side = "P"
    # combine
    df = as.data.frame(rbind(calls, puts))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}
# ***************************************************************************
#                    Order Placement
# ***************************************************************************

#' Preflight Single-Leg
#'@return Calculates the estimated financial impact of a potential trade before execution and returns as a \code{data.frame}.
#'@param          accountId = Public Brokerage Account Number
#'@param             ticker = Ticker symbol: Ex. "SPY"
#'@param            symType = Ticker Type: Ex. 'EQUITY','OPTION','MULTI_LEG_INSTRUMENT', 'CRYPTO', 'ALT','TREASURY', 'BOND', 'INDEX'
#'@param               side = The Order Side BUY/SELL. For Options also include the openCloseIndicator. Ex. 'BUY' OR 'SELL'
#'@param            ordType = The Type of order: Ex. 'MARKET','LIMIT', 'STOP', 'STOP_LIMIT'
#'@param        timeInForce = The time in for the order: Ex. 'DAY' OR 'GTD"
#'@param     expirationTime = The expiration date. Only used when timeInForce is GTD, cannot be more than 90 days in the future
#'@param                qty = The order quantity. Used when buying/selling whole shares and when selling fractional. Mutually exclusive with amount
#'@param                amt = The order amount. Used when buying/selling shares for a specific notional value
#'@param             lmtPrc = The limit price. Used when orderType = LIMIT or orderType = STOP_LIMIT
#'@param            stopPrc = The stop price. Used when orderType = STOP or orderType = STOP_LIMIT
#'@param openCloseIndicator = Used for options only. Indicates if this is BUY to OPEN/CLOSE
#'@examples
#' \dontrun{
#'  # Fetches costs associated with the type of order being placed
#'  my_acc <- rp_getAccts()
#'  rp_preOrder_singleLeg(accountId = my_acc$accountId, ticker = "SPY250815C00633000", 
#'                        symType = "OPTION", side = "BUY", ordType = "MARKET", 
#'                        timeInForce = "DAY", qty = 1, openCloseIndicator = "OPEN")
#'
#'  rp_preOrder_singleLeg(accountId = my_acc$accountId, ticker = "TSLA", 
#'                        symType = "EQUITY", side = "BUY", ordType = "MARKET",
#'                        timeInForce = "DAY", qty = 0.50, 
#'                        openCloseIndicator = "OPEN")
#' }
#'@export
rp_preOrder_singleLeg = function(accountId, ticker, symType, side = NULL, ordType = NULL, 
                                 timeInForce = NULL, expirationTime = NULL, qty = NULL, 
                                 amt = NULL, lmtPrc = NULL, stopPrc = NULL, 
                                 openCloseIndicator = NULL){
  # assign token envir
  .rp_read_tokens()
  # build data
  payload = .rp_make_ord_payload(ticker, symType, side = side, ordType = ordType, 
                                 timeInForce = timeInForce, expirationTime = expirationTime, qty = qty, 
                                 amt = amt, lmtPrc = lmtPrc, stopPrc = stopPrc, 
                                 openCloseIndicator = openCloseIndicator)
  
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::POST(url = paste0('https://api.public.com/userapigateway/trading/',accountId,'/preflight/single-leg'),
                    httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                      `Content-Type` = 'application/json'
                    ), 
                    body = jsonlite::toJSON(payload, pretty = T, auto_unbox = T)
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}


#' Preflight Multiple-Leg
#'@return Calculates the estimated financial impact of a complex multi-leg trade before execution and returns as a \code{data.frame}.
#'@param     accountId = Public Brokerage Account Number
#'@param     orderType = The Type of order: Ex. 'MARKET','LIMIT', 'STOP', 'STOP_LIMIT'
#'@param           qty = leg_ratio multiple: ex. '2' multiples the leg_ratios by 2X
#'@param   leg_symbols = Symbols: ex. c("SPY250815C00631000", "SPY250815C00631000")
#'@param     leg_types = Symbol types: ex. c("OPTION", "OPTION")
#'@param     leg_sides = The side for each leg: ex. c("BUY", "SELL")
#'@param leg_indicator = Indicates if this is BUY to OPEN/CLOSE ex. c("OPEN", "OPEN")
#'@param    leg_ratios = The number of contracts to BUY/SELL: ex. c('5','5')
#'@param           tif = The time in for the order: 'DAY' or 'GTD"
#'@param          mins = Minutes till order expires. 
#'@param        lmtPrc = The limit price. Used when orderType = LIMIT or orderType = STOP_LIMIT
#'@examples
#' \dontrun{
#'  # Fetches costs associated with the type of order being placed
#'  my_acc <- rp_getAccts()
#' 
#'  # open bull-call spread for 0.25 (buy 2, sell 2)
#'  rp_preOrder_multiLeg(accountId = my_acc$accountId, orderType = "LIMIT", qty = 2, 
#'                       leg_symbols = c("SPY250815C00630000","SPY250815C00632000"),
#'                       leg_types = c("OPTION", "OPTION"), leg_sides = c("BUY","SELL"), 
#'                       leg_indicator = c("OPEN", "OPEN"), leg_ratios = c(1, 1), 
#'                       tif = "DAY", lmtPrc = 0.25)
#'
#'  # open long butterfly for 0.05 
#'  rp_preOrder_multiLeg(accountId = my_acc$accountId, orderType = "LIMIT", qty = 1, 
#'                       leg_symbols = c("SPY250815C00630000",
#'                                       "SPY250815C00631000",
#'                                       "SPY250815C00632000"), 
#'                       leg_types = c("OPTION", "OPTION", "OPTION"), 
#'                       leg_sides = c("BUY","SELL","BUY"), 
#'                       leg_indicator = c("OPEN","OPEN","OPEN"), 
#'                       leg_ratios = c(1, 2, 1), tif = "DAY", lmtPrc = 0.05)
#'
#' # open iron-condor
#'  rp_preOrder_multiLeg(accountId = my_acc$accountId, orderType = "LIMIT", qty = 1, 
#'                       leg_symbols = c("SPY250815C00631000","SPY250815C00630000",
#'                                       "SPY250815C00625000","SPY250815C00624000"), 
#'                       leg_types = c("OPTION", "OPTION", "OPTION","OPTION"), 
#'                       leg_sides = c("SELL","BUY","SELL","BUY"), 
#'                       leg_indicator = c("OPEN","OPEN","OPEN","OPEN"), 
#'                       leg_ratios = c(1, 1, 1, 1), tif = "DAY", lmtPrc = 0.30)
#' }
#'@export
rp_preOrder_multiLeg = function(accountId, orderType, qty, leg_symbols, leg_types, leg_sides, leg_indicator, leg_ratios, tif, mins=NULL, lmtPrc=NULL){
  # assign token envir
  .rp_read_tokens()
  # build data
  payload = .rp_make_multileg_payload(orderType = orderType, 
                                      qty=qty, 
                                      leg_symbols = leg_symbols,
                                      leg_types = leg_types, 
                                      leg_sides = leg_sides,
                                      leg_indicator = leg_indicator, 
                                      leg_ratios = leg_ratios, 
                                      tif = tif,
                                      expTime = mins,
                                      lmtPrc = lmtPrc)
  
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::POST(url = paste0('https://api.public.com/userapigateway/trading/',accountId,'/preflight/multi-leg'),
                    httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                      `Content-Type` = 'application/json'
                    ), 
                    body = jsonlite::toJSON(payload, pretty = T, auto_unbox = T)
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}

#' Single-Leg Live Order
#'@return Submit a live single-leg order and and returns the order ID as a \code{data.frame}.
#'@param          accountId = Public Brokerage Account Number
#'@param             ticker = Ticker symbol: Ex. "SPY"
#'@param            symType = Ticker Type: Ex. 'EQUITY','OPTION','MULTI_LEG_INSTRUMENT', 'CRYPTO', 'ALT','TREASURY', 'BOND', 'INDEX'
#'@param            orderId = The order ID: use rp_getOrderId()
#'@param               side = The Order Side BUY/SELL. For Options also include the openCloseIndicator. Ex. 'BUY' OR 'SELL'
#'@param            ordType = The Type of order: Ex. 'MARKET','LIMIT', 'STOP', 'STOP_LIMIT'
#'@param        timeInForce = The time in for the order: Ex. 'DAY' OR 'GTD"
#'@param     expirationTime = The expiration date. Only used when timeInForce is GTD, cannot be more than 90 days in the future
#'@param                qty = The order quantity. Used when buying/selling whole shares and when selling fractional. Mutually exclusive with amount
#'@param                amt = The order amount. Used when buying/selling shares for a specific notional value
#'@param             lmtPrc = The limit price. Used when orderType = LIMIT or orderType = STOP_LIMIT
#'@param            stopPrc = The stop price. Used when orderType = STOP or orderType = STOP_LIMIT
#'@param openCloseIndicator = Used for options only. Indicates if this is BUY to OPEN/CLOSE
#'@examples
#' \dontrun{
#'  # Submit a live single-leg order to your Public Brokerage Account
#'  my_acc <- rp_getAccts() 
#'  
#'  # Option Order
#'  rp_order_singleLeg(accountId = my_acc$accountId, ticker = "SPY250815C00633000", symType = "OPTION", 
#'                     orderId = rp_getOrderId(), side = "BUY", ordType = "LIMIT", lmtPrc = 1.50, 
#'                     timeInForce = "DAY", qty = 1, openCloseIndicator = "OPEN")
#'                
#'  # Equity Fraction Share Order                   
#'  rp_preOrder_singleLeg(accountId = my_acc$accountId, ticker = "TSLA", symType = "EQUITY", 
#'                        side = "BUY", ordType = "MARKET", timeInForce = "DAY", qty = 0.50,
#'                        openCloseIndicator = "OPEN")
#' }
#'@export
rp_order_single = function(accountId, ticker, symType, orderId, side = NULL, ordType = NULL, 
                           timeInForce = NULL, expirationTime = NULL, qty = NULL, 
                           amt = NULL, lmtPrc = NULL, stopPrc = NULL, 
                           openCloseIndicator = NULL){
  # assign token envir
  .rp_read_tokens()
  # build data
  payload = .rp_make_ord_payload(ticker, symType, orderId = orderId, side = side, ordType = ordType, 
                                 timeInForce = timeInForce, expirationTime = expirationTime, qty = qty, 
                                 amt = amt, lmtPrc = lmtPrc, stopPrc = stopPrc, 
                                 openCloseIndicator = openCloseIndicator)
  
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::POST(url = paste0('https://api.public.com/userapigateway/trading/',accountId,'/order'),
                    httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                      `Content-Type` = 'application/json'
                    ), 
                    body = jsonlite::toJSON(payload, pretty = T, auto_unbox = T)
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}


#' Multi-Leg Live Order
#'@return Place a new multi-leg order and returns order id as a \code{data.frame}.
#'@param     accountId = Public Brokerage Account Number
#'@param     orderType = The Type of order: Ex. 'MARKET','LIMIT', 'STOP', 'STOP_LIMIT'
#'@param           qty = leg_ratio multiple: ex. '2' multiples the leg_ratios by 2X
#'@param   leg_symbols = Symbols: ex. c("SPY250815C00631000", "SPY250815C00631000")
#'@param     leg_types = Symbol types: ex. c("OPTION", "OPTION")
#'@param     leg_sides = The side for each leg: ex. c("BUY", "SELL")
#'@param leg_indicator = Indicates if this is BUY to OPEN/CLOSE ex. c("OPEN", "OPEN")
#'@param    leg_ratios = The number of contracts to BUY/SELL: ex. c('5','5')
#'@param           tif = The time in for the order: 'DAY' or 'GTD"
#'@param          mins = Minutes till order expires. 
#'@param        lmtPrc = The limit price. Used when orderType = LIMIT or orderType = STOP_LIMIT
#'@param       orderId = The order ID: use rp_getOrderId()
#'@examples
#' \dontrun{
#'  # Fetches costs associated with the type of order being placed
#'  my_acc <- rp_getAccts()
#' 
#'  # open bull-call spread for 0.25  (buy 2, sell 2)
#' rp_order_multi(accountId = my_acc$accountId, orderType = "LIMIT", qty = 2,
#'                leg_symbols = c("SPY250815C00630000","SPY250815C00632000"),
#'                leg_types = c("OPTION", "OPTION"), leg_sides = c("BUY","SELL"),
#'                leg_indicator = c("OPEN", "OPEN"), leg_ratios = c(1, 1),
#'                tif = "DAY", lmtPrc = 0.25, orderId = rp_getOrderId())
#'
#'  # open long butterfly for 0.05 
#'  rp_order_multi(accountId = my_acc$accountId, orderType = "LIMIT", qty = 1, 
#'                 leg_symbols = c("SPY250815C00630000",
#'                                 "SPY250815C00631000",
#'                                 "SPY250815C00632000"), 
#'                 leg_types = c("OPTION", "OPTION", "OPTION"), 
#'                 leg_sides = c("BUY","SELL","BUY"), 
#'                 leg_indicator = c("OPEN","OPEN","OPEN"), 
#'                 leg_ratios = c(1, 2, 1), tif = "DAY", lmtPrc = 0.05, 
#'                 orderId = rp_getOrderId())
#'
#' # open iron-condor
#'  rp_order_multi(accountId = my_acc$accountId, orderType = "LIMIT", qty = 1, 
#'                 leg_symbols = c("SPY250815C00631000","SPY250815C00630000",
#'                                 "SPY250815C00625000","SPY250815C00624000"), 
#'                 leg_types = c("OPTION", "OPTION", "OPTION","OPTION"), 
#'                 leg_sides = c("SELL","BUY","SELL","BUY"), 
#'                 leg_indicator = c("OPEN","OPEN","OPEN","OPEN"), 
#'                 leg_ratios = c(1, 1, 1, 1), tif = "DAY", lmtPrc = 0.30, 
#'                 orderId = rp_getOrderId())
#'                 
#' # covered call
#'   rp_order_multi(accountId = my_acc$accountId, orderType = "LIMIT", qty = 1, 
#'                                 leg_symbols = c("RIVN","RIVN250815C00012000"),
#'                                 leg_types = c("EQUITY", "OPTION"), 
#'                                 leg_sides = c("BUY","SELL"), 
#'                                 leg_indicator = c("OPEN", "OPEN"), 
#'                                 leg_ratios = c(100, 1), 
#'                                 tif = "DAY", lmtPrc = 11.75, 
#'                                 orderId = rp_getOrderId())
#'
#' }
#'@export
rp_order_multi = function(accountId, orderType, orderId, qty, leg_symbols, leg_types, leg_sides, leg_indicator, leg_ratios, tif, mins=NULL, lmtPrc=NULL){
  # assign token envir
  .rp_read_tokens()
  # build data
  payload = .rp_make_multileg_payload(orderType = orderType, 
                                      orderId = orderId,
                                      qty=qty, 
                                      leg_symbols = leg_symbols,
                                      leg_types = leg_types, 
                                      leg_sides = leg_sides,
                                      leg_indicator = leg_indicator, 
                                      leg_ratios = leg_ratios, 
                                      tif = tif,
                                      expTime = mins,
                                      lmtPrc = lmtPrc)
  
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::POST(url = paste0('https://api.public.com/userapigateway/trading/',accountId,'/order/multileg'),
                    httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                      `Content-Type` = 'application/json'
                    ), 
                    body = jsonlite::toJSON(payload, pretty = T, auto_unbox = T)
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}



#' Get Order Details
#'@return Retrieve order details & return as a \code{data.frame}.
#'@param     accountId = Public Brokerage Account Number
#'@param       orderId = The order ID
#'@examples
#' \dontrun{
#'  # Fetches Specific Order
#'  my_acc <- rp_getAccts()
#'  rp_get_order(accountId = my_acc$accountId, 
#'               orderId = "c99be1dd-bb87-4f7a-803f-ec47226bf64e") 
#' }
#'@export
rp_get_order = function(accountId, orderId){
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::GET(url = paste0('https://api.public.com/userapigateway/trading/',accountId,'/order/',orderId),
                   httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                     `Content-Type` = 'application/json'
                   )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}

#' Cancel Order
#'@return Request order cancellation & return as a \code{data.frame}.
#'@param     accountId = Public Brokerage Account Number
#'@param       orderId = The order ID
#'@examples
#' \dontrun{
#'  # Cancels Specific Order
#'  my_acc <- rp_getAccts()
#'  rp_cancel_order(accountId = my_acc$accountId, 
#'                  orderId = "c99be1dd-bb87-4f7a-803f-ec47226bf64e") 
#' }
#'@export
rp_cancel_order = function(accountId, orderId){
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::DELETE(url = paste0('https://api.public.com/userapigateway/trading/',accountId,'/order/',orderId),
                      httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                        `Content-Type` = 'application/json'
                      )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}

# ***************************************************************************
#                          Get Option Greeks
# ***************************************************************************

#' Get Option Greeks
#'@return Request order cancellation & return as a \code{data.frame}.
#'@param       accountId = Public Brokerage Account Number
#'@param osiOptionSymbol = option symbol
#'@examples
#' \dontrun{
#'  # get account number
#'  my_acc <- rp_getAccts()
#'  
#'  # build option symbol
#'  this_op = .rp_make_opt_symbol(under_sym = "SPY", exp = "2025-08-22", 
#'                                type = "P", strike = 600)
#'  
#'  # get greeks
#'  rp_get_greeks(accountId = my_acc$accountId, osiOptionSymbol = this_op) 
#' }
#'@export
rp_get_greeks = function(accountId, osiOptionSymbol){
  # assign token envir
  .rp_read_tokens()
  # update token if needed
  .rp_checkAccessToken()
  # Request
  resp = httr::GET(url = paste0('https://api.public.com/userapigateway/option-details/',accountId,'/',osiOptionSymbol,'/greeks'),
                   httr::add_headers(`Authorization` = paste0("Bearer ", .rp_env$rp$access_token),
                                     `Content-Type` = 'application/json'
                   )
  )
  
  # extract content from response
  if(httr::status_code(resp) == 200){
    # extract data
    dta = httr::content(resp)
    # format data
    df = data.frame(t(unlist(dta)))
  }else{
    # get error code
    err = httr::content(resp)
    df = NULL
    stop(paste0(err$message," : ", err$errors[[1]]))
  }
  df
}


