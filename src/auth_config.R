# --- File: auth_config.R ---

# Helper function for handling NULL or empty values
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# --- Configuration (Load from Environment Variables or Secure Source) ---
DEX_ISSUER <- Sys.getenv("DEX_ISSUER")
DEX_CLIENT_ID <- Sys.getenv("DEX_CLIENT_ID")
DEX_CLIENT_SECRET <- Sys.getenv("DEX_CLIENT_SECRET")
APP_REDIRECT_URI <- Sys.getenv("APP_REDIRECT_URI")


# --- Fetch OIDC Discovery Information ---
get_oidc_discovery <- function(issuer_url) {
  discovery_url <- paste0(issuer_url, "/.well-known/openid-configuration")
  tryCatch({
    resp <- httr2::request(discovery_url) |> # Use httr2:: explicitly if not loading here
      httr2::req_perform() |>
      httr2::resp_body_json()
    return(resp)
  }, error = function(e) {
    warning(paste("Failed to fetch OIDC discovery document from:", discovery_url, "\nError:", e$message))
    return(NULL)
  })
}

oidc_config <- get_oidc_discovery(DEX_ISSUER)

# Extract endpoints
DEX_AUTH_ENDPOINT <- oidc_config$authorization_endpoint %||% paste0(DEX_ISSUER, "/auth")
DEX_TOKEN_ENDPOINT <- oidc_config$token_endpoint %||% paste0(DEX_ISSUER, "/token")
DEX_JWKS_ENDPOINT <- oidc_config$jwks_uri %||% paste0(DEX_ISSUER, "/keys")
DEX_USERINFO_ENDPOINT <- oidc_config$userinfo_endpoint
DEX_LOGOUT_ENDPOINT <- oidc_config$end_session_endpoint

# --- Create httr2 OAuth Client ---
dex_client <- httr2::oauth_client( # Use httr2:: explicitly if not loading here
  id = DEX_CLIENT_ID,
  secret = DEX_CLIENT_SECRET,
  token_url = DEX_TOKEN_ENDPOINT,
  name = "MADILumiReaderLocalClient"
)

# --- Define Scopes ---
OIDC_SCOPES <- "openid profile email"

# --- JWKS Cache ---
jwks_cache <- NULL
jwks_cache_time <- NULL
JWKS_CACHE_EXPIRY_SECS <- 5


get_jwks <- function(jwks_uri) {
  tryCatch({
    message(paste("Fetching JWKS from:", jwks_uri))
    resp <- httr2::request(jwks_uri) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) == 200) {
      keys <- httr2::resp_body_json(resp)
      return(keys$keys)
    } else {
      warning(paste("Failed to fetch JWKS, status code:", httr2::resp_status(resp)))
      return(NULL)
    }

  }, error = function(e) {
    warning(paste("Error fetching JWKS from:", jwks_uri, "\nDetails:", e$message))
    return(NULL)
  })
}


# Optional: Print a message to confirm sourcing
message("Authentication configuration loaded.")
