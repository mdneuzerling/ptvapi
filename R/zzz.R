# pkg_env is used for "private" package variables.
# They're not truly private, but they are more difficult to access.
# In our case, this is a useful place to cache results.
pkg_env <- new.env(parent = emptyenv())
