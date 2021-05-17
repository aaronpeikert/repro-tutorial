#----hash----
# compute hash using md5
hash <- digest::digest(res, "md5")
if(hash != "e9e0dfe4b4753c1e00bb02e4205d8772"){
  warning("Mismatch between original and current simulations!\nHash now is:\n    '", hash, "'")
}
