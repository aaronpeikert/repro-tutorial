#----hash----
# compute hash using md5
hash <- digest::digest(res_raw, "md5")
if(hash != "cd14a91f680aef9355d8b7fe99e7f20f"){
  warning("Mismatch between original and current simulations!\nHash now is:\n    '", hash, "'")
}
