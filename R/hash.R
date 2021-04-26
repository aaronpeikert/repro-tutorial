#----hash----
# compute hash using md5
hash <- digest::digest(res_raw, "md5")
if(hash != "4203854848ad0cc5d2f848c44b70815d"){
  warning("Mismatch between original and current simulations!\nHash now is:\n    '", hash, "'")
}
