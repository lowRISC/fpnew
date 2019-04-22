let mask64 = Int64.sub (Int64.shift_left 1L 52) 1L;;
let mask32 = (1 lsl 23)-1

let to64 x = Printf.sprintf "%.16LX" (Int64.bits_of_float x);;
let to32 x = Printf.sprintf "%.8lX" (Int32.bits_of_float x);;

let expon64 (sgn,exp,mant) = 2.**(float_of_int (exp-1075)) *. (float_of_int mant)
let expon32 (sgn,exp,mant) = 2.**(float_of_int (exp-150)) *. (float_of_int mant)

let split64 x =
  let bits = Int64.bits_of_float x in
  let sgn = (Int64.to_int (Int64.shift_right_logical bits 63)) land 1 in
  let exp = (Int64.to_int (Int64.shift_right_logical bits 52)) land 2047 in
  let mant = (1 lsl 52) lor (Int64.to_int (Int64.logand bits mask64)) in
  (sgn,exp,mant,expon64 (sgn,exp,mant))

let split32 x =
  let bits = Int32.to_int (Int32.bits_of_float x) in
  let sgn = (bits lsr 31) land 1 in
  let exp = (bits lsr 23) land 255 in
  let mant = (1 lsl 23) lor (bits land mask32) in
  (sgn,exp,mant,expon32 (sgn,exp,mant))

let join64 (sgn,exp,mant,_) =
  let mant' = Int64.logand (Int64.of_int mant) mask64 in
  let exp' = Int64.of_int exp in
  let sgn' = Int64.of_int sgn in
  let bits' = Int64.logor (Int64.logor (Int64.shift_left sgn' 63) (Int64.shift_left exp' 52)) mant' in
  Printf.sprintf "%.16LX" bits'

let split32to64 (sgn,exp,mant,_) =
  let exp' = if exp > 0 then exp + 896 else 0 in
  let mant' = mant lsl 29 in
  (sgn,exp',mant',expon64 (sgn,exp',mant'))
