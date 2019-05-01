let mask64 = Int64.sub (Int64.shift_left 1L 52) 1L;;
let mask32 = (1 lsl 23)-1

let to64' x = Printf.sprintf "%.16LX" x;;
let to32' x = Printf.sprintf "%.8lX" x;;
let to64 x = to64' (Int64.bits_of_float x);;
let to32 x = to32' (Int32.bits_of_float x);;

let expon64 (sgn,exp,mant) = 2.**(float_of_int (exp-1075)) *. (float_of_int mant)
let expon32 (sgn,exp,mant) = 2.**(float_of_int (exp-150)) *. (float_of_int mant)

let split64' bits =
  let sgn = (Int64.to_int (Int64.shift_right_logical bits 63)) land 1 in
  let exp = (Int64.to_int (Int64.shift_right_logical bits 52)) land 2047 in
  let mant = (1 lsl 52) lor (Int64.to_int (Int64.logand bits mask64)) in
  (sgn,exp,mant,expon64 (sgn,exp,mant))

let split64 x = split64' (Int64.bits_of_float x)

let of64 x = Scanf.sscanf x "%Lx" (fun x -> split64' x)
let float64 x = Scanf.sscanf x "%Lx" (fun x -> Int64.float_of_bits x)
let int64 x = Scanf.sscanf x "%Lx" (fun x -> x)

let split32' x =
  let bits = Int32.to_int x in
  let sgn = (bits lsr 31) land 1 in
  let exp = (bits lsr 23) land 255 in
  let mant = (1 lsl 23) lor (bits land mask32) in
  (sgn,exp,mant,expon32 (sgn,exp,mant))

let split32 x = split32' (Int32.bits_of_float x)

let of32 x = Scanf.sscanf x "%lx" (fun x -> split32' x)
let float32 x = Scanf.sscanf x "%lx" (fun x -> Int32.float_of_bits x)
let int32 x = Scanf.sscanf x "%lx" (fun x -> x)

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

let join32 (sgn,exp,mant,_) =
  let mant' = Int32.of_int (mant land mask32) in
  let exp' = Int32.of_int exp in
  let sgn' = Int32.of_int sgn in
  let bits' = Int32.logor (Int32.logor (Int32.shift_left sgn' 31) (Int32.shift_left exp' 23)) mant' in
  Printf.sprintf "%.8lX" bits'

let split64to32 (sgn,exp,mant,_) =
  let exp' = if exp > 0 then (if exp > 896 then exp - 896 else 0) else 0 in
  let mant' = if exp > 0 then (if exp > 896 then mant lsr 29 else if exp > 872 then mant lsr (29+897-exp) else 0) else 0 in
  (sgn,exp',mant',expon64 (sgn,exp',mant'))

let trunc32 x = float32(join32(split64to32 (split64 x)));;

let f2i (sgn,exp,mant,_) =
  Int64.mul (if sgn > 0 then -1L else 1L) (Int64.shift_right_logical (Int64.shift_left (Int64.of_int mant) 11) (63-exp+1023))

(*
 (arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13,arg14,arg15,arg16,arg17,arg18,arg19,arg20,arg21,arg22,arg23)
 *)

let scan1 f =
  let fd = open_in f in
  let lines = ref [] in (try while true do lines := input_line fd :: !lines done with err -> close_in fd);
  Array.of_list (List.rev !lines)


(*
 let (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x) = Scanf.fscanf fd "%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,]"
 (fun a b c d e f g h i j k l m n o p q r s t u v w x -> (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x)) in
  (try
    Scanf.fscanf fd "%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX"
    (fun arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10 arg11 arg12 arg13 arg14 arg15 arg16 arg17 arg18 arg19 arg20 arg21 arg22 arg23 -> incr cnt)
    with _ -> close_in fd);
  !cnt
*)

let contents = scan1 "/local/scratch/jrrk2/ariane-lowrisc-genesys2/fpga/iladata.csv";;
let headers = Scanf.sscanf contents.(0) "%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,],%[^,]" (fun a b c d e f g h i j k l m n o p q r s t u v w -> ([a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;q;r;s;t;u;v;w]));;
let array ix = Scanf.sscanf contents.(ix) "%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX,%LX" (fun a b c d e f g h i j k l m n o p q r s t u v w -> Array.of_list([a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;q;r;s;t;u;v;w]));;
