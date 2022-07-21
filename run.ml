let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x  

let random_int_list ~size = 
  let ints = 
    (0,[]) |> iter_k (fun ~k (len,xs) -> 
        if len >=size then xs else
          k (len+1,(Random.int (1024 * 1024 * 1024 -1))::xs))
  in
  ints

type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module Int_mmap : 
sig 
  type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t = private { fn:string; fd:Unix.file_descr; mutable arr: int_bigarray }
  val create : fn:string -> sz:int -> t
    
  (** NOTE [open_ ~fn ~sz] can use [sz=-1] to open with size based on the size of the
      underlying file *)
  val open_  : fn:string -> sz:int -> t
  val close  : t -> unit
end      
= struct
  type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t = { fn:string; fd:Unix.file_descr; mutable arr: int_bigarray }

  (* NOTE both following are shared *)
  let shared = true

  let create ~fn ~sz =
    assert(not (Sys.file_exists fn) || begin
        Printf.printf "File exists: %s\n%!" fn; false end);
    let fd = Unix.(openfile fn [O_CREAT;O_RDWR;O_TRUNC;O_EXCL;O_CLOEXEC] 0o660) in
    let arr = 
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  (* NOTE sz=-1 is recognized by [map_file] as "derive from size of file"; if we want a
     different size (eg because we want the file to grow) we can provide it explicitly *)
  let open_ ~fn ~sz =
    assert(Sys.file_exists fn);
    let fd = Unix.(openfile fn [O_RDWR] 0o660) in
    let arr = 
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  let close t = 
    Unix.close t.fd;
    (* following tries to make the array unreachable, so GC'able; however, no guarantee
       that arr actually is unreachable *)
    t.arr <- Bigarray.(Array1.create Int c_layout 0);
    ()

end

(* create a file with 15M integers in it (120_000_000 bytes, 115M via ls) *)
let create fn = 
  let sz = 15_000_000 in
  let ints = random_int_list ~size:sz in
  let c = Mtime_clock.counter () in
  let mmap = Int_mmap.create ~fn ~sz in
  let arr = mmap.arr in
  ints |> List.iteri (fun i n -> arr.{i} <- n);
  let ms = Mtime_clock.count c |> Mtime.Span.to_ms in
  Int_mmap.close mmap;
  Printf.printf "Created %s in %.4f ms\n%!" fn ms;
  ()
(* typical output for 15M ints: 
Created test.tmp in 103.1314 ms
*)

let load_and_sum fn =
  let c = Mtime_clock.counter () in
  let mmap = Int_mmap.open_ ~fn ~sz:(-1) in
  let arr = mmap.arr in
  let sz = Bigarray.Array1.dim arr in
  let tot = 
    (0,0) |> iter_k (fun ~k (i,tot) -> match i < sz with
        | true ->  k (i+1, (tot+(arr.{i} mod 2)))
        | false -> tot)
  in
  ignore tot;
  let ms = Mtime_clock.count c |> Mtime.Span.to_ms in
  Printf.printf "Loaded and summed %s in %.4f ms\n%!" fn ms;
  ()
(* Typical output, 15M ints, without flushing caches after creation:
Loaded and summed test.tmp in 58.7820 ms

With "sudo sh -c 'echo 3 >/proc/sys/vm/drop_caches'" to drop caches:
Loaded and summed test.tmp in 119.3393 ms

A typical SSD should be able to read, say, 500MB per s; so 100MB should take about 200
ms. Even at 200MB/s, an SSD should read 100MB in 0.5s. The time above of 119ms is for an
Samsung M.2 NVMe, with claimed "up to 3000MB/s read".

The takeaway is that reading via mmap seems pretty fast.

*)


(* following may not be a fair comparison with mmap, but should give an idea of the
   timings for buffered IO *)
let load_via_in_channel fn =
  let c = Mtime_clock.counter () in
  let ic = Stdlib.open_in_bin fn in
  let tot = 
    let buf = Bytes.create 8 in
    (0,0) |> iter_k (fun ~k (i,tot) ->
        let n = Stdlib.input ic buf 0 8 in
        match n with
        | 0 -> tot
        | 8 ->           
          let x = Bytes.get_int64_ne buf 0 |> Int64.to_int in
          k (i+1, (tot+(x mod 2)))
        | _ -> failwith "invalid read length")
  in
  ignore tot;
  let ms = Mtime_clock.count c |> Mtime.Span.to_ms in
  Printf.printf "Loaded via in channel, and summed %s in %.4f ms\n%!" fn ms;
  ()
(* Typical output:

Loaded via in channel, and summed test.tmp in 251.3884 ms

So, just over twice the time it took the mmap. 

*)  

let test_fn = "test.tmp"

let _main =
  match Sys.argv.(1) with
  | "create" -> 
    create test_fn;
    ()
  | "load" -> 
    load_and_sum test_fn;
    ()
  | "load_ic" -> 
    load_via_in_channel test_fn;
    ()
  | _ -> failwith "unknown command line arg"
