open! Import
open Rv32.Fence_type

let fence_type = Alcotest.testable pp_fence_type equal_fence_type

let test_show_fence_type_i () =
  Alcotest.(check string) "show_fence_type i" (show_fence_type i) "i"

let test_show_fence_type_i_o () =
  Alcotest.(check string)
    "show_fence_type (i |+ o)"
    (show_fence_type (i |+ o))
    "io"

let test_show_fence_type_i_o_r () =
  Alcotest.(check string)
    "show_fence_type (i |+ o |+ r)"
    (show_fence_type (i |+ o |+ r))
    "ior"

let test_show_fence_type_i_o_r_w () =
  Alcotest.(check string)
    "show_fence_type (i |+ o |+ r |+ w)"
    (show_fence_type (i |+ o |+ r |+ w))
    "iorw"

let test_show_fence_type_i_o_r_w_i () =
  Alcotest.(check string)
    "show_fence_type (i |+ o |+ r |+ w |+ i)"
    (show_fence_type (i |+ o |+ r |+ w |+ i))
    "iorw"

let test_add_comm () = Alcotest.(check fence_type) "add_comm" (i |+ o) (o |+ i)

let suite =
  [
    ("Show fence type i", `Quick, test_show_fence_type_i);
    ("Show fence type io", `Quick, test_show_fence_type_i_o);
    ("Show fence type ior", `Quick, test_show_fence_type_i_o_r);
    ("Show fence type iorw", `Quick, test_show_fence_type_i_o_r_w);
    ("Show fence type iorw|+i", `Quick, test_show_fence_type_i_o_r_w_i);
    ("Fence add comm", `Quick, test_add_comm);
  ]
