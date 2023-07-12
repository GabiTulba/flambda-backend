module DLL = Flambda_backend_utils.Doubly_linked_list

let is_useless_mov (fst_mov:Cfg.basic Cfg.instruction) (snd_mov:Cfg.basic Cfg.instruction) =
    let fst_src, fst_dst = fst_mov.arg.(0), fst_mov.res.(0) in
    let snd_src, snd_dst = snd_mov.arg.(0), snd_mov.res.(0) in
    let is_same_loc = Reg.same_loc fst_src snd_dst && Reg.same_loc fst_dst snd_src in
    let is_same_reg_class = Regalloc_utils.same_reg_class fst_src snd_dst && Regalloc_utils.same_reg_class fst_dst snd_src in
    (Bool.to_string is_same_loc) ^ " " ^ (Bool.to_string is_same_reg_class) |> print_string;
    print_endline "";
    is_same_loc && is_same_reg_class
    

let rec optimize_body_list (body_list: Cfg.basic Cfg.instruction list) =
    match body_list with
    | [] -> []
    | [x] -> [x]
    | hd1 :: hd2 :: tl -> 
        match hd1.desc, hd2.desc with 
        | Op Move, Op Move -> if is_useless_mov hd1 hd2 then (optimize_body_list (hd1 :: tl)) else hd1 :: (optimize_body_list (hd2 :: tl))
        | _ -> hd1 :: (optimize_body_list (hd2 :: tl))

(* CR-soon gtulba-lecu for gtulba-lecu: Look into Flambda_backend_utils.Doubly_linked_list
   for better ways to alter the block's body *)
let optimize_block (_:Label.t) (block:Cfg.basic_block) =
    let optimized_body_list = block.body |> DLL.to_list |> optimize_body_list in
    DLL.clear block.body;
    DLL.transfer ~to_:block.body ~from:(DLL.of_list optimized_body_list) ()
;;

let peephole_optimize_cfg cfg_with_layout =
    Label.Tbl.iter optimize_block (Cfg_with_layout.cfg cfg_with_layout).blocks;
    cfg_with_layout
;;