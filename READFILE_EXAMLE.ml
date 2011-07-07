let readProg filename =
  let channel = open_in filename in
  let rec read_file () =
     try
       let data = input_line channel in
         data ^ read_file ()
     with End_of_file -> close_in channel ; "\n"
  in
     print_string(read_file ()) ;;


let _ = readProg "syntax.ml";;

