open Core.Std

type read_status = Tag | TagEnd | Content | ContentEnd | Title | TitleEnd

exception IncorrectFileFormat
exception EmptyTagFileError

let get_tag_num tag =
	In_channel.with_file ("tags/" ^ tag ^ ".txt")
	~f:(fun fd ->
		match In_channel.input_line fd with
		| None -> raise EmptyTagFileError
		| Some line -> let name = String.split ~on:' ' line in
						List.hd_exn (List.tl_exn name))

let process_tag line _ =
	let tag_list2 = String.split ~on:' ' line in
		List.iter tag_list2 ~f:(fun tag ->
			if String.is_empty tag then ()
			else begin
				if Sys.is_file_exn ("tags/" ^ tag ^ ".txt")
				then begin
					let num = string_of_int ((int_of_string (get_tag_num tag)) + 1) in
					Out_channel.with_file ("tags/" ^ tag ^ ".txt") ~append:false
					~f:(fun fd -> Out_channel.output_string fd (tag ^ " " ^ num ^ "\n"))
				end
				else begin
					Out_channel.with_file ("tags/" ^ tag ^ ".txt")
					~f:(fun fd -> printf "%s\n" tag; Out_channel.output_string fd (tag ^ " 1\n"))
				end
			end 
		);
		tag_list2

let rec loop_file fd status tag_list =
	let process_line ~fd ~status ~f ~check ~tag_list =
	match In_channel.input_line fd with
	| None -> if check then raise IncorrectFileFormat else ()
	| Some line -> let tag_list2 = (f line tag_list) in (loop_file fd status tag_list2)
	in 
	match status with
	| Tag -> process_line ~fd ~status:TagEnd ~check:false ~tag_list 
			 ~f:(fun line tag_list-> process_tag line tag_list)
	| TagEnd -> process_line ~fd ~status:Content ~check:true ~tag_list 
			 ~f:(fun _ tag_list -> tag_list)
	| Content -> process_line ~fd ~status:ContentEnd ~check:true ~tag_list
			 ~f:(fun _ tag_list -> tag_list)
			 (* split the whole string into word by word, then
				create file for each word and then insert (word,tag,occurance)
				into content directory *)
	| ContentEnd -> process_line ~fd ~status:Title ~check:true ~tag_list
			 ~f:(fun _ tag_list -> tag_list)
	| Title -> process_line ~fd ~status:TitleEnd ~check:true ~tag_list
			 ~f:(fun _ tag_list -> tag_list)
			 (* split the whole string into word by word, same with content,
				into title directory *)
	| TitleEnd -> process_line ~fd ~status:Tag ~check:false ~tag_list
			 ~f:(fun _ tag_list -> tag_list)
	
let extract_file filename =
	In_channel.with_file filename 
		~f:(fun fd -> loop_file fd Tag [])

let prepare_dir dir_list =
	List.iter ~f:(fun name -> 
		if Sys.is_directory_exn name then ()
		else Unix.mkdir name 
	) dir_list

let () = 
	prepare_dir ["title";"content";"tags"];
	Command.basic 
		~summary: "Extract word co-occurances and tags information from file"
		Command.Spec.(
			empty
			+> anon ("[filename]" %: string)
		)
		(fun filename () -> extract_file filename)
	|> Command.run ~version:"0.1" ~build_info:"ENGG5013"
