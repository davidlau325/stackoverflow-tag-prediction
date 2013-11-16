open Core.Std

type read_status = Tag | TagEnd | Content | ContentEnd | Title | TitleEnd

exception IncorrectFileFormat

let rec loop_file fd status tag_list =
	match status with
	| Tag -> begin match In_channel.input_line fd with
			 | None -> ()
			 | Some tags -> printf "Tag here: %s\n" tags; loop_file fd TagEnd tag_list
			 (* handle tag file creation and store as temprorary list *)
			  end
	| TagEnd -> begin match In_channel.input_line fd with
			 | None -> raise IncorrectFileFormat
			 | Some _ -> loop_file fd Content tag_list
			end 
	| Content -> begin match In_channel.input_line fd with
			 | None -> raise IncorrectFileFormat
			 | Some content -> printf "Content here: %s\n" content; loop_file fd ContentEnd tag_list
			 (* split the whole string into word by word, then
				create file for each word and then insert (word,tag,occurance)
				into content directory *)
			 end
	| ContentEnd -> begin match In_channel.input_line fd with
			 | None -> raise IncorrectFileFormat
			 | Some _ -> loop_file fd Title tag_list
			end 
	| Title -> begin match In_channel.input_line fd with
			 | None -> raise IncorrectFileFormat
			 | Some title -> printf "Title here: %s\n" title; loop_file fd TitleEnd tag_list
			 (* split the whole string into word by word, same with content,
				into title directory *)
			 end
	| TitleEnd -> begin match In_channel.input_line fd with
			 | None -> ()
			 | Some _ -> loop_file fd Tag tag_list
			end
	
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
