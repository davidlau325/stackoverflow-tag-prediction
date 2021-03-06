open Core.Std
open Async.Std
open Re2

let query_uri sort page =
  let base_uri = Uri.of_string "http://stackoverflow.com/questions" in
    let first_param = Uri.add_query_param base_uri ("sort",[sort]) in
      Uri.add_query_param first_param ("page", [page])

let question_url question = 
  Uri.of_string ("http://stackoverflow.com" ^ question) 

let get_html uri =
  Cohttp_async.Client.get uri
  >>= fun (_, body) ->
  Pipe.to_list body
  >>| fun strings -> String.concat strings

let is_question line =
    let question = Regex.create_exn "(.*)class=\"question-hyperlink\">(.*)" in
    Regex.matches question line

let is_title line =
    let title = Regex.create_exn "(.*)itemprop=\"name\">(.*)" in
    Regex.matches title line

let get_title line =
    let title = Regex.create_exn "class=\"question-hyperlink\">(.*?)</a>" in
      let sub = Regex.find_submatches_exn title line in
        match sub.(1) with
        | None -> "No found"
        | Some text -> text

let is_content line =
    let content = Regex.create_exn "(.*)class=\"post-text\" itemprop=\"description\"(.*)" in
    Regex.matches content line

let is_full_content line =
    let content = Regex.create_exn "<p>(.*)</p>" in
    Regex.matches content line

let get_full_content line =
    let content = Regex.create_exn "<p>(.*?)</p>" in
    Regex.find_first_exn ~sub:(`Index 1) content line

let is_valid_content line =
    let content = Regex.create_exn "<p>" in
    Regex.matches content line

let is_content_end line =
    let content = Regex.create_exn "</p>" in
    Regex.matches content line

let is_code_content line =
    let content = Regex.create_exn "<code>" in
    Regex.matches content line

let is_code_end line =
    let content = Regex.create_exn "</code></pre>" in
    Regex.matches content line

let is_div_end line =
    let content_end = Regex.create_exn "(.*)</div>(.*)" in
    Regex.matches content_end line

let is_tag line =
    let tag = Regex.create_exn "<div class=\"post-taglist\">" in
    Regex.matches tag line

let get_tag line =
    let tag = Regex.create_exn "rel=\"tag\">(.*?)</a>" in
    String.concat ~sep:" " (Regex.find_all_exn ~sub:(`Index 1) tag line)

let is_url line =
    let url = Regex.create_exn "href=\"(.*?)\"" in
      let sub = Regex.find_submatches_exn url line in
        match sub.(1) with
        | None -> "No found"
        | Some text -> text

let filter_question html =
    let html_list = String.split_lines html in
      let filtered_list = List.filter ~f: is_question html_list in
        List.map ~f: is_url filtered_list

type content_type = Title | Content | ContentEnd | Tag | TagEnd

let rec is_title_content = function
        | ([],_) -> []
        | (x::xs,Title) -> if (is_title x) 
                        then (get_title x)::"\n###\n"::(is_title_content (xs,Content))
                        else is_title_content (xs,Title)
        | (x::xs,Content) -> if (is_content x)
                             then is_title_content (xs,ContentEnd)
                             else is_title_content (xs,Content)
        | (x::xs,ContentEnd) -> if (is_div_end x)
                                then "\n###\n"::(is_title_content (xs,Tag))
                                else if String.is_empty x
                                then is_title_content (xs,ContentEnd) 
                                else x::" "::(is_title_content (xs,ContentEnd))
        | (x::xs,Tag) -> if (is_tag x)
                         then is_title_content (xs,TagEnd)
                         else is_title_content (xs,Tag)
        | (x::xs,TagEnd) -> if (is_div_end x)
                            then []
                            else (get_tag x)::(is_title_content (xs,TagEnd))

let filter_extra_white html =
    let non = Regex.create_exn " +" in
      Regex.replace_exn ~f:(fun _->" ") non html

let filter_meaningless html =
    let less = Regex.create_exn "<[^>]*>|<a(.+?)</a>|&#39;|&#39;|&nbsp;|&ldquo;|&rdquo;|&gt;|&lt;|[^a-zA-Z0-9#\+\-\n ]" in 
      Regex.replace_exn ~f:(fun _->" ") less html

let filter_tag_meaningless html =
    let less = Regex.create_exn "<[^>]*>|<a(.+?)</a>|&#39;|&#39;|&nbsp;|&ldquo;|&rdquo;|&gt;|&lt;" in 
      Regex.replace_exn ~f:(fun _->" ") less html

let filter_title_content html = 
    let post = List.rev (is_title_content ((String.split_lines html),Title)) in
      let tags = List.hd post in
        match tags with
        | None -> ""
        | Some tag -> begin
        let rest = String.concat (List.tl_exn post) in
        String.concat [(filter_tag_meaningless tag);(filter_meaningless rest)]
        |> filter_extra_white
        |> String.lowercase
        end
    
let print_result html_list sort page folder = 
    let final_list = List.map ~f:filter_title_content html_list in
      let filtered_final_list = List.filter final_list ~f:(fun line -> line <> "") in
      let fd = Out_channel.create (folder ^ "/" ^ sort ^ "-" ^ page ^ ".txt") in
        protect ~f:(fun () ->
          Out_channel.output_string fd (String.concat ~sep:"\n@@@@\n" filtered_final_list);
          printf "Done! %s %s\n" sort page;
          Out_channel.flush stdout
        )
        ~finally:(fun () -> Out_channel.close fd)

let get_question uri =
    get_html uri
    >>| fun html -> html

let rec crawl_question url_list =
     match url_list with
     | [] -> []
     | x::xs -> (get_question (question_url x))::crawl_question xs

let prepare_dir dirname =
  Sys.is_directory_exn dirname 
  >>| fun is_dir -> if is_dir then ()
  else ignore(Unix.mkdir dirname) 

let crawl_page sort page folder =
    get_html (query_uri sort page)
    >>| fun url_list -> 
          let filtered = filter_question url_list in
              Deferred.all (crawl_question filtered)
              >>| fun html_list -> print_result html_list sort page folder

let rec init_list start last =
  if start = last then []
  else start::(init_list (start+1) last)

let crawl_question_url sort start page test =
  let folder = if test then "test" else "crawl" in
      ignore(prepare_dir folder);
      ignore(Deferred.List.map ~how:`Sequential (init_list start (start+page))
        ~f:(fun num -> (after (sec 8.))
                        >>| fun () -> (crawl_page sort (string_of_int num) folder)
                        >>| fun a -> a
                        >>| fun _ -> ()
      )); 

(*
      for i=start to (start+page) do
        ignore((crawl_page sort (string_of_int i) folder))
      done;  *)
      Deferred.never ()

let () =
    Command.async_basic
    ~summary:"Crawl questions from StackOverflow"
    Command.Spec.(
        empty
        +> anon ("[Sort Type]" %:string)
        +> anon ("[Start Page Number]" %:int)
        +> anon ("[Number of Pages]" %:int)
        +> flag "-test" no_arg ~doc:" store crawl data as test data"
    )
    (fun sort start page test () -> crawl_question_url sort start page test)
    |> Command.run ~version:"0.1" ~build_info:"ENGG5103"


