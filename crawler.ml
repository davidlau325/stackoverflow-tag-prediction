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
    let content_end = Regex.create_exn "</div>" in
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

type content_type = Title | CodeEnd | PEnd | Content | ContentEnd | Tag | TagEnd

let rec is_title_content = function
        | ([],_) -> []
        | (x::xs,Title) -> if (is_title x) 
                        then  (get_title x)::"\n###\n"::(is_title_content (xs,Content))
                        else is_title_content (xs,Title)
        | (x::xs,Content) -> if (is_content x)
                             then is_title_content (xs,ContentEnd)
                             else is_title_content (xs,Content)
        | (x::xs,CodeEnd) -> if (is_code_end x)
                             then is_title_content (xs,ContentEnd)
                             else x::(is_title_content (xs,CodeEnd))
        | (x::xs,PEnd) -> if (is_content_end x)
                          then x::(is_title_content (xs,ContentEnd))
                          else x::(is_title_content (xs,PEnd))
        | (x::xs,ContentEnd) -> if (is_div_end x)
                                then "\n###\n"::(is_title_content (xs,Tag))
                                else if (is_full_content x)
                                then (get_full_content x)::(is_title_content (xs,ContentEnd))
                                else if (is_valid_content x) 
                                then x::(is_title_content (xs,PEnd))
                                else if (is_code_content x)
                                then x::(is_title_content (xs,CodeEnd))
                                else is_title_content (xs,ContentEnd) 
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
    let non = Regex.create_exn " p | pre | em | strong | href | nbsp | ldquo | rdquo " in
      filter_extra_white (Regex.replace_exn ~f:(fun _->"") non html)

let filter_whole_content html =
    let non_word = Regex.create_exn "[^a-zA-Z0-9#\+\-\n ]" in
      filter_meaningless (Regex.replace_exn ~f:(fun _->" ") non_word html)

let filter_title_content html = 
    let post = List.rev (is_title_content ((String.split_lines html),Title)) in
        String.lowercase (filter_whole_content (String.concat post))
    
let print_result html_list sort page =
    let final_list = List.map ~f:filter_title_content html_list in
      let fd = Out_channel.create ("crawl/" ^ sort ^ "-" ^ page ^ ".txt") in
        protect ~f:(fun () ->
          Out_channel.output_string fd (String.concat ~sep:"\n@@@@\n" final_list);
          printf "Done!\n"
        )
        ~finally:(fun () -> Out_channel.close fd; Shutdown.shutdown 0)

let get_question uri =
    get_html uri
    >>| fun html -> html

let rec crawl_question url_list =
     match url_list with
     | [] -> []
     | x::xs -> (get_question (question_url x))::crawl_question xs

let crawl_question_url sort page =
    ignore(Unix.mkdir "crawl");
    ignore(get_html (query_uri sort page)
    >>| fun url_list -> 
          let filtered = filter_question url_list in
              Deferred.all (crawl_question filtered)
              >>| fun html_list -> print_result html_list sort page);
    Deferred.never ()


let () =
    Command.async_basic
    ~summary:"Crawl questions from StackOverflow"
    Command.Spec.(
        empty
        +> anon ("[Sort Type]" %:string)
        +> anon ("[Page Number]" %:string)
    )
    (fun sort page () -> crawl_question_url sort page)
    |> Command.run ~version:"0.1" ~build_info:"ENGG5103"


