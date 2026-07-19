let () =
  let content = In_channel.with_open_bin "test.markup" In_channel.input_all in
  let html = Markup.parse content in
  print_endline html
