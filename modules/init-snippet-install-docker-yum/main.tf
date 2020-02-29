data "template_file" "init_snippet" {
  template = file("${path.module}/snippet.tpl")
}

output "init_snippet" {
  value = data.template_file.init_snippet.rendered
}

