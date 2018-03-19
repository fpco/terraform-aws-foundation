output "user_data" {
  value       = "${template_file.generic_init.rendered}"
  description = "The template, rendered"
}
