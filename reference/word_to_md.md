# Convert a Word File to Markdown with Optional Embedded Images

This function converts a Word (\`.docx\`) file to Markdown using Pandoc.
Optionally, it embeds images as Base64 for a self-contained Markdown
file.

## Usage

``` r
word_to_md(
  docx_file,
  output_file = NULL,
  embed_images = FALSE,
  overwrite = FALSE
)
```

## Arguments

- docx_file:

  Path to the input Word file.

- output_file:

  Path to the final Markdown output file. If null, the original file
  name.

- embed_images:

  Logical. If \`TRUE\`, all images will be embedded as Base64. Default
  is \`FALSE\`.

- overwrite:

  Logical. If \`TRUE\`, Output file is overwrote. Default is \`FALSE\`

## Value

Saves a Markdown file (optionally with Base64-embedded images).

## Examples

``` r
# Convert Word to Markdown without embedding images
if (FALSE) { # \dontrun{
word_to_md("input.docx", "output.md", embed_images = FALSE)

# Convert and embed images as Base64
word_to_md("input.docx", "output_embedded.md", embed_images = TRUE)
} # }
```
