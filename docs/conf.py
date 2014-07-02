import sys
import os

extensions = [
    'sphinx.ext.pngmath',
    'sphinx.ext.graphviz'
]

templates_path = ['_templates']
source_suffix = '.rst'
master_doc = 'index'

project = 'Corvus'
copyright = '2014, Fernando Borretti'

version = '0.1'
release = '0.1'

exclude_patterns = ['_build']

pygments_style = 'sphinx'
highlight_language = 'cl'

html_theme = 'haiku'

html_static_path = ['_static']

html_use_smartypants = True

htmlhelp_basename = 'Corvusdoc'


latex_elements = {
# The paper size ('letterpaper' or 'a4paper').
#'papersize': 'letterpaper',

# The font size ('10pt', '11pt' or '12pt').
#'pointsize': '10pt',

# Additional stuff for the LaTeX preamble.
#'preamble': '',
}

latex_documents = [
  ('index', 'Corvus.tex', 'Corvus Documentation',
   'Fernando Borretti', 'manual'),
]

man_pages = [
    ('index', 'corvus', 'Corvus Documentation',
     ['Fernando Borretti'], 1)
]

texinfo_documents = [
  ('index', 'Corvus', 'Corvus Documentation',
   'Fernando Borretti', 'Corvus', 'One line description of project.',
   'Miscellaneous'),
]
