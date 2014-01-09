# Available Variables:
# exe:    Path to IHaskell kernel.
c = get_config()
c.KernelManager.kernel_cmd = [exe, 'kernel', '{connection_file}']
c.Session.key = b''
c.Session.keyfile = b''

# Syntax highlight properly in Haskell notebooks. 
c.NbConvertBase.default_language = "haskell"

# Where to look for templates.
template_path = "/".join(__file__.split("/")[:-1] + ["templates"])
c.TemplateExporter.template_path = [template_path]
