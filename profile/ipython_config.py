# Available Variables:
# exe:    Path to IHaskell kernel.
c = get_config()
c.KernelManager.kernel_cmd = [exe, 'kernel', '{connection_file}']
c.Session.key = ''
c.Session.keyfile = ''
