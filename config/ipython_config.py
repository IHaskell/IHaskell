# Available Subsitutions:
# ${executable}:    Path to IHaskell kernel.
c = get_config()
exe = '${executable}'.replace(' ', '\\\\ ')
c.KernelManager.kernel_cmd = [exe, 'kernel', '{connection_file}']
c.Session.key = ''
c.Session.keyfile = ''
