c = get_config()

# QtConsole try to guess base on Python lexing when the input is done to auto
# execute.  This Fails on Haskell, and while it is not possible to do the
# lexing in the kernel just deactivate functionality
c.IPythonWidget.execute_on_complete_input = False
