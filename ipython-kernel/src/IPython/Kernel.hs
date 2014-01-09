-- | This module exports all the types and functions necessary to create an
-- IPython language kernel that supports the @ipython console@ and @ipython
-- notebook@ frontends. 
module IPython.Kernel (
  module IPython.Types,
  module IPython.Message.Writer,
  module IPython.Message.Parser,
  module IPython.Message.UUID,
  module IPython.ZeroMQ,
  ) where

import IPython.Types
import IPython.Message.Writer
import IPython.Message.Parser
import IPython.Message.UUID
import IPython.ZeroMQ
