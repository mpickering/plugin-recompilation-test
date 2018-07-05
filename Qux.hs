module Qux (plugin) where
import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
                         installCoreToDos = install,
                         pluginRecompile = purePlugin
                           }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
      return (CoreDoPluginPass "Say name" pass : todo)

pass :: ModGuts -> CoreM ModGuts
pass = bindsOnlyPass (mapM printBind)
  where printBind :: CoreBind -> CoreM CoreBind
        printBind bndr@(NonRec b _) = do
                           putMsgS $ "Non-recursive binding named " ++ showSDocUnsafe (ppr b)
                           return bndr
        printBind bndr = return bndr
