
module My (myRestart, myXmonadBar, myStatusBar, myBitmapsDir, myTerminal, myTabTheme, myScratchpads) where
  import XMonad
  import qualified XMonad.Util.Themes as Theme
  import XMonad.Util.NamedScratchpad
  import qualified XMonad.StackSet as W

  myRestart  = "for pid in `pgrep conky`; do kill -9 $pid; done && " ++
        "for pid in `pgrep dzen2`; do kill -9 $pid; done && " ++
        "xmonad --recompile && xmonad --restart"

  myXmonadBar = "dzen2 -xs '0' -y '0' -h '14' -w '1440' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
  myStatusBar = "conky -c ~/.conkyrc | dzen2 -xs '2' -w '1440' -h '14' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0'"
  myBitmapsDir = "~/.xmonad/dzen2"

  myTerminal      = "gnome-terminal"

  myTabTheme = (Theme.theme Theme.xmonadTheme)

  myScratchpads = [
       NS "term" "gnome-terminal --role myterm -x tmux new-session -A -s workbench" (role =? "myterm")
           (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
       NS "notes" "gnome-terminal --role mynotes -x vim ~/notes/notes.txt" (role =? "mynotes")
           (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
       NS "mytodo" "gnome-terminal --role mytodo -x vim ~/notes/home/todo.txt" (role =? "mytodo")
           (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
       NS "worktodo" "gnome-terminal --role worktodo -x vim ~/notes/work/todo.txt" (role =? "worktodo")
           (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)),
       NS "htop" "gnome-terminal --role myhtop -x htop" (role =? "myhtop") defaultFloating
   ] where role = stringProperty "WM_WINDOW_ROLE"
