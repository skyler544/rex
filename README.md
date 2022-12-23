# rex

Yet another Emacs configuration. I doubt that you're interested, but if you are, here are the goals of this project:

* Consistent use of `use-package`
* Use of well-documented packages
* Stability

Comments in the code itself (wip) attempt to make certain aspects more clear.



Why is it called rex? I don't know. Maybe it's an acronym; maybe it's just short and easy to type.
I use [`with-emacs.sh`](https://github.com/alphapapa/with-emacs.sh) (alphapapa) for starting it, like this:

> `~/.local/bin/rex`  
> ```sh
> #!/bin/bash
> with-emacs.sh -R -d $HOME/build/rex/ -- $@
> ```

If you've just cloned it, you'll want to remove the `-R` flag (and set the path to wherever you cloned it) so that the packages can be installed. After that, using `-R` speeds up initialization.
