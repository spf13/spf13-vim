# Background
This branch feature/vim-plug is an attempt to replace [Vundle](https://github.com/VundleVim/Vundle.vim) with [Vim Plug](https://github.com/junegunn/vim-plug)  
The main reason being able to install/update/remove of the plugins asynchronously 

.vimrc has included .vim_plug.unplug which has the UnPlug command  
so that the user can remove unwanted plugins based from the default list, just like Vundle 

For example, to replace [Syntastic](https://github.com/vim-syntastic/syntastic) with [ALE](https://github.com/w0rp/ale)

In $HOME/.vimrc.bundles.local  
````
Plug 'w0rp/ALE'
UnPlug 'syntastic'
````

The user is to run
````
vim +PlugInstall +PlugClean!
````

Contributions are welcome!
