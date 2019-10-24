# Some trip

# Vim
## Upgrade to 8.0
```
sudo add-apt-repository ppa:jonathonf/vim
sudo apt update
sudo apt install vim
```


## Uninstall Vim 8.0:
To uninstall Vim 8.0 and downgrade it to the stock version in Ubuntu repository, run the command below to purge the PPA:

```
sudo apt install ppa-purge && sudo ppa-purge ppa:jonathonf/vim
```

## +lua

```
sudo apt install vim-gtk/nox
```
It may improve the autocomplete feature

