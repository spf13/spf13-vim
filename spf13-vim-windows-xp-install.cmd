@if not exist "%HOME%" @set HOME=%HOMEDRIVE%%HOMEPATH%
@if not exist "%HOME%" @set HOME=%USERPROFILE%

@set BASE_DIR=%HOME%\.spf13-vim-3
call git clone --recursive -b 3.0 git://github.com/spf13/spf13-vim.git "%BASE_DIR%"
call mkdir "%BASE_DIR%\.vim\bundle"
call copy "%BASE_DIR%\.vim" "%HOME%\.vim"
call copy "%BASE_DIR%\.vimrc" "%HOME%\.vimrc"
call copy "%BASE_DIR%\.vimrc" "%HOME%\_vimrc"
call copy "%BASE_DIR%\.vimrc.bundles" "%HOME%\.vimrc.bundles"

call git clone http://github.com/gmarik/vundle.git "%HOME%/.vim/bundle/vundle"
call vim -u "$BASE_DIR/.vimrc.bundles" - +BundleInstall! +BundleClean +qall
