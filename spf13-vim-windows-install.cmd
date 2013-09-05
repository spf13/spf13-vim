@if not exist "%HOME%" @set HOME=%HOMEDRIVE%%HOMEPATH%
@if not exist "%HOME%" @set HOME=%USERPROFILE%

@set BASE_DIR=%HOME%\.spf13-vim-3
IF NOT EXIST "%BASE_DIR%" (
  call git clone --recursive -b 3.0 https://github.com/spf13/spf13-vim.git "%BASE_DIR%"
) ELSE (
	@set ORIGINAL_DIR=%CD%
    echo updating spf13-vim
    chdir /d "%BASE_DIR%" 
	call git pull
    chdir /d "%ORIGINAL_DIR%"
	call cd "%BASE_DIR%" 
)

call mklink "%HOME%\.vimrc" "%BASE_DIR%\.vimrc"
call mklink "%HOME%\_vimrc" "%BASE_DIR%\.vimrc"
call mklink "%HOME%\.vimrc.fork" "%BASE_DIR%\.vimrc.fork"
call mklink "%HOME%\.vimrc.bundles" "%BASE_DIR%\.vimrc.bundles"
call mklink "%HOME%\.vimrc.bundles.fork" "%BASE_DIR%\.vimrc.bundles.fork"
call mklink "%HOME%\.vimrc.before" "%BASE_DIR%\.vimrc.before"
call mklink "%HOME%\.vimrc.before.fork" "%BASE_DIR%\.vimrc.before.fork"
call mklink /J "%HOME%\.vim" "%BASE_DIR%\.vim"

IF NOT EXIST "%BASE_DIR%\.vim\bundle" (
	call mkdir "%BASE_DIR%\.vim\bundle"
)

IF NOT EXIST "%HOME%/.vim/bundle/vundle" (
	call git clone https://github.com/gmarik/vundle.git "%HOME%/.vim/bundle/vundle"
)

call vim -u "%BASE_DIR%/.vimrc.bundles" +BundleInstall! +BundleClean +qall
