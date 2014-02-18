@if not exist "%HOME%" @set HOME=%HOMEDRIVE%%HOMEPATH%
@if not exist "%HOME%" @set HOME=%USERPROFILE%

@set APP_DIR=%HOME%\.spf13-vim-3
IF NOT EXIST "%APP_DIR%" (
  call git clone --recursive -b 3.0 https://github.com/spf13/spf13-vim.git "%APP_DIR%"
) ELSE (
	@set ORIGINAL_DIR=%CD%
    echo updating spf13-vim
    chdir /d "%APP_DIR%" 
	call git pull
    chdir /d "%ORIGINAL_DIR%"
	call cd "%APP_DIR%" 
)

call mklink "%HOME%\.vimrc" "%APP_DIR%\.vimrc"
call mklink "%HOME%\_vimrc" "%APP_DIR%\.vimrc"
call mklink "%HOME%\.vimrc.fork" "%APP_DIR%\.vimrc.fork"
call mklink "%HOME%\.vimrc.bundles" "%APP_DIR%\.vimrc.bundles"
call mklink "%HOME%\.vimrc.bundles.fork" "%APP_DIR%\.vimrc.bundles.fork"
call mklink "%HOME%\.vimrc.before" "%APP_DIR%\.vimrc.before"
call mklink "%HOME%\.vimrc.before.fork" "%APP_DIR%\.vimrc.before.fork"
call mklink /J "%HOME%\.vim" "%APP_DIR%\.vim"

IF NOT EXIST "%APP_DIR%\.vim\bundle" (
	call mkdir "%APP_DIR%\.vim\bundle"
)

IF NOT EXIST "%HOME%/.vim/bundle/vundle" (
	call git clone https://github.com/gmarik/vundle.git "%HOME%/.vim/bundle/vundle"
)

call vim -u "%APP_DIR%/.vimrc.bundles" +BundleInstall! +BundleClean +qall
