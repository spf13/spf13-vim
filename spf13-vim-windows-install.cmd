@echo off
rem Try to write redundant file to windows directory to check admin rights
rem http://stackoverflow.com/a/4052002
copy /b/y NUL %WINDIR%\06CF2EB6-94E6-4a60-91D8-AB945AE8CF38 >NUL 2>&1
if errorlevel 1 goto:nonadmin
del %WINDIR%\06CF2EB6-94E6-4a60-91D8-AB945AE8CF38 >NUL 2>&1
:admin
@if not exist "%HOME%" @set HOME=%HOMEDRIVE%%HOMEPATH%
@if not exist "%HOME%" @set HOME=%USERPROFILE%

@set BASE_DIR=%HOME%\.spf13-vim-nb
IF NOT EXIST "%BASE_DIR%" (
    echo Installing spf13-vim (using neobundle^)
    call git clone --recursive -b nb https://github.com/spf13/spf13-vim.git "%BASE_DIR%"
    IF EXIST "%HOME%\.vimrc" (
    echo Backing up current .vimrc's...
        ren "%HOME%\.vimrc" ".vimrc.%DATE:~10,4%-%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    IF EXIST "%HOME%\_vimrc" (
        rename "%HOME%\_vimrc" "_vimrc.%DATE:~10,4%-%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    IF EXIST "%HOME%\.vimrc.fork" (
        rename "%HOME%\.vimrc.fork" ".vimrc.fork.%DATE:~10,4%-%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    IF EXIST "%HOME%\.vimrc.bundles" (
        rename "%HOME%\.vimrc.bundles" ".vimrc.bundles.%DATE:~10,4%-%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    IF EXIST "%HOME%\.vimrc.bundles.fork" (
        rename "%HOME%\.vimrc.bundles.fork" ".vimrc.bundles.fork.%DATE:~10,4%-%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    IF EXIST "%HOME%\.vimrc.before" (
        rename "%HOME%\.vimrc.before" ".vimrc.before%DATE:~10,4%-.%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    IF EXIST "%HOME%\.vimrc.before.fork" (
        rename "%HOME%\.vimrc.before.fork" ".vimrc.before.fork.%DATE:~10,4%-%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    IF EXIST "%HOME%\.vim" (
        rename "%HOME%\.vim" ".vim.%DATE:~10,4%-%DATE:~4,2%-%DATE:~7,2%.backup"
    )
    echo Creating symbolic links...
    call mklink "%HOME%\.vimrc" "%BASE_DIR%\.vimrc"
    call mklink "%HOME%\_vimrc" "%BASE_DIR%\.vimrc"
    call mklink "%HOME%\.vimrc.fork" "%BASE_DIR%\.vimrc.fork"
    call mklink "%HOME%\.vimrc.bundles" "%BASE_DIR%\.vimrc.bundles"
    call mklink "%HOME%\.vimrc.bundles.fork" "%BASE_DIR%\.vimrc.bundles.fork"
    call mklink "%HOME%\.vimrc.before" "%BASE_DIR%\.vimrc.before"
    call mklink "%HOME%\.vimrc.before.fork" "%BASE_DIR%\.vimrc.before.fork"
    call mklink /J "%HOME%\.vim" "%BASE_DIR%\.vim"
    echo Installing neobundle.vim
    IF NOT EXIST "%BASE_DIR%\.vim\bundle" (
        call mkdir "%BASE_DIR%\.vim\bundle"
    )

    IF NOT EXIST "%HOME%/.vim/bundle/neobundle" (
        call git clone https://github.com/Shougo/neobundle.vim "%HOME%/.vim/bundle/neobundle.vim"
    )
) ELSE (
    @set ORIGINAL_DIR=%CD%
    echo updating spf13-vim (using neobundle^)
    cd "%BASE_DIR%"
    call git pull
    cd "%ORIGINAL_DIR%"
    call cd "%BASE_DIR%"
)

call vim +NeoBundleInstall! +NeoBundleClean +qall
echo Installation finished, thank you for using spf13-vim!
pause
goto:eof
:nonadmin
echo You must 'Run as Administrator'
pause

