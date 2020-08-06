@echo off
REM    Copyright 2014 Steve Francia
REM 
REM    Licensed under the Apache License, Version 2.0 (the "License");
REM    you may not use this file except in compliance with the License.
REM    You may obtain a copy of the License at
REM 
REM        http://www.apache.org/licenses/LICENSE-2.0
REM 
REM    Unless required by applicable law or agreed to in writing, software
REM    distributed under the License is distributed on an "AS IS" BASIS,
REM    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
REM    See the License for the specific language governing permissions and
REM    limitations under the License.


@if not exist "%HOME%" @set HOME=%HOMEDRIVE%%HOMEPATH%
@if not exist "%HOME%" @set HOME=%USERPROFILE%

@set APP_PATH=%HOME%\.spf13-vim-3
IF NOT EXIST "%APP_PATH%" (
    call git clone -b feature/vim-plug https://github.com/compleatguru/spf13-vim.git "%APP_PATH%"
) ELSE (
    @set ORIGINAL_DIR=%CD%
    echo updating spf13-vim
    chdir /d "%APP_PATH%"
    call git pull
    chdir /d "%ORIGINAL_DIR%"
    call cd "%APP_PATH%"
)

cp "%APP_PATH%\.vimrc" "%HOME%\.vimrc"
cp "%APP_PATH%\.vimrc" "%HOME%\_vimrc"
cp "%APP_PATH%\.vimrc.fork" "%HOME%\.vimrc.fork"
cp "%APP_PATH%\.vimrc.bundles" "%HOME%\.vimrc.bundles"
cp "%APP_PATH%\.vimrc.bundles.fork" "%HOME%\.vimrc.bundles.fork"
cp "%APP_PATH%\.vimrc.before" "%HOME%\.vimrc.before"
cp "%APP_PATH%\.vimrc.before.fork" "%HOME%\.vimrc.before.fork"
call mklink /J "%HOME%\.vim" "%APP_PATH%\.vim"

IF NOT EXIST "%APP_PATH%\.vim\bundle" (
    call mkdir "%APP_PATH%\.vim\bundle"
)

IF NOT EXIST "%HOME%/.vim/bundle/vim-plug" (
    call git clone https://github.com/junegunn/vim-plug.git "%HOME%/.vim/bundle/vim-plug"
) ELSE (
  REM Update Vim-Plug
  call cd "%HOME%/.vim/bundle/vim-plug"
  call git pull
  call cd %HOME%
)

call vim -u "%APP_PATH%/.vimrc.bundles" +PlugInstall! +PlugClean +qall
