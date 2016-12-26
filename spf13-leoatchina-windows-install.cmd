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

@set APP_PATH=%~dp0
echo "%APP_PATH%"
call mklink "%HOME%/.vimrc" "%APP_PATH%/.vimrc"
call mklink "%HOME%/_vimrc" "%APP_PATH%/.vimrc"
call mklink "%HOME%/.vimrc.bundles" "%APP_PATH%/.vimrc.bundles"
call mklink "%HOME%/.vimrc.before" "%APP_PATH%/.vimrc.before"

call mkdir "%HOME%/.vim"

IF NOT EXIST "%HOME%/.vim/bundle" (
    call mkdir "%HOME%/.vim/bundle"
)

IF NOT EXIST "%HOME%/.vim/bundle/vundle" (
    call git clone https://github.com/gmarik/vundle.git "%HOME%/.vim/bundle/vundle"
) ELSE (
  call cd "%HOME%/.vim/bundle/vundle"
  call git pull
  call cd %HOME%
)

call vim -u "%HOME%/.vimrc.bundles" +BundleInstall! +BundleClean +qall
