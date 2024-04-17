@echo off

set current_dir=%CD%
set script_dir=%~dp0
cd %script_dir%\..\lib

if exist .\eprintf.dll (
   del .\eprintf.dll
)

gcc -I ./ -shared -o eprintf.dll eprintf.c

cd %script_dir%\..\

if exist .\release (
   rmdir /S /Q .\release
)

mkdir .\release
mkdir .\release\youdown
mkdir .\release\youdown\lib

copy .\bin\youdown.bat .\release\youdown.bat
copy .\src\youdown.el .\release\youdown\youdown.el
copy .\lib\eprintf.dll .\release\youdown\lib\eprintf.dll
copy .\lib\master-lib.el .\release\youdown\lib\master-lib.el

powershell compress-archive .\release\* .\release\youdown

cd %current_dir%
