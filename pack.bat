@echo off
if .%1==. goto help

if exist tmp\* rd /q /s tmp
md tmp
md tmp\src

call clean.bat
call build.bat
copy *.pas tmp\src\
copy *.bat tmp\src\
copy *.exe tmp\
copy *.cfg tmp\
copy *.diz tmp\

rar a -r0 -ep1 %1 tmp\*

rd /q /s tmp
call clean.bat

goto end

:help
echo usage: %0 filename

:end
