@echo off
setlocal
set "this=%~dpn0"
ros -Q -L sbcl-bin -- "%this%.ros"  %*