@echo off
CHCP  936
title 模版工具
cls
::color 0A
:cho

echo.
echo ==========================================
echo    1. 导入
echo    2. 导出
echo    3. 导入全部
echo    4. 导出全部
echo ==========================================
:choice
set choice=
set /p choice=          请选择: 
if /i "%choice%"=="1" goto input_one
if /i "%choice%"=="2" goto output_one
if /i "%choice%"=="3" goto input_all
if /i "%choice%"=="4" goto output_all
echo 输入错误，请重新输入
echo.
goto cho

:input_all
echo. 
echo 正在导入...
db_tool.exe input_all

goto end

:output_all
echo. 
echo 正在导出...
db_tool.exe output_all

goto end


:input_one
set /p table=       请输入表名: 
echo. 
echo 正在导入...
db_tool.exe input_one %table%
goto end

:output_one
set /p table=       请输入表名: 
echo. 
echo 正在导出...
db_tool.exe output_one %table%
goto end

:end
goto :cho

:quit