@echo off
CHCP  936
title ģ�湤��
cls
::color 0A
:cho

echo.
echo ==========================================
echo    1. ����
echo    2. ����
echo    3. ����ȫ��
echo    4. ����ȫ��
echo ==========================================
:choice
set choice=
set /p choice=          ��ѡ��: 
if /i "%choice%"=="1" goto input_one
if /i "%choice%"=="2" goto output_one
if /i "%choice%"=="3" goto input_all
if /i "%choice%"=="4" goto output_all
echo �����������������
echo.
goto cho

:input_all
echo. 
echo ���ڵ���...
db_tool.exe input_all

goto end

:output_all
echo. 
echo ���ڵ���...
db_tool.exe output_all

goto end


:input_one
set /p table=       ���������: 
echo. 
echo ���ڵ���...
db_tool.exe input_one %table%
goto end

:output_one
set /p table=       ���������: 
echo. 
echo ���ڵ���...
db_tool.exe output_one %table%
goto end

:end
goto :cho

:quit