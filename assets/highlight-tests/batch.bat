@echo off
REM --- String, Variable, Label, Command, Operator, Number, Delimiter, Comment ---

:: Label
:Start

:: Variable assignment and usage
set "VAR1=Hello"
set VAR2=World

:: String with spaces and special characters
set "STR=Batch ^& CMD!"

:: Arithmetic operation (number, operator)
set /a SUM=5+10

:: IF statement (keyword, operator, string, variable)
if "%VAR1%"=="Hello" (
    echo %VAR1%, %VAR2%! %STR%
) else (
    echo Not matched!
)

:: FOR loop (keyword, variable, delimiter, string)
for %%F in (*.bat) do (
    echo Found file: %%F
)

:: CALL command (keyword, label)
call :SubRoutine

:: GOTO command (keyword, label)
goto :End

:: Subroutine with parameter
:SubRoutine
echo In subroutine with SUM=%SUM%
goto :eof

:End
REM End of script
