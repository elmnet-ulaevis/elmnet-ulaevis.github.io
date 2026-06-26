@echo off
setlocal

cd /d "%~dp0"

if not exist "_site\data.html" (
  echo ERROR: _site\data.html was not found.
  echo Place this file in the ElmNET project folder and run it again.
  pause
  exit /b 1
)

if not exist "elmnet_map_runtime_fix.js" (
  echo ERROR: elmnet_map_runtime_fix.js was not found.
  echo Place both files in the ElmNET project folder.
  pause
  exit /b 1
)

copy /Y "elmnet_map_runtime_fix.js" "_site\elmnet_map_runtime_fix.js" >nul

powershell -NoProfile -ExecutionPolicy Bypass -Command "$p='_site\data.html'; $c=[IO.File]::ReadAllText($p); if($c -notmatch 'elmnet_map_runtime_fix\.js'){ $c=$c.Replace('</body>','<script src=elmnet_map_runtime_fix.js></script></body>'); [IO.File]::WriteAllText($p,$c,(New-Object System.Text.UTF8Encoding($false))) }"

if errorlevel 1 (
  echo ERROR: data.html could not be updated.
  pause
  exit /b 1
)

echo.
echo ElmNET map fix installed in _site.
echo.
echo Test with the local _site server, then publish with:
echo quarto publish gh-pages --no-render
echo.
pause
