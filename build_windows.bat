SET TYPHON_ROOT=C:\codetyphon
SET TYPHON=%TYPHON_ROOT%\typhon
SET FPC=%TYPHON_ROOT%\fpc\fpc64\bin\x86_64-win64\fpc.exe
SET ARCH=x86_64-win64

%FPC% -MObjFPC -Scghi -O1 -g -gl -WG -l -vewnhibq ^
	-Fibuild\%ARCH% ^
	-FUbuild\%ARCH% ^
	-Fu. -Fuform -Fubase -Fuutil -Fuview -Fuview\synedit -Fuapi ^
	-Fu%TYPHON%\lcl\units\%ARCH%\win32 ^
	-Fu%TYPHON%\lcl\units\%ARCH% ^
	-Fu%TYPHON%\components\BaseUtils\lib\%ARCH% ^
	-Fu%TYPHON%\packager\units\%ARCH% ^
	-dLCL -dLCLwin32 ^
	smaliide.ppr

