# Microsoft Developer Studio Project File - Name="PhenologyMMS_NCB" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=PhenologyMMS_NCB - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "PhenologyMMS_NCB.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PhenologyMMS_NCB.mak" CFG="PhenologyMMS_NCB - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PhenologyMMS_NCB - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "PhenologyMMS_NCB - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "PhenologyMMS_NCB - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "PhenologyMMS_NCB - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "PhenologyMMS_NCB - Win32 Release"
# Name "PhenologyMMS_NCB - Win32 Debug"
# Begin Source File

SOURCE=.\canopycn.for
# End Source File
# Begin Source File

SOURCE=.\canopyhm.for
# End Source File
# Begin Source File

SOURCE=.\canopyht.for
# End Source File
# Begin Source File

SOURCE=.\canopypm.for
# End Source File
# Begin Source File

SOURCE=.\canopysb.for
# End Source File
# Begin Source File

SOURCE=.\canopysf.for
# End Source File
# Begin Source File

SOURCE=.\canopysg.for
# End Source File
# Begin Source File

SOURCE=.\canopysw.for
# End Source File
# Begin Source File

SOURCE=.\canopywb.for
# End Source File
# Begin Source File

SOURCE=.\canopyww.for
# End Source File
# Begin Source File

SOURCE=.\date1.for
# End Source File
# Begin Source File

SOURCE=.\daylth.for
# End Source File
# Begin Source File

SOURCE=.\echo.for
# End Source File
# Begin Source File

SOURCE=.\emerge.for
# End Source File
# Begin Source File

SOURCE=.\gddcalc.for
# End Source File
# Begin Source File

SOURCE=.\initcepv.for
# End Source File
# Begin Source File

SOURCE=.\initcorn.for
# End Source File
# Begin Source File

SOURCE=.\initday.for
# End Source File
# Begin Source File

SOURCE=.\initgs.for
# End Source File
# Begin Source File

SOURCE=.\initparm.for
# End Source File
# Begin Source File

SOURCE=.\initsunf.for
# End Source File
# Begin Source File

SOURCE=.\initwthr.for
# End Source File
# Begin Source File

SOURCE=.\output.for
# End Source File
# Begin Source File

SOURCE=.\phenol.for
# End Source File
# Begin Source File

SOURCE=.\phenolcn.for
# End Source File
# Begin Source File

SOURCE=.\phenolhm.for
# End Source File
# Begin Source File

SOURCE=.\PhenologyMMS.fi
# End Source File
# Begin Source File

SOURCE=.\PhenologyMMS.rc
# End Source File
# Begin Source File

SOURCE=.\PhenologyMMS_NCB.for
# End Source File
# Begin Source File

SOURCE=.\phenolpm.for
# End Source File
# Begin Source File

SOURCE=.\phenolsb.for
# End Source File
# Begin Source File

SOURCE=.\phenolsf.for
# End Source File
# Begin Source File

SOURCE=.\phenolsg.for
# End Source File
# Begin Source File

SOURCE=.\phenolsw.for
# End Source File
# Begin Source File

SOURCE=.\phenolwb.for
# End Source File
# Begin Source File

SOURCE=.\phenolww.for
# End Source File
# Begin Source File

SOURCE=.\phyllo.for
# End Source File
# Begin Source File

SOURCE=.\raicalc.for
# End Source File
# Begin Source File

SOURCE=.\Resource.fd
# End Source File
# Begin Source File

SOURCE=.\setup.for
# End Source File
# Begin Source File

SOURCE=.\vernaliz.for
# End Source File
# End Target
# End Project
