# Microsoft Developer Studio Project File - Name="PhenologyMMS" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=PhenologyMMS - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "PhenologyMMS.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "PhenologyMMS.mak" CFG="PhenologyMMS - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "PhenologyMMS - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "PhenologyMMS - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "PhenologyMMS - Win32 Release"

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
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "PhenologyMMS - Win32 Debug"

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
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "PhenologyMMS - Win32 Release"
# Name "PhenologyMMS - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\canopycn.for
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\canopyhm.for
DEP_F90_CANOP=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\canopyht.for
DEP_F90_CANOPY=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\canopypm.for
DEP_F90_CANOPYP=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\canopysb.for
DEP_F90_CANOPYS=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\canopysf.for
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\canopysg.for
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\canopysw.for
DEP_F90_CANOPYSW=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\canopywb.for
DEP_F90_CANOPYW=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\canopyww.for
DEP_F90_CANOPYWW=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\convtogdd.for
DEP_F90_CONVT=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\date1.for
DEP_F90_DATE1=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\daylth.for
DEP_F90_DAYLT=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\echo.for
# End Source File
# Begin Source File

SOURCE=.\emerge.for
DEP_F90_EMERG=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\gddcalc.for
DEP_F90_GDDCA=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\init.for
DEP_F90_INIT_=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\output.for
DEP_F90_OUTPU=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phenol.for
DEP_F90_PHENO=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phenolcn.for
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\phenolhm.for
DEP_F90_PHENOL=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\PhenologyMMS.for
DEP_F90_PHENOLO=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phenolpm.for
DEP_F90_PHENOLP=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phenolsb.for
DEP_F90_PHENOLS=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phenolsf.for
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\phenolsg.for
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=.\phenolsw.for
DEP_F90_PHENOLSW=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phenolwb.for
DEP_F90_PHENOLW=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phenolww.for
DEP_F90_PHENOLWW=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\phyllo.for
DEP_F90_PHYLL=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\raicalc.for
DEP_F90_RAICA=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\setup.for
DEP_F90_SETUP=\
	".\shtgro.inc"\
	
# End Source File
# Begin Source File

SOURCE=.\vernaliz.for
DEP_F90_VERNA=\
	".\shtgro.inc"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
