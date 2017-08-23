Welcome to Enhanced Transportation Flow Open-source Microscopic Model (ETFOMM) project. 

ETFOMM inherits 40 years of FHWA development of traffic simulation algorithms and flow theories while overcoming CORSIM’s limitations in supporting research. For more detail functions about ETFOMM, please refer to https://www.fhwa.dot.gov/publications/research/operations/17028/17028.pdf. For tutoring and technical support, please visit ETFOMM.org or email ngsim@ngsim.com.

This project also includes ETFOMM Application Programming Interface (ETAPI) that provides communications between ETFOMM core simulation engine and user-developed applications (Apps) through ETRunner, a Windows console program.

Here are the folders included in this project:

Documentations: this folder contains documents of ETFOMM project, including:
	ETFOMM Reference Manual Vol 1
	ETFOMM Reference Manual Vol 2
	ETFOMM Build Instructions
	ETFOMM Application Programming Interface Reference
	ETFOMM Application Programming Interface Report

ETAPIEXE: this folder contains the executable files of the console program that runs ETFOMM and a console ETAPI client program, along with dependencies. Please download ETAPIEXE.zip to obtain all files in this folder.


ETAPISourceCode: this folder contains the Visual Studio 2012 solution files for ETAPI. Please download ETAPISourceCode.zip to obtain all files in this folder.
	APIClient: this folder contains the source code of a console ETAPI client program.
	eTFOMM Web: this folder contains the source code of a web client program.
	etRunner64: this folder contains the source code of the console program that runs ETFOMM.
	include: this folder contains the head file shared by all programs in this solution.
	WCFServer: this folder contains the source code of the WCF Server used by etRunner and client programs.

ETFOMMDLL: this folder contains 32-bit and 64-bit versions of ETFOMM DLL. 

ETFOMMEXE: this folder contains the executable file of the console program that runs ETFOMM and all dependencies. Please download ETFOMMEXE.zip to obtain all files in this folder.

ETFOMMSourceCode: this folder contains the Visual Studio 2012 solution files for ETFOMM. This project can be used to modify and compile ETFOMM DLL. Please download ETFOMMSourceCode.zip to obtain all files in this folder.

NOTE: ETFOMM GUI application is proprietary software and is not publicly available. Please contact ngsim@ngsim.com or (662)-341-5724 for details.



