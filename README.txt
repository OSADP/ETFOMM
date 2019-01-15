Welcome to Enhanced Transportation Flow Open-source Microscopic Model (ETFOMM) project. 

ETFOMM inherits 40 years of FHWA development of traffic simulation algorithms and flow theories while overcoming CORSIM’s limitations in supporting research. For more detail functions about ETFOMM, please refer to https://www.fhwa.dot.gov/publications/research/operations/17028/17028.pdf. For tutoring, technical support and future updates, please visit ETFOMM.org or email etfomm@ngsim.com.

This project also includes ETFOMM Application Programming Interface (ETAPI) that provides communications between ETFOMM core simulation engine and user-developed applications (Apps) through ETRunner, a Windows console program.

What's new in (2.1) since last update

+Integration of open source dynamic traffic assignment DTA Lite/ETFOMM/SSAM
+Directly add roadway geometric and elevation data
+Automatically build highway, light rail, tram  and surface street networks
+Export to 3D Max
+Explicitly model pedestrians
+Push button to traffic signal controller functions
+Tram/Light rail signal controller and priority
+NTCIP Based Transit Priority and preemption (work in progress)
+V2I Tram/Light rail control to reduce/eliminate stop at highway crossing/intersections (work in progress)
+Explicitly model tram/light rail and movement
+Feet/mile/meter/GPS Coordinates
+Large network editing: Cut/merge Networks
+HOV/Buses (Routes on Freeway)
+Dilemma zone safety (Decision control system)
+Ramp Meters/Freeway Detectors
+Advanced/Mixed Car Following logic with CAV
+Import/Export TRF files: CORSIM/SIMTRAFFIC-Synchro
+Direct Import DTA Lite Files
+Many improvements to Vehicle Movement, Intersection Model and Animation
+Multiple Run SSAM Summary for Project Level Safety Analysis

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



