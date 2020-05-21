This file gives some basic information about the DOS Lemmings Clone programs I wrote.
(Original Lemmings, OhNo More Lemmings and Holiday 1993 Lemmings)
This directory should contain all files needed for recompilation with Delphi. If not let me know.
Because my time is limited at the moment, I decided to put all sourcecode online.
****************************************************************************************************
You may change, redistribute and recompile the sourcecode, but do not make money with it.
It is Freeware and must stay so. If you change the code I would like to know.
The use of the sourcecode is at your own risk.
Ok enough of this...
***************************************************************************************************

The lemming clone programs work with one single executable. All needed data is compiled into this
exe-file: the sounds, the levels, the graphics and even bassmod.dll (to play mod-files).
The only other external file, that is created by the exe is an ini-file, which contains some settings.
There are always 3 resourcefiles included. these files can be found in the subdirectories of the
different lemmings versions.
To minimize the size of the executable all these files are compressed.
In LemRes.pas is decided with conditional defines which files are imported into the executable.
1) lemdata.res (subdirectory data)
2) lemmusic.res (subdirectory music)
3) lemsounds.res (subdirectory sounds, currently the winlemmings-sounds are used)
4) explode.res (root directory)

Here is the complete list of files and some comments. Some files are empty or unused or obsolete,
but I am too lazy to filter them. If files are missing for a complete compilation, let me know:
ericlangedijk<AT>upcmail<DOT>nl

Cursors\DosGameCursor1.bmp                                     clone of the doscursor
Cursors\DosGameCursor2.bmp                                     clone of the doscursor (selection)
Cursors\DosGameCursorMask1.bmp                                 mask needed by windows for doscursor
Cursors\DosGameCursorMask2.bmp                                 mask needed by windows for doscursor (selection)
----------------------------------------------------------------------------------------------------
Data\h93\ADLIB.DAT                                             original dos file
Data\h93\GREET.DAT                                             original dos file
Data\h93\GROUND1O.DAT                                          original dos file
Data\h93\GROUND2O.DAT                                          original dos file
Data\h93\lemdata.arc                                           all compressed files
Data\h93\lemdata.rc                                            resource script
Data\h93\lemdata.RES                                           all compressed files in resource format
Data\h93\LEVEL000.DAT                                          original dos file
Data\h93\LEVEL001.DAT                                          original dos file
Data\h93\LEVEL002.DAT                                          original dos file
Data\h93\LEVEL003.DAT                                          original dos file
Data\h93\MAIN.DAT                                              original dos file
Data\h93\TANDYSND.DAT                                          original dos file
Data\h93\UNH93.EXE                                             original dos file
Data\h93\VGAGR1.DAT                                            original dos file
Data\h93\VGAGR2.DAT                                            original dos file
Data\h93\VGALEMMI.EXE                                          original dos file
----------------------------------------------------------------------------------------------------
Data\h94\ADLIB.DAT                                             original dos file
Data\h94\GREET.DAT                                             original dos file
Data\h94\GROUND1O.DAT                                          original dos file
Data\h94\GROUND2O.DAT                                          original dos file
Data\h94\LEMMINGS.BAT                                          original dos file
Data\h94\LEVEL000.DAT                                          original dos file
Data\h94\LEVEL001.DAT                                          original dos file
Data\h94\LEVEL002.DAT                                          original dos file
Data\h94\LEVEL003.DAT                                          original dos file
Data\h94\LEVEL004.DAT                                          original dos file
Data\h94\LEVEL005.DAT                                          original dos file
Data\h94\LEVEL006.DAT                                          original dos file
Data\h94\LEVEL007.DAT                                          original dos file
Data\h94\MAIN.DAT                                              original dos file
Data\h94\TANDYSND.DAT                                          original dos file
Data\h94\VGAGR1.DAT                                            original dos file
Data\h94\VGAGR2.DAT                                            original dos file
Data\h94\VGALEMMI.EXE                                          original dos file
----------------------------------------------------------------------------------------------------
Data\ohno\Dlvel000.dat                                         original dos file
Data\ohno\Dlvel001.dat                                         original dos file
Data\ohno\Dlvel002.dat                                         original dos file
Data\ohno\Dlvel003.dat                                         original dos file
Data\ohno\Dlvel004.dat                                         original dos file
Data\ohno\Dlvel005.dat                                         original dos file
Data\ohno\Dlvel006.dat                                         original dos file
Data\ohno\Dlvel007.dat                                         original dos file
Data\ohno\Dlvel008.dat                                         original dos file
Data\ohno\Dlvel009.dat                                         original dos file
Data\ohno\Dlvel010.dat                                         original dos file
Data\ohno\Dlvel011.dat                                         original dos file
Data\ohno\Dlvel012.dat                                         original dos file
Data\ohno\Ground0o.dat                                         original dos file
Data\ohno\Ground1o.dat                                         original dos file
Data\ohno\Ground2o.dat                                         original dos file
Data\ohno\Ground3o.dat                                         original dos file
Data\ohno\lemdata.arc                                          all compressed files
Data\ohno\lemdata.rc                                           resource script
Data\ohno\lemdata.res                                          all compressed files in resource format
Data\ohno\Main.dat                                             original dos file
Data\ohno\Vgagr0.dat                                           original dos file
Data\ohno\Vgagr1.dat                                           original dos file
Data\ohno\Vgagr2.dat                                           original dos file
Data\ohno\Vgagr3.dat                                           original dos file
----------------------------------------------------------------------------------------------------
Data\orig\GROUND0O.DAT                                         original dos file
Data\orig\GROUND1O.DAT                                         original dos file
Data\orig\GROUND2O.DAT                                         original dos file
Data\orig\GROUND3O.DAT                                         original dos file
Data\orig\GROUND4O.DAT                                         original dos file
Data\orig\lemdata.arc                                          all compressed files
Data\orig\lemdata.rc                                           resource script
Data\orig\lemdata.res                                          all compressed files in resource format
Data\orig\LEVEL000.DAT                                         original dos file
Data\orig\LEVEL001.DAT                                         original dos file
Data\orig\LEVEL002.DAT                                         original dos file
Data\orig\LEVEL003.DAT                                         original dos file
Data\orig\LEVEL004.DAT                                         original dos file
Data\orig\LEVEL005.DAT                                         original dos file
Data\orig\LEVEL006.DAT                                         original dos file
Data\orig\LEVEL007.DAT                                         original dos file
Data\orig\LEVEL008.DAT                                         original dos file
Data\orig\LEVEL009.DAT                                         original dos file
Data\orig\MAIN.DAT                                             original dos file
Data\orig\ODDTABLE.DAT                                         original dos file
Data\orig\VGAGR0.DAT                                           original dos file
Data\orig\VGAGR1.DAT                                           original dos file
Data\orig\VGAGR2.DAT                                           original dos file
Data\orig\VGAGR3.DAT                                           original dos file
Data\orig\VGAGR4.DAT                                           original dos file
Data\orig\VGASPEC0.DAT                                         original dos file
Data\orig\VGASPEC1.DAT                                         original dos file
Data\orig\VGASPEC2.DAT                                         original dos file
Data\orig\VGASPEC3.DAT                                         original dos file
----------------------------------------------------------------------------------------------------
Data\remake\GROUND0O.DAT                                       original dos file
Data\remake\GROUND1O.DAT                                       original dos file
Data\remake\GROUND2O.DAT                                       original dos file
Data\remake\GROUND3O.DAT                                       original dos file
Data\remake\GROUND4O.DAT                                       original dos file
Data\remake\GROUND5O.DAT                                       original dos file
Data\remake\GROUND6O.DAT                                       original dos file
Data\remake\GROUND7O.DAT                                       original dos file
Data\remake\GROUND8O.DAT                                       original dos file
Data\remake\GROUND9O.DAT                                       original dos file
Data\remake\lemdata.arc                                        all compressed files
Data\remake\lemdata.rc                                         resource script
Data\remake\lemdata.res                                        all compressed files in resource format
Data\remake\LEVEL000.DAT                                       original dos file
Data\remake\LEVEL001.DAT                                       original dos file
Data\remake\LEVEL002.DAT                                       original dos file
Data\remake\LEVEL003.DAT                                       original dos file
Data\remake\LEVEL004.DAT                                       original dos file
Data\remake\LEVEL005.DAT                                       original dos file
Data\remake\LEVEL006.DAT                                       original dos file
Data\remake\LEVEL007.DAT                                       original dos file
Data\remake\LEVEL008.DAT                                       original dos file
Data\remake\LEVEL009.DAT                                       original dos file
Data\remake\LEVEL010.DAT                                       original dos file
Data\remake\LEVEL011.DAT                                       original dos file
Data\remake\LEVEL012.DAT                                       original dos file
Data\remake\LEVEL013.DAT                                       original dos file
Data\remake\LEVEL014.DAT                                       original dos file
Data\remake\LEVEL015.DAT                                       original dos file
Data\remake\LEVEL016.DAT                                       original dos file
Data\remake\LEVEL017.DAT                                       original dos file
Data\remake\MAIN.DAT                                           original dos file
Data\remake\section_00.bmp                                     bitmap for main menu
Data\remake\section_01.bmp                                     bitmap for main menu
Data\remake\section_02.bmp                                     bitmap for main menu
Data\remake\section_03.bmp                                     bitmap for main menu
Data\remake\section_04.bmp                                     bitmap for main menu
Data\remake\section_05.bmp                                     bitmap for main menu
Data\remake\VGAGR0.DAT                                         original dos file
Data\remake\VGAGR1.DAT                                         original dos file
Data\remake\VGAGR2.DAT                                         original dos file
Data\remake\VGAGR3.DAT                                         original dos file
Data\remake\VGAGR4.DAT                                         original dos file
Data\remake\VGAGR5.DAT                                         original dos file
Data\remake\VGAGR6.DAT                                         original dos file
Data\remake\VGAGR7.DAT                                         original dos file
Data\remake\VGAGR8.DAT                                         original dos file
Data\remake\VGAGR9.DAT                                         original dos file
Data\remake\VGASPEC0.DAT                                       original dos file
Data\remake\VGASPEC1.DAT                                       original dos file
Data\remake\VGASPEC2.DAT                                       original dos file
Data\remake\VGASPEC3.DAT                                       original dos file
----------------------------------------------------------------------------------------------------
Data\explode.arc                                               compressed explode data
Data\explode.dat                                               explode data (made by ccexplore)
Data\explode.rc                                                resourcescript
Data\explode.res                                               explode data in resource format
----------------------------------------------------------------------------------------------------
Graphics32\Packages\GR32_BDS2006.bdsproj                       graphics32
Graphics32\Packages\GR32_BDS2006.dpk                           graphics32
Graphics32\Packages\GR32_BDS2006.res                           graphics32
Graphics32\Packages\GR32_CB5.bpk                               graphics32
Graphics32\Packages\GR32_CB5.cpp                               graphics32
Graphics32\Packages\GR32_CB5.res                               graphics32
Graphics32\Packages\GR32_CB6.bpk                               graphics32
Graphics32\Packages\GR32_CB6.cpp                               graphics32
Graphics32\Packages\GR32_CB6.res                               graphics32
Graphics32\Packages\GR32_D2005.bdsproj                         graphics32
Graphics32\Packages\GR32_D2005.dpk                             graphics32
Graphics32\Packages\GR32_D2005.res                             graphics32
Graphics32\Packages\GR32_D5.dpk                                graphics32
Graphics32\Packages\GR32_D5.res                                graphics32
Graphics32\Packages\GR32_D6.dpk                                graphics32
Graphics32\Packages\GR32_D6.res                                graphics32
Graphics32\Packages\GR32_D6_CLX.dpk                            graphics32
Graphics32\Packages\GR32_D6_CLX.res                            graphics32
Graphics32\Packages\GR32_D7.cfg                                graphics32
Graphics32\Packages\GR32_D7.dcp                                graphics32
Graphics32\Packages\GR32_D7.dof                                graphics32
Graphics32\Packages\GR32_D7.dpk                                graphics32
Graphics32\Packages\GR32_D7.res                                graphics32
Graphics32\Packages\GR32_D7.~dpk                               graphics32
Graphics32\Packages\GR32_D7_CLX.dpk                            graphics32
Graphics32\Packages\GR32_D7_CLX.res                            graphics32
Graphics32\Packages\GR32_DSGN_BDS2006.bdsproj                  graphics32
Graphics32\Packages\GR32_DSGN_BDS2006.dpk                      graphics32
Graphics32\Packages\GR32_DSGN_BDS2006.res                      graphics32
Graphics32\Packages\GR32_DSGN_CB5.bpk                          graphics32
Graphics32\Packages\GR32_DSGN_CB5.cpp                          graphics32
Graphics32\Packages\GR32_DSGN_CB5.res                          graphics32
Graphics32\Packages\GR32_DSGN_CB6.bpk                          graphics32
Graphics32\Packages\GR32_DSGN_CB6.cpp                          graphics32
Graphics32\Packages\GR32_DSGN_CB6.res                          graphics32
Graphics32\Packages\GR32_DSGN_D2005.bdsproj                    graphics32
Graphics32\Packages\GR32_DSGN_D2005.dpk                        graphics32
Graphics32\Packages\GR32_DSGN_D5.dpk                           graphics32
Graphics32\Packages\GR32_DSGN_D5.res                           graphics32
Graphics32\Packages\GR32_DSGN_D6.dpk                           graphics32
Graphics32\Packages\GR32_DSGN_D6.res                           graphics32
Graphics32\Packages\GR32_DSGN_D6_CLX.dpk                       graphics32
Graphics32\Packages\GR32_DSGN_D6_CLX.res                       graphics32
Graphics32\Packages\GR32_DSGN_D7.cfg                           graphics32
Graphics32\Packages\GR32_DSGN_D7.dcp                           graphics32
Graphics32\Packages\GR32_DSGN_D7.dcu                           graphics32
Graphics32\Packages\GR32_DSGN_D7.dof                           graphics32
Graphics32\Packages\GR32_DSGN_D7.dpk                           graphics32
Graphics32\Packages\GR32_DSGN_D7.res                           graphics32
Graphics32\Packages\GR32_DSGN_D7_CLX.dpk                       graphics32
Graphics32\Packages\GR32_DSGN_D7_CLX.res                       graphics32
Graphics32\Packages\GR32_DSGN_K.dpk                            graphics32
Graphics32\Packages\GR32_DSGN_K.res                            graphics32
Graphics32\Packages\GR32_K.dpk                                 graphics32
Graphics32\Packages\GR32_K.res                                 graphics32
Graphics32\Contributors.txt                                    graphics32
Graphics32\GR32.inc                                            graphics32
Graphics32\GR32.pas                                            graphics32
Graphics32\GR32_Blend.pas                                      graphics32
Graphics32\GR32_Containers.pas                                 graphics32
Graphics32\GR32_DrawingEx.pas                                  graphics32
Graphics32\GR32_Dsgn_Bitmap.bak                                graphics32
Graphics32\GR32_Dsgn_Bitmap.dfm                                graphics32
Graphics32\GR32_Dsgn_Bitmap.pas                                graphics32
Graphics32\GR32_Dsgn_Bitmap.xfm                                graphics32
Graphics32\GR32_Dsgn_Color.pas                                 graphics32
Graphics32\GR32_Dsgn_Misc.pas                                  graphics32
Graphics32\GR32_ExtImage.pas                                   graphics32
Graphics32\GR32_Filters.pas                                    graphics32
Graphics32\GR32_Image.pas                                      graphics32
Graphics32\GR32_Layers.pas                                     graphics32
Graphics32\GR32_LowLevel.pas                                   graphics32
Graphics32\GR32_Math.pas                                       graphics32
Graphics32\GR32_MicroTiles.pas                                 graphics32
Graphics32\GR32_OrdinalMaps.pas                                graphics32
Graphics32\GR32_Polygons.pas                                   graphics32
Graphics32\GR32_RangeBars.pas                                  graphics32
Graphics32\GR32_Rasterizers.pas                                graphics32
Graphics32\GR32_Reg.dcr                                        graphics32
Graphics32\GR32_Reg.pas                                        graphics32
Graphics32\GR32_RepaintOpt.pas                                 graphics32
Graphics32\GR32_Resamplers.pas                                 graphics32
Graphics32\GR32_System.pas                                     graphics32
Graphics32\GR32_Transforms.pas                                 graphics32
Graphics32\GR32_VectorMaps.pas                                 graphics32
Graphics32\Graphics32.chm                                      graphics32
Graphics32\License.html                                        graphics32
Graphics32\License.txt                                         graphics32
Graphics32\Readme.txt                                          graphics32
Graphics32\_clean.bat                                          graphics32
----------------------------------------------------------------------------------------------------
Kernel\ZLib\adler32.c                                          core compression code
Kernel\ZLib\adler32.obj                                        core compression code
Kernel\ZLib\compress.c                                         core compression code
Kernel\ZLib\crc32.c                                            core compression code
Kernel\ZLib\deflate.c                                          core compression code
Kernel\ZLib\deflate.h                                          core compression code
Kernel\ZLib\deflate.obj                                        core compression code
Kernel\ZLib\DelphiZLib.bpr                                     core compression code
Kernel\ZLib\DelphiZLib.cpp                                     core compression code
Kernel\ZLib\DelphiZLib.zip                                     core compression code
Kernel\ZLib\example.c                                          core compression code
Kernel\ZLib\gzio.c                                             core compression code
Kernel\ZLib\infblock.c                                         core compression code
Kernel\ZLib\infblock.h                                         core compression code
Kernel\ZLib\infblock.obj                                       core compression code
Kernel\ZLib\infcodes.c                                         core compression code
Kernel\ZLib\infcodes.h                                         core compression code
Kernel\ZLib\infcodes.obj                                       core compression code
Kernel\ZLib\inffast.c                                          core compression code
Kernel\ZLib\inffast.h                                          core compression code
Kernel\ZLib\inffast.obj                                        core compression code
Kernel\ZLib\inffixed.h                                         core compression code
Kernel\ZLib\inflate.c                                          core compression code
Kernel\ZLib\inflate.obj                                        core compression code
Kernel\ZLib\inftrees.c                                         core compression code
Kernel\ZLib\inftrees.h                                         core compression code
Kernel\ZLib\inftrees.obj                                       core compression code
Kernel\ZLib\infutil.c                                          core compression code
Kernel\ZLib\infutil.h                                          core compression code
Kernel\ZLib\infutil.obj                                        core compression code
Kernel\ZLib\maketree.c                                         core compression code
Kernel\ZLib\minigzip.c                                         core compression code
Kernel\ZLib\README.TXT                                         core compression code
Kernel\ZLib\trees.c                                            core compression code
Kernel\ZLib\trees.h                                            core compression code
Kernel\ZLib\trees.obj                                          core compression code
Kernel\ZLib\uncompr.c                                          core compression code
Kernel\ZLib\zconf.h                                            core compression code
Kernel\ZLib\zlib.h                                             core compression code
Kernel\ZLib\ZLibEx.dcu                                         core compression code
Kernel\ZLib\ZLibEx.pas                                         core compression code
Kernel\ZLib\zutil.c                                            core compression code
Kernel\ZLib\zutil.h                                            core compression code
----------------------------------------------------------------------------------------------------
Kernel\UFiles.pas                                              file listing routines (only used by LemResource project)
Kernel\UMisc.pas                                               miscellaneous routines
Kernel\UTools.pas                                              miscellaneous routines
Kernel\UWintools.pas                                           miscellaneous windows routines (only used by LemResource project)
Kernel\UZip.pas                                                compression routines
Kernel\ZoZip.exe                                               exe to view compressed files (.arc)

----------------------------------------------------------------------------------------------------
DIRECTORY LemResource: contains files for building the LemResourceBuilder project which can build
compressed resourcefiles for the lemmings clones. LemResourceBuilder is a helper program.
----------------------------------------------------------------------------------------------------
LemResource\FMain.ddp                                          mainform LemResourceBuilder
LemResource\FMain.dfm                                          mainform LemResourceBuilder
LemResource\FMain.pas                                          mainform LemResourceBuilder (with code to create resources of lemmingsdata)
LemResource\LemResourceBuilder.cfg                             LemResourceBuilder project
LemResource\LemResourceBuilder.dof                             LemResourceBuilder project
LemResource\LemResourceBuilder.dpr                             LemResourceBuilder project
LemResource\LemResourceBuilder.dsk                             LemResourceBuilder project
LemResource\LemResourceBuilder.res                             LemResourceBuilder project
LemResource\UCodes.pas                                         obsolete

----------------------------------------------------------------------------------------------------
DIRECTORY Mechanics: contains e-mails and pseudocode for all lemmings game mechanics
----------------------------------------------------------------------------------------------------
KMechanics\@Lemmings_mechanics001.txt                          pseudo code mechanics
Mechanics\@Lemmings_mechanics002.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics003.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics004.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics005.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics006.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics007.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics008.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics009.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics010.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics011.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics012.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics013.txt                           pseudo code mechanics
Mechanics\@Lemmings_mechanics014.txt                           pseudo code mechanics
Mechanics\Lemmings_mechanics001.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics002.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics003.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics004.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics005.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics006.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics007.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics008.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics009.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics010.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics011.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics012.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics013.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics014.txt                            e-mail ccexplore --> ericlang
Mechanics\Lemmings_mechanics015.txt                            e-mail ccexplore --> ericlang
----------------------------------------------------------------------------------------------------
DIRECTORY Music: contains mod-files and resourcefiles
----------------------------------------------------------------------------------------------------
Music\amiga\ohno\lemmusic.arc                                  compressed file with all musics
Music\amiga\ohno\lemmusic.rc                                   resource script
Music\amiga\ohno\lemmusic.RES                                  compressed file with all musics in resource format
Music\amiga\ohno\track_01.mod                                  mod-file
Music\amiga\ohno\track_02.mod                                  mod-file
Music\amiga\ohno\track_03.mod                                  mod-file
Music\amiga\ohno\track_04.mod                                  mod-file
Music\amiga\ohno\track_05.mod                                  mod-file
Music\amiga\ohno\track_06.mod                                  mod-file
----------------------------------------------------------------------------------------------------
Music\amiga\orig\intro_00.mod                                  mod-file
Music\amiga\orig\lemmusic.arc                                  compressed file with all musics
Music\amiga\orig\lemmusic.rc                                   resource script
Music\amiga\orig\lemmusic.res                                  compressed file with all musics in resource format
Music\amiga\orig\track_01.mod                                  mod-file
Music\amiga\orig\track_02.mod                                  mod-file
Music\amiga\orig\track_03.mod                                  mod-file
Music\amiga\orig\track_04.mod                                  mod-file
Music\amiga\orig\track_05.mod                                  mod-file
Music\amiga\orig\track_06.mod                                  mod-file
Music\amiga\orig\track_07.mod                                  mod-file
Music\amiga\orig\track_08.mod                                  mod-file
Music\amiga\orig\track_09.mod                                  mod-file
Music\amiga\orig\track_10.mod                                  mod-file
Music\amiga\orig\track_11.mod                                  mod-file
Music\amiga\orig\track_12.mod                                  mod-file
Music\amiga\orig\track_13.mod                                  mod-file
Music\amiga\orig\track_14.mod                                  mod-file
Music\amiga\orig\track_15.mod                                  mod-file
Music\amiga\orig\track_16.mod                                  mod-file
Music\amiga\orig\track_17.mod                                  mod-file
Music\amiga\orig\track_18.mod                                  mod-file
Music\amiga\orig\track_19.mod                                  mod-file
Music\amiga\orig\track_20.mod                                  mod-file
Music\amiga\orig\track_21.mod                                  mod-file
----------------------------------------------------------------------------------------------------
DIRECTORY Replay: contains some replayfiles, generated by the lemmings program
----------------------------------------------------------------------------------------------------
Replay\Fun_01.lrb                                              replay file
Replay\Fun_01.txt                                              replay text file
Replay\Fun_03.lrb                                              replay file
Replay\Fun_03.txt                                              replay text file
Replay\Havoc_04.lrb                                            replay file
Replay\Havoc_04.txt                                            replay text file
Replay\Mayhem_05.lrb                                           replay file
Replay\Mayhem_05.txt                                           replay text file
Replay\Tame_01.lrb                                             replay file
Replay\Tame_01.txt                                             replay text file
Replay\Tricky_01.lrb                                           replay file
Replay\Tricky_01.txt                                           replay text file
----------------------------------------------------------------------------------------------------
DIRECTORY Sounds: contains all sounds.
----------------------------------------------------------------------------------------------------
Sounds\amiga\BasicFX.0.wav                                     wav-file
Sounds\amiga\BasicFX.1.wav                                     wav-file
Sounds\amiga\BasicFX.10.wav                                    wav-file
Sounds\amiga\BasicFX.2.wav                                     wav-file
Sounds\amiga\BasicFX.3.wav                                     wav-file
Sounds\amiga\BasicFX.4.wav                                     wav-file
Sounds\amiga\BasicFX.5.wav                                     wav-file
Sounds\amiga\BasicFX.6.wav                                     wav-file
Sounds\amiga\BasicFX.7.wav                                     wav-file
Sounds\amiga\BasicFX.8.wav                                     wav-file
Sounds\amiga\BasicFX.9.wav                                     wav-file
Sounds\amiga\FullFX.0.wav                                      wav-file
Sounds\amiga\FullFX.1.wav                                      wav-file
Sounds\amiga\FullFX.2.wav                                      wav-file
Sounds\amiga\FullFX.3.wav                                      wav-file
Sounds\amiga\FullFX.4.wav                                      wav-file
Sounds\amiga\FullFX.5.wav                                      wav-file
Sounds\amiga\FullFX.6.wav                                      wav-file
Sounds\amiga\FullFX.7.wav                                      wav-file
Sounds\amiga\FullFX.8.wav                                      wav-file
Sounds\amiga\FullFX.9.wav                                      wav-file
Sounds\amiga\noises.0.wav                                      wav-file
Sounds\amiga\noises.1.wav                                      wav-file
Sounds\amiga\noises.10.wav                                     wav-file
Sounds\amiga\noises.2.wav                                      wav-file
Sounds\amiga\noises.3.wav                                      wav-file
Sounds\amiga\noises.4.wav                                      wav-file
Sounds\amiga\noises.5.wav                                      wav-file
Sounds\amiga\noises.6.wav                                      wav-file
Sounds\amiga\noises.7.wav                                      wav-file
Sounds\amiga\noises.8.wav                                      wav-file
Sounds\amiga\noises.9.wav                                      wav-file
----------------------------------------------------------------------------------------------------
Sounds\funny\diescream.wav                                     wav-file
Sounds\orig\nero_dosorig_assignjob.wav                         wav-file
Sounds\orig\nero_dosorig_endbuild.wav                          wav-file
Sounds\orig\nero_dosorig_exit.wav                              wav-file
Sounds\orig\nero_dosorig_explode.wav                           wav-file
Sounds\orig\nero_dosorig_letsgo.wav                            wav-file
Sounds\orig\nero_dosorig_ohno.wav                              wav-file
Sounds\orig\nero_dosorig_openentry.wav                         wav-file
Sounds\orig\nero_dosorig_pinktrapdestroy.wav                   wav-file
Sounds\orig\nero_dosorig_selectbutton.wav                      wav-file
----------------------------------------------------------------------------------------------------
Sounds\speaker\sfx_exit_speaker.wav                            wav-file
Sounds\speaker\sfx_explosion_speaker.wav                       wav-file
Sounds\speaker\sfx_general_speaker.wav                         wav-file
----------------------------------------------------------------------------------------------------
Sounds\win\BANG.WAV                                            wav-file
Sounds\win\bell.wav                                            wav-file
Sounds\win\CHAIN.WAV                                           wav-file
Sounds\win\CHANGEOP.WAV                                        wav-file
Sounds\win\CHINK.WAV                                           wav-file
Sounds\win\DIE.WAV                                             wav-file
Sounds\win\DOOR.WAV                                            wav-file
Sounds\win\ELECTRIC.WAV                                        wav-file
Sounds\win\EXPLODE.WAV                                         wav-file
Sounds\win\FIRE.WAV                                            wav-file
Sounds\win\GLUG.WAV                                            wav-file
Sounds\win\lemsounds.arc                                       compressed sound files
Sounds\win\lemsounds.rc                                        resource script
Sounds\win\lemsounds.res                                       compressed sound files in resource format
Sounds\win\LETSGO.WAV                                          wav-file
Sounds\win\MANTRAP.WAV                                         wav-file
Sounds\win\MOUSEPRE.WAV                                        wav-file
Sounds\win\OHNO.WAV                                            wav-file
Sounds\win\OING.WAV                                            wav-file
Sounds\win\SCRAPE.WAV                                          wav-file
Sounds\win\SLICER.WAV                                          wav-file
Sounds\win\SPLASH.WAV                                          wav-file
Sounds\win\SPLAT.WAV                                           wav-file
Sounds\win\TENTON.WAV                                          wav-file
Sounds\win\THUD.WAV                                            wav-file
Sounds\win\THUNK.WAV                                           wav-file
Sounds\win\TING.WAV                                            wav-file
Sounds\win\YIPPEE.WAV                                          wav-file
----------------------------------------------------------------------------------------------------
AllLemmixProjects.bpg                                          project group for all clones
AllLemmixProjects.dsk                                          project group for all clones delphi file
AppController.pas                                              contains the main loop for the lemmings screens
bassmod.arc                                                    compressed file with bassmod.dll in it
BASSMOD.dll                                                    DLL for playing musics (mod-files)
bassmod.rc                                                     resource script
bassmod.res                                                    compressed file in resource format
DLLUnit.pas                                                    unit to load a DLL from memory instead of from disk
explode_readme.txt                                             notes about exploding lemmings (made by ccexplore)
explodearray.txt                                               "readable" file with explosion coordinates
FBaseDosForm.ddp
FBaseDosForm.dfm                                               
FBaseDosForm.pas                                               basic ancestor screen for our lemmings-screens
FMain.ddp
FMain.dfm
FMain.pas                                                      main form for the application, which is black and fullscreen and acts as background
GameBaseScreen.pas
GameControl.pas                                                contains basic class for playing a game 
GameInterfaces.pas                                             contains interface to toolbar (skillpanel)
GameLevelCodeScreen.pas                                        code for the screen to enter a level code
GameMenuScreen.pas                                             code for the main menu screen
GameModPlay.pas                                                wrapper around bassmod
GameOptionsScreen.pas                                          code for options screen (not used)
GamePostviewScreen.pas                                         code for screen after you played a level
GamePreviewScreen.pas                                          code for the preview screen (before you play a level)
GameReplay.pas                                                 replay classes
GameSkillPanel.pas                                             code for the skillpanel
GameSound.pas                                                  classes for playing musics and sounds
GameWindow.pas                                                 the gameplay window
LemAnimation.pas                                               not used
LemAnimationSet.pas                                            class for lemming animationset (wrapper class)
LemConsts.pas                                                  array for explosions, extracted from explode.dat (probably not used, but not sure)
LemCore.pas                                                    some core lemming types (actions, skills)
LemDosAnimationSet.pas                                         classes for animationsets for dos lemmings
LemDosBmp.pas                                                  classes for converting planar bitmaps as stored in the original lemmings files
LemDosCmp.pas                                                  classes for compression and decompression as used in the orginal lemmings files
LemDosGraphicSet.pas                                           class to store graphic sets
LemDosMainDat.pas                                              class dedicated to main.dat lemmins file
LemDosMisc.pas                                                 contains a palette conversion procedure
LemDosStructures.pas                                           some basic dos-structures (ground.dat etc)
LemDosStyle.pas                                                classes for style and levelsystem
LemEdit.pas                                                    not used
LemFileSystem.pas                                              empty, not used
LemGame.pas                                                    game mechanics
LemGraphicSet.pas                                              class for graphic sets (tiles, objects)
LemInteractiveObject.pas                                       classes for interactive objects
LemLemminiLoader.pas                                           not used
LemLevel.pas                                                   class with leveldata
LemLevelLoad.pas                                               not used
LemLevelSystem.pas                                             base classes for levelsystems
LemLVLLoader.pas                                               probably not used
LemMetaAnimation.pas                                           meta data of animations
LemMetaObject.pas                                              meta data of objects
LemMetaPiece.pas                                               meta data of basic piece
LemMetaTerrain.pas                                             meta data of terrains
LemMisc.pas                                                    empty not used
Lemmix.cfg                                                     not used
Lemmix.dof                                                     not used
Lemmix.dpr                                                     not used
Lemmix.dsk                                                     not used
Lemmix.res                                                     not used
LemmixPlayer.cfg                                               original lemmings project file
LemmixPlayer.dof                                               original lemmings project file
LemmixPlayer.dpr                                               original lemmings project file
LemmixPlayer.dsk                                               original lemmings project file
LemmixPlayer.res                                               original lemmings project file
LemmixPlayerH93.cfg                                            holiday 93 lemmings project file
LemmixPlayerH93.dof                                            holiday 93 lemmings project file
LemmixPlayerH93.dpr                                            holiday 93 lemmings project file
LemmixPlayerH93.res                                            holiday 93 lemmings project file
LemmixPlayerOhNo.cfg                                           ohno lemmings project file
LemmixPlayerOhNo.dof                                           ohno lemmings project file
LemmixPlayerOhNo.dpr                                           ohno lemmings project file
LemmixPlayerOhNo.dsk                                           ohno lemmings project file
LemmixPlayerOhNo.res                                           ohno lemmings project file
LemmixPlayerRemake.cfg                                         remake lemmings project file
LemmixPlayerRemake.dof                                         remake lemmings project file
LemmixPlayerRemake.dpr                                         remake lemmings project file
LemmixPlayerRemake.ini                                         remake lemmings project file
LemmixPlayerRemake.res                                         remake lemmings project file
LemmixResources.res                                            resource file with cursors
LemMusicSystem.pas                                             base class for musicsystem
LemPalette.pas                                                 not used
LemPiece.pas                                                   base class with lemmings-piece
LemRendering.pas                                               code for painting levels
LemRes.pas                                                     conditional defines (preprocessing) for compiler
LemSettings.pas                                                not used
LemSteel.pas                                                   classes for steel
LemStrings.pas                                                 all texts
LemStyle.pas                                                   baseclass for lemming styles
LemTemp.pas                                                    not used
LemTerrain.pas                                                 class for terrain
LemTriggers.pas                                                not used
LemTypes.pas                                                   some consts and types
lem_directives.inc                                             conditional defines (preprocessor) (Keyfile for compilation)
ohnocodes.txt                                                  levelcodes (maybe obsolete)
origcodes.txt                                                  levelcodes (maybe obsolete)
ReadMe.txt                                                     this file 



