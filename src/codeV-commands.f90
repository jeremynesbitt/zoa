! THis module contains Code V style commands and translates them to KDP style commands
! TODO:
! Convert all subs here to new implemenation that accepts string input and zoa_cmd
! Add useful things to zoa_cmd.  Some ideas
! An identifier to describe the type of command (or commands) it is.  FOr example,
! a lens update command or a plot command or a command that updates a plot
! Or adding a description for help that explains the command
! or an identifier that explains the type of command it is for error checking (eg 
! command should be of format CMD Sk ACTION)
! Other items
! Should redirect output from UI to somewhere else (fake element) when calling KDP cmds
! from codeV cmds.  For example, if the user enters in THI SO 125, in the ui will see
! U L; CHO 0; TH 125; EOS.  While this is helpful for developers, not so for regular users
! So add some logic that if not in debug mode, in exec codeV cmd, start by redirect output to 
! fake textView, print code v cmd, then when done redirect output to window

module codeV_commands
    use codeV_legacy_wrappers
    use zoa_ui
    use iso_fortran_env, only: real64
    use plot_setting_manager, only: zoaplot_setting_manager
    use type_utils
    use zoa_output, only: zoa_emit
    use strings
    use globals
    
    implicit none
    interface
    module subroutine execSUR(iptStr)
    character(len=*) :: iptStr
    end subroutine execSUR
    module subroutine setPlotWavelength(iptStr)
   character(len=*) :: iptStr
   end subroutine setPlotWavelength
   module subroutine setPlotDensity(iptStr)
   character(len=*) :: iptStr
   end subroutine setPlotDensity
   module subroutine setPlotZernikeCoefficients(iptStr)
   character(len=*) :: iptStr
   end subroutine setPlotZernikeCoefficients
   module subroutine ZERN_TST(iptStr)
   character(len=*) :: iptStr
   end subroutine ZERN_TST
   module subroutine execVie(iptStr)
   character(len=*) :: iptStr
   end subroutine execVie
   module subroutine execRMSPlot(iptStr)
   character(len=*) :: iptStr
   end subroutine execRMSPlot
   module subroutine execSeidelBarChart(iptStr)
   character(len=*) :: iptStr
   end subroutine execSeidelBarChart
   module function adjustImageFocus(x) result(f)
   double precision, intent(in) :: x
   double precision :: f
   end function adjustImageFocus
   module subroutine findBestFocus(iptStr)
   character(len=*) :: iptStr
   end subroutine findBestFocus
   module subroutine execFAN(iptStr)
   character(len=*) :: iptStr
   end subroutine execFAN
   module function checkForExistingPlot(tokens, psm, plot_code) result(plotExists)
   character(len=*), dimension(:) :: tokens
   type(zoaplot_setting_manager), intent(inout) :: psm
   integer, intent(in) :: plot_code
   logical :: plotExists
   end function checkForExistingPlot
   module function initiatePlotLoop(iptStr, plot_code, psm) result(boolResult)
   character(len=*) :: iptStr
   integer :: plot_code
   type(zoaplot_setting_manager) :: psm
   logical :: boolResult
   end function initiatePlotLoop
   module subroutine setPlotScale(iptStr)
   character(len=*) :: iptStr
   end subroutine setPlotScale
   module subroutine updateMaxFrequency(iptStr)
   character(len=*) :: iptStr
   end subroutine updateMaxFrequency
   module subroutine updateFrequencyInterval(iptStr)
   character(len=*) :: iptStr
   end subroutine updateFrequencyInterval
   module subroutine execRayAberrationPlot(iptStr)
   character(len=*) :: iptStr
   end subroutine execRayAberrationPlot
   module subroutine execPMAPlot(iptStr)
   character(len=*) :: iptStr
   end subroutine execPMAPlot
   module subroutine execAstigFieldCurvDistPlot(iptStr)
   character(len=*) :: iptStr
   end subroutine execAstigFieldCurvDistPlot
   module subroutine execTOW(iptStr)
   character(len=*) :: iptStr
   end subroutine execTOW
   module subroutine execAUT(iptStr)
   character(len=*) :: iptStr
   end subroutine execAUT
   module subroutine execTAR(iptStr)
   character(len=*) :: iptStr
   end subroutine execTAR
   module subroutine execSPO(iptStr)
   character(len=*) :: iptStr
   end subroutine execSPO
   module subroutine execSPO_old(iptStr)
   character(len=*) :: iptStr
   end subroutine execSPO_old
   module subroutine execCIR(iptStr)
   character(len=*) :: iptStr
   end subroutine execCIR
   module subroutine execSTO(iptStr)
   character(len=*) :: iptStr
   end subroutine execSTO
   module subroutine execRestore(iptStr)
   character(len=*) :: iptStr
   end subroutine execRestore
   module subroutine execRestore_old(iptStr)
   character(len=*) :: iptStr
   end subroutine execRestore_old
   module subroutine exportLensToCodeV(iptStr)
   character(len=*) :: iptStr
   end subroutine exportLensToCodeV
   module subroutine exportLensToZemax(iptStr)
   character(len=*) :: iptStr
   end subroutine exportLensToZemax
   module subroutine execSAV(iptStr)
   character(len=*) :: iptStr
   end subroutine execSAV
   module subroutine execSaveSessionToFile(iptStr)
   character(len=*) :: iptStr
   end subroutine execSaveSessionToFile
   module subroutine execSetWavelengthIndex(iptStr)
   character(len=*) :: iptStr
   end subroutine execSetWavelengthIndex
   module subroutine execRMD(iptStr)
   character(len=*) :: iptStr
   end subroutine execRMD
   module subroutine execSetCodeVCmd()
   end subroutine execSetCodeVCmd
   module subroutine setMagSolve(iptStr)
   character(len=*) :: iptStr
   end subroutine setMagSolve
   module subroutine deleteStuff(iptStr)
   character(len=*) :: iptStr
   end subroutine deleteStuff
   module subroutine setEPD()
   end subroutine setEPD
   module subroutine setParaxialImageSolve()
   end subroutine setParaxialImageSolve
   module function getSetGlassText(strInput) result(strOut)
   character(len=*) :: strInput
   character(len=1024) :: strOut
   end function getSetGlassText
   module subroutine updateVarCodes(iptStr)
   character(len=*) :: iptStr
   end subroutine updateVarCodes
   module subroutine updateConstraint(iptStr)
   character(len=*) :: iptStr
   end subroutine updateConstraint
   module subroutine execNBR(iptStr)
   character(len=*) :: iptStr
   end subroutine execNBR
   module subroutine execCLI(iptStr)
   character(len=*) :: iptStr
   end subroutine execCLI
   module subroutine insertSurf(iptStr)
   character(len=*) :: iptStr
   end subroutine insertSurf
   module subroutine setDim(iptStr)
   character(len=*) :: iptStr
   end subroutine setDim
   module subroutine newLens()
   end subroutine newLens
   module subroutine resetToNewLensTemplate()
   end subroutine resetToNewLensTemplate
   module subroutine setLensTitle(iptStr)
   character(len=*) :: iptStr
   end subroutine setLensTitle
   module subroutine processFileComment(iptStr)
   character(len=*) :: iptStr
   end subroutine processFileComment
   module subroutine setFieldWeights(iptStr)
   character(len=*) :: iptStr
   end subroutine setFieldWeights
   module subroutine setWavelengthWeights(iptStr)
   character(len=*) :: iptStr
   end subroutine setWavelengthWeights
   module subroutine setField(iptStr)
   character(len=*) :: iptStr
   end subroutine setField
   module subroutine setWavelength(iptStr)
   character(len=*) :: iptStr
   end subroutine setWavelength
   module subroutine setSurfaceCodeVStyle(iptStr)
   character(len=*) :: iptStr
   end subroutine setSurfaceCodeVStyle
   module subroutine scaleSystem(iptStr)
   character(len=*) :: iptStr
   end subroutine scaleSystem
   module function getDefaultMaxFrequency() result(maxFreq)
   real :: maxFreq
   end function getDefaultMaxFrequency
   module subroutine updateOptimImprovementGoal(iptStr)
   character(len=*) :: iptStr
   end subroutine updateOptimImprovementGoal
   module subroutine updateRMSPlotType(iptStr)
   character(len=*) :: iptStr
   end subroutine updateRMSPlotType
   module subroutine aut_ui(iptStr)
   character(len=*) :: iptStr
   end subroutine aut_ui
   module subroutine updateDatabase(iptStr)
   character(len=*) :: iptStr
   end subroutine updateDatabase
   module subroutine changeDatabase(iptStr)
   character(len=*) :: iptStr
   end subroutine changeDatabase
   module subroutine evaluateCmd(iptStr)
   character(len=*) :: iptStr
   end subroutine evaluateCmd
   module function evalFunc(iptStr, logResult) result(res)
   character(len=*) :: iptStr
   logical, optional :: logResult
   real(kind=long) :: res
   end function evalFunc
   module subroutine listConstraints(iptStr)
   character(len=*) :: iptStr
   end subroutine listConstraints
   module subroutine deleteConstraints(iptStr)
   character(len=*) :: iptStr
   end subroutine deleteConstraints
   module subroutine executeGo()
   end subroutine executeGo
   module subroutine setLens()
   end subroutine setLens
   module subroutine flipSurfaces(iptStr)
   character(len=*) :: iptStr
   end subroutine flipSurfaces
   module function getSymmetryPlane(surfs) result(midPoint)
   integer, dimension(:) :: surfs
   real :: midPoint
   end function getSymmetryPlane
   module function isInputSurfaceParameter(iptStr) result(boolResult)
   character(len=*) :: iptStr
   logical :: boolResult
   end function isInputSurfaceParameter
   module subroutine setPickup(param1, si, sj, scale, offset, param2)
   character(len=*) :: param1
   integer :: si, sj
   real(long) :: scale, offset
   character(len=*), optional :: param2
   end subroutine setPickup
   module subroutine parsePickupInput(iptStr)
   character(len=*) :: iptStr
   end subroutine parsePickupInput
   module subroutine processZoaFileInput(iptStr, printOnly)
   character(len=*) :: iptStr
   logical, optional :: printOnly
   end subroutine processZoaFileInput
   module subroutine printFile(iptStr)
   character(len=*) :: iptStr
   end subroutine printFile
   module subroutine getRayData(iptStr)
   character(len=*) :: iptStr
   end subroutine getRayData
   module subroutine setThickness(iptStr)
    character(len=*) :: iptStr
   end subroutine setThickness
   module subroutine setGlass(iptStr)
    character(len=*) :: iptStr
   end subroutine setGlass
   module subroutine setCurvature(iptStr)
    character(len=*) :: iptStr
   end subroutine setCurvature
   module subroutine setCurvatureX(iptStr)
    character(len=*) :: iptStr
   end subroutine setCurvatureX
   module subroutine setRadius(iptStr)
    character(len=*) :: iptStr
   end subroutine setRadius
   module subroutine execPSF(iptStr)
    character(len=*) :: iptStr
   end subroutine execPSF
   module subroutine execMTF(iptStr)
    character(len=*) :: iptStr
   end subroutine execMTF
   module subroutine execAsphere(iptStr)
    character(len=*) :: iptStr
   end subroutine execAsphere
   module subroutine execSphere(iptStr)
    character(len=*) :: iptStr
   end subroutine execSphere
   module subroutine updateAsphereTerms(iptStr)
    character(len=*) :: iptStr
   end subroutine updateAsphereTerms
   module subroutine updateConicConstant(iptStr)
    character(len=*) :: iptStr
   end subroutine updateConicConstant
   module subroutine updateSurfaceLabel(iptStr)
    character(len=*) :: iptStr
   end subroutine updateSurfaceLabel
   module subroutine execEDI(iptStr)
    character(len=*) :: iptStr
   end subroutine execEDI
   module subroutine execTERM(iptStr)
    character(len=*) :: iptStr
   end subroutine execTERM
   module subroutine execRESAUTO(iptStr)
    character(len=*) :: iptStr
   end subroutine execRESAUTO
   module subroutine execTHO(iptStr)
    character(len=*) :: iptStr
   end subroutine execTHO
   module subroutine execSET(iptStr)
    character(len=*) :: iptStr
   end subroutine execSET
   module subroutine execRAYREF(iptStr)
    character(len=*) :: iptStr
   end subroutine execRAYREF
   module subroutine execUndo(iptStr)
    character(len=*) :: iptStr
   end subroutine execUndo
   module subroutine execRedo(iptStr)
    character(len=*) :: iptStr
   end subroutine execRedo
   module subroutine execZOO(iptStr)
    character(len=*) :: iptStr
   end subroutine execZOO
   module subroutine execPOS(iptStr)
    character(len=*) :: iptStr
   end subroutine execPOS
   ! VIE lens-drawing setting commands (NUMRAYS/DRAWSI/DRAWSF/ELEV/AZI/ORIENT).
   ! All route here; the keyword selects which curr_psm setting to update.
   module subroutine adjustVieSettings(iptStr)
    character(len=*) :: iptStr
   end subroutine adjustVieSettings
    end interface


    type zoa_cmd
      character(len=8) :: cmd
      procedure (cmdImplementation), pointer, nopass :: execFunc

    end type

    abstract interface
    subroutine cmdImplementation (iptStr)
       import zoa_cmd
       !class(zoa_cmd) :: self
       character(len=*) ::iptStr
       !real, intent (in) :: z
    end subroutine cmdImplementation 
 end interface    


    character(len=4), dimension(500) :: surfCmds
    type(zoa_cmd), dimension(720) :: zoaCmds

    type(zoaplot_setting_manager)  :: curr_psm
    character(len=10024) :: cmdTOW

    integer :: cmd_loop = 0
    integer, parameter :: VIE_LOOP = 1
    integer, parameter :: DRAW_LOOP = 2 ! While plot is being drawn
    integer, parameter :: SPO_LOOP = 3
    integer, parameter :: ZERN_LOOP = 4
    integer, parameter :: TOW_LOOP = 5
    integer, parameter :: AUT_LOOP = 6
    integer, parameter :: TAR_LOOP = 7
    integer, parameter :: CON_UPDATE_LOOP = 7


    contains

    subroutine initializeCmds()
        ! This is called when the program is initialized (currently INITKDP.FOR)
        use iso_c_binding, only: c_null_ptr
        use global_widgets, only: ioConfig

        integer :: i
        character(len=1), dimension(8) :: evenAsphereTerms = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
        character(len=2), dimension(9) :: asphereVarCodes  = ['AC', 'BC', 'CC', 'DC', 'EC', 'FC', 'GC', 'HC', 'IC']

        ! Initialize all command slots to empty (prevents garbage matches)
        do i = 1, size(zoaCmds)
            zoaCmds(i)%cmd = ''
            zoaCmds(i)%execFunc => null()
        end do

        ! Initialize null ptr textView to dump KDP print statements when needed
        call ioConfig%registerTextView(c_null_ptr, ID_TERMINAL_KDPDUMP)


        !zoaCmds(2)%cmd = 'WL'
        !zoaCmds(2)%execFunc => setWavelength
        do i = 1,499
            zoaCmds(i)%cmd = 'S'//trim(int2str(i))
            zoaCmds(i)%execFunc => setSurfaceCodeVStyle    
        end do        
        zoaCmds(500)%cmd = "SAV"
        zoaCmds(500)%execFunc => execSAV
        zoaCmds(501)%cmd = "REF"
        zoaCmds(501)%execFunc => execSetWavelengthIndex  
        zoaCmds(502)%cmd = "RES"
        zoaCmds(502)%execFunc => execRestore 
        zoaCmds(503)%cmd = "VIE"
        zoaCmds(503)%execFunc => execVie    
        zoaCmds(504)%cmd = "SUR"
        zoaCmds(504)%execFunc => execSUR                               
        zoaCmds(505)%cmd = "STO"
        zoaCmds(505)%execFunc => execSTO    
        zoaCmds(506)%cmd = "S"
        zoaCmds(506)%execFunc => setSurfaceCodeVStyle    
        zoaCmds(507)%cmd = "SO"
        zoaCmds(507)%execFunc => setSurfaceCodeVStyle   
        zoaCmds(508)%cmd = 'WL'
        zoaCmds(508)%execFunc => setWavelength    
        zoaCmds(509)%cmd = 'STOP'
        zoaCmds(509)%execFunc => execSTO  
        zoaCmds(510)%cmd = 'YAN'
        zoaCmds(510)%execFunc => setField
        zoaCmds(511)%cmd = 'XAN'
        zoaCmds(511)%execFunc => setField                                    
        zoaCmds(512)%cmd = 'CIR'
        zoaCmds(512)%execFunc => execCIR  
        zoaCmds(513)%cmd = 'SI'
        zoaCmds(513)%execFunc => setSurfaceCodeVStyle 
        zoaCmds(514)%cmd = 'WTW'
        zoaCmds(514)%execFunc => setWavelengthWeights          
        zoaCmds(515)%cmd = 'WTF'
        zoaCmds(515)%execFunc => setFieldWeights
        zoaCmds(516)%cmd = 'YOB'
        zoaCmds(516)%execFunc => setField
        zoaCmds(517)%cmd = 'XOB'
        zoaCmds(517)%execFunc => setField     
        zoaCmds(518)%cmd = 'INS'
        zoaCmds(518)%execFunc => insertSurf                         
        zoaCmds(519)%cmd = 'FAN'
        zoaCmds(519)%execFunc => execFAN        
        zoaCmds(520)%cmd = 'RSI'
        zoaCmds(520)%execFunc => execRSI         
        zoaCmds(521)%cmd = 'NBR'
        zoaCmds(521)%execFunc => execNBR      
        zoaCmds(522)%cmd = 'FIO'
        zoaCmds(522)%execFunc => execFIO   
        zoaCmds(523)%cmd = 'BES'
        zoaCmds(523)%execFunc => findBestFocus     
        zoaCmds(524)%cmd = 'SPO'
        zoaCmds(524)%execFunc => execSPO   
        zoaCmds(525)%cmd = 'ZERN_TST'
        zoaCmds(525)%execFunc => ZERN_TST     
        zoaCmds(526)%cmd = 'SETWV'
        zoaCmds(526)%execFunc => setPlotWavelength 
        zoaCmds(527)%cmd = 'SETDENS'
        zoaCmds(527)%execFunc => setPlotDensity     
        zoaCmds(528)%cmd = 'SETZERNC'
        zoaCmds(528)%execFunc => setPlotZernikeCoefficients    
        zoaCmds(529)%cmd = 'TOW'
        zoaCmds(529)%execFunc => execTOW   
        zoaCmds(530)%cmd = 'FIE'
        zoaCmds(530)%execFunc => execAstigFieldCurvDistPlot                              
        zoaCmds(531)%cmd = 'PMA'
        zoaCmds(531)%execFunc => execPMAPlot                              
        zoaCmds(532)%cmd = 'SSI'
        zoaCmds(532)%execFunc => setPlotScale     
        zoaCmds(533)%cmd = 'RIM'
        zoaCmds(533)%execFunc => execRayAberrationPlot                                    
        zoaCmds(534)%cmd = 'PLOTTHO'
        zoaCmds(534)%execFunc => execSeidelBarChart
        zoaCmds(535)%cmd = 'PLTRMS'
        zoaCmds(535)%execFunc => execRMSPlot
        zoaCmds(536)%cmd = 'XOFF'
        zoaCmds(536)%execFunc => execXOFF
        zoaCmds(537)%cmd = 'YOFF'
        zoaCmds(537)%execFunc => execYOFF                
        zoaCmds(538)%cmd = 'SAVESESS'
        zoaCmds(538)%execFunc => execSaveSessionToFile                                       
        zoaCmds(539)%cmd = 'CLI'
        zoaCmds(539)%execFunc => execCLI              
        zoaCmds(540)%cmd = 'YIM'
        zoaCmds(540)%execFunc => setField        
        zoaCmds(541)%cmd = 'IND'
        zoaCmds(541)%execFunc => printRefractiveIndices  
        zoaCmds(542)%cmd = 'AUT'
        zoaCmds(542)%execFunc => execAUT 
        !zoaCmds(543)%cmd = 'FRZ'
        !zoaCmds(543)%execFunc => freezeParams 
        zoaCmds(544)%cmd = 'THC'
        zoaCmds(544)%execFunc => updateVarCodes    
        zoaCmds(545)%cmd = 'CCY'
        zoaCmds(545)%execFunc => updateVarCodes           
        zoaCmds(546)%cmd = 'EFL'
        zoaCmds(546)%execFunc => updateConstraint                         
        zoaCmds(547)%cmd = 'TAR'
        zoaCmds(547)%execFunc => execTAR     
        zoaCmds(548)%cmd = 'FRZ'
        zoaCmds(548)%execFunc => execFreeze    
        zoaCmds(549)%cmd = 'TCO'
        zoaCmds(549)%execFunc => updateConstraint   
        zoaCmds(550)%cmd = 'FLY'
        zoaCmds(550)%execFunc => flipSurfaces    
        zoaCmds(551)%cmd = 'SCA'
        zoaCmds(551)%execFunc => scaleSystem    
        zoaCmds(552)%cmd = 'PIK'
        zoaCmds(552)%execFunc => parsePickupInput 
        zoaCmds(553)%cmd = 'RED'
        zoaCmds(553)%execFunc => setMagSolve           
        zoaCmds(554)%cmd = 'TAS'
        zoaCmds(554)%execFunc => updateConstraint     
        zoaCmds(555)%cmd = 'IMC'
        zoaCmds(555)%execFunc => updateConstraint    
        zoaCmds(556)%cmd = 'PTB'
        zoaCmds(556)%execFunc => updateConstraint   
        zoaCmds(557)%cmd = 'PRT'
        zoaCmds(557)%execFunc => printFile    
        zoaCmds(558)%cmd = 'THI'
        zoaCmds(558)%execFunc => setThickness
        zoaCmds(559)%cmd = 'KC' ! TODO:  Verify this is the correct value
        zoaCmds(559)%execFunc => updateVarCodes                            
        zoaCmds(560)%cmd = 'GLA'
        zoaCmds(560)%execFunc => setGlass
        zoaCmds(561)%cmd = 'CUY'
        zoaCmds(561)%execFunc => setCurvature  
        zoaCmds(562)%cmd = 'DEL'
        zoaCmds(562)%execFunc => deleteStuff   
        zoaCmds(563)%cmd = 'RDY'
        zoaCmds(563)%execFunc => setRadius    
        zoaCmds(564)%cmd = 'TIT'
        zoaCmds(564)%execFunc => setLensTitle
        zoaCmds(565)%cmd = 'DIM'
        zoaCmds(565)%execFunc => setDim                        
        zoaCmds(566)%cmd = 'PSF'
        zoaCmds(566)%execFunc => execPSF    
        zoaCmds(567)%cmd = 'MTF'
        zoaCmds(567)%execFunc => execMTF       
        zoaCmds(568)%cmd = 'MFR'
        zoaCmds(568)%execFunc => updateMaxFrequency         
        zoaCmds(569)%cmd = 'IFR'
        zoaCmds(569)%execFunc => updateFrequencyInterval                             
        zoaCmds(570)%cmd = 'CY'
        zoaCmds(570)%execFunc => getRayData    
        zoaCmds(571)%cmd = 'CX'
        zoaCmds(571)%execFunc => getRayData    
        zoaCmds(572)%cmd = 'ASP'
        zoaCmds(572)%execFunc => execAsphere                                            
        do i=1,8
            zoaCmds(572+i)%cmd      = evenAsphereTerms(i)
            zoaCmds(572+i)%execFunc => updateAsphereTerms
        end do        
        zoaCmds(581)%cmd = 'K'          
        zoaCmds(581)%execFunc => updateConicConstant  
        zoaCmds(582)%cmd = '!'          
        zoaCmds(582)%execFunc => processFileComment          
        zoaCmds(583)%cmd = 'ZOA2CV'
        zoaCmds(583)%execFunc => exportLensToCodeV  
        zoaCmds(584)%cmd = 'ZOA2ZMX'
        zoaCmds(584)%execFunc => exportLensToZemax  
        zoaCmds(585)%cmd = 'SLB'
        zoaCmds(585)%execFunc => updateSurfaceLabel    
        zoaCmds(586)%cmd = 'SAS'
        zoaCmds(586)%execFunc => updateConstraint   
        zoaCmds(587)%cmd = 'RMSDATA'
        zoaCmds(587)%execFunc => updateRMSPlotType         
        zoaCmds(588)%cmd = 'IMP'
        zoaCmds(588)%execFunc => updateOptimImprovementGoal   
        zoaCmds(589)%cmd = 'AUTUI'
        zoaCmds(589)%execFunc => aut_ui      
        zoaCmds(590)%cmd = 'UPD'
        zoaCmds(590)%execFunc => updateDatabase    
        zoaCmds(591)%cmd = 'CHA'
        zoaCmds(591)%execFunc => changeDatabase  
        zoaCmds(592)%cmd = 'EVA'
        zoaCmds(592)%execFunc => evaluateCmd

        ! ---- Batch migration from CMDER.FOR (simple commands) ----
        zoaCmds(593)%cmd = 'ABSORB'
        zoaCmds(593)%execFunc => wrap_ABSORB
        zoaCmds(594)%cmd = 'ECHO'
        zoaCmds(594)%execFunc => wrap_ECHO
        ! CV2PRG kept in CMDER.FOR (calls PROCESKDP internally, needs CMDER context)
        zoaCmds(595)%cmd = 'ZMX2PRG'
        zoaCmds(595)%execFunc => wrap_ZMX2PRG
        zoaCmds(597)%cmd = 'CF'
        zoaCmds(597)%execFunc => wrap_CF
        zoaCmds(598)%cmd = 'GCONVERT'
        zoaCmds(598)%execFunc => wrap_GCONVERT
        zoaCmds(599)%cmd = 'INI'
        zoaCmds(599)%execFunc => wrap_INI
        zoaCmds(600)%cmd = 'LTYPE'
        zoaCmds(600)%execFunc => wrap_LTYPE
        zoaCmds(601)%cmd = 'WV'
        zoaCmds(601)%execFunc => wrap_WV
        zoaCmds(602)%cmd = 'UNITS'
        zoaCmds(602)%execFunc => wrap_UNITS
        zoaCmds(603)%cmd = 'TPLATE'
        zoaCmds(603)%execFunc => wrap_TPLATE
        zoaCmds(604)%cmd = 'ASTOP'
        zoaCmds(604)%execFunc => wrap_ASTOP
        zoaCmds(605)%cmd = 'ZERNREPT'
        zoaCmds(605)%execFunc => wrap_ZERNREPT
        zoaCmds(606)%cmd = 'MODE'
        zoaCmds(606)%execFunc => wrap_MODE
        zoaCmds(607)%cmd = 'FINDGLAS'
        zoaCmds(607)%execFunc => wrap_FINDGLAS
        zoaCmds(608)%cmd = 'COLORSET'
        zoaCmds(608)%execFunc => wrap_COLORSET
        zoaCmds(609)%cmd = 'GREYSPOT'
        zoaCmds(609)%execFunc => wrap_GREYSPOT
        zoaCmds(610)%cmd = 'REFK'
        zoaCmds(610)%execFunc => wrap_REFK
        zoaCmds(611)%cmd = 'PIVAXIS'
        zoaCmds(611)%execFunc => wrap_PIVAXIS
        zoaCmds(612)%cmd = 'DISP'
        zoaCmds(612)%execFunc => wrap_DISP
        zoaCmds(613)%cmd = 'STILT'
        zoaCmds(613)%execFunc => wrap_STILT
        zoaCmds(614)%cmd = 'BTILT'
        zoaCmds(614)%execFunc => wrap_BTILT
        zoaCmds(615)%cmd = 'ROLL'
        zoaCmds(615)%execFunc => wrap_ROLL
        zoaCmds(616)%cmd = 'FLIP'
        zoaCmds(616)%execFunc => wrap_FLIP
        zoaCmds(617)%cmd = 'SPC'
        zoaCmds(617)%execFunc => wrap_SPC
        zoaCmds(618)%cmd = 'INVAR'
        zoaCmds(618)%execFunc => wrap_INVAR
        zoaCmds(619)%cmd = 'CHRSHIFT'
        zoaCmds(619)%execFunc => wrap_CHRSHIFT
        zoaCmds(620)%cmd = 'FIRD'
        zoaCmds(620)%execFunc => wrap_FIRD
        zoaCmds(621)%cmd = 'OBJLEV'
        zoaCmds(621)%execFunc => wrap_OBJLEV
        zoaCmds(622)%cmd = 'FIGURE'
        zoaCmds(622)%execFunc => wrap_FIGURE
        zoaCmds(623)%cmd = 'INCR'
        zoaCmds(623)%execFunc => wrap_INCR
        zoaCmds(624)%cmd = 'FOBDUMP'
        zoaCmds(624)%execFunc => wrap_FOBDUMP
        zoaCmds(626)%cmd = 'OPD'
        zoaCmds(626)%execFunc => wrap_OPD
        zoaCmds(627)%cmd = 'AUTO'
        zoaCmds(627)%execFunc => wrap_AUTO
        zoaCmds(628)%cmd = 'HEADINGS'
        zoaCmds(628)%execFunc => wrap_HEADINGS
        zoaCmds(629)%cmd = 'DXF'
        zoaCmds(629)%execFunc => wrap_DXF
        zoaCmds(630)%cmd = 'FANS'
        zoaCmds(630)%execFunc => wrap_FANS
        zoaCmds(631)%cmd = 'VIEOFF'
        zoaCmds(631)%execFunc => wrap_VIEOFF
        zoaCmds(632)%cmd = 'SHOWNSS'
        zoaCmds(632)%execFunc => wrap_SHOWNSS
        zoaCmds(633)%cmd = 'SPDSSI'
        zoaCmds(633)%execFunc => wrap_SPDSSI
        zoaCmds(634)%cmd = 'DET'
        zoaCmds(634)%execFunc => wrap_DET
        zoaCmds(637)%cmd = 'GRID'
        zoaCmds(637)%execFunc => wrap_GRID
        zoaCmds(638)%cmd = 'SPACE'
        zoaCmds(638)%execFunc => wrap_SPACE
        zoaCmds(639)%cmd = 'CUTOFF'
        zoaCmds(639)%execFunc => wrap_CUTOFF
        zoaCmds(640)%cmd = 'WAMAP'
        zoaCmds(640)%execFunc => wrap_WAMAP
        zoaCmds(641)%cmd = 'AMAP'
        zoaCmds(641)%execFunc => wrap_AMAP
        zoaCmds(642)%cmd = 'RAYLEIGH'
        zoaCmds(642)%execFunc => wrap_RAYLEIGH
        zoaCmds(643)%cmd = 'WEIGHT'
        zoaCmds(643)%execFunc => wrap_WEIGHT
        zoaCmds(644)%cmd = 'COST'
        zoaCmds(644)%execFunc => wrap_COST
        zoaCmds(645)%cmd = 'DEFORM'
        zoaCmds(645)%execFunc => wrap_DEFORM
        zoaCmds(646)%cmd = 'OUTFLAT'
        zoaCmds(646)%execFunc => wrap_OUTFLAT
        zoaCmds(647)%cmd = 'EXPUP'
        zoaCmds(647)%execFunc => wrap_EXPUP
        zoaCmds(648)%cmd = 'RSPH'
        zoaCmds(648)%execFunc => wrap_RSPH
        zoaCmds(649)%cmd = 'PRINT'
        zoaCmds(649)%execFunc => wrap_PRINT
        zoaCmds(650)%cmd = 'FITZERN'
        zoaCmds(650)%execFunc => wrap_FITZERN
        zoaCmds(651)%cmd = 'LISTOPD'
        zoaCmds(651)%execFunc => wrap_LISTOPD
        zoaCmds(652)%cmd = 'LISTZERN'
        zoaCmds(652)%execFunc => wrap_LISTZERN
        zoaCmds(653)%cmd = 'LISTREPT'
        zoaCmds(653)%execFunc => wrap_LISTREPT
        zoaCmds(654)%cmd = 'OIF'
        zoaCmds(654)%execFunc => wrap_OIF
        zoaCmds(655)%cmd = 'XXF'
        zoaCmds(655)%execFunc => wrap_XXF
        zoaCmds(656)%cmd = 'XXFF'
        zoaCmds(656)%execFunc => wrap_XXFF
        zoaCmds(657)%cmd = 'IMAGEDIR'
        zoaCmds(657)%execFunc => wrap_IMAGEDIR
        zoaCmds(658)%cmd = 'CAPFNOUT'
        zoaCmds(658)%execFunc => wrap_CAPFNOUT
        zoaCmds(659)%cmd = 'CAPGRID'
        zoaCmds(659)%execFunc => wrap_CAPGRID
        zoaCmds(660)%cmd = 'FUNNAME'
        zoaCmds(660)%execFunc => wrap_FUNNAME
        zoaCmds(661)%cmd = 'GLASSWV'
        zoaCmds(661)%execFunc => wrap_GLASSWV
        zoaCmds(662)%cmd = 'DO'
        zoaCmds(662)%execFunc => wrap_DO
        zoaCmds(663)%cmd = 'PRES'
        zoaCmds(663)%execFunc => wrap_PRES
        zoaCmds(664)%cmd = 'STATS'
        zoaCmds(664)%execFunc => wrap_STATS
        zoaCmds(665)%cmd = 'SPGR'
        zoaCmds(665)%execFunc => wrap_SPGR
        zoaCmds(666)%cmd = 'PRICE'
        zoaCmds(666)%execFunc => wrap_PRICE
        zoaCmds(667)%cmd = 'AUTOFUNC'
        zoaCmds(667)%execFunc => wrap_AUTOFUNC
        zoaCmds(668)%cmd = 'THM'
        zoaCmds(668)%execFunc => wrap_THM
        zoaCmds(669)%cmd = 'INR'
        zoaCmds(669)%execFunc => wrap_INR
        zoaCmds(670)%cmd = 'INRD'
        zoaCmds(670)%execFunc => wrap_INRD
        zoaCmds(671)%cmd = 'VIEOVER'
        zoaCmds(671)%execFunc => wrap_VIEOVER
        zoaCmds(672)%cmd = 'TFMOTION'
        zoaCmds(672)%execFunc => wrap_TFMOTION
        zoaCmds(673)%cmd = 'FLDSARE'
        zoaCmds(673)%execFunc => wrap_FLDSARE
        zoaCmds(674)%cmd = 'SEED'
        zoaCmds(674)%execFunc => wrap_SEED
        zoaCmds(675)%cmd = 'PROGSIZE'
        zoaCmds(675)%execFunc => wrap_PROGSIZE
        zoaCmds(676)%cmd = 'RAYERROR'
        zoaCmds(676)%execFunc => wrap_RAYERROR
        zoaCmds(677)%cmd = 'READIRAD'
        zoaCmds(677)%execFunc => wrap_READIRAD
        zoaCmds(678)%cmd = 'TSTCMDS'
        zoaCmds(678)%execFunc => wrap_TSTCMDS
        zoaCmds(679)%cmd = 'LCON'
        zoaCmds(679)%execFunc => listConstraints
        zoaCmds(680)%cmd = 'DCON'
        zoaCmds(680)%execFunc => deleteConstraints
        zoaCmds(681)%cmd = "RMD"
        zoaCmds(681)%execFunc => execRMD
        zoaCmds(682)%cmd = "EDI"
        zoaCmds(682)%execFunc => execEDI
        zoaCmds(683)%cmd = "TERM"
        zoaCmds(683)%execFunc => execTERM
        zoaCmds(684)%cmd = "SPH"
        zoaCmds(684)%execFunc => execSphere
        zoaCmds(685)%cmd = "RESAUTO"
        zoaCmds(685)%execFunc => execRESAUTO
        zoaCmds(686)%cmd = "THO"
        zoaCmds(686)%execFunc => execTHO
        zoaCmds(687)%cmd = "SET"
        zoaCmds(687)%execFunc => execSET
        zoaCmds(688)%cmd = "UNDO"
        zoaCmds(688)%execFunc => execUndo
        zoaCmds(689)%cmd = "REDO"
        zoaCmds(689)%execFunc => execRedo
        zoaCmds(690)%cmd = "ZOO"
        zoaCmds(690)%execFunc => execZOO
        zoaCmds(691)%cmd = "POS"
        zoaCmds(691)%execFunc => execPOS
        zoaCmds(692)%cmd = "RAYREF"
        zoaCmds(692)%execFunc => execRAYREF
        ! VIE lens-drawing setting commands (valid inside a VIE ; ... ; GO loop)
        zoaCmds(693)%cmd = "NUMRAYS"
        zoaCmds(693)%execFunc => adjustVieSettings
        zoaCmds(694)%cmd = "DRAWSI"
        zoaCmds(694)%execFunc => adjustVieSettings
        zoaCmds(695)%cmd = "DRAWSF"
        zoaCmds(695)%execFunc => adjustVieSettings
        zoaCmds(696)%cmd = "ELEV"
        zoaCmds(696)%execFunc => adjustVieSettings
        zoaCmds(697)%cmd = "AZI"
        zoaCmds(697)%execFunc => adjustVieSettings
        zoaCmds(698)%cmd = "ORIENT"
        zoaCmds(698)%execFunc => adjustVieSettings

        ! 20th-order asphere coefficient (completes A..I; KDP command AL)
        zoaCmds(699)%cmd = 'I'
        zoaCmds(699)%execFunc => updateAsphereTerms

        zoaCmds(700)%cmd = 'CUX'
        zoaCmds(700)%execFunc => setCurvatureX

        ! CODE V-style variable codes for the asphere coefficients A4..A20
        ! (param cmd + 'C', paralleling CCY/THC/KC):  <code> Sj 0 = variable,
        ! <code> Sj 100 = frozen.  All route through updateVarCodes, which
        ! passes the command name to the name->VAR_* maps.
        ! (The legacy KDP commands CC and AC were renamed CCK/ACK to free
        ! these names.)
        do i=1,9
            zoaCmds(699+i)%cmd      = asphereVarCodes(i)
            zoaCmds(699+i)%execFunc => updateVarCodes
        end do


    end subroutine

    function startCodeVLensUpdateCmd(iptCmd) result(boolResult)

        character(len=*) :: iptCmd
        integer :: ii
        logical :: boolResult
        integer :: spacePos
        character(len=40) :: valStr

        boolResult = .FALSE.

        ! (VIE-loop setting subcommands NUMRAYS/DRAWSI/DRAWSF/ELEV/AZI/ORIENT are
        !  now registered CodeV commands routed to adjustVieSettings, below.)

        do ii=1,size(zoaCmds)
        if (iptCmd == zoaCmds(ii)%cmd) then
            if (cmd_loop == TOW_LOOP .AND. iptCmd /= 'GO') then
                if (len(trim(cmdTOW)) == 0) then
                    cmdTOW = currentCommand
                else
                    cmdTOW = trim(cmdTOW)//'; '//currentCommand
                end if
                boolResult = .TRUE.
                return
            end if
                
            if (.not. associated(zoaCmds(ii)%execFunc)) then
                print *, "BUG: null execFunc for cmd=[", trim(zoaCmds(ii)%cmd), "] iptCmd=[", trim(iptCmd), "] ii=", ii
                boolResult = .FALSE.
                return
            end if
            call zoaCmds(ii)%execFunc(currentCommand)
            boolResult = .TRUE.
            return
        end if
        end do

        select case (iptCmd)
           
        case('GO')
            CALL executeGo()
            boolResult = .TRUE.
            return
         
        case ('PIM')
            call setParaxialImageSolve()
            boolResult = .TRUE.
            return     
        case ('EPD')
            call setEPD()
            boolResult = .TRUE.
            return   
        ! case ('CUY')
        !     call setCurvature()
        !     boolResult = .TRUE.
        !     return             
            
        case ('SETC')
            call execSetCodeVCmd()
            boolResult = .TRUE.
            return              

        end select

        ! Handle Sk separately
        ! IF(isSurfCommand(iptCmd)) then
        !     CALL setSurfaceCodeVStyle(iptCmd)
        !     return
        !   END IF            
              
    end function
     
    ! Would like to move these cmd_parser to command-utils, but there is a circulat
    ! dependency due to updateTerminalLog I need to solve.  Sigh.
    subroutine cmd_parser_get_real_pair(tokens, real1, real2, real1Bounds, real2Bounds) 
    ! Look for 2 and only 2 inputs that don't have a string character in them    
        use command_utils, only: isInputNumber
        implicit none
        character(len=*), dimension(:) :: tokens
        real(kind=real64) :: real1, real2
        real(kind=real64) :: tmpVal
        real, dimension(2), optional :: real1Bounds
        real, dimension(2), optional :: real2Bounds
        integer :: i, numRealInputs 

        numRealInputs = 0

        if(size(tokens) > 1) then
            do i=2,size(tokens)
                if (isInputNumber(tokens(i))) then
                    numRealInputs = numRealInputs + 1
                    select case(numRealInputs)
                    case (1)
                        if (present(real1Bounds)) then
                            tmpVal = str2real8(tokens(i))
                            if (tmpVal >= real1Bounds(1) .AND. tmpVal <= real1Bounds(2)) then
                                real1 = tmpVal
                            else
                                call zoa_emit("Error:  Real 1 Input Outside Bounds "//trim(tokens(i)), "red")
                                return
                            end if
                        else
                            real1 = str2real8(tokens(i))
                        end if

                    case (2)
                        if (present(real2Bounds)) then
                            tmpVal = str2real8(tokens(i))
                            if (tmpVal >= real1Bounds(1) .AND. tmpVal <= real1Bounds(2)) then
                                real2 = tmpVal
                            else
                                call zoa_emit("Error:  Real 2 Input Outside Bounds "//trim(tokens(i)), "red")
                                return
                            end if
                        else
                            real2 = str2real8(tokens(i))
                        end if                        
                      
                    case default
                        call zoa_emit("Warning:  Detected more than two valid inputs.  Ignoring "//trim(tokens(i)), "red")

                    end select
                end if
            end do
        end if        

    
    end subroutine

    function cmd_parser_get_integer_range(iptStr, intArr) result(goodResult)
        use command_utils, only: isInputNumber
        implicit none
        logical :: goodResult
        character(len=*) :: iptStr
        integer, allocatable :: intArr(:)
        integer :: i, s0, sf, dotLoc

        goodResult = .FALSE.
        ! Check for ellipsis
        dotLoc = index(iptStr,'..') 
        if(dotLoc > 0) then
            ! Require input to be i..k

            if(isInputNumber(iptStr(2:dotLoc-1)).AND. &
            &  isInputNumber(iptStr(dotLoc+2:len(iptStr)))) then
               goodResult = .TRUE.
               s0 = str2int(iptStr(1:dotLoc-1))
               sf = str2int(iptStr(dotLoc+2:len(iptStr)))
               intArr = (/ (i,i=s0,sf)/)
            else
                call zoa_emit("Error:  Could not convert input to X..Y "//iptStr, "red")
            end if
        else ! No dots found
            if(isInputNumber(iptStr)) then
            goodResult = .TRUE.
            ! Assignment auto-(re)allocates, so this is safe even when the
            ! caller invokes us again for another token of the same prefix
            ! (e.g. "f1 f2"); explicit allocate() would abort as already
            ! allocated. Repeated bare tokens are last-wins; use "f1..k"
            ! to request a range.
            intArr = [ str2int(iptStr) ]
            end if
        end if


    end function
    

    function cmd_parser_get_int_input_for_prefix(prefix, tokens) result(intArr)
        use global_widgets, only: sysConfig
        use command_utils, only: isInputNumber

        implicit none
        character(len=*) :: prefix
        character(len=*), dimension(:) :: tokens
        integer, allocatable :: intArr(:)
        integer :: i
        logical :: userDefined 

        userDefined = .FALSE.
        
        if(size(tokens) > 1) then
            do i=2,size(tokens)
                if (uppercase(tokens(i)(1:len(prefix))) == uppercase(prefix) ) then
                    userDefined = cmd_parser_get_integer_range(tokens(i)(2:len(tokens(i))), intArr)

                    !if (isInputNumber(tokens(i)(2:len(tokens)))) then
                    !    userDefined = .TRUE.
                    !    allocate(fields(1))
                    !    fields = str2int(tokens(i)(2:len(tokens)))
                    !end if
                end if
                !call LogTermFOR("Tokens "// tokens(i))

            end do
        end if

        !Default is all fields
        if (.NOT.userDefined) then

            select case (prefix)

            case ('f')
                intArr =  (/ (i,i=1,sysConfig%numFields)/)
            case ('w')
                intArr =  (/ (i,i=1,sysConfig%numWavelengths)/)
            case default
                allocate(intArr(1))
                intArr(1) = 0

            end select
        end if




    end function

    subroutine execXOFF(iptStr)
        use command_utils, only: isInputNumber

        implicit none
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        character(len=80) :: tokens(40)
        integer :: numTokens
        INTEGER VIEXOF,VIEYOF, VIEROT
  
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT


        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens==2) then
            if (isInputNumber(tokens(2))) then
                VIEXOF = INT(str2real8(tokens(2)))
            end if
        end if

    end subroutine

    subroutine execYOFF(iptStr)
        use command_utils, only: isInputNumber

        implicit none
        !class(zoa_cmd) :: self
        character(len=*) :: iptStr

        character(len=80) :: tokens(40)
        integer :: numTokens
        INTEGER VIEXOF,VIEYOF, VIEROT
  
        COMMON/OFFVIE/VIEXOF,VIEYOF,VIEROT


        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if (numTokens==2) then
            if (isInputNumber(tokens(2))) then
                VIEYOF =  INT(str2real8(tokens(2)))
            end if
        end if

    end subroutine

    subroutine execFreeze(iptStr)
        
        use optim_types, only: optim
        

        implicit none

        character(len=*) :: iptStr     
        character(len=80) :: tokens(40)
        integer :: numTokens
        
        call parse(trim(iptStr), ' ', tokens, numTokens) 

        if(numTokens == 2) then
            if (tokens(2) == 'SA') then
                call optim%freezeAllSurfaces()
            end if
            if (tokens(2) == 'CON') then
                call optim%removeAllConstraints()
            end if

        else
            call zoa_emit("Error!  FRZ must include a qualifier, such as SA to freeze control variables", "red")
        end if
        

    end subroutine

    ! This is a very important command that performs ray traces for a user defined
    ! number of field points, wavelengths, and relative aperture locations
    ! The output depends on the cmd level
    ! For normal operation a table will be output with the results
    ! If in VIE level it will save the rays intersection coordinates for use with plooting
    ! Format RSI fi..k wi..k relApeX relApeY
    subroutine execRSI(iptStr)
        use global_widgets, only: sysConfig, curr_lens_data
        use command_utils, only : parseCommandIntoTokens, isInputNumber
        use DATLEN, only: RAYRAY, NEWOBJ, NEWIMG
        use DATMAI, only: PII, TWOPII
        use result_builder

        implicit none

        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        integer :: numTokens
        integer, allocatable :: fields(:), wavelengths(:)
        real(kind=real64) :: relApeX, relApeY
        integer :: i, j
        integer :: nrayWavs, nrayFields
        logical :: multipleRays

        ! Structured-result locals
        integer :: nsurfs, surf_idx, nrows
        character(len=16), allocatable :: row_labels(:)
        character(len=8), parameter :: col_labels(5) = &
            ['X    ', 'Y    ', 'Z    ', 'XANG ', 'YANG ']
        real(real64), allocatable :: tdata(:,:)
        real(real64) :: xa, ya, deg_per_rad
        character(len=8) :: surf_label

        ! Defaults
        relApeX = 0.0
        relApeY = 0.0

        call parseCommandIntoTokens(trim(iptStr), tokens, numTokens, ' ')

        fields = cmd_parser_get_int_input_for_prefix('f', tokens(1:numTokens))
        wavelengths = cmd_parser_get_int_input_for_prefix('w', tokens(1:numTokens))
        call cmd_parser_get_real_pair(tokens(1:numTokens), relApeX, relApeY, &
        & real1Bounds=[-1.0,1.0], real2Bounds=[-1.0,1.0])
        !fields = cmd_parser_get_fields(tokens(1:numTokens))
        do i=1,size(fields)
            call LogTermFOR("RSI fields are "//int2str(fields(i)))

        end do
        do i=1,size(wavelengths)
            call LogTermFOR("RSI wavelengths are "//int2str(wavelengths(i)))
        end do

        call LogTermFOR("Relative X Aperture is " // real2str(relApeX))
        call LogTermFOR("Relative Y Aperture is " // real2str(relApeY))

        nrayWavs   = size(wavelengths)
        nrayFields = size(fields)
        multipleRays = (nrayWavs * nrayFields > 1)

        ! Now that we have inputs, trace all rays needed.
        do i = 1,nrayWavs
        do j = 1,nrayFields
            !SAVE_KDP(1)=SAVEINPT(1)

            ! TODO:  Put COLRAY value into sysConfig (current field Color)
            !COLRAY = sysConfig%fieldColorCodes(fields(j))
            !WRITE(INPUT, *) "FOB ", )
            CALL PROCESKDP("FOB "//trim(real2str(sysConfig%relativeFields(2,fields(j)))))
            !REST_KDP(1)=RESTINPT(1)


      ! Add loop for wavelengths
      ! By definition RSI only traces a ray of a single angle
            !SAVE_KDP(1)=SAVEINPT(1)
            CALL PROCESKDP("RAY "//real2str(relApeY)// &
            & " "//real2str(relApeX)//" "//int2str(wavelengths(i)))
            CALL PROCESKDP("PRXYZ ALL")
            !REST_KDP(1)=RESTINPT(1)
    end do ! fields
   end do  !  wavelengths

        ! --- Populate result_builder from the last traced ray ---
        ! RAYRAY indices run from NEWOBJ to NEWIMG (object surface to image surface).
        ! NEWOBJ is typically 0 (object surface), NEWIMG is the last surface index.
        nsurfs = NEWIMG - NEWOBJ + 1   ! total number of surfaces including object
        nrows  = nsurfs

        allocate(row_labels(nrows))
        allocate(tdata(nrows, 5))

        deg_per_rad = 180.0_real64 / PII

        do surf_idx = 1, nrows
            ! Surface index into RAYRAY: NEWOBJ + surf_idx - 1
            i = NEWOBJ + surf_idx - 1
            write(surf_label, '("S",I0)') i
            row_labels(surf_idx) = trim(surf_label)

            tdata(surf_idx, 1) = RAYRAY(1, i)   ! X
            tdata(surf_idx, 2) = RAYRAY(2, i)   ! Y
            tdata(surf_idx, 3) = RAYRAY(3, i)   ! Z

            ! XANG: RAYRAY(11,i) is UX in radians; apply RS-equivalent wrap then convert to degrees
            xa = RAYRAY(11, i)
            if (xa > PII)     xa = xa - TWOPII
            if (xa < -PII)    xa = xa + TWOPII
            tdata(surf_idx, 4) = xa * deg_per_rad  ! XANG in degrees

            ! YANG: RAYRAY(12,i) is UY in radians; apply RS-equivalent wrap then convert to degrees
            ya = RAYRAY(12, i)
            if (ya > PII)     ya = ya - TWOPII
            if (ya < -PII)    ya = ya + TWOPII
            tdata(surf_idx, 5) = ya * deg_per_rad  ! YANG in degrees
        end do

        call result_begin("raytrace")
        call result_set_meta("field",   int2str(fields(nrayFields)))
        call result_set_meta("wavelength", int2str(wavelengths(nrayWavs)))
        call result_set_meta("relApeX", real2str(relApeX))
        call result_set_meta("relApeY", real2str(relApeY))
        if (multipleRays) then
            call result_add_message( &
                "Multiple fields/wavelengths traced; table contains last ray only " // &
                "(f"//int2str(fields(nrayFields))// &
                " w"//int2str(wavelengths(nrayWavs))//")")
        end if

        call result_add_table("ray", row_labels, col_labels, tdata)

        deallocate(row_labels)
        deallocate(tdata)

    end subroutine

    subroutine execFIO(iptStr)

        use global_widgets, only: curr_par_ray_trace, curr_lens_data, sysConfig
        use kdp_utils

        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        character(len=3), dimension(4) :: colHeaders
        real(kind=long), dimension(4,curr_lens_data%num_surfaces):: dataArray

        print *, "Size of dataArray is ", size(dataArray,1)
        dataArray(1,:) = curr_par_ray_trace%marginal_ray_height
        dataArray(2,:) = curr_par_ray_trace%marginal_ray_angle        
        dataArray(3,:) = curr_par_ray_trace%chief_ray_height
        dataArray(4,:) = curr_par_ray_trace%chief_ray_angle

        colHeaders(1) = "HMY"
        colHeaders(2) = "UMY"
        colHeaders(3) = "HCY"
        colHeaders(4) = "UCY"

        call OUTKDP('   '//trim(sysConfig%lensTitle))
        call logDataVsSurface(dataArray, colHeaders)
      
        ! call updateTerminalLog("UMY"//blankStr(5)//"HMY"//blankStr(5)//"UCY"//blankStr(5)//"HCY", "black")
        ! do i=1,curr_lens_data%num_surfaces
        !     call updateTerminalLog(trim(real2str(curr_par_ray_trace%marginal_ray_angle(i)))//blankStr(2)// &
        !     &                      trim(real2str(curr_par_ray_trace%marginal_ray_height(i)))//blankStr(2)// &
        !     &                      trim(real2str(curr_par_ray_trace%chief_ray_angle(i)))//blankStr(2)// &
        !     &                      trim(real2str(curr_par_ray_trace%chief_ray_height(i))), "black" )


        ! end do

    end subroutine

    subroutine printRefractiveIndices(iptStr)

        use global_widgets, only: curr_par_ray_trace, curr_lens_data, sysConfig
        use kdp_utils
        use mod_lens_data_manager
 

        implicit none        

        !class(zoa_cmd) :: self
        character(len=*) :: iptStr
        character(len=80) :: tokens(40)
        character(len=3), dimension(4) :: colHeaders
        real(kind=long), dimension(4,curr_lens_data%num_surfaces):: dataArray
        integer :: i,j
        character(len=1024) :: outStr

        print *, "Size of dataArray is ", size(dataArray,1)
        dataArray(1,:) = curr_par_ray_trace%marginal_ray_height
        dataArray(2,:) = curr_par_ray_trace%marginal_ray_angle        
        dataArray(3,:) = curr_par_ray_trace%chief_ray_height
        dataArray(4,:) = curr_par_ray_trace%chief_ray_angle

        colHeaders(1) = "HMY"
        colHeaders(2) = "UMY"
        colHeaders(3) = "HCY"
        colHeaders(4) = "UCY"

        call OUTKDP('   '//trim(sysConfig%lensTitle))
        call OUTKDP('REFRACTIVE INDICES')
        outStr = '   GLASS CODE'
        do j=1,sysConfig%numWavelengths
            outStr = trim(outStr)//blankStr(6)//real2str(1000.0*sysConfig%getWavelength(j),2)
        end do
        call OUTKDP(trim(outStr))
        do i=0,ldm%getLastSurf()
            if (ldm%isGlassSurf(i)) then
                outStr = blankStr(3)//ldm%getGlassName(i)
                do j=1,sysConfig%numWavelengths
                    outStr=trim(outStr)//blankStr(5)//real2str(ldm%getSurfIndex(i,j))
                end do
                call outKDP(trim(outStr))
            end if
        end do




    end subroutine



    ! Plot/focus command implementations live in codeV-commands-plots.f90

    ! Save/restore and lens-update command implementations live in codeV-commands-lensops.f90




    ! Edit/update/UI command implementations live in codeV-commands-editops.f90

      ! Utility command implementations live in codeV-commands-utils.f90

      function isSurfCommand(tstCmd) result(boolResult)
        implicit none
        character(len=*) :: tstCmd
        logical :: boolResult
        integer :: i

        boolResult = .FALSE.
        
        ! Special case:  SO or S0 (letter-O and digit-zero both mean the object surface)
        if (tstCmd.EQ.'SO' .OR. tstCmd.EQ.'S0') then
            boolResult = .TRUE.
            return
        end if

        do i=1,size(surfCmds)
            surfCmds(i) = 'S'//trim(int2str(i))
            if(tstCmd.EQ.surfCmds(i)) then
                boolResult = .TRUE.
            end if

        end do

        if (tstCmd.EQ.'SI') then
            boolResult = .TRUE.
            return
        end if        

        ! SA for SUR command
        if (tstCmd.EQ.'SA') then
            boolResult = .TRUE.
            return
        end if        





      end function

     ! This is a way to deal with this allocatable atring issue  
!       type string
!       character(len=:), allocatable :: s
!    end type string
! ! create an array of strings where each element is separately allocatable
!    type(string) :: month(4)
!    integer :: j   
!    month(1)%s = 'January'   
!    month(2)%s = 'February'
!    month(3)%s = 'March'
!    month(4)%s = 'April'
!    print *, (month(j)%s, ' ', j=1,size(month))
!    print *, (len(month(j)%s), j=1, size(month))

      function isCodeVCommand(tstCmd) result(boolResult)
        implicit none 

        logical :: boolResult
        character(len=*) :: tstCmd
        type(string) :: tstCmds(6+size(zoaCmds))
        integer :: i


        ! TODO:  Find some better way to do this.  For now, brute force it
        ! codeVCmds = [character(len=4) :: 'YAN', 'TIT', 'WL', 'SO','S','GO', &
        ! &'DIM', 'RDY', 'THI', 'INS', 'GLA', 'PIM', 'EPD', 'CUY', &
        ! & 'DEL', 'RED', 'SETC', 'AAA', 'AAA']
        ! do i=1,size(zoaCmds)
        !    codeVCmds(i+17) = zoaCmds(i)%cmd
        ! end do

        ! This hard coding of cmds is temporary, until I migrate these to new format

        !tstCmds(1)%s = 'YAN'
        !tstCmds(2)%s = 'TIT'
        !tstCmds(3)%s = 'WL'
        !tstCmds(8)%s = 'RDY'
        !tstCmds(9)%s = 'THI'
        !tstCmds(10)%s = 'INS'
        !tstCmds(11)%s = 'GLA'
        !tstCmds(14)%s = 'CUY'
        !tstCmds(16)%s = 'RED'        
        tstCmds(1)%s = 'GO'
        tstCmds(2)%s = 'DIM'
        tstCmds(3)%s = 'PIM'
        tstCmds(4)%s = 'EPD'
        tstCmds(5)%s = 'DEL'
        tstCmds(6)%s = 'SETC'
        do i=1,size(zoaCmds)
            tstCmds(6+i)%s = zoaCmds(i)%cmd
        end do



        boolResult = .FALSE.
        do i=1,size(tstCmds)
            if (tstCmds(i)%s.EQ.tstCmd) then
                boolResult = .TRUE.
                return
            end if
        end do
        ! If we've gotten here check if it is a surface command
        boolResult = isSurfCommand(tstCmd)

      end function

      subroutine executeCodeVLensUpdateCommand(iptCmd, debugFlag, exitLensUpdate, refreshSurf, refreshAll)
        use kdp_utils, only: inLensUpdateLevel
        use global_widgets, only: ioConfig
        use DATLEN, only: SURF
        use mod_lens_data_manager, only: ldm

        implicit none
        character(len=*) :: iptCmd
        logical, optional :: debugFlag, exitLensUpdate
        ! refreshSurf: a surface whose geometry this command just changed in ALENS.
        ! Its frozen copy in the typed store (ldm%surfaces(k)%s) is re-synced from
        ! ALENS BELOW, before the finalizing EOS traces, so a PIM/PY solve resolves
        ! against the new geometry instead of the stale frozen radius.
        integer, optional :: refreshSurf
        ! refreshAll: this command changed the surface COUNT/layout (e.g. INSK), so
        ! the whole typed store must be rebuilt from ALENS before the EOS traces --
        ! otherwise the finalizing trace (and any PIM re-solve) runs against a
        ! stale-layout store.
        logical, optional :: refreshAll
        logical :: redirectFlag, inUpdate
        integer :: savedSurf

        if(present(debugFlag)) then
            redirectFlag = .NOT.debugFlag
        else
            redirectFlag = .TRUE.
        end if

        ! Hide KDP Commands from user
        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_KDPDUMP)

        ! Preserve the current-surface pointer across the update.  Some commands
        ! issued here recompute the lens (e.g. STO -> REFS) which resets the global
        ! SURF to 0; without this, a saved lens's bare "S/STO/CIR" build sequence
        ! loses its place and overwrites a surface instead of adding one.
        savedSurf = SURF

        inUpdate = inLensUpdateLevel()
        if (inUpdate) then
            call PROCESKDP(iptCmd)
        else
            !call PROCESKDP('U L;'// iptCmd //';EOS')
            ! Update - do not exit lens update level to better support stops
            ! clear apertures, etc
            call PROCESKDP('U L;'// iptCmd )
        end if

        ! Re-sync the typed store BEFORE the EOS below traces, so PIM/PY solves
        ! resolve off the new geometry: a full rebuild for a topology change
        ! (refreshAll), else just the one edited surface (refreshSurf).
        if (present(refreshAll)) then
            if (refreshAll) call ldm%load_surfaces_from_alens()
        else if (present(refreshSurf)) then
            call ldm%refresh_typed_surf_geom(refreshSurf)
        end if

        ! If the called asked to exit update, then exit.
        ! If we were not in lens update level, then exit (return to prior state)
        !eosCalled = .FALSE.
        if(present(exitLensUpdate)) then
            if(exitLensUpdate) CALL PROCESKDP('EOS')
        !    if(exitLensUpdate.eqv..TRUE..OR.inUpdate.eqv..FALSE.) CALL PROCESKDP('EOS')
        end if
         if(inUpdate.eqv..FALSE.) CALL PROCESKDP('EOS')

        ! Restore the surface pointer clobbered by any recompute above.
        SURF = savedSurf
        

    


        if (redirectFlag) call ioConfig%setTextView(ID_TERMINAL_DEFAULT)
      end subroutine

      function getSurfNumFromSurfCommand(iptCmd) result(surfNum)
        use mod_lens_data_manager
        use global_widgets, only: curr_lens_data
        use command_utils, only: isInputNumber
        implicit none
        character(len=*) :: iptCmd
        integer :: surfNum, ptrIdx

        !print *, "IPTCMD is ", iptCmd
        !print *, "len of iptCmd is ", len(iptCmd)

        surfnum = -1 ! For error checking

        if(len(iptCmd).EQ.1) then ! It is S, which means we either move to the next surface or add a surface before
                                  ! the last surface.
            ptrIdx = ldm%getSurfacePointer()
            if (ptrIdx >= ldm%getLastSurf()-1) then
                ! At (or past) the last real surface: add a new surface before the
                ! image, then advance the pointer to it so following modifiers and
                ! the next bare S act on / after the surface just added.  (The
                ! pointer must be set explicitly: a recompute inside the update --
                ! e.g. REFS from a STO -- resets the global SURF, so the bare S/STO/
                ! CIR sequence emitted by a saved lens would otherwise overwrite an
                ! existing surface instead of adding one.  See setSurfacePointer.)
                surfNum = curr_lens_data%num_surfaces-1
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                &  "; INSK "//trim(int2str(surfNum)))
                call ldm%setSurfacePointer(surfNum)
            else ! Move pointer to next surface
                call ldm%incrementSurfacePointer()
                surfNum = ptrIdx+1
            end if
            !surfNum = 1
            return
        end if
        if(len(iptCmd).EQ.2) then
            if (iptCmd(2:2).EQ.'O' .OR. iptCmd(2:2).EQ.'0') then ! SO or S0 (letter-O or digit-zero)
                surfNum = 0
                return
            end if
            if (iptCmd(2:2).EQ.'I') then ! CMD is SI
                surfNum = curr_lens_data%num_surfaces-1 ! Not sure thsi is correct
                !call LogTermFOR("DEBUG:  Setting SurfNum to "//int2str(surfNum))
                return
            end if
        end if

        if(len(iptCmd).GT.1) then
            if (isInputNumber(iptCmd(2:len(iptCmd)))) surfNum = str2int(iptCmd(2:len(iptCmd)))
        end if




      end function

      

    function isSpecialGlass(tstStr) result(boolResult)
        character(len=*) :: tstStr
        logical :: boolResult

        boolResult = .FALSE.

        if(tstStr.EQ.'AIR') boolResult = .TRUE.
        if(tstStr.EQ.'REFL') boolResult = .TRUE.


    end function


    

    
    
    ! THis should go somewhere else, but not sure where so keep it here for now
    

    


    

    

    !Format PIK PARAM Si [PARAM] Sj [sf off]
    !Set value of Si based on Sj*sf + off and param2 if it exists

    ! The error checking/parsing here is a mess.  There must be a better way but not confident enough of a design to 
    ! eval this at present

    


    

    



    !Format:  CMD sk wk fk rx ry 
    


        !        
    !     (iptCmd)
    !     use command_utils, only : checkCommandInput, getInputNumber
    !     character(len=*) :: iptCmd
    !     integer :: surfNum

    !     surfNum = getSurfNumFromSurfCommand(trim(iptCmd))

        
       
    !     if (checkCommandInput([ID_CMD_NUM], max_num_terms=3)) then
    !         call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
    !         & '; RD, ' // real2str(getInputNumber(1))//";TH, "// &
    !         & real2str(getInputNumber(2)))
    !     end if            

    !   end subroutine

    

end module
