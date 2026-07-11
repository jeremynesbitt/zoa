submodule (codeV_commands) mod_sur
implicit none
contains
module procedure execSUR
    ! for now support SUR SA only
    ! New code - add abstraction of row titles and new columns of RMD GLA CCY THC GLC
                
        use strings, only: parse
        use mod_lens_data_manager

        implicit none

        !class(zoa_cmd) :: self
        integer :: ii
        character(len=80) :: tokens(40)
        character(len=256) :: fullLine
        character(len=4)  :: surfTxt
        character(len=23) :: radTxt
        character(len=23) :: thiTxt
        character(len=15) :: glaTxt
        character(len=10) :: rmdTxt
        integer :: numTokens

        !numSurfaces = curr_lens_data%num_surfaces
        !call LogTermFOR("Num Surfaces is "//trim(int2str(curr_lens_data%num_surfaces)))

        call parse(trim(iptStr), ' ', tokens, numTokens)
        if (numTokens == 2 ) then
            if (isSurfCommand(trim(tokens(2)))) then
                !call logTermFOR("SUR Cmd here!")
                ! SA
                fullLine = blankStr(10)//"RDY"//blankStr(10)//"THI"//blankStr(5)//"RMD"//blankStr(10)//"GLA" &
                & //blankStr(10)//"CCY"//blankStr(5)//"THC"//blankStr(5)//"GLC"
                call zoa_emit(trim(fullLine), "black")
                ! 12/12/24:  Change this to 0 indexed surfaees
                do ii=0,ldm%getLastSurf()
                    surfTxt = ldm%getSurfName(ii)//':'
                    if (ldm%getSurfRad(ii) == 0) then
                        radTxt = 'INFINITY'
                    else ! Should abstract this with THI
                        if(ldm%getSurfRad(ii) < 0) then
                           radTxt = real2str(ldm%getSurfRad(ii),3)
                        else
                           radTxt = real2str(ldm%getSurfRad(ii),5)
                        end if
                    end if
                    if (ldm%getSurfThi(ii) > 1e10) then
                        thiTxt = 'INFINITY'
                    else
                        
                        if (ldm%getSurfThi(ii) < 0) then
                            thiTxt = real2str(ldm%getSurfThi(ii),3)
                        else
                            thiTxt = real2str(ldm%getSurfThi(ii),5)
                        end if

                    end if
                    glaTxt = ldm%getGlassName(ii)
  

                    if (glaTxt(1:4).EQ.'REFL') then
                        rmdTxt = 'REFL'//blankStr(6)
                    else
                        rmdTxt = blankStr(len(rmdTxt))
                    end if

                    glaTxt = getGlassText(ii)


                    fullLine = surfTxt//blankStr(4)//trim(radTxt)// &
                    & blankStr(4)//trim(thiTxt)//blankStr(5)//rmdTxt &
                    & //glaTxt//ldm%getCCYCodeAsStr(ii)//blankStr(5)//ldm%getTHCCodeAsStr(ii)//blankStr(5)         


                    call zoa_emit(trim(fullLine), "black")


                end do
                ! Pseudocode for a better solution
                ! call ldm%gatherSpecialSurfaces(nSpecial, specTypes)
                ! if (nSpecial > 0) then 
                ! do ii=1,nSpecial
                ! surfs = ldm%getAllSurfacesOfType(specTypes(ii))
                ! print ldm%getSurfaceTypeName(specTypes(ii)) 
                ! print ldm%getSurfaceTypeExtraParamNames(specTypes(ii))
                ! print ldm%getSurfaceTypeExtraParamValues(specTypes(ii))
                ! end 
                !
                ! If there are special surfaces, print extra parameters.  For now only look for aspheres
                do ii=0,ldm%getLastSurf()
                    if (ldm%getSurfTypeName(ii) == 'Asphere') then 
                        call zoa_emit('___', "black")
                        call zoa_emit('ASPHERES', "black")

                        fullLine = '# '//trim(int2str(ii))//' K '//trim(real2str(ldm%getConicConstant(ii)))
                        call zoa_emit(trim(fullLine), "black")
                    end if
                end do
                



            else

            call zoa_emit("SUR Should have a surface identifier (S0, Sk, Si, SA)", "red")
            end if

        else
            call zoa_emit("No Surface identifier given!  Please try again", "red")

        end if



    end procedure

    !## cmd:      THI
    !## syntax:   THI Sk X  |  THI Sk <qual> j
    !## category: Surface Parameters
    !## desc:     Sets the thickness on surface Sk to X.
    !##           Sk can be S0 (object), S1..SN (surface by number), or Si (current surface).
    !##           X is the new thickness value in current lens units.
    !##           A solve qualifier sets a thickness solve instead: HMY j (paraxial
    !##           marginal height, KDP PY), HCY j (chief height, PCY), HMX/HCX for XZ.
    !##
    module procedure setThickness
        logical :: handled
        call tryCodeVSolve(iptStr, 'THI', handled)
        if (.not. handled) call execTranslatedSurfCmd(iptStr, 'TH')

    end procedure

    !## cmd:      GLA
    !## syntax:   GLA Sk name
    !## category: Surface Parameters
    !## desc:     Sets the glass at surface Sk to the named material.
    !##           Searches through available glass catalogs for the name.
    !##           Use a numeric nd,vd pair (e.g. 1.5,50) to specify a model glass.
    !##
    module procedure setGlass
        use command_utils, only : checkCommandInput, getInputNumber, isInputNumber
        use glass_manager, only: parseModelGlassEntry
        use DATMAI
        use strings

        implicit none
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens


        !call updateTerminalLog("Starting to update GLA ", "blue" )
        call parse(iptStr, ' ', tokens, numTokens)

        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; '//trim(getSetGlassText(trim(tokens(3))))//';GO')        
        else
            call zoa_emit("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if                
               
    end procedure

    function getGlassText(surf) result(glaTxt)
        use type_utils, only: blankStr
        use mod_lens_data_manager
        implicit none
        character(len=15) :: glaTxt
        integer, intent(in) :: surf

        glaTxt = blankStr(len(glaTxt))! Initialize

        glaTxt = ldm%getGlassName(surf)
        !glaTxt = curr_lens_data%glassnames(surf)


        ! if (glaTxt(1:1).NE.' ') then
        ! !else
        !     glaTxt = trim(curr_lens_data%glassnames(surf))//'_'//trim(curr_lens_data%catalognames(surf))
        ! end if 

        if (glaTxt(1:4).EQ.'REFL') then
            glaTxt = blankStr(len(glaTxt))         
        end if   

    end function

    !## cmd:      CUY
    !## syntax:   CUY Sk X  |  CUY Sk <qual> j
    !## category: Surface Parameters
    !## desc:     Sets the YZ curvature (1/radius) on surface Sk to X.
    !##           Sk can be S0 (object), S1..SN (surface by number), or Si (current surface).
    !##           A solve qualifier sets a curvature solve instead: AMY (aplanatic
    !##           marginal, KDP APY), ACY (aplanatic chief, APCY), IMY j (angle of
    !##           incidence, PIY), ICY j (PICY), UMY j (ray slope, PUY), UCY j (PUCY).
    !##
    module procedure setCurvature
        use command_utils, only : isInputNumber
        use strings, only: parse
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        logical :: handled

        ! Solve form (CUY Sk <qual> j) handled by the shared helper.
        call tryCodeVSolve(iptStr, 'CUY', handled)
        if (handled) return

        call parse(iptStr, ' ', tokens, numTokens)
        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            if (numTokens > 2) then
               if (isInputNumber(trim(tokens(3)))) then ! FORMAT: CUY Sk VAL
                ! Like RDY: refresh the edited surface's typed-store radius before
                ! the finalizing EOS so a PIM/PY solve resolves off the new
                ! curvature (drop ';GO', let executeCodeVLensUpdateCommand EOS).
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; CV, ' // trim(tokens(3)), refreshSurf=surfNum)
               else
                call zoa_emit("Unrecognized YZ curvature solve.  Expect e.g. CUY Sk UMY j", "red")
               end if ! Tokens > 2 loop
            else
                call zoa_emit("No Angle Solve Specified.  Please try again", "red")
            end if

        else
            call zoa_emit("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if

    end procedure

    !## cmd:      CUX
    !## syntax:   CUX Sk <qual> j
    !## category: Surface Parameters
    !## desc:     Sets an XZ (X-toric) curvature solve on surface Sk.  Qualifiers:
    !##           AMX (aplanatic marginal, KDP APX), ACX (aplanatic chief, APCX),
    !##           IMX j (angle of incidence, PIX), ICX j (PICX), UMX j (ray slope,
    !##           PUX), UCX j (PUCX).
    !##
    module procedure setCurvatureX
        implicit none
        logical :: handled

        call tryCodeVSolve(iptStr, 'CUX', handled)
        if (.not. handled) call zoa_emit( &
        & "Unrecognized XZ curvature solve.  Expect e.g. CUX Sk UMX j", "red")

    end procedure

    !## cmd:      RDY
    !## syntax:   RDY Sk X
    !## category: Surface Parameters
    !## desc:     Sets the radius of surface Sk to X.
    !##           Sk can be S0 (object), S1..SN (surface by number), or Si (current surface).
    !##           X is the new radius value in current lens units.
    !##
    module procedure setRadius
        use strings
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        if(isSurfCommand(trim(tokens(2)))) then
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            ! No ';GO': let executeCodeVLensUpdateCommand issue the finalizing EOS,
            ! and pass refreshSurf so it re-syncs this surface's typed-store radius
            ! from ALENS *before* that EOS traces.  Otherwise a PIM/PY solve would
            ! resolve off the stale frozen radius and lag one edit behind.
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
            & '; RD, ' // trim(tokens(3)), refreshSurf=surfNum)
        else
            call zoa_emit("Surface not input correctly.  Should be SO or Sk where k is the surface of interest", "red")
            return
        end if             
       
    end procedure

    module procedure execAsphere
        use strings, only: parse
        use mod_lens_data_manager
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, surfNum

        call parse(iptStr, ' ', tokens, numTokens)

        select case (numTokens)
        case (1)
            surfNum = ldm%getSurfacePointer()
        case default
            if (.not. isSurfCommand(trim(tokens(2)))) then
                call zoa_emit("ASP: usage: ASP [Sk]", "red")
                return
            end if
            surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
        end select

        call ldm%setSurfaceType(surfNum, 'ASP')
        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum)))
    end procedure

    module procedure execSphere
        use strings, only: parse
        use mod_lens_data_manager
        implicit none

        character(len=80) :: tokens(40)
        integer :: numTokens, surfNum

        call parse(iptStr, ' ', tokens, numTokens)

        if (numTokens < 2 .or. .not. isSurfCommand(trim(tokens(2)))) then
            call zoa_emit("SPH: usage: SPH Sk", "red")
            return
        end if
        surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
        call ldm%setSurfaceType(surfNum, 'SPH')
        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum)), exitLensUpdate=.TRUE.)
    end procedure

    module procedure updateAsphereTerms
        use strings
        use DATLEN, only: ALENS
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        character(len=2) :: aspKDP
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)

        ! Copied from CV2PRG (essentially)
        select case(trim(tokens(1)))
        case('A')
           aspKDP =  'AD'
        case('B')
            aspKDP = 'AE'
        case('C')
            aspKDP = 'AF'
        case('D')
            aspKDP = 'AG'
        case('E')
            aspKDP = 'AH'
        case('F')
            aspKDP = 'AI'
        case('G')
            aspKDP = 'AJ'
        case('H')
            aspKDP = 'AK'
        case('I')
            aspKDP = 'AL'

        end select
        call execTranslatedSurfCmd(iptStr, aspKDP)
    end procedure   
    
    ! Format
    ! K Sk Val - update on lens Sk
    ! K Val - update current lens (eg when loading from file)
    ! K Sk - return val on current lens (not currently implemented) 
    module procedure updateConicConstant
        use command_utils, only: isInputNumber
        use strings, only: parse
        use mod_surface, only: set_surf_conic, set_surf_asphere_flag, surf_is_asphere
        use mod_lens_data_manager
        implicit none

        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens
        real(real64) :: newVal

        call parse(iptStr, ' ', tokens, numTokens)

        select case (numTokens)
        case(2)
            if (isSurfCommand(trim(tokens(2)))) then
                call zoa_emit("K: no value specified for surface "//trim(tokens(2)), "red")
                return
            else if (isInputNumber(trim(tokens(2)))) then
                surfNum = ldm%getSurfacePointer()
                read(tokens(2), *) newVal
                call applyConicUpdate(surfNum, newVal)
                return
            end if
        case(3)
            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
                if (isInputNumber(trim(tokens(3)))) then
                    read(tokens(3), *) newVal
                    call applyConicUpdate(surfNum, newVal)
                    return
                end if
            end if
        end select

        call zoa_emit("K: usage: K [Sk] value", "red")

    contains

        subroutine applyConicUpdate(s, val)
            use type_utils, only: int2str
            integer, intent(in)      :: s
            real(real64), intent(in) :: val
            call set_surf_conic(s, val)
            ! Mirror what the KDP CC handler does: mark asphere when K != 0
            if (val /= 0.0_real64) call set_surf_asphere_flag(s, .true.)
            call ldm%load_surfaces_from_alens()
            call executeCodeVLensUpdateCommand('CHG '//trim(int2str(s)))
        end subroutine

    end procedure

    module procedure updateSurfaceLabel
        call execTranslatedSurfCmd(iptStr, 'LBL')    
    end procedure

    ! Expected Format for iptStr:
    ! cmd Sk Val - update on lens Sk
    ! cmd Val - update current lens (eg when loading from file)
    ! cmd Sk - return val on current lens (not currently implemented) 
    ! kdpCmd - the translated command for cmd
    subroutine execTranslatedSurfCmd(iptStr, kdpCmd)
        use command_utils, only: isInputNumber, removeQuotes
        use mod_lens_data_manager
        
        character(len=*) :: iptStr
        character(len=*) :: kdpCmd
        integer :: surfNum
        character(len=80) :: tokens(40)
        integer :: numTokens

        call parse(iptStr, ' ', tokens, numTokens)
        
        select case (numTokens)

        case(2) 
            if (isSurfCommand(trim(tokens(2)))) then
                    surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
            else
                if (isInputNumber(trim(tokens(2)))) then
                    ! Use current surface
                    surfNum = ldm%getSurfacePointer()
                    call executeCodeVLensUpdateCommand(kdpCmd//' '//trim(tokens(2)))
                    return 
                else
                    ! Some commands are not numbers
                    if (trim(kdpCmd) == 'LBL') then 
                        surfNum = ldm%getSurfacePointer()    
                        tokens(2) = removeQuotes(trim(tokens(2)))
                        call executeCodeVLensUpdateCommand(kdpCmd//' '//trim(tokens(2)))
                        return 
                    end if                       

                ! If not number and no special case then complain    
                call zoa_emit("Error! For "//trim(tokens(1))//"expect second argument to be Sk &
                & or value to update for current lens pointer surface ", "red")
                return
                end if
            end if

        case(3) ! K Sk Val
            if (isSurfCommand(trim(tokens(2)))) then
                surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))  
            end if          
            if (isInputNumber(trim(tokens(3)))) then
                call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                & '; '//kdpCmd//' '//trim(tokens(3)))
                return 
            else
                ! Special case
                if (trim(kdpCmd) == 'LBL') then 
                    tokens(3) = removeQuotes(trim(tokens(3)))
                    call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
                    & '; '//kdpCmd//' '//trim(tokens(3)))                       
                end if                
            end if           
        end select


    end subroutine

    ! Try to interpret "<verb> Sk <qual> [val]" as a CODE V solve (e.g.
    ! THI S2 HCY 1.5, CUY S3 UMY 0.1, CUX S3 AMX).  If tokens(3) is a solve
    ! qualifier for this verb, issue the matching KDP solve on surface Sk and
    ! set handled=.TRUE.  Otherwise handled=.FALSE. and the caller falls back
    ! to its normal (plain value) handling.  All the command<->KDP knowledge
    ! lives in the solve_manager kind table.
    subroutine tryCodeVSolve(iptStr, verb, handled)
        use iso_fortran_env, only: real64
        use command_utils, only: isInputNumber
        use solve_manager, only: solve_kdp_from_codev, solve_kind_from_cmd, solve_set_cmd
        character(len=*), intent(in) :: iptStr, verb
        logical, intent(out) :: handled
        integer :: surfNum, numTokens, kidx
        character(len=80) :: tokens(40)
        character(len=8) :: kdpWord
        real(real64) :: target

        handled = .FALSE.
        call parse(iptStr, ' ', tokens, numTokens)
        if (numTokens < 3) return
        if (.not. isSurfCommand(trim(tokens(2)))) return

        ! Is tokens(3) a solve qualifier for this verb?  (Empty => it's a plain
        ! numeric value, or garbage; let the caller deal with it.)
        kdpWord = solve_kdp_from_codev(verb, trim(tokens(3)))
        if (len_trim(kdpWord) == 0) return

        surfNum = getSurfNumFromSurfCommand(trim(tokens(2)))
        target = 0.0d0
        if (numTokens >= 4) then
            if (isInputNumber(trim(tokens(4)))) read(tokens(4), *) target
        end if

        kidx = solve_kind_from_cmd(trim(kdpWord))
        call executeCodeVLensUpdateCommand('CHG '//trim(int2str(surfNum))// &
        & '; '//trim(solve_set_cmd(kidx, target)), refreshSurf=surfNum)
        handled = .TRUE.
    end subroutine


end submodule