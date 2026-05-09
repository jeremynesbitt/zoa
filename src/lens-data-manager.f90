!Notes:
!It should be true that surfaces start from 0.  curr_lens_data is not stored this way
!so this can lead to confusion

! Notes on longer term implementation.
! this holds a series of objects (surfaces)
! for zooms, then need and array of systems
! for saving, each object will have a genSavOutputText
! for ui, each object will have a get / set cmd stored for reading / writing
! things like solves, pickups, and apertures are separate objects?

module mod_lens_data_manager
    use iso_fortran_env, only: real64
    use global_widgets, only: curr_lens_data, curr_par_ray_trace, sysConfig
    use globals, only: long
    use zoa_ui

    implicit none

    type lens_data_manager

    real(long), dimension(0:499,3) :: vars ! CCY THC GLC for now.  Hard code max of 500 surfaces.  Default is 100
   
    contains
     procedure :: initialize => init_ldm
     procedure, public, pass(self) :: getSurfThi, setSurfThi
     procedure, public, pass(self) :: isThiSolveOnSurf
     procedure, public, pass(self) :: isYZCurvSolveOnSurf
     procedure, public, pass(self) :: getSurfCurv, getSurfRad
     procedure, public, pass(self) :: getConicConstant
     procedure, public, pass(self) :: getSurfIndex
     procedure, public, pass(self) :: getIndexRatio
     procedure, public, pass(self) :: getRefractionPowerFactor
     procedure, public, pass(self) :: getSurfDispersion
     procedure, public, pass(self) :: getLastSurf
     procedure, public, pass(self) :: getEFL
     procedure, public, pass(self) :: getTrackLength
     procedure :: getCurrentConfig
     procedure :: getSurfName, getSurfLabel
     procedure :: getStopSurf
     procedure :: isGlassSurf
     procedure :: getGlassName
     procedure :: updateOptimVars
     procedure :: setVarOnSurf
     procedure :: isSolveOnSurf, isPikupOnSurf, isVarOnSurf
     procedure :: getCCYCodeAsStr, getTHCCodeAsStr
     procedure :: getSurfacePointer, incrementSurfacePointer
     procedure, public, pass(self) :: genSaveOutputText => genLDMSaveOutputText
     procedure :: outputPikupText, genSurfPikupSavText, getSurfTypeName, getExtraParamCmd
     procedure, public, pass(self) :: removeAllSurfaceData
     procedure, public, pass(self) :: clearClearApertureData
     procedure, public, pass(self) :: clearObscurationData
     procedure, public, pass(self) :: clearGrtInitData
     procedure, public, pass(self) :: clearGratingData
     procedure, public, pass(self) :: clearToricData
     procedure, public, pass(self) :: clearTiltAngles
     procedure, public, pass(self) :: clearTiltDegAngles
     procedure, public, pass(self) :: clearGlobalCoordData
     procedure, public, pass(self) :: clearDecentData
     procedure, public, pass(self) :: clearPivotAndFocusData
     procedure, public, pass(self) :: initRefractiveIndices
     procedure, public, pass(self) :: clearCurvAndConic
     procedure, public, pass(self) :: clearAsphericCoeffs
     procedure, public, pass(self) :: clearAsphericData
     procedure, public, pass(self) :: clearClearApertureParams
     procedure, public, pass(self) :: clearObscurationShape
     procedure, public, pass(self) :: clearObscurationShapeParams
     procedure, public, pass(self) :: clearToricFlagAndCurv
     procedure, public, pass(self) :: clearTiltAlphaBetaGamma
     procedure, public, pass(self) :: clearTiltAnglesAndDecentValues
     procedure, public, pass(self) :: clearTiltAndDecentData
     procedure, public, pass(self) :: clearDecentFlagAndY
     procedure, public, pass(self) :: clearDecentValues
     procedure, public, pass(self) :: clearDecentAnamAndConfig
     procedure, public, pass(self) :: clearXDecentAnamAndConfig
     procedure, public, pass(self) :: clearSolvesAndSurfType
     procedure, public, pass(self) :: clearAnamorphicCoeffs
     procedure, public, pass(self) :: clearApertureRegion
     procedure, public, pass(self) :: clearPivotData
     procedure, public, pass(self) :: clearPivotXY
     procedure, public, pass(self) :: clearPivotAndHigherAspherics
     procedure, public, pass(self) :: clearHigherOrderAsphericCoeffs
     procedure, public, pass(self) :: clearDeformableData
     procedure, public, pass(self) :: clearFocusData
     procedure, public, pass(self) :: clearFocusAndTiltDegData
     procedure, public, pass(self) :: clearGlassColorData
     procedure, public, pass(self) :: clearMultiApertureData
     procedure, public, pass(self) :: clearArrayData
     procedure, public, pass(self) :: clearSpiderData
     procedure, public, pass(self) :: clearExtendedSurfaceData
     procedure, public, pass(self) :: clearAsphericCoeffsAndFlag
     procedure, public, pass(self) :: clearAnamorphicData
     procedure, public, pass(self) :: clearApertureTypeAndParams
     procedure, public, pass(self) :: clearApertureTypeAndAllParams

    end type

    type(lens_data_manager) :: ldm

    

    contains
    subroutine init_ldm(self)
        class (lens_data_manager) :: self
        ! Set all vars to default
        ldm%vars(:,:) = 100
    
    end subroutine

    function getGlassName(self, idx) result(strGlassName)
        use DATLEN, only: GLANAM
        use type_utils, only: blankStr
        implicit none
        class(lens_data_manager) :: self
        integer :: idx
        character(len=15) :: strGlassName

        strGlassName = trim(GLANAM(idx,2))//'_'//trim(GLANAM(idx,1))

        ! Zero out if we get AIR or LAST SURFACE
        if (GLANAM(idx,2).EQ.'AIR') strGlassName = blankStr(len(strGlassName))
        if (GLANAM(idx,2).EQ.'LAST SURFACE') strGlassName = blankStr(len(strGlassName))        



    end function

    function isGlassSurf(self, idx) result(boolResult)
        use DATLEN, only: GLANAM
        class(lens_data_manager) :: self
        integer :: idx
        logical :: boolResult

        boolResult = .TRUE.
        if (GLANAM(idx,2) == 'AIR') boolResult = .FALSE.
        if (GLANAM(idx,2).EQ.'LAST SURFACE') boolResult = .FALSE.

    end function

    function getStopSurf(self) result(iStop)
        class(lens_data_manager) :: self
        integer :: iStop

        ! Since curr lens data is not a 0 indexed array, subtract 1
        iStop = curr_lens_data%ref_stop-1


    end function

    function getSurfName(self, idx) result(strName)
        class(lens_data_manager) :: self
        integer, intent(in) :: idx
        character(len=3) :: strName

        write(strName, '(I0.3)')  idx
        ! Handle Special surfaces
        if (idx==0) strName = 'OBJ'
        if (idx==self%getStopSurf()) strName = 'STO'
        if (idx==self%getLastSurf()) strName = 'IMG'

    end function
 
    function getSurfLabel(self, idx) result(strLabel)
        use DATLEN, only : LBL
        class(lens_data_manager) :: self
        integer, intent(in) :: idx
        character(len=80) :: strLabel

        strLabel = LBL(idx)(1:80)

    end function    

    !Sphere, Asphere, etc.  eventually need to compile a list of these
    !For UI selection
    function getSurfTypeName(self, idx) result(strName)
        use mod_surface, only: surf_is_asphere
        class(lens_data_manager) :: self
        integer, intent(in) :: idx
        character(len=20) :: strName

        strName='Sphere'
        if(surf_is_asphere(idx)) strName='Asphere'

    end function

    !TODO:  update this when surf type abstraction is ready
    function getExtraParamCmd(self, surfIdx, colIdx) result (cmd)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx, colIdx
        character(len=20) :: strName
        character(len=3)  :: cmd
        character(len=3), dimension(10) :: extraParamCmds = ['K', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I']

        strName = self%getSurfTypeName(surfIdx)

        cmd = '  '
        select case (trim(strName))
        case('Sphere') ! No extra params
            cmd = ' '
        case('Asphere')
            if(colIdx < size(extraParamCmds)) then 
                cmd = extraParamCmds(colIdx)
            end if
        end select

    end function       

    function getCurrentConfig(self) result(cfg)
        class(lens_data_manager) :: self
        integer :: cfg

        cfg = 1 ! TODO:  Update when configs are fully supported

    end function

    function getLastSurf(self) result(Sf)
        class(lens_data_manager) :: self
        integer :: Sf

        Sf = curr_lens_data%num_surfaces-1

    end function

    function getSurfacePointer(self) result(idx)
        use DATLEN, only: SURF
        implicit none
        class(lens_data_manager) :: self 
        integer :: idx

        idx = SURF

    end function

    subroutine incrementSurfacePointer(self) 
        use DATLEN, only: SURF
        implicit none
        class(lens_data_manager) :: self 

        SURF = SURF + 1

    end subroutine 

    function getSurfThi(self, surfIdx) result(thi)
        use mod_surface, only: surf_thickness
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: thi

        thi = surf_thickness(surfIdx)
        !thi = curr_lens_data%thicknesses(surfIdx+1)

    end function

    function getConicConstant(self, surfIdx) result(k)
        use mod_surface, only: surf_conic
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: k

        k = surf_conic(surfIdx)

    end function


    subroutine setSurfThi(self, surfIdx, thi)
        use mod_surface, only: set_surf_thickness
        implicit none
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: thi

        call set_surf_thickness(surfIdx, thi)
   
    end subroutine    

    function isThiSolveOnSurf(self, surfIdx) result(boolResult)
        use DATLEN, only: SOLVE
        class(lens_data_manager) :: self
        integer :: surfIdx
        logical :: boolResult

        boolResult = .FALSE.
        IF(SOLVE(6,surfIdx).NE.0.0D0) boolResult = .TRUE.

    end function

    function isYZCurvSolveOnSurf(self, surfIdx) result(boolResult)
        use DATLEN, only: SOLVE
        class(lens_data_manager) :: self
        integer :: surfIdx
        logical :: boolResult

        boolResult = .FALSE.
        IF(SOLVE(8,surfIdx).NE.0.0D0) boolResult = .TRUE.

    end function

    function getSurfRad(self, surfIdx) result (rad)
        implicit none
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: rad

        rad = curr_lens_data%radii(surfIdx+1)

    end function

    function getSurfCurv(self, surfIdx, useXZPlane) result(curv)
        use mod_surface, only: surf_toric_flag, surf_toric_curvature, &
                               surf_curvature, surf_asphere_coeff
        class(lens_data_manager) :: self
        integer :: surfIdx
        logical, optional :: useXZPlane
        real(kind=real64) :: curv

        ! TODO: clean this up, add XZ logic

        ! X-toric surfaces store the toric curvature at index 24
        if (surf_toric_flag(surfIdx) == 2) then
            curv = surf_toric_curvature(surfIdx)
        else
            ! Plano surfaces with a 2nd-order aspheric term store effective curvature there
            if (surf_curvature(surfIdx) == 0.0_real64 .and. &
                surf_asphere_coeff(surfIdx, 2) /= 0.0_real64) then
                curv = surf_asphere_coeff(surfIdx, 2) * 2.0_real64
            else
                curv = surf_curvature(surfIdx)
            end if
        end if

    end function 

    function getEFL(self) result(EFL)
        implicit none
        class(lens_data_manager) :: self
        real(kind=real64) :: EFL

        EFL = curr_par_ray_trace%EFL

    end function

    function getTrackLength(self) result (OAL)
        implicit none
        class(lens_data_manager) :: self
        real(kind=real64) :: OAL

        OAL = curr_par_ray_trace%OAL


    end function


    
    function getSurfIndex(self, surfIdx, lambdaIdx) result(index)        
        use DATLEN, only: ALENS
        class(lens_data_manager) :: self
        integer :: surfIdx
        integer, optional :: lambdaIdx
        real(kind=real64) :: index

        INTEGER :: WWVN

        ! TODO:  CLean up 

        if(present(lambdaIdx) .EQV. .FALSE. ) then
            WWVN = sysConfig%refWavelengthIndex
        else
           if (lambdaIdx.GT.0.AND.lambdaIdx.LT.6) then
            WWVN = lambdaIdx+45 ! From 46-50
           end if
           if (lambdaIdx.GT.5.AND.lambdaIdx.LT.11) then
            WWVN = lambdaIdx+65 ! From 71-75
           end if           
        end if

        index = ALENS(WWVN,surfIdx)

    end function

    function getIndexRatio(self, surfPrev, surfCurr, wlNum) result(ratio)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfPrev, surfCurr, wlNum
        real(kind=real64) :: ratio
        ratio = self%getSurfIndex(surfPrev, wlNum) / self%getSurfIndex(surfCurr, wlNum)
    end function

    function getRefractionPowerFactor(self, surfPrev, surfCurr, wlNum) result(factor)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfPrev, surfCurr, wlNum
        real(kind=real64) :: factor
        factor = (self%getSurfIndex(surfCurr, wlNum) - self%getSurfIndex(surfPrev, wlNum)) / &
                 self%getSurfIndex(surfCurr, wlNum)
    end function

    function getSurfDispersion(self, surfIdx, wl1Num, wl2Num) result(disp)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx, wl1Num, wl2Num
        real(kind=real64) :: disp
        disp = self%getSurfIndex(surfIdx, wl1Num) - self%getSurfIndex(surfIdx, wl2Num)
    end function

    subroutine updateOptimVars(self, varName, s0,sf,intCode)
        use type_utils
        !use optim_types
        class(lens_data_manager) :: self
        character(len=*) :: varName
        integer, intent(in) :: s0, sf, intCode
        integer :: i, VAR_CODE

        select case (varName)
        case('THC')
            VAR_CODE = VAR_THI
        case('CCY')
            VAR_CODE = VAR_CURV
        case('KC')
            VAR_CODE = VAR_K
        end select

        if (s0==sf) then
            call self%setVarOnSurf(s0, VAR_CODE, intCode)                                
        else
            do i=s0,sf
                call self%setVarOnSurf(i, VAR_CODE, intCode)    
            end do
        end if

     end subroutine

    function isVarOnSurf(self, surf, var_code) result(boolResult)
        class(lens_data_manager) :: self
        integer, intent(in) :: surf, var_code
        logical :: boolResult


        ! Default is false
        boolResult = .FALSE.

        if (self%vars(surf,var_code) /= 100) then
            boolResult = .TRUE.
        end if


    end function


    function isSolveOnSurf(self, surf, var_code) result(boolResult)
        class(lens_data_manager) :: self
        integer, intent(in) :: surf, var_code
        logical :: boolResult

        ! Default is false
        boolResult = .FALSE.

        select case (var_code)
        case (VAR_CURV)
            boolResult = self%isYZCurvSolveOnSurf(surf)
        case (VAR_THI)
            boolResult = self%isThiSolveOnSurf(surf)
        end select

    end function

    ! TODO:  Refactor with solve?
    function isPikupOnSurf(self, sur, var_code) result(boolResult)
        use DATLEN
        implicit none
        class(lens_data_manager) :: self
        integer, intent(in) :: sur, var_code
        logical :: boolResult

        ! Default is false
        boolResult = .FALSE.

        select case (var_code)
        case (VAR_CURV)
            if(PIKUP(1,sur, ID_PICKUP_RAD) == 1.0) boolResult = .TRUE.
        case (VAR_THI)
            if(PIKUP(1,sur,ID_PICKUP_THIC) == 1.0) boolResult = .TRUE.
        end select

    end function


    subroutine setVarOnSurf(self, surf, var_code, val)
        use type_utils, only: int2str
        implicit none

        class(lens_data_manager) :: self
        integer, intent(in) :: surf, var_code, val

        ! Make sure there are no solves or pickups on the surface.  

        if (self%isSolveOnSurf(surf, var_code)) then
            call logtermFOR("Error!  Cannot add variable to surface "//int2str(surf)//" due to presence of solve.  Please remove it first")
            return
        end if
        if (self%isPikupOnSurf(surf, var_code)) then
            call logtermFOR("Error!  Cannot add variable to surface "//int2str(surf)//" due to presence of solve.  Please remove it first")
            return
        end if


        ! Code is 0.  Place it in if var_code is within range
        if (var_code > 0 .and. var_code <= ubound(self%vars,dim=2)) then 
            print *, "UPdating value ", val
        self%vars(surf,var_code) = val
        ! Add variable count so optmizer knows
        ! Want to do it here but need to resolve circular dependency
        !call addOptimVariable(surf, var_code)

        end if
                     

    end subroutine

  

    ! These funcs are interfaces for printing info in SUR 
    function getCCYCodeAsStr(self, si) result(outStr)
        use type_utils

        implicit none
        
        class(lens_data_manager) :: self
        character(len=3) :: outStr
        integer :: si

        integer :: optimCode

        outStr = '100'

        outStr = int2str(INT(self%vars(si,1)))


    end function

    function getTHCCodeAsStr(self, si) result(outStr)
        use type_utils

        implicit none
        
        class(lens_data_manager) :: self
        character(len=3) :: outStr
        integer :: si

        integer :: optimCode

        outStr = '100'

        outStr = int2str(INT(self%vars(si,2)))


    end function    

    subroutine outputPikupText(self, fID)
        class(lens_data_manager) :: self
        integer :: fID
        integer :: ii, jj
        character(len=512) :: outTxt
        integer, dimension(2) :: pickupTypes = [ID_PICKUP_RAD, ID_PICKUP_THIC]

        do ii=0,self%getLastSurf()
            do jj=1,size(pickupTypes)
                outTxt = self%genSurfPikupSavText(ii, pickupTypes(jj))
                if (len(trim(outTxt)) > 0) then 
                    write(fID, *) trim(outTxt)
                end if
            end do
        end do


    end subroutine

    function genSurfPikupSavText(self, surf, var_code) result(outTxt)
        use DATLEN, only: PIKUP
        use type_utils
        class(lens_data_manager) :: self
        integer :: surf, var_code, si, sf
        character(len=512) :: outTxt, surfTxt 
        character(len=3) :: pType
        real(kind=long) :: scale, offset

        outTxt = ' '
        if (self%isPikupOnSurf(surf, var_code)) then 
            si = INT(PIKUP(2,surf,var_code)) ! Start Surface
            sf = si ! Temp
            !sf = INT(PIKUP(3,surf,var_code)) ! End Surface
            scale = PIKUP(3,surf, var_code) ! Default 1
            offset = PIKUP(4,surf,var_code) ! Default 0

            select case (var_code)
            case (ID_PICKUP_RAD)
                pType = 'RDY'
            case (ID_PICKUP_THIC)
                pType = 'THI'
            end select

            if (si==sf) then
                surfTxt = 'S'//trim(int2str(si))
            else
                surfTxt = 'S'//trim(int2str(si))//'..'//trim(int2str(sf))
            end if

            outTxt = 'PIK '//trim(pType)//' S'//trim(int2str(surf))//' '//trim(pType)//' '// &
            & trim(surfTxt)//' '//trim(real2str(scale,4))//' '//trim(real2str(offset,4))
           
        end if

    end function


    subroutine genLDMSaveOutputText(self, fID)
        use type_utils, only: real2str, blankStr, int2str
        use zoa_file_handler, only: genOutputLineWithSpacing
        use mod_surface, only: surf_curvature, surf_radius, surf_thickness, surf_conic
        class(lens_data_manager) :: self
        integer :: fID
        integer :: ii, jj
        character(len=1024) :: strSurfLine, strTHI, strRdy
        character(len=4) :: surfStr
        !character(len=80) :: glassStr
        logical :: rdmFlag
      
        rdmFlag = .TRUE.
      
        ! Do Object SUrface
        strSurfLine = 'SO'
        write(strTHI, '(D23.15)') surf_thickness(0)
        if (surf_curvature(0) == 0.0_real64) then
          write(strRdy, '(D23.15)') 0.0d0
        else
          write(strRdy, '(D23.15)') surf_radius(0)
        end if
      
        if (rdmFlag) then
          if (curr_lens_data%thicknesses(1) > 1e11) then
          strSurfLine = genOutputLineWithSpacing(blankStr(1), 'SO', trim(strRdy), &
          & trim(strTHI), trim(curr_lens_data%glassnames(1)))
          else 
            strSurfLine = genOutputLineWithSpacing(blankStr(1), 'SO', trim(strRdy), &
            & trim(strTHI), trim(curr_lens_data%glassnames(1)))  
          end if
          !strSurfLine = 'SO'//blankStr(1)//trim(real2str(self%radii(1),4))//blankStr(1)// &
          !& trim(real2str(self%thicknesses(1),sci=.TRUE.))//blankStr(1)//trim(self%glassnames(1))
        else
          strSurfLine = 'SO'//blankStr(3)//real2str(curr_lens_data%curvatures(1),4)//blankStr(5)//real2str(curr_lens_data%thicknesses(1))// &
          & blankStr(5)//curr_lens_data%glassnames(1)    
        end if
        write(fID, *) trim(strSurfLine)
      
        do ii=2,curr_lens_data%num_surfaces-1
          surfStr = 'S' !//trim(int2str(ii-1))
          !if (ii==2) surfStr = 'S1'
      
          !glassStr = self%glassnames(ii)
          !if (isModelGlass(glassStr)) glassStr = set 
          write(strTHI, '(D23.15)') surf_thickness(ii-1)
          if (surf_curvature(ii-1) == 0.0_real64) then
            write(strRdy, '(D23.15)') 0.0d0
          else
            write(strRdy, '(D23.15)') surf_radius(ii-1)
          end if
          if(rdmFlag) then
            strSurfLine = genOutputLineWithSpacing(blankStr(1), trim(surfStr), & 
            & trim(strRdy), trim(strTHI), & 
            & trim(curr_lens_data%glassnames(ii)))      
            !strSurfLine = 'S'//int2str(ii-1)//blankStr(3)//real2str(self%radii(ii),4)// &
            !& blankStr(5)//real2str(self%thicknesses(ii),4)// &
            !& blankStr(5)//self%glassnames(ii)
          else
            strSurfLine = 'S'//int2str(ii-1)//blankStr(3)//real2str(curr_lens_data%curvatures(ii),9)// &
            & blankStr(5)//trim(strTHI)// &
            & blankStr(5)//curr_lens_data%glassnames(ii)    
          end if      
          write(fID, *) trim(strSurfLine)
          ! Check for ref stop
          if (curr_lens_data%ref_stop == ii) then
            strSurfLine = blankStr(2)//'STO'
            write(fID, *) trim(strSurfLine)
          end if
          if (trim(ldm%getSurfLabel(ii-1)) .NE. ' ') then
            strSurfLine = blankStr(2)//'SLB "'//trim(ldm%getSurfLabel(ii-1))//""""
            write(fID, *) trim(strSurfLine)
          end if
          
          ! Check for user specified clear aperture.  TODO:  Need to implement a more sophisticated
          ! way to store CA info, as the geometry is not always circular.  But for now
          ! just support circular until I get some to mkae it more abstract
          if (curr_lens_data%clearAps(ii)%userDefined .OR. curr_lens_data%ref_stop == ii) then
            strSurfLine = blankStr(2)//'CIR '//trim(real2str(curr_lens_data%clearAps(ii)%yRad, 10))
            write(fID, *) trim(strSurfLine)
          end if
      
          if (curr_lens_data%isAsphereOnSurface(ii-1)) then
            strSurfLine = curr_lens_data%genAsphereSavOutputText(ii-1, fID)
            !write(fID, *) trim(strSurfLine)
          end if
      
          ! Do not like directly acccessing ALENS here.  THink I should move this func to lens_Data_manager
          if (curr_lens_data%isConicConstantOnSurface(ii-1)) then
            strSurfLine = blankStr(2)//'K '//trim(real2str(surf_conic(ii-1), sci=.TRUE.))
            write(fID, *) trim(strSurfLine)
          end if
      
            if (curr_lens_data%isSolveOnSurface(ii)) then
              strSurfLine = curr_lens_data%thickSolves(ii)%genCodeVCMDToSetSolve()
              write(fID, *) trim(strSurfLine)
            end if
          
        end do
      
        ! Do Image SUrface.  
        write(strTHI, '(D23.15)') curr_lens_data%thicknesses(curr_lens_data%num_surfaces)
        if (rdmFlag) then
          strSurfLine = 'SI'//blankStr(2)//trim(real2str(curr_lens_data%radii(curr_lens_data%num_surfaces),4))//blankStr(3)// &
          & trim(strTHI)//blankStr(3)//curr_lens_data%glassnames(curr_lens_data%num_surfaces)
        else
          strSurfLine = 'SI'//blankStr(2)//trim(real2str(curr_lens_data%curvatures(curr_lens_data%num_surfaces),4))//blankStr(3)// &
          & trim(strTHI)//blankStr(3)//curr_lens_data%glassnames(curr_lens_data%num_surfaces)  
        end if
        write(fID, *) trim(strSurfLine)
        if (curr_lens_data%clearAps(curr_lens_data%num_surfaces)%userDefined) then
          strSurfLine = blankStr(2)//'CIR '//trim(real2str(curr_lens_data%num_surfaces))
          write(fID, *) trim(strSurfLine)
        end if  
      
      
        ! Gen pickup data
        ! iterate surfaces
        ! for each surface
        ! if any pickup type (list of pickups)
        ! gen pickup text
        ! Now that we are done send GO cmd to leave lens update level
        call self%outputPikupText(fID)
        write(fID, *) "GO"

      end subroutine

      subroutine removeAllSurfaceData(self, surfIdx)
        use DATLEN, only: ALENS, LSIZ
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(1:LSIZ, surfIdx) = 0.0D0
      end subroutine

      subroutine clearClearApertureData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(51:57, surfIdx) = 0.0D0
      end subroutine

      subroutine clearObscurationData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(61:67, surfIdx) = 0.0D0
      end subroutine

      subroutine clearGrtInitData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(97:99, surfIdx) = 0.0D0
      end subroutine

      subroutine clearGratingData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(96:101, surfIdx) = 0.0D0
      end subroutine

      subroutine clearToricData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(23:24, surfIdx) = 0.0D0
        ALENS(36:41, surfIdx) = 0.0D0
      end subroutine

      subroutine clearTiltAngles(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(25:28, surfIdx) = 0.0D0
      end subroutine

      subroutine clearTiltDegAngles(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(118:120, surfIdx) = 0.0D0
      end subroutine

      subroutine clearGlobalCoordData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(90:95, surfIdx) = 0.0D0
      end subroutine

      subroutine clearDecentData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(29:31, surfIdx) = 0.0D0
      end subroutine

      subroutine clearPivotAndFocusData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(113:116, surfIdx) = 0.0D0
      end subroutine

      subroutine initRefractiveIndices(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(46:50, surfIdx) = 1.0D0
        ALENS(71:75, surfIdx) = 1.0D0
      end subroutine

      subroutine clearCurvAndConic(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(1:2, surfIdx) = 0.0D0
      end subroutine

      subroutine clearAsphericCoeffs(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(4:7, surfIdx) = 0.0D0
      end subroutine

      subroutine clearAsphericData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(4:9, surfIdx) = 0.0D0
      end subroutine

      subroutine clearClearApertureParams(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(10:15, surfIdx) = 0.0D0
      end subroutine

      subroutine clearObscurationShape(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(16:22, surfIdx) = 0.0D0
      end subroutine

      subroutine clearObscurationShapeParams(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(19:22, surfIdx) = 0.0D0
      end subroutine

      subroutine clearToricFlagAndCurv(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(23:24, surfIdx) = 0.0D0
      end subroutine

      subroutine clearTiltAlphaBetaGamma(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(26:28, surfIdx) = 0.0D0
      end subroutine

      subroutine clearTiltAnglesAndDecentValues(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(26:31, surfIdx) = 0.0D0
      end subroutine

      subroutine clearTiltAndDecentData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(25:31, surfIdx) = 0.0D0
      end subroutine

      subroutine clearDecentFlagAndY(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(29:30, surfIdx) = 0.0D0
      end subroutine

      subroutine clearDecentValues(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(30:31, surfIdx) = 0.0D0
      end subroutine

      subroutine clearDecentAnamAndConfig(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(30:43, surfIdx) = 0.0D0
      end subroutine

      subroutine clearXDecentAnamAndConfig(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(31:43, surfIdx) = 0.0D0
      end subroutine

      subroutine clearSolvesAndSurfType(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(33:34, surfIdx) = 0.0D0
      end subroutine

      subroutine clearAnamorphicCoeffs(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(36:40, surfIdx) = 0.0D0
      end subroutine

      subroutine clearApertureRegion(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(51:70, surfIdx) = 0.0D0
      end subroutine

      subroutine clearPivotData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(77:80, surfIdx) = 0.0D0
      end subroutine

      subroutine clearPivotXY(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(78:79, surfIdx) = 0.0D0
      end subroutine

      subroutine clearPivotAndHigherAspherics(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(76:85, surfIdx) = 0.0D0
      end subroutine

      subroutine clearHigherOrderAsphericCoeffs(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(81:85, surfIdx) = 0.0D0
      end subroutine

      subroutine clearDeformableData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(103:106, surfIdx) = 0.0D0
      end subroutine

      subroutine clearFocusData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(114:116, surfIdx) = 0.0D0
      end subroutine

      subroutine clearFocusAndTiltDegData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(114:120, surfIdx) = 0.0D0
      end subroutine

      subroutine clearGlassColorData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(122:123, surfIdx) = 0.0D0
      end subroutine

      subroutine clearMultiApertureData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(127:128, surfIdx) = 0.0D0
      end subroutine

      subroutine clearArrayData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(131:133, surfIdx) = 0.0D0
      end subroutine

      subroutine clearSpiderData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(134:137, surfIdx) = 0.0D0
      end subroutine

      subroutine clearExtendedSurfaceData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(77:110, surfIdx) = 0.0D0
      end subroutine

      subroutine clearAsphericCoeffsAndFlag(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(4:8, surfIdx) = 0.0D0
      end subroutine

      subroutine clearAnamorphicData(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(36:41, surfIdx) = 0.0D0
      end subroutine

      subroutine clearApertureTypeAndParams(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(9:14, surfIdx) = 0.0D0
      end subroutine

      subroutine clearApertureTypeAndAllParams(self, surfIdx)
        use DATLEN, only: ALENS
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: surfIdx
        ALENS(9:15, surfIdx) = 0.0D0
      end subroutine


end module