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
    use mod_surface_type, only: surface_type, sphere_surface, asphere_surface, surf_slot

    implicit none

    type lens_data_manager

    ! Per-surface variable codes, column = VAR_* code (see zoa-ui.f90):
    ! 1=CCY curvature, 2=THC thickness, 3=KC conic, 4..12=AC..IC asphere coeffs
    ! A4..A20.  Value 0 => variable, 100 => frozen (default).
    ! Hard code max of 500 surfaces.
    real(long), dimension(0:499,NUM_VAR_CODES) :: vars

    ! Persistent per-surface edge (physical) semi-aperture in Y.  0 => unset.
    ! Owned here (NOT ALENS) so it survives load_surfaces_from_alens() rebuilds;
    ! copied into surfaces(s)%s%clap%semi_edge_y after each rebuild.  Set via CIR EDG.
    real(real64), dimension(0:499) :: edge_semi_y = 0.0_real64

    ! Array of surface type objects indexed 0:last_surf.
    ! Each slot holds an allocatable CLASS(surface_type) so surfaces can have
    ! different dynamic types (sphere_surface, asphere_surface, etc.).
    ! Populated by load_surfaces_from_alens(); accessed as surfaces(i)%s
    type(surf_slot), allocatable :: surfaces(:)

    contains
     procedure :: initialize => init_ldm
     procedure, public, pass(self) :: getSurfThi, setSurfThi
     procedure, public, pass(self) :: getSurfXDec, getSurfYDec
     procedure, public, pass(self) :: getSurfAutoSemiX, getSurfAutoSemiY
     procedure, public, pass(self) :: getEdgeSemiAperture, setEdgeSemiAperture
     procedure, public, pass(self) :: getEdgeApertureScale, clearEdgeApertures
     procedure, public, pass(self) :: deleteAllApertures
     procedure, public, pass(self) :: getClearApertureForLensDraw
     procedure, public, pass(self) :: getSurfIdealEFL, getSurfSpecialType
     procedure, public, pass(self) :: isThiSolveOnSurf
     procedure, public, pass(self) :: isPIMSolveOnSurf
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
     procedure :: isSolveOnSurf, isPikupOnSurf, isPikupOnSurfJ, isVarOnSurf
     procedure :: getCCYCodeAsStr, getTHCCodeAsStr
     procedure :: getSurfacePointer, incrementSurfacePointer, setSurfacePointer
     procedure, public, pass(self) :: genSaveOutputText => genLDMSaveOutputText
     procedure :: outputPikupText, genSurfPikupSavText, getSurfTypeName, getExtraParamCmd
     procedure :: getExtraParamKdpCmd, getExtraParamPikupQual, getExtraParamPikupIdx
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
     procedure, public, pass(self) :: load_surfaces_from_alens
     procedure, public, pass(self) :: refresh_typed_surf_geom
     procedure, public, pass(self) :: setSurfaceType

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

        ! Prefer the typed surface's name if available; fall back to ALENS flag.
        if (allocated(self%surfaces)) then
          if (idx >= lbound(self%surfaces,1) .and. idx <= ubound(self%surfaces,1)) then
            if (allocated(self%surfaces(idx)%s)) then
              strName = trim(self%surfaces(idx)%s%type_name)
              return
            end if
          end if
        end if
        strName = 'Sphere'
        if (surf_is_asphere(idx)) strName = 'Asphere'

    end function

    function getExtraParamCmd(self, surfIdx, colIdx) result(cmd)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx, colIdx
        character(len=4) :: cmd

        cmd = ' '
        ! Read from typed surface if available.
        if (allocated(self%surfaces)) then
          if (surfIdx >= lbound(self%surfaces,1) .and. surfIdx <= ubound(self%surfaces,1)) then
            if (allocated(self%surfaces(surfIdx)%s)) then
              if (colIdx >= 1 .and. colIdx <= self%surfaces(surfIdx)%s%num_params) then
                cmd = trim(self%surfaces(surfIdx)%s%param_cmds(colIdx))
              end if
              return
            end if
          end if
        end if
        ! Fallback: hardcoded asphere table.
        if (trim(self%getSurfTypeName(surfIdx)) == 'Asphere') then
          block
            character(len=4), dimension(10) :: t = ['K   ','A   ','B   ','C   ','D   ', &
                                                     'E   ','F   ','G   ','H   ','I   ']
            if (colIdx >= 1 .and. colIdx <= size(t)) cmd = t(colIdx)
          end block
        end if

    end function

    ! KDP set-command for extra param colIdx on surfIdx (e.g. CCK, AD..AL).
    ! Blank => the surface type defines no such param / no KDP command.
    function getExtraParamKdpCmd(self, surfIdx, colIdx) result(cmd)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx, colIdx
        character(len=4) :: cmd

        cmd = ' '
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        if (colIdx >= 1 .and. colIdx <= self%surfaces(surfIdx)%s%num_params) &
            cmd = trim(self%surfaces(surfIdx)%s%param_cmds_kdp(colIdx))
    end function

    ! PIKUP/PIKD qualifier word for extra param colIdx (e.g. CC, AD..AL).
    function getExtraParamPikupQual(self, surfIdx, colIdx) result(qual)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx, colIdx
        character(len=4) :: qual

        qual = ' '
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        if (colIdx >= 1 .and. colIdx <= self%surfaces(surfIdx)%s%num_params) &
            qual = trim(self%surfaces(surfIdx)%s%param_pikup_qual(colIdx))
    end function

    ! PIKUP array J index for extra param colIdx (0 => no pickup support).
    function getExtraParamPikupIdx(self, surfIdx, colIdx) result(jIdx)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx, colIdx
        integer :: jIdx

        jIdx = 0
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        if (colIdx >= 1 .and. colIdx <= self%surfaces(surfIdx)%s%num_params) &
            jIdx = self%surfaces(surfIdx)%s%param_pikup_idx(colIdx)
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

    subroutine setSurfacePointer(self, idx)
        use DATLEN, only: SURF
        implicit none
        class(lens_data_manager) :: self
        integer, intent(in) :: idx

        SURF = idx

    end subroutine

    ! Ray-traced (display-only) clear-aperture semi-extent for a surface, filled by
    ! check_clear_apertures.  Returns 0 if not computed / surface out of range.
    function getSurfAutoSemiY(self, surfIdx) result(semi)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx
        real(kind=real64) :: semi
        semi = 0.0_real64
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        semi = self%surfaces(surfIdx)%s%clap%auto_semi_y
    end function

    ! Raw per-surface edge semi-aperture (Y).  0 => unset (caller applies the
    ! system default edge scale factor).  Reads the typed clear_aperture.
    function getEdgeSemiAperture(self, surfIdx) result(semi)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx
        real(kind=real64) :: semi
        semi = 0.0_real64
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        semi = self%surfaces(surfIdx)%s%clap%semi_edge_y
    end function

    subroutine setEdgeSemiAperture(self, surfIdx, val)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx
        real(kind=real64), intent(in) :: val
        if (surfIdx < 0 .or. surfIdx > ubound(self%edge_semi_y,1)) return
        ! Persistent owner (survives load_surfaces_from_alens rebuilds)...
        self%edge_semi_y(surfIdx) = val
        ! ...and mirror onto the live typed surface if present.
        if (allocated(self%surfaces)) then
          if (surfIdx >= lbound(self%surfaces,1) .and. surfIdx <= ubound(self%surfaces,1)) then
            if (allocated(self%surfaces(surfIdx)%s)) &
              self%surfaces(surfIdx)%s%clap%semi_edge_y = val
          end if
        end if
    end subroutine

    ! Per-surface edge-aperture scale factor for lens drawing.  If the surface has
    ! an explicit edge semi-aperture, convert it to a factor relative to the clear
    ! (clipping) aperture; otherwise use the supplied system default factor.
    function getEdgeApertureScale(self, surfIdx, defaultFactor) result(sfi)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx
        real(kind=real64), intent(in) :: defaultFactor
        real(kind=real64) :: sfi, edgeSemi, clipSemi
        sfi = defaultFactor
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        edgeSemi = self%surfaces(surfIdx)%s%clap%semi_edge_y
        if (edgeSemi <= 0.0_real64) return                 ! unset -> default factor
        clipSemi = self%surfaces(surfIdx)%s%clap%display_semi_y()
        if (clipSemi <= 0.0_real64) return                 ! no clip to scale from
        sfi = edgeSemi / clipSemi
    end function

    ! Semi-aperture to DRAW for a surface in the lens layout (typed replacement
    ! for the raw ALENS(10)/ALENS(11) reads in the drawing routines).
    !   axis = 1 -> Y semi (clap dim1),  axis = 2 -> X semi (clap dim2)
    ! Returns the physical (edge) size: the clear aperture scaled by the surface's
    ! edge factor -- an explicit CIR EDG value if set, else defaultFactor.  The Y
    ! axis therefore returns the explicit edge value exactly when one is set.
    function getClearApertureForLensDraw(self, surfIdx, axis, defaultFactor) result(semi)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx, axis
        real(kind=real64), intent(in) :: defaultFactor
        real(kind=real64) :: semi, clipAxis, clipY, edgeY, factor
        semi = 0.0_real64
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        clipY = self%surfaces(surfIdx)%s%clap%dim1      ! ALENS(10): Y semi
        edgeY = self%surfaces(surfIdx)%s%clap%semi_edge_y
        if (edgeY > 0.0_real64 .and. clipY > 0.0_real64) then
          factor = edgeY / clipY                        ! explicit edge -> exact Y
        else
          factor = defaultFactor
        end if
        if (axis == 1) then
          clipAxis = clipY
        else
          clipAxis = self%surfaces(surfIdx)%s%clap%dim2  ! ALENS(11): X semi
        end if
        semi = clipAxis * factor
    end function

    ! Reset all per-surface edge apertures.  Called wherever the lens is replaced
    ! (see the unified-reset TODO in test/KNOWN_ISSUES.md).
    subroutine clearEdgeApertures(self)
        class(lens_data_manager) :: self
        integer :: s
        self%edge_semi_y = 0.0_real64
        if (allocated(self%surfaces)) then
          do s = lbound(self%surfaces,1), ubound(self%surfaces,1)
            if (allocated(self%surfaces(s)%s)) &
              self%surfaces(s)%s%clap%semi_edge_y = 0.0_real64
          end do
        end if
    end subroutine

    ! Delete every clear aperture AND edge aperture on all surfaces (DEL APE SA).
    ! Mirrors the per-surface CLAPD delete (ldm8.f90) across all surfaces.
    subroutine deleteAllApertures(self)
        use mod_surface, only: set_surf_clap_type, set_surf_multi_clap_flag
        class(lens_data_manager) :: self
        integer :: s
        do s = 0, self%getLastSurf()
            call set_surf_clap_type(s, 0)
            call set_surf_multi_clap_flag(s, 0)
            call self%clearClearApertureParams(s)
            call self%clearClearApertureData(s)
        end do
        call self%clearEdgeApertures()
    end subroutine

    function getSurfAutoSemiX(self, surfIdx) result(semi)
        class(lens_data_manager) :: self
        integer, intent(in) :: surfIdx
        real(kind=real64) :: semi
        semi = 0.0_real64
        if (.not. allocated(self%surfaces)) return
        if (surfIdx < lbound(self%surfaces,1) .or. surfIdx > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(surfIdx)%s)) return
        semi = self%surfaces(surfIdx)%s%clap%auto_semi_x
    end function

    function getSurfThi(self, surfIdx) result(thi)
        use mod_surface, only: surf_thickness
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: thi

        thi = surf_thickness(surfIdx)
        !thi = curr_lens_data%thicknesses(surfIdx+1)

    end function

    function getSurfXDec(self, surfIdx) result(xdec)
        use mod_surface, only: surf_decenter_x
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: xdec
        xdec = surf_decenter_x(surfIdx)
    end function

    function getSurfYDec(self, surfIdx) result(ydec)
        use mod_surface, only: surf_decenter_y
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: ydec
        ydec = surf_decenter_y(surfIdx)
    end function

    function getSurfIdealEFL(self, surfIdx) result(efl)
        use mod_surface, only: surf_ideal_efl
        class(lens_data_manager) :: self
        integer :: surfIdx
        real(kind=real64) :: efl
        efl = surf_ideal_efl(surfIdx)
    end function

    function getSurfSpecialType(self, surfIdx) result(stype)
        use mod_surface, only: surf_special_type
        class(lens_data_manager) :: self
        integer :: surfIdx
        integer :: stype
        stype = surf_special_type(surfIdx)
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

    function isPIMSolveOnSurf(self, surfIdx) result(boolResult)
        use DATLEN, only: SOLVE
        class(lens_data_manager) :: self
        integer :: surfIdx
        logical :: boolResult

        boolResult = (SOLVE(6,surfIdx) == 1.0D0)

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
        case('AC')
            VAR_CODE = VAR_A4
        case('BC')
            VAR_CODE = VAR_A6
        case('CC')
            VAR_CODE = VAR_A8
        case('DC')
            VAR_CODE = VAR_A10
        case('EC')
            VAR_CODE = VAR_A12
        case('FC')
            VAR_CODE = VAR_A14
        case('GC')
            VAR_CODE = VAR_A16
        case('HC')
            VAR_CODE = VAR_A18
        case('IC')
            VAR_CODE = VAR_A20
        case default
            return
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

    ! PIKUP array J index for a VAR_* code (0 => no pickup type for that code).
    ! J values from the PIKUPS2 qualifier map: RD=1, TH=3, CC=4, AD..AG=5..8,
    ! AH..AL=27..31.
    function varCodeToPikupJ(var_code) result(jIdx)
        integer, intent(in) :: var_code
        integer :: jIdx
        integer, parameter :: asphJ(9) = [5, 6, 7, 8, 27, 28, 29, 30, 31]

        select case (var_code)
        case (VAR_CURV)
            jIdx = ID_PICKUP_RAD
        case (VAR_THI)
            jIdx = ID_PICKUP_THIC
        case (VAR_K)
            jIdx = 4
        case (VAR_A4:VAR_A20)
            jIdx = asphJ(var_code - VAR_A4 + 1)
        case default
            jIdx = 0
        end select
    end function

    ! Direct PIKUP-existence check by PIKUP array J index.
    function isPikupOnSurfJ(self, sur, jIdx) result(boolResult)
        use DATLEN, only: PIKUP
        implicit none
        class(lens_data_manager) :: self
        integer, intent(in) :: sur, jIdx
        logical :: boolResult

        boolResult = .FALSE.
        if (jIdx >= 1 .and. jIdx <= 45) then
            if (PIKUP(1,sur,jIdx) == 1.0) boolResult = .TRUE.
        end if
    end function

    ! TODO:  Refactor with solve?
    function isPikupOnSurf(self, sur, var_code) result(boolResult)
        use DATLEN
        implicit none
        class(lens_data_manager) :: self
        integer, intent(in) :: sur, var_code
        logical :: boolResult

        boolResult = self%isPikupOnSurfJ(sur, varCodeToPikupJ(var_code))

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
        ! PIKUP array J indexes and the CLI parameter name each saves as.
        ! RD/TH plus conic (CC) and the asphere coefficients AD..AL.
        integer,          parameter :: pickupJ(12)    = [1, 3, 4, 5, 6, 7, 8, 27, 28, 29, 30, 31]
        character(len=3), parameter :: pickupCli(12)  = ['RDY', 'THI', 'K  ', 'A  ', 'B  ', 'C  ', &
                                                       & 'D  ', 'E  ', 'F  ', 'G  ', 'H  ', 'I  ']

        do ii=0,self%getLastSurf()
            do jj=1,size(pickupJ)
                outTxt = self%genSurfPikupSavText(ii, pickupJ(jj), pickupCli(jj))
                if (len(trim(outTxt)) > 0) then
                    write(fID, *) trim(outTxt)
                end if
            end do
        end do


    end subroutine

    function genSurfPikupSavText(self, surf, jIdx, cliName) result(outTxt)
        use DATLEN, only: PIKUP
        use type_utils
        class(lens_data_manager) :: self
        integer :: surf, jIdx, si, sf
        character(len=*) :: cliName
        character(len=512) :: outTxt, surfTxt
        real(kind=long) :: scale, offset

        outTxt = ' '
        if (self%isPikupOnSurfJ(surf, jIdx)) then
            si = INT(PIKUP(2,surf,jIdx)) ! Start Surface
            sf = si ! Temp
            !sf = INT(PIKUP(3,surf,jIdx)) ! End Surface
            scale = PIKUP(3,surf, jIdx) ! Default 1
            offset = PIKUP(4,surf,jIdx) ! Default 0

            if (si==sf) then
                surfTxt = 'S'//trim(int2str(si))
            else
                surfTxt = 'S'//trim(int2str(si))//'..'//trim(int2str(sf))
            end if

            outTxt = 'PIK '//trim(cliName)//' S'//trim(int2str(surf))//' '//trim(cliName)//' '// &
            & trim(surfTxt)//' '//trim(real2str(scale,4))//' '//trim(real2str(offset,4))

        end if

    end function


    subroutine genLDMSaveOutputText(self, fID, skip_alens_refresh)
        use type_utils, only: real2str, blankStr, int2str
        use zoa_file_handler, only: genOutputLineWithSpacing
        use mod_surface, only: surf_curvature, surf_radius, surf_thickness, surf_conic
        class(lens_data_manager) :: self
        integer :: fID
        ! skip_alens_refresh: when .TRUE., skip the load_surfaces_from_alens() call.
        ! Used by undo_manager's write_snapshot to avoid reallocating ldm%surfaces
        ! mid-execution, which can cause check_clear_apertures (called by the next EOS)
        ! to run with freshly-zeroed surfaces and produce a corrupted PIM solve.
        logical, optional, intent(in) :: skip_alens_refresh
        integer :: ii, jj
        character(len=1024) :: strSurfLine, strTHI, strRdy
        character(len=4) :: surfStr
        !character(len=80) :: glassStr
        logical :: rdmFlag
        logical :: skipRefresh

        rdmFlag = .TRUE.

        ! Refresh the typed surfaces from current ALENS so the clap data read below
        ! reflects any CIR/CLAP commands issued since the last lens load.
        ! Skipped when called from write_snapshot (skip_alens_refresh=.TRUE.) to
        ! prevent mid-execution ALENS corruption via check_clear_apertures.
        ! NOTE: Fortran .and. does not short-circuit, so an absent optional must
        ! not appear in the same expression as its present() check.
        skipRefresh = .FALSE.
        if (present(skip_alens_refresh)) skipRefresh = skip_alens_refresh
        if (.not. skipRefresh) then
            call self%load_surfaces_from_alens()
        end if

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
          
          ! Save an assigned clear aperture from the typed clap (circular for now;
          ! shape>=2 geometries are a TODO). Only ALENS-backed apertures (is_set)
          ! are persisted; ray-traced auto extents are not real apertures.
          if (allocated(ldm%surfaces)) then
            if (ii-1 <= ubound(ldm%surfaces,1)) then
              if (allocated(ldm%surfaces(ii-1)%s)) then
                if (ldm%surfaces(ii-1)%s%clap%is_set()) then
                  strSurfLine = blankStr(2)//'CIR '// &
                  & trim(real2str(ldm%surfaces(ii-1)%s%clap%dim1, 10))
                  write(fID, *) trim(strSurfLine)
                end if
                ! Explicit edge (physical) aperture, if set.  Emitted without a
                ! surface qualifier so it attaches to the current surface being
                ! built (the surface pointer), like the CIR clear aperture above.
                if (ldm%surfaces(ii-1)%s%clap%semi_edge_y /= 0.0_real64) then
                  strSurfLine = blankStr(2)//'CIR EDG '// &
                  & trim(real2str(ldm%surfaces(ii-1)%s%clap%semi_edge_y, 10))
                  write(fID, *) trim(strSurfLine)
                end if
              end if
            end if
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
        if (allocated(ldm%surfaces)) then
          if (curr_lens_data%num_surfaces-1 <= ubound(ldm%surfaces,1)) then
            if (allocated(ldm%surfaces(curr_lens_data%num_surfaces-1)%s)) then
              if (ldm%surfaces(curr_lens_data%num_surfaces-1)%s%clap%is_set()) then
                ! NOTE: previously wrote the surface COUNT here (a bug); now writes
                ! the image-surface aperture radius from the typed clap.
                strSurfLine = blankStr(2)//'CIR '// &
                & trim(real2str(ldm%surfaces(curr_lens_data%num_surfaces-1)%s%clap%dim1, 10))
                write(fID, *) trim(strSurfLine)
              end if
            end if
          end if
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

      ! Build self%surfaces(0:last_surf) from current ALENS data.
      ! Called after every lens load so typed objects reflect the live system.
      subroutine load_surfaces_from_alens(self)
        use mod_surface_type, only: make_sphere, make_asphere
        use mod_surface, only: surf_radius, surf_is_asphere, surf_conic, &
                               surf_thickness, surf_asphere_coeff, &
                               surf_refractive_index, surf_curvature
        use DATLEN, only: GLANAM
        class(lens_data_manager), intent(inout) :: self
        integer :: s, last, w
        real(real64) :: coeffs(10)

        last = self%getLastSurf()
        if (allocated(self%surfaces)) deallocate(self%surfaces)
        allocate(self%surfaces(0:last))

        do s = 0, last
          if (surf_is_asphere(s)) then
            coeffs(1)  = surf_asphere_coeff(s, 4)
            coeffs(2)  = surf_asphere_coeff(s, 6)
            coeffs(3)  = surf_asphere_coeff(s, 8)
            coeffs(4)  = surf_asphere_coeff(s, 10)
            coeffs(5)  = surf_asphere_coeff(s, 12)
            coeffs(6)  = surf_asphere_coeff(s, 14)
            coeffs(7)  = surf_asphere_coeff(s, 16)
            coeffs(8)  = surf_asphere_coeff(s, 18)
            coeffs(9)  = surf_asphere_coeff(s, 20)
            coeffs(10) = surf_asphere_coeff(s, 2)
            allocate(asphere_surface :: self%surfaces(s)%s)
            select type(item => self%surfaces(s)%s)
            type is (asphere_surface)
              item = make_asphere(surf_radius(s), surf_thickness(s), surf_conic(s), &
                                  trim(GLANAM(s,2)), trim(GLANAM(s,1)), coeffs)
            end select
          else
            allocate(sphere_surface :: self%surfaces(s)%s)
            select type(item => self%surfaces(s)%s)
            type is (sphere_surface)
              item = make_sphere(surf_radius(s), surf_thickness(s), surf_conic(s), &
                                 trim(GLANAM(s,2)), trim(GLANAM(s,1)))
            end select
          end if

          ! Authoritative curvature straight from ALENS (exact), overwriting the
          ! 1/radius the factory derived -- avoids the 1/(1/curvature) round-trip.
          self%surfaces(s)%s%cv = surf_curvature(s)

          do w = 1, 10
            self%surfaces(s)%s%n_post(w) = surf_refractive_index(s, w)
            if (s > 0) self%surfaces(s)%s%n_pre(w) = surf_refractive_index(s-1, w)
          end do
          call self%surfaces(s)%s%clap%from_alens(s)
          ! Edge aperture is not an ALENS quantity; restore it from the ldm's
          ! persistent store so a rebuild does not drop a CIR EDG setting.
          self%surfaces(s)%s%clap%semi_edge_y = self%edge_semi_y(s)
        end do
      end subroutine load_surfaces_from_alens

      ! Refresh only the geometry (cv/thickness/conic) of ONE existing typed
      ! surface slot from live ALENS -- no reallocation, no topology change.
      ! The typed store is a frozen copy that load_surfaces_from_alens rebuilds
      ! only on load/editor/replot; an in-place edit (RDY/CUY) or a mid-trace
      ! pickup/solve updates ALENS but not this copy, so the paraxial/real trace
      ! (which refracts through surfaces(k)%s%cv) goes stale.  This copies the
      ! same surf_curvature/surf_thickness/surf_conic values the full rebuild
      ! uses, so for an unchanged surface it reproduces the identical bits; for a
      ! just-edited/just-resolved surface it brings the frozen copy current.
      ! Cheap enough to call per surface at the exact point geometry changes.
      subroutine refresh_typed_surf_geom(self, s)
        use mod_surface, only: surf_thickness, surf_conic, surf_curvature
        class(lens_data_manager), intent(inout) :: self
        integer, intent(in) :: s

        if (.not. allocated(self%surfaces)) return
        if (s < lbound(self%surfaces,1) .or. s > ubound(self%surfaces,1)) return
        if (.not. allocated(self%surfaces(s)%s)) return

        self%surfaces(s)%s%cv        = surf_curvature(s)   ! exact, authoritative
        self%surfaces(s)%s%thickness = surf_thickness(s)
        self%surfaces(s)%s%conic     = surf_conic(s)
      end subroutine refresh_typed_surf_geom


      ! Change surface s to asphere type (sets flag in ALENS, rebuilds surfaces(:)).
      ! Set surface s to the named type. No-op if already that type.
      ! surStr: 'ASP' = asphere, 'SPH' = sphere (extensible for future types).
      subroutine setSurfaceType(self, s, surStr)
        use mod_surface, only: set_surf_asphere_flag, surf_is_asphere, set_surf_conic
        class(lens_data_manager), intent(inout) :: self
        integer,          intent(in) :: s
        character(len=*), intent(in) :: surStr

        select case (trim(surStr))
        case ('ASP')
          if (surf_is_asphere(s)) return
          call set_surf_asphere_flag(s, .true.)
        case ('SPH')
          if (.not. surf_is_asphere(s)) return
          call set_surf_asphere_flag(s, .false.)
          call set_surf_conic(s, 0.0_real64)
        case default
          return
        end select

        call self%load_surfaces_from_alens()
      end subroutine setSurfaceType


end module