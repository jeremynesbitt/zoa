module codeV_legacy_wrappers
    implicit none

contains

    ! =========================================================================
    ! Wrapper subroutines for simple CMDER.FOR commands
    ! Each wraps a legacy subroutine to match the cmdImplementation interface
    ! =========================================================================

    subroutine wrap_ABSORB(iptStr)
        character(len=*) :: iptStr
        call ABSORB
    end subroutine

    subroutine wrap_ECHO(iptStr)
        character(len=*) :: iptStr
        call ECHO
    end subroutine

    subroutine wrap_CV2PRG(iptStr)
        character(len=*) :: iptStr
        call CV2PRG
        ! execSAV call temporarily removed for debugging
        !call execSAV('SAV')
    end subroutine

    subroutine wrap_ZMX2PRG(iptStr)
        character(len=*) :: iptStr
        call ZMX2PRG
    end subroutine

    subroutine wrap_CF(iptStr)
        character(len=*) :: iptStr
        call CFGPRT
    end subroutine

    subroutine wrap_GCONVERT(iptStr)
        character(len=*) :: iptStr
        call GCONVERT
    end subroutine

    subroutine wrap_INI(iptStr)
        character(len=*) :: iptStr
        call SINI
    end subroutine

    subroutine wrap_LTYPE(iptStr)
        character(len=*) :: iptStr
        call SLTYPE
    end subroutine

    subroutine wrap_WV(iptStr)
        character(len=*) :: iptStr
        call SWV
    end subroutine

    subroutine wrap_UNITS(iptStr)
        character(len=*) :: iptStr
        call SUNITS
    end subroutine

    subroutine wrap_TPLATE(iptStr)
        character(len=*) :: iptStr
        call TSTPLATE
    end subroutine

    subroutine wrap_ASTOP(iptStr)
        character(len=*) :: iptStr
        call SASTOP
    end subroutine

    subroutine wrap_ZERNREPT(iptStr)
        character(len=*) :: iptStr
        call ZERNREPT
    end subroutine

    subroutine wrap_MODE(iptStr)
        character(len=*) :: iptStr
        call SMODE
    end subroutine

    subroutine wrap_FINDGLAS(iptStr)
        character(len=*) :: iptStr
        call FNDGLS
    end subroutine

    subroutine wrap_COLORSET(iptStr)
        character(len=*) :: iptStr
        call COLORS
    end subroutine

    subroutine wrap_GREYSPOT(iptStr)
        character(len=*) :: iptStr
        call GREYSPOT
    end subroutine

    subroutine wrap_REFK(iptStr)
        character(len=*) :: iptStr
        call SREF
    end subroutine

    subroutine wrap_PIVAXIS(iptStr)
        character(len=*) :: iptStr
        call PIVAXOUT
    end subroutine

    subroutine wrap_DISP(iptStr)
        character(len=*) :: iptStr
        call HEXDISP
    end subroutine

    subroutine wrap_STILT(iptStr)
        character(len=*) :: iptStr
        call HEXSTILT
    end subroutine

    subroutine wrap_BTILT(iptStr)
        character(len=*) :: iptStr
        call HEXBTILT
    end subroutine

    subroutine wrap_ROLL(iptStr)
        character(len=*) :: iptStr
        call HEXROLL
    end subroutine

    subroutine wrap_FLIP(iptStr)
        character(len=*) :: iptStr
        call CVFLIP
    end subroutine

    subroutine wrap_SPC(iptStr)
        character(len=*) :: iptStr
        call SSPC
    end subroutine

    subroutine wrap_INVAR(iptStr)
        character(len=*) :: iptStr
        call INVAR
    end subroutine

    subroutine wrap_CHRSHIFT(iptStr)
        character(len=*) :: iptStr
        call CHRSHIFT
    end subroutine

    subroutine wrap_FIRD(iptStr)
        character(len=*) :: iptStr
        call FIRD
    end subroutine

    subroutine wrap_OBJLEV(iptStr)
        character(len=*) :: iptStr
        call OBJLEV
    end subroutine

    subroutine wrap_FIGURE(iptStr)
        character(len=*) :: iptStr
        call FIGURE
    end subroutine

    subroutine wrap_INCR(iptStr)
        character(len=*) :: iptStr
        call INCR
    end subroutine

    subroutine wrap_FOBDUMP(iptStr)
        character(len=*) :: iptStr
        call FOBDMP
    end subroutine

    subroutine wrap_OPD(iptStr)
        character(len=*) :: iptStr
        call PROPD
    end subroutine

    subroutine wrap_AUTO(iptStr)
        character(len=*) :: iptStr
        call AUTO
    end subroutine

    subroutine wrap_HEADINGS(iptStr)
        character(len=*) :: iptStr
        call SETHED
    end subroutine

    subroutine wrap_DXF(iptStr)
        character(len=*) :: iptStr
        call DDXFF
    end subroutine

    subroutine wrap_FANS(iptStr)
        use GLOBALS, only: HEADLESS_MODE
        use zoa_output, only: zoa_emit
        character(len=*) :: iptStr
        if (HEADLESS_MODE) then
            call zoa_emit("FANS requires GUI", "red")
            return
        end if
        call RIMS
    end subroutine

    subroutine wrap_VIEOFF(iptStr)
        character(len=*) :: iptStr
        call VIEOFF
    end subroutine

    subroutine wrap_SHOWNSS(iptStr)
        character(len=*) :: iptStr
        call SHOWNSS
    end subroutine

    subroutine wrap_SPDSSI(iptStr)
        character(len=*) :: iptStr
        call SPDSSI
    end subroutine

    subroutine wrap_DET(iptStr)
        character(len=*) :: iptStr
        call DETECTOR
    end subroutine

! wrap_FANFIELD removed (FANFOV was in PLOTCAD8_fan, now deleted)
! wrap_GRAOUT removed (GRAOUT plot-to-file command retired)

    subroutine wrap_GRID(iptStr)
        character(len=*) :: iptStr
        call MTFGRID
    end subroutine

    subroutine wrap_SPACE(iptStr)
        character(len=*) :: iptStr
        call SPACER
    end subroutine

    subroutine wrap_CUTOFF(iptStr)
        character(len=*) :: iptStr
        call CUTOFF
    end subroutine

    subroutine wrap_WAMAP(iptStr)
        character(len=*) :: iptStr
        call WAMAP
    end subroutine

    subroutine wrap_AMAP(iptStr)
        character(len=*) :: iptStr
        call AMAP
    end subroutine

    subroutine wrap_RAYLEIGH(iptStr)
        character(len=*) :: iptStr
        call RAYLEIGH
    end subroutine

    subroutine wrap_WEIGHT(iptStr)
        character(len=*) :: iptStr
        call WEIGHT
    end subroutine

    subroutine wrap_COST(iptStr)
        character(len=*) :: iptStr
        call COST
    end subroutine

    subroutine wrap_DEFORM(iptStr)
        character(len=*) :: iptStr
        call DEFIT
    end subroutine

    subroutine wrap_OUTFLAT(iptStr)
        character(len=*) :: iptStr
        call OUTFLT
    end subroutine

    subroutine wrap_EXPUP(iptStr)
        character(len=*) :: iptStr
        call EXPUP
    end subroutine

    subroutine wrap_RSPH(iptStr)
        character(len=*) :: iptStr
        call RSPH
    end subroutine

    subroutine wrap_PRINT(iptStr)
        character(len=*) :: iptStr
        call PRNLP
    end subroutine

    subroutine wrap_FITZERN(iptStr)
        character(len=*) :: iptStr
        call OPDLOD
    end subroutine

    subroutine wrap_LISTOPD(iptStr)
        character(len=*) :: iptStr
        call OPDLIS
    end subroutine

    subroutine wrap_LISTZERN(iptStr)
        character(len=*) :: iptStr
        call WRTCOEFS
    end subroutine

    subroutine wrap_LISTREPT(iptStr)
        character(len=*) :: iptStr
        call WRTREPORT
    end subroutine

    subroutine wrap_OIF(iptStr)
        character(len=*) :: iptStr
        call OIF
    end subroutine

    subroutine wrap_XXF(iptStr)
        character(len=*) :: iptStr
        call XXF
    end subroutine

    subroutine wrap_XXFF(iptStr)
        character(len=*) :: iptStr
        call XXFF
    end subroutine

    subroutine wrap_IMAGEDIR(iptStr)
        character(len=*) :: iptStr
        call IMAGEDIR
    end subroutine

    subroutine wrap_CAPFNOUT(iptStr)
        character(len=*) :: iptStr
        call OPDOUT
    end subroutine

    subroutine wrap_CAPGRID(iptStr)
        character(len=*) :: iptStr
        call CAPGRID
    end subroutine

    subroutine wrap_FUNNAME(iptStr)
        character(len=*) :: iptStr
        call FUNNAME
    end subroutine

    subroutine wrap_GLASSWV(iptStr)
        character(len=*) :: iptStr
        call GLSWVL
    end subroutine

    subroutine wrap_DO(iptStr)
        character(len=*) :: iptStr
        call DODODO
    end subroutine

    subroutine wrap_PRES(iptStr)
        character(len=*) :: iptStr
        call PRES
    end subroutine

    subroutine wrap_STATS(iptStr)
        character(len=*) :: iptStr
        call STATT
    end subroutine

    subroutine wrap_SPGR(iptStr)
        character(len=*) :: iptStr
        call SPGR
    end subroutine

    subroutine wrap_PRICE(iptStr)
        character(len=*) :: iptStr
        call PPRICE
    end subroutine

    subroutine wrap_AUTOFUNC(iptStr)
        character(len=*) :: iptStr
        call AUTOFUNC
    end subroutine

    subroutine wrap_THM(iptStr)
        character(len=*) :: iptStr
        call TTHM
    end subroutine

    subroutine wrap_INR(iptStr)
        character(len=*) :: iptStr
        call INRINR
    end subroutine

    subroutine wrap_INRD(iptStr)
        character(len=*) :: iptStr
        call INRINRD
    end subroutine

    subroutine wrap_VIEOVER(iptStr)
        character(len=*) :: iptStr
        call VIEOVER
    end subroutine

    subroutine wrap_TFMOTION(iptStr)
        character(len=*) :: iptStr
        call TFMOTION
    end subroutine

    subroutine wrap_FLDSARE(iptStr)
        character(len=*) :: iptStr
        call FLDSARE
    end subroutine

    subroutine wrap_SEED(iptStr)
        character(len=*) :: iptStr
        call MYNEWSEED
    end subroutine

    subroutine wrap_PROGSIZE(iptStr)
        character(len=*) :: iptStr
        call PROGSIZE
    end subroutine

    subroutine wrap_RAYERROR(iptStr)
        character(len=*) :: iptStr
        call RERROR
    end subroutine

    subroutine wrap_READIRAD(iptStr)
        character(len=*) :: iptStr
        call READIRAD
    end subroutine

    subroutine wrap_TSTCMDS(iptStr)
        character(len=*) :: iptStr
        call TestCommands
    end subroutine


end module codeV_legacy_wrappers
