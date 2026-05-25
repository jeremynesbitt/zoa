! SUB MMAB3.FOR
subroutine MMAB3_NEW(YFLAG, idxWV, printTable)
            use global_widgets, only:  curr_lens_data, curr_par_ray_trace
            use iso_fortran_env, only: real64
            use parax_calcs, only: calcInvariant
            use seidel_calcs, only: calcSeidelTerms
            use kdp_utils
            use command_utils
            use type_utils
            use mod_lens_data_manager

            use DATLEN
            use DATMAI
            use mod_system, only: sys_last_surf
            IMPLICIT NONE

            INTEGER I
            logical :: YFLAG
            integer, intent(in) :: idxWV
            logical, optional, intent(in) :: printTable

            real(kind=real64), allocatable, dimension(:,:) :: CS, CXS

            REAL(kind=real64) :: INV
            character(len=40), dimension(7) :: colHeaders

            CALL PRTRC
            CALL PRTRB

            ! When called programmatically (printTable present), skip command
            ! input validation — the caller controls output via printTable.
            if (.not. present(printTable)) then
                if (.not.checkCommandInput( &
                & ID_CMD_TYPE= [ID_CMD_QUAL, ID_CMD_NUM], &
                & qual_words=[character(len=6) :: "ALL", "I"], &
                & qual_only_err_msg = "Test error message", &
                & max_num_terms = 1)) then
                     CALL MACFAL
                     return
                 end if
            end if

            IF (curr_lens_data%num_surfaces.EQ.0) THEN
              call OUTKDP('LENS SYSTEM HAS NO SURFACES',1)
              call OUTKDP('NO PARAXIAL OR THIRD ORDER DATA EXISTS',1)
              CALL MACFAL
              RETURN
            END IF

            colHeaders(1) = "SA3"
            colHeaders(2) = "CMA3"
            colHeaders(3) = "AST3"
            colHeaders(4) = "DIS3"
            colHeaders(5) = "PTZ3"
            colHeaders(6) = "AC3"
            colHeaders(7) = "LC3"

            INV = calcInvariant()

            call calcSeidelTerms(INV)

            call OUTKDP(trim(sysConfig%lensTitle))
            call OUTKDP(blankStr(10)//"Position "//trim(int2str(ldm%getCurrentConfig()))//", Wavelength = "// &
            & trim(real2str(1000.0*sysConfig%getWavelength(idxWV),1)) //" nm")

            ! Print full surface-by-surface table when requested programmatically,
            ! or when the MAB3 ALL command was entered at the terminal.
            if ((present(printTable) .and. printTable) .or. &
                (.not. present(printTable) .and. WQ.EQ.'ALL')) then
                if(YFLAG) then
                    call logDataVsSurface(curr_par_ray_trace%CSeidel, colHeaders, 'SUM')
                else
                    call logDataVsSurface(curr_par_ray_trace%CXSeidel, colHeaders, 'SUM')
                end if
            end if

            ! Single-surface output (command-line only path: MAB3 <surfnum>)
            if (.not. present(printTable) .and. SQ.EQ.0 .and. DF1.NE.1) then
                IF(INT(W1).GT.INT(sys_last_surf()).OR.INT(W1).LT.0) THEN
                    CALL REPORT_ERROR_AND_FAIL('SURFACE NUMBER BEYOND LEGAL RANGE', 1)
                    RETURN
                END IF
                if(sysConfig%numWavelengths == 1) then
                    if(YFLAG) then
                        call logDataVsSurface(curr_par_ray_trace%CSeidel(1:5,:), colHeaders(1:5), singleSurface=INT(W1))
                    else
                        call logDataVsSurface(curr_par_ray_trace%CXSeidel(1:5,:), colHeaders(1:5), singleSurface=INT(W1))
                    end if
                else
                    if(YFLAG) then
                        call logDataVsSurface(curr_par_ray_trace%CSeidel, colHeaders, singleSurface=INT(W1))
                    else
                        call logDataVsSurface(curr_par_ray_trace%CXSeidel, colHeaders, singleSurface=INT(W1))
                    end if
                end if
            end if

end subroutine
