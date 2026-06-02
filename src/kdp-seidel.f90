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
            use result_builder

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

            ! --- Populate result_builder with structured Seidel data ---
            call populate_seidel_result(idxWV)

            if (.not. (present(printTable) .and. .not. printTable)) then
                call OUTKDP(trim(sysConfig%lensTitle))
                call OUTKDP(blankStr(10)//"Position "//trim(int2str(ldm%getCurrentConfig()))//", Wavelength = "// &
                & trim(real2str(1000.0*sysConfig%getWavelength(idxWV),1)) //" nm")
            end if

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

contains

    ! Populate result_builder with structured Seidel data.
    ! CSeidel has dimensions (7, 0:num_surfaces).
    ! Column 0..num_surfaces-1 are per-surface; column num_surfaces is the sum.
    ! We expose:
    !   table "seidel": rows = surfaces (including SUM), cols = 7 aberration terms
    !   scalars: sum_spherical, sum_coma, sum_astigmatism, sum_distortion,
    !            sum_curvature, sum_axial_chromatic, sum_lateral_chromatic
    subroutine populate_seidel_result(idxWV)
        use global_widgets, only: curr_lens_data, curr_par_ray_trace, sysConfig
        use type_utils, only: int2str, real2str
        use result_builder
        use iso_fortran_env, only: real64
        implicit none
        integer, intent(in) :: idxWV

        integer :: nsurfs, nrows, i
        character(len=16), allocatable :: row_labels(:)
        character(len=16), parameter   :: col_labels(7) = [ &
            character(len=16) :: &
            'SA3', 'CMA3', 'AST3', 'DIS3', 'PTZ3', 'AC3', 'LC3']
        real(real64), allocatable :: tdata(:,:)   ! (nrows, 7)
        character(len=32) :: wv_str

        ! Guard: CSeidel must be allocated
        if (.not. allocated(curr_par_ray_trace%CSeidel)) return
        nsurfs = curr_lens_data%num_surfaces
        ! nrows = nsurfs per-surface rows + 1 SUM row
        nrows = nsurfs + 1

        allocate(row_labels(nrows))
        allocate(tdata(nrows, 7))

        ! Build row labels: S0..S{nsurfs-1}, then SUM
        do i = 1, nsurfs
            row_labels(i) = 'S' // trim(int2str(i-1))
        end do
        row_labels(nrows) = 'SUM'

        ! Fill table data: CSeidel(term, surf_idx)
        ! surf_idx runs 0..nsurfs; 0..nsurfs-1 are per-surface, nsurfs is sum.
        do i = 1, nsurfs
            tdata(i, :) = curr_par_ray_trace%CSeidel(1:7, i-1)
        end do
        tdata(nrows, :) = curr_par_ray_trace%CSeidel(1:7, nsurfs)

        write(wv_str, '(F10.4)') 1000.0_real64 * sysConfig%getWavelength(idxWV)

        call result_begin("seidel")
        call result_set_meta("title", trim(sysConfig%lensTitle))
        call result_set_meta("wavelength_nm", trim(adjustl(wv_str)))
        call result_set_meta("units", "lens units")

        call result_add_table("seidel", row_labels, col_labels, tdata)

        ! Convenience scalars from the SUM column
        call result_set_scalar("sum_spherical",       curr_par_ray_trace%CSeidel(1, nsurfs))
        call result_set_scalar("sum_coma",            curr_par_ray_trace%CSeidel(2, nsurfs))
        call result_set_scalar("sum_astigmatism",     curr_par_ray_trace%CSeidel(3, nsurfs))
        call result_set_scalar("sum_distortion",      curr_par_ray_trace%CSeidel(4, nsurfs))
        call result_set_scalar("sum_curvature",       curr_par_ray_trace%CSeidel(5, nsurfs))
        call result_set_scalar("sum_axial_chromatic", curr_par_ray_trace%CSeidel(6, nsurfs))
        call result_set_scalar("sum_lateral_chromatic", curr_par_ray_trace%CSeidel(7, nsurfs))

        deallocate(row_labels)
        deallocate(tdata)
    end subroutine populate_seidel_result

end subroutine
