! SUB MMAB3.FOR
subroutine MMAB3_NEW(YFLAG, idxWV)
            use global_widgets, only:  curr_lens_data, curr_par_ray_trace
            use iso_fortran_env, only: real64
            use parax_calcs, only: calcInvariant
            use seidel_calcs, only: calcSeidelTerms
            use kdp_utils
            use command_utils
            use type_utils
            use mod_lens_data_manager
    
            IMPLICIT NONE
    
    !       THIS IS SUBROUTINE MMAB3. THIS SUBROUTINE IMPLEMENTS
    !       THE MAB3 (THIRD ORDER ABERRATION) PRINTOUT AT THE
    !       CMD LEVEL AND IS A MODEL FOR MAB5,MABX5 ETC..
    
            INTEGER I
            logical :: YFLAG
            integer, intent(in) :: idxWV

            real(kind=real64), allocatable, dimension(:,:) :: CS, CXS
    
            REAL(kind=real64) :: INV
            character(len=40), dimension(7) :: colHeaders
    
            INCLUDE 'DATLEN.INC'
            INCLUDE 'DATMAI.INC'
    
            CALL PRTRC
            CALL PRTRB
    
    !       THE MAB3 AND XMAB3 COMMAND ACCEPTS QUALIFIER OR NUMERIC
    !       INPUT IN AN EITHER OR MODE. IT DOES NOT ACCEPT STING
    !       INPUT. THE VALID QUALIFIERS ARE "ALL" AND "OBJ" AND "OB"
    !       "I","IM", AND "IMAGE"
    !       THE VALID NUMERIC INPUT IS THE SURFACE NUMBER FOR
    !       WHICH IT IS DESIRED TO PRODUCE OUTPUT.
    !       
            if (.not.checkCommandInput( &
            & ID_CMD_TYPE= [ID_CMD_QUAL, ID_CMD_NUM], &
            & qual_words=[character(len=6) :: "ALL", "I"], &
            & qual_only_err_msg = "Test error message", & 
            & max_num_terms = 1)) then 
                 CALL MACFAL
                 return
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

            !PRINT *, "Invariant is ", INV

            call calcSeidelTerms(INV)
            ! CS(:,:)
            ! col 1 = coefficient
            ! col 2 = Y-Z at each surface
            ! CXS is same as C but X-Z plane

            call OUTKDP(trim(sysConfig%lensTitle))
            call OUTKDP(blankStr(10)//"Position "//trim(int2str(ldm%getCurrentConfig()))//", Wavelength = "// &
            & trim(real2str(1000.0*sysConfig%getWavelength(idxWV),1)) //" nm")

! 5002   FORMAT('ABERRATION CONTRIBUTIONS',' - (CFG #',I2,')')            
! 5501   FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
! 5502   FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
! 5503   FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
! 5504   FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
                        
!             WRITE(OUTLYNE,5002) INT(F12)
!             CALL SHOWIT(0)
!             IF(SYSTEM(30).EQ.1.0) WRITE(OUTLYNE,5501)
!             IF(SYSTEM(30).EQ.2.0) WRITE(OUTLYNE,5502)
!             IF(SYSTEM(30).EQ.3.0) WRITE(OUTLYNE,5503)
!             IF(SYSTEM(30).EQ.4.0) WRITE(OUTLYNE,5504)
!             CALL SHOWIT(0)
            !WRITE(OUTLYNE,2501)
            !CALL SHOWIT(0)
            !WRITE(OUTLYNE,5000)
            !CALL SHOWIT(0)                     
            
            if (WQ.EQ.'ALL') then 


            if(YFLAG) then
              call logDataVsSurface(curr_par_ray_trace%CSeidel, colHeaders, 'SUM')
            else
              call logDataVsSurface(curr_par_ray_trace%CXSeidel, colHeaders, 'SUM')
            end if
             end if
            
            !TODO make a type that stores results from command
             ! this should be if(CMD_TYPE_NUM) then get num

             IF(SQ.EQ.0.AND.DF1.NE.1) THEN

               
                IF(INT(W1).GT.INT(SYSTEM(20)).OR.INT(W1).LT.0) THEN
                 OUTLYNE='SURFACE NUMBER BEYOND LEGAL RANGE'
                 CALL SHOWIT(1)
                 CALL MACFAL
                 RETURN
                END IF

            !TODO:  CLean this up.  Perhaps make Seidel 3 dims to avoid this mess?    
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
          END IF

     

end subroutine