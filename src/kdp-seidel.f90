! SUB MMAB3.FOR
subroutine MMAB3_NEW(YFLAG)
            use global_widgets, only:  curr_lens_data, curr_par_ray_trace
            use iso_fortran_env, only: real64
            use parax_calcs, only: calcInvariant
            use seidel_calcs, only: calcSeidelTerms
            use kdp_utils
            use command_utils
    
            IMPLICIT NONE
    
    !       THIS IS SUBROUTINE MMAB3. THIS SUBROUTINE IMPLEMENTS
    !       THE MAB3 (THIRD ORDER ABERRATION) PRINTOUT AT THE
    !       CMD LEVEL AND IS A MODEL FOR MAB5,MABX5 ETC..
    
            INTEGER I
            logical :: YFLAG

            real(kind=real64), allocatable, dimension(:,:) :: CS, CXS
    
            REAL(kind=real64) :: INV
            character(len=40), dimension(5) :: colHeaders
    
            INCLUDE 'DATLEN.INC'
            INCLUDE 'DATMAI.INC'
    
            CALL PRTRC
    
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

            colHeaders(1) = "Spherical"
            colHeaders(2) = "Coma"
            colHeaders(3) = "Astigmatism"
            colHeaders(4) = "Field Curvature"
            colHeaders(5) = "Distortion"
            !colHeaders(6) = "Axial Chromatic"
            !colHeaders(7) = "Lateral Chromatic"



            INV = calcInvariant()  

            call calcSeidelTerms(INV)
            ! CS(:,:)
            ! col 1 = coefficient
            ! col 2 = Y-Z at each surface
            ! CXS is same as C but X-Z plane


            if(YFLAG) then
                call OUTKDP("Y-Z Plane Seidel Aberrations")
            else
                call OUTKDP("X-Z Plane Seidel Aberrations")
            end if   
5002   FORMAT('ABERRATION CONTRIBUTIONS',' - (CFG #',I2,')')            
5501   FORMAT('TRANSVERSE - WITH FINAL SURFACE CONVERSION')
5502   FORMAT('TRANSVERSE - WITHOUT FINAL SURFACE CONVERSION')
5503   FORMAT('ANGULAR - WITH FINAL SURFACE CONVERSION')
5504   FORMAT('ANGULAR - WITHOUT FINAL SURFACE CONVERSION')
                        
            WRITE(OUTLYNE,5002) INT(F12)
            CALL SHOWIT(0)
            IF(SYSTEM(30).EQ.1.0) WRITE(OUTLYNE,5501)
            IF(SYSTEM(30).EQ.2.0) WRITE(OUTLYNE,5502)
            IF(SYSTEM(30).EQ.3.0) WRITE(OUTLYNE,5503)
            IF(SYSTEM(30).EQ.4.0) WRITE(OUTLYNE,5504)
            CALL SHOWIT(0)
            !WRITE(OUTLYNE,2501)
            !CALL SHOWIT(0)
            !WRITE(OUTLYNE,5000)
            !CALL SHOWIT(0)                     
            
            if (WQ.EQ.'ALL') then 


            if(YFLAG) then
              call logDataVsSurface(curr_par_ray_trace%CSeidel, colHeaders, 'Sum')
            else
              call logDataVsSurface(curr_par_ray_trace%CXSeidel, colHeaders)
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

            if(YFLAG) then
                call logDataVsSurface(curr_par_ray_trace%CSeidel, colHeaders, singleSurface=INT(W1))
            else
                call logDataVsSurface(curr_par_ray_trace%CXSeidel, colHeaders, singleSurface=INT(W1))
            end if
            
            END IF

     

end subroutine