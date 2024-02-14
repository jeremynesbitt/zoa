module parax_calcs

contains

  function calcEFL() result(EFL)
        use iso_fortran_env, only: real64
        use global_widgets, only: curr_lens_data
        implicit none
        real(kind=real64) :: EFL
        integer :: I, J
        include "DATLEN.INC"

        I=0
        J=curr_lens_data%num_surfaces-2
      
        IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))).NE.0.0D0) THEN
        EFL=-(((PXTRAY(2,I)*PXTRAY(5,I+1))-(PXTRAY(1,I+1)*PXTRAY(6,I)))/ &
        & ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
          ELSE
        EFL=1.0D20
          END IF

      
  end function


  function calcBFL() result(BFL)
        use iso_fortran_env, only: real64
        use global_widgets
        implicit none
        real(kind=real64) :: BFL
        integer :: I, J
        include "DATLEN.INC"

        I=0
        !J=curr_lens_data%num_surfaces-1
        !PRINT *, "num surfaces is ", curr_lens_data%num_surfaces
        J=curr_lens_data%num_surfaces-2

        !PRINT *, "J is ", J

         IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))).NE.0.0D0) THEN
         BFL=-(((PXTRAY(2,I)*PXTRAY(5,J))-(PXTRAY(6,I)*PXTRAY(1,J)))/ &
         & ((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
                        ELSE
         BFL=1.0D20
                        END IF        

  end function

  function calcFFL() result(FFL)
        use iso_fortran_env, only: real64
        use global_widgets
        implicit none
        real(kind=real64) :: FFL
        integer :: I, J
        include "DATLEN.INC"

        I=0
        !J=curr_lens_data%num_surfaces-1
        !PRINT *, "num surfaces is ", curr_lens_data%num_surfaces
        J=curr_lens_data%num_surfaces-2

        IF(((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))).NE.0.0D0) THEN
        FFL=-(((PXTRAY(1,I+1)*PXTRAY(6,J))-(PXTRAY(2,J)*PXTRAY(5,I+1)))/((PXTRAY(2,I)*PXTRAY(6,J))-(PXTRAY(6,I)*PXTRAY(2,J))))
                        ELSE
        FFL=1.0D20
                        END IF        


  end function  

  subroutine calcExitPupil(epRadius, epPosition)

        real :: epRadius, epPosition, imgAngle
        integer :: newImage
        include "DATMAI.INC"
        include "DATLEN.INC"

        newImage = NEWIMG

        IF(ALENS(3,(newImage-1)).EQ.0.0D0.OR.ALENS(3,(newImage-1)).NE.0.0D0.AND..NOT.LDIF2) THEN
!       DIST FROM I-1 TO I IS ZERO OF LDIF2 IS FALSE AND DIST NOT 0
!       DO AN INTERNAL "ASTOP EX" ADJUSTMENT
        IF(DABS(PXTRAY(5,newImage)*1.0D-15).LT.DABS(PXTRAY(6,newImage))) THEN
              PRINT *, "EpRadius ASTOP EX Adjustment"
              epPosition=(PXTRAY(5,newImage)/PXTRAY(6,newImage))

        END IF
        IF(DABS(PXTRAY(5,newImage)*1.0D-15).GE.DABS(PXTRAY(6,newImage))) THEN
!       REF RAY IS TELECENTRIC, SET RAD INFINITY
            epPosition=1.0D20
        END IF
!       USE EXISTING THICKNESS OF NEWIMG-1 AS RAD
        ELSE
        Print *, "Use Existing Thickness for epRadius"
        epPosition=ALENS(3,newImage-1)
                        END IF
        !epPosition = 0.0
        imgAngle =  PXTRAY(2,INT(SYSTEM(20)))

        epRadius = ABS(TAN(imgAngle*3.14159265/180.0)*epPosition)

        PRINT *, "epRadius = ", epRadius
        PRINT *, "epPosition = ", epPosition
        PRINT *, "imgAngle = ", imgAngle



        !PRINT *, "ImageNA? ",

  end subroutine

  function calcInvariant() result(INV)
        use ISO_FORTRAN_ENV, only: real64    
        use global_widgets
        !use global_widgets, only: sysConfig, curr_lens_data
        implicit none
                        
        real(kind=real64)  :: INV   
        integer :: i, CW, SF


        include "DATMAI.INC"   
        include "DATLEN.INC"

        SF=INT(SYSTEM(20))
        IF(INT(SYSTEM(11)).GE.1.AND.INT(SYSTEM(11)).LE.5) THEN
                        CW=INT(SYSTEM(11))+45
                                END IF
        IF(INT(SYSTEM(11)).GE.6.AND.INT(SYSTEM(11)).LE.10) THEN
                        CW=INT(SYSTEM(11))+65
                                END IF                
              
                INV=1.0D0
        if(sysConfig%isFocalSystem()) THEN        
        !IF(SYSTEM(30).EQ.1.0D0) THEN
          ! MODE IS FOCAL
          INV=-2.0*ALENS(CW,(SF-1))*PXTRAY(2,(SF-1))
         
        else
         !MODE IS AFOCAL
          INV= 2.0*ALENS(CW,(SF-1))*PXTRAY(1,SF)
        end if
 
        IF(DABS(INV).LE.1.0D-10) THEN
                CALL MACFAL
                RETURN
        END IF       

        ! This needs to be in a checkINVValue method
        IF(INV.EQ.0.0D0) THEN
        OUTLYNE='THE LENS (MODE) IS NOT CONSISTENT WITH PARAXIAL VALUES'
        CALL SHOWIT(1)
        OUTLYNE='ABERRATIONS ARE NOT CALCULABLE'
        CALL SHOWIT(1)
        IF(sysConfig%isFocalSystem()) THEN
        OUTLYNE='CHANGE FROM "MODE FOCAL" TO "MODE AFOCAL"'
        ELSE
        OUTLYNE='CHANGE FROM "MODE AFOCAL" TO "MODE FOCAL"'
        CALL SHOWIT(1)
        OUTLYNE='THEN RE-ENTER COMMAND'
        CALL SHOWIT(1)
        CALL MACFAL
        RETURN
        END IF
        END IF                

         end function  



end module

module seidel_calcs

        !type seidel_data, extends(lens_data)


        !end type
     contains   


         subroutine calcSeidelTerms(INV)
                ! This routine needs to be called by the MMAB3 routine at present so the MAB3 and COLOR arrays can be populated
            use ISO_FORTRAN_ENV, only: real64  
            use kdp_data_types  
            use global_widgets, only: curr_lens_data, curr_par_ray_trace, sysConfig
            implicit none            

            real(kind=real64) :: INV    
            integer :: i



            include "DATMAI.INC"
            include "DATLEN.INC"

            if (allocated(curr_par_ray_trace%CSeidel)) deallocate(curr_par_ray_trace%CSeidel)
            if (allocated(curr_par_ray_trace%CXSeidel)) deallocate(curr_par_ray_trace%CXSeidel)
            

            allocate(curr_par_ray_trace%CSeidel(7, 0:curr_lens_data%num_surfaces))
            allocate(curr_par_ray_trace%CXSeidel(7,0:curr_lens_data%num_surfaces))

            ! Eventuall move MAB3 calc to this method
            curr_par_ray_trace%CSeidel(1:5,0:curr_lens_data%num_surfaces-1)=MAB3(1:5,0:curr_lens_data%num_surfaces-1)
            curr_par_ray_trace%CXSeidel(1:5,0:curr_lens_data%num_surfaces-1)=XMAB3(1:5,0:curr_lens_data%num_surfaces-1)

            ! Chromatic Aberrations

            curr_par_ray_trace%CSeidel(6:7,0:curr_lens_data%num_surfaces-1)=COLORY(1:2,0:curr_lens_data%num_surfaces-1)
            curr_par_ray_trace%CXSeidel(6:7,0:curr_lens_data%num_surfaces-1)=COLORX(1:2,0:curr_lens_data%num_surfaces-1)

            ! TODO:  ADD Inverse Calcs
            if (.not.sysConfig%isUSystem()) then 
                curr_par_ray_trace%CSeidel = curr_par_ray_trace%CSeidel/INV
                curr_par_ray_trace%CXSeidel = curr_par_ray_trace%CXSeidel/INV
            end if



            curr_par_ray_trace%CSeidel(:,curr_lens_data%num_surfaces) = 0.0
            curr_par_ray_trace%CXSeidel(:,curr_lens_data%num_surfaces) = 0.0

            do i=0,curr_lens_data%num_surfaces-1
                curr_par_ray_trace%CSeidel(:,curr_lens_data%num_surfaces) = &
                & curr_par_ray_trace%CSeidel(:,curr_lens_data%num_surfaces) + &
                & curr_par_ray_trace%CSeidel(:,i)
                
                curr_par_ray_trace%CXSeidel(:,curr_lens_data%num_surfaces) = &
                & curr_par_ray_trace%CXSeidel(:,curr_lens_data%num_surfaces) + &
                & curr_par_ray_trace%CXSeidel(:,i)                
            end do   
                

         end subroutine

end module