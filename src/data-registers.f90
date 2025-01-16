! 
module data_registers
    use GLOBALS, only: long

    implicit none

    type image_data
        real(long), allocatable :: img(:,:)
        integer :: N ! Assume square
        real(long) :: pS ! pixel size of data.  Eg for wavefront data will be NA diff between points
        character(len=1024) :: cmd ! the command that produced this data
    end type

    interface getData
        module procedure getData_scalar
        module procedure getData_2d
        module procedure getData_3d
        module procedure getData_img

        !module procedure convert_c_string_array_cptr
    end interface getData
    
    ! Register Data
    
    real(long) :: regData1d
    real(long), allocatable :: regData2d(:,:), regData3d(:,:,:)
    integer :: id_1d, id_2d, id_3d ! Codes associated with register data
    type(image_data), allocatable :: currImg

    contains

    subroutine getData_scalar(strCmd, data1d)
        character(len=*), intent(in) :: strCmd
        real(long), intent(out) :: data1d
    end subroutine

    subroutine getData_2d(strCmd, data2d)
        character(len=*), intent(in) :: strCmd
        real(long), allocatable, intent(out) :: data2d(:,:)
    end subroutine

    subroutine getData_3d(strCmd, data3d)
        character(len=*), intent(in) :: strCmd
        real(long), allocatable, intent(out) :: data3d(:,:,:)
        integer :: cmdID

        ! Would like to implement this but this would require some 
        ! infra changes
        !cmdID = getCodeFromCmd(strCmd)

        if (allocated(regData3d)) deallocate(regData3d)
        call PROCESSILENT(strCmd)
        if(allocated(regData3d)) then 
            allocate(data3d, mold=regData3d)
            data3d = regData3d
        end if

    end subroutine

    subroutine getData_img(strCmd, dataImg)
        character(len=*), intent(in) :: strCmd
        type(image_data), allocatable, intent(out) :: dataImg
        integer :: cmdID

        ! Would like to implement this but this would require some 
        ! infra changes
        !cmdID = getCodeFromCmd(strCmd)

        if (allocated(currImg)) deallocate(currImg)
        call PROCESSILENT(strCmd)
        if(allocated(currImg)) then
            currImg%cmd = strCmd 
            allocate(dataImg)
            dataImg = currImg
        end if

    end subroutine



end module