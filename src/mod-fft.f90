module mod_fft
    use globals, only: long
    implicit none
    real :: pi = 4.D0*atan(1.D0)


	INTERFACE swap
		MODULE PROCEDURE swap_i,swap_r,swap_rv,swap_c, &
			swap_cv,swap_cm,swap_z,swap_zv,swap_zm, &
			masked_swap_rs,masked_swap_rv,masked_swap_rm
	END INTERFACE

	INTERFACE fourrow
		MODULE PROCEDURE fourrow_dp, fourrow_sp !, fourrow_3d
	END INTERFACE

    interface fftshift
        module procedure fftshift_1d, fftshift_2d, fftshift_2d_dp
    end interface


  contains
  
    ! subroutine fft2(input, output)
    !   complex(8), intent(in) :: input(:,:)
    !   complex(8), intent(out) :: output(size(input,1),size(input,2))
    !   integer :: i, j
  
    !   ! Perform 1D FFT along rows
    !   do i = 1, size(input,1)
    !      call fft(input(i,:), output(i,:))
    !   end do
  
    !   ! Perform 1D FFT along columns
    !   do j = 1, size(input,2)
    !      call fft(output(:,j), output(:,j))
    !   end do
    ! end subroutine fft2
  
    ! subroutine fft(input, output)
    !   complex(8), dimension(:), intent(in) :: input
    !   complex(8), dimension(:), intent(out) :: output
    !   integer :: n, m, k, j
    !   complex(8) :: temp, twiddle, sum
    !   real(8) :: theta
  
    !   n = size(input)
    !   output = input
  
    !   ! Apply the Cooley-Tukey FFT algorithm
    !   m = 1
    !   do while (m < n)
    !      m = m * 2
    !      theta = -2.0 * 3.14159 / real(m)
    !      do k = 1, m / 2
    !         twiddle = cmplx(cos(real(k) * theta), sin(real(k) * theta))
    !         do j = 1, n, m
    !            sum = output(j+k-1) + twiddle * output(j+k-1+m/2)
    !            temp = output(j+k-1) - twiddle * output(j+k-1+m/2)
    !            output(j+k-1) = sum
    !            output(j+k-1+m/2) = temp
    !         end do
    !      end do
    !   end do
    ! end subroutine fft
  

  

     ! This was chatGPT produced, and does not work as I wanted.  So scrapped it
    ! Function to compute the 2D FFT
    ! function fft2(array) result (arrOut)
    !   complex, dimension(:,:), intent(in) :: array
    !   complex, dimension(size(array,1),size(array,2)) :: arrOut
    !   integer :: nx, ny, i, j
    !   complex :: temp
    !   complex, dimension(:), allocatable :: row, col
 
    !   ! Get the dimensions of the 2D array
    !   nx = size(array, 1)
    !   ny = size(array, 2)
  
    !   ! Perform 1D FFT on each row (along the x-axis)
    !   allocate(row(nx))
    !   do i = 1, ny
    !       row = array(:, i)
    !       call fft1d(row)
    !       arrOut(:,i) = row
    !       !array(:, i) = row
    !   end do
  
    !   ! Perform 1D FFT on each column (along the y-axis)
    !   allocate(col(ny))
    !   do j = 1, nx
    !       col = array(j, :)
    !       call fft1d(col)
    !       arrOut(j,:) = col
    !       !array(j, :) = col
    !   end do
  
    !   deallocate(row, col)
  
    ! end function fft2
  
    ! Function to compute the 1D FFT (Cooley-Tukey algorithm)
    subroutine fft1d(a)
      complex, dimension(:), intent(inout) :: a
      integer :: n, m, j, i, k, l
      complex :: t, w, temp, w_m, a1
  
      ! Get the size of the input array
      n = size(a)
  
      ! Bit-reversal reordering (in-place)
      m = n / 2
      j = 1
      do i = 2, n
          if (i < j) then
              t = a(i)
              a(i) = a(j)
              a(j) = t
          end if
          k = m
          do while (k .ge. 1 .and. j .gt. k)
              j = j - k
              k = k / 2
          end do
          j = j + k
      end do
  
      ! FFT computation (Cooley-Tukey decimation-in-time)
      m = 1
      do while (m < n)
          w_m = cmplx(cos(2.0 * pi / (2.0 * m + 1.0)), sin(2.0 * pi / (2.0 * m + 1.0)))
          do k = 0, m - 1
              w = cmplx(cos(2.0 * pi * k / (2.0 * m)), sin(2.0 * pi * k / (2.0 * m)))
              do i = k + 1, n, 2 * m
                  j = i + m
                  temp = w * a(j)
                  a(j) = a(i) - temp
                  a(i) = a(i) + temp
              end do
          end do
          m = 2 * m
      end do
    end subroutine fft1d
  
	SUBROUTINE fourrow_sp(data,isign)
        IMPLICIT NONE
        COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: data
        INTEGER, INTENT(IN) :: isign
        INTEGER :: n,i,istep,j,m,mmax,n2
        REAL(long) :: theta
        COMPLEX, DIMENSION(size(data,1)) :: temp
        COMPLEX(long) :: w,wp
        COMPLEX :: ws
        n=size(data,2)
        !call assert(iand(n,n-1)==0, 'n must be a power of 2 in fourrow_sp')
        n2=n/2
        j=n2
        do i=1,n-2
            if (j > i) call swap(data(:,j+1),data(:,i+1))
            m=n2
            do
                if (m < 2 .or. j < m) exit
                j=j-m
                m=m/2
            end do
            j=j+m
        end do
        mmax=1
        do
            if (n <= mmax) exit
            istep=2*mmax
            theta=pi/(isign*mmax)
            wp=cmplx(-2.0_long*sin(0.5_long*theta)**2,sin(theta))
            w=cmplx(1.0_long,0.0_long)
            do m=1,mmax
                ws=w
                do i=m,n,istep
                    j=i+mmax
                    temp=ws*data(:,j)
                    data(:,j)=data(:,i)-temp
                    data(:,i)=data(:,i)+temp
                end do
                w=w*wp+w
            end do
            mmax=istep
        end do
        END SUBROUTINE fourrow_sp
    
        SUBROUTINE fourrow_dp(data,isign)
        IMPLICIT NONE
        COMPLEX(long), DIMENSION(:,:), INTENT(INOUT) :: data
        INTEGER, INTENT(IN) :: isign
        INTEGER :: n,i,istep,j,m,mmax,n2
        REAL(long) :: theta
        COMPLEX(long), DIMENSION(size(data,1)) :: temp
        COMPLEX(long) :: w,wp
        COMPLEX(long) :: ws
        n=size(data,2)
        !call assert(iand(n,n-1)==0, 'n must be a power of 2 in fourrow_dp')
        n2=n/2
        j=n2
        do i=1,n-2
            if (j > i) call swap(data(:,j+1),data(:,i+1))
            m=n2
            do
                if (m < 2 .or. j < m) exit
                j=j-m
                m=m/2
            end do
            j=j+m
        end do
        mmax=1
        do
            if (n <= mmax) exit
            istep=2*mmax
            theta=pi/(isign*mmax)
            wp=cmplx(-2.0_long*sin(0.5_long*theta)**2,sin(theta))
            w=cmplx(1.0_long,0.0_long)
            do m=1,mmax
                ws=w
                do i=m,n,istep
                    j=i+mmax
                    temp=ws*data(:,j)
                    data(:,j)=data(:,i)-temp
                    data(:,i)=data(:,i)+temp
                end do
                w=w*wp+w
            end do
            mmax=istep
        end do
        END SUBROUTINE fourrow_dp

    function fft2(iptData,isign) result(fftData)
        IMPLICIT NONE
        COMPLEX(long), DIMENSION(:,:), INTENT(IN) :: iptData
        COMPLEX(long), DIMENSION(size(iptData,2),size(iptData,1)) :: fftData
        INTEGER, optional, INTENT(IN) :: isign
        COMPLEX(long), DIMENSION(size(iptData,2),size(iptData,1)) :: temp
        integer :: ii
        if(.not.(present(isign))) then 
            ii = 1
        else
            ii = isign
        end if
        fftData = iptData
        call fourrow(fftData,ii)
        temp=transpose(fftData)
        call fourrow(temp,ii)
        fftData=transpose(temp)
        
    end function

	SUBROUTINE four2(data,isign)
        IMPLICIT NONE
        COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: data
        INTEGER, INTENT(IN) :: isign
        COMPLEX, DIMENSION(size(data,2),size(data,1)) :: temp
        call fourrow(data,isign)
        temp=transpose(data)
        call fourrow(temp,isign)
        data=transpose(temp)
        END SUBROUTINE four2

!BL
        SUBROUTINE swap_i(a,b)
            INTEGER, INTENT(INOUT) :: a,b
            INTEGER :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_i
        !BL
            SUBROUTINE swap_r(a,b)
            REAL, INTENT(INOUT) :: a,b
            REAL :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_r
        !BL
            SUBROUTINE swap_rv(a,b)
            REAL, DIMENSION(:), INTENT(INOUT) :: a,b
            REAL, DIMENSION(SIZE(a)) :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_rv
        !BL
            SUBROUTINE swap_c(a,b)
            COMPLEX, INTENT(INOUT) :: a,b
            COMPLEX :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_c
        !BL
            SUBROUTINE swap_cv(a,b)
            COMPLEX, DIMENSION(:), INTENT(INOUT) :: a,b
            COMPLEX, DIMENSION(SIZE(a)) :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_cv
        !BL
            SUBROUTINE swap_cm(a,b)
            COMPLEX, DIMENSION(:,:), INTENT(INOUT) :: a,b
            COMPLEX, DIMENSION(size(a,1),size(a,2)) :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_cm
        !BL
            SUBROUTINE swap_z(a,b)
            COMPLEX(long), INTENT(INOUT) :: a,b
            COMPLEX(long) :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_z
        !BL
            SUBROUTINE swap_zv(a,b)
            COMPLEX(long), DIMENSION(:), INTENT(INOUT) :: a,b
            COMPLEX(long), DIMENSION(SIZE(a)) :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_zv
        !BL
            SUBROUTINE swap_zm(a,b)
            COMPLEX(long), DIMENSION(:,:), INTENT(INOUT) :: a,b
            COMPLEX(long), DIMENSION(size(a,1),size(a,2)) :: dum
            dum=a
            a=b
            b=dum
            END SUBROUTINE swap_zm
        !BL
            SUBROUTINE masked_swap_rs(a,b,mask)
            REAL, INTENT(INOUT) :: a,b
            LOGICAL, INTENT(IN) :: mask
            REAL :: swp
            if (mask) then
                swp=a
                a=b
                b=swp
            end if
            END SUBROUTINE masked_swap_rs
        !BL
            SUBROUTINE masked_swap_rv(a,b,mask)
            REAL, DIMENSION(:), INTENT(INOUT) :: a,b
            LOGICAL, DIMENSION(:), INTENT(IN) :: mask
            REAL, DIMENSION(size(a)) :: swp
            where (mask)
                swp=a
                a=b
                b=swp
            end where
            END SUBROUTINE masked_swap_rv
        !BL
            SUBROUTINE masked_swap_rm(a,b,mask)
            REAL, DIMENSION(:,:), INTENT(INOUT) :: a,b
            LOGICAL, DIMENSION(:,:), INTENT(IN) :: mask
            REAL, DIMENSION(size(a,1),size(a,2)) :: swp
            where (mask)
                swp=a
                a=b
                b=swp
            end where
            END SUBROUTINE masked_swap_rm


  ! Function to perform fftshift on a 1D array
            subroutine fftshift_1d(arr)
                complex, dimension(:), intent(inout) :: arr
                integer :: n, mid, i
                complex :: temp
            
                n = size(arr)
                mid = n / 2
            
                ! Swap the two halves of the array
                do i = 1, mid
                  temp = arr(i)
                  arr(i) = arr(i + mid)
                  arr(i + mid) = temp
                end do
            
              end subroutine fftshift_1d
            
              ! Function to perform fftshift on a 2D array
              subroutine fftshift_2d(arr)
                complex, dimension(:,:), intent(inout) :: arr
                integer :: nrows, ncols, mid_row, mid_col, i, j
                complex :: temp
            
                nrows = size(arr, 1)
                ncols = size(arr, 2)
                mid_row = nrows / 2
                mid_col = ncols / 2
            
                ! Swap the quadrants of the array
                do i = 1, mid_row
                  do j = 1, mid_col
                    temp = arr(i, j)
                    arr(i, j) = arr(i + mid_row, j + mid_col)
                    arr(i + mid_row, j + mid_col) = temp
            
                    temp = arr(i, j + mid_col)
                    arr(i, j + mid_col) = arr(i + mid_row, j)
                    arr(i + mid_row, j) = temp
                  end do
                end do
              end subroutine fftshift_2d

              ! Function to perform fftshift on a 2D array
              subroutine fftshift_2d_dp(arr)
                complex(long), dimension(:,:), intent(inout) :: arr
                integer :: nrows, ncols, mid_row, mid_col, i, j
                complex :: temp
            
                nrows = size(arr, 1)
                ncols = size(arr, 2)
                mid_row = nrows / 2
                mid_col = ncols / 2
            
                ! Swap the quadrants of the array
                do i = 1, mid_row
                  do j = 1, mid_col
                    temp = arr(i, j)
                    arr(i, j) = arr(i + mid_row, j + mid_col)
                    arr(i + mid_row, j + mid_col) = temp
            
                    temp = arr(i, j + mid_col)
                    arr(i, j + mid_col) = arr(i + mid_row, j)
                    arr(i + mid_row, j) = temp
                  end do
                end do
              end subroutine fftshift_2d_dp              

  end module mod_fft
  