! zoa-plot-output.f90
! Lightweight module for coordinating headless plot output.
! Plot routines set plot_was_generated and last_plot_file;
! the ZMQ server reads them to append __IMAGE__ markers to replies.
!
! When announce_plots is enabled (test runner only), every successful PNG
! write also emits a "PLOT: <basename> WxH" line via zoa_emit, so plot
! generation becomes part of the golden-file output and the runner can
! sanity-check the PNG on disk.

module zoa_plot_output
  implicit none

  logical :: plot_was_generated = .false.
  character(len=512) :: last_plot_file = ''
  integer :: plot_counter = 0
  logical :: announce_plots = .false.

contains

  subroutine enable_plot_announcements()
    announce_plots = .true.
  end subroutine

  subroutine set_plot_output(filepath, width, height)
    use zoa_output, only: zoa_emit
    character(len=*), intent(in) :: filepath
    integer, intent(in), optional :: width, height
    character(len=600) :: line
    integer :: islash
    last_plot_file = filepath
    plot_was_generated = .true.
    if (announce_plots) then
      ! basename only -- keep absolute paths out of golden reference files
      islash = index(filepath, '/', back=.true.)
      line = 'PLOT: '//trim(filepath(islash+1:))
      if (present(width) .and. present(height)) then
        write(line, '(A,1X,I0,A,I0)') trim(line), width, 'x', height
      end if
      call zoa_emit(trim(line), "black")
    end if
  end subroutine

  subroutine clear_plot_output()
    plot_was_generated = .false.
    last_plot_file = ''
  end subroutine

  function next_plot_path() result(path)
    character(len=512) :: path
    plot_counter = plot_counter + 1
    write(path, '(A,I0,A)') '/tmp/zoa_plot_', plot_counter, '.png'
  end function

end module zoa_plot_output
