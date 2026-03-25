! zoa-plot-output.f90
! Lightweight module for coordinating headless plot output.
! Plot routines set plot_was_generated and last_plot_file;
! the ZMQ server reads them to append __IMAGE__ markers to replies.

module zoa_plot_output
  implicit none

  logical :: plot_was_generated = .false.
  character(len=512) :: last_plot_file = ''
  integer :: plot_counter = 0

contains

  subroutine set_plot_output(filepath)
    character(len=*), intent(in) :: filepath
    last_plot_file = filepath
    plot_was_generated = .true.
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
