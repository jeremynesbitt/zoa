! zoa-headless-plot.f90
! Headless rendering of lens drawings (VIE) to PNG files.
! Creates a file-backed Cairo image surface and calls the existing
! DRAWOPTICALSYSTEM renderer, then saves to PNG.

module zoa_headless_plot
  use iso_c_binding
  implicit none

  integer(c_int), parameter :: CAIRO_FMT_RGB24 = 1

  ! Minimal C bindings for Cairo functions not in gtk-fortran
  interface
    function c_cairo_image_surface_create(format, width, height) &
        bind(c, name='cairo_image_surface_create')
      import :: c_ptr, c_int
      integer(c_int), value :: format, width, height
      type(c_ptr) :: c_cairo_image_surface_create
    end function

    function c_cairo_create(surface) bind(c, name='cairo_create')
      import :: c_ptr
      type(c_ptr), value :: surface
      type(c_ptr) :: c_cairo_create
    end function

    subroutine c_cairo_destroy(cr) bind(c, name='cairo_destroy')
      import :: c_ptr
      type(c_ptr), value :: cr
    end subroutine

    subroutine c_cairo_surface_destroy(surface) &
        bind(c, name='cairo_surface_destroy')
      import :: c_ptr
      type(c_ptr), value :: surface
    end subroutine

    function c_cairo_surface_write_to_png(surface, filename) &
        bind(c, name='cairo_surface_write_to_png')
      import :: c_ptr, c_char, c_int
      type(c_ptr), value :: surface
      character(kind=c_char), intent(in) :: filename(*)
      integer(c_int) :: c_cairo_surface_write_to_png
    end function
  end interface

contains

  subroutine render_vie_to_png(filename)
    use kdp_draw, only: DRAWOPTICALSYSTEM
    use zoa_plot_output, only: set_plot_output

    character(len=*), intent(in) :: filename
    type(c_ptr) :: surface, cr
    integer(c_int) :: width, height, status

    width = 1200
    height = 800
    surface = c_cairo_image_surface_create(CAIRO_FMT_RGB24, width, height)
    cr = c_cairo_create(surface)

    ! Render using existing DRAWOPTICALSYSTEM
    ! Pass c_null_ptr for widget and gdata (headless guards in kdp-draw.f90)
    call DRAWOPTICALSYSTEM(c_null_ptr, cr, width, height, c_null_ptr)

    ! Save to PNG
    status = c_cairo_surface_write_to_png(surface, trim(filename) // c_null_char)

    call c_cairo_destroy(cr)
    call c_cairo_surface_destroy(surface)

    if (status == 0) then
      call set_plot_output(filename)
    end if
  end subroutine

end module zoa_headless_plot
