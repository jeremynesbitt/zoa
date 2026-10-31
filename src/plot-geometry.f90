! The geometry of the KDP plot page -- the one place it is defined.
!
! Every legacy drawing (the lens drawing VIE, the PLOT ... commands in
! PLOTCAD*.f90) is written in plot units on a page KDP_PAGE_W x KDP_PAGE_H,
! origin at the lower left.  MY_PLOT records are clipped to the page plus a
! KDP_CLIP_BORDER band (PAGE_PLOT supplies that box); PART drawings use a
! wider band.  The renderer (kdp-draw.f90) shows the page with an even
! KDP_PAGE_MARGIN on every side, centred in its canvas, and the cursor
! readout (mod_vie_transform) inverts the same mapping.
module plot_geometry
  use iso_fortran_env, only: real64
  implicit none
  public

  integer, parameter :: KDP_PAGE_W = 10000
  integer, parameter :: KDP_PAGE_H = 7000

  ! clip box of an ordinary page drawing
  integer, parameter :: KDP_CLIP_BORDER = 10
  integer, parameter :: KDP_CLIP_XLO = -KDP_CLIP_BORDER
  integer, parameter :: KDP_CLIP_XHI = KDP_PAGE_W + KDP_CLIP_BORDER
  integer, parameter :: KDP_CLIP_YLO = -KDP_CLIP_BORDER
  integer, parameter :: KDP_CLIP_YHI = KDP_PAGE_H + KDP_CLIP_BORDER

  ! clip box of a PART drawing (PLOTCAD6), whose title block and notes run
  ! past the page
  integer, parameter :: KDP_PART_BORDER = 500
  integer, parameter :: KDP_PART_XLO = -KDP_PART_BORDER
  integer, parameter :: KDP_PART_XHI = KDP_PAGE_W + KDP_PART_BORDER
  integer, parameter :: KDP_PART_YLO = -KDP_PART_BORDER
  integer, parameter :: KDP_PART_YHI = KDP_PAGE_H + KDP_PART_BORDER

  ! what the renderer shows: the page plus an even margin on every side
  real(real64), parameter :: KDP_PAGE_MARGIN = 250.0_real64
  real(real64), parameter :: KDP_DEVICE_W = KDP_PAGE_W + 2*KDP_PAGE_MARGIN
  real(real64), parameter :: KDP_DEVICE_H = KDP_PAGE_H + 2*KDP_PAGE_MARGIN

end module plot_geometry
