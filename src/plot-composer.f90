! MULTIPLOT page composer: the state between MULTIPLOT and its closing GO.
!
!   MULTIPLOT rows cols ['title'] [Pn]   -- start a page (a plot tab)
!   SUBPLOT r c [rspan cspan]            -- the region the next analysis fills
!   <analysis> ... GO                    -- e.g. RIM ; SETFLD 2 ; GO
!   GO                                   -- (nothing open) draw the page
!
! While a page is being composed, initializeGoPlot / finalizeGoPlot_new
! (plot_functions) hand each analysis's finished multiplot to the page
! instead of opening a tab of its own.  The commands issued at the
! MULTIPLOT's own PROCESKDP level are recorded as the page's script, which
! is the tab's plot command: re-running it rebuilds the page on a lens
! change and restores it from a .zin.  Commands deeper than that level are
! the analyses' own internal ones and are not recorded.
!
! States nest (a stack): a page's script re-run by a replot while another
! page is being composed starts a page of its own, deeper in PROCESKDP, and
! closes it before control returns.
!
! Leaf module (zoa_plot only) so plot_functions and codeV_commands can both
! use it.
module plot_composer
  use zoa_plot, only: multiplot
  use iso_c_binding, only: c_null_ptr
  use macro_recorder, only: rec_depth, rec_suppress_level
  implicit none
  private
  public :: comp_begin, comp_end, comp_active, comp_depth, comp_rows, comp_cols
  public :: comp_record, comp_script, comp_obj_idx, comp_set_page, comp_get_page
  public :: comp_set_slot, comp_claim_slot, comp_store, comp_page, comp_count

  integer, parameter :: MAX_NEST = 4
  ! page cell size and title band, pixels
  integer, parameter, public :: CELL_W = 900, CELL_H = 600, TITLE_H = 50

  type :: comp_state
    integer :: rows = 0, cols = 0
    integer :: slot(4) = 0          ! SUBPLOT r c rspan cspan (0: none chosen)
    integer :: pending(4) = 0       ! the region the running analysis fills
    integer :: depth = 0            ! PROCESKDP depth of the MULTIPLOT
    integer :: suppress = 0         ! and its internal-work level (a redraw)
    integer :: objIdx = -1          ! the page's tab (-1 headless)
    integer :: plotNum = -1         ! its P number
    logical :: replot = .false.     ! the tab existed (a redraw)
    integer :: nStored = 0
    character(len=:), allocatable :: script
    type(multiplot) :: page
  end type

  type(comp_state), target :: st(MAX_NEST)
  integer :: top = 0

contains

  ! Start a page of rows x cols cells.  script: its opening command (the
  ! MULTIPLOT with its plot number).  False when pages nest too deeply.
  logical function comp_begin(rows, cols, script, title) result(ok)
    integer, intent(in) :: rows, cols
    character(len=*), intent(in) :: script, title
    ok = .false.
    if (top >= MAX_NEST) return
    top = top + 1
    associate (s => st(top))
      s%rows = rows
      s%cols = cols
      s%slot = 0
      s%pending = 0
      s%depth = rec_depth()
      s%suppress = rec_suppress_level()
      s%objIdx = -1
      s%nStored = 0
      s%script = trim(script)
      call s%page%initialize(c_null_ptr, rows, cols)
      s%page%m_title = title
      s%page%isPage = .true.
      s%page%m_hasTitle = len_trim(title) > 0
      ! Each cell about the size of a plot of its own, so its text is drawn
      ! at (nearly) its usual size; the page's tab scrolls rather than
      ! shrinking it to fit (multiplot_page_end).
      s%page%width = CELL_W*cols
      s%page%height = CELL_H*rows
      if (s%page%m_hasTitle) s%page%height = s%page%height + TITLE_H
    end associate
    ok = .true.
  end function

  subroutine comp_end()
    if (top < 1) return
    call st(top)%page%clear()
    if (allocated(st(top)%script)) deallocate(st(top)%script)
    st(top)%rows = 0
    st(top)%cols = 0
    top = top - 1
  end subroutine

  ! A page is being composed at this command level: the commands running now
  ! are its script's (not a redraw of another plot that ran in between, at
  ! the same depth but as Zoa's own work, nor an analysis's own internal
  ! commands, deeper).
  logical function comp_active()
    comp_active = .false.
    if (top < 1) return
    comp_active = rec_depth() == st(top)%depth .and. &
    &             rec_suppress_level() == st(top)%suppress
  end function

  integer function comp_depth()
    comp_depth = 0
    if (top > 0) comp_depth = st(top)%depth
  end function

  integer function comp_rows()
    comp_rows = 0
    if (top > 0) comp_rows = st(top)%rows
  end function

  integer function comp_cols()
    comp_cols = 0
    if (top > 0) comp_cols = st(top)%cols
  end function

  ! the number of subplots stored so far
  integer function comp_count()
    comp_count = 0
    if (top > 0) comp_count = st(top)%page%nChildren
  end function

  ! Append a command to the page's script.
  subroutine comp_record(seg)
    character(len=*), intent(in) :: seg
    if (top < 1) return
    if (len_trim(seg) == 0) return
    st(top)%script = st(top)%script//'; '//trim(adjustl(seg))
  end subroutine

  function comp_script() result(s)
    character(len=:), allocatable :: s
    s = ''
    if (top > 0) s = st(top)%script
  end function

  integer function comp_obj_idx()
    comp_obj_idx = -1
    if (top > 0) comp_obj_idx = st(top)%objIdx
  end function

  subroutine comp_set_page(objIdx, plotNum, replot)
    integer, intent(in) :: objIdx, plotNum
    logical, intent(in) :: replot
    if (top < 1) return
    st(top)%objIdx = objIdx
    st(top)%plotNum = plotNum
    st(top)%replot = replot
  end subroutine

  subroutine comp_get_page(objIdx, plotNum, replot)
    integer, intent(out) :: objIdx, plotNum
    logical, intent(out) :: replot
    objIdx = -1; plotNum = -1; replot = .false.
    if (top < 1) return
    objIdx = st(top)%objIdx
    plotNum = st(top)%plotNum
    replot = st(top)%replot
  end subroutine

  ! SUBPLOT r c [rspan cspan]: the region for the next analysis.  msg is set
  ! (and false returned) when it does not fit the page.
  logical function comp_set_slot(r, c, rs, cs, msg) result(ok)
    integer, intent(in) :: r, c, rs, cs
    character(len=*), intent(out) :: msg
    ok = .false.
    msg = ''
    if (top < 1) then
      msg = 'SUBPLOT: no MULTIPLOT page is open'
      return
    end if
    if (r < 1 .or. c < 1 .or. rs < 1 .or. cs < 1 .or. &
    &   r + rs - 1 > st(top)%rows .or. c + cs - 1 > st(top)%cols) then
      write(msg, '(A,I0,A,I0,A)') 'SUBPLOT: the region is outside the page (', &
      &   st(top)%rows, ' x ', st(top)%cols, ' cells)'
      return
    end if
    st(top)%slot = [r, c, rs, cs]
    ok = .true.
  end function

  ! The region the analysis now starting fills: the SUBPLOT one if chosen,
  ! else the first cell (by rows) no stored subplot covers.  False when the
  ! page is full.  The region is held until comp_store.
  logical function comp_claim_slot(r, c, rs, cs) result(ok)
    integer, intent(out) :: r, c, rs, cs
    integer :: i, j
    ok = .false.
    r = 0; c = 0; rs = 0; cs = 0
    if (top < 1) return
    associate (s => st(top))
      if (s%slot(1) > 0) then
        s%pending = s%slot
        s%slot = 0
      else
        s%pending = 0
        outer: do i = 1, s%rows
          do j = 1, s%cols
            if (.not. covered(i, j)) then
              s%pending = [i, j, 1, 1]
              exit outer
            end if
          end do
        end do outer
        if (s%pending(1) == 0) return
      end if
      r = s%pending(1); c = s%pending(2); rs = s%pending(3); cs = s%pending(4)
    end associate
    ok = .true.
  contains
    logical function covered(i, j)
      integer, intent(in) :: i, j
      integer :: k
      covered = .false.
      do k = 1, st(top)%page%nChildren
        associate (g => st(top)%page%childRegion(:,k))
          if (i >= g(1) .and. i <= g(1) + g(3) - 1 .and. &
          &   j >= g(2) .and. j <= g(2) + g(4) - 1) covered = .true.
        end associate
      end do
    end function
  end function

  ! The finished multiplot of the analysis that claimed a region.
  subroutine comp_store(mplt)
    type(multiplot), intent(in) :: mplt
    if (top < 1) return
    associate (s => st(top))
      if (s%pending(1) < 1) return
      call s%page%addChild(mplt, s%pending(1), s%pending(2), s%pending(3), s%pending(4))
      s%pending = 0
    end associate
  end subroutine

  function comp_page() result(p)
    type(multiplot), pointer :: p
    p => null()
    if (top > 0) p => st(top)%page
  end function

end module plot_composer
