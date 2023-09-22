!Module for bar chart plotting
module zoa_plot
    use collections
    use GLOBALS
    use, intrinsic :: iso_c_binding, only: c_ptr, c_char, c_int, c_float, c_null_char

    !use handlers

   !use global_widgets
   use plplot
   use plplot_extra
   use gtk_draw_hl
    use cairo, only: cairo_get_target, cairo_image_surface_get_height, &
       & cairo_image_surface_get_width

   implicit none

   ! Private type to keep track of plot data
   type :: plotdata2d
     real, allocatable ::  x(:), y(:)
     integer :: dataColorCode, lineStyleCode

   contains
     procedure, public :: initialize => plotdata2d_init

   end type


   ! Constants for plotting
   integer, parameter :: PL_PLOT_BLACK = 15
   integer, parameter :: PL_PLOT_RED = 2
   integer, parameter :: PL_PLOT_YELLOW = 2
   integer, parameter :: PL_PLOT_GREEN = 3
   integer, parameter :: PL_PLOT_AQUAMARINE = 9
   integer, parameter :: PL_PLOT_PINK = 5
   integer, parameter :: PL_PLOT_WHEAT = 6
   integer, parameter :: PL_PLOT_GREY = 7
   integer, parameter :: PL_PLOT_BROWN = 8
   integer, parameter :: PL_PLOT_BLUE = 4
   integer, parameter :: PL_PLOT_BLUEVIOLET = 10
   integer, parameter :: PL_PLOT_CYAN = 11
   integer, parameter :: PL_PLOT_TURQUOISE = 12
   integer, parameter :: PL_PLOT_MAGENTA = 13
   integer, parameter :: PL_PLOT_SALMON = 14
   integer, parameter :: PL_PLOT_WHITE = 0

type :: zoaplot

    type(c_ptr) :: area

    real, allocatable ::  x(:), y(:)
    character(len=100) :: title = 'untitled'
    character(len=100) :: xlabel = ' x '
    character(len=100) :: ylabel = ' y '
    character(len=20)  :: labelFontColor = "BLACK"
    character(len=20)  :: xPlotCode, yPlotCode 
    logical :: useGridLines = .TRUE.
    !character(len=20)  :: labelDataColor = "BLACK"
    integer :: dataColorCode = 0 !  See PL_PLOT paramaters for decoding
    integer :: numSeries = 0
    type(plotdata2d), dimension(9) :: plotdatalist


contains
    !procedure, public :: initialize
    procedure, public :: initialize => zp_init
    procedure, public, pass(self) :: drawPlot
    procedure, public, pass(self) :: setLabelFont
    procedure, private, pass(self) :: getAxesLimits
    procedure, private, pass(self) :: getLabelFontCode
    procedure, public, pass(self) :: setDataColorCode
    procedure, public, pass(self) :: setLineStyleCode
    procedure, public, pass(self) :: addXYPlot
    procedure, public, pass(self) :: updatePlotData
    procedure, private, pass(self) :: buildPlotCode


end type

!TODO:  Fold this into zoaplot by specifying barchart to get rid of this
!separate type
type, extends(zoaplot) :: barchart

    !integer, allocatable :: surface(:)

    !character(kind=plchar_vector) :: title, xlabel, ylabel
    !character(len=100) :: ylabel
    !character(len=100) :: title = 'untitled'
    !character(len=100) :: xlabel = 'x'
    !character(len=100) :: ylabel = 'y'
contains

    procedure, public :: initialize => bc_init
    procedure, public :: drawPlot => bc_drawPlot

    !procedure, private :: barChartBox


end type barchart

  type :: multiplot
        !> The collection of plot objects.
        type(list) :: m_plots
        !> The number of rows of plots.
        integer :: m_rows = 0
        !> The number of columns of plots.
        integer :: m_cols = 0
        !> The page title.
        character(len = 100) :: m_title
        !> Has a title?
        logical :: m_hasTitle = .false.
        type(c_ptr) :: area = c_null_ptr
        type(c_ptr) :: cc = c_null_ptr
        !> The BNUPLOT terminal object to target.
        !class(terminal), pointer :: m_terminal => null()
    contains
        !final :: mp_clean

        procedure, public :: initialize => mp_init
        !procedure, public :: get_row_count => mp_get_rows
        !procedure, public :: get_column_count => mp_get_cols
        !procedure, public :: get_plot_count => mp_get_count
        !procedure, public :: get_title => mp_get_title
        !procedure, public :: set_title => mp_set_title
        procedure, public :: draw => mp_draw
        procedure, public :: get => mp_get
        procedure, public :: set => mp_set

        !procedure, public :: is_title_defined => mp_has_title
        !procedure, public :: get_font_name => mp_get_font
        !procedure, public :: set_font_name => mp_set_font
        !procedure, public :: get_font_size => mp_get_font_size
        !procedure, public :: set_font_size => mp_set_font_size
    end type



! interface zoaplot
!     module procedure :: initialize
! end interface zoaplot

contains
!
! Candidate for submodule for multiplot
    subroutine mp_init(self, area, m, n)
        class(multiplot), intent(inout) :: self
        type(c_ptr) :: area
        integer, intent(in) :: m,n
        integer :: i
        !real, allocatable :: xin(:), yin(:)
        !integer :: arraysize

        ! TODO - Move this to common function
        !arraysize = size(xin)

        !allocate(self%x(arraysize))
        !allocate(self%y(arraysize))

        self%area = area
        !PRINT *, "AREA PTR IS ", self%area

        self%m_rows = m
        self%m_cols = n

        do i = 1, m * n
            call self%m_plots%push(i)
        end do

    end subroutine


    subroutine mp_set(self, i, j, plotter)
        ! Arguments
        class(multiplot), intent(inout) :: self
        integer, intent(in) :: i, j
        class(zoaplot), intent(in) :: plotter

        ! Local Variables
        integer :: ind

        PRINT *, "REACHED MP_SET!"

        ! Process
        ind = self%m_rows * (j - 1) + i
        call self%m_plots%set(ind, plotter)
        PRINT *, "END MP_SET"
    end subroutine

    function mp_get(self,i,j) result(plotter)
        ! Arguments
        implicit none

        class(multiplot), intent(in) :: self
        integer, intent(in) :: i, j
        class(zoaplot), pointer :: plotter

        ! Local Variables
        class(*), pointer :: item
        integer :: ind

        ! Process
        ind = self%m_rows * (j - 1) + i
        item => self%m_plots%get(ind)
        select type (item)
        class is (zoaplot)
            PRINT *, "Found Zoaplot obj!"
            plotter => item
        class default
            nullify(plotter)
        end select

    end function

    subroutine mp_draw(self)


        class(multiplot):: self

        type(c_ptr)  :: cc, cs, isurface
        character(len=20) :: string
        character(len=25) :: geometry
        integer :: m, n
        integer :: plparseopts_rc
        integer :: plsetopt_rc
        class(zoaplot), pointer :: plotter




        ! Define colour map 0 to match the "GRAFFER" colour table in
        ! place of the PLPLOT default.
        ! Note that cmap(0) is the background and is white here.
        ! TODO : put this in zoaplot, and map the parameter labels to these values
        integer, parameter, dimension(16) :: rval = (/255, 0, 255, &
             & 0, 0, 0, 255, 255, 255, 127, 0, 0, 127, 255, 85, 170/),&
             & gval = (/ 255, 0, 0, 255, 0, 255, 0, 255, 127, 255, 255, 127,&
             & 0, 0, 85, 170/), &
             & bval = (/ 255, 0, 0, 0, 255, 255, 255, 0, 0, 0, 127, 255, 255,&
             & 127, 85, 170/)

        PRINT *, "Starting mp_plot routine"
        PRINT *, "DRAWING AREA PTR IS ", LOC(self%area)



        ! Added if statement 6/5 b/c I don't think this is adding any value when area is undefined
        isurface = c_null_ptr
        if (c_associated(self%area)) then

          isurface = g_object_get_data(self%area, "backing-surface")
          PRINT *, "isurface in mp_draw is ", LOC(isurface)


        if (.not. c_associated(isurface)) then
           PRINT *, "mp_draw :: Backing surface is NULL"
          isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200, 500)
          isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
          call g_object_set_data(self%area, "backing-surface", isurface)
        end if
        end if

        PRINT *, "self%area is now", LOC(self%area)

        cc = hl_gtk_drawing_area_cairo_new(self%area)
        cs = cairo_get_target(cc)

        call logger%logText("AFTER CS DEFINED")
        !  Initialize plplot
        call plscmap0(rval, gval, bval)
        call logger%logText("AFTER PLPLOT MAP CALLED")

        !call plscolbg(1,1,1)
        call plsdev("extcairo")

        ! By default the "extcairo" driver does not reset the background
        ! This is equivalent to the command line option "-drvopt set_background=1"
        plsetopt_rc = plsetopt("drvopt", "set_background=1")
        call logger%logText("AFTER PLPLOT Background set")
        if (plsetopt_rc .ne. 0) stop "plsetopt error"

    !      Process command-line arguments
    !plparseopts(PL_PARSE_FULL)
    !if(plparseopts_rc .ne. 0) stop "plparseopts error"

        !      Load color palettes
        !call plspal0('cmap0_white_bg.pal')

        ! The "extcairo" device doesn't read the size from the context.
        write(geometry, "(I0,'x',I0)") cairo_image_surface_get_width(cs), &
             & cairo_image_surface_get_height(cs)
        call logger%logText('GEOMETRY IS '//geometry)
        plsetopt_rc = plsetopt( 'geometry', geometry)
        call logger%logText("AFTER plsetopt")
        if (plsetopt_rc .ne. 0) stop "plsetopt error"

        !call plinit()
        !  Divide page into 2x2 plots
        call plstar(self%m_rows,self%m_cols)

        call pl_cmd(PLESC_DEVINIT, cc)

        !call pladv(0)

        !call plvsta
        !call plcol0(15)

        do m=1, self%m_rows
          do n=1, self%m_cols
            call pladv(0)


            !Selects the largest viewport within the subpage that leaves a
            !standard margin (left-hand margin of eight character heights,
            !and a margin around the other three sides of five character heights).
            call plvsta
            plotter => self%get(m,n)
            call logger%logText("Starting to Draw Plot from Plotter")
            call plotter%drawPlot


          end do
        end do





        !    Don't forget to call PLEND to finish off!
        call plend
        call gtk_widget_queue_draw(self%area)
        call hl_gtk_drawing_area_cairo_destroy(self%cc)

        end subroutine


    subroutine plotdata2d_init(self, x, y, dataColorCode, lineStyleCode)
      class(plotdata2d), intent(inout) :: self
      real, intent(in) :: x(:), y(:)
      integer, optional, intent(in) :: dataColorCode, lineStyleCode
      !character(len=40), optional :: dataColor, lineStyle

      self%x = x
      self%y = y

      if (present(dataColorCode)) then
         self%dataColorCode = dataColorCode
      else
         self%dataColorCode = PL_PLOT_RED
      end if

      if (present(lineStyleCode)) then
         self%lineStyleCode = lineStyleCode
      else
         self%lineStyleCode = 1
      end if

    end subroutine

! TODO - How to combine this with barchart_init?
    subroutine zp_init(self, area, x, y, xlabel, ylabel, title)
      !import zoa_plot
      class(zoaplot), intent(inout) :: self
      type(c_ptr), intent(in) :: area
      real ::  x(:), y(:)
      character(len=*), optional :: xlabel, ylabel, title
      integer :: arraysize, i
      !type(plotdata2d) :: zpinitdata

      arraysize = size(x)

      allocate(self%x(arraysize))
      allocate(self%y(arraysize))
      PRINT *, "ARRAY SIZE IS ", arraysize


      self%area = area
      !Print *, "About to crash before x/y assignment?"

      !if(allocated(self%x)) PRINT *, "X ALLOCATED BEFORE ASSIGNMENT!"

      self%x = x
      self%y = y

      !if(allocated(self%x)) PRINT *, "X ALLOCATED AFTER ASSIGNMENT!"



      !self % title = trim("untitled")
      !self % xlabel = trim("           x axis")

      !self % ylabel = "y axis"

      self % labelFontColor = trim("BLACK")
      self % dataColorCode = PL_PLOT_RED


    if (present(title)) then
       self%title = title
    else
       self%title = 'untitled'
    end if


    if (present(xlabel)) then
       self%xlabel = xlabel
    else
       self%xlabel = 'x'
    end if

    if (present(ylabel)) then
       self%ylabel = ylabel
    else
       self%ylabel = 'y'
    end if

    ! PRINT *, "About to crash?"
    !   do i = 1, 9
    !       call self%plotdatalist%push(i)
    !   end do

    self%numSeries = self%numSeries + 1

    ! Comment out 6/5 to try and debug a crash
    call self%plotdatalist(self%numSeries)%initialize(x,y)

    ! call zpinitdata%initialize(x,y)

    ! call self%plotdatalist%set(self%numSeries, zpinitdata)

  end subroutine

!! Canidate for dubmodule for zoaplot
    subroutine bc_init(self, area, x, y, xlabel, ylabel, title)
      !import zoa_plot
      class(barchart), intent(inout) :: self
      type(c_ptr), intent(in) :: area
      real ::  x(:), y(:)
      character(len=*), optional :: xlabel, ylabel, title
      integer :: arraysize, i

      arraysize = size(x)

      allocate(self%x(arraysize))
      allocate(self%y(arraysize))
 
      self%area = area
      self%x = x
      self%y = y
      


      !self % title = trim("untitled")
      !self % xlabel = trim("           x axis")

      !self % ylabel = "y axis"

      self % labelFontColor = trim("BLACK")
      self % dataColorCode = PL_PLOT_RED

    if (present(title)) then
       self%title = title
    else
       self%title = 'untitled'
    end if


    if (present(xlabel)) then
       self%xlabel = xlabel
    else
       self%xlabel = 'x'
    end if

    if (present(ylabel)) then
       self%ylabel = ylabel
    else
       self%ylabel = 'y'
    end if

    ! PRINT *, "About to crash?"
    !   do i = 1, 9
    !       call self%plotdatalist%push(i)
    !   end do
    self%numSeries = self%numSeries + 1
    !call self%plotdatalist(self%numSeries)%initialize(x,y)


  end subroutine

  subroutine bc_drawPlot(self)
    class(barchart), intent(in) :: self
    integer :: i
    real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax
    type(c_ptr) :: isurface




            call getAxesLimits(self, xmin, xmax, ymin, ymax)
            call plwind(xmin, xmax, ymin, ymax)
        isurface = g_object_get_data(self%area, "backing-surface")
    ! Create the backing surface

    !isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200, 500)
    !isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
    !call g_object_set_data(self%area, "backing-surface", isurface)
        PRINT *, "isurface in mp_draw is ", LOC(isurface)
        if (.not. c_associated(isurface)) then
           PRINT *, "mp_draw :: Backing surface is NULL"
          isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200, 500)
          isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
          call g_object_set_data(self%area, "backing-surface", isurface)
        end if
            call self%buildPlotCode()
            call plbox(trim(self%xPlotCode),0.0_pl_test_flt, 0, trim(self%yPlotCode), 0.0_pl_test_flt, 0 ) 
            !call plbox( 'bcgnt', 0.0_pl_test_flt, 0, 'bcgntv', 0.0_pl_test_flt, 0 )
            call plcol0(getLabelFontCode(self))
            call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)
            !call plscmap1l(.true.,pos,red,green,blue)

            do i = 1, size(self%x)
                !       call plcol0(i + 1)
                !call plcol1(real(i,kind=pl_test_flt)/9.0_pl_test_flt)
                call plcol0(self%dataColorCode)
                !PRINT *, "Data Color Code is ", self%dataColorCode
                !call plcol1(real(1,kind=pl_test_flt))
                !call plcol1(1.0_pl_test_fit)
                call plpsty(0)
                !call plfbox( 1980._pl_test_flt+i, y(i+1) )
                call barChartBox(self%x(i), self%y(i) )
          enddo



  end subroutine


    subroutine drawPlot(self)
        class(zoaplot), intent(in) :: self

    type(c_ptr)  :: cc, cs, isurface
    character(len=20) :: string
    character(len=25) :: geometry
    integer :: i
    integer :: plparseopts_rc
    integer :: plsetopt_rc

    real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax


    ! Getter for dataSeries - separate routine?
    class(plotdata2d), pointer :: dataSeries
    class(*), pointer :: item

    !call plvsta
    !call plwind( 1980._pl_test_flt, 1990._pl_test_flt, -15._pl_test_flt, 40._pl_test_flt )

    call getAxesLimits(self, xmin, xmax, ymin, ymax)
    PRINT *, "Axes Limits ymin is ", ymin
    PRINT *, "Axes Limits ymaxs is ", ymax
    call plwind(xmin, xmax, ymin, ymax)

        isurface = c_null_ptr
        if (c_associated(self%area)) then
          isurface = g_object_get_data(self%area, "backing-surface")
          PRINT *, "self%area is ", LOC(self%area)

        PRINT *, "isurface in mp_draw is ", LOC(isurface)
        if (.not. c_associated(isurface)) then
           PRINT *, "mp_draw :: Backing surface is NULL"
          isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200, 500)
          isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
          call g_object_set_data(self%area, "backing-surface", isurface)
        end if

        end if
    ! Create the backing surface

    !isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200, 500)
    !isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
    !call g_object_set_data(self%area, "backing-surface", isurface)


        !PRINT *, "self%area is now ", self%area

    call plbox( 'bcgnt', 0.0_pl_test_flt, 0, 'bcgntv', 0.0_pl_test_flt, 0 )

    call plcol0(getLabelFontCode(self))

    call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)



    !call plcol0(self%dataColorCode)
    !call plpsty(0)
    !call plline(self%x,self%y)
    !call plcol0(getLabelFontCode(self))



        ! Process
        ! item => self%plotdatalist%get(self%numSeries)
        ! select type (item)
        ! class is (plotdata2d)
        !     dataSeries => item
        !     PRINT *, "dataSeries x is ", dataSeries%x
        ! end select

    do i=1,self%numSeries
      call plcol0(self%plotDataList(i)%dataColorCode)
      ! PL PLOT Area fill
      call plpsty(0)
      ! PL PLOT Line Style
      if (self%plotdatalist(i)%lineStyleCode > -1) THEN
      call pllsty(self%plotdatalist(i)%lineStyleCode)
      call plline(self%plotdatalist(i)%x, &
      &           self%plotdatalist(i)%y)
    else ! Scatter Plot
      call plstring(self%plotdatalist(i)%x, &
      &           self%plotdatalist(i)%y, '*')
    end if
      call plcol0(getLabelFontCode(self))
    end do

    !PRINT *, "ydata is ", self%plotdatalist(1)%y

    end subroutine drawPlot

    subroutine addXYPlot(self, X, Y)

        implicit none
        class(zoaplot), intent(inout) :: self
        real, dimension(:), intent(in) :: X,Y

        self%numSeries = self%numSeries + 1
        call self%plotdatalist(self%numSeries)%initialize(X,Y)

  end subroutine

  subroutine updatePlotData(self, x, y, seriesNum)
    implicit none
    class(zoaplot) :: self
    real :: x(:), y(:)
    integer :: seriesNum

    deallocate(self%x)
    deallocate(self%y)
    allocate(self%x(size(x)))
    allocate(self%y(size(y)))
    self%x = x
    self%y = y

    deallocate(self%plotdatalist(seriesNum)%x)
    deallocate(self%plotdatalist(seriesNum)%y)
    allocate(self%plotdatalist(seriesNum)%x(size(x)))
    allocate(self%plotdatalist(seriesNum)%y(size(y)))

    self%plotdatalist(seriesNum)%x = x
    self%plotdatalist(seriesNum)%y = y

  end subroutine

  ! As needed, add more options to this 
  subroutine buildPlotCode(self)
    class(zoaplot) :: self

    if (self%useGridLines) then
      self%xPlotCode = 'bcgnt'
      self%yPlotCode = 'bcgntv'
    else
      self%xPlotCode = 'bcgnt'
      self%yPlotCode = 'bcgntv'
    end if


  end subroutine

!
!     subroutine kdp_drawPlot(self)
!
!       use kdp_plot_impl
!         class(zoaplot), intent(in) :: self
!
!     type(c_ptr)  :: cc, cs, isurface
!     character(len=20) :: string
!     character(len=25) :: geometry
!
!     real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax
!
!     ! Getter for dataSeries - separate routine?
!     class(plotdata2d), pointer :: dataSeries
!     class(*), pointer :: item
!
!       CHARACTER UNN*9,DUNN*12,NNTT1*80,BLNOTE*80,BL20*20,CRANGE*8,B*80,DTY*10,TMY*8,LABX*40,LABY*40
!
!       REAL*8 WOR1(0:50),WOR2(0:50),RANGE,FACTY,ORI,DTA11(0:50),DTA22(0:50),DDTA(0:50),ADTA(0:50)
!
!       REAL LLIM,ULIM,UFLIM,LFLIM,DELX1,FLDAN(0:50)
!
!       COMMON/FIFI/FLDAN
!
!       REAL X1(1:51),Y(1:51),XRAN1,YRAN,YMINJK,XMINJK1,XMAXJK1,YMAXJK,X2(1:51),XMINJK2,XMAXJK2,XRAN2,XRAN,XMAXJK,XMINJK,X(1:51)
!
!       INTEGER NX,NY,COLPAS,MYJK,DFLAG,I,PNTNUM,NT1ANG,NT1SIZ
!
!       LOGICAL :: COMMANDINFOCHECK, STRINGINPUTCHECK, CHECKMAXFLOATINPUTS
!
!       COMMON/NUMPNT/PNTNUM,ORI,FACTY
!
!       LOGICAL ASTEXT,FLDEXT,DISEXT,FDISEXT
!       COMMON/FIELDEXT/ASTEXT,FLDEXT,DISEXT,FDISEXT
!
!       COMMON/ABSSS/WOR1,WOR2,DTA11,DTA22,DDTA,ADTA
!
!         INCLUDE 'DATMAI.INC'
!         INCLUDE 'DATLEN.INC'
!         INCLUDE 'DATHGR.INC'
!
!       MYJK=0
!
!       IF(DF2.EQ.1.OR.DF2.EQ.0.AND.W2.EQ.0.0D0) DFLAG=0
!       IF(DF2.EQ.0.AND.W2.NE.0.0D0) DFLAG=1
!
! !     GENERATE GRAPHIC
! !     DO A PLOT NEW
!       DEVTYP=1
!       LOOKY=0.0D0
!       LOOKX=-1.0D0
!       LOOKZ=0.0D0
!       CALL PLTDEV
!       GRASET=.TRUE.
!       PLEXIS=.TRUE.
! !     SET LETTER SIZE AND ANGLE
!       BL20='                    '
!       BLNOTE=BL20//BL20//BL20//BL20
!                         NT1SIZ=1
!                         NT1ANG=0
!       PRINT *, "zoa-plot NEUTTOTAL IS ", NEUTTOTAL
!       CALL MY_SETCHARASPECT(1.5,1.5)
!       PRINT *, "zoa-plot NEUTTOTAL IS ", NEUTTOTAL
! !     LIFT PEN, MOVE TO FRAME START
! !
!       CALL PLOTBOX
! !
!       COLPAS=COLLBL
!       CALL MY_COLTYP(COLPAS)
! !     DO THE PLOTTING OF THE LENS IDENTIFIER
! !     AT X=200, Y=500
!       NT1SIZ=1
!       NT1ANG=0
!       IF(STMPT) CALL MYTIME(TMY)
!       IF(STMPD) CALL MYDATE(DTY)
!         IF(.NOT.STMPT.AND..NOT.STMPD) NNTT1=LI
!         IF(STMPT.AND.STMPD) NNTT1=TMY//' '//DTY//LI
!         IF(STMPT.AND..NOT.STMPD) NNTT1=TMY//' '//LI
!         IF(.NOT.STMPT.AND.STMPD) NNTT1=DTY//LI
!                 IF(NNTT1.NE.BLNOTE) THEN
!         CALL MY_JUSTSTRING(200,500,NNTT1(1:80),NT1ANG,NT1SIZ,3,3)
!                         ELSE
! !     LI BLANK, NOT ACTION
!                         END IF
!
!       RANGE=ORI
!       WRITE(B,101) RANGE
!       READ(B,200) CRANGE
! 101   FORMAT(F8.3)
! 200   FORMAT(A8)
!       NNTT1='ORIENTATION ANGLE = '
! !
!       CALL MY_JUSTSTRING(200,250,NNTT1(1:20),NT1ANG,NT1SIZ,3,3)
!
!       NNTT1=CRANGE//' '//'DEGREES'
! !
!       CALL MY_JUSTSTRING(1700,250,NNTT1(1:16),NT1ANG,NT1SIZ,3,3)
!
! !     HERE GO THE REAL PLOTTING COMMANDS
! !     **********************************
! !     THE FUNCTIONAL VALUES ARE IN THE ARRAYS DDTA
! !     THE INDEPENDENT VARIABLE IS THE LIST OF
! !     RADIAL FIELD POSITIONS WHICH WILL BE GENERATED AND PLACED
! !     IN THE 'Y' ARRAY
! !     LOAD Y
!       PNTNUM = 9
!
!        XMINJK1=1.0E20
!        XMAXJK1=-1.0E20
!
!       YMAXJK=self%y(PNTNUM)
!       YMINJK=self%y(1)
! !
!       YRAN=ABS(YMAXJK-YMINJK)
!       XRAN1=ABS(XMAXJK1-XMINJK1)
!           XRAN=XRAN1
!       IF(DF1.EQ.0) XRAN=ABS(W1)
!           XMINJK=XMINJK1
!           XMAXJK=XMAXJK1
!
!
!       CALL PRINTNEUTARRAY
! !     PLOT THE AXES AND TICS
!       call plot_axes(700,1000,.1,.1,0)
!       !CALL PLOTAXES2
!       PRINT *, "AFTER PLOTAXES2 PRINTING NEUTARRAY"
!       CALL PRINTNEUTARRAY
!
! !     COMMENT OUT BELOW FOR DEBUGGING
! !
! ! !     PLOT THE HORIZONTAL AXIS NAME
! !       CALL PLOTHNAME("xlabel",11)
! ! !     PLOT THE VERTICAL AXIS NAME
! !       CALL PLOTVNAME("ylabel",11)
! !
! ! !     PLOT THE VALUES FOR THE TIC MARKS
! !       DELX1=(YMAXJK)/2.0
! !       LLIM=0.0
! !       CALL PLOTVVAL2(YMINJK,DELX1)
! !       XMINJK = self%x(1)
! !       XMAXJK = self%x(9)
! !       !CALL PLOTUXAXIS(XMINJK,XMAXJK)
! !       ! IF(DABS(DBLE(XMAXJK)).GT.DABS(DBLE(XMINJK))) THEN
! !       ! XMAXJK=DABS(DBLE(XMAXJK))
! !       ! XMINJK=-DABS(DBLE(XMAXJK))
! !       !                  ELSE
! !       ! XMAXJK=DABS(DBLE(XMINJK))
! !       ! XMINJK=-DABS(DBLE(XMINJK))
! !       !                  END IF
! !       DELX1=(XMAXJK-XMINJK)/4.0
! !
! !       ! For some reason this call isn't doing anything.
! !       CALL PLOTHVAL2(XMINJK+1,YMAXJK/2.0)
! !
! !       LLIM=XMINJK
! !       ULIM=XMAXJK
! !       UFLIM=YMAXJK
! !       LFLIM=0.0
! !       PRINT *, "XMINJK is ", XMINJK
! !       PRINT *, "XMAXJK is ", XMAXJK
! !       PRINT *, "YMINJK is ", YMINJK
! !       PRINT *, "YMAXJK is ", YMAXJK
! !
! !       call logger%logText('About to call plotfunc4')
! !       CALL PLOTFUNC4(self%x,self%y,10,XMINJK,XMAXJK,YMINJK,YMAXJK,0)
! !       !CALL PLOTFUNC1(self%x,self%y,10,XMINJK,XMAXJK,YMINJK,YMAXJK,0)
! !       IF(.NOT.PLEXIS) PLEXIS=.TRUE.
!       call logger%logText('Done with plotfunc4')
!
!
! !     **********************************
! !     DATA PLOTTING DONE
!
!       IF(DFLAG.EQ.0) THEN
!                         SAVE_KDP(1)=SAVEINPT(1)
!                         INPUT='DRAW'
!                         CALL PROCES
!                         REST_KDP(1)=RESTINPT(1)
!                        END IF
!                        RETURN
!
!
!     end subroutine


    subroutine setLabelFont(self, desiredColor)

    class(zoaplot), intent(inout) :: self
    character(len=20), intent(in) :: desiredColor

    !if trim(desiredColor) == "RED"
    self % labelFontColor = trim(desiredColor)

    end subroutine setLabelFont

   subroutine setLineStyleCode(self, code)
      class(zoaplot), intent(inout) :: self
      integer, intent(in) :: code
      self%plotdatalist(self%numSeries)%lineStyleCode = code
   end subroutine

    subroutine setDataColorCode(self, code)
      class(zoaplot), intent(inout) :: self
      integer, intent(in) :: code
      self%dataColorCode = code
      self%plotdatalist(self%numSeries)%dataColorCode = code
    end subroutine

    function getLabelFontCode(self) result(r)

       ! Currently this function does not work correctly
       ! The string in labelFontColor is not being stored
       ! correctly

       class(zoaplot), intent(in) :: self
       integer :: r

       r = 1

       if (trim(self % labelFontColor) == trim("BLACK")) then
           r = 1
       else if (trim(self % labelFontColor) == trim("RED")) then
           r = 2
       end if
       print *, "labelFontColor ", trim(self%labelFontColor)
    end function getLabelFontCode

    subroutine getAxesLimits(self, xmin, xmax, ymin, ymax)

    class(zoaplot), intent(in) :: self
    real(kind=pl_test_flt), intent(inout) :: xmin, xmax, ymin, ymax
    real(kind=pl_test_flt) :: yrng, xrng

    xmin = minval(self%x)
    xmax = maxval(self%x)
    ymin = minval(self%y)
    ymax = maxval(self%y)

    yrng = (ymax-ymin)
    xrng = (xmax-xmin)

    ymax = ymax + .05*yrng
    ymin = ymin - .05*yrng

    xmax = xmax + .05*xrng
    xmin = xmin - .05*xrng

    !if (ymax.LT.1) ymax = 1.05*ymax


    end subroutine getAxesLimits

    subroutine barChartBox(x0, y0)
        !real(kind=pl_test_flt) x0, y0, x(4), y(4)
        real :: x0, y0, x(5), y(5)



        x(1) = x0+0.25
        y(1) = y0
        x(2) = x0-0.25
        y(2) = y0
        !x(3) = x0+1._pl_test_flt
        x(3) = x0-0.25
        y(3) = 0.0
        !x(4) = x0+1._pl_test_flt
        x(4) = x0+0.25
        y(4) = 0.0
        x(5) = x0+0.25
        y(5) = y0
        call plfill(x, y)
        call plcol0(1)
        call pllsty(1)
        call plline(x, y)
    end subroutine barChartBox

end module zoa_plot
