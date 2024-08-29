!Module for bar chart plotting

! THis needs some cleanup but no major surgery?
! Eg if there are types of zoatab that contain plots then I think this can be used as is


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

   type :: plotdata3d
     real(kind=pl_test_flt), allocatable :: x(:), y(:), z(:)
     integer :: dataColorCode, lineStyleCode
     integer :: xpts, ypts

     contains
     procedure, public :: initialize => plotdata3d_init
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

   integer, parameter :: POS_UPPER_RIGHT = 1

! For each plot type:  eg barchart, linechart, 3d surface, shades
! Have a datatype that contains the data to plot and any unique settings
! drawplot would draw upon this resource

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

    logical :: useLegend
    ! AFAIK have to make character array a a fixed size
    character(len=30)  :: legendNames(16)
    integer :: numLegendNames

    ! In case user wants to add text to plot
    logical :: addTextToPlot
    integer :: numTextLabels
    character(len=1024), dimension(16) :: textLabels
    integer, dimension(16) :: textLabelPositions

    ! Scale related
    logical :: manualYScale
    real(kind=pl_test_flt) ::  yScale 



contains
    !procedure, public :: initialize
    procedure, public :: initialize => zp_init
    procedure, public, pass(self) :: drawPlot
    procedure, public, pass(self) :: addLegend
    procedure, public, pass(self) :: drawLegend  
    procedure, public, pass(self) :: drawBottomRightLegend  
    procedure, public, pass(self) :: setLabelFont
    procedure, private, pass(self) :: getAxesLimits
    procedure, private, pass(self) :: getLabelFontCode
    procedure, public, pass(self) :: setDataColorCode
    procedure, public, pass(self) :: setLineStyleCode
    procedure, public, pass(self) :: addXYPlot
    procedure, public, pass(self) :: updatePlotData
    procedure, private, pass(self) :: buildPlotCode
    procedure, private, pass(self) :: checkBackingSurface
    procedure :: addText
    procedure :: setYScale


end type

type, extends(zoaplot) :: zoaPlot3d
  type(plotdata3d), dimension(9) :: plotdatalist3d
contains
procedure, public ::  init3d => plot3d_initialize
procedure, public :: drawPlot => drawPlot_plot3d

end type


type, extends(zoaplot) :: zoaPlotImg
  type(plotdata3d), dimension(9) :: plotdatalist3d
contains
procedure, public ::  init3d => plotImg_initialize
procedure, public :: drawPlot => drawPlot_plotImg

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


        ! Bottom Panel Vars
        logical :: hasBottomPanel
        character(len=80) :: bottomPanelBigLabel
        character(len=160) :: bottomPanelLittleLabel
        character(len=80) :: bottomPanelLegend
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

        procedure :: addBottomPanel

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

        self%hasBottomPanel = .FALSE.



    end subroutine


    subroutine mp_set(self, i, j, plotter)
        ! Arguments
        class(multiplot), intent(inout) :: self
        integer, intent(in) :: i, j
        class(zoaplot), intent(in) :: plotter

        ! Local Variables
        integer :: ind

        !PRINT *, "REACHED MP_SET!"

        ! Process
        ind = self%m_rows * (j - 1) + i
        call self%m_plots%set(ind, plotter)
        !PRINT *, "END MP_SET"
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
            !PRINT *, "Found Zoaplot obj!"
            plotter => item
        class default
            nullify(plotter)
        end select

    end function

    subroutine addBottomPanel(self, strBig, strSmall, strLegend)
      class(multiplot) :: self
      character(len=*), intent(in) :: strBig, strSmall, strLegend

      self%hasBottomPanel = .TRUE.
      self%bottomPanelBigLabel = strBig
      self%bottomPanelLittleLabel = strSmall
      self%bottomPanelLegend = strLegend


    end subroutine

    ! THIS SUB NEEDS REFACTORING!!
    subroutine mp_draw(self)


        class(multiplot):: self

        type(c_ptr)  :: cc, cs, isurface
        character(len=20) :: string
        character(len=25) :: geometry
        integer :: m, n
        integer :: plparseopts_rc
        integer :: plsetopt_rc
        class(zoaplot), pointer :: plotter
        ! TEMP
        real(kind=pl_test_flt)  :: legend_width, legend_height




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
          call LogTermFOR("Loose pointer in mp_draw")
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
        !call plstar(self%m_rows,self%m_cols)
        ! this is plstar(x,y) which is why columns go first then rows
        
        

        !call plstar(self%m_cols,self%m_rows)
        
        ! To get the title/legend I want, manually set up subpages

        if (self%hasBottomPanel) then

        
        call plstar(1_c_int,1_c_int)
 
        call pl_cmd(PLESC_DEVINIT, cc)

        call pladv(0) ! Comment this out when using the subpage way!
        !

        !call plvsta
        !call plcol0(15)

        call plschr(0.0, 0.75)
        do m=1, self%m_rows
          do n=1, self%m_cols
            !call pladv(0)

            plotter => self%get(m,n)
            ! FOr testing, brute force this
  
              select case (m)

             
              case (1)
                if (n==1) call plvpor(0.15, .45, 0.75, .95)
                if (n==2) call plvpor(.6, .9, 0.75, .95)
              case (2)
                if (n==1) call plvpor(.15, .45, 0.45, .65)
                if (n==2) call plvpor(.6, .9, 0.45, .65)         
              case (3)
                if (n==1) call plvpor(.15, .45, 0.2, .4)
                if (n==2) call plvpor(.6, .9, 0.2, .4)                                
              end select                
          
            call logger%logText("Starting to Draw Plot from Plotter")

            
            call plotter%drawPlot


          end do
        end do

        call plvpor(0.0, 1.0, 0.00, 0.2)  
        !call plgvpw (	p_xmin,p_xmax,p_ymin,p_ymax)
        call plwind(0.0, 1.0, 0.05, 0.2)
        call pljoin(0.0, 0.1, 1.0, 0.1)
        call pljoin(0.6, 0.1, .6, 0.00)
        call pljoin(0.0, .07, 0.6, 0.07)
        call plschr(0.0, 0.75)
        call plptex(0.3, .09, 0.0, 0.0, 0.5, trim(self%bottomPanelBigLabel)//c_null_char)
        call plschr(0.0, 0.55)
        call plptex(0.3, .074, 0.0, 0.0, 0.5, trim(self%bottomPanelLittleLabel)//c_null_char)

        plotter%numLegendNames = 1
        call plotter%drawBottomRightLegend(trim(self%bottomPanelLegend))

      else
        
        call plstar(self%m_cols,self%m_rows)
        
          call pl_cmd(PLESC_DEVINIT, cc)
       
        do m=1, self%m_rows
          do n=1, self%m_cols
            call pladv(0)

            plotter => self%get(m,n)

             if (plotter%useLegend) then
              !PRINT *, "setting smaller viewport"
              call plvpor(.15, .85, .2, .8)
        
            else
            !Selects the largest viewport within the subpage that leaves a
            !standard margin (left-hand margin of eight character heights,
            !and a margin around the other three sides of five character heights).              
              call plvsta
            end if            
            call logger%logText("Starting to Draw Plot from Plotter")

            call plotter%drawPlot


          end do
        end do
      end if





        !    Don't forget to call PLEND to finish off!
        call plend
        call gtk_widget_queue_draw(self%area)
        call hl_gtk_drawing_area_cairo_destroy(self%cc)

        end subroutine

        subroutine plotdata3d_init(self, x, y, z, xpts, ypts, dataColorCode, lineStyleCode)
          class(plotdata3d), intent(inout) :: self
          real, intent(in) :: x(:), y(:), z(:)
          integer, intent(in) :: xpts, ypts
          integer, optional, intent(in) :: dataColorCode, lineStyleCode
          !character(len=40), optional :: dataColor, lineStyle
    
          self%x = x
          self%y = y
          self%z = z

          self%xpts = xpts
          self%ypts = ypts
      
    
          if (present(dataColorCode)) then
             self%dataColorCode = dataColorCode
          else
             self%dataColorCode = PL_PLOT_BLACK
          end if
    
          if (present(lineStyleCode)) then
             self%lineStyleCode = lineStyleCode
          else
             self%lineStyleCode = 1
          end if
    
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

    subroutine plot3d_initialize(self, area, x, y, z, xpts, ypts, xlabel, ylabel, title)
      class(zoaPlot3d), intent(inout) :: self
      type(c_ptr), intent(in) :: area
      integer, intent(in) :: xpts, ypts
      real ::  x(:), y(:), z(:)
      character(len=*), optional :: xlabel, ylabel, title
      integer :: arraysize, i
      !type(plotdata2d) :: zpinitdata



      self%area = area

      self % labelFontColor = trim("BLACK")
      self % dataColorCode = PL_PLOT_RED

      self % useLegend  = .FALSE.


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


    self%numSeries = self%numSeries + 1

    call self%plotdatalist3d(self%numSeries)%initialize(x,y,z, xpts, ypts)


  end subroutine

  subroutine plotImg_initialize(self, area, x, y, z, xpts, ypts, xlabel, ylabel, title)
    class(zoaPlotImg), intent(inout) :: self
    type(c_ptr), intent(in) :: area
    integer, intent(in) :: xpts, ypts
    real ::  x(:), y(:), z(:)
    character(len=*), optional :: xlabel, ylabel, title
    integer :: arraysize, i
    !type(plotdata2d) :: zpinitdata



    self%area = area

    self % labelFontColor = trim("BLACK")
    self % dataColorCode = PL_PLOT_BLACK

    self % useLegend  = .FALSE.


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


  self%numSeries = self%numSeries + 1

  call self%plotdatalist3d(self%numSeries)%initialize(x,y,z, xpts, ypts)


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

      self % useLegend  = .FALSE.


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

    self%addTextToPlot = .FALSE.
    self%numTextLabels = 0

    self%manualYScale = .FALSE.

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
       if (c_associated(self%area)) then        
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
      end if
            call self%buildPlotCode()
            call plbox(trim(self%xPlotCode),0.0_pl_test_flt, 0, trim(self%yPlotCode), max(ymax,abs(ymin)), 0 ) 
            !call plbox(trim(self%xPlotCode),0.0_pl_test_flt, 0, trim(self%yPlotCode), 0.0_pl_test_flt, 0 ) 
            
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

    real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax, zmin, zmax, y_abs, y_norm
    real(kind=pl_test_flt) :: defCharHgt, currCharHgt, p_xmin, p_xmax, p_ymin, p_ymax


    ! Getter for dataSeries - separate routine?
    class(plotdata2d), pointer :: dataSeries
    class(*), pointer :: item

    !call plvsta
    !call plwind( 1980._pl_test_flt, 1990._pl_test_flt, -15._pl_test_flt, 40._pl_test_flt )

    call getAxesLimits(self, xmin, xmax, ymin, ymax)
    !if (self%useLegend) call plvpor(.05, .95, 0.2, .8)
    if (self%manualYScale) then 
      call plwind(xmin, xmax, -1.0_pl_test_flt*self%yScale, self%yScale)
      !call plwind(xmin, xmax, -3.5_pl_test_flt, 3.5_pl_test_flt)
    else
    call plwind(xmin, xmax, ymin, ymax)
    end if

        isurface = c_null_ptr
        if (c_associated(self%area)) then
          isurface = g_object_get_data(self%area, "backing-surface")
        if (.not. c_associated(isurface)) then
           call LogTermDebug("Loose pointer in drawPlot") 
          isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200, 500)
          isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
          call g_object_set_data(self%area, "backing-surface", isurface)
        end if

        end if

    ! Micromanage tick intervals if the user selected manual scale
        if (self%manualYScale) then 
          call plsyax(7, 0)
          call plbox( 'bcgnt', 0.0_pl_test_flt, 0, 'bcgntv', self%yScale/5.0_pl_test_flt, 0)
        else
          call plbox( 'bcgnt', 0.0_pl_test_flt, 0, 'bcgntv', 0.0_pl_test_flt, 0 )
        end if        

    call plcol0(getLabelFontCode(self))

    call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)

    if (self%addTextToPlot) then
      call plgchr(defCharHgt, currCharHgt)
      call plgvpd(p_xmin, p_xmax, p_ymin, p_ymax)
      print *, 'vp p_xmin ', p_xmin
      print *, 'vp p_xmax ', p_xmax
      print *, 'vp p_ymin ', p_ymin
      print *, 'vp p_ymax ', p_ymax   
      y_norm = p_ymax-p_ymin   
      call plgspa(p_xmin, p_xmax, p_ymin, p_ymax)
      print *, 'mm p_xmin ', p_xmin
      print *, 'mm p_xmax ', p_xmax
      print *, 'mm p_ymin ', p_ymin
      print *, 'mm p_ymax ', p_ymax
      y_abs = p_ymax-p_ymin
      call plgvpw(p_xmin, p_xmax, p_ymin, p_ymax) ! World
      !call plgspa(p_xmin, p_xmax, p_ymin, p_ymax) !mm
      print *, 'defCharHgt is ', defCharHgt
      print *, 'currCharHgt is ', currCharHgt

      print *, 'p_xmin ', p_xmin
      print *, 'p_xmax ', p_xmax
      print *, 'p_ymin ', p_ymin
      print *, 'p_ymax ', p_ymax


      do i=1,self%numTextLabels

        select case (self%textLabelPositions(i))

        case (POS_UPPER_RIGHT)
          !Need to offset this by 1/2 the character height to avoid clipping.  
          !Character height is in absolute coordinates, plptex wants world coordinates.
          !To convert to world coordinates, first get the entire plot size in absolute coordinates (y_abs)
          ! and then get the viewport size in normalized coordinates (y_norm)
          ! This will tell me how big the character is relatie to the entire size of the viewport
          ! Then I need to convert it to world coordniates (p_ymax).  This gets me what I want (ugh..)
          
          call plptex(0.0,real(p_ymax-currCharHgt/(y_norm*y_abs)*p_ymax), 0.0, 0.0, 0.0, trim(self%textLabels(i))//c_null_char)


  
        end select
  

      end do
    end if


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

    if (self%useLegend) call self%drawLegend()

    !PRINT *, "ydata is ", self%plotdatalist(1)%y

    end subroutine drawPlot

    subroutine addText(self, strText, ID_POS)
      class(zoaplot) :: self
      character(len=*) :: strText
      integer, intent(in) :: ID_POS

      self%addTextToPlot = .TRUE.
      self%numTextLabels = self%numTextLabels + 1
      self%textLabels(self%numTextLabels) = strText
      self%textLabelPositions(self%numTextLabels) = ID_POS

    end subroutine

    subroutine setYScale(self, yScale)
      class(zoaplot) :: self
      real(kind=pl_test_flt) :: yScale

      self%manualYScale = .TRUE.
      self%yScale = abs(yScale)

      end subroutine

    subroutine drawPlot_plotImg(self)
      
      class(zoaPlotImg), intent(in) :: self

      !type(c_ptr)  :: cc, cs, isurface
      character(len=20) :: string
      character(len=25) :: geometry
      integer :: i, j
      integer :: plparseopts_rc
      integer :: plsetopt_rc
      integer :: xpts, ypts
      ! TODO Fixt this hack!  should not be hard coded
      !integer, parameter :: xpts = 35
      !integer, parameter :: ypts = 45  
      integer, parameter :: nl = 1024
      real(kind=pl_test_flt), allocatable :: xg(:), yg(:), zg(:,:)
      !real(kind=pl_test_flt) :: yg(ypts)
      !real(kind=pl_test_flt) :: zg(xpts,ypts)
      real(kind=pl_test_flt) :: clev(nl)
      real(kind=pl_test_flt) :: lzmin, lzmax      


      integer, parameter :: nlevel = 10
      real(kind=pl_test_flt)   :: zmin, zmax, step, clevel(nlevel)
      real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax
      real(c_double) :: alt=90._c_double, az=00._c_double

  
      class(*), pointer :: item


      xpts = self%plotdatalist3d(1)%xpts
      ypts = self%plotdatalist3d(1)%ypts
      allocate(xg(xpts))
      allocate(yg(ypts))
      allocate(zg(xpts,ypts))




      !xpts = size(self%plotdatalist3d(1)%x)
      !ypts = size(self%plotdatalist3d(1)%y)

      !allocate(zg(xpts,ypts))

      PRINT *, "drawPlot_plot3d started"
      zmin = minval( self%plotdatalist3d(1)%z )
      zmax = maxval( self%plotdatalist3d(1)%z )

      xmin = minval(self%plotdatalist3d(1)%x)
      ymin = minval(self%plotdatalist3d(1)%y)
      xmax = maxval(self%plotdatalist3d(1)%x)
      ymax = maxval(self%plotdatalist3d(1)%y)
  
      do i=1,xpts
          xg(i) = xmin + (xmax-xmin)*(i-1._pl_test_flt)/(xpts-1._pl_test_flt)
      enddo
      do i=1,ypts
          yg(i) = ymin + (ymax-ymin)*(i-1._pl_test_flt)/(ypts-1._pl_test_flt)
      enddo

      



      step = (zmax-zmin)/(nlevel+1)
      do i = 1, nlevel
        clevel(i) = zmin + step*i
     enddo

      !call getAxesLimits(self, xmin, xmax, ymin, ymax)
      call cmap1_init(0)
      call plwind(xmin, xmax, ymin, ymax)

      PRINT *, "About to check backing surface"

      call self%checkBackingSurface()

      !call plbox( 'bcgnt', 0.0_pl_test_flt, 0, 'bcgntv', 0.0_pl_test_flt, 0 )
      call plcol0(15)
      !call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)

      PRINT *, "About to plot series"
      !call pllightsource(1._pl_test_flt, 1._pl_test_flt, 1._pl_test_flt)
      !call pladv(0)
      !call plclear()
      !call plvpor(0.0_plflt, 1.0_plflt, 0.0_plflt, 0.9_plflt )
      !call plwind(-1.0_plflt, 1.0_plflt, -0.9_plflt, 1.1_plflt )      
      !call pladv(0)
      do i=1,self%numSeries
        !call plclear()
        call plcol0(self%plotDataList3d(i)%dataColorCode)


        call a2mnmx(zg, xpts, ypts, lzmin, lzmax, xpts)

        lzmin = min(lzmin, zmin)
        lzmax = max(lzmax, zmax)

        lzmin = lzmin - 0.01_pl_test_flt
        lzmax = lzmax + 0.01_pl_test_flt
        do j=1,nl
          clev(j) = lzmin + (lzmax-lzmin)/(nl-1._pl_test_flt)*(j-1._pl_test_flt)
      enddo                
        call plenv0(xmin, xmax, ymin, ymax, 2, 0)
        call plcol0(15)
        call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)

        call plimage(reshape(self%plotDataList3d(i)%z, [xpts, ypts]), &
        & xmin, xmax, ymin, ymax, zmin, zmax, &
        & xmin, xmax, ymin, ymax)          
        !& -1._pl_test_flt, 1._pl_test_flt, -1._pl_test_flt, 1._pl_test_flt)  
        
        call plcol0(2)        

      end do


  
      PRINT *, "About to check legend status"
      if (self%useLegend) call self%drawLegend()      
  

    end subroutine

    ! subroutine drawPlot_plot3d(self)
      
    !   class(zoaPlot3d), intent(in) :: self

    !   !type(c_ptr)  :: cc, cs, isurface
    !   character(len=20) :: string
    !   character(len=25) :: geometry
    !   integer :: i, j
    !   integer :: plparseopts_rc
    !   integer :: plsetopt_rc
    !   integer :: xpts, ypts
    !   ! TODO Fixt this hack!  should not be hard coded
    !   !integer, parameter :: xpts = 35
    !   !integer, parameter :: ypts = 45  
    !   integer, parameter :: nl = 1024
    !   real(kind=pl_test_flt), allocatable :: xg(:), yg(:), zg(:,:)
    !   !real(kind=pl_test_flt) :: yg(ypts)
    !   !real(kind=pl_test_flt) :: zg(xpts,ypts)
    !   real(kind=pl_test_flt) :: clev(nl)
    !   real(kind=pl_test_flt) :: lzmin, lzmax      


    !   integer, parameter :: nlevel = 10
    !   real(kind=pl_test_flt)   :: zmin, zmax, step, clevel(nlevel)
    !   real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax
    !   real(c_double) :: alt=90._c_double, az=00._c_double

  
    !   class(*), pointer :: item


    !   xpts = self%plotdatalist3d(1)%xpts
    !   ypts = self%plotdatalist3d(1)%ypts
    !   allocate(xg(xpts))
    !   allocate(yg(ypts))
    !   allocate(zg(xpts,ypts))




    !   !xpts = size(self%plotdatalist3d(1)%x)
    !   !ypts = size(self%plotdatalist3d(1)%y)

    !   !allocate(zg(xpts,ypts))

    !   PRINT *, "drawPlot_plot3d started"
    !   zmin = minval( self%plotdatalist3d(1)%z )
    !   zmax = maxval( self%plotdatalist3d(1)%z )

    !   xmin = minval(self%plotdatalist3d(1)%x)
    !   ymin = minval(self%plotdatalist3d(1)%y)
    !   xmax = maxval(self%plotdatalist3d(1)%x)
    !   ymax = maxval(self%plotdatalist3d(1)%y)
  
    !   do i=1,xpts
    !       xg(i) = xmin + (xmax-xmin)*(i-1._pl_test_flt)/(xpts-1._pl_test_flt)
    !   enddo
    !   do i=1,ypts
    !       yg(i) = ymin + (ymax-ymin)*(i-1._pl_test_flt)/(ypts-1._pl_test_flt)
    !   enddo

      



    !   step = (zmax-zmin)/(nlevel+1)
    !   do i = 1, nlevel
    !     clevel(i) = zmin + step*i
    !  enddo

    !   !call getAxesLimits(self, xmin, xmax, ymin, ymax)
    !   call cmap1_init()
    !   call plwind(xmin, xmax, ymin, ymax)

    !   PRINT *, "About to check backing surface"

    !   call self%checkBackingSurface()

    !   !call plbox( 'bcgnt', 0.0_pl_test_flt, 0, 'bcgntv', 0.0_pl_test_flt, 0 )
    !   call plcol0(getLabelFontCode(self))
    !   !call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)

    !   PRINT *, "About to plot series"
    !   !call pllightsource(1._pl_test_flt, 1._pl_test_flt, 1._pl_test_flt)
    !   !call pladv(0)
    !   !call plclear()
    !   !call plvpor(0.0_plflt, 1.0_plflt, 0.0_plflt, 0.9_plflt )
    !   !call plwind(-1.0_plflt, 1.0_plflt, -0.9_plflt, 1.1_plflt )      
    !   !call pladv(0)
    !   do i=1,self%numSeries
    !     !call plclear()
    !     call plcol0(self%plotDataList3d(i)%dataColorCode)
    !     print *, "b4 plgrid data"
    !     call plgriddata(self%plotDataList3d(i)%x, self%plotDataList3d(i)%y, & 
    !     self%plotDataList3d(i)%z, & 
    !     xg, yg, zg, 1_c_int, 0.0_pl_test_flt)
    !     print *, "after plgriddata"

    !     call a2mnmx(zg, xpts, ypts, lzmin, lzmax, xpts)

    !     lzmin = min(lzmin, zmin)
    !     lzmax = max(lzmax, zmax)

    !     lzmin = lzmin - 0.01_pl_test_flt
    !     lzmax = lzmax + 0.01_pl_test_flt
    !     do j=1,nl
    !       clev(j) = lzmin + (lzmax-lzmin)/(nl-1._pl_test_flt)*(j-1._pl_test_flt)
    !   enddo                
    !     call plenv0(xmin, xmax, ymin, ymax, 2, 0)
    !     call plcol0(15)
    !     call pllab("X", "Y", 'tst'//c_null_char)
    !     !call plimage(reshape(self%plotDataList3d(i)%z, [self%xpts, self%ypts]), &
    !     !& xmin, xmax, ymin, ymax, zmin, zmax, &
    !     !& -1._pl_test_flt, 1._pl_test_flt, -1._pl_test_flt, 1._pl_test_flt)  
        
    ! !  call plsurf3d(self%plotdatalist3d(i)%x, self%plotdatalist3d(i)%y, &
    ! !      & reshape(self%plotDataList3d(i)%z, [self%xpts, self%ypts]), & 
    ! !      & MAG_COLOR, clevel(nlevel:1))         
    !     ! print *, "before plshades"
    !     call plshades(zg, xmin, xmax, ymin, &
    !            ymax, clev, 1._pl_test_flt, 0, 1._pl_test_flt, .true. )
        

    !     !call plshades(zg, xmin, xmax, ymin, &
    !     !       ymax, clev, 1._pl_test_flt, 0, 1._pl_test_flt, .true. )
    !            print *, "after plshades"
    !     call plcol0(2)        
    !     ! PL PLOT Area fill
    !     !call plpsty(0)
    !     ! PL PLOT Line Style
    !     !if (self%plotdatalist3d(i)%lineStyleCode > -1) THEN
    !     !call pllsty(self%plotdatalist3d(i)%lineStyleCode)
    !     !call plline(self%plotdatalist3d(i)%x, &
    !     !call plcol0(3)
    !     !call plmtex('t', 1.0_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 'title'//c_null_char)
     
    !     !call plcol0(1)
                  
    !   end do


  
    !   PRINT *, "About to check legend status"
    !   if (self%useLegend) call self%drawLegend()      
  

    ! end subroutine

  !   subroutine cmap1_init()
  !     implicit none
  !     real(kind=pl_test_flt) i(2), h(2), l(2), s(2)

  !     i(1) = 0._pl_test_flt
  !     i(2) = 1._pl_test_flt

  !     h(1) = 240._pl_test_flt
  !     h(2) = 0._pl_test_flt

  !     l(1) = 0.6_pl_test_flt
  !     l(2) = 0.6_pl_test_flt

  !     s(1) = 0.8_pl_test_flt
  !     s(2) = 0.8_pl_test_flt

  !     call plscmap1n(256)
  !     call plscmap1l(.false., i, h, l, s)
  ! end subroutine cmap1_init

        !----------------------------------------------------------------------------
    !      Subroutine a2mnmx
    !      Minimum and the maximum elements of a 2-d array.

    subroutine a2mnmx(f, nx, ny, fmin, fmax, xdim)
      use plplot
      implicit none

      integer   i, j, nx, ny, xdim
      real(kind=pl_test_flt) f(xdim, ny), fmin, fmax

      fmax = f(1, 1)
      fmin = fmax
      do j = 1, ny
          do  i = 1, nx
              fmax = max(fmax, f(i, j))
              fmin = min(fmin, f(i, j))
          enddo
      enddo
  end subroutine a2mnmx

    subroutine drawPlot_plot3d(self)
      
      class(zoaPlot3d), intent(in) :: self

      !type(c_ptr)  :: cc, cs, isurface
      character(len=20) :: string
      character(len=25) :: geometry
      integer :: i
      integer :: plparseopts_rc
      integer :: plsetopt_rc
      integer :: xpts, ypts, index
      integer, parameter :: nlevel = 10
      real(kind=pl_test_flt)   :: zmin, zmax, step, clevel(nlevel)
      real(kind=pl_test_flt) :: xmin, xmax, ymin, ymax
      real(kind=pl_test_flt), allocatable :: xdim(:), ydim(:)

      real(c_double) :: alt=60._c_double, az=30._c_double
  
      class(*), pointer :: item

      PRINT *, "drawPlot_plot3d started"
      zmin = minval( self%plotdatalist3d(1)%z )
      zmax = maxval( self%plotdatalist3d(1)%z )

      xmin = minval(self%plotdatalist3d(1)%x)
      ymin = minval(self%plotdatalist3d(1)%y)
      xmax = maxval(self%plotdatalist3d(1)%x)
      ymax = maxval(self%plotdatalist3d(1)%y)
      
      xpts = self%plotdatalist3d(1)%xpts
      ypts = self%plotdatalist3d(1)%ypts

      allocate(xdim(xpts))
      allocate(ydim(ypts))

      ! xdim = self%plotdatalist3d(1)%x(1:xpts)
      ! index = 1
      ! do i=1,xpts*ypts,xpts
      !   PRINT *, "i is ", i
      !   ydim(index) = self%plotdatalist3d(1)%y(i)
      !   index = index+1
      ! end do

      ! Do it via min/max
      do i=1,xpts
        xdim(i) = xmin + (i-1)*(xmax-xmin)/(xpts-1)
      end do
      do i=1,ypts
        ydim(i) = ymin + (i-1)*(ymax-ymin)/(ypts-1)
      end do


      !zmin = minval( self%plotdatalist3d(1)%z(:xpts,:) )
      !zmax = maxval( self%plotdatalist3d(1)%z(:xpts,:) )
  
      step = (zmax-zmin)/(nlevel+1)
      do i = 1, nlevel
        clevel(i) = zmin + step*i
     enddo

      !call getAxesLimits(self, xmin, xmax, ymin, ymax)
      call plwind(xmin, xmax, ymin, ymax)

      PRINT *, "About to check backing surface"

      call self%checkBackingSurface()

      !call plbox( 'bcgnt', 0.0_pl_test_flt, 0, 'bcgntv', 0.0_pl_test_flt, 0 )
      call plcol0(getLabelFontCode(self))
      !call pllab( trim(self%xlabel)//c_null_char, trim(self%ylabel)//c_null_char, trim(self%title)//c_null_char)

      PRINT *, "About to plot series"
      call pllightsource(1._pl_test_flt, 1._pl_test_flt, 1._pl_test_flt)
      call pladv(0)
      call plclear()
      call plvpor(0.0_plflt, 1.0_plflt, 0.0_plflt, 0.9_plflt )
      call plwind(-1.0_plflt, 1.0_plflt, -0.9_plflt, 1.1_plflt )      
      !call pladv(0)
      do i=1,self%numSeries
        !call plclear()
        call plcol0(self%plotDataList3d(i)%dataColorCode)
        ! PL PLOT Area fill
        !call plpsty(0)
        ! PL PLOT Line Style
        !if (self%plotdatalist3d(i)%lineStyleCode > -1) THEN
        !call pllsty(self%plotdatalist3d(i)%lineStyleCode)
        !call plline(self%plotdatalist3d(i)%x, &
        !call plcol0(3)
        call plmtex('t', 1.0_pl_test_flt, 0.5_pl_test_flt, 0.5_pl_test_flt, 'title'//c_null_char)
     
        !call plcol0(1)
        call plw3d(1.0_pl_test_flt, 1.0_pl_test_flt, 1.0_pl_test_flt, -1.0_pl_test_flt, &
         1.0_pl_test_flt, -1.0_pl_test_flt, 1.0_pl_test_flt, zmin, zmax, alt,az)  
        call plbox3('bnstu','x axis', 0.0_pl_test_flt, 0, &
        'bnstu', 'y axis', 0.0_pl_test_flt, 0, &
        'bcdmnstuv','z axis', 0.0_pl_test_flt, 0) 
        !call plcol0(15)
        call cmap1_init(0)
        call plsurf3d(xdim, ydim, &
        & reshape(self%plotdatalist3d(i)%z, [xpts,ypts]), MAG_COLOR, clevel(nlevel:1)) 
        !PRINT *, "z is ", reshape(self%plotdatalist3d(i)%z, [xpts,ypts])
        PRINT *, "X is ", xdim
        PRINT *, "Y is ", ydim

                  
      end do


  
      PRINT *, "About to check legend status"
      if (self%useLegend) call self%drawLegend()      
  

    end subroutine    

    !----------------------------------------------------------------------------
    subroutine cmap1_init(gray)

      !   For gray.eq.1, basic grayscale variation from half-dark
      !   to light.  Otherwise, hue variations around the front of the
      !   colour wheel from blue to green to red with constant lightness
      !   and saturation.

      integer          :: gray
      real(kind=pl_test_flt) :: i(0:1), h(0:1), l(0:1), s(0:1)

      !   left boundary
      i(0) = 0._pl_test_flt
      !   right boundary
      i(1) = 1._pl_test_flt
      if (gray == 1) then
          !       hue -- low: red (arbitrary if s=0)
          h(0) = 0.0_pl_test_flt
          !       hue -- high: red (arbitrary if s=0)
          h(1) = 0.0_pl_test_flt
          !       lightness -- low: half-dark
          l(0) = 0.5_pl_test_flt
          !       lightness -- high: light
          l(1) = 1.0_pl_test_flt
          !       minimum saturation
          s(0) = 0.0_pl_test_flt
          !       minimum saturation
          s(1) = 0.0_pl_test_flt
      else
          !       This combination of hues ranges from blue to cyan to green to yellow
          !       to red (front of colour wheel) with constant lightness = 0.6
          !       and saturation = 0.8.

          !       hue -- low: blue
          h(0) = 240._pl_test_flt
          !       hue -- high: red
          h(1) = 0.0_pl_test_flt
          !       lightness -- low:
          l(0) = 0.6_pl_test_flt
          !       lightness -- high:
          l(1) = 0.6_pl_test_flt
          !       saturation
          s(0) = 0.8_pl_test_flt
          !       minimum saturation
          s(1) = 0.8_pl_test_flt
      endif
      call plscmap1n(256)
      call plscmap1l(.false., i, h, l, s)
  end subroutine cmap1_init

    subroutine addLegend(self, legendNames)

      class(zoaplot) :: self
      character(len=*) :: legendNames(:)
      integer :: i

      self%useLegend = .TRUE.

      do i=1,size(legendNames)
        self%legendNames(i) = trim(legendNames(i))
      end do

      self%numLegendNames = size(legendNames)

      PRINT *, "LegendNames is ", self%legendNames


    end subroutine

    subroutine drawBottomRightLegend(self, strLegend)
      implicit none
      class(zoaplot) :: self
      character(len=*) :: strLegend
      
      integer           :: type, i
      integer           :: nlegend
      !integer :: nlegend = self%numLegendNames


      real(kind=pl_test_flt)  :: legend_width, legend_height
      integer           :: opt_array(self%numLegendNames), text_colors(self%numLegendNames), & 
             line_colors(self%numLegendNames), &
             line_styles(self%numLegendNames), symbol_colors(self%numLegendNames), symbol_numbers(self%numLegendNames)
      real(kind=pl_test_flt)  :: line_widths(self%numLegendNames), symbol_scales(self%numLegendNames), &
      & box_scales(self%numLegendNames)
      integer           :: box_colors(self%numLegendNames), box_patterns(self%numLegendNames)
      real(kind=pl_test_flt)  :: box_line_widths(self%numLegendNames)
      character(len=len(strLegend)) :: text(self%numLegendNames)
      character(len=20)  :: symbols(self%numLegendNames)      
      character(len=20) :: texttest(self%numLegendNames)

      nlegend = self%numLegendNames

        !   Draw a legend
        !   First legend entry.
      PRINT *, "before legend vars defined"
      do i=1,nlegend

        opt_array(i)   = PL_LEGEND_LINE !PL_LEGEND_SYMBOL
        ! Todo:  Tie this to what was actually set
        text_colors(i) = PL_PLOT_RED !self%plotDataList(i)%dataColorCode 
        line_colors(i) = PL_PLOT_RED !self%plotDataList(i)%dataColorCode
        line_styles(i) = 1
        line_widths(i) = 1 
        symbols(i) = '-'
        text(i) = strLegend//c_null_char
        box_scales(i) = 0.1
        symbol_colors(i)  = PL_PLOT_RED !self%plotDataList(i)%dataColorCode
        symbol_scales(i)  = 1.0
        symbol_numbers(i) = 2            
      end do
       

     call plscol0a( 15, 32, 32, 32, 0.70_pl_test_flt )

      PRINT *, "Before pllegend called"
      ! See doc here:  There are ALOT of arguments and easy to mess up and not get what you want
      ! http://plplot.org/docbook-manual/plplot-html-5.15.0/pllegend.html
      call pllegend( legend_width, legend_height, &
      PL_LEGEND_BACKGROUND + PL_LEGEND_BOUNDING_BOX, & 
             PL_POSITION_VIEWPORT, &
             .15_pl_test_flt, 0.8_pl_test_flt, 0.05_pl_test_flt, 0, & ! x offset, y offset, plot width (includes width of symbols), bg col
             0, 0, 1, nlegend, &
             opt_array, &
             1.0_pl_test_flt, .70_pl_test_flt, 2.0_pl_test_flt, &
             1.0_pl_test_flt, text_colors, text, &
             box_colors, box_patterns, box_scales, box_line_widths, &
             line_colors, line_styles, line_widths, &
             symbol_colors, symbol_scales, symbol_numbers, symbols )

    end subroutine

    subroutine drawLegend(self)

      implicit none
      
      ! This is from example 4 - use as template to get legend working
      class(zoaplot) :: self      
       
      integer           :: type, i
      integer           :: nlegend
      !integer :: nlegend = self%numLegendNames


      real(kind=pl_test_flt)  :: legend_width, legend_height
      integer           :: opt_array(self%numLegendNames), text_colors(self%numLegendNames), & 
             line_colors(self%numLegendNames), &
             line_styles(self%numLegendNames), symbol_colors(self%numLegendNames), symbol_numbers(self%numLegendNames)
      real(kind=pl_test_flt)  :: line_widths(self%numLegendNames), symbol_scales(self%numLegendNames), &
      & box_scales(self%numLegendNames)
      integer           :: box_colors(self%numLegendNames), box_patterns(self%numLegendNames)
      real(kind=pl_test_flt)  :: box_line_widths(self%numLegendNames)
      character(len=5) :: text(self%numLegendNames)
      character(len=20)  :: symbols(self%numLegendNames)      
      character(len=20) :: texttest(self%numLegendNames)

      nlegend = self%numLegendNames

        !   Draw a legend
        !   First legend entry.
      do i=1,self%numLegendNames

        opt_array(i)   = PL_LEGEND_LINE !PL_LEGEND_SYMBOL
        ! Todo:  Tie this to what was actually set
        text_colors(i) = self%plotDataList(i)%dataColorCode 
        line_colors(i) = self%plotDataList(i)%dataColorCode
        line_styles(i) = 1
        line_widths(i) = 1   
        symbols(i) = '-'
        text(i) = trim(self%legendNames(i))//c_null_char
        box_scales(i) = 0.1
        symbol_colors(i)  = self%plotDataList(i)%dataColorCode
        symbol_scales(i)  = 1.0
        symbol_numbers(i) = 2            
      end do
       
      !text = self%legendNames(1:self%numLegendNames)

      ! opt_array(1)   = PL_LEGEND_LINE
      ! text_colors(1) = 4
      ! text(1)        = 'Amplitude'
      ! line_colors(1) = 4
      ! line_styles(1) = 1
      ! line_widths(1) = 1
      !   note from the above opt_array the first symbol (and box) indices
      !   do not have to be specified, at least in C. For Fortran we need
      !   to set the symbols to be something, since the string is always
      !   copied as part of the bindings.
      !symbols(1) = ''      

      !   note from the above opt_array the first symbol (and box) indices
      !   do not have to be specified, at least in C. For Fortran we need
      !   to set the symbols to be something, since the string is always
      !   copied as part of the bindings.
      !symbols(1) = ''

      !   Second legend entry.

      ! opt_array(2)      = PL_LEGEND_LINE + PL_LEGEND_SYMBOL
      ! text_colors(2)    = 3
      ! text(2)           = 'Phase shift'
      ! line_colors(2)    = 3
      ! line_styles(2)    = 1
      ! line_widths(2)    = 1
      ! symbol_colors(2)  = 3
      ! symbol_scales(2)  = 1.0
      ! symbol_numbers(2) = 4
      ! symbols(2)        = "#(728)"

      !   from the above opt_arrays we can completely ignore everything
      !   to do with boxes. (Hence the size 0 for the associated arrays)

      call plscol0a( 15, 32, 32, 32, 0.70_pl_test_flt )
      !pllegend(p_legend_width, p_legend_height, opt, position, x, y, 
      !plot_width, bg_color, bb_color, bb_style, nrow, ncolumn, 
      !opt_array, text_offset, text_scale, text_spacing, 
      !test_justification, text_colors, text, box_colors, 
      !box_patterns, box_scales, box_line_widths, line_colors, 
      !line_styles, line_widths, symbol_colors, symbol_scales, symbol_numbers, symbols)
      call pllegend( legend_width, legend_height, &
             PL_LEGEND_BACKGROUND + PL_LEGEND_BOUNDING_BOX, & 
             PL_POSITION_OUTSIDE + PL_POSITION_BOTTOM, &
             0.0_pl_test_flt, +0.25_pl_test_flt, -0.05_pl_test_flt, 0, &
             0, 0, 1, nlegend, &
             opt_array, &
             1.0_pl_test_flt, 1.0_pl_test_flt, 2.0_pl_test_flt, &
             1.0_pl_test_flt, text_colors(1:nlegend), text(1:nlegend), &
             box_colors(1:nlegend), box_patterns(1:nlegend), box_scales(1:nlegend), box_line_widths(1:nlegend), &
             line_colors(1:nlegend), line_styles(1:nlegend), line_widths(1:nlegend), &
             symbol_colors(1:nlegend), symbol_scales(1:nlegend), symbol_numbers(1:nlegend), symbols(1:nlegend) )



             PRINT *, "legend_width is ", legend_width
             PRINT *, "legend_height is ", legend_height
             PRINT *, "Legend Symbol is ", symbols(1:nlegend)
             PRINT *, "OPT is ",  PL_LEGEND_BACKGROUND + PL_LEGEND_BOUNDING_BOX
             PRINT *, "POS is ", PL_POSITION_VIEWPORT
             PRINT *, "nlegend is ", nlegend
             PRINT *, "LIne Style is ", line_styles(1:nlegend)
             PRINT *, "symbol_scales is ", symbol_scales(1:nlegend)
             PRINT *, "symbol_numbers is ", symbol_numbers(1:nlegend)


    end subroutine

    subroutine addXYPlot(self, X, Y)

        implicit none
        class(zoaplot), intent(inout) :: self
        real, dimension(:), intent(in) :: X,Y

        self%numSeries = self%numSeries + 1
        call self%plotdatalist(self%numSeries)%initialize(X,Y)

  end subroutine

  subroutine checkBackingSurface(self)
    implicit none
    class(zoaplot) :: self
    type(c_ptr) :: isurface

    isurface = c_null_ptr
    if (c_associated(self%area)) then
      isurface = g_object_get_data(self%area, "backing-surface")
      PRINT *, "self%area is ", LOC(self%area)

    !PRINT *, "isurface in mp_draw is ", LOC(isurface)
    if (.not. c_associated(isurface)) then
       PRINT *, "Backing surface is NULL.  Create one"
       ! TODO:  Should not have hard coded size right here
       call LogTermFOR("Loose pointer in check backing surface") 
      isurface = cairo_image_surface_create(CAIRO_FORMAT_RGB24, 1200, 500)
      isurface = cairo_surface_reference(isurface)   ! Prevent accidental deletion
      call g_object_set_data(self%area, "backing-surface", isurface)
    end if

    end if   

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
       !print *, "labelFontColor ", trim(self%labelFontColor)
    end function getLabelFontCode

    subroutine getAxesLimits(self, xmin, xmax, ymin, ymax)

    class(zoaplot), intent(in) :: self
    real(kind=pl_test_flt), intent(inout) :: xmin, xmax, ymin, ymax
    real(kind=pl_test_flt) :: yrng, xrng
    integer :: j

    if (self%numSeries > 1) then
      xmin = minval(self%plotdatalist(1)%x)
      xmax = maxval(self%plotdatalist(1)%x)
      ymin = minval(self%plotdatalist(1)%y)
      ymax = maxval(self%plotdatalist(1)%y)     
      do j=2,self%numSeries
        xmin = min(xmin,minval(self%plotdatalist(j)%x))
        xmax = max(xmax,maxval(self%plotdatalist(j)%x))
        ymin = min(ymin,minval(self%plotdatalist(j)%y))
        ymax = max(ymax,maxval(self%plotdatalist(j)%y))
      end do

    else

    xmin = minval(self%x)
    xmax = maxval(self%x)
    ymin = minval(self%y)
    ymax = maxval(self%y)

    end if

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
