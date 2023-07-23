

strID = "ID_PLOTTYPE_ZERNIKE"
modFileName = "ui-zernike.f90"
strWinTitle = "Zernike Plot"
newplot_tab = "zernike_tab"
plot_settings = "zernike_settings"
modName = "ui_zernike"

main():


# Add unique ID to zoa

# open zoa-ui.f90
# find all ID_PLOTTYPE entries
# get unique # from entries
# insert new line at end with entry

# Make a copy of ui-template.f90
# rename it to modFileName
# change names of newui and newui_sdttings to the new names.

# In zoa-tab-manager.f90 addNewPlot
# update include for modName
# find end of select loop
# Add in minimal code to add new tab

    case (strID)
        call logger%logText('New Diagram Starting')
        winTitle = strWinTitle
        allocate(newplot_tab :: self%tabInfo(self%tabNum)%tabObj)
        call self%tabInfo(self%tabNum)%tabObj%initialize(self%notebook, trim(winTitle), strID)
        call self%tabInfo(self%tabNum)%tabObj%newPlot()
        allocate(spot_settings :: self%tabInfo(self%tabNum)%settings )
        self%tabInfo(self%tabNum)%settings = spot_struct_settings

# ROUTEDRAWING - add replot code
    case (strID)
        call plot_settings%replot()

# add a new command in NAMES.FOR and CMDER.FOR

# add new routines to interface with new command.  Eg for RMSFIELD

subroutine RMSFIELD
use GLOBALS
use global_widgets
use handlers, only : updateTerminalLog
 use zoa_plot
   call logger%logText('RMSField Routine Starting')

   call RMSFIELD_PLOT

end subroutine

subroutine RMSFIELD_PLOT
   use global_widgets

  use zoa_plot
  use zoa_tab
  use zoa_ui
  !use zoa_tab_manager
  use gtk_draw_hl
  !use zoa_tab_manager
  use handlers
  implicit none
  type(c_ptr) :: localcanvas

    localcanvas = hl_gtk_drawing_area_new(size=[1200,500], &
         & has_alpha=FALSE)


  call zoatabMgr%addPlotTab(ID_PLOTTYPE_RMSFIELD, inputTitle='RMS Field Plot', extcanvas=localcanvas)

end subroutine

# Update makefile
