program plot_command_test
    use plot_functions, only: setPlotNum
    use plot_setting_manager, only: zoaplot_setting_manager
    implicit none
    type(zoaplot_setting_manager) :: psm

    ! Exercise the real module and the same derived-type fields as VIE_GO.
    psm%baseCmd = 'VIE'
    psm%plotNum = 1
    call setPlotNum(psm%baseCmd, psm%plotNum)
    if (trim(psm%baseCmd) /= 'VIE P1') stop 1
    call setPlotNum(psm%baseCmd, psm%plotNum)
    if (trim(psm%baseCmd) /= 'VIE P1') stop 2
    psm%baseCmd = 'VIE P12 P12'
    psm%plotNum = 3
    call setPlotNum(psm%baseCmd, psm%plotNum)
    if (trim(psm%baseCmd) /= 'VIE P3') stop 3
    psm%baseCmd = 'PMA p2'
    psm%plotNum = 12
    call setPlotNum(psm%baseCmd, psm%plotNum)
    if (trim(psm%baseCmd) /= 'PMA P12') stop 4
    psm%baseCmd = 'VIE PROJECTION'
    call setPlotNum(psm%baseCmd, psm%plotNum)
    if (trim(psm%baseCmd) /= 'VIE PROJECTION P12') stop 5
end program plot_command_test
