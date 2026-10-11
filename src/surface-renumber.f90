! Surface insert/delete bookkeeping for the typed stores the legacy KDP
! routines do not know about.  SINS/SDEL (LDM6.f90) renumber the legacy
! data (pickups, solves, configs, tilt returns) and then call these
! external routines, once per surface, so every insert/delete path (lens
! editor, DEL Sk, INS, macros) keeps the typed stores attached to the same
! surfaces:
!   optimizer variables (ldm%vars, and the variable count nV)
!   edge apertures (ldm%edge_semi_y)
!   zoom operands (zoom_manager)
!   merit-entry surface qualifiers (UCY S5 ...)
! Deliberately external (no module) so LDM6 can call them without a use.

! Surface k was inserted: old surfaces k, k+1, ... are now k+1, k+2, ...
subroutine typed_surface_inserted(k)
  use mod_lens_data_manager, only: ldm
  use optim_types, only: nM, meritInUse
  use zoom_manager, only: zoom_surfaces_shifted
  implicit none
  integer, intent(in) :: k
  integer :: n, i

  n = ubound(ldm%vars, 1)
  if (k < 0 .or. k > n) return
  ldm%vars(k+1:n, :) = ldm%vars(k:n-1, :)
  ldm%vars(k, :) = 100
  ldm%edge_semi_y(k+1:n) = ldm%edge_semi_y(k:n-1)
  ldm%edge_semi_y(k) = 0.0d0

  do i = 1, nM
    if (meritInUse(i)%iS >= k) meritInUse(i)%iS = meritInUse(i)%iS + 1
  end do
  call zoom_surfaces_shifted(k, +1)
  call recount_optim_vars()
end subroutine typed_surface_inserted

! Surface k was deleted: old surfaces k+1, k+2, ... are now k, k+1, ...
! Anything on surface k itself goes with it.
subroutine typed_surface_deleted(k)
  use mod_lens_data_manager, only: ldm
  use optim_types, only: nM, meritInUse, deleteConstraint, meritNameField
  use zoom_manager, only: zoom_surfaces_shifted
  use zoa_output, only: zoa_emit
  implicit none
  integer, intent(in) :: k
  integer :: n, i

  n = ubound(ldm%vars, 1)
  if (k < 0 .or. k > n) return
  ldm%vars(k:n-1, :) = ldm%vars(k+1:n, :)
  ldm%vars(n, :) = 100
  ldm%edge_semi_y(k:n-1) = ldm%edge_semi_y(k+1:n)
  ldm%edge_semi_y(n) = 0.0d0

  i = 1
  do while (i <= nM)
    if (meritInUse(i)%iS == k) then
      call zoa_emit("Merit entry "//meritNameField(meritInUse(i))// &
      &  " removed: its surface was deleted", "black")
      call deleteConstraint(i)
      cycle
    end if
    if (meritInUse(i)%iS > k) meritInUse(i)%iS = meritInUse(i)%iS - 1
    i = i + 1
  end do
  call zoom_surfaces_shifted(k, -1)
  call recount_optim_vars()
end subroutine typed_surface_deleted

! nV (the optimizer's variable count) = the variable flags now set in
! ldm%vars.  GLC (VAR_GLA) is bookkeeping only and is not an optimizer
! variable, matching updateOptimVarsNew.
subroutine recount_optim_vars()
  use mod_lens_data_manager, only: ldm
  use optim_types, only: nV
  use zoa_ui, only: VAR_GLA
  implicit none
  integer :: j
  nV = 0
  do j = 1, ubound(ldm%vars, 2)
    if (j == VAR_GLA) cycle
    nV = nV + count(ldm%vars(:, j) == 0)
  end do
end subroutine recount_optim_vars
