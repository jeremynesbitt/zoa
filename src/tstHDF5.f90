module testHDF5
  use h5fortran

contains

subroutine tstwrite

  PRINT *, "IT Compiles!"

  call h5write('golt.h5','/x', [1,2,3,4,5,6])

end subroutine

end module
