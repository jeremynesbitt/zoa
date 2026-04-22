! This file contains all the interfaces that cause conflicts (or I aspire this to be the case)
module zoa_interfaces

  interface
    subroutine addOptimVariable(surf, int_code)
        integer :: surf, int_code
    end subroutine    
  end interface
  
end module