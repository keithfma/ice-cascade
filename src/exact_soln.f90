! =============================================================================
! Exact solution methods and results, common code.
!
! Contains:
!   type soln_type (public)
!   NOTE: list is not yet complete
! ============================================================================

module soln_module

use types, only: dp
use grid_module, only: grid_type

implicit none
private
public soln_topo

  ! ---------------------------------------------------------------------------
  ! FUNC TEMPLATE: common form for the topography solution functions
  ! ---------------------------------------------------------------------------
  abstract interface
    function soln_topo(G, time) result(soln)
      import                       :: dp, grid_type       
      class(grid_type), intent(in) :: G                    ! grid object defining domain
      real(dp), intent(in)         :: time                 ! current model time
      real(dp)                     :: soln(G%nx+2, G%ny+2) ! output, computed solution
    end function soln_topo
  end interface


  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the various exact solutions
  ! NOTE: Only topo solution is currently included.
  ! ---------------------------------------------------------------------------
  type soln_type
    type(grid_type), pointer                :: g        ! pointer to shared grid object
    character(len=100)                     :: topo_name ! name of topo solution method
    logical                                :: topo_on   ! enable/disable topo solution
    procedure (soln_topo), pointer, nopass :: topo      ! function to compute topo soln
  end type soln_type

contains


  ! --------------------------------------------------------------------------- 
  ! SUB: initialize a solution object
  ! --------------------------------------------------------------------------- 
  subroutine init(S)

    class(soln_type) :: S ! solution object to initialize

    ! setup topo solution
    call set_topo_proc(S%topo_name, S%topo_on, S%topo)

    ! (add other solutions as needed)

  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: parse solution name and associate the procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine set_topo_proc(str, on, ptr)
!
    character(len=*), intent(in)                :: str ! solution name
    logical, intent(out)                        :: on  ! enable/disable solution
    procedure (soln_topo), pointer, intent(out) :: ptr ! procedure pointer to be set
!
!    if ('bench_hill_ss' .eq. trim(str)) ptr => bc_bench_hill_ss_sin
!    
  end subroutine set_topo_proc

end module soln_module
