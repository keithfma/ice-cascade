! =============================================================================
! Boundary condition methods, functional form is common for all model types.
!
! contains:
! ============================================================================

module bc_module

use types, only: dp

implicit none
private
public bc, set_bc_proc, bc_zero_grad, bc_bench_hill_ss_sin, bc_bench_hill_ss_const

  ! ---------------------------------------------------------------------------
  ! interface: template for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface
    function bc(edge, intr) result(bnd)
      import :: dp
      real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
      real(dp), intent(in) :: intr(:)         ! (not used for this bc)
      real(dp)             :: bnd(size(edge)) ! bc points, elev [m]
    end function bc
  end interface

  ! ---------------------------------------------------------------------------
  ! parameters
  ! ---------------------------------------------------------------------------
  
  ! common
  real(dp), parameter :: pi = 4.0_dp*atan(1.0_dp)
  
  ! benchmark, hill, steady state
  real(dp), parameter :: hill_ss_t1 = 1.0_dp 
  real(dp), parameter :: hill_ss_tm = 9.0_dp

contains

  ! ---------------------------------------------------------------------------
  ! SUB: parse BC name and associate the BC procedure pointer
  ! ---------------------------------------------------------------------------
  subroutine set_bc_proc(str, ptr)

    character(len=*), intent(in)         :: str ! BC name
    procedure (bc), pointer, intent(out) :: ptr ! procedure pointer to be set

    if ('zero_grad'           .eq. trim(str)) ptr => bc_zero_grad
    if ('bench_hill_ss_sin'   .eq. trim(str)) ptr => bc_bench_hill_ss_sin
    if ('bench_hill_ss_const' .eq. trim(str)) ptr => bc_bench_hill_ss_const
    
  end subroutine set_bc_proc
  

  ! ---------------------------------------------------------------------------
  ! FUNC: Boundary condition, zero surface gradient normal to boundary (no-flux) 
  ! ---------------------------------------------------------------------------
  function bc_zero_grad(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = edge ! surface gradient -> 1, then flux -> 0
    
    return
  end function bc_zero_grad
 

  ! ===========================================================================
  ! Steady-state benchmark 
  !  
  ! Steady state case with sinusoidal topography at the northern boundary, and
  ! constant topography elsewhere. Solution is derived at:
  ! http://www.mhhe.com/engcs/mech/holman/graphics/samplech_3.pdf
  ! ===========================================================================

  ! ---------------------------------------------------------------------------
  ! FUNC: Benchmark, Hill, Steady-state, sin-wave Dirichlet 
  ! ---------------------------------------------------------------------------
  function bc_bench_hill_ss_sin(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    integer  :: n, i
    real(dp) :: c

    n = size(bnd)
    c = pi/(n-3)
    do i = 1,n
      bnd(i) = hill_ss_tm*sin(c*(i-2))+hill_ss_t1
    end do

    return
  end function bc_bench_hill_ss_sin

  ! ---------------------------------------------------------------------------
  ! FUNC: Benchmark, Hill, Steady-state, constant value Dirichlet
  ! ---------------------------------------------------------------------------
  function bc_bench_hill_ss_const(edge, intr) result(bnd)

    real(dp), intent(in) :: edge(:)         ! domain edge, elev [m]
    real(dp), intent(in) :: intr(:)         ! (not used for this BC)
    real(dp)             :: bnd(size(edge)) ! bc points, elev [m]

    bnd = hill_ss_t1 

    return
  end function bc_bench_hill_ss_const

end module bc_module

