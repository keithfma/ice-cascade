! =============================================================================
! Glacier dynamics model component for ice-cascade
!
! Contains:
!   type ice_type (public)
!   NOTE: list is not yet complete
!
! References:
! (1) Bueler et al 2005...
!
! ============================================================================

module ice_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
public ice_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the glacier model component
  ! ---------------------------------------------------------------------------
  type ice_type
    logical :: on ! enable/disable ice flow
    logical :: on_soln ! enable/disable solution calculation
    character(len=100) :: name ! flow model name
    character(len=100) :: name_nbc ! north BC name
    character(len=100) :: name_sbc ! south BC name
    character(len=100) :: name_wbc ! west BC name
    character(len=100) :: name_ebc ! east BC name
    character(len=100) :: name_soln ! exact solution name
    real(rp), dimension(10) :: param ! flow model parameters, [various]
    real(rp), dimension(10) :: param_soln ! solution parameters, [various]
    ! procedure for flow
    ! procedure for solution


    !logical :: on ! enable/disable
    !logical :: on_soln ! enable disable exact solution
    !real(rp) :: A0 ! ice flow law coeff prefactor, [Pa-3 a-1]
    !real(rp) :: As0 ! ice sliding law coeff prefactor, [?]
    !real(rp), allocatable :: A(:,:) ! ice flow law coeff, [Pa-3 a-1]
    !real(rp), allocatable :: As(:,:) ! ice sliding law coeff, [?]
    !character(len=100) :: name_flow ! ice flow method name
    !character(len=100) :: name_soln ! exact solution name
    !real(rp), dimension(10) :: param_soln !  solution param, [various]
    !procedure(bc_tmpl), pointer, nopass :: apply_nbc ! set north BC
    !procedure(bc_tmpl), pointer, nopass :: apply_sbc ! set south BC
    !procedure(bc_tmpl), pointer, nopass :: apply_wbc ! set west BC
    !procedure(bc_tmpl), pointer, nopass :: apply_ebc ! set east BC
    !procedure(flow_tmpl), pointer, pass :: flow  ! ice flow  
    !procedure(soln_tmpl), pointer, pass :: solve ! exact solution  
!  contains
!    procedure, pass :: init ! initialize all components
!    procedure, pass :: update ! run model                   
  end type ice_type


  ! ---------------------------------------------------------------------------
  ! FUNC TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface 
    function bc_tmpl(topo_edge, topo_intr, topo_oppo, ice_edge, ice_intr, &
                     ice_oppo) result(bnd)
      import :: rp                                       ! use special types
      real(rp), intent(in) :: topo_edge(:), ice_edge(:)  ! domain edge 
      real(rp), intent(in) :: topo_intr(:), ice_intr(:)  ! domain edge-1
      real(rp), intent(in) :: topo_oppo(:), ice_oppo(:)  ! opposite domain edge
      real(rp) :: bnd(size(edge))                        ! bc points
    end function bc_tmpl 
  end interface


  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for the ice flow subroutines
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine flow_tmpl(g, s)
      import :: ice_type, state_type       ! use special types
      class(ice_type), intent(in) :: g     ! ice model spec
      type(state_type), intent(inout) :: s ! state vars
    end subroutine flow_tmpl 
  end interface

  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for the exact solution subroutines
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine soln_tmpl(g, s)
      import :: ice_type, state_type       ! use special types
      class(ice_type), intent(in) :: g     ! ice model spec
      type(state_type), intent(inout) :: s ! state vars
    end subroutine soln_tmpl 
  end interface

  ! ---------------------------------------------------------------------------
  ! PARAMETERS: constants for benchmarks, etc
  ! ---------------------------------------------------------------------------


contains


  !! ---------------------------------------------------------------------------
  !! SUB: Initialize ice model object 
  !! ---------------------------------------------------------------------------
  !subroutine init(g, s)

  !  class(ice_type), intent(inout) :: g
  !  type(state_type), intent(in) :: s

  !  ! Allocate arrays
  !  allocate(g%A(s%nx+2, s%ny+2))
  !  allocate(g%As(s%nx+2, s%ny+2))

  !  ! Populate arrays
  !  g%A = g%A0
  !  g%As = g%As0

  !  ! Set boundary condition procedures (uses subroutine due to repitition)
  !  call set_bc_pointer(g%name_nbc, g%apply_nbc)
  !  call set_bc_pointer(g%name_sbc, g%apply_sbc)
  !  call set_bc_pointer(g%name_ebc, g%apply_ebc)
  !  call set_bc_pointer(g%name_wbc, g%apply_nbc)
  !  
  !  ! Set ice flow procedure
  !  select case (g%name_flow)
  !    case ('mahaffy')
  !      g%on_soln = .true.
  !      g%flow => NULL() ! NOT YET IMPLEMENTED
  !    case default
  !      print *, "Invalid name for glacier flow algorithm: " // trim(g%name_flow)
  !      stop -1
  !  end select

  !  ! Set exact solution procedure
  !  select case (g%name_soln)
  !    case ('none')
  !      g%on_soln = .false.
  !      g%solve => NULL() ! will seg-fault if called, by design
  !    case ('bueler_isothermal_a')
  !      g%on_soln = .true.
  !      g%solve => solve_bueler_isothermal_a 
  !    case default
  !      print *, "Invalid name for exact solution: " // trim(g%name_soln)
  !      stop -1
  !  end select

  !end subroutine init


  !! ---------------------------------------------------------------------------
  !! SUB: Parse boundary condition names and associate procedure pointers
  !! ---------------------------------------------------------------------------
  !subroutine set_bc_pointer(str, ptr)

  !  character(len=*), intent(in) :: str
  !  procedure(bc_tmpl), pointer, intent(out) :: ptr

  !  select case (str)
  !    case ("no_ice")
  !      ptr => NULL() ! NOT YET IMPLEMENTED 
  !    case default
  !      print *, "Invalid name for boundary condition: " // trim(str)
  !      stop -1
  !  end select

  !end subroutine set_bc_pointer


  !! ---------------------------------------------------------------------------
  !! SUB: Update ice model 
  !! ---------------------------------------------------------------------------
  !subroutine update(g, s)

  !  class(ice_type), intent(inout) :: g
  !  type(state_type), intent(inout) :: s

  !  ! NOT YET IMPLEMENTED

  !end subroutine update


  !! ---------------------------------------------------------------------------
  !! SUB: Benchmark solution, Bueler isothermal A
  !!   Parameters
  !!   L: fixed ice cap radius [m] =  g%param_soln(1)
  !!   M0: constant positive surface ice flux [m_ice/a] =  g%param_soln(2)
  !!   A: isothermal ice deformation parameter [Pa-3 a-1]  = g%A0
  !! ---------------------------------------------------------------------------
  !subroutine solve_bueler_isothermal_a(g, s)

  !  class(ice_type), intent(in) :: g
  !  type(state_type), intent(inout) :: s

  !  integer :: i, j
  !  real(rp) :: A, c1, c2, gam, L, M0, r

  !  ! rename parameters for local use
  !  L = g%param_soln(1)     
  !  M0 = g%param_soln(2)
  !  A = g%A0

  !  ! pre-compute constants
  !  gam = 2.0_rp/5.0_rp*A*(s%rhoi*s%grav)**3.0_rp
  !  c1 = (4.0_rp*M0/gam)**(1.0_rp/8.0_rp)
  !  c2 = L**(4.0_rp/3.0_rp)

  !  ! solve
  !  do j = 1, s%ny+2
  !    do i = 1, s%nx+2
  !      r = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
  !      if (r .le. L) then
  !        s%ice_h_soln(i,j) = c1*(c2-r**(4.0_rp/3.0_rp))**(3.0_rp/8.0_rp)
  !      else
  !        s%ice_h_soln(i,j) = 0.0_rp
  !      end if
  !    end do
  !  end do

  !end subroutine solve_bueler_isothermal_a


  !! ---------------------------------------------------------------------------
  !! SUB: Benchmark solution, Bueler isothermal B
  !! ---------------------------------------------------------------------------

end module ice_mod
