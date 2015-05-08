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
    procedure(bc_tmpl), pointer, nopass :: apply_nbc ! set north BC
    procedure(bc_tmpl), pointer, nopass :: apply_sbc ! set south BC
    procedure(bc_tmpl), pointer, nopass :: apply_wbc ! set west BC
    procedure(bc_tmpl), pointer, nopass :: apply_ebc ! set east BC
    procedure(update_tmpl), pointer, nopass :: update ! ice model method
    procedure(soln_tmpl), pointer, nopass :: solve ! exact solution  
  contains
    procedure, pass :: init ! initialize object
  end type ice_type


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface 
    function bc_tmpl(topo_edge, topo_intr, topo_oppo, &
                     ice_edge, ice_intr, ice_oppo) result(bnd)
      import :: rp                                       ! use special types
      real(rp), intent(in) :: topo_edge(:), ice_edge(:)  ! domain edge 
      real(rp), intent(in) :: topo_intr(:), ice_intr(:)  ! domain edge-1
      real(rp), intent(in) :: topo_oppo(:), ice_oppo(:)  ! opposite domain edge
      real(rp) :: bnd(size(topo_edge))                   ! bc points
    end function bc_tmpl 
  end interface


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the ice model method
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine update_tmpl(p, s)
      import :: param_type, state_type     ! use special types
      type(param_type), intent(in)    :: p ! parameters
      type(state_type), intent(inout) :: s ! state vars
    end subroutine update_tmpl 
  end interface

  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the exact solution subroutines
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine soln_tmpl(p, s)
      import :: param_type, state_type     ! use special types
      type(param_type), intent(in)    :: p ! parameters
      type(state_type), intent(inout) :: s ! state vars
    end subroutine soln_tmpl 
  end interface


contains


  ! ---------------------------------------------------------------------------
  ! SUB: Initialize ice model object 
  ! ---------------------------------------------------------------------------
  subroutine init(g, p)

    class(ice_type), intent(out) :: g
    type(param_type), intent(in) :: p

    character(len=100) :: nbc_name, ebc_name, sbc_name, wbc_name
    integer :: comma(3)

    ! Set ice model method
    select case (p%ice_name)

      case ('none')
        g%on = .false.
        g%update => NULL() ! will fail if called, by design

      case ('mahaffy_isothermal_nonsliding')
        g%on_soln = .false.
        g%solve => NULL() ! NOT YET IMPLEMENTED
    
      case default
        print *, "Invalid name for ice model method: " // trim(p%ice_name)
        stop 'Stopped.'

    end select

    ! Set exact solution procedure
    select case (p%ice_soln_name)

      case ('none')
        g%on_soln = .false.
        g%solve => NULL() ! will seg-fault if called, by design

      case('bueler_isothermal_a')
        g%on_soln = .true.
        g%solve => NULL() ! NOT YET IMPLEMENTED

      case default
        print *, "Invalid name for exact solution: " // trim(p%ice_soln_name)
        stop 'Stopped.'

    end select

    ! Parse boundary condition names from comma-separated list
    comma(1) = index(p%ice_bc_name, ',')
    comma(2) = index(p%ice_bc_name((comma(1)+1):), ',')+comma(1)
    comma(3) = index(p%ice_bc_name((comma(2)+1):), ',')+comma(2)
    nbc_name = p%ice_bc_name(1:comma(1)-1)
    ebc_name = p%ice_bc_name(comma(1)+1:comma(2)-1)
    sbc_name = p%ice_bc_name(comma(2)+1:comma(3)-1)
    wbc_name = p%ice_bc_name(comma(3)+1:)

    ! Set boundary condition procedures (uses subroutine due to repitition)
    call set_bc_pointer(nbc_name, g%apply_nbc)
    call set_bc_pointer(ebc_name, g%apply_ebc)
    call set_bc_pointer(sbc_name, g%apply_sbc)
    call set_bc_pointer(wbc_name, g%apply_nbc)

  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: Parse boundary condition names and associate procedure pointers
  ! ---------------------------------------------------------------------------
  subroutine set_bc_pointer(str, ptr)

    character(len=*), intent(in) :: str
    procedure(bc_tmpl), pointer, intent(out) :: ptr

    select case (str)

      case ('none')
        ptr => NULL() ! will fail if called, by design. 
    
      case default
        print *, "Invalid name for boundary condition: " // trim(str)
        stop 'Stopped.'
   
    end select

  end subroutine set_bc_pointer

end module ice_mod
