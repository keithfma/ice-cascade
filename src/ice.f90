! =============================================================================
! Glacier dynamics model component for ice-cascade
!
! Contains:
!   type ice_type (public)
!   NOTE: list is not yet complete
! ============================================================================

module ice_mod

use kinds_mod, only: rp
use param_mod, only: param_type
use state_mod, only: state_type

implicit none
private
public :: ice_type

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
    procedure(update_tmpl), pointer, pass :: update ! ice model method
    procedure(soln_tmpl), pointer, nopass :: solve ! exact solution  
  contains
    procedure, pass :: init ! initialize object
  end type ice_type


! TEMPLATES !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the bc functions
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine bc_tmpl(topo_edge, topo_intr, topo_oppo, topo_bnd, &
                       ice_edge, ice_intr, ice_oppo, ice_bnd)
      import :: rp                                       ! use special types
      real(rp), intent(in) :: topo_edge(:), ice_edge(:)  ! domain edge 
      real(rp), intent(in) :: topo_intr(:), ice_intr(:)  ! domain edge-1
      real(rp), intent(in) :: topo_oppo(:), ice_oppo(:)  ! opposite domain edge
      real(rp), intent(in) :: topo_bnd(:), ice_bnd(:)    ! bc ghost points
    end subroutine bc_tmpl 
  end interface


  ! ---------------------------------------------------------------------------
  ! TEMPLATE: common form for the ice model method
  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine update_tmpl(g, p, s)
      import :: ice_type, param_type, state_type ! use special types
      class(ice_type), intent(in) :: g           ! procedures
      type(param_type), intent(in) :: p          ! parameters
      type(state_type), intent(inout) :: s       ! state vars
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


! INITIALIZE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! SUB: Initialize ice model object 
  ! ---------------------------------------------------------------------------
  subroutine init(g, p)

    class(ice_type), intent(out) :: g
    type(param_type), intent(in) :: p

    character(len=100) :: nbc_name, ebc_name, sbc_name, wbc_name
    integer :: comma(3)

    ! Set ice flow method procedure and check its parameters for sanity
    select case (p%ice_name)

      case ('none')
        g%on = .false.
        g%update => NULL() ! will seg-fault if called, by design

      case ('hindmarsh2_explicit')
        call set_hindmarsh2_explicit(g, p)
    
      case default
        print *, "Invalid name for ice model method: " // trim(p%ice_name)
        stop 'Stopped.'

    end select

    ! Set exact solution procedure and check its parameters for sanity
    select case (p%ice_soln_name)

      case ('none')
        g%on_soln = .false.
        g%solve => NULL() ! will seg-fault if called, by design

      case('bueler_isothermal_a')
        call set_bueler_isothermal_a(g, p)

      case('bueler_isothermal_b')
        call set_bueler_isothermal_b(g, p)

      case default
        print *, "Invalid name for exact solution: " // trim(p%ice_soln_name)
        stop 

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

      case ('no_ice')
        ptr => bc_no_ice ! will fail if called, by design. 
    
      case default
        print *, "Invalid name for boundary condition: " // trim(str)
        stop 
   
    end select

  end subroutine set_bc_pointer


! BOUNDARY CONDITIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! SUB: BC, no ice, flat topography
  ! ---------------------------------------------------------------------------
  subroutine bc_no_ice(topo_edge, topo_intr, topo_oppo, topo_bnd, &
                       ice_edge, ice_intr, ice_oppo, ice_bnd) 
    real(rp), intent(in) :: topo_edge(:), ice_edge(:)  ! domain edge 
    real(rp), intent(in) :: topo_intr(:), ice_intr(:)  ! domain edge-1
    real(rp), intent(in) :: topo_oppo(:), ice_oppo(:)  ! opposite domain edge
    real(rp), intent(out) :: topo_bnd(:), ice_bnd(:)   ! bc ghost points

    topo_bnd = topo_edge
    ice_bnd = 0.0_rp

  end subroutine bc_no_ice 


! ICE FLOW !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! SUB: Isothermal shallow ice flow flow using Hindmarsh "method 2" stencil, no
  !   basal slinding and explicit adaptive timestep. Enabled if ice_name is
  !   'hindmarsh2_explicit'
  !
  ! Spatial discretization: Hindmarsh "method 2" stencil (see [1] and references
  !   therein), also commonly refered to as the Mahaffy method (see [2]). Ice flux
  !   is computed on at midpoints staggered in the flow direction only.
  !
  ! Time discretization: Explicit time stepping with an adaptive timestep taken
  !   (see eq 103 in [3]) 
  !
  ! Parameters:
  !   (1) A: isothermal ice deformation parameter [Pa-3 a-1] 
  !
  ! References:
  ! [1] Hindmarsh, R. C. A., & Payne, A. (1996). Time-step limits for stable
  !   solutions of the ice-sheet equation. Annals of Glaciology, 17(4), 391-412.
  !   doi:10.1177/030913339301700401
  ! [2] Mahaffy, M. W. (1976). A Three-Dimensional Numerical Model of Ice Sheets:
  !   Tests on the Barnes Ice Cap, Northwest Territories. Journal of Geophysical
  !   Research, 81(6), 1059-1066. doi:10.1029/JC081i006p01059
  ! [3] Hindmarsh, R. C. A. (2001). Notes on basic glaciological computational
  !   methods and algorithms. In B. Straughan, R. Greve, H. Ehrentraut, & Y. Wang
  !   (Eds.), Continuum Mechanics and Applications in Geophysics and the
  !   Environment (pp.  222–249). Springer Berlin Heidelberg.
  !   doi:10.1007/978-3-662-04439-1_13
  ! ---------------------------------------------------------------------------

  subroutine set_hindmarsh2_explicit(g, p)
    
    type(ice_type), intent(inout) :: g
    type(param_type), intent(in) :: p

    g%on = .true.
    g%update => update_hindmarsh2_explicit

    if(p%ice_param(1) .le. 0.0_rp) then
      print *, 'Invalid ice model parameter: flow law coefficient A must &
               &be positive'
      stop
    end if 

  end subroutine set_hindmarsh2_explicit


  subroutine update_hindmarsh2_explicit(ice, prm, sta)

    class(ice_type), intent(in) :: ice
    type(param_type), intent(in) :: prm
    type(state_type), intent(inout) :: sta

    ! saved vars (init once)
    logical, save :: init ! flag indicating if saved vars have been initialized
    real(rp), save :: A ! isothermal ice deformation parameter [Pa-3 a-1] 
    real(rp), allocatable, save :: h(:,:) ! ice thickness, w/ ghost pts
    real(rp), allocatable, save :: s(:,:) ! ice/bedrock surface elev, w/ ghost pts
    real(rp), allocatable, save :: Dx(:,:) ! diffusivity at x-midpoints 
    real(rp), allocatable, save :: Dy(:,:) ! diffusivity at y-midpoints
    real(rp), allocatable, save :: qx(:,:) ! ice flux at x-midpoints
    real(rp), allocatable, save :: qy(:,:) ! ice flux at y-midpoints

    ! unsaved vars
    real(rp) :: t, dt 
    
    ! init, first time only
    if (.not. init) then
      A = prm%ice_param(1)
      allocate(h(prm%nx+2, prm%ny+2))
      allocate(s(prm%nx+2, prm%ny+2))
      allocate(Dx(prm%nx+1, prm%ny))
      allocate(qx(prm%nx+1, prm%ny))
      allocate(Dy(prm%nx, prm%ny+1))
      allocate(qy(prm%nx, prm%ny+1))
      init = .true.
    end if

    ! copy in shared values
    h(2:prm%nx+1, 2:prm%ny+1) = sta%ice_h
    s(2:prm%nx+1, 2:prm%ny+1) = sta%ice_h+sta%topo
    
    ! start time stepping
    t = 0.0_rp
    do while (t .lt. prm%time_step)

      ! boundary conditions

      ! diffusivity and ice flux

      ! thickness rate of change 
   
    end do
    ! end time stepping 

    ! velocities

    ! copy out shared variables

  end subroutine update_hindmarsh2_explicit


! EXACT SOLUTIONS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  ! ---------------------------------------------------------------------------
  ! Bueler et al 2005 test A methods. Enabled if ice_soln_name is 
  !   'bueler_isothermal_a'. 
  !
  ! Parameters:
  !   (1) M0: constant positive balance rate within the ice cap
  !   (2) L: constant fixed radius of the ice cap 
  !   (3) A: constant ice deformation flow factor
  !
  ! References:
  ! [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman, L.
  ! N. (2005). Exact solutions and verification of numerical models for isothermal
  ! ice sheets. Journal of Glaciology, 51(173), 291-306.
  ! doi:10.3189/172756505781829449
  ! ---------------------------------------------------------------------------

  subroutine set_bueler_isothermal_a(g, p)

    type(ice_type), intent(inout) :: g
    type(param_type), intent(in) :: p

    g%on_soln = .true.
    g%solve => solve_bueler_isothermal_a

    ! M0 must be positive
    if (p%ice_soln_param(1) .le. 0.0_rp) then
      print *, 'Invalid ice solution parameters: method requires a positive M0'
      stop
    end if

    ! L must be positive
    if (p%ice_soln_param(2) .le. 0.0_rp) then
      print *, 'Invalid ice solution parameters: method requires a positive L'
      stop 
    end if

    ! A must be positive
    if (p%ice_soln_param(3) .le. 0.0_rp) then
      print *, 'Invalid ice solution parameters: method requires a positive A'
      stop 
    end if

  end subroutine set_bueler_isothermal_a


  subroutine solve_bueler_isothermal_a(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    ! saved vars (init once)
    logical, save :: set ! flag indicating if param have been set
    real(rp), save :: M0 ! constant positive surface ice flux [m_ice/a]
    real(rp), save :: L ! fixed ice cap radius [m]
    real(rp), save :: A ! isothermal ice deformation parameter [Pa-3 a-1] 
    real(rp), allocatable, save :: r(:,:) ! distance from corner [m]

    ! unsaved vars
    integer :: i, j
    real(rp) :: c1, c2, gam

    ! init, first time only
    if (.not. set) then
      set = .true.
      M0 = p%ice_soln_param(1)
      L = p%ice_soln_param(2)
      A = p%ice_soln_param(3)
      allocate(r(p%nx, p%ny))
      do j = 1, p%ny
        do i = 1, p%nx
          r(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        end do
      end do
    end if

    ! pre-compute constants
    gam = 2.0_rp/5.0_rp*A*(p%rhoi*p%grav)**3.0_rp
    c1 = (4.0_rp*M0/gam)**(1.0_rp/8.0_rp)
    c2 = L**(4.0_rp/3.0_rp)

    ! solve
    where (r .lt. L) 
      s%ice_h_soln = c1*(c2-r**(4.0_rp/3.0_rp))**(3.0_rp/8.0_rp)
    elsewhere
      s%ice_h_soln = 0.0_rp
    end where

  end subroutine solve_bueler_isothermal_a


  ! -----------------------------------------------------------------------------
  ! Bueler et al 2005 test B methods. Enabled if ice_soln_name is
  ! 'bueler_isothermal_b'
  !
  ! Parameters (see [1] for definitions):
  !   (1) alpha
  !   (2) beta 
  !   (3) H0
  !   (4) R0
  !   (5) t0
  !
  ! References:
  ! [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman, L.
  !   N. (2005). Exact solutions and verification of numerical models for
  !   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
  !   doi:10.3189/172756505781829449
  ! ----------------------------------------------------------------------------

  subroutine set_bueler_isothermal_b(g, p)

    type(ice_type), intent(inout) :: g
    type(param_type), intent(in) :: p

    g%on_soln = .true.
    g%solve => solve_bueler_isothermal_b

    ! H0 must be positive
    if (p%ice_soln_param(3) .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive H0'
      stop 
    end if

    ! R0 must be positive
    if (p%ice_soln_param(4) .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive R0'
      stop
    end if

    ! t0 must be positive
    if (p%ice_soln_param(5) .le. 0.0_rp) then
      print *, 'Invalid parameters: method requires a positive t0'
      stop 
    end if

  end subroutine set_bueler_isothermal_b


  subroutine solve_bueler_isothermal_b(p, s)

    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s

    ! save vars (init once)
    logical, save :: set 
    real(rp), save :: alpha
    real(rp), save :: beta
    real(rp), save :: H0
    real(rp), save :: R0
    real(rp), save :: t0
    real(rp), allocatable, save :: r(:,:) ! distance from corner [m]

    ! unsaved vars
    integer :: i, j
    real(rp) :: e1, e2, Hd, Rm 

    ! init, first time only
    if (.not. set) then 
      set = .true.
      alpha = p%ice_soln_param(1)
      beta = p%ice_soln_param(2)
      H0 = p%ice_soln_param(3)
      R0 = p%ice_soln_param(4)
      t0 = p%ice_soln_param(5)
      allocate(r(p%nx, p%ny))
      do j = 1, p%ny
        do i = 1, p%nx
          r(i,j) = sqrt(s%x(i)*s%x(i)+s%y(j)*s%y(j))
        end do
      end do
    end if

    ! compute constants
    Rm = R0*(s%time_now/t0)**beta ! current ice cap radius
    Hd = H0*(s%time_now/t0)**(-alpha) ! current dome height
    e1 = 4.0_rp/3.0_rp
    e2 = 3.0_rp/7.0_rp

    ! solve
    where (r .lt. Rm)
      s%ice_h_soln = Hd*(1.0_rp-(r/Rm)**e1)**e2
    elsewhere
      s%ice_h_soln = 0.0_rp
    end where

  end subroutine solve_bueler_isothermal_b


end module ice_mod
