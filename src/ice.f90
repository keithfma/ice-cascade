! =============================================================================
! Glacier dynamics model component for ice-cascade
!
! Public:
!   on_ice: logical, enable/disable model component
!   on_ice_soln: logical, enable/disable exact solution
!   init_ice: subroutine, select and intialize model procedures
!   update_ice: subroutine, run ice model for one timestep
!   solve_ice: procedure, selected exact solution procedure
!
! Private:
!   bc_tmpl: template, common form for bc procedures 
!   flow_tmpl: template, common form for numerical ice flow procedures
!   solve_tmpl: template, common form for exact solution procedures
!   sbc, nbc, wbc, ebc: procedure ptrs, selected bc procedures
!   flow: procedure ptr, selected numerical ice flow procedure
!   volume: function, compute ice volume using eq. 31 in ref [1]
!
! References:
!   [1] Bueler, E., Lingle, C. S., Kallen-Brown, J. A., Covey, D. N., & Bowman,
!   L.  N. (2005). Exact solutions and verification of numerical models for
!   isothermal ice sheets. Journal of Glaciology, 51(173), 291-306.
!   doi:10.3189/172756505781829449
!   
! ============================================================================

module ice

use kinds, only: rp
use param, only: param_type
use state, only: state_type
use ice_bc_no_ice ! nbc_, ebc_, sbc_, wbc_ 
use ice_bc_no_flux ! nbc_, ebc_, sbc_, wbc_ 
use ice_bc_mirror ! nbc_, ebc_, sbc_, wbc_ 
use ice_flow_hindmarsh2_explicit ! init_, flow_, velo_
use ice_flow_hindmarsh2_sliding1_explicit !init_, flow_, velo_
use ice_soln_bueler_isothermal_a ! init_, solve_
use ice_soln_bueler_isothermal_b ! init_, solve_ 
use ice_soln_bueler_isothermal_c ! init_, solve_
use ice_soln_bueler_isothermal_d ! init_, solve_
use ice_soln_bueler_isothermal_e ! init_, solve_

implicit none
private
public :: on_ice, init_ice, update_ice


  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine bc_tmpl(s)
      import :: state_type ! use special types
      type(state_type), intent(inout) :: s   
    end subroutine bc_tmpl 
  end interface
  !
  ! ABOUT: Template, common form for the bc functions
  ! ---------------------------------------------------------------------------


  ! ---------------------------------------------------------------------------
  abstract interface 
    function flow_tmpl(p, s) result(dt)
      import :: rp, param_type, state_type ! use special types
      type(param_type), intent(in) :: p          ! parameters
      type(state_type), intent(inout) :: s       ! state vars
      real(rp) :: dt ! timestep
    end function flow_tmpl 
  end interface
  !
  ! ABOUT: Template, common form for the numerical ice flow routine
  !
  !   Flow routines compute a numerical approximation to the ice flow equations,
  !   including sliding, temperature, and hydrology, where applicable. They MUST
  !   set the value of the ice_h_dot variable, and may set the value of other
  !   state variables as well.
  ! ---------------------------------------------------------------------------


  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine velo_tmpl(p, s) 
      import :: param_type, state_type ! use special types
      type(param_type), intent(in) :: p ! parameters
      type(state_type), intent(inout) :: s ! state vars
    end subroutine velo_tmpl 
  end interface
  !
  ! ABOUT: Template, common form for computing velocities using the numerical
  !   ice flow algorithm. Updates ice deformation and sliding velocity grids  
  ! ---------------------------------------------------------------------------


  ! ---------------------------------------------------------------------------
  abstract interface 
    subroutine solve_tmpl(p, s)
      import :: param_type, state_type     ! use special types
      type(param_type), intent(in)    :: p ! parameters
      type(state_type), intent(inout) :: s ! state vars
    end subroutine solve_tmpl 
  end interface
  !
  ! ABOUT: Template, common form for the exact solution subroutines
  ! ---------------------------------------------------------------------------


  ! ---------------------------------------------------------------------------
  logical :: on_ice ! enable/disable model
  logical :: on_ice_soln ! enable/disable exact solution
  procedure(bc_tmpl), pointer :: nbc ! apply north BC, sets (:,end)
  procedure(bc_tmpl), pointer :: sbc ! apply south BC, sets (:,1)
  procedure(bc_tmpl), pointer :: ebc ! apply east BC, sets (end,:)
  procedure(bc_tmpl), pointer :: wbc ! apply west BC, sets (1,:)
  procedure(flow_tmpl), pointer :: flow ! ice model method
  procedure(velo_tmpl), pointer :: velo ! ice velocity method
  procedure(solve_tmpl), pointer :: solve_ice ! exact solution  
  !
  ! ABOUT: Shared procedures and variables, set in init_ice 
  ! ---------------------------------------------------------------------------


contains


  ! ---------------------------------------------------------------------------
  subroutine init_ice(p, s)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
  !
  ! ABOUT: initialize procedures and vars
  ! ---------------------------------------------------------------------------

    character(len=100), dimension(4) :: bc_names
    integer :: comma(3), i

    ! select BC procedures
    read(p%ice_bc_name,*) bc_names ! n,e,s,w

    do i = 1, 4
      select case (bc_names(i))

        case ('none') ! will seg-fault if called, by design 
          if (i .eq. 1) nbc => NULL() 
          if (i .eq. 2) ebc => NULL() 
          if (i .eq. 3) sbc => NULL() 
          if (i .eq. 4) wbc => NULL() 

        case ('no_ice')
          if (i .eq. 1) nbc => nbc_no_ice 
          if (i .eq. 2) ebc => ebc_no_ice
          if (i .eq. 3) sbc => sbc_no_ice
          if (i .eq. 4) wbc => wbc_no_ice

        case ('no_flux')
          if (i .eq. 1) nbc => nbc_no_flux 
          if (i .eq. 2) ebc => ebc_no_flux
          if (i .eq. 3) sbc => sbc_no_flux
          if (i .eq. 4) wbc => wbc_no_flux

        case ('mirror')
          if (i .eq. 1) nbc => nbc_mirror 
          if (i .eq. 2) ebc => ebc_mirror
          if (i .eq. 3) sbc => sbc_mirror
          if (i .eq. 4) wbc => wbc_mirror

        case default
          print *, "Invalid name for boundary condition: " // trim(bc_names(i))
          stop 

      end select
    end do

    ! select flow procedure
    select case (p%ice_name)

      case ('none') 
        on_ice = .false. 
        flow => NULL() ! will seg-fault if called, by design

      case ('hindmarsh2_explicit')
        on_ice = .true.
        call init_hindmarsh2_explicit(p, s)
        flow => flow_hindmarsh2_explicit 
        velo => velo_hindmarsh2_explicit

      case ('hindmarsh2_sliding1_explicit')
        on_ice = .true.
        call init_hindmarsh2_sliding1_explicit(p, s)
        flow => flow_hindmarsh2_sliding1_explicit 
        velo => velo_hindmarsh2_sliding1_explicit

      case default
        print *, "Invalid name for glacier flow method: " // trim(p%ice_name)
        stop 

    end select

    ! select exact solution procedure
    select case (p%ice_soln_name)

      case ('none') 
        on_ice_soln = .false. 
        solve_ice => NULL() ! will seg-fault if called, by design

      case('bueler_isothermal_a')
        on_ice_soln = .true.
        call init_bueler_isothermal_a(p, s)
        solve_ice => solve_bueler_isothermal_a

      case('bueler_isothermal_b')
        on_ice_soln = .true.
        call init_bueler_isothermal_b(p, s)
        solve_ice => solve_bueler_isothermal_b

      case('bueler_isothermal_c')
        on_ice_soln = .true.
        call init_bueler_isothermal_c(p, s)
        solve_ice => solve_bueler_isothermal_c

      case('bueler_isothermal_d')
        on_ice_soln = .true.
        call init_bueler_isothermal_d(p, s)
        solve_ice => solve_bueler_isothermal_d

      case('bueler_isothermal_e')
        on_ice_soln = .true.
        call init_bueler_isothermal_e(p, s)
        solve_ice => solve_bueler_isothermal_e

      case default
        print *, "Invalid name for glacier exact solution: " // trim(p%ice_soln_name)
        stop 

    end select

  end subroutine init_ice


  ! ---------------------------------------------------------------------------
  subroutine update_ice(p, s, write_now)
  !
    type(param_type), intent(in) :: p
    type(state_type), intent(inout) :: s
    logical, intent(in) :: write_now ! .true. if this step will be written to output 
  !
  ! ABOUT: run ice model for one timestep
  ! ---------------------------------------------------------------------------

    logical :: write_velo
    integer :: i, j ! debug
    real(rp) :: t, dt

    t = 0.0_rp
    do while (t .lt. s%step) 

      ! init grids
      call nbc(s)
      call ebc(s)
      call sbc(s)
      call wbc(s)

      s%surf = s%topo+s%ice_h

      ! ice flow procedure
      dt = flow(p, s)
      dt = min(dt, s%step-t) ! trim last step

      ! update
      t = t+dt
      s%ice_h = s%ice_h+dt*(s%ice_h_dot+s%ice_q_surf)
      s%ice_h = max(s%ice_h, 0.0_rp)

    end do
      
    ! update other variables for the output file 
    if (write_now) then
  
      ! ice area and volume
      if (p%write_ice_area_vol .eq. 1) then
        s%ice_vol = volume(s%ice_h, p%dx, p%dy)
        s%ice_area = real(count(s%ice_h .gt. 0.0_rp), rp)*p%dx*p%dy
      end if

      ! ice velocities
      if ((p%write_ice_uv_defm .eq. 1) &
          .or. (p%write_ice_uv_slid .eq. 1)) call velo(p, s) 

      ! exact solution
      if (on_ice_soln) call solve_ice(p, s)

    end if

  end subroutine update_ice


  ! ---------------------------------------------------------------------------
  function volume(thk, dx, dy) result(vol)
  ! 
    real(rp), intent(in) :: thk(:,:) ! ice thickness, [m]
    real(rp), intent(in) :: dx, dy ! grid spacing, [m]
    real(rp) :: vol ! ice volume, [m3]
  !  
  ! ABOUT: compute ice volume using trapezoidal rule (ref [1], eq. 31) and
  !   compensated summation (Kahan algorithm), ignore ghost points
  ! ---------------------------------------------------------------------------

    integer :: i, j
    real(rp) :: thk_sum, wrk1, wrk2, corr

    thk_sum = 0.0_rp
    corr = 0.0_rp
    do j = 2, size(thk, 2)-1
      do i = 2, size(thk, 1)-1
        wrk1 = thk(i,j)-corr
        wrk2 = thk_sum+wrk1
        corr = (wrk2-thk_sum)-wrk1
        thk_sum = wrk2
      end do
    end do

    vol = thk_sum*dx*dy

    return

  end function


end module ice
