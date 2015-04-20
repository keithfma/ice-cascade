! =============================================================================
! Climate model, including variables and procedures
!
! Contains:
!   type climate_type (public)
!   NOTE: list is not yet complete
! ============================================================================

module climate_module

use types, only: dp
use grid_module, only: grid_type

implicit none
private
public climate_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the climate model component
  ! ---------------------------------------------------------------------------
  type climate_type
    logical                       :: on        ! enable/disable 
    logical                       :: on_t      ! enable/disable surf temp component
    logical                       :: on_p      ! enable/disable precip component
    logical                       :: on_i      ! enable/disable surf ice flux component
    integer                       :: nx        ! num grid points in x-dir, [1]
    integer                       :: ny        ! num grid points in y-dir, [1]
    real(dp)                      :: rhoi      ! density of glacial ice, [kg/m3]
    character(len=100)            :: tName     ! surface temperature model name
    character(len=100)            :: pName     ! precipitation model name
    character(len=100)            :: iName     ! surface ice flux model name
    real(dp), dimension(10)       :: tParam    ! temperature model parameters, [various]
    real(dp), dimension(10)       :: pParam    ! precipitation model parameters, [various]
    real(dp), dimension(10)       :: iParam    ! surface ice flux model parameters, [various]
    real(dp), allocatable         :: x(:)      ! x coordinate vector, [m]
    real(dp), allocatable         :: y(:)      ! y coordinate vector, [m]
    real(dp), allocatable         :: t(:,:)    ! surface temperature, [C]
    real(dp), allocatable         :: p(:,:)    ! precipitation rate, [m/a]
    real(dp), allocatable         :: i(:,:)    ! surface ice flux, [m_ice/a]
    procedure(tpi), pointer, pass :: getTemp   ! compute temperature
    procedure(tpi), pointer, pass :: getPrecip ! compute precipitation
    procedure(tpi), pointer, pass :: getIce    ! compute surface ice flux
    logical                       :: write_t   ! enable/disable writing temperature
    logical                       :: write_p   ! enable/disable writing precip
    logical                       :: write_i   ! enable/disable writing surface ice flux
  contains
   procedure, pass                :: init      ! initialize components 
   procedure, pass                :: run       ! run the climate model 
  end type climate_type


  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for surface temperature, precipitation, and
  !   surface ice flux subroutines
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine tpi(c, time, z) 
      import                             :: dp, climate_type ! use special types
      class(climate_type), intent(inout) :: c                ! climate object to update
      real(dp), intent(in)               :: time             ! model time, [a]
      real(dp), intent(in)               :: z(:,:)           ! surface elevation, [m]
    end subroutine tpi 
  end interface

contains


  ! ---------------------------------------------------------------------------
  ! SUB: initialize a climate model object
  !   Note: tName, pName, tParam, pParam, write_t, and write_p must be set
  ! ---------------------------------------------------------------------------
  subroutine init(c, g)
    
    class(climate_type), intent(inout) :: c ! climate modekl object to init
    class(grid_type), intent(in)       :: g ! coordinate grid information

    ! object is already initialized, exit with error
    if (allocated(c%x)) then
      print *, 'Attempted to initialize ice_type object twice, exiting.'
      stop -1
    end if

    ! climate model common components
    if (c%on_t .or. c%on_p .or. c%on_i) then
      allocate(c%x(g%nx+2))
      allocate(c%y(g%ny+2))
      c%nx = g%nx
      c%ny = g%ny
      c%x = g%x
      c%y = g%y
    else
      allocate(c%x(1))
      allocate(c%y(1))
      c%nx = -1
      c%ny = -1
      c%x = -1.0_dp
      c%y = -1.0_dp
    end if

    ! surface temperature model components
    if (c%on_t) then
      allocate(c%t(g%nx+2, g%ny+2))
    else
      allocate(c%t(1,1))
      c%tName = 'none'
      c%tParam(:) = -1.0_dp
      c%t(1,1) = -1.0_dp
      c%write_t = .false.
    end if
    
    ! precipitation model componenets
    if (c%on_p) then
      allocate(c%p(g%nx+2, g%ny+2))
    else
      allocate(c%p(1,1))
      c%pName = 'none'
      c%pParam(:) = -1.0_dp
      c%p(1,1) = -1.0_dp
      c%write_p = .false.
    end if

    ! surface ice flux model components
    if (c%on_i) then
      allocate(c%i(g%nx+2, g%ny+2))
    else
      allocate(c%i(1,1))
      c%iName = 'none'
      c%iParam(:) = -1.0_dp
      c%i(1,1) = -1.0_dp
      c%write_i = .false.
    end if

    ! parse surface temperature model name, set procedure pointer
    select case (c%tName)

      case ("none") 
        c%getTemp => NULL()
    
      case ("constant")
        c%getTemp => t_constant
      
      case ("sawtooth_lapse")
        c%getTemp => t_sawtooth_lapse
      
      case default
        print *, "Invalid name for surface temperature model name: ", trim(c%tName)
        stop -1
    end select

    ! parse precipitation model name, set procedure pointer
    select case (c%pName)
      
      case ("none")
        c%getPrecip => NULL()
      
      case ("constant")
        c%getPrecip => p_constant
      
      case default
        print *, "Invalid name for precipitation model name: ", trim(c%pName)
        stop -1
    end select

    ! parse surface ice flux model name, set procedure pointer
    select case (c%iName)
      
      case ("none")
        c%getIce => NULL()
      
      case ("constant")
        c%getIce => i_constant
      
      case ("linear_temp_capped_max_min")
        if (c%on_t .eqv. .false.) then
          print *, 'Invalid combination of climate model parameters: this surface ice flux model requires a temperature model.'
          stop -1
        end if
        c%getIce => i_linear_temp_capped_max_min

      case default 
        print *, 'Invalid name for surface ice flux model: ', trim(c%iName)
        stop -1
    end select
  
  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: evaluate the climate model for a specified time and topography
  ! ---------------------------------------------------------------------------
  subroutine run(c, time, z)
    
    class(climate_type), intent(inout) :: c      ! climate to evaluate and set
    real(dp), intent(in)               :: time   ! current model time, [a]
    real(dp), intent(in)               :: z(:,:) ! surface elevation, [m]

    if (c%on_t) call c%getTemp(time, z)
    if (c%on_p) call c%getPrecip(time, z)
    if (c%on_i) call c%getIce(time, z)
    
  end subroutine run


  ! ---------------------------------------------------------------------------
  ! SUB: Surface temperature model, constant in space and time
  !   Parameters:
  !     c%tParam(1) = temperature, [C]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine t_constant(c, time, z) 

    class(climate_type), intent(inout) :: c      ! climate object to update
    real(dp), intent(in)               :: time   ! model time, [a] (unused)
    real(dp), intent(in)               :: z(:,:) ! surface elev, [m] (unused)
  
    c%t = c%tParam(1)    

  end subroutine t_constant 


  ! ---------------------------------------------------------------------------
  ! SUB: Surface temperature model, sawtooth in time, linear in elevation
  !
  !   Sea level temperature is a sawtooth function that begins at the maximum
  !   value at time = 0, decreases to the minimum value over a fixed duration
  !   cooling period, then increases to the maximum temperature over a fixed
  !   duration warming period. Surface temperature is related to sea level
  !   temperature by a simple liear lase rate (T = T_sl-lapse*elev)
  !   
  !   Parameters:
  !     c%tParam(1) = maximum sea-level temperature, [C]
  !     c%tParam(2) = minimum sea-level temperature, [C]
  !     c%tParam(3) = duration of cooling period, [a]
  !     c%tParam(4) = duration of warming period, [a]
  !     c%tParam(5) = lapse rate, [C/m]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine t_sawtooth_lapse(c, time, z) 

    class(climate_type), intent(inout) :: c      ! climate object to update
    real(dp), intent(in)               :: time   ! model time, [a] (unused)
    real(dp), intent(in)               :: z(:,:) ! surface elev, [m] (unused)

    real(dp) :: tempSlMax, tempSlMin, dtCool, dtWarm, dt, lapse, tempSl, tprime

    ! gather parameters
    tempSlMax = c%tParam(1)
    tempSlMin = c%tParam(2)
    dtCool = c%tParam(3)
    dtWarm = c%tParam(4)
    dt = dtCool+dtWarm
    lapse = c%tParam(5)
  
    ! compute sea level temperature
    tprime = mod(time, dt)
    if (tprime <= dtCool) then
      tempSl = tempSlMax+(tempSlMin-tempSlMax)/dtCool*tprime
    else
      tempSl = tempSlMin+(tempSlMax-tempSlMin)/dtWarm*(tprime-dtCool)
    end if

    ! compute surface temperature
    c%t = tempSl-lapse*z    

  end subroutine t_sawtooth_lapse 


  ! ---------------------------------------------------------------------------
  ! SUB: Precipitation model, constant in space and time
  !   Parameters:
  !     c%pParam(1) = precipitation rate, [m/a]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine p_constant(c, time, z) 

    class(climate_type), intent(inout) :: c      ! climate object to update
    real(dp), intent(in)               :: time   ! model time, [a] (unused)
    real(dp), intent(in)               :: z(:,:) ! surface elev, [m] (unused)
  
    c%p = c%pParam(1)    

  end subroutine p_constant 
  

  ! ---------------------------------------------------------------------------
  ! SUB: Surface ice flux  model, constant in space and time
  !   Parameters:
  !     c%iParam(1) = surface ice flux, [m_ice/a]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine i_constant(c, time, z) 

    class(climate_type), intent(inout) :: c      ! climate object to update
    real(dp), intent(in)               :: time   ! model time, [a] (unused)
    real(dp), intent(in)               :: z(:,:) ! surface elev, [m] (unused)
  
    c%i = c%iParam(1)    

  end subroutine i_constant 

  ! ---------------------------------------------------------------------------
  ! SUB: Surface ice flux model, linear f(temp), capped min and max
  !
  !   Ice flux scales linearly with temperature, and is 0 where temp = 0 C. The
  !   result is clipped to the maximum and minimum values provided in the
  !   parameters. 
  !
  !   Parameters:
  !     c%iParam(1) = scaling constant, [m_ice/a/C]
  !     c%iParam(2) = maximum surface ice flux, [m_ice/a]
  !     c%iParam(3) = minimum surface ice flux, [m_ice/a]
  !     all others unused.
  ! ---------------------------------------------------------------------------
  subroutine i_linear_temp_capped_max_min(c, time, z) 

    class(climate_type), intent(inout) :: c      ! climate object to update
    real(dp), intent(in)               :: time   ! model time, [a] (unused)
    real(dp), intent(in)               :: z(:,:) ! surface elev, [m] (unused)

    real(dp) :: temp2flux, maxflux, minflux

    ! gather parameters
    temp2flux = c%iParam(1)
    maxflux = c%iParam(2)
    minflux = c%iParam(3)

    ! compute ice flux
    c%i = min(maxflux, max(minflux, temp2flux*c%t))    

  end subroutine i_linear_temp_capped_max_min 

end module climate_module
