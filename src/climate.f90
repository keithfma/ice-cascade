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
    logical                        :: on        ! enable/disable model component
    integer                        :: nx        ! num grid points in x-dir, [1]
    integer                        :: ny        ! num grid points in y-dir, [1]
    character(len=100)             :: tName     ! surface temperature model name
    character(len=100)             :: pName     ! precipitation model name
    real(dp), dimension(10)        :: tParam    ! temperature model parameters, [various]
    real(dp), dimension(10)        :: pParam    ! precipitation model parameters, [various]
    real(dp), allocatable          :: x(:)      ! x coordinate vector, [m]
    real(dp), allocatable          :: y(:)      ! y coordinate vector, [m]
    real(dp), allocatable          :: t(:,:)    ! surface temperature, [C]
    real(dp), allocatable          :: p(:,:)    ! precipitation rate, [m/a]
    procedure(tp), pointer, pass :: getTemp   ! compute temperature
    procedure(tp), pointer, pass :: getPrecip ! compute precipitation
    logical                        :: write_t   ! enable/disable writing temperature
    logical                        :: write_p   ! enable/disable writing precip
  contains
   procedure, pass                 :: init      ! initialize components 
   procedure, pass                 :: run       ! run the climate model 
  end type climate_type


  ! ---------------------------------------------------------------------------
  ! SUB TEMPLATE: common form for temperature and precipitation subroutines
  ! ---------------------------------------------------------------------------
  abstract interface
    subroutine tp(c, time, z) 
      import                             :: dp, climate_type ! use special types
      class(climate_type), intent(inout) :: c                ! climate object to update
      real(dp), intent(in)               :: time             ! model time, [a]
      real(dp), intent(in)               :: z(:,:)           ! surface elevation, [m]
    end subroutine tp 
  end interface

contains


  ! ---------------------------------------------------------------------------
  ! SUB: initialize a climate model object
  !   Note: tName, pName, tParam, pParam, write_t, and write_p must be set
  ! ---------------------------------------------------------------------------
  subroutine init(c, g)
    
    class(climate_type), intent(inout) :: c ! climate modekl object to init
    class(grid_type), intent(in)       :: g ! coordinate grid information

    ! model disabled, clear all object components
    if (c%on .eqv. .false.) then
    
      c%nx = -1
      c%ny = -1
      c%tName = 'none'
      c%pName = 'none'
      c%tParam(:) = -1.0_dp
      c%pParam(:) = -1.0_dp
      if (allocated(c%t) .eqv. .true.) deallocate(c%t)
      allocate(c%t(1,1))
      c%t(1,1) = -1.0_dp
      if (allocated(c%p) .eqv. .true.) deallocate(c%p)
      allocate(c%p(1,1))
      c%p(1,1) = -1.0_dp
      if (allocated(c%x) .eqv. .true.) deallocate(c%x)
      allocate(c%x(1))
      c%x = -1.0_dp
      if (allocated(c%y) .eqv. .true.) deallocate(c%y)
      allocate(c%y(1))
      c%y = -1.0_dp
      c%getTemp => NULL()
      c%getPrecip => NULL()
      c%write_t = .false.
      c%write_p = .false.
   
    ! model enabled, init components
    else

      c%nx = g%nx
      c%ny = g%ny
      if (allocated(c%t) .eqv. .true.) deallocate(c%t)
      allocate(c%t(g%nx+2, g%ny+2))
      if (allocated(c%p) .eqv. .true.) deallocate(c%p)
      allocate(c%p(g%nx+2, g%ny+2))
      if (allocated(c%x) .eqv. .true.) deallocate(c%x)
      allocate(c%x(g%nx+2))
      c%x = g%x
      if (allocated(c%y) .eqv. .true.) deallocate(c%y)
      allocate(c%y(g%ny+2))
      c%y = g%y

      ! parse surface temperature model name, set procedure pointer
      select case (c%tName)
        case ("constant")
          c%getTemp => t_constant
        case default
          print *, "Invalid name for surface temperature model name: ", trim(c%tName)
          stop -1
      end select

      ! parse precipitation model name, set procedure pointer
      select case (c%pName)
        case ("constant")
          c%getPrecip => p_constant
        case default
          print *, "Invalid name for precipitation model name: ", trim(c%pName)
          stop -1
      end select
      
    end if
  
  end subroutine init


  ! ---------------------------------------------------------------------------
  ! SUB: evaluate the climate model for a specified time and topography
  ! ---------------------------------------------------------------------------
  subroutine run(c, time, z)
    
    class(climate_type), intent(inout) :: c      ! climate to evaluate and set
    real(dp), intent(in)               :: time   ! current model time, [a]
    real(dp), intent(in)               :: z(:,:) ! surface elevation, [m]

    call c%getTemp(time, z)
    call c%getPrecip(time, z)
    
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

end module climate_module
