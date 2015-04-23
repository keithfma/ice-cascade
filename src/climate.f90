! =============================================================================
! Climate model, including variables and procedures
!
! Contains:
!   type climate_type (public)
!   NOTE: list is not yet complete
! ============================================================================

module climate_mod

use kinds_mod, only: rp

implicit none
private
public climate_type

  ! --------------------------------------------------------------------------- 
  ! TYPE: all variables and procedures for the climate model component
  ! ---------------------------------------------------------------------------
  type climate_type
    logical :: on_temp ! enable/disable temperature
    logical :: on_precip ! enable/disable precipitation
    logical :: on_ice_q_surf ! enable/disable surface ice flux
    logical :: on_runoff ! enable/disable runoff
    character(len=100) :: name_temp ! temperature model name
    character(len=100) :: name_precip ! precipitation model name
    character(len=100) :: name_ice_q_surf ! surface ice flux model name
    character(len=100) :: name_runoff ! runoff model name
    real(rp), dimension(10) :: param_temp ! temperature model param, [various]
    real(rp), dimension(10) :: param_precip !  model param, [various]
    real(rp), dimension(10) :: param_ice_q_surf !  model param, [various]
    real(rp), dimension(10) :: param_runoff !  model param, [various]
  end type climate_type

contains

end module climate_mod
