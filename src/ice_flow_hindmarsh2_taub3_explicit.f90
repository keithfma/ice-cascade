! =============================================================================
! Ice flow procedures for hindmarsh2_taub3_explicit method
!
! Description: isothermal shallow ice flow flow using Hindmarsh "method 2"
!   stencil, basal sliding using a power law sliding function (sliding_velocity =
!   sliding_coefficient*basal_shear_stress**3), and explicit adaptive timestep.
!   Enabled if ice_name is hindmarsh2_taub3_explicit.
! 
! Spatial discretization: Hindmarsh "method 2" stencil (see [1] and references
!   therein), also commonly refered to as the Mahaffy method (see [2]). Ice flux
!   is computed on at midpoints staggered in the flow direction only.
!
! Time discretization: Explicit time stepping with an adaptive timestep taken
!   (see eq 103 in [3]) 

! NOTE: ice deformation parameter is read from the state variable ice_a_defm
!  with units [1/Pa3/a]. If this variable is not provided in the input file, the
!  default value of 0.0 will be used, and the ice will not deform.
!
! NOTE: ice sliding parameter is read from the state variable ice_a_slid
!  with units [m/a/Pa**3]. If this variable is not provided in the input file, the
!  default value of 0.0 will be used, and the ice will not slide.
!
! Parameters:
!   None.  
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
!
! Public: init_hindmarsh2_taub3_explicit, flow_hindmarsh2_taub3_explicit,
!         velo_hindmarsh2_taub3_explicit
! 
! =============================================================================
