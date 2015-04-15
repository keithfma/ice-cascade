! =============================================================================
! Input/Output routines for the ice-cascade landscape evolution model.
!
! Contains:
!   readParams (public): read from the input file specified on the command line
!   
! ============================================================================

module io_module

use types, only: sp, dp, sp_nc, dp_nc, int_nc
use grid_module, only: grid_type
use time_module, only: time_type
use topo_module, only: topo_type
use climate_module, only: climate_type
use ice_module, only: ice_type
use hill_module, only: hill_type
use netcdf

implicit none
private
public :: readParam, createOutfile, writeStep


  ! ---------------------------------------------------------------------------
  ! PARAMETERS
  ! ---------------------------------------------------------------------------
  integer, parameter :: p = dp       ! real kind for output
  integer, parameter :: p_nc = dp_nc ! netcdf real kind for output


contains

  ! ---------------------------------------------------------------------------
  ! SUB: read from the input file specified on the command line
  ! ---------------------------------------------------------------------------
  subroutine readParam (runname, fgrid, time, ftopo, fclimate, fice, fhill)
    
    character(len=*), intent(out)   :: runname  ! name for run
    type(grid_type), intent(out)    :: fgrid    ! high-res grid
    type(time_type), intent(out)    :: time     ! model time vars
    type(topo_type), intent(out)    :: ftopo    ! high-res topography
    type(climate_type), intent(out) :: fclimate ! climate model
    type(ice_type), intent(out)     :: fice     ! ice model
    type(hill_type), intent(out)    :: fhill    ! hilllslope model

    character(len=100) :: infile, line
    real(dp) :: rhoi
    integer :: i, tmp, msg
  	
    ! Get input file name
    select case (command_argument_count())
      case (1)
      	call get_command_argument(1,infile) 
      case default
      	print*,'ICE-CASCADE expects exactly 1 input argument'
      	stop
    end select
    
    ! Copy input to scratch file, dropping comments and empty lines
    open(54, file = trim(infile), status = 'old', iostat = msg)
    if (msg .ne. 0) then
    	print*, 'The input file ', trim(infile), ' does not exist, exiting.'
    	stop
    end if	
    open (55, status = 'scratch')
    do while (msg .ge. 0)
    	read (54, '(a)', iostat = msg) line
    	if ((line(1:1) .ne. '$') .and. (line(1:1) .ne. ' ')) write (55, '(a)') line		
    enddo 
    close (54)
    rewind (55)
    
    ! Read in input parameters
    read(55, *) runname	
    read(55, *) time%start
    read(55, *) time%finish
    read(55, *) time%step
    read(55, *) time%write_period
    read(55, *) rhoi ! shared
    read(55, *) fgrid%nx	
    read(55, *) fgrid%ny
    read(55, *) fgrid%dx
    read(55, *) fgrid%dy	
    read(55, *) ftopo%filename
    read(55, *) ftopo%write_z
    read(55, *) fclimate%on_t 
    read(55, *) fclimate%tName 
    read(55, *) fclimate%tParam(:) 
    read(55, *) fclimate%on_p 
    read(55, *) fclimate%pName
    read(55, *) fclimate%pParam(:) 
    read(55, *) fclimate%on_i 
    read(55, *) fclimate%iName
    read(55, *) fclimate%iParam(:) 
    read(55, *) fclimate%write_t
    read(55, *) fclimate%write_p 
    read(55, *) fclimate%write_i 
    read(55, *) fice%on
    read(55, *) fice%c_b
    read(55, *) fice%flowName
    read(55, *) fice%nbcName
    read(55, *) fice%sbcName
    read(55, *) fice%ebcName
    read(55, *) fice%wbcName
    read(55, *) fice%write_h
    read(55, *) fice%write_uvdefm
    read(55, *) fhill%on 
    read(55, *) fhill%D	
    read(55, *) fhill%nbcName
    read(55, *) fhill%sbcName 
    read(55, *) fhill%ebcName
    read(55, *) fhill%wbcName 
    read(55, *) fhill%solnName 	
    read(55, *) fhill%write_dzdt
    close(55)

    ! assign shared values
    fclimate%rhoi = rhoi

  end subroutine readParam


  ! ---------------------------------------------------------------------------
  ! SUB: create netcdf file and vars for output
  ! ---------------------------------------------------------------------------
  subroutine createOutfile(runname, fgrid, time, ftopo, fclimate, fice,  fhill)

    character(len=*), intent(in)   :: runname
    type(grid_type), intent(in)    :: fgrid
    type(time_type), intent(in)    :: time
    type(topo_type), intent(in)    :: ftopo
    type(climate_type), intent(in) :: fclimate
    type(ice_type), intent(in)     :: fice
    type(hill_type), intent(in)    :: fhill

    logical :: shuf
    integer, dimension(3) :: fchunk
    integer :: i, j, defLvl, msg, id_file, id_dim_fx, id_dim_fy, &
      id_dim_time, id_var
 
    ! define compression and chunking parameters 
    defLvl = 1 ! compression, 0 = none, 9 = max, best value is 1
    fchunk = [fgrid%nx, fgrid%ny, 1]
    shuf = .true.

    ! create new file
    msg = nf90_create(trim(runname)//'.out', nf90_netcdf4, id_file)

    ! write parameters as global attributes
    ! model
    msg = nf90_put_att(id_file, nf90_global, 'model_start_time__a', time%start)
    msg = nf90_put_att(id_file, nf90_global, 'model_end_time__a', time%finish)
    msg = nf90_put_att(id_file, nf90_global, 'model_time_step__a', time%step)
    msg = nf90_put_att(id_file, nf90_global, 'model_output_interval__steps', time%write_period)
    ! constants
    msg = nf90_put_att(id_file, nf90_global, 'const_density_ice__kg1m-3', fclimate%rhoi)
    ! grids
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_nx__1', fgrid%nx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_ny__1', fgrid%ny)
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_dx__m', fgrid%dx)
    msg = nf90_put_att(id_file, nf90_global, 'grid_high_res_dy__m', fgrid%dy)
    ! topo
    msg = nf90_put_att(id_file, nf90_global, 'topo_high_res_name__file', ftopo%filename)
    ! climate
    msg = nf90_put_att(id_file, nf90_global, 'climate_temp_on__tf', merge(1, 0, fclimate%on_t))
    if (fclimate%on_t) then
      msg = nf90_put_att(id_file, nf90_global, 'climate_temp__name', fclimate%tName)
      msg = nf90_put_att(id_file, nf90_global, 'climate_temp_param__various', fclimate%tParam)
    end if
    msg = nf90_put_att(id_file, nf90_global, 'climate_precip_on__tf', merge(1, 0, fclimate%on_p))
    if (fclimate%on_p) then
      msg = nf90_put_att(id_file, nf90_global, 'climate_precip__name', fclimate%pName)
      msg = nf90_put_att(id_file, nf90_global, 'climate_precip_param__various', fclimate%pParam)
    end if
    msg = nf90_put_att(id_file, nf90_global, 'climate_iceflux_on__tf', merge(1, 0, fclimate%on_i))
    if (fclimate%on_i) then
      msg = nf90_put_att(id_file, nf90_global, 'climate_iceflux__name', fclimate%iName)
      msg = nf90_put_att(id_file, nf90_global, 'climate_iceflux_param__various', fclimate%iParam)
    end if
    ! ice
    msg = nf90_put_att(id_file, nf90_global, 'ice_on__tf', merge(1, 0, fice%on))
    if (fice%on) then
      msg = nf90_put_att(id_file, nf90_global, 'ice_defm_coeff__Pa-3a-1', fhill%D)
      msg = nf90_put_att(id_file, nf90_global, 'ice_north_bc__name', trim(fice%nbcName))
      msg = nf90_put_att(id_file, nf90_global, 'ice_south_bc__name', trim(fice%sbcName))
      msg = nf90_put_att(id_file, nf90_global, 'ice_west_bc__name', trim(fice%wbcName))
      msg = nf90_put_att(id_file, nf90_global, 'ice_east_bc__name', trim(fice%ebcName))
      msg = nf90_put_att(id_file, nf90_global, 'ice_flow_method__name', trim(fice%flowName))
    end if
    ! hill
    msg = nf90_put_att(id_file, nf90_global, 'hill_on__tf', merge(1, 0, fhill%on))
    if (fhill%on) then
      msg = nf90_put_att(id_file, nf90_global, 'hill_diffusivity__m2a-1', fhill%D)
      msg = nf90_put_att(id_file, nf90_global, 'hill_north_bc__name', trim(fhill%nbcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_south_bc__name', trim(fhill%sbcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_west_bc__name', trim(fhill%wbcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_east_bc__name', trim(fhill%ebcName))
      msg = nf90_put_att(id_file, nf90_global, 'hill_topo_soln__name', trim(fhill%solnName))
    end if

    ! define dimensions
    msg = nf90_def_dim(id_file, 'fx', fgrid%nx, id_dim_fx) 
    msg = nf90_def_dim(id_file, 'fy', fgrid%ny, id_dim_fy) 
    msg = nf90_def_dim(id_file, 'time', nf90_unlimited, id_dim_time)

    ! create coordinate variables
    msg = nf90_def_var(id_file, 'time', p_nc, id_dim_time, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'model_time')
    msg = nf90_put_att(id_file, id_var, 'units', 'a')

    msg = nf90_def_var(id_file, 'fx', p_nc, id_dim_fx, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'x_coord_high_res')
    msg = nf90_put_att(id_file, id_var, 'units', 'm')
    
    msg = nf90_def_var(id_file, 'fy', p_nc, id_dim_fy, id_var)
    msg = nf90_put_att(id_file, id_var, 'long_name', 'y_coord_high_res')
    msg = nf90_put_att(id_file, id_var, 'units', 'm')

    ! create topography variables
    if (ftopo%write_z) then
     	msg = nf90_def_var(id_file, 'ftopo_z', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'topography_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

    ! create climate variables
    if (fclimate%write_t) then
     	msg = nf90_def_var(id_file, 'fclim_t', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_temperature_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'C')
    end if

    if (fclimate%write_p) then
     	msg = nf90_def_var(id_file, 'fclim_p', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'precipitation_rate_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
    end if

    if (fclimate%write_i) then
     	msg = nf90_def_var(id_file, 'fclim_i', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'surface_ice_flux_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
    end if

    ! create ice variables
    if (fice%write_h) then
     	msg = nf90_def_var(id_file, 'fice_h', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_thickness_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

    if (fice%write_uvdefm) then
     	msg = nf90_def_var(id_file, 'fice_udefm', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_deformation_velocity_xdir_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')

     	msg = nf90_def_var(id_file, 'fice_vdefm', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'ice_deformation_velocity_ydir_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
    end if

    ! create hillslope variables
    if (fhill%write_dzdt) then
     	msg = nf90_def_var(id_file, 'fhill_dzdt', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'hillslope_dzdt_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm a-1')
    end if

    if (fhill%write_soln) then
     	msg = nf90_def_var(id_file, 'fhill_soln', p_nc, [id_dim_fx, id_dim_fy, id_dim_time],  &
        id_var, chunksizes = fchunk, shuffle = shuf, deflate_level = defLvl )
     	msg = nf90_put_att(id_file, id_var, 'long_name', 'hillslope_topo_solution_high_res')
     	msg = nf90_put_att(id_file, id_var, 'units', 'm')
    end if

    ! exit definition mode
    msg = nf90_enddef(id_file) 

    ! populate dimension variables
    msg = nf90_inq_varid(id_file, 'fx', id_var)
    msg = nf90_put_var(id_file, id_var, fgrid%x(2:fgrid%nx+1))

    msg = nf90_inq_varid(id_file, 'fy', id_var)
    msg = nf90_put_var(id_file, id_var, fgrid%y(2:fgrid%ny+1))

    ! close file
    msg = nf90_close(id_file)

  end subroutine createOutfile


  ! ---------------------------------------------------------------------------
  ! SUB: write data to output file for this step
  ! ---------------------------------------------------------------------------
  subroutine writeStep(runname, time, ftopo, fclimate, fice, fhill)
  
    character(len=*), intent(in)   :: runname
    type(time_type), intent(inout) :: time
    type(topo_type), intent(in)    :: ftopo
    type(climate_type), intent(in) :: fclimate
    type(ice_type), intent(in)     :: fice
    type(hill_type), intent(in)    :: fhill
  
    real(dp) :: fsoln(ftopo%nx, ftopo%ny)
    integer :: i0f, i1f, j0f, j1f, msg, id_file, id_var

    ! increment step counter
    time%out_step = time%out_step+1

    ! prompt user
    print *, 'TIME = ', time%now

    ! define limits of interior points, for convenience
    i0f = 2
    i1f = ftopo%nx+1
    j0f = 2
    j1f = ftopo%ny+1

    ! open file
    msg = nf90_open(trim(runname)//'.out', nf90_write, id_file)

    ! write time data
    msg = nf90_inq_varid(id_file, 'time', id_var)
    msg = nf90_put_var(id_file, id_var, real(time%now, p), [time%out_step] )

    ! write topography data
    if (ftopo%write_z) then
      msg = nf90_inq_varid(id_file, 'ftopo_z', id_var)
      msg = nf90_put_var(id_file, id_var, real(ftopo%z(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
    end if

    ! write climate data
    if (fclimate%write_t) then
      msg = nf90_inq_varid(id_file, 'fclim_t', id_var)
      msg = nf90_put_var(id_file, id_var, real(fclimate%t(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
    end if

    if (fclimate%write_p) then
      msg = nf90_inq_varid(id_file, 'fclim_p', id_var)
      msg = nf90_put_var(id_file, id_var, real(fclimate%p(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
    end if

    if (fclimate%write_i) then
      msg = nf90_inq_varid(id_file, 'fclim_i', id_var)
      msg = nf90_put_var(id_file, id_var, real(fclimate%i(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
    end if

    ! write ice data
    if (fice%write_h) then
      msg = nf90_inq_varid(id_file, 'fice_h', id_var)
      msg = nf90_put_var(id_file, id_var, real(fice%h(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
    end if

    if (fice%write_uvdefm) then
      msg = nf90_inq_varid(id_file, 'fice_udefm', id_var)
      msg = nf90_put_var(id_file, id_var, real(fice%udefm(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
      
      msg = nf90_inq_varid(id_file, 'fice_vdefm', id_var)
      msg = nf90_put_var(id_file, id_var, real(fice%vdefm(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
    end if


    ! write hillslope data
    if (fhill%write_dzdt) then
      msg = nf90_inq_varid(id_file, 'fhill_dzdt', id_var)
      msg = nf90_put_var(id_file, id_var, real(fhill%dzdt(i0f:i1f, j0f:j1f), p), [1, 1, time%out_step] )
    end if

    if (fhill%write_soln) then
      fsoln = fhill%solve(time%now)
      msg = nf90_inq_varid(id_file, 'fhill_soln', id_var)
      msg = nf90_put_var(id_file, id_var, real(fsoln, p), [1, 1, time%out_step] )
    end if

    ! close file
    msg = nf90_close(id_file)

  end subroutine writeStep

end module io_module
