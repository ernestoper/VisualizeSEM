! This programs writes Ensight Gold binary file collecting binary mesh and volume data
! files produced by SPECFEM3D. The Ensight Gold file can then be visualized in 
! VTK/ParaView. See http://www.vtk.org and http://www.paraview.org for details.
!------------------------------------------
! DEPENDENCY:
!   cfunc4fortran.c, visualize_par.f90, visualize_collect.f90, write_ensight.f90,
!   write_vtu.f90 
! COMPILE
!   >> make
! USAGE
!   ./xvisualize [input filename]
!   e.g., ./xvisualize visualize.in
!   see visualize.in for input detail
! HISTORY:
!   Hom Nath Gharti, NORSAR
!   Mar 10,2010 (NORSAR)
! FEEDBACK:
!   hgharti_AT_princeton_DOT_edu
!------------------------------------------  
  
program visualize
use visualize_par
implicit none

integer :: i_plot
character(len=80) :: inp_fname  
  
if (iargc() <= 0) then
  write(*,'(/,a)')'ERROR: no input file!'
  stop! counts total number of nodes and elementsp
endif
  
write(*,'(a)',advance='no')'reading main input file...'
call get_command_argument(1, inp_fname)
   
call read_input(inp_fname)
write(*,'(a)')'complete!'
  
! check and display key input parameters...'
if (out_res == 0) then
  NENOD_OUT = 8
elseif (out_res == 1) then
  NENOD_OUT = 20
elseif (out_res == 2) then
  NENOD_OUT = 8
else
  write(*,'(/,a)')'ERROR: wrong out_res value!'
  stop
endif  

if (out_ncomp > inp_ncomp)then
  write(*,'(/,a)')'ERROR: number of components for output cannot be greater than for input!'
  stop
elseif (out_ncomp>1 .and. out_ncomp /= inp_ncomp)then
  write(*,'(/,a)')'ERROR: not supported components transformation!'
  stop
endif

! Display key information
write(*,'(a)')'-------------------------------'
write(*,*)'Number of input processors: ',nproc
write(*,*)'number of image frames: ',t_nstep
write(*,*)'input directory:',inp_path
if (inp_ncomp==1) then
  write(*,*) 'input data type: SCALAR'
elseif (inp_ncomp==3) then
  write(*,*) 'input data type: VECTOR'
elseif (inp_ncomp==6) then
  write(*,*) 'input data type: 9C SYMMETRIC TENSOR'
else
  write(*,'(/,a)')'ERROR: unsupported data type!'
  stop
endif
write(*,*)'output directory:',out_path
if (out_ncomp==1) then
  write(*,*) 'output data type: SCALAR'
elseif (out_ncomp==3) then
  write(*,*) 'output data type: VECTOR'
elseif (out_ncomp==6) then
  write(*,*) 'output data type: 9C SYMMETRIC TENSOR'
else
  write(*,'(/,a)')'ERROR: unsupported data type!'
  stop
endif
if (out_format==0) then
  write(*,*)'output format: VTK'
elseif (out_format==1) then
  write(*,*)'output format: Ensight Gold'
else
  write(*,'(/,a)')'ERROR: unsupported output format!'
  stop
endif
if (out_res==0) then
  write(*,*)'resolution: LOW'
elseif (out_res==1) then
  write(*,*)'resolution: MEDIUM'
elseif (out_res==2) then
  write(*,*)'resolution: HIGH'
else
  write(*,'(/,a)')'ERROR: unsupported resolution!'
  stop
endif

write(*,*)'number of output plots: ',nplot
  
write(*,'(a)')'-------------------------------'
write(*,'(a)',advance='no')'counting meshes...'
  
! count total number of nodes and elements in all plots
allocate(plot_nnode(nplot))
allocate(plot_nelmt(nplot))
! Loop over output plots
do i_plot=1,nplot    
  
  plot_nnode(i_plot) = 0
  plot_nelmt(i_plot) = 0
  
  call cvd_count_totals_ext_mesh(plot_nproc(i_plot), &
  plot_proc_list(i_plot,1:plot_nproc(i_plot)),proc_width, &
  inp_path,plot_nnode(i_plot),plot_nelmt(i_plot),out_res)    
enddo
write(*,'(a)')'complete!'
   
if (out_format==0)then
  ! VTK files
  call write_vtu()
elseif (out_format==1)then
  ! Ensight Gold files
  call write_ensight()
else
  write(*,'(/,a)')'ERROR: unsupported ouput format!'
  stop
endif
write(*,'(a)')'-------------------------------'
 
end program visualize
