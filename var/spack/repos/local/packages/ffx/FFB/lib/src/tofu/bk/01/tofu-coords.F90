! Obtain tofu-coordinate
! 5.1.2.1 FJMPI_TOPOLOGY_GET_COORDS
program tofu_coord
use mpi_f08_ext
implicit none
integer :: myrank, numrank, ierr, maxdims, view
integer :: coords(6)

coords(:) = 0
call MPI_Init(ierr)
call MPI_Comm_size(MPI_COMM_WORLD, numrank, ierr)
call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)

! for logical coordinate
view = FJMPI_LOGICAL
maxdims = 3
call FJMPI_Topology_get_coords(MPI_COMM_WORLD, myrank, view, maxdims, coords, ierr)
write(*,*) "FJMPI_LOGICAL myrank=", myrank, ", coords=", coords

! for Tofu coordinate
view = FJMPI_TOFU_SYS
maxdims = 6
call FJMPI_Topology_get_coords(MPI_COMM_WORLD, myrank, view, maxdims, coords, ierr)
write(*,*) "FJMPI_TOFU_SYS myrank=", myrank, ", coords=", coords

! for Tofu relative coordinate
view = FJMPI_TOFU_REL
maxdims = 6
call FJMPI_Topology_get_coords(MPI_COMM_WORLD, myrank, view, maxdims, coords, ierr)
write(*,*) "FJMPI_TOFU_REL myrank=", myrank, ", coords=", coords

call MPI_Finalize(ierr)
stop
end program tofu_coord

