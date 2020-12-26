#define nvars 10

#ifdef versionA
#define ncols 10
#else
#define ncols 1
#endif

program test
  use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
  implicit none

  integer :: i, j, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10
  integer, parameter :: n=200, np=2000000
  real(kind=REAL64), dimension(:,:), allocatable :: array
  
  i1=1; i2=2; i3=3; i4=4; i5=5; i6=6; i7=7; i8=8; i9=9; i10=10

  allocate(array(ncols,np))

  do i=1,n
    call test_sub(array, mod(i, nvars)+1, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
    call dummy_sub(array, i) ! dummy_sub prevents the compiler from rewriting test_sub
  enddo

contains

  subroutine test_sub(arr, iter, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in) :: iter, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10
    integer :: i, imax

    imax = ubound(arr, 2)
    !$omp parallel do schedule(static)
    do i=1, imax
      ! do lots of stuff, some of which the below expressions should depend on
#ifdef versionA
      arr(1,i) = real(i, kind=REAL64)
      arr(2,i) = real(2*i, kind=REAL64)
      arr(3,i) = real(3*i, kind=REAL64)
      arr(4,i) = real(4*i, kind=REAL64)
      arr(5,i) = real(5*i, kind=REAL64)
      arr(6,i) = real(6*i, kind=REAL64)
      arr(7,i) = real(7*i, kind=REAL64)
      arr(8,i) = real(8*i, kind=REAL64)
      arr(9,i) = real(9*i, kind=REAL64)
      arr(10,i) = real(10*i, kind=REAL64)
#elif defined(versionB)
      select case(iter)
      case(1)
        arr(1,i) = real(i, kind=REAL64)
      case(2)
        arr(1,i) = real(2*i, kind=REAL64)
      case(3)
        arr(1,i) = real(3*i, kind=REAL64)
      case(4)
        arr(1,i) = real(4*i, kind=REAL64)
      case(5)
        arr(1,i) = real(5*i, kind=REAL64)
      case(6)
        arr(1,i) = real(6*i, kind=REAL64)
      case(7)
        arr(1,i) = real(7*i, kind=REAL64)
      case(8)
        arr(1,i) = real(8*i, kind=REAL64)
      case(9)
        arr(1,i) = real(9*i, kind=REAL64)
      case(10)
        arr(1,i) = real(10*i, kind=REAL64)
      end select
#elif defined(versionC)
      if(iter==1)  arr(1,i) = real(i, kind=REAL64)
      if(iter==2)  arr(1,i) = real(2*i, kind=REAL64)
      if(iter==3)  arr(1,i) = real(3*i, kind=REAL64)
      if(iter==4)  arr(1,i) = real(4*i, kind=REAL64)
      if(iter==5)  arr(1,i) = real(5*i, kind=REAL64)
      if(iter==6)  arr(1,i) = real(6*i, kind=REAL64)
      if(iter==7)  arr(1,i) = real(7*i, kind=REAL64)
      if(iter==8)  arr(1,i) = real(8*i, kind=REAL64)
      if(iter==9)  arr(1,i) = real(9*i, kind=REAL64)
      if(iter==10) arr(1,i) = real(10*i, kind=REAL64)
#elif defined(versionD)
      if(iter==1) then
         arr(1,i) = real(i, kind=REAL64)
      elseif(iter==2) then
         arr(1,i) = real(2*i, kind=REAL64)
      elseif(iter==3) then
         arr(1,i) = real(3*i, kind=REAL64)
      elseif(iter==4) then
         arr(1,i) = real(4*i, kind=REAL64)
      elseif(iter==5) then
         arr(1,i) = real(5*i, kind=REAL64)
      elseif(iter==6) then
         arr(1,i) = real(6*i, kind=REAL64)
      elseif(iter==7) then
         arr(1,i) = real(7*i, kind=REAL64)
      elseif(iter==8) then
         arr(1,i) = real(8*i, kind=REAL64)
      elseif(iter==9) then
         arr(1,i) = real(9*i, kind=REAL64)
      elseif(iter==10) then
         arr(1,i) = real(10*i, kind=REAL64)
      endif
#else
      if(iter==i1)  arr(1,i) = real(i, kind=REAL64)
      if(iter==i2)  arr(1,i) = real(2*i, kind=REAL64)
      if(iter==i3)  arr(1,i) = real(3*i, kind=REAL64)
      if(iter==i4)  arr(1,i) = real(4*i, kind=REAL64)
      if(iter==i5)  arr(1,i) = real(5*i, kind=REAL64)
      if(iter==i6)  arr(1,i) = real(6*i, kind=REAL64)
      if(iter==i7)  arr(1,i) = real(7*i, kind=REAL64)
      if(iter==i8)  arr(1,i) = real(8*i, kind=REAL64)
      if(iter==i9)  arr(1,i) = real(9*i, kind=REAL64)
      if(iter==i10) arr(1,i) = real(10*i, kind=REAL64)
#endif
    enddo
    !$omp end parallel do
  end subroutine test_sub

  subroutine dummy_sub(arr, i)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in) :: i

    if(i*sin(real(i)) - arr(1,4000) == 0.1234) then
      arr(:,:) = arr(:,:) * 1.0
      print *, arr(1,int(arr(1,3000)/arr(1,2000)))
    endif

  end subroutine dummy_sub
end program test
