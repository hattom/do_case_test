#define nvars 10

#ifdef versionA
#define ncols nvars
#else
#define ncols 1
#endif

#ifndef ROW_MAJ
#if !defined(COL_MAJ)
#define COL_MAJ
#endif
#define ELEM(x,y) x,y
#define NP_DIM 2
#else
#define ELEM(x,y) y,x
#define NP_DIM 1
#endif

program test
  use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
  implicit none

  integer :: i, j, i1,i2,i3,i4,i5,i6,i7,i8,i9,i10
  integer, parameter :: n=200, np=2000000
  real(kind=REAL64), dimension(:,:), allocatable :: array
  
  i1=1; i2=2; i3=3; i4=4; i5=5; i6=6; i7=7; i8=8; i9=9; i10=10

  allocate(array(ELEM(ncols,np)))
#ifdef DEBUG
  print *, shape(array)
#endif

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

    imax = ubound(arr, NP_DIM)
    !$omp parallel do schedule(static)
    do i=1, imax
      ! do lots of stuff, some of which the below expressions should depend on
#ifdef versionA
      arr(ELEM(1,i)) = real(i, kind=REAL64)
      arr(ELEM(2,i)) = real(2*i, kind=REAL64)
      arr(ELEM(3,i)) = real(3*i, kind=REAL64)
      arr(ELEM(4,i)) = real(4*i, kind=REAL64)
      arr(ELEM(5,i)) = real(5*i, kind=REAL64)
      arr(ELEM(6,i)) = real(6*i, kind=REAL64)
      arr(ELEM(7,i)) = real(7*i, kind=REAL64)
      arr(ELEM(8,i)) = real(8*i, kind=REAL64)
      arr(ELEM(9,i)) = real(9*i, kind=REAL64)
      arr(ELEM(10,i)) = real(10*i, kind=REAL64)
#elif defined(versionB)
      select case(iter)
      case(1)
        arr(ELEM(1,i)) = real(i, kind=REAL64)
      case(2)
        arr(ELEM(1,i)) = real(2*i, kind=REAL64)
      case(3)
        arr(ELEM(1,i)) = real(3*i, kind=REAL64)
      case(4)
        arr(ELEM(1,i)) = real(4*i, kind=REAL64)
      case(5)
        arr(ELEM(1,i)) = real(5*i, kind=REAL64)
      case(6)
        arr(ELEM(1,i)) = real(6*i, kind=REAL64)
      case(7)
        arr(ELEM(1,i)) = real(7*i, kind=REAL64)
      case(8)
        arr(ELEM(1,i)) = real(8*i, kind=REAL64)
      case(9)
        arr(ELEM(1,i)) = real(9*i, kind=REAL64)
      case(10)
        arr(ELEM(1,i)) = real(10*i, kind=REAL64)
      end select
#elif defined(versionC)
      if(iter==1)  arr(ELEM(1,i)) = real(i, kind=REAL64)
      if(iter==2)  arr(ELEM(1,i)) = real(2*i, kind=REAL64)
      if(iter==3)  arr(ELEM(1,i)) = real(3*i, kind=REAL64)
      if(iter==4)  arr(ELEM(1,i)) = real(4*i, kind=REAL64)
      if(iter==5)  arr(ELEM(1,i)) = real(5*i, kind=REAL64)
      if(iter==6)  arr(ELEM(1,i)) = real(6*i, kind=REAL64)
      if(iter==7)  arr(ELEM(1,i)) = real(7*i, kind=REAL64)
      if(iter==8)  arr(ELEM(1,i)) = real(8*i, kind=REAL64)
      if(iter==9)  arr(ELEM(1,i)) = real(9*i, kind=REAL64)
      if(iter==10) arr(ELEM(1,i)) = real(10*i, kind=REAL64)
#elif defined(versionD)
      if(iter==1) then
         arr(ELEM(1,i)) = real(i, kind=REAL64)
      elseif(iter==2) then
         arr(ELEM(1,i)) = real(2*i, kind=REAL64)
      elseif(iter==3) then
         arr(ELEM(1,i)) = real(3*i, kind=REAL64)
      elseif(iter==4) then
         arr(ELEM(1,i)) = real(4*i, kind=REAL64)
      elseif(iter==5) then
         arr(ELEM(1,i)) = real(5*i, kind=REAL64)
      elseif(iter==6) then
         arr(ELEM(1,i)) = real(6*i, kind=REAL64)
      elseif(iter==7) then
         arr(ELEM(1,i)) = real(7*i, kind=REAL64)
      elseif(iter==8) then
         arr(ELEM(1,i)) = real(8*i, kind=REAL64)
      elseif(iter==9) then
         arr(ELEM(1,i)) = real(9*i, kind=REAL64)
      elseif(iter==10) then
         arr(ELEM(1,i)) = real(10*i, kind=REAL64)
      endif
#elif defined(versionE)
      if(iter==i1)  arr(ELEM(1,i)) = real(i, kind=REAL64)
      if(iter==i2)  arr(ELEM(1,i)) = real(2*i, kind=REAL64)
      if(iter==i3)  arr(ELEM(1,i)) = real(3*i, kind=REAL64)
      if(iter==i4)  arr(ELEM(1,i)) = real(4*i, kind=REAL64)
      if(iter==i5)  arr(ELEM(1,i)) = real(5*i, kind=REAL64)
      if(iter==i6)  arr(ELEM(1,i)) = real(6*i, kind=REAL64)
      if(iter==i7)  arr(ELEM(1,i)) = real(7*i, kind=REAL64)
      if(iter==i8)  arr(ELEM(1,i)) = real(8*i, kind=REAL64)
      if(iter==i9)  arr(ELEM(1,i)) = real(9*i, kind=REAL64)
      if(iter==i10) arr(ELEM(1,i)) = real(10*i, kind=REAL64)
#endif
    enddo
    !$omp end parallel do
  end subroutine test_sub

  subroutine dummy_sub(arr, i)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in) :: i

    if(i*sin(real(i)) - arr(ELEM(1,4000)) == 0.1234) then
      arr(ELEM(:,:)) = arr(ELEM(:,:)) * 1.1233532
#ifdef COL_MAJ
      print *, arr(1,int(arr(1,3000)/arr(1,2000)))
#else
      print *, arr(int(arr(3000,1)/arr(2000,1)),1)
#endif
    endif

#ifdef DEBUG
    if (i==1) then
#ifdef COL_MAJ
    print *, arr(1,int(arr(1,3000)/arr(1,2000)))
#else
    print *, arr(int(arr(3000,1)/arr(2000,1)),1)
#endif
    endif
#endif

  end subroutine dummy_sub
end program test
