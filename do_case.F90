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

#define _RHS_(x) real(x*i, kind=REAL64)

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
    call test_sub(array, mod(i, nvars)+1 &
#ifdef versionE
    , i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 &
#endif
    )
    call dummy_sub(array, i) ! dummy_sub prevents the compiler from rewriting test_sub
  enddo

contains

  subroutine test_sub(arr, iter &
#ifdef versionE
    , i1,i2,i3,i4,i5,i6,i7,i8,i9,i10 &
#endif
    )
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none

    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in) :: iter
#ifdef versionE
    integer, intent(in) :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10
#endif
    integer :: i, imax

    imax = ubound(arr, NP_DIM)
    !$omp parallel do schedule(static)
    do i=1, imax
      ! do lots of stuff, some of which the below expressions should depend on
#ifdef versionA
      arr(ELEM(1,i)) = _RHS_(1)
      arr(ELEM(2,i)) = _RHS_(2)
      arr(ELEM(3,i)) = _RHS_(3)
      arr(ELEM(4,i)) = _RHS_(4)
      arr(ELEM(5,i)) = _RHS_(5)
      arr(ELEM(6,i)) = _RHS_(6)
      arr(ELEM(7,i)) = _RHS_(7)
      arr(ELEM(8,i)) = _RHS_(8)
      arr(ELEM(9,i)) = _RHS_(9)
      arr(ELEM(10,i)) = _RHS_(1)
#elif defined(versionB)
      select case(iter)
      case(1)
        arr(ELEM(1,i)) = _RHS_(1)
      case(2)
        arr(ELEM(1,i)) = _RHS_(2)
      case(3)
        arr(ELEM(1,i)) = _RHS_(3)
      case(4)
        arr(ELEM(1,i)) = _RHS_(4)
      case(5)
        arr(ELEM(1,i)) = _RHS_(5)
      case(6)
        arr(ELEM(1,i)) = _RHS_(6)
      case(7)
        arr(ELEM(1,i)) = _RHS_(7)
      case(8)
        arr(ELEM(1,i)) = _RHS_(8)
      case(9)
        arr(ELEM(1,i)) = _RHS_(9)
      case(10)
        arr(ELEM(1,i)) = _RHS_(1)
      end select
#elif defined(versionC)
      if(iter==1)  arr(ELEM(1,i)) = _RHS_(1)
      if(iter==2)  arr(ELEM(1,i)) = _RHS_(2)
      if(iter==3)  arr(ELEM(1,i)) = _RHS_(3)
      if(iter==4)  arr(ELEM(1,i)) = _RHS_(4)
      if(iter==5)  arr(ELEM(1,i)) = _RHS_(5)
      if(iter==6)  arr(ELEM(1,i)) = _RHS_(6)
      if(iter==7)  arr(ELEM(1,i)) = _RHS_(7)
      if(iter==8)  arr(ELEM(1,i)) = _RHS_(8)
      if(iter==9)  arr(ELEM(1,i)) = _RHS_(9)
      if(iter==10) arr(ELEM(1,i)) = _RHS_(1)
#elif defined(versionD)
      if(iter==1) then
         arr(ELEM(1,i)) = _RHS_(1)
      elseif(iter==2) then
         arr(ELEM(1,i)) = _RHS_(2)
      elseif(iter==3) then
         arr(ELEM(1,i)) = _RHS_(3)
      elseif(iter==4) then
         arr(ELEM(1,i)) = _RHS_(4)
      elseif(iter==5) then
         arr(ELEM(1,i)) = _RHS_(5)
      elseif(iter==6) then
         arr(ELEM(1,i)) = _RHS_(6)
      elseif(iter==7) then
         arr(ELEM(1,i)) = _RHS_(7)
      elseif(iter==8) then
         arr(ELEM(1,i)) = _RHS_(8)
      elseif(iter==9) then
         arr(ELEM(1,i)) = _RHS_(9)
      elseif(iter==10) then
         arr(ELEM(1,i)) = _RHS_(1)
      endif
#elif defined(versionE)
      if(iter==i1)  arr(ELEM(1,i)) = _RHS_(1)
      if(iter==i2)  arr(ELEM(1,i)) = _RHS_(2)
      if(iter==i3)  arr(ELEM(1,i)) = _RHS_(3)
      if(iter==i4)  arr(ELEM(1,i)) = _RHS_(4)
      if(iter==i5)  arr(ELEM(1,i)) = _RHS_(5)
      if(iter==i6)  arr(ELEM(1,i)) = _RHS_(6)
      if(iter==i7)  arr(ELEM(1,i)) = _RHS_(7)
      if(iter==i8)  arr(ELEM(1,i)) = _RHS_(8)
      if(iter==i9)  arr(ELEM(1,i)) = _RHS_(9)
      if(iter==i10) arr(ELEM(1,i)) = _RHS_(1)
#elif defined(versionF)
      if(iter==1) then
        call set_elem(arr, i, 1, _RHS_(1))
      elseif(iter==2) then
        call set_elem(arr, i, 1, _RHS_(2))
      elseif(iter==3) then
        call set_elem(arr, i, 1, _RHS_(3))
      elseif(iter==4) then
        call set_elem(arr, i, 1, _RHS_(4))
      elseif(iter==5) then
        call set_elem(arr, i, 1, _RHS_(5))
      elseif(iter==6) then
        call set_elem(arr, i, 1, _RHS_(6))
      elseif(iter==7) then
        call set_elem(arr, i, 1, _RHS_(7))
      elseif(iter==8) then
        call set_elem(arr, i, 1, _RHS_(8))
      elseif(iter==9) then
        call set_elem(arr, i, 1, _RHS_(9))
      elseif(iter==10) then
        call set_elem(arr, i, 1, _RHS_(10))
      endif
#elif defined(versionG)
      if(iter==1) then
         call set_elem_val(arr, i, 1, _RHS_(1))
      elseif(iter==2) then
         call set_elem_val(arr, i, 1, _RHS_(2))
      elseif(iter==3) then
         call set_elem_val(arr, i, 1, _RHS_(3))
      elseif(iter==4) then
         call set_elem_val(arr, i, 1, _RHS_(4))
      elseif(iter==5) then
         call set_elem_val(arr, i, 1, _RHS_(5))
      elseif(iter==6) then
         call set_elem_val(arr, i, 1, _RHS_(6))
      elseif(iter==7) then
         call set_elem_val(arr, i, 1, _RHS_(7))
      elseif(iter==8) then
         call set_elem_val(arr, i, 1, _RHS_(8))
      elseif(iter==9) then
         call set_elem_val(arr, i, 1, _RHS_(9))
      elseif(iter==10) then
         call set_elem_val(arr, i, 1, _RHS_(10))
      endif
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
    print *, arr(1,max(int(arr(1,3000)/arr(1,2000)),100000))
#else
    print *, arr(max(int(arr(3000,1)/arr(2000,1)),100000),1)
#endif
    endif
#endif

  end subroutine dummy_sub

  subroutine set_elem(arr, ip, col, val)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none
    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in) :: ip, col
    real(kind=REAL64), intent(in) :: val

    arr(ELEM(col,ip)) = val
  end subroutine set_elem

  subroutine set_elem_val(arr, ip, col, val)
    use, intrinsic :: iso_fortran_env, only : REAL32, REAL64
    implicit none
    real(kind=REAL64), dimension(:,:), allocatable, intent(inout) :: arr
    integer, intent(in), value :: ip, col
    real(kind=REAL64), intent(in), value :: val

    arr(ELEM(col,ip)) = val
  end subroutine set_elem_val
end program test
