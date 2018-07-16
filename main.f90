program gridmaker
use mod_szk

implicit none

type operation
!ver1.00
  integer :: divpnt
  logical :: fg_prg
  real(8) :: div
  integer :: idx
  character(64) :: dummy
endtype


!=============================IO=======================
integer, parameter :: lread=5, lwrt=6
character(300) :: boutfil
character(5), allocatable :: bcmdhst
integer :: icmdhst, icmdhstlen
logical :: fg_cmd

!=============================Grid=====================
real(8), allocatable :: dgrd(:,:,:)
integer :: ljdiv, lkdiv
type(operation), allocatable :: pjdiv(:), pkdiv(:)
integer :: ljend, lkend




!==================================ここからメインプログラム==================================

write(lwrt, *) 'Hello, this is gdszk!!'
write(lwrt, *) ' '
call createGrid
do while(fg_cmd)
  call setCommand
enddo
!call outputFile
write(lwrt, *) 'MISSION ACCOMPLISHED !!!!'
stop

!==================================ここまでメインプログラム==================================

contains

!==================================ここからサブルーチン======================================

  subroutine input(kunt, efil, kjend, kkend, rgrd, kjdiv, njdiv, kkdiv, nkdiv)
    integer, intent(in) :: kunt
    character(*), intent(in) :: efil
    integer, intent(out) :: kjend, kkend
    real(8), allocatable, intent(out) :: rgrd(:,:,:)
    character(8) :: cszk
    integer, intent(out) :: kjdiv, kkdiv
    type(operation), allocatable, intent(out) :: njdiv, nkdiv
    integer :: j, k, i

    open(unit=kunt, file=efil, form='unformatted', status='old')
    read(kunt) kjend, kkend
    allocate(rgrd(kjend,kkend,2))
    read(kunt) (((grid(j,k,2), j=1, kjend), k=1, kkend), i=1, 2)
    read(kunt) cszk
    if(cszk(1:3)/='SZK')then
      write(lwrt,*) 'ERROR!! not szk file...'
      stop
    endif
    read(kunt) kjdiv
    allocatable(njdiv(kjdiv))
    read(kunt) (njdiv(i), i=1, kjdiv)
    read(kunt) kkdiv
    allocatable(nkdiv(kkdiv))
    read(kunt) (nkdiv(i), i=1, kkdiv)
    close(kunt)
  endsubroutine


  subroutine output(kunt, efil, kjend, kkend, rgrd, kdiv, ndiv)
    integer, intent(in) :: kunt
    character(*), intent(in) :: efil
    integer, intent(in) :: kjend, kkend
    real(8), allocatable, intent(in) :: rgrd(:,:,:)
    character(8), parameter :: cszk = 'SZK01.00'
    integer, intent(out) :: kdiv
    type(operation), allocatable, intent(out) :: ndiv
    integer :: j, k, i

    open(unit=kunt, file=efil, form='unformatted', status='relpace')
    write(kunt) kjend, kkend
    write(kunt) (((grid(j,k,2), j=1, kjend), k=1, kkend), i=1, 2)
    write(kunt) cszk
    read(kunt) kdiv
    read(kunt) (ndiv(i), i=1, kdiv)
    close(kunt)
  endsubroutine

 function createArith(inum, d, rofs)
  integer, intent(in) :: inum
  real(8), intent(in) :: d, ofs
  real(8), allocatable :: createArith
  integer :: i

  allocate(createArith(inum))
  do i=1, inum
    createArith(i) = rofs + d * (i - 1)
  enddo
 endfunction
 
 function createGeom(inum, d, r, rofs)
  integer, intent(in) :: inum
  real(8), intent(in) :: d, ofs, r
  real(8), allocatable :: createGeom
  integer :: i

  allocate(createGeom(inum))
  do i=1, inum
    createGeom(i) = rofs + d * (r**i-1.d0) / (r-1.d0)
  enddo
 endfunction

  subroutine createGrid
    integer :: ioutfil, iarg
    character(300) :: cbuf
    integer :: ibuf
    type(operation) :: njopr, nkopr

    if(command_argument_count() == 1)then
      cbuf = ''
      call get_command_argument(1, boutfil)
    elseif(command_argument_count() == 2)then
      call get_command_argument(1, cbuf)
      call get_command_argument(2, boutfil)
    else
      write(lwrt,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++'
      write(lwrt,*) ' '
      write(lwrt,*) 'INPUT >> input file name (or press only "Enter" key to create newly)'
      read(lread,'(a)') cbuf
      write(lwrt,*) ' '
      write(lwrt,*) 'INPUT >> output file name'
      read(lread,*) boutfil
    endif
    write(lwrt,*) ' input file: '//cbuf
    write(lwrt,*) 'output file: '//boutfil
    write(lwrt,*) ' '
    if(len_trim(cbuf) == 0)then
      write(lwrt,*) 'INPUT >> number of Xi direction points'
      write(lwrt,*) '      >> arithmetic (0) or geometric (1) progression'
      read(lread,*) njopr%divpnt, ibuf
      if(ibuf==1)then
        njopr%fg_prg = .true.
        write(lwrt,*) 'INPUT >> r for Xi (>1.0) and use r (1) or 1/r (-1)'
        write(lwrt,*) '         r: common ratio'
        read(lread,*) njopr%div, njopr%idx
      else
        njopr%fg_prg = .false.
        write(lwrt,*) 'INPUT >> d for Xi'
        write(lwrt,*) '         d: common difference'
        read(lread,*) njopr%div
      endif
      write(lwrt,*) 'INPUT >> number of Eta direction points'
      write(lwrt,*) '      >> arithmetic (0) or geometric (1) progression'
      read(lread,*) nkopr%divpnt, ibuf
      if(ibuf==1)then
        nkopr%fg_prg = .true.
        write(lwrt,*) 'INPUT >> r for Eta (>1.0) and use r (1) or 1/r (-1)'
        write(lwrt,*) '         r: common ratio'
        read(lread,*) nkopr%div, nkopr%idx
      else
        nkopr%fg_prg = .false.
        write(lwrt,*) 'INPUT >> d for Eta'
        write(lwrt,*) '         d: common difference'
        read(lread,*) nkopr%div
      endif
      allocate(pjdiv(1))
      pjdiv(1) = njopr
      ljdiv = 1
      allocate(pkdiv(1))
      pkdiv(1) = nkopr
      lkdiv = 1
    else
      call input(10, trim(cbuf), ljend, lkend, dgrd, ljdiv, pjdiv, lkdiv, pkdiv)
    endif
  endsubroutine


subroutine setCommand
  character(100) :: ccmd
  character(:), allocatable :: ccmdlst
  character(100) :: cbuf(5)
  integer :: ibuf(5)
  real(8) :: abuf(5)

  write(lwrt,*) 'INPUT >> grid command'
  write(lwrt,*) ' exp l1 l2 : expand array of grid (d1: Xi, d2: Eta)'
  write(lwrt,*) ' trm l1 l2 : trim array of grid (d1: Xi, d2: Eta)'
  write(lwrt,*) ' sht l1 l2 : shift array of grid (d1: Xi, d2: Eta)'
  write(lwrt,*) ' vld       : validate array of grid'
  write(lwrt,*) ' red  : redo'
  write(lwrt,*) ' end  : save grid and end this program'
  read(lread,*) ccmd
  write(lwrt,*) ' '

  call splitall(ccmd, ' ', ccmdlst, 10)
  write(lwrt,*) 'command: '//ccmd
  select case(ccmdlst(1))
  case('exp')
    call gridExpand(toInteger(ccmdlst(2)), toInteger(ccmdlst(3)))
  case('trm')
    call gridTrim(toInteger(ccmdlst(2)), toInteger(ccmdlst(3)))
  case ('end')
    fg_cmd = .false.
  end select
endsubroutine


subroutine gridExpand(kjpls, kkpls)
  integer,intent(in) :: kjpls, kkpls
  real(8) :: agrd(:,:,:)
  integer :: j, k, i

  allocate(agrd(ljend+kjpls, lkend+ kkpls, 2))
  agrd = d_quiet_nan()
  do i=1, 2
    do k=1, lkend
      do j=1, ljend
        agrd(j, k, i) = dgrd(j, k, i)
      enddo
    enddo
  enddo
  deallocate(dgrd)
  allocate(dgrd(ljend+kjpls, lkend+ kkpls, 2))
  dgrd = agrd
  deallocate(agrd)
endsubroutine


subroutine gridTrim(kjmns, kkmns)
  integer,intent(in) :: kjmns, kkmns
  integer :: ijend, ikend
  real(8) :: agrd(:,:,:)
  integer :: j, k, i

  ijend = ljend - kjmns
  ikend = lkend - kkmns
  allocate(agrd(ijend, ikend, 2))
  do i=1, 2
    do k=1, ikend
      do j=1, ijend
        agrd(j, k, i) = dgrd(j, k, i)
      enddo
    enddo
  enddo
  deallocate(dgrd)
  allocate(dgrd(ijend, ikend, 2))
  dgrd = agrd
  deallocate(agrd)
endsubroutine

subroutine gridShift(kjpls, kkpls)
  integer,intent(in) :: kjpls, kkpls
  integer :: i, j, k
  integer :: ijdst, ikdst
  real(8) :: agrd(:,:,:)

  allocate(agrd(ljend, lkend, 2))
  agrd = d_quiet_nan()
  do i=1, 2
    do k=1, lkend
      ikdst = k + kkpls
      if(1<=ikdst .and. ikdst<=lkend)then
        do j=1, ljend
          ijdst = j + kjpls
          if(1<=ijdst .and. ijdst<=ljend) agrd(j+kjpls, k+kkpls, i) = dgrd(j, k, i)
        enddo
      endif
    enddo
  enddo
  deallocate(dgrd)
  allocate(dgrd(ljend, lkend, 2))
  dgrd = agrd
  deallocate(agrd)
endsubroutine

function validate
  logical :: validate(2,2)
  if(dgrd())
endfunction

!==================================ここまでサブルーチン======================================

end program
