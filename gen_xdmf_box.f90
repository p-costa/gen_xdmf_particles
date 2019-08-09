program gen_xdmf
implicit none
include 'param.h90'
integer :: nflds,islice
character(len=1), parameter :: lf = char(10)
character(len=400) :: buffer
character(len=6) :: ichar
character(len=3) :: fldname
character(len=4) :: islicechar
integer :: ixdmf
integer :: e_io,indent
integer :: i
!
ixdmf = 99 
nflds = (fldend-fldstart)/nskip + 1
indent = 0
write(islicechar,'(I4.4)') islice 
open(unit = ixdmf, file = 'view_box_'//trim(casename)//'.xmf',form='formatted')
write(unit=ixdmf,fmt='(A)') '<?xml version="1.0" ?>'
write(unit=ixdmf,fmt='(A)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
write(unit=ixdmf,fmt='(A)') '<Xdmf xmlns:xi="http://www.w3.org/2001/XInclude" Version="2.0">'
write(unit=ixdmf,fmt='(A)') '<Domain>'
indent = indent + 4
  write(buffer,fmt='(A,3I5,A)') repeat(' ',indent)//'<Topology name="TOPO" TopologyType="3DCoRectMesh" Dimensions="',2,2,2,'"/>'
  write(unit=ixdmf,fmt='(A)') trim(buffer)
write(buffer,fmt='(A)') repeat(' ',indent)//'<Geometry name="GEO" Type="ORIGIN_DXDYDZ">'
write(unit=ixdmf,fmt='(A)')trim(buffer)
indent = indent + 4
  write(buffer,fmt='(A)') repeat(' ',indent)//'<DataItem Format="XML" Dimensions="3">'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  write(buffer,fmt='(A,3E15.6)') repeat(' ',indent),z0,y0,x0
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  write(buffer,fmt='(A)') repeat(' ',indent)//'</DataItem>'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  write(buffer,fmt='(A)') repeat(' ',indent)//'<DataItem Format="XML" Dimensions="3">'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  write(buffer,fmt='(A,3E15.6)') repeat(' ',indent),lz,ly,lx
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  write(buffer,fmt='(A)') repeat(' ',indent)//'</DataItem>'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  indent = indent - 4
write(buffer,fmt='(A)') repeat(' ',indent)//'</Geometry>'
write(unit=ixdmf,fmt='(A)')trim(buffer)
write(buffer,fmt='(A)') repeat(' ',indent)//'<Grid Name="TimeSeries" GridType="Collection" CollectionType="Temporal">'
write(unit=ixdmf,fmt='(A)')trim(buffer)
indent = indent + 4
  write(buffer,fmt='(A)') repeat(' ',indent)//'<Time TimeType="List">'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  indent = indent + 4
    write(buffer,fmt='(A,I6,A)') repeat(' ',indent)//'<DataItem Format="XML" NumberType="Float" Dimensions="',nflds,'">'
    write(unit=ixdmf,fmt='(A)')trim(buffer) 
    write(buffer,fmt='(A,I6)') repeat(' ',indent)
    write(ixdmf,fmt='(A)',advance='no') trim(buffer)
    do i = 1,nflds
    write(ixdmf,fmt='(E15.6)',advance='no') 1.*(i-1)*dt*nskip
    enddo
    write(buffer,fmt='(A)') repeat(' ',indent)//'</DataItem>'
    write(unit=ixdmf,fmt='(A)')trim(buffer) 
  indent = indent - 4
  write(buffer,fmt='(A)') repeat(' ',indent)//'</Time>'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  do i = fldstart,fldend,nskip
    write(ichar,fmt='(i6.6)') i
    write(buffer,fmt='(A)') repeat(' ',indent)//'<Grid Name="T'//ichar//'" GridType="Uniform">'
    write(unit=ixdmf,fmt='(A)')trim(buffer)
    indent = indent + 4
      write(buffer,fmt='(A)') repeat(' ',indent)//'<Topology Reference="/Xdmf/Domain/Topology[1]"/>'
      write(unit=ixdmf,fmt='(A)')trim(buffer)
      write(buffer,fmt='(A)') repeat(' ',indent)//'<Geometry Reference="/Xdmf/Domain/Geometry[1]"/>'
      write(unit=ixdmf,fmt='(A)')trim(buffer)
    indent = indent - 4
    write(buffer,fmt='(A)') repeat(' ',indent)//'</Grid>'
    write(unit=ixdmf,fmt='(A)')trim(buffer)
  enddo
  indent = indent - 4
  write(buffer,fmt='(A)') repeat(' ',indent)//'</Grid>'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
indent = indent - 4
write(buffer,fmt='(A)') repeat(' ',indent)//'</Domain>'
write(unit=ixdmf,fmt='(A)')trim(buffer)
write(buffer,fmt='(A)') repeat(' ',indent)//'</Xdmf>'
write(unit=ixdmf,fmt='(A)')trim(buffer)
close(ixdmf)
!
end program gen_xdmf
