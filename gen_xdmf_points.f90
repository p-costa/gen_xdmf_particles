program gen_xdmf
implicit none
include 'param.h90'
integer, parameter :: ixdmf = 99
integer, parameter :: nflds = (fldend-fldstart)/nskip+1
character(len=1), parameter :: lf = char(10)
character(len=400) :: buffer
character(len=6) :: ichar
integer :: indent
integer :: ifld,ie
integer :: iseek
character(len=10), dimension(ne) :: ctypes
!
do ie = 1,ne
  if(dims(ie).eq.1) then
    ctypes(ie) = 'Scalar'
  else
    ctypes(ie) = 'Vector'
  endif
enddo
open(unit = ixdmf, file = 'view_points_'//trim(casename)//'.xmf',form='formatted')
write(unit=ixdmf,fmt='(A)') '<?xml version="1.0" ?>'
write(unit=ixdmf,fmt='(A)') '<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
write(unit=ixdmf,fmt='(A)') '<Xdmf xmlns:xi="http://www.w3.org/2001/XInclude" Version="2.0">'
write(unit=ixdmf,fmt='(A)') '<Domain>'
indent = indent + 4
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
    do ifld = 1,nflds
    write(ixdmf,fmt='(E15.6)',advance='no') 1.*(ifld-1)*dt*nskip
    enddo
    write(buffer,fmt='(A)') repeat(' ',indent)//'</DataItem>'
    write(unit=ixdmf,fmt='(A)')trim(buffer) 
  indent = indent - 4
  write(buffer,fmt='(A)') repeat(' ',indent)//'</Time>'
  write(unit=ixdmf,fmt='(A)')trim(buffer)
  do ifld = fldstart,fldend,nskip
    write(ichar,fmt='(i6.6)') ifld
    iseek = 0
    write(buffer,fmt='(A)') repeat(' ',indent)//'<Grid Name="T'//ichar//'" GridType="Uniform">'
    write(unit=ixdmf,fmt='(A)')trim(buffer)
    indent = indent + 4
      write(buffer,fmt='(A,I9,A)') repeat(' ',indent)//'<Topology TopologyType="Polyvertex" NodesPerElement="',np,'"/>'
      write(unit=ixdmf,fmt='(A)')trim(buffer)
      write(buffer,fmt='(A)') repeat(' ',indent)//'<Geometry GeometryType="'//trim(names(1))//'">'
      write(unit=ixdmf,fmt='(A)')trim(buffer)
      indent = indent + 4
        write(buffer,fmt='(A,I1,A,I2,I9,A,I10,A)') repeat(' ',indent)// '<DataItem Format="Binary"' // &
                                                            ' DataType="Float" Precision="',iprec,'" Endian="Native"' // &
                                                            ' Dimensions="',dims(1),np,'" Seek="',iseek,'">'
        iseek = iseek+np*dims(1)*iprec
        write(unit=ixdmf,fmt='(A)')trim(buffer)
        indent = indent + 4
          write(buffer,fmt='(A,i7.7,A)') repeat(' ',indent)//trim(filename_i),ifld,trim(fileext_i)
          write(unit=ixdmf,fmt='(A)')trim(buffer)
        indent = indent - 4
      write(buffer,fmt='(A)') repeat(' ',indent)//'</DataItem>'
      write(unit=ixdmf,fmt='(A)')trim(buffer) 
      indent = indent - 4
      write(buffer,fmt='(A)') repeat(' ',indent)//'</Geometry>'
      write(unit=ixdmf,fmt='(A)')trim(buffer)
      do ie = 2,ne
        write(buffer,fmt='(A)') &
          repeat(' ',indent)//'<Attribute Type="'//trim(ctypes(ie))//'" Center="Node" Name="'//trim(names(ie))//'">'
        write(unit=ixdmf,fmt='(A)')trim(buffer)
        indent = indent + 4
          write(buffer,fmt='(A,I1,A,I2,I9,A,I10,A)') repeat(' ',indent)// '<DataItem Format="Binary"' // &
                                                              ' DataType="Float" Precision="',iprec,'" Endian="Native"' // &
                                                              ' Dimensions="',dims(ie),np,'" Seek="',iseek,'">'
          iseek = iseek+np*dims(ie)*iprec
          write(unit=ixdmf,fmt='(A)')trim(buffer)
          indent = indent + 4
            write(buffer,fmt='(A,i7.7,A)') repeat(' ',indent)//trim(filename_i),ifld,trim(fileext_i)
            write(unit=ixdmf,fmt='(A)')trim(buffer)
          indent = indent - 4
        write(buffer,fmt='(A)') repeat(' ',indent)//'</DataItem>'
        write(unit=ixdmf,fmt='(A)')trim(buffer)
        indent = indent - 4
        write(buffer,fmt='(A)') repeat(' ',indent)//'</Attribute>'
        write(unit=ixdmf,fmt='(A)')trim(buffer)
      enddo
      if(is_cmp) then
        iseek = np*(sum(dims(1:ie_cmp))-dims(ie_cmp))*iprec
        write(buffer,fmt='(A)') &
          repeat(' ',indent)//'<Attribute Type="'//trim(ctypes(ie_cmp))//'" Center="Node" Name="'//trim(names(ie_cmp))//'_0">'
        write(unit=ixdmf,fmt='(A)')trim(buffer)
        indent = indent + 4
          write(buffer,fmt='(A,I1,A,I2,I9,A,I10,A)') repeat(' ',indent)// '<DataItem Format="Binary"' // &
                                                              ' DataType="Float" Precision="',iprec,'" Endian="Native"' // &
                                                              ' Dimensions="',dims(ie_cmp),np,'" Seek="',iseek,'">'
          write(unit=ixdmf,fmt='(A)')trim(buffer)
          indent = indent + 4
            write(buffer,fmt='(A,i7.7,A)') repeat(' ',indent)//trim(filename_i),ifld_cmp,trim(fileext_i)
            write(unit=ixdmf,fmt='(A)')trim(buffer)
          indent = indent - 4
        write(buffer,fmt='(A)') repeat(' ',indent)//'</DataItem>'
        write(unit=ixdmf,fmt='(A)')trim(buffer)
        indent = indent - 4
        write(buffer,fmt='(A)') repeat(' ',indent)//'</Attribute>'
        write(unit=ixdmf,fmt='(A)')trim(buffer)
      endif
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
end program gen_xdmf
