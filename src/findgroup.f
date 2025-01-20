      subroutine findgroup(Rx,Ry,Rxc,Ryc,grptype,
     +                     Ndata,Igroup)
! Group center (rxgcenter, rygcenter) contains the coordiantes of the 
! first fire point. For this reason, it is necessary to process  
! MODIS files first. 
! Group type (grptype) is 0 if the first fire point is
! MODIS, otherwise it is 1.
! If the center of the current fire point lies within the
! pixel radius of the first point in the group, then we have
! an overlap. In this case, the group index is returned.       
      implicit none
      real Rx , Ry
      real Rxc(Ndata) , Ryc(Ndata)
      integer grptype(ndata)
      integer Ndata , Igroup
      integer i
      real D0  ! distance range
 
      real d
      Igroup = 0
      do i = 1 , Ndata
         if (grptype(i).eq.0) then 
           D0 = 1000.0/2.0 ! approximate radius (m) a MODIS pixel
         else
           D0 = 350.0/2.0  ! approximate radius of a VIIRS pixel
         endif
         d = sqrt((Rx-Rxc(i))**2.0+(Ry-Ryc(i))**2.0)
         if ( d.le.D0 ) then
            Igroup = i
            return
         endif
      enddo
      end
