      subroutine read_modis_500m(sdir, rlat, rlon, rvalue, ierr)

      real rlat, rlon 
      real rvalue
      character*256 sdir
      integer ierr

c this information is copied from the 'index' file
      integer, parameter :: nx=2400, ny=2400
      integer, parameter :: nz=1
      integer, parameter :: wordsize=1
      integer, parameter :: endian=1
      real, parameter    :: scalefactor = 1.0
      integer, parameter :: isigned = 0
      real, parameter    :: dx=0.00416667, dy=0.00416667
      real, parameter    :: known_lat=-89.9979167, known_lon=-179.9979167
      integer, parameter :: tile_x=2400, tile_y=2400

c data cache for the current tile 
      real, save :: rdata(nx, ny)
      character*24, save :: sfilesave =''

      character*256 sf
      character*24 sfile
      integer nlen
      
      integer ix, iy, isx, isy
      character*5 sx,sy,ex,ey

      iy=(rlat-known_lat)/dy+1
      ix=(rlon-known_lon)/dx+1

      isx=(ix/tile_x)*tile_x+1
      isy=(iy/tile_y)*tile_y+1
      write (sx,'(I5.5)') isx
      write (sy,'(I5.5)') isy
      write (ex,'(I5.5)') isx+tile_x-1
      write (ey,'(I5.5)') isy+tile_y-1
      sfile=sx//'-'//ex//'.'//sy//'-'//ey
!      print *,'save:',trim(sfilesave)
!      print *,'file:',trim(sfile)
      if (sfile.ne.sfilesave) then 
         sf=trim(sdir)//'/'//trim(sfile)
         nlen=len_trim(sf)
!         print *,'cache update:',trim(sfile)
         call read_geogrid(sf, nlen, rdata, nx, ny, nz,
     +           isigned, endian, scalefactor, wordsize, ierr)
         sfilesave=sfile
      endif
      rvalue=rdata(ix-isx+1, iy-isy+1)
      end
      

