      program testread
      real rlat, rlon, rvalue
      integer ierr
      character*256 sdir

      sdir='/scratch/user/qying/AQ_forecast/fire/testread'

      rlat=39.9042
      rlon=116.4074
      call read_modis_500m(sdir,rlat,rlon,rvalue,ierr)
      print *,'rvalue=',rvalue
      rlat=25.1331
      rlon=119.9263
      call read_modis_500m(sdir,rlat,rlon,rvalue,ierr)
      print *,'rvalue=',rvalue
      end
      
