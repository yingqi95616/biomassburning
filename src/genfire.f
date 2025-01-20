      program GenFire
c******************************************************************************
c
c The purpose of this program is to generate near real time
c fire emissions for air quality forecasting/modeling, particulately for
c the WRF/CMAQ model system.
c
c The program can generate emissions for any chemical
c mechanism, dependeing on the specation profiles provided as input
c files. The program also calculates plume rise internally using 
c meteorolgical files generated from MCIP. 
c
c written by: Qi Ying
c             TAMU, 9/15/2018
c
c Last modified by: Qi Ying
c             TAMU, 02/07/2024
c
c******************************************************************************
      implicit none
 
c ioapi headers
      include 'PARMS3.EXT'      ! I/O parameters definitions
      include 'FDESC3.EXT'      ! file header data structure
      include 'IODECL3.EXT'     ! I/O definitions and declarations
 
c name of the program
      character*16 pname
      data pname/'tamu_fire'/
      integer logunit
      character*16 sfenv
      character*256 sfile
      logical , external :: setenvvar
 
      include 'CONST.EXT'
c model parameters
c      
c method to calculate plume                                                        
c top and bottom. Valid options are 0 or 1
c it might be better to use option 1 
c as night time plume is quite thin
c see https://www.cmascenter.org/cmaq/science_documentation/pdf/ch09.pdf, 9-6
      integer , parameter :: IPVERT = 1                 

c how emissions are distributed between the top and bottom 
c option 0 - based on layer thickness
c option 1 - based on pressure differences in each layer
      integer , parameter :: ivdistr = 1

c emission factors based on
c "The Blended Global Biomass Burning Emission Product from MODIS,
c  VIIRS and GEO-stationary Satellites (GBBEPx)", NOAA
c 1=forest, 2=savana, 3=shrubland, 4=grass land, 5=cropland, 6=other
      integer , parameter :: NTYPE = 6
c species
      integer , parameter :: NSPC = 10
      integer , parameter :: IPM25 = 1
      integer , parameter :: ICO = 2
      integer , parameter :: IOC = 3
      integer , parameter :: IBC = 4
      integer , parameter :: ISO2 = 5
      integer , parameter :: ICO2 = 6
      integer , parameter :: ICH4 = 7
      integer , parameter :: INOX = 8
      integer , parameter :: INMHC = 9
      integer , parameter :: INH3 = 10
c maximum number of VOCs and PM species in each VOC/PM speciation profile 
      integer , parameter :: NVOC_MAX = 100
      integer , parameter :: NPM_MAX = 30
 
      integer , parameter :: INO2_EMIS = 1 , INO_EMIS = 2 , 
     &                       ICO_EMIS = 3 , INH3_EMIS = 4 , 
     &                       ISO2_EMIS = 5 , ICH4_EMIS = 6 , 
     &                       ICO2_EMIS = 7 , IBC_EMIS = 8 , IOC_EMIS = 9

c 20-category MODIS 
      integer , parameter :: NLANDUSETYPE = 21
      integer :: ildmap(NLANDUSETYPE)
      data ildmap/1 , 1 , 1 , 1 , 1 , 3 , 3 , 2 , 2 , 4 , 6 , 5 , 6 , 
     &     5 , 6 , 6 , 6 , 6 , 6 , 6, 6/
      real rlandusefrac(NLANDUSETYPE)
c    directory with the modis land use data
      character*256 sdir_geog 
 
c max number of fire pixels in the domain
      integer , parameter :: MAXFIRE = 100000           
c max number of fire groups per model grid
      integer , parameter :: MAXPERGRID = 3000          
c max number of total fire groups
      integer , parameter :: MAXGROUP = 100000          
c max number of pixels per group
      integer , parameter :: MAXMEMBER = 100            
c rate of biomass burned (kg/s) per MW FRP
c Wooster et al., JGR, 110(2005), D24311
      real , parameter :: BETA = 0.368                  
c keep the EC OC in the speciation profiles and rename them as PEC1 and POC1
c If set to false, the EC OC in the speciation profiles will be ignored 
      logical , parameter :: ECOC = .true.              

c convert Pa to mb
      real , parameter :: CONVPA = 1.0E-2

c emission factors in g/kg biomass burned 
      real ef(NSPC,NTYPE)
      data ef(IPM25,:)/12.3 , 7.35 , 9.3 , 5.4 , 5.8 , 8.03/
      data ef(ICO,:)/106.4 , 63.5 , 68.0 , 59.0 , 111.0 , 81.58/
      data ef(IOC,:)/7.74 , 4.6 , 6.6 , 2.6 , 3.3 , 4.968/
      data ef(IBC,:)/0.408 , 0.435 , 0.5 , 0.37 , 0.69 , 0.4806/
      data ef(ISO2,:)/0.89 , 0.58 , 0.68 , 0.48 , 0.4 , 0.606/
      data ef(ICO2,:)/1586.0 , 1704.0 , 1716.0 , 1692.0 , 1537.0 , 
     &     1647.04/
      data ef(ICH4,:)/5.42 , 2.05 , 2.6 , 1.5 , 6.0 , 3.514/
      data ef(INOX,:)/2.0 , 3.35 , 3.9 , 2.8 , 3.5 , 3.11/
      data ef(INMHC,:)/4.9 , 3.4 , 3.4 , 3.4 , 7.0 , 4.42/
      data ef(INH3,:)/2.152 , 0.845 , 1.2 , 0.49 , 2.3 , 1.3974/
 
c speciation profiles for VOCs
c read speciation profiles from input text files
      real rvocspec(NVOC_MAX,NTYPE)
      real rpmspec(NVOC_MAX,NTYPE)
 
      character*16 sspec(9+NVOC_MAX+NPM_MAX+1)
      data sspec(INO2_EMIS)/'NO2'/
      data sspec(INO_EMIS)/'NO'/
      data sspec(ICO_EMIS)/'CO'/
      data sspec(INH3_EMIS)/'NH3'/
      data sspec(ISO2_EMIS)/'SO2'/
      data sspec(ICH4_EMIS)/'CH4'/
      data sspec(ICO2_EMIS)/'CO2'/
      data sspec(IBC_EMIS)/'PEC'/
      data sspec(IOC_EMIS)/'POC'/

      integer npm_emis
      integer nvoc_emis
 
c met variables
      real , allocatable :: temp3d(:,:,:,:) , uwind(:,:,:,:) , 
     &                      vwind(:,:,:,:) , q(:,:,:,:) , zf(:,:,:,:) , 
     &                      zh(:,:,:,:)
      real , allocatable :: temp2d(:,:,:) , veg(:,:) , air_dens(:,:,:) , 
     &                      ustar(:,:,:) , pbl(:,:,:) , prsfc(:,:,:)
      real , allocatable :: vglvs_gd(:)
      real vgtop_gd
 
 
      real , allocatable :: ddzf(:) , qv(:) , ta(:) , uw(:) , vw(:) , 
     &                      zh1d(:) , zf1d(:) , press1d(:)
      real hmix , hts , psfc , ts , ustar1
      integer lpbl
      real , allocatable :: dthdz(:) , wspd(:)
 
c gridded daily fire radiative power, MW
      real :: frp(MAXFIRE)
      character dayornight(MAXFIRE) , dn1
      character satellite(MAXFIRE) 
      character*16 sat1
      integer acqtime(MAXFIRE) , acqtm1
      real xpos(MAXFIRE) , ypos(MAXFIRE)
      integer , allocatable :: nfiregrid(:,:)
      integer , allocatable :: firegrpid(:,:,:)
      integer nfire
      real frp1d(MAXMEMBER) , hour1d(MAXMEMBER)
 
      integer nemislayers
      integer ntsteps
 
c emission
      real e_model(9+NVOC_MAX+NPM_MAX+1)
      real e(NSPC,NTYPE)
      real zplm
      real tstk , wstk
      integer lstk
      logical fireflg
      real burnedarea , hflx , bflx
      real frp1 , frp2,  frptot, burnedareatot
      real , allocatable :: rfraction(:) , tfrac(:)
      real rsum
      integer lbot , ltop
      real ztop , zbot , zdiff , ddz
      real ptop , pbot , pdiff 
      real besize , sfract
 
      logical , external :: setlam , ll2lam
      logical , external :: setpol , ll2pol
      integer , external :: junit
 
      integer i , j , k , nfile , ispc , ix , iy , iloc , ilduse , iz
      integer ilat , ilon , icnfd , ild , ierr , ifrp , ifire , id ,
     +        ip, itype, id1
      integer isat , idn , iacqtime , l
      integer iacqdate
      integer iunit , iunit_o
      integer idate , itime
      integer isdate, istime, tstep
      character*16 sname1
      character*2 c2
      character*2 fid , fid1
      real r1 , rlat , rlon , rfrp , rx , ry , rcnfd , x0 , y0 , rfac
      character*16 sdata(100)
      integer ndatacol

      integer iyear, imonth, iday
      integer iyear_aq, imonth_aq, iday_aq
      integer ifiretype
 
c fire point groups
c -----------------
c ngroup: total number of groups
c rxgcenter, rygcenter: center of the group      
c ngrpmember: number of fire points (pixels) in the group
c rxgmember, rygmember: location of each fire point in the group
c fireidmember: index of the fire in the total list of fire points.
c frpgrp: hourly FRP for each fire group
      integer igroup , ngroup , igrpmember
      real rxgcenter(MAXGROUP) , rygcenter(MAXGROUP)
      integer grptype(MAXGROUP)
      integer ngrpmember(MAXGROUP)
      real rg(MAXGROUP)
      real rxgmember(MAXMEMBER,MAXGROUP)
      real rygmember(MAXMEMBER,MAXGROUP)
      integer fireidmember(MAXMEMBER,MAXGROUP)
      real frpgrp(25,MAXGROUP)

! Connected fire groups
! ---------------------
! Each connect fire group is treated as one fire. 
! It includes fire groups that are adjacent ("connected") to each other
! burnarea_connected_group: total pixel area of the connected group, 
!                           i.e., (burned area, km2)
! frp_connected_group: total FRP for the connected group      
! These are used for plume rise calculations. Big fire will have more
! burned area and large FRP, which could potentially leads to higher 
! plume rise.       
      integer, parameter :: max_connected_groups = 50000
      integer n_connected_groups
      real burnarea_connected_group(max_connected_groups)
      real frp_connected_group(max_connected_groups)
      integer id_connected_group(maxgroup)

! option to generate text/binary intermediate output files
! binary output has not been implemented in this version
      logical b_txtoutput

! set to true to genreate extensive debug outputs
      logical bdebug

c******************************************************************************
c Start of the program
 
      logunit = init3()
      read (*,*) bdebug

c read year month day
      read (*,*) iyear, imonth, iday 
 
c read speciation profiles
      iunit = junit()
      nvoc_emis = 0
      do i = 1 , NTYPE
         read (*,'(a)') sfile
         print * , 'reading...'//trim(sfile)
         open (unit=iunit,file=sfile,status='old')
         do while ( .true. )
            read (iunit,*,end=50) sname1 , r1
            call slocate(sspec,9+nvoc_emis,sname1,iloc)
            if ( iloc.eq.0 ) then
               nvoc_emis = nvoc_emis + 1
               sspec(9+nvoc_emis) = sname1
               iloc = nvoc_emis + 9
            endif
            rvocspec(iloc-9,i) = r1
         enddo
 50      close (iunit)
      enddo
 
      npm_emis = 0
      do i = 1 , NTYPE
         read (*,'(a)') sfile
         print * , 'reading...'//trim(sfile)
         open (unit=iunit,file=sfile,status='old')
         do while ( .true. )
            read (iunit,*,end=100) sname1 , r1
            if ( ECOC ) then
               if ( trim(sname1).eq.'PEC' ) sname1 = 'PEC1'
               if ( trim(sname1).eq.'POC' ) sname1 = 'POC1'
            endif
            if ( trim(sname1).eq.'PEC' .or. trim(sname1).eq.'POC' )
     &           cycle
            call slocate(sspec,9+nvoc_emis+npm_emis,sname1,iloc)
            if ( iloc.eq.0 ) then
               npm_emis = npm_emis + 1
               sspec(9+nvoc_emis+npm_emis) = sname1
               iloc = npm_emis + nvoc_emis + 9
            endif
            rpmspec(iloc-9-nvoc_emis,i) = r1
         enddo
 100     close (iunit)
      enddo

      sspec(9+nvoc_emis+npm_emis+1)='CO_FIRE'

c land use is now read from the WPS geogrid files
      read ( *, '(a)') sdir_geog
 
c read met file (metcro3d)
      read (*,'(a)') sfile
      print * , 'reading...'//trim(sfile)
      sfenv = 'INFILE'
      if ( .not.setenvvar(sfenv,sfile) )
     &      call m3exit(pname,0,0,'FAIL TO SET INPUT FILE',-1)
      if ( .not.open3(sfenv,FSREAD3,pname) )
     &      call m3exit(pname,0,0,'FAIL TO OPEN INPUT FILE',-1)
c get file description
      if ( .not.desc3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO GET FILE DESC',-1)
 
      allocate (pbl(NCOls3d,NROws3d,MXRec3d))
      allocate (ustar(NCOls3d,NROws3d,MXRec3d))
      allocate (prsfc(NCOls3d,NROws3d,MXRec3d))
      allocate (veg(NCOls3d,NROws3d))
      allocate (nfiregrid(NCOls3d,NROws3d))
      allocate (firegrpid(NCOls3d,NROws3d,MAXPERGRID))
      allocate (temp2d(NCOls3d,NROws3d,MXRec3d))
      allocate (air_dens(NCOls3d,NROws3d,MXRec3d))
      allocate (zf(NCOls3d,NROws3d,NLAys3d,MXRec3d))
      allocate (zh(NCOls3d,NROws3d,NLAys3d,MXRec3d))
      allocate (q(NCOls3d,NROws3d,NLAys3d,MXRec3d))
      allocate (temp3d(NCOls3d,NROws3d,NLAys3d,MXRec3d))
      allocate (uwind(NCOls3d+1,NROws3d+1,NLAys3d,MXRec3d))
      allocate (vwind(NCOls3d+1,NROws3d+1,NLAys3d,MXRec3d))
      allocate (vglvs_gd(0:NLAys3d))
      allocate (ddzf(NLAys3d))
      allocate (qv(NLAys3d))
      allocate (ta(NLAys3d))
      allocate (uw(NLAys3d))
      allocate (vw(NLAys3d))
      allocate (zh1d(NLAys3d))
      allocate (zf1d(0:NLAys3d))
      allocate (press1d(0:NLAys3d))
      allocate (dthdz(NLAys3d))
      allocate (wspd(NLAys3d))
      allocate (rfraction(NLAys3d))
      allocate (tfrac(NLAys3d))
 
c read the metcro3d file
      idate = SDAte3d
      itime = STIme3d
      do k = 1 , MXRec3d
         sname1 = 'TA'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,temp3d(:,:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'QV'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,q(:,:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'ZF'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,zf(:,:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'ZH'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,zh(:,:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'DENS'
         if ( .not.read3(sfenv,sname1,1,idate,itime,air_dens(:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         call nextime(idate,itime,TSTep3d)
      enddo
      nemislayers = NLAys3d
      vgtop_gd = VGTop3d
C Store local layer information
      j = lbound(VGLvs3d,1)
      vglvs_gd(0) = VGLvs3d(j)
      do i = 1 , NLAys3d
         j = j + 1
         vglvs_gd(i) = VGLvs3d(j)
      enddo
      if ( .not.close3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO CLOSE INPUT FILE',-1)
 
c read the metdot3d file
      read (*,'(a)') sfile
      if ( .not.setenvvar(sfenv,sfile) )
     &      call m3exit(pname,0,0,'FAIL TO SET INPUT FILE',-1)
      if ( .not.open3(sfenv,FSREAD3,pname) )
     &      call m3exit(pname,0,0,'FAIL TO OPEN INPUT FILE',-1)
c get file description
      if ( .not.desc3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO GET FILE DESC',-1)
      idate = SDAte3d
      itime = STIme3d
      do k = 1 , MXRec3d
         sname1 = 'UWIND'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,uwind(:,:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'VWIND'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,vwind(:,:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         call nextime(idate,itime,TSTep3d)
      enddo
      if ( .not.close3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO CLOSE INPUT FILE',-1)
 
c read the metcro2d file
      read (*,'(a)') sfile
      if ( .not.setenvvar(sfenv,sfile) )
     &      call m3exit(pname,0,0,'FAIL TO SET INPUT FILE',-1)
      if ( .not.open3(sfenv,FSREAD3,pname) )
     &      call m3exit(pname,0,0,'FAIL TO OPEN INPUT FILE',-1)
c get file description
      if ( .not.desc3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO GET FILE DESC',-1)
      idate = SDAte3d
      itime = STIme3d
      do k = 1 , MXRec3d
         sname1 = 'TEMP2'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,temp2d(:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'PBL'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,pbl(:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'USTAR'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,ustar(:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
         sname1 = 'PRSFC'
         if ( .not.read3(sfenv,sname1,-1,idate,itime,prsfc(:,:,k)) )
     &        call m3exit(pname,idate,itime,
     &                    'failed to read temp3d data',-1)
 
         if ( k.eq.1 ) then
            sname1 = 'VEG'
            if ( .not.read3(sfenv,sname1,-1,idate,itime,veg) )
     &           call m3exit(pname,idate,itime,
     &                       'failed to read temp3d data',-1)
         endif
         call nextime(idate,itime,TSTep3d)
      enddo
      ntsteps = MXRec3d
      if ( .not.close3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO CLOSE INPUT FILE',-1)
 
      x0 = XORig3d
      y0 = YORig3d

      if ( GDTyp3d.eq.LAMGRD3 ) then
         if ( .not.setlam(sngl(P_Alp3d),sngl(P_Bet3d),sngl(P_Gam3d),
     &         sngl(XCEnt3d),sngl(YCEnt3d)) ) 
     &      call m3exit(pname,0,0,'Lambert projection setup error',2)
      elseif ( GDTyp3d.eq. POLGRD3 ) then 
         if ( .not.setpol(sngl(P_Alp3d),sngl(P_Bet3d),sngl(P_Gam3d),
     &         sngl(XCEnt3d),sngl(YCEnt3d)) ) 
     &      call m3exit(pname,0,0,
     &                  'polar steogrphic projection setup error',2)
      else
         call m3exit(pname,0,0,'project not supported yet.',2)
      endif
 
c read the fire emission inventories
      frp = 0.0
c nfire is the total number of fire points in all
c inventories processed
      nfire = 0
c ngroup is the total number of fire groups
c each group includes one or more fire points from different inventories
c that overlap each other. 
c MODIS pixels are bigger so one pixel can cover several VIIRS points. 
      ngroup = 0

      read (*,*) nfile
      do i = 1 , nfile
         read (*,'(a)') sfile
         print *,trim(sfile)
         open (unit=iunit,file=sfile,status='old')
         call readcvs1(iunit,sdata,100,ndatacol,ierr)

! make sure these required fields are available
         sname1 = 'frp'
         call slocate(sdata,ndatacol,sname1,ifrp)
         if ( ifrp.eq.0 ) call m3exit(pname,idate,itime,
     &                                'no FRP in input file',-1)
         sname1 = 'latitude'
         call slocate(sdata,ndatacol,sname1,ilat)
         if ( ilat.eq.0 ) call m3exit(pname,idate,itime,
     &                                'no latitude in input file',-1)
         sname1 = 'longitude'
         call slocate(sdata,ndatacol,sname1,ilon)
         if ( ilon.eq.0 ) call m3exit(pname,idate,itime,
     &                                'no longitude in input file',-1)
         sname1 = 'confidence'
         call slocate(sdata,ndatacol,sname1,icnfd)
         if ( icnfd.eq.0 ) call m3exit(pname,idate,itime,
     &                                 'no longitude in input file',-1)
         sname1 = 'satellite'
         call slocate(sdata,ndatacol,sname1,isat)
         if ( isat.eq.0 ) call m3exit(pname,idate,itime,
     &                                'no satellite info',-1)
         sname1 = 'daynight'
         call slocate(sdata,ndatacol,sname1,idn)
         if ( idn.eq.0 ) call m3exit(pname,idate,itime,
     &                                'no day or night info',-1)
         sname1 = 'acq_time'
         call slocate(sdata,ndatacol,sname1,iacqtime)
         if ( iacqtime.eq.0 )
     &      call m3exit(pname,idate,itime,'no acquisition time info',-1)
         sname1 = 'acq_date'
         call slocate(sdata,ndatacol,sname1,iacqdate)
         if ( iacqtime.eq.0 )
     &      call m3exit(pname,idate,itime,'no acquisition time info',-1)
         sname1 = 'type'
         call slocate(sdata,ndatacol,sname1,itype)
         if ( itype.eq.0 ) call m3warn(pname,idate,itime,
     &              'no thermal anomaly type information in input file')
 
! loop over each entry in the inventory file
         do while ( .true. )
            call readcvs1(iunit,sdata,100,ndatacol,ierr)
            if ( ierr.ne.0 ) exit

! DATA SCREENING STEP
! -------------------------------------
! (1) skip data not for the specific day
            read (sdata(iacqdate)(1:4),*) iyear_aq
            if (iyear_aq.ne.iyear) cycle
            read (sdata(iacqdate)(6:7),*) imonth_aq
            if (imonth_aq.ne.imonth) cycle
            read (sdata(iacqdate)(9:10),*) iday_aq
            if (iday_aq.ne.iday) cycle

            read (sdata(ilat),*) rlat
            read (sdata(ilon),*) rlon
            read (sdata(ifrp),*) rfrp
            read (sdata(isat),*) sat1

! (2) skip data with low confidence
!     20% cut for MODIS points. Changed to match FINN
!     For VIIRS, low confidence points will be ignored.
!     This is consistent with FINN. 
            if (trim(sat1).eq.'Terra'.or.trim(sat1).eq.'Aqua') then
              read (sdata(icnfd),*) rcnfd
              if ( rcnfd.lt.20.0 ) cycle
            else
              if ( trim(sdata(icnfd)).eq.'low') cycle 
            endif

! (3) skip non-vegetation burning data (type!=0, e.g., volcanos)
!     This is consistent with FINN
            if (itype.gt.0) then 
               read (sdata(itype),*) ifiretype
               if (ifiretype.ne.0) cycle
            endif

! (4) skip data if not in the model domain
            if (gdtyp3d .eq. LAMGRD3 ) then  
              if ( .not.ll2lam(rlon,rlat,rx,ry) )
     &            call m3exit(pname,0,0,'Lambert conversion error',2)
            elseif (gdtyp3d .eq. POLGRD3 ) then 
              if ( .not.ll2pol(rlon,rlat,rx,ry) )
     &            call m3exit(pname,0,0,'Polar conversion error',2)
            else
              call m3exit(pname,0,0,'project not supported yet.',2)
            endif

            ix = int((rx-x0)/XCEll3d) + 1
            iy = int((ry-y0)/YCEll3d) + 1
            if ( ix.le.0 .or. ix.gt.NCOls3d .or. iy.le.0 .or. 
     &           iy.gt.NROws3d ) cycle

! DATA PROCESSING
! at this point, we have a valid fire point 
            nfire = nfire + 1
! save the instrument information 
! T - MODIS from Terra, A - MODIS from Aqua, otherwise N- VIIRS 
            if (sat1(1:1).eq.'T') then 
              satellite(nfire)='T'
            elseif (sat1(1:1).eq.'A') then 
              satellite(nfire)='A'
            else
              satellite(nfire)='N'
            endif

            read (sdata(idn),*) dn1
            read (sdata(iacqtime),*) acqtm1

! basic information of the fire point
            frp(nfire) = rfrp
            dayornight(nfire) = dn1
            acqtime(nfire) = acqtm1
            xpos(nfire) = rlon
            ypos(nfire) = rlat

            if (bdebug) print *, 'got fire:',nfire, sat1,rfrp, dn1, acqtm1, rlon, rlat

! Now it is necessary to check if a fire point overlaps with
! fire points processed earlier. This happens when two MODIS
! satellites detect the same fire or a small VIIRS fire
! point is within a MODIS fire point.

            call findgroup(rx,ry,rxgcenter,rygcenter,grptype,
     +                     ngroup,igroup)

            if (bdebug) print *,'find group result:',igroup

! a new group is created if the current pixel does not
! overlap with existing groups
            if ( igroup.eq.0 ) then
c              increase the fire group count                    
               ngroup = ngroup + 1
c              record the center location of the fire group
c              which is the location of the first fire
               rxgcenter(ngroup) = rx
               rygcenter(ngroup) = ry
c              there can multiple points in a fire group
c              the locations of the points are recorded
c              as well as the index in the linear list of fire points
               ngrpmember(ngroup) = 1              
               rxgmember(1,ngroup) = rlon          
               rygmember(1,ngroup) = rlat
               fireidmember(1,ngroup) = nfire
c              record the type of the fire group. 
               if (satellite(nfire).eq.'T' .or. 
     +             satellite(nfire).eq.'A') then 
                  grptype(ngroup)=0       ! MODIS group
               else
                  grptype(ngroup)=1       ! VIIRS group
               endif 
               if (bdebug) print *,'create group:',ngroup
               if (bdebug) print *,'center:', rx,ry

! the fire point is in overlap with an existing group (igroup!=0)               
            else
               igrpmember = ngrpmember(igroup)
c              fid is formed as 'T/A/N' + 'D/N' 
               fid = satellite(nfire)//dayornight(nfire)

c MODIS pixel within a VIIRS group (group type1)
c This is not allowed. MODIS should be processed first
               if ((satellite(nfire).eq.'T' .or.
     +              satellite(nfire).eq.'A') .and.
     +              grptype(igroup).eq.1 )  then 
                  print *,'need to process all MODIS pixels first'
                  print *,'change input file sequence'
                  stop
               endif 

c VIIRS pixel within a MODIS pixel group
c
c   Try to see if there is already a VIIRS pixel under
c   the MODIS pixel with the same day/night designation. 
c   If so, we do not create a new member in the group.
c   Instead, we add the FRP to the existing VIIRS point.
c   However, if the current VIIRS pixel time (D/N) is different
c   form the existing one, then a new point will be added to the current
c   group. 
c
c   The VIIRS points have acquition time that are different than 
c   the MODIS points. They are used to interpolate the hourly 
c   FRP. The entire group is treated as a MODIS pixel group.
c   The VIIRS FRP does not add to the MODIS FRP, thus does not 
c   lead to double counting of the emissions. 
c
               if (satellite(nfire).eq.'N' .and. 
     +             grptype(igroup).eq.0) then 
                  do l=1, igrpmember
                    id=fireidmember(l,igroup)
                    fid1=satellite(id)//dayornight(id)
                    if (fid.eq.fid1) then 
                      frp(id)=frp(id)+rfrp
                      if (bdebug) then 
                         print *,'find existing fire:',fid
                         print *,'added FRP:',rfrp
                         print *,'FRP updated to:',frp(id)
                      endif
                      goto 150
                    endif
                  enddo 
               endif

c MODIS pixel within a MODIS pixel group
c remove duplicate points
c this usually should not occur unless two MODIS files with the same
c spatial coverages are used
               if ((satellite(nfire).eq.'T' .or.
     +              satellite(nfire).eq.'A') .and.
     +              grptype(igroup).eq.0 ) then 
                 do l = 1 , igrpmember            ! average points 
                    id = fireidmember(l,igroup)
                    fid1 = satellite(id)//dayornight(id)
                    if ( fid.eq.fid1 ) then 
                      if (bdebug) then 
                         print *,'MODIS pixels over lap'
                         print *,'Original FRP  :',frp(id)
                         print *,'Current  FRP  :',rfrp
                         print *,'FRP updated to:',(frp(id)+rfrp)/2.0
                      endif
                      frp(id)=(frp(id)+rfrp)/2.0
                      goto 150 
                    endif
                 enddo
               endif

c VIIRS pixel within a VIIRS pixel group
c remove duplicate points if a matching D/N points are found
c this should not happen unless two VIIRS files with the same spatial
c coverage are used.
               if (satellite(nfire).eq.'N' .and. 
     +             grptype(igroup).eq.1) then 
                 do l = 1 , igrpmember            ! average points
                    id = fireidmember(l,igroup)
                    fid1 = satellite(id)//dayornight(id)
                    if ( fid.eq.fid1 ) then 
                      if (bdebug) then 
                         print *,'VIIRS pixels over lap'
                         print *,'Original FRP  :',frp(id)
                         print *,'Current  FRP  :',rfrp
                         print *,'FRP updated to:',(frp(id)+rfrp)/2.0
                      endif
                      frp(id)=(frp(id)+rfrp)/2.0
                      goto 150 
                    endif
                 enddo
               endif

c at this point, add the fire as a new member in this fire group
c adjust the center of the group 
               rxgcenter(igroup) = (rxgcenter(igroup)*igrpmember+rx)
     &                             /(igrpmember+1)
               rygcenter(igroup) = (rygcenter(igroup)*igrpmember+ry)
     &                             /(igrpmember+1)
               ngrpmember(igroup) = ngrpmember(igroup) + 1
               rxgmember(igrpmember+1,igroup) = rlon
               rygmember(igrpmember+1,igroup) = rlat
               fireidmember(igrpmember+1,igroup) = nfire
               if (bdebug) then 
                 print *,'find new member',ngrpmember(igroup)
                 print *,'new center:',rxgcenter(igroup),rygcenter(igroup)
               endif
            endif
 150     enddo
         close (iunit)
      enddo


c After grouping the fire points, we need to handle gridding of the
c fires to AQ model grids.
c   Number of fire groups in each model grid 
      nfiregrid = 0 
      do i = 1 , ngroup
         ix = int((rxgcenter(i)-x0)/XCEll3d) + 1
         iy = int((rygcenter(i)-y0)/YCEll3d) + 1
         nfiregrid(ix,iy) = nfiregrid(ix,iy) + 1
         k = nfiregrid(ix,iy)
         firegrpid(ix,iy,k) = i
!         print *,'group:',i, ngrpmember(i), grptype(i), ix, iy
c        print *,i,ngrpmember(i), ix, iy
      enddo
 
c generate hourly FRP for each fire group
c here we need a better way of generating hourly emissions. 
c there are some references talking about this
c also, it is useful to use the TEMPO satellite to help determing
c dirunal variations of emissions. 
c the other thing that needs to be addressed is the 
      frpgrp = 0.0
! fill the entire day with FRP
      do i=1, ngroup
         igrpmember=ngrpmember(i)
         do k=1, igrpmember
           id=fireidmember(k,i)
           frp1d(k)=frp(id)
           hour1d(k)=acqtime(id)/100.0+1.0
         enddo
c sort the FRP data in the same fire group         
         do k = 1 , igrpmember
            do l = k + 1 , igrpmember
               if ( hour1d(k).gt.hour1d(l) ) then
                  call swap(hour1d(k),hour1d(l))
                  call swap(frp1d(k),frp1d(l))
               endif
            enddo
         enddo
         if (bdebug) then 
           if (igrpmember.gt.1) then 
             do k=1, igrpmember
               id = fireidmember(k,i)
                print *,'sorted:',k,hour1d(k),frp1d(k),
     +                  satellite(id),dayornight(id)
             enddo
           endif
         endif
c interpolate the FRP to each hour         
         do k=1, ntsteps
           if (k.le.hour1d(1)) then 
               frpgrp(k,i)=frp1d(1)
           elseif  (k.ge.hour1d(igrpmember)) then 
               frpgrp(k,i)=frp1d(igrpmember)
           else
             do j=2, igrpmember
               if (k.gt.hour1d(j-1) .and. k.le.hour1d(j)) then 
                 frpgrp(k,i)=frp1d(j)
                 exit
               endif
             enddo
           endif
         enddo
      enddo


! for all the fire groups, divide them into connected groups
! area burned and and frp are calculated for each connected group
      do i=1, ngroup
        if (grptype(i).eq.0) then 
          rg(i)=1000.0 ! radius of the MODIS pixel
        else
          rg(i)=375.0 ! radius of a VIIRS pixel 
        endif
      enddo

      call gridgroup(ngroup, rxgcenter, rygcenter, rg,
     +               max_connected_groups,
     +               n_connected_groups, 
     +               id_connected_group)

      if (bdebug) then 
        print *, n_connected_groups
        do i=1, ngroup 
          print *,i, rxgcenter(i), rygcenter(i), rg(i), id_connected_group(i)
        enddo
      endif

      frp_connected_group=0.0
      burnarea_connected_group=0.0
      do i=1, ngroup
        j=id_connected_group(i)
        igrpmember=ngrpmember(i)
        do k=1, igrpmember
          frp_connected_group(j)=frp_connected_group(j)+frpgrp(k,i)
        enddo
        if (grptype(j).eq.0) then ! MODIS
          burnarea_connected_group(j)=burnarea_connected_group(j)+1.0
        else
          burnarea_connected_group(j)=burnarea_connected_group(j)+0.14
        endif

      enddo
        if (bdebug) then 
          print *,'burned area and frp:'
          do i=1, n_connected_groups
            print *,i,burnarea_connected_group(i), frp_connected_group(i)
          enddo
        endif

c generate output file
      read (*,'(a)') sfile
      if (index(sfile,'.txt').gt.0) then 
        b_txtoutput=.true.
        iunit_o = junit()
        open (unit=iunit_o,file=sfile,status='new')
        write (iunit_o,'(I4)') 9 + nvoc_emis + npm_emis + 1
        do i = 1 , 7 
          write (iunit_o,'(A16,1x,A16)') sspec(i),'moles/s'
        enddo
        do i=8,9
          write (iunit_o,'(A16,1x,A16)') sspec(i),'g/s'
        enddo
        do i = 10 , 9 + nvoc_emis ! + npm_emis
          write (iunit_o,'(A16,1x,A16)') sspec(i),'moles/s'
        enddo
        do i = 10+nvoc_emis , 9 + nvoc_emis+npm_emis 
          write (iunit_o,'(A16,1x,A16)') sspec(i),'g/s'
        enddo
        write (iunit_o,'(A16,1x,A16)') sspec(9+nvoc_emis+npm_emis+1),'moles/s'
        write (iunit_o,*) nemislayers 
      else  
        b_txtoutput=.false.
      endif
 
c loop over each point and calculate emission and plume rise
      do k = 1 , ntsteps

         if (b_txtoutput) write (iunit_o,*) 'Hour:' , k
         do i = 1 , NCOls3d
            do j = 1 , NROws3d
!               if ( nfiregrid(i,j).le.0 .or. veg(i,j).le.0 ) cycle
               if ( nfiregrid(i,j).le.0 ) cycle
               ! total FRP and burned area in each grid
               frptot = 0.0
               burnedareatot = 0.0
               do ifire = 1 , nfiregrid(i,j)
                  id = firegrpid(i,j,ifire)
                  frptot = frptot + frpgrp(k,id)
                  if (grptype(id).eq.0) then 
                     burnedareatot = burnedareatot + 1.0
                   else
                     burnedareatot = burnedareatot + 0.14
                   endif
               enddo
               if ( frptot.eq.0 ) cycle

c             calculate land use fractions
               rlandusefrac = 0.0
               ilduse=0

c             Loop over all fires in a model grid to find
c             land use fraction under the fire
c             This is more accurat than using the gridded land use
c             data from MCIP, as the output grid can be low
c             resolution 
               do ifire = 1, nfiregrid(i,j)
                  id = firegrpid(i,j,ifire) ! group id
                  do ip=1, ngrpmember(id)
                    rlon=rxgmember(ip,id)  
                    rlat=rygmember(ip,id)
                    call read_modis_500m(sdir_geog, rlat, rlon, r1, 
     +                                  ierr)
                    if (ierr.ne.0) then 
                       print *,'failed reading geogrid landuse data.'
                       stop
                    endif
                    if (r1.lt.1 .or. r1.gt.NLANDUSETYPE) then 
                      print *,'land use type out of range', r1, i, j,
     +                        rlat, rlon
                      stop
                    endif
                    l=int(r1)
                    rlandusefrac(l) = rlandusefrac(l) + 1
                    ilduse=ilduse+1
                  enddo
               enddo 
               do l=1, NLANDUSETYPE
                  rlandusefrac(l)=rlandusefrac(l)/real(ilduse)
               enddo

c             calculate emission of each species (g/s)
               e = 0.0
               do ispc = 1 , NSPC
                  do ilduse = 1 , NLANDUSETYPE
                     ild = ildmap(ilduse)
                     e(ispc,ild) = e(ispc,ild)
     &                             + frptot*BETA*ef(ispc,ild)
     &                             *rlandusefrac(ilduse)
                  enddo
               enddo

               e_model = 0.0
c apply speciation profiles for major (6) landuse types               
               do ilduse = 1 , NTYPE
                  e_model(INO2_EMIS) = e_model(INO2_EMIS)
     &                                 + e(INOX,ilduse)*0.05/46.01
                  e_model(INO_EMIS) = e_model(INO_EMIS) + e(INOX,ilduse)
     &                                *0.95/30.01
                  e_model(ISO2_EMIS) = e_model(ISO2_EMIS)
     &                                 + e(ISO2,ilduse)/64.07
                  e_model(ICO_EMIS) = e_model(ICO_EMIS) + e(ICO,ilduse)
     &                                /28.01
                  e_model(ICO2_EMIS) = e_model(ICO2_EMIS)
     &                                 + e(ICO2,ilduse)/44.01
                  e_model(ICH4_EMIS) = e_model(ICH4_EMIS)
     &                                 + e(ICH4,ilduse)/16.04
                  e_model(INH3_EMIS) = e_model(INH3_EMIS)
     &                                 + e(INH3,ilduse)/17.031
                  e_model(IBC_EMIS) = e_model(IBC_EMIS) + e(IBC,ilduse)
                  e_model(IOC_EMIS) = e_model(IOC_EMIS) + e(IOC,ilduse)
                  if ( e(INMHC,ilduse).gt.0 ) then
c                    convert NMHC to TOG
                     rfac = e(INMHC,ilduse)
     &                      /(e(INMHC,ilduse)+e(ICH4,ilduse))
                     do ispc = 1 , nvoc_emis
                        e_model(9+ispc) = e_model(9+ispc)
     &                     + e(INMHC,ilduse)/rfac*rvocspec(ispc,ilduse)
                     enddo
                  endif
                  if ( e(IPM25,ilduse).gt.0 ) then
                     do ispc = 1 , npm_emis
                        e_model(9+nvoc_emis+ispc)
     &                     = e_model(9+nvoc_emis+ispc) + e(IPM25,ilduse)
     &                     *rpmspec(ispc,ilduse)
                     enddo
                  endif
               enddo
c              add CO_FIRE as a wildfire emission tracer
               e_model(9+nvoc_emis+npm_emis+1)=e_model(ICO_EMIS)
               

c plume rise calculation
c as plume rise is not linear with frp, needs to be calculated individually
c modified so that for each fire group in a connected group, use the 
c rfp and burned area from the connected group

               fireflg = .true.
c              no stack for open burning fire              
               hts = 0.0 
c              surface meteorology information
c              hmix: mixing height
c              psfc: surface pressure
c              ts  : temperature 
               hmix = pbl(i,j,k)
               psfc = prsfc(i,j,k)
               ts = temp2d(i,j,k)
               do iz = 1 , nemislayers
c                 ddzf: inverse of the layer height
                  if ( iz.eq.1 ) then
                     ddzf(iz) = 1.0/zf(i,j,iz,k)
                  else
                     ddzf(iz) = 1.0/(zf(i,j,iz,k)-zf(i,j,iz-1,k))
                  endif
c                 water vapor, temperature, wind, pressure, and center and face
c                 height of each layer
                  qv(iz) = q(i,j,iz,k)
                  ta(iz) = temp3d(i,j,iz,k)
                  uw(iz) = (uwind(i,j,iz,k)+uwind(i+1,j,iz,k))*0.5
                  vw(iz) = (vwind(i,j,iz,k)+vwind(i,j+1,iz,k))*0.5
                  zh1d(iz) = zh(i,j,iz,k)
                  zf1d(iz) = zf(i,j,iz,k)
                  press1d(iz) = (vglvs_gd(iz)*(psfc-vgtop_gd)+vgtop_gd)
     &                          *CONVPA
               enddo
               zf1d(0) = 0.0
c              note: pressure needs to be in mb               
               press1d(0) = psfc*CONVPA
               psfc = psfc*CONVPA
 
c               print *,'calling preplm'
c               do l=1, nemislayers
c                 print *,k,l,uw(l),vw(l)
c               enddo
               call preplm(fireflg,nemislayers,hmix,hts,psfc,ts,ddzf,qv,
     &                     ta,uw,vw,zh1d,zf1d(1:),press1d,lstk,lpbl,
     &                     tstk,wstk,dthdz,wspd)
 
               ustar1 = max(ustar(i,j,k),0.1)   ! make sure ustar is not too small
 
               rfraction = 0.0

               do ifire = 1 , nfiregrid(i,j)
c                 group id
                  id = firegrpid(i,j,ifire)
c                 connected group index 
                  id1=id_connected_group(id)
c                 FRP in connected group. This is for plume rise                   
                  frp1 = frp_connected_group(id1)
                  if (frp1.eq.0) cycle
c                 this is for plume rise weighting in the grid
                  frp2 = frpgrp(k,id)                    
                  burnedarea=burnarea_connected_group(id1)*1.0e6

                  if (bdebug) then 
                     print *,'group:',id, 'connected group:',id1,frp1,burnedarea
                  endif
                   
c                 convert units to W/m2                   
                  hflx = (frp1*1.0E6/burnedarea)
c                 convert units to m K/s                  
                  hflx = hflx/(CPD*air_dens(i,j,k))
c                 convert bouncy heat flux to m4/s3                  
                  bflx = frp1*1E6/0.2931*2.58E-6
c              print *,'calling fire_plmris:', lstk, hflx, bflx, tstk,
c     +               ustar1, wstk
c              do l=1, nemislayers
c               print *,l,dthdz(l), ta(l), wspd(l), zf1d(l)
c              enddo
                  call fire_plmris(nemislayers,lstk,hflx,hmix,bflx,tstk,
     &                             ustar1,dthdz,ta,wspd,zf1d,wstk,zplm)
                  !print *,'plume rise:',zplm

c distribution fraction
C Determine the bottom and top heights of the plume.
                  if ( IPVERT.eq.0 ) then
C Default Turner approach.  Plume thickness = amount of plume rise
c plume rise dh = zplm minus the stack height stkht
                     ztop = 1.5*zplm
                     zbot = 0.5*zplm
                  else
c alternative method to compute plume top/bot heights
                     call plsprd(dthdz,zf1d,nemislayers,zplm,ztop,zbot)
                  endif

                  tfrac=0.0

c               adjustment for smoldering fraction                  
c                 this is the smoldering fraction, as described in
c                 Pouliot  et al. (2005)
c                 "Wildfire emission modeling: integrating bluesky and
c                 SMOKE". 
c                 burned area is in m2
                  besize = 0.0703*log(0.000247105*burnedarea) + 0.3
                  besize = min(besize,1.0)
                  sfract = 1.0 - besize

c                !print *,'ztop, zbottom:',ztop, zbot
c compute lbot, ltop such that
C  ZZF( LBOT-1 ) <= ZBOT < ZZF( LBOT ) and
C  ZZF( LTOP-1 ) <= ZTOP < ZZF( LTOP )
                  lbot = nemislayers
                  ! the layer where the bottom of the plume is
                  ! note that if plume bottom is above the top level of
                  ! the emission file, the top layer is used
                  do iz = 1 , nemislayers - 1
                     if ( zbot.le.zf1d(iz) ) then
                        lbot = iz
                        exit
                     else
                        tfrac(iz) = 0.0    ! fractions below plume
                     endif
                  enddo
                  !print *,'lbot=',lbot

                  if ( ztop.le.zf1d(lbot) ) then
                  ! if the plume top is also in this layer 
                     tfrac(lbot) = 1.0
                     ltop = lbot
                     ! fractions above this layer are set to zero
                     do iz = lbot + 1 , nemislayers
                        tfrac(iz) = 0.0
                     enddo
                  elseif ( lbot.eq.nemislayers ) then
                  ! if plume bottom exceeds the top layer
                  ! all emisisons goes to the top layer
                     tfrac(lbot) = 1.0
                     do iz = 1 , nemislayers - 1
                        tfrac(iz) = 0.0
                     enddo
                  else                            ! plume crosses layers
                  ! when we reach here, plume bottm is within the
                  ! emission layers
                  ! now check where the plume top is
                  ! is the top exceeds the top layer, it will fallback
                  ! to the top layer
                     ltop = nemislayers
                     do iz = lbot + 1 , nemislayers
                        if ( ztop.le.zf1d(iz) ) then
                           ltop = iz
                           exit
                        endif
                     enddo
                     !print *,'ltop=',ltop
                     
                     ! vertical distribution can be done based on
                     ! height or pressure differences  
                     if (ivdistr.eq.0) then    
                       zdiff = ztop - zbot
                       if ( zdiff.gt.0.0 ) then
                         ddz = 1.0/zdiff
                         tfrac(lbot) = ddz*(zf1d(lbot)-zbot)
                         tfrac(ltop) = ddz*(ztop-zf1d(ltop-1))
                       else
                         ! zdiff .le. 0
                         lbot = 1 ! ?do we need this?
                         tfrac(lbot) = 1.0
                      endif
                      ! in the plume
                      do iz = lbot + 1 , ltop - 1
                        tfrac(iz) = ddz*(zf1d(iz)-zf1d(iz-1))
                      enddo
                      ! above the plume                     
                      do iz = ltop + 1 , nemislayers
                        tfrac(iz) = 0.0
                      enddo
                    elseif (ivdistr.eq.1) then 
                      ! press1d is pressre at layer boundaries 
                      ptop=press1d(ltop)   
                      pbot=press1d(lbot-1)
                      pdiff = ptop - pbot
                      ddz = 1.0/pdiff
                      do iz = lbot, ltop
                        tfrac(lbot) = ddz*(press1d(iz)-press1d(iz-1))
                      enddo
                      do iz = ltop + 1 , nemislayers
                        tfrac(iz) = 0.0
                      enddo
                    endif
                  endif
                  tfrac=tfrac*(1.0-sfract)
                  !print *,'bottom level',lbot

                  ! attribute smoldering fractions to layers below plume
                  ! bottom
                  ! if plume bottom is in the first layer or second
                  ! layer then add all smoldering fraction to the first layer
                  if (lbot.eq.1 .or. lbot.eq.2) then 
                    tfrac(1)=tfrac(1)+sfract
                  else
                    if (ivdistr.eq.0) then 
                       zdiff=zf1d(lbot-1)-zf1d(0)
                       ddz = 1.0/zdiff
                       do iz = 1, lbot - 1
                         tfrac(iz) = ddz*(zf1d(iz)-zf1d(iz-1))*sfract
                       enddo
                    elseif (ivdistr.eq.1) then 
                       pdiff=press1d(lbot-1)-press1d(0)
                       ddz = 1.0/pdiff
                       do iz = 1, lbot - 1
                         tfrac(iz) = ddz*(press1d(iz)-press1d(iz-1))
     &                               *sfract
                       enddo
                    endif
                  endif
 
                  do iz = 1 , nemislayers
                     rfraction(iz) = rfraction(iz) + tfrac(iz)
     &                               *frp2/frptot
!                      if (rfraction(iz).gt.0) 
!     +                   print *,'layer fraction:',iz, tfrac(iz)
                  enddo
                  !print *,'-----------'
               enddo

               if (bdebug) then 
                 rsum=0.0
                 do iz=1, nemislayers
                   rsum=rsum+rfraction(iz)
                   if (rfraction(iz).gt.0) then 
                      print *,'layer fraction:',iz, rfraction(iz)
                   endif
                 enddo
                 print *,'rsum=',rsum
               endif
c print out
c            each grid with fire has two lines
c            first line is related to plume rise
c            second line is related to emissions
               if (b_txtoutput) then 
                 write (iunit_o,'(I3, 1x, I3, 1x, 100(E12.5,1x))') i,j , 
     &                zplm , ztop , zbot , 
     &                (rfraction(iz),iz=1,nemislayers),frptot,
     &                burnedareatot
                 write (iunit_o,'(I3, 1x, I3, 1x, 100(E12.5,1x))') i,j , 
     &                (e_model(ispc),ispc=1,9+nvoc_emis+npm_emis+1)
               endif 
               
c           end of j loop
            enddo
c        end of i loop            
         enddo
c     end of hour loop         
      enddo ! hour

      close (iunit_o)
      end
 
      subroutine swap(R1,R2)
      implicit none
      real R1 , R2
      real temp
      temp = R1
      R1 = R2
      R2 = temp
      end
