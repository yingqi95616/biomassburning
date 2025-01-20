        program mergefire
! The purpose of this program is to merge the text file fire emission file
! with IO/API emission files.

      implicit none
 
c ioapi headers
      include 'PARMS3.EXT'      ! I/O parameters definitions
      include 'FDESC3.EXT'      ! file header data structure
      include 'IODECL3.EXT'     ! I/O definitions and declarations
 
c name of the program
      character*16 pname
      data pname/'fire_merge'/
      integer logunit
      character*16 sfenv, sfenvout
      character*256 sfile
      logical , external :: setenvvar

      integer iunit
      integer nspec
      integer, parameter :: maxspec=500
      integer, parameter :: maxlayer=100
      real rfrac(maxlayer)
      real remis(maxspec)
      real frp, area
      character*16 sname(maxspec),sunits(maxspec)
      integer imap(maxspec)
      character*1024 stemp
      real, allocatable :: rdata4d(:,:,:,:)
      real, allocatable :: rdata4dw(:,:,:,:)
      integer i,j,k,idate,itime,irec,ix,iy,iloc,iexit,ierr
      integer idateout, itimeout
      integer , external :: junit
      integer nvars3dout
      integer nfirelays
      real rtemp
      logical bfireonly
      integer n_layout


      logunit = init3()
c open fire emission file and read the header
      iunit = junit()
      read (*,'(a)') sfile
      open (unit=iunit, file=sfile, status='old')
      read (iunit,*) nspec
      do i=1, nspec
        read (iunit,'(A16,1x,A16)') sname(i),sunits(i)
        if (trim(sname(i)).eq.'13BDE') sname(i)='BDE13'
        if (trim(sname(i)).eq.'ACYE') sname(i)='ACY'
!        if (trim(sname(i)).eq.'ARO1') sname(i)='ARO1NBZ'
!        if (trim(sname(i)).eq.'ETHE') sname(i)='ETHENE'
!        if (trim(sname(i)).eq.'ISOP') sname(i)='ISOPRENE'
!        if (trim(sname(i)).eq.'PRD2') sname(i)='PROD2'
!        if (trim(sname(i)).eq.'TERP') sname(i)='TRP1'
      enddo
      read (iunit,*) nfirelays 
      read(iunit,'(a)') stemp
        
c read emis or layer definition file file 
      read (*,'(a)') sfile
      read (*,*) n_layout       

      print *,'nlayer=',n_layout
c flag to indicate include fire emission only
      read (*,*) bfireonly    

c number of layers in the output file (used only if generate fire
c emissions only)

      print * , 'reading...'//trim(sfile)
      sfenv = 'INFILE'
      if ( .not.setenvvar(sfenv,sfile) )
     &      call m3exit(pname,0,0,'FAIL TO SET INPUT FILE',-1)
      if ( .not.open3(sfenv,FSREAD3,pname) )
     &      call m3exit(pname,0,0,'FAIL TO OPEN INPUT FILE',-1)
c get file description
      if ( .not.desc3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO GET FILE DESC',-1)
      if (.not.bfireonly) then 
        allocate(rdata4d(ncols3d, nrows3d, nlays3d,nvars3d))
      else
        allocate(rdata4d(ncols3d, nrows3d, nlays3d,nspec))
      endif

c find species mapping
c if merging with an existing 3D emission file
      if (.not.bfireonly) then 
        do i=1, nspec
          call slocate(vname3d, nvars3d, sname(i), iloc)
          if (iloc.eq.0) then 
            print *,'species:',sname(i), 'not found'
            imap(i)=0
          else
            imap(i)=iloc
          endif
        enddo
      else
c otherwise create a new file
        do i=1, nspec
          vname3d(i)=adjustl(sname(i))
          units3d(i)=adjustl(sunits(i))
          vdesc3d(i)='emission of '//trim(sname(i))
          vtype3d(i)=M3REAL
          imap(i)=i
        enddo
        nvars3d=nspec
        if (n_layout.gt.0) then 
           nlays3d=n_layout
        endif
      endif

c save the input file hour
      idate=sdate3d
      itime=stime3d

c read the output file hour
      read (*,*) idateout
      read (*,*) itimeout
      sdate3d=idateout
      stime3d=itimeout
      ! add FRP and burned area 
      nvars3dout=nvars3d+2

      vname3d(nvars3d+1)='FRP'
      units3d(nvars3d+1)='MW'
      vdesc3d(nvars3d+1)='Fire Radiative Power'
      vtype3d(nvars3d+1)=M3REAL

      vname3d(nvars3d+2)='BURNAREA'
      units3d(nvars3d+2)='km2'
      vdesc3d(nvars3d+2)='burned area'
      vtype3d(nvars3d+2)=M3REAL

      nvars3d=nvars3d+2
      allocate(rdata4dw(ncols3d, nrows3d, nlays3d,nvars3d))

      read (*,'(a)') sfile
      print * , 'output file...'//trim(sfile)
      sfenvout = 'OUTFILE'
      if ( .not.setenvvar(sfenvout,sfile) )
     &      call m3exit(pname,0,0,'FAIL TO SET OUTPUT FILE',-1)
      if ( .not.open3(sfenvout,FSCREA3,pname) )
     &      call m3exit(pname,0,0,'FAIL TO OPEN INPUT FILE',-1)

      print *,'nlays3d=',nlays3d
c loop over all the hours
      do irec=1, mxrec3d
        print *,'rec=',irec
        if (.not. bfireonly) then 
          if ( .not.desc3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO GET FILE DESC',-1)
          if ( .not.read3(sfenv, 'ALL', -1, idate, itime, rdata4d)) 
     &        call m3exit(pname,0,0,'Fail to read emission data')
        else
           nvars3d=nspec
           if (n_layout.gt.0) then 
             nlays3d=n_layout
           endif
           rdata4d=0.0
        endif

!call readuntil(iunit,'Hour', stemp)
        iexit=0
        rdata4dw=0.0
        do while (iexit.eq.0)
          read(iunit,'(a)',iostat=ierr) stemp
          if (ierr.ne.0) then 
            iexit=1
            cycle
          endif
          if (index(stemp,'Hour').ne.0) then 
            iexit=1
            cycle
          endif
          read (stemp,*) ix,iy, rtemp, rtemp, rtemp, 
     &                   (rfrac(j),j=1,nfirelays),frp,area
! genfire.f generates emissions with more layers          
          rtemp=0.0
          if (nfirelays.gt.nlays3d) then 
             do j=nlays3d+1,nfirelays
               rfrac(nlays3d)=rfrac(nlays3d)+rfrac(j)
               rtemp=rtemp+rfrac(j)
             enddo
          endif
          if (rtemp.gt.0.1) then 
              print *,'warning...more than 10% above top layer'
     &             ,ix,iy,rtemp
          endif
          read (iunit,'(a)') stemp
          read (stemp,*) ix,iy, (remis(j),j=1,nspec)
          do j=1, nspec
            i=imap(j)
            if (i.eq.0) cycle
            do k=1, nlays3d
              rdata4dw(ix,iy,k,i)=rdata4d(ix,iy,k,i)+remis(j)*rfrac(k)
            enddo
            rdata4dw(ix,iy,1,nvars3d+1)=frp
            rdata4dw(ix,iy,1,nvars3d+2)=area
          enddo
        enddo

        if ( .not.desc3(sfenvout) )
     &      call m3exit(pname,0,0,'FAIL TO GET FILE DESC',-1)
        if ( .not.write3(sfenvout,ALLVAR3, idateout, itimeout, rdata4dw))
     &     call m3exit(pname, 0,0, 'Fail to write emission data')
        call nextime(idate,itime, tstep3d)
        call nextime(idateout,itimeout, tstep3d)
      enddo

      close(iunit)
      if ( .not.close3(sfenv) )
     &      call m3exit(pname,0,0,'FAIL TO CLOSE EMISSION FILE',-1)
      if ( .not.close3(sfenvout) )
     &      call m3exit(pname,0,0,'FAIL TO CLOSE EMISSION FILE',-1)
      end

      subroutine readuntil(iunit, starget, sout)
      implicit none
      integer iunit
      character*(*) starget
      character*(*) sout
      character*256 stemp
      integer ifound

      ifound=0
      rewind(iunit)
      do while (ifound.eq.0)
        read (iunit,'(a)') stemp
!        print *,starget,' ',trim(stemp)
        if (index(stemp,trim(starget)).ne.0) ifound=1
      enddo
      if (ifound.eq.0) stop 'target not found'
      end


