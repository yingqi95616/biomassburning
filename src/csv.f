c      program main
c      CHARACTER *31 SDATA(100)
c      OPEN (UNIT =1, FILE='WEATHER', STATUS='OLD')
c      CALL READCVS1(1,SDATA,100,NDATA,IERR)
c      PRINT *, NDATA
c      DO I=1, NDATA
c        PRINT *,I,SDATA(I)
c      ENDDO
c      END


      SUBROUTINE GETNCOL(Iunit,Ncol,Ipos,Ierr)
      IMPLICIT NONE
      INTEGER i , LENS
c******************************************************************************
c
c The purpose of this subroutine is to get number of columns in a comma
c seperated cvs file.
c
c Written by: Qi Ying (November 2004)
c             University of California, Davis
c
c******************************************************************************
      INTEGER Iunit
      CHARACTER*256 Sfile
      INTEGER Ncol
      CHARACTER*2048 stemp
      INTEGER ilen
      INTEGER Ipos(*)

      INTEGER Ierr

      Ncol = 0
      READ (Iunit,'(A)',iostat=ierr) stemp
      if (ierr.ne.0) return
      ilen = len_trim(stemp)
      Ipos(1)=0
      DO i = 1 , ilen
         IF ( stemp(i:i).EQ.',' ) THEN
            Ncol = Ncol + 1
            Ipos(Ncol+1) = i
         ENDIF
      ENDDO
      Ncol = Ncol + 1
      Ipos(Ncol+1)=ilen+1

      BACKSPACE(Iunit)
      END

      subroutine readcvs1(iunit,sdata,maxdata,ndata,ierr)
c******************************************************************************
c
c The purpose of this program is to read a comma seperated text file, determine
c the number of columns in the data, and return each column in a string array.
c
c Written by: Qi Ying (April 2006)
c             PTSD, ARB
C
C SUBROUTINE REQUIRED: GETNCOL
c
c******************************************************************************
      integer idbg
      parameter (idbg=0)
c input:
      integer iunit, maxdata
c output:
      character*(*) sdata(maxdata)
      integer ndata, ierr

      character*1024 stemp
      integer ipos(1024)
      logical lop

      ierr=0

      inquire(iunit,OPENED=lop)
      if (.not.lop) then
        print *,'File not opened before calling readcvs1'
        stop
      endif

      call getncol(iunit,ndata,ipos,ierr)
      if (ierr.ne.0) return

      read (iunit,'(a)') stemp
      if (idbg.eq.1) print *,'line read:', stemp

      do i=1,ndata
        i1=ipos(i)+1
        i2=ipos(i+1)-1
        if (i2.ge.i1) then
          read (stemp(i1:i2),'(a)') sdata(i)
        else
          sdata(i)=' '
        endif
        if (idbg.eq.1) print *,i,':',sdata(i)
      enddo
      end

      subroutine writecvs1(iunit,sdata,ndata,ldblqt,ierr)
c******************************************************************************
c
c The purpose of this program is to write data to a comma seperated text file.
c
c Written by: Qi Ying (April 2006)
c             PTSD, ARB
c
c******************************************************************************
      integer iunit
      character*(*) sdata(ndata)
      integer ndata,ierr
      logical ldblqt
      
      logical lop
      character*1024 stemp
      character*1024 stemp1

      ierr=0
      inquire(iunit,OPENED=lop)
      if (.not.lop) then
        print *,'File not opened before calling readcvs1'
        stop
      endif
      stemp=''
 
      do i=1, ndata
        i1=len_trim(sdata(i))
        if (i1.ne.0) then 
          stemp1=sdata(i)(1:len_trim(sdata(i)))
          if (ldblqt) stemp1='\"'//sdata(i)(1:len_trim(sdata(i)))//'\"'
        else
          stemp1=' '
        endif
        if (i.ne.ndata) stemp1=stemp1(1:len_trim(stemp1))//','
        stemp=stemp(1:len_trim(stemp))//stemp1(1:len_trim(stemp1))
c        print *,i,stemp1
      enddo
      write (iunit,'(a)') stemp(1:len_trim(stemp))
      end

