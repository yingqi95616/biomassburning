!        program testgroup
!        real rx(7),ry(7),rp(7)
!        integer npoints
!        integer,parameter ::  maxgrp = 10
!        integer ngroups
!        integer igrpid(maxgrp)
!
!        npoints=7
!        data rx /1,1,2,4,5,5,3/
!        data ry /1,2,2,3,3,4,6/
!        data rp /0.1, 0.1, 1.1, 1.1, 1.1, 1.1, 1.1/
!
!        call gridgroup(npoints, rx, ry, rp, maxgrp, ngroups, igrpid)
!        print *,ngroups
!        do i=1, npoints
!          print *,i,igrpid(i)
!        enddo
!        end 
        subroutine gridgroup(npoints,rx,ry,rp,maxgrp,ngroups,igrpid)
c-----------------------------------------------------------------------
c The purpose of this subroutine is to group points 
c into ngroups of connected points
c written by: Qi Ying (January 2024)
c             Texas A&M University
c inputs:
c       npoints:        number of points
c       rx, ry :        coordinates of the points
c       rp     :        radius of the points
c       maxgrp :        maximum number of groups
c outputs:
c       ngroups:        total number of groups
c       igrpid :        group id
c-----------------------------------------------------------------------
        implicit none
        integer npoints
        real rx(npoints), ry(npoints), rp(npoints)
        integer maxgrp
        integer ngroups
        integer igrpid(maxgrp)

        integer, parameter :: maxmember=10000
        integer np_current
        integer idx_current(maxmember) 
        
        integer i,j,k,istart,id
        logical run
        real rx1, rx2, ry1, ry2
        real dcut
        real r1,r2
        real d

        ngroups=0
        igrpid=0
        do i=1, npoints
          np_current=0
          idx_current=0

          ! find the first unassigned point
          run=.false.
          do j=i, npoints
            if (igrpid(j).eq.0) then
               istart=j
               run=.true.
               exit
            endif
          enddo
          if (.not.run) exit
          ngroups=ngroups+1 ! create a new group
          if (ngroups.gt.maxgrp) then 
             print *,'max number of connected groups exceeded'
             print *,'please modify maxgrp and recompile'
             stop
          endif
          !print *,'next unssigned point:',istart
          !print *,'group number:',ngroups
          ! this is the list to store points in the current group
          np_current=1
          idx_current(np_current)=istart  

          ! loop over the rest of the unssigned points
          run=.true.
          ! keep running until no points are added
          do while (run)
            run=.false.            
            do j=istart, npoints
              if (igrpid(j).ne.0) cycle
              rx1=rx(j)
              ry1=ry(j)
              r1=rp(j)
              do k=1, np_current
                id=idx_current(k)
                rx2=rx(id)
                ry2=ry(id)
                r2=rp(id)
                d=sqrt((rx1-rx2)**2+(ry1-ry2)**2)
                if (d.le.(r1+r2)) then ! the point is close to points in
                                       !the current group 
                   np_current=np_current+1
                   if (np_current.gt.maxmember) then 
                      print *,'np_current',np_current, 
     &                     'is greater than maxmember',maxmember
                      print *,'please modify code and recompile.'
                      stop
                   endif
                   idx_current(np_current)=j
                   igrpid(j)=ngroups
                   !print *,'point ',j,'belongs to group ',ngroups
                   run=.true.
                   exit
                endif
              enddo
            enddo
            !if (.not.run) then 
            !  print *,'no more points found for the current group.'
            !endif
          enddo
        enddo
        end
