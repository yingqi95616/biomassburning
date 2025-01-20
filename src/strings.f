      INTEGER FUNCTION LENS(S1)
      IMPLICIT NONE
c******************************************************************************
c
c The purpose of this function is to return the actual length of a string,
c not counting the tailing spaces.
c
c Written By: Qi Ying (November 2004)
c             University of California, Davis
c******************************************************************************
      CHARACTER*(*) S1
      INTEGER i1
c get the total length of the string first
      i1 = LEN(S1)
c find the first non-space position
      DO WHILE ( S1(i1:i1).EQ.' ' )
         i1 = i1 - 1
      ENDDO
c return
      LENS = i1
      END

      SUBROUTINE SLOCATE(Slist,Ilist,Ssearch,Iloc)
      IMPLICIT NONE
      INTEGER i , Ilist , LENS
c******************************************************************************
c
c The purpose of this subroutine is to locate the postion of a string in a
c string array.
c
c Written by: Qi Ying (November 2004)
c             University of California, Davis
c
c******************************************************************************
      CHARACTER*(*) Slist(Ilist)
      CHARACTER*(*) Ssearch
      INTEGER Iloc
      INTEGER is1,is2
      is1 = LENS(Ssearch)
      Iloc = 0
      DO i = 1 , Ilist
         is2=lens(slist(i))
         IF ( Slist(i)(1:is2).eq.Ssearch(1:is1)) Iloc = i
      ENDDO
      END

      SUBROUTINE UPPERCASE(S1)
      INTEGER i , i1 , ioffset
c******************************************************************************
c
c The purpose of this subroutine is to convert lower letters in a string
c to upper letters.
c
c******************************************************************************
      CHARACTER*(*) S1
      i1 = LEN(S1)
      ioffset = ICHAR('a') - ICHAR('A')
      DO i = 1 , i1
         IF ( S1(i:i).GE.'a' .AND. S1(i:i).LE.'z' ) S1(i:i)
     &        = CHAR(ICHAR(S1(i:i))-ioffset)    
      ENDDO                                     
      END                                       

      subroutine allupper(slist,nlist)
c******************************************************************************
c
c The purpose of this subroutine is to convert the items in a string list
c to upper letters.
c
c Written: Qi Ying
c
c******************************************************************************
      character*(*) slist(nlist)
      do i=1,nlist
        call uppercase(slist(i))
      enddo
      end

      integer function ifirstc(stemp)
c******************************************************************************
c
c The purpose of this subroutine is to get the position of the first none
c zero position in a given string.
c
c Written by: Qi Ying 
c
c******************************************************************************
      character*(*) stemp
      i1=lens(stemp)
      if (i1.gt.0) then 
        ifirstc=1
        do i=1, i1
          if (stemp(i:i).ne.' ') then 
            ifirstc=i
            return
          endif
        enddo
      else
        ifirstc=0
        return
      endif
      end

