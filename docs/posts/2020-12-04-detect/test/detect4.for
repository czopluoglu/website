c Louis changed the confirmatory analysis option so that the clusters would
c  be written on the standard detect output file.  This was not previously
c  done because the thinking was that the user specified the clusters so
c  there was no need to include it in the detect output.  The thinking now is
c  that this is helpful because users may do more than one c.a. run, and may
c  lose track of which cluster specifications go with which analyses.  Also,
c  users need to check that they have correctly inputted their intended
c  clusters (or check that the software correctly read the clusters).
c
c Louis eliminated the "roussos iterations" option
c
c Louis increased the upper limit on the number of examinees to be 120,000
c
c Louis also changed IDN to be the proportion of ccov signs that are
c correctly placed, in terms of the withins being positive and the
c betweens being negative.
c
c         Changes made by HRK   23  Jul  2003
c
c          - cluster setting can be read in from the file
c            'cluster.in'
c          - items can be dropped for the analysis
c          - when requested, cluster output will be written
c            in the file 'cluster.out'
c          - when requested, the covariance matrix will be
c            written in the file 'cov.out'
c
c         ----------------------------------------------------
c
c         Last change:  SMH  12 Jun 2002   11:06 pm
c
c
c         ****************************************************
c         A version of detect that can handle up to 150 items.
c         ****************************************************          
c
c         last updated 2/12/02
c
c         This is the code that goes with the Visual Basic 
c         Front End.
c
c         Comments have been added to the code by Sarah and Amy.  
c   
c         Detect is the work of Hae-Rim Kim, Jinming Zhang,
c         and William Stout.
c
c         This code is the property of the Statistical
c         Laboratory for Educational and Psychological
c         Measurement at the University of Illinois in
c         Urbana-Champaign.
c
c
c
       program main

c      max # of items=150
c      max # of examinees=999999

       implicit real*4(a-h,o-z)
       parameter (mxe=999999,imx=150,mxd=12)
       real mnnex, avnex
       integer ix(mxe,imx),vmax(imx),ix0(999999,imx)
       integer nexam,origin,keep(imx)
       integer clm(mxd,imx),nic(mxd),irel(imx),drop(imx)
       integer p(mxe),op(mxe)
       integer dropflag,qcluster,count,kmax,kmaxt
       integer clusflag,covflag,cloutflg
       real ccov(imx,imx)
       real xjunk(mxe)
       character*1 chcc(imx,imx)
       character*120 infile,outfile
       integer itemlabl(imx),flag(imx,imx)
       integer clst2run,crosflag,ncros,ncldrop(imx)
       integer clusdrop(imx,imx)
       integer run,nitem,iseed0,rousflag,mmut
       integer temp(mxd,imx),jtem(imx)
       integer mincell,mxdu,ndrop

   12  format(a120)

c***********************************************************************
c       Read in information 
c***********************************************************************

c        write(*,361)
c  361   format(' Enter the name of input for response data:'
c     1         /' ')
c        read(*,12) infile
c
c 
c        write(*,362)
c  362   format(' Enter the number of items on the test:'
c     1         /'  ')
c        read(*,*) nitem
c
c        write(*,363)
c  363   format(' Enter the number of examinees taking' 
c     1         ' the test:'/'  ')
c        read(*,*) nexam
c     
c        write(*,364)
c  364   format(' Enter the minimum number of examinees'
c     1         ' for one cell:'/'  ')
c        read(*,*) mincell
c
c        write(*,365)
c  365   format(' Enter the number of vectors to be mutated.'
c     1         /' A fifth of the number of items recommended:' 
c     1         /'  ')
c        read(*,*) mmut
c
c        write(*,366)
c  366   format(' Enter the number of dimensions you want'
c     1         ' to run.'
c     1         /' Must be no more than 12:' 
c     1         /'  ')  
c        read(*,*) mxdu
c
c        write(*,368)
c  368   format(' Do you want to drop items from the analysis?'
c     1         /' Enter 1 for yes and 0 for no:'
c     1         /'  ')
c        read(*,*) dropflag
c        if (dropflag.eq.1) then
c           write(*,369)
c  369      format(' Enter the number of items to drop:'
c     1            /'  ')
c           read(*,*) ndrop
c           if (ndrop.ne.0) then
c             write(*,370)
c  370        format(' Enter the item numbers to drop:'
c     1            /'  ')
c             read(*,*) (drop(i),i=1,ndrop)
c           endif
c        endif
c 
c        write(*,371)
c  371   format(' Do you want to use your own item cluster setting'
c     1         /' in calculating DETECT value?'
c     1         /' Enter 1 for yes and 0 for no:'
c     1         /'  ')
c        read(*,*) clusflag
c
c        if (clusflag.eq.0) then
c        write(*,372)
c  372   format(' Do you want to do cross validation?'
c     1         /' Enter 1 for yes and 0 for no:'
c     1         /'  ') 
c        read(*,*) crosflag
c        if (crosflag.eq.1) then
c           write(*,373)
c  373      format(' Enter the number of examinees to set aside'
c     1            /' for cross validation:'
c     1            /'  ')
c           read(*,*) ncros
c        endif
c        endif
c
c        write(*,374)
c  374   format(' Enter an integer as the seed for random'
c     1         ' number generation:'
c     1         /'  ')
c        read(*,*) iseed0
c
c
c        write(*,375)
c  375   format(' Enter the name of detect summary output file:'
c     1         /'  ')
c        read(*,12) outfile
c       
c        write(*,376) 
c  376   format(' Do you want to have a separate cluster'
c     1         ' output file?'
c     1         /' Enter 1 for yes and 0 for no:'
c     1         /'  ')
c        read(*,*) cloutflg
c
c        write(*,378)
c  378   format(' Do you want to have a covariance output file?'
c     1         /' Enter 1 for yes and 0 for no:'
c     1         /'  ')
c        read(*,*) covflag
c
c        write(*,380)
c  380   format(' Do you want to do Roussos iteration?'
c     1         /' Enter 1 for yes and 0 for no:'
c     1         /'  ')
c        read(*,*) rousflag
c
c**********************************************************************
c       Read in information from input file
c       Input file is set to be "detect.in"
c**********************************************************************

        open(unit=1,file="detect.in")

        read(1,*)
        read(1,12) infile
        WRITE(*,*) ' * name of data file:  ',infile

        read(1,*)  
        read(1,*) nitem
        WRITE(*,*) ' * no. of items:  ',nitem

        read(1,*)
        read(1,*) nexam
        WRITE(*,*) ' * no. of examinees:  ',nexam

        read(1,*)  
        read(1,*) mincell
        WRITE(*,*) ' * mincell:  ',mincell

        read(1,*)
        read(1,*) mmut
        WRITE(*,*) ' * mutations:  ',mmut

        read(1,*)  
        read(1,*) mxdu
        WRITE(*,*) ' * max dimensions:  ',mxdu

        read(1,*)
        read(1,*) dropflag
        write(*,*)' * item drop flag:  ',dropflag

        if (dropflag.eq.0) then
           read(1,*)
           read(1,*)
           read(1,*)
           read(1,*)
        else if (dropflag.eq.1) then
           read(1,*)
           read(1,*) ndrop
           WRITE(*,*)' * no. of items to drop from the analysis:  '
     1                ,ndrop
           read(1,*)                          
           read(1,*) (drop(i),i=1,ndrop)
           write(*,*)' * items to be dropped:  ',(drop(i),i=1,ndrop) 
        endif

        read(1,*)
        read(1,*) clusflag
        WRITE(*,*) ' * cluster input flag:  ',clusflag

        read(1,*)
        read(1,*) crosflag
        WRITE(*,*) ' * cross validation flag:  ',crosflag
        if (crosflag.eq.1) then         
           read(1,*)
           read (1,*) ncros
           WRITE(*,*) ' * no. of examinees for cross validation:  '
     1                   ,ncros
        else 
           read(1,*)
           read(1,*)
        endif

        read(1,*)
        read(1,*) iseed0
        WRITE(*,*) ' * seed:  ',iseed0 

        read(1,*)
        read(1,12) outfile
        WRITE(*,*)' * name of detect summary output file:  ',outfile
	 
        read(1,*)
        read(1,*) cloutflg
        WRITE(*,*) ' * cluster out flag:  ',cloutflg

        read(1,*)
        read(1,*) covflag
        WRITE(*,*) ' * covariance out flag:  ',covflag
          	     
c        read(1,*)
c        read(1,*) rousflag
c        WRITE(*,*) ' * cluster splitting flag:  ',rousflag

        rousflag=0

        close(1)

c***********************************************************************
c      save original seed value for output
c***********************************************************************

        iseed = iseed0
        iseed0 = -abs(iseed)

c***********************************************************************
c      order dropped items
c***********************************************************************

        if (dropflag.eq.1) then
          do i=1,ndrop
            jtem(i)=drop(i)
          end do
          do 250 j=1,ndrop
            mmin=jtem(1)
            kk=1
            do 251 l=1,ndrop-j+1
              if(mmin.gt.jtem(l)) then
                mmin=jtem(l)
                kk=l
              endif
  251       continue
            drop(j)=mmin
            do 252 mm=kk,ndrop-1
              jtem(mm)=jtem(mm+1)
  252       continue
  250     continue
        endif

c***********************************************************************
c      assign qcluster value
c      qcluster = 1 confirmatory DETECT using user given cluster 
c                   setting read from file "cluster.in"
c      qcluster = 2 exploratory DETECT without cross validation
c      qcluster = 3 exploratory DETECT with cross validation 
c***********************************************************************

       if (clusflag.eq.1) qcluster = 1  
       if ((clusflag.eq.0).and.(crosflag.eq.0)) qcluster = 2
       if ((clusflag.eq.0).and.(crosflag.eq.1)) qcluster = 3

c***********************************************************************
c      read in examinee responses from infile
c***********************************************************************

       open(unit=20,file=infile)

       do 10 j=1,nexam
  100    format(150i1)
	   read(20,100) (ix0(j,n),n=1,nitem)
         do 360 i=1,nitem
           if(ix0(j,i).ne.1) ix0(j,i) = 0
  360    continue
   10  continue
    
       close(20)
      
c***********************************************************************
c      Loop for Roussos technique: repeat on clusters until
c      detect value is less than 0.3
c***********************************************************************

       clst2run=0
       run=0
       origin = 0

       open(unit=22,file=outfile)

       DO 9975 count=1,nitem
	
         nn=nitem
         jj=nexam
         mnnex=0.0
         avnex=0.0

         do j=1,nn
	   do i=1,jj
	     ix(i,j)=ix0(i,j)
	   end do
         end do
	
c***********************************************************************
c      set the labels for the items used in the computations
c***********************************************************************

         do 7 i=1,nn
           itemlabl(i)=i
    7    continue
	     
         if (dropflag.eq.1) then
           do 1234 i=1,ndrop
             do 1235 j=1,nn
               if (j.eq.drop(i)) then
                 do 1236 k=j+1-(i-1),nn
                   itemlabl(k-1)=itemlabl(k)
                   do 1237 ii=1,jj
                     ix(ii,k-1)=ix(ii,k)
 1237              continue
 1236            continue
               endif
 1235        continue
 1234      continue
           nn=nn-ndrop
         endif

c**********************************************************************
c      limit the number of vector mutations when the number
c      of items is either less than 17 or less than 11
c**********************************************************************

	 if (nn.lt.17) mmut=3
	 if (nn.lt.11) mmut=2

c**********************************************************************
c      read in the clusters from the cluster input file,
c      "cluster.in".
c      Do this only if running confirmatory DETECT
c**********************************************************************

	 if (qcluster.eq.1) then

           open (unit=30,file="cluster.in")
	    read(30,*) nclus 
	    do 501 i=1,nclus
	      read(30,*) nic(i)
	      read(30,*) (clm(i,j),j=1,nic(i))
  501       continue
           close(30)

c**********************************************************************
c        order read in items in ascending order in each cluster
c**********************************************************************

          do 96 i=1,nclus
           do 97 j=1,nic(i)
             temp(i,j)=clm(i,j)
   97      continue
   96     continue

          do 95 i=1,nclus
           do 93 j=1,nic(i)
             min=temp(i,1)
             k=1
             do 92 l=1,nic(i)-j+1
               if(min.gt.temp(i,l)) then
                 min=temp(i,l)
                 k=l
               endif
   92        continue
             clm(i,j)=min
             do 94 mm=k,nic(i)-1
               temp(i,mm)=temp(i,mm+1)
   94        continue
   93      continue
   95     continue

c**********************************************************************
c        rearrange items
c**********************************************************************

          do 550 i=1,nclus
           do 560 j=1,nic(i)
             do 570 k=1,nn
               if (clm(i,j).eq.itemlabl(k)) clm(i,j)=k 
  570        continue
  560      continue
  550     continue
 
          kmaxt=nclus

          do 2060 j=1,kmaxt
           do 2070 k=1,nic(j)
             flag(j,k) = 1
 2070      continue
 2060     continue
 
          do 2030 i=1,nn
           do 2040 j=1,kmaxt
             do 2050 k=1,nic(j)
               if (flag(j,k).eq.0) goto 2050
               if (clm(j,k).eq.i) then
                 vmax(i)=j
                 flag(j,k)=0 
               endif
 2050        continue
 2040      continue   
 2030     continue 

         endif

c**********************************************************************
c      calculate the conditional covariance matrix and
c      search for maximum detect clusters given the 
c      original clusters provided by the user       
c      CONFIRMATORY DETECT PROGRAM
c**********************************************************************

         if (qcluster.eq.1) then
       
           call mcondcov(ix,nn,jj,mincell,ccov,mnnex,avnex)       
	   call cv(dem,dcl,rmax,vmax,ccov,nn)
	 	      
c**********************************************************************
c      calculate the conditional covariance matrix and
c      search for maximum detect clusters 
c      do not use cross validation to obtain the detect
c      statistic value 
c      EXPLORATORY DETECT PROGRAM(W/O CROSS VALIDATION)
c**********************************************************************

         else if (qcluster.eq.2) then
             
           call mcondcov(ix,nn,jj,mincell,ccov,mnnex,avnex)
           ijunk=mxdu
           if(mxdu.gt.nn) ijunk=nn
           call uni(vmax,kmax,rmax,dem,dcl,duni,
     1	               ccov,mmut,nn,iseed,ijunk)
	  
c**********************************************************************
c      calculate the conditional covariance matrix and 
c      search for the maximum detect clusters.  
c      use cross validation to obtain the detect statistic value     
c      CROSS VALIDATION DETECT PROGRAM
c**********************************************************************

         else if (qcluster.eq.3) then

c**********************************************************************
c      sort examinees
c**********************************************************************

          junk=1
          xseed = real(iseed0)

          call NRABS(xseed,junk,jj,jj,p)

          do 2080 i=1,jj
             xjunk(i) = real(abs(p(i)))
 2080     continue

          call indexx(jj,xjunk,op)

c**********************************************************************
c      find MAX partition with 
c      first jj-ncros sorted examinees
c**********************************************************************

          do 1125 k=1,jj-ncros
            do 1135 n=1,nn
               ix(k,n) = ix(op(k),n)
 1135       continue
 1125     continue

          call mcondcov(ix,nn,jj-ncros,mincell,ccov,mnnex,avnex)

          ijunk=mxdu
          if(mxdu.gt.nn) ijunk=nn

          call uni(vmax,kmax,rmax,dem,dcl,duni,
     1	                ccov,mmut,nn,iseed,ijunk)
        

c**********************************************************************
c      calculate DETECT statistics with
c      last ncros sorted examinees
c**********************************************************************

          do 112 k=1,ncros
             do 113 n=1,nn
               ix(k,n)= ix(op(jj-ncros+k),n)
  113        continue
  112     continue

          call mcondcov(ix,nn,ncros,mincell,ccov,mnnex,avnex)       
          call cv(dem,dcl,rmax,vmax,ccov,nn)


       endif

c**********************************************************************
c      begin writing output to file 
c      origin will control the amount of output for the 
c      Roussos cluster splitting technique
c**********************************************************************

       if (qcluster.ne.1) call newrelab(vmax,nn,kmax,clm,nic,kmaxt)
       if (qcluster.eq.1) then
          kmax = nclus
          call newrelab(vmax,nn,kmax,clm,nic,kmaxt)
       endif


  554  format(55('-'))

       if (origin.eq.0) then
         write(22,554)
         write(22,*) '                 DETECT SUMMARY OUTPUT'
         write(22,554)
         write(22,*)

C**********************************************************************
c      write out basic file information
c**********************************************************************

  109    format('                    Data File Name: ',a120
     1        /,/,'              Number of Items used: '5x,i3
     1        /,/,'           Number of Items dropped: '5x,i3
     2        /,/,'               Number of Examinees: '3x,i5
     3        /,/,'                 Minimum Number of '
     4        /,  '                Examinees per Cell: '6x,i2
     4        /,/,'         Number of Vectors Mutated: '6x,i2
     5        /,/,'      Maximum Number of Dimensions: '6x,i2
     6        /,/,'                Randomization Seed: ',i8)

  111    format('                    Data File Name: ',a120
     1      /,/,'              Number of Items used: '5x,i3
     1      /,/,'           Number of Items dropped: '5x,i3
     2      /,/,'               Number of Examinees' 
     3      /,  '             Used for Maximization: '3x,i5
     4      /,/,'               Number of Examinees' 
     5      /,  '         Used for Cross Validation: '3x,i5
     6      /,/,'                 Minimum Number of '
     4      /,  '                Examinees per Cell: '5x,i3
     7      /,/,'         Number of Vectors Mutated: '6x,i2
     8      /,/,'      Maximum Number of Dimensions: '6x,i2
     9      /,/,'                Randomization Seed: ',i8)

         if (qcluster.ne.3) then 
              write(22,109)infile,nn,ndrop,jj,mincell,mmut,mxdu,
     1         abs(iseed0)
         else 
              write(22,111)infile,nn,ndrop,jj-ncros,ncros,mincell,
     &                mmut,mxdu,abs(iseed0)
         endif

   62  format(/,'   Minimum percentage of examinees'
     1        /,'         used after deleting cells'
     1        /,'     having less than',i3,' examinees:',f9.2
     1      /,/,'   Average percentage of examinees'
     1        /,'         used after deleting cells'
     1        /,'     having less than',i3,' examinees:',f9.2)

         write(22,62) mincell,100*mnnex,mincell,100*avnex

         write(22,*)
         write(22,554)
       endif

c**********************************************************************
c      write out detect clusters
c**********************************************************************
       
   19  format(/,' NUMBER OF DIMENSIONS THAT MAXIMIZE DETECT:',i3)

       if (qcluster.eq.1) then
          write(22,*)
          write(22,*) ' CONFIRMATORY ANALYSIS:'
       elseif (qcluster.eq.2) then
          write(22,19) kmaxt
          write(22,*)
          write(22,*) ' Exploratory DETECT Statistics:'      
       elseif (qcluster.eq.3) then
          write(22,19) kmaxt
          write(22,*)
          write(22,*) ' DETECT Statistics Based on Cross-Validation:'
       endif

   29  format(/,'              Maximum DETECT value:',f9.4
     1        /,'                   IDN index value:',f9.4
     2        /,'                           Ratio r:',f9.4)
   23  format(/,'               DETECT value:',f9.4
     1        /,'            IDN index value:',f9.4
     2        /,'                    Ratio r:',f9.4)
   28  format(/,'            Cross-validated DETECT'
     1        /,'        value (after maximization):',f9.4
     2        /,'                   IDN index value:',f9.4
     3        /,'                           Ratio r:',f9.4)

       dclnew = (1. + dcl)/2.
       if (qcluster.eq.1) write(22,23)dem,dclnew,rmax
       if (qcluster.eq.2) write(22,29)dem,dclnew,rmax
       if (qcluster.eq.3) write(22,28)dem,dclnew,rmax


   39  format(/,' PARTITION WITH MAXIMUM DETECT VALUE:',/)
  399  format(/,' PARTITION SPECIFIED FOR CONFIRMATORY ANALYSIS:',/)
   49  format(10(2x,i3))

       
       if((qcluster.ne.1).and.(dropflag.eq.0)) then
          write(22,39)
          write(22,49)(vmax(i),i=1,nn)
       endif
       if((qcluster.eq.1).and.(dropflag.eq.0)) then
          write(22,399)
          write(22,49)(vmax(i),i=1,nn)
       endif


   69  format(/,' CLUSTER MEMBERSHIPS:')
   79  format(/,3x,'-----------CLUSTER ',i2,'-------------'/) 
   89  format(10(2x,i3))
  
       write(22,69)

       do 200 k=1,kmaxt
         write(22,79) k
         write(22,89) (itemlabl(clm(k,it)),it=1,nic(k))
  200  continue

       write(22,*)
       write(22,*) '  ----------------------------------'


c***********************************************************************
c      write out the clusters to a separate file
c***********************************************************************

   90  format(i3,9(1x,i3))

       if (origin.eq.0) then    
       if (cloutflg.eq.1) then
       open(unit=26,file="cluster.out")
         write(26,90) kmaxt
         do 526 i=1,kmaxt
            write(26,*)nic(i)
            write(26,90) (itemlabl(clm(i,it)),it=1,nic(i))
  526    continue
       close(26)
       endif
       endif

c***********************************************************************
c      set up and write out the covariance sign matrix
c***********************************************************************

       if (origin.eq.0) then
         nrel=0
         do 400 k=1,kmaxt
           do 410 it=1,nic(k)
             nrel=nrel+1
             irel(nrel)=clm(k,it)
  410      continue
  400    continue

         do 450 it1=1,nrel
           do 460 it2=it1,nrel
             if(it2.eq.it1)then
               chcc(it1,it2)='d'
             else
               if(ccov(irel(it1),irel(it2)).lt.0.)then
                 chcc(it1,it2)='-'
               elseif(ccov(irel(it1),irel(it2)).gt.0.)then
                 chcc(it1,it2)='+'
               else
                 chcc(it1,it2)='0'
               endif
             endif
             chcc(it2,it1)=chcc(it1,it2)
  460      continue
  450    continue

         write(22,*)
         write(22,*)' Covariance Sign Pattern Matrix:'
         if(nrel.le.80)then
            do 500 it1=1,nrel
               write(22,179)(chcc(it1,it2),it2=1,nrel)
  179          format(3x,80a1)
  500       continue
         else
            do 510 it1=1,nrel
               write(22,179)(chcc(it1,it2),it2=1,80)
  510       continue
            write(22,*)'  '
              do 520 it1=1,nrel 
                write(22,179)(chcc(it1,it2),it2=81,nrel) 
  520         continue
          endif
       endif

  129  format(' No cross validation for this DETECT run')

       if (qcluster.eq.2) then
         write(22,*)
         write(22,129)
       endif

c***********************************************************************
c      write the covariance between clusters output to 
c      the file
c      This takes the place of the extended output option.
c***********************************************************************

       if (origin.eq.0) then
       if (covflag.eq.1) then

   99  format(4x,'---COVARIANCE BETWEEN CLUSTERS ',i2,
     1           ' AND ',i2,'---',/)
  125  format(20x,'------------------cluster',x,i3,x,
     1           'items------------------') 
  139  format(3x,9(3x,i5))
  149  format(3x,9(3x,i5))     
  159  format(i3,9(3x,f5.2))
  169  format('Note: all the elements of the below covariance ',
     1        'matrices'/
     2        6x,'have been multiplied by 100.'/)

        open(unit=24,file="cov.out")

        write(24,*)' '

        write(24,169)

        write(24,*)' '
        do 300 k1=1,kmaxt
           do 310 k2=k1,kmaxt
             write(24,99)k1,k2
             itmp=nic(k2)/9
             if(itmp.lt.1)then
               write(24,125)k2
               write(24,139)(itemlabl(clm(k2,it2)),it2=1,nic(k2))
               do 320 it1=1,nic(k1)
                 write(24,159)itemlabl(clm(k1,it1)),
     1               (ccov(clm(k1,it1),clm(k2,it2)),it2=1,nic(k2))
                 write(24,*)' '
  320          continue
             else
               do 311 itm=1,itmp
	         write(24,125)k2
	         write(24,149)(itemlabl(clm(k2,it2)),
     1                             it2=(itm-1)*9+1,itm*9)
	         do 340 it1=1,nic(k1)
                   write(24,159)itemlabl(clm(k1,it1)),
     1                  (ccov(clm(k1,it1),clm(k2,it2)),
     1                       it2=(itm-1)*9+1,itm*9)
                   write(24,*)' '
  340            continue
  311          continue
               if(nic(k2).gt.itmp*10)then
                 write(24,125)k2
                 write(24,149)(itemlabl(clm(k2,it2)),
     1                             it2=itmp*9+1,nic(k2))
                 do 350 it1=1,nic(k1) 
                   write(24,159)itemlabl(clm(k1,it1)),
     1                 (ccov(clm(k1,it1),clm(k2,it2)),
     1                       it2=itmp*9+1,nic(k2)) 
                   write(24,*)' ' 
  350            continue 
               endif
             endif
  310      continue
  300   continue


        close(24)

        endif
        endif

c************************************************************************
c      Begin Roussos cluster spitting technique
c************************************************************************

       if (rousflag.eq.0) EXIT

       if (qcluster.ne.1) then
         if (dem.ge.0.3) then
           if (kmaxt.gt.1) then
             do i=1,kmaxt
               if (nic(i).gt.2) then
                 clst2run=clst2run+1
                 ncldrop(clst2run)=0
                 do j=1,nitem
                   dropflag=1     
                   do jj=1,nic(i)
                     if (itemlabl(clm(i,jj)).eq.j)dropflag=0
                   end do
                   if (dropflag.eq.1) then 
                     ncldrop(clst2run)=ncldrop(clst2run)+1
                     clusdrop(clst2run,ncldrop(clst2run))=j
                   endif
                 end do
               endif
             end do
           else
             write(22,*)' Maximum Number of Dimensions is 1.'
             if (origin.eq.0) then
               write(22,*)' Roussos Cluster Splitting Technique
     1                    Not Used.'
             else
               write(22,*)' Roussos Cluster Splitting Technique
     1                    Complete.'
             endif
           endif
         else
           write(22,*)
           write(22,*)' Maximum DETECT value is less than 0.3.'
           if (origin.eq.0) then
             write(22,*)' Roussos Cluster Splitting Teachique Not Used.'
           else
             write(22,*)' Roussos Cluster Splitting Technique Complete.'
           endif
         endif
       else
         write(22,*)
         write(22,*)' Confirmatory DETECT Program does not include'
         write(22,*)' Roussos Cluster Splitting Technique.'
       endif
       
       run=run+1
       origin = 1

       if (run.gt.clst2run) EXIT
     
       dropflag=1	      
       ndrop=ncldrop(run)
       
       do j=1,ncldrop(run)
         drop(j)=clusdrop(run,j)
       end do
       drop(ncldrop(run)+1) = 0

       j=1
       k=1
       
       do 624 i=1,nitem
         if (i.ne.drop(j)) then
             keep(k) = i
             k=k+1
         else
             j=j+1
         endif
  624  continue

  555  format(' Roussos Cluster Splitting Technique: DETECT Run', i3)
  556  format(' Items:',/, 10(2x,i3)) 

       write(22,*)
       write(22,554)
       write(22,555) run
       write(22,554)
       write(22,556) (keep(i),i=1,k-1)
       write(22,554)

 9975  continue
       
       close(22)

c       open(unit=1,file = "comflag")
c         write(1,*) 'The program is complete.'
c       close(1)

       end


c*************************************************************************
c*************************************************************************
c
c      begin subroutines and functions
c
c*************************************************************************
c*************************************************************************

       subroutine mcondcov(ix,nn,jj,mincell,ccov,minex,avex)

c      maximum # of items: 150
c      maximum # of examinees: 999999
c
c      calculate ccov
c
c      inputs: ix,nn,jj,mincell
c      outputs: ccov,minex,avex

       implicit real(a-h,o-z)
       real minex,avex
       parameter(mxe=999999,mxl=11250,imx=150)
       integer ix(mxe,imx),kount(0:imx),kountt(0:imx),nc(mxe)
       integer i1(mxl),i2(mxl)
       integer isum1(0:imx),isum2(0:imx),icrsum(0:imx)
       integer isum1t(0:imx),isum2t(0:imx),icrsumt(0:imx)
       real ccovs(0:imx),ccovt(0:imx),ccov(imx,imx)

c      calculate number correct for each examinee

       do 20 j=1,jj
          nc(j)=0
          do 30 n=1,nn
	     nc(j)=nc(j)+ix(j,n)
   30     continue
   20  continue

c      generate the two vectors that tell which items
c      go with pair

       icnt=0
       do 320 j1=1,nn-1
          do 330 j2=j1+1,nn
	     icnt=icnt+1
	     i1(icnt)=j1
	     i2(icnt)=j2
  330     continue
  320  continue

c      calculate covariances for each pair of items
c      conditioning on the total score over the remaining
c      items excluding two items
c      also covariances conditioning on the total score
c      then take the average of the above two covariances

       minex = 1.
       avex = 0.
       do 510 l=1,icnt
          temp1=0.
          temp2=0.

          nnp=0
          nnpt=0

          do 520 kk=0,nn-2
	     kount(kk)=0
	     isum1(kk)=0
	     isum2(kk)=0
	     icrsum(kk)=0
  520     continue

          do 521 kk=0,nn
	     kountt(kk)=0
	     isum1t(kk)=0
	     isum2t(kk)=0
	     icrsumt(kk)=0
  521     continue

          icscor=0
          icscort=0

          do 530 k=1,jj
	     icscor=nc(k)-ix(k,i1(l))-ix(k,i2(l))
	     isum1(icscor)=isum1(icscor)+ix(k,i1(l))
	     isum2(icscor)=isum2(icscor)+ix(k,i2(l))
	     kount(icscor)=kount(icscor)+1
	     icrsum(icscor)=icrsum(icscor)+ix(k,i1(l))*ix(k,i2(l))
	     icscort = nc(k)
	     isum1t(icscort)=isum1t(icscort)+ix(k,i1(l))
	     isum2t(icscort)=isum2t(icscort)+ix(k,i2(l))
	     kountt(icscort)=kountt(icscort)+1
	     icrsumt(icscort)=icrsumt(icscort)+ix(k,i1(l))*ix(k,i2(l))
  530     continue
 
	  do 560 m=0,nn-2
	     ccovs(m)=0.
  560     continue

	  do 561 m=0,nn
	     ccovt(m)=0.
  561     continue

	  do 550 m=0,nn-2
	     if(kount(m).gt.0)then
               ccovs(m)=real(icrsum(m))-
     1             real(isum1(m)*isum2(m))/real(kount(m))
	     else
               ccovs(m)=0.
	     endif
  550     continue

	  do 551 m=0,nn
	     if(kountt(m).gt.0)then
               ccovt(m)=real(icrsumt(m))-
     1             real(isum1t(m)*isum2t(m))/real(kountt(m))
             else
               ccovt(m)=0.
             endif
  551     continue
 
	  do 430 m=0,nn-2
             if(kount(m).lt.mincell)then
                nnp=nnp+kount(m)
                goto 430
             else
                temp1=temp1+ccovs(m)
             endif
  430     continue

	  if(nnp.eq.jj) then 
	    temp1=0.
	  else
	    temp1=50.*temp1/real(jj-nnp)
	  endif 
 
	  do 431 m=0,nn
             if(kountt(m).lt.mincell)then
               nnpt=nnpt+kountt(m)
               goto 431
             else
               temp2=temp2+ccovt(m)
             endif
  431     continue

	  if(nnpt.eq.jj) then
             temp2=0.
	  else
             temp2=50.*temp2/real(jj-nnpt)
	  endif
 
	  ccov(i1(l),i2(l))=temp1+temp2
	  ccov(i2(l),i1(l))=temp1+temp2

         
          avex=avex+(2*jj-nnp-nnpt)/real(2.*icnt*jj)
          minex=min(minex,(jj-nnp)/real(jj),(jj-nnpt)/real(jj))
       

  510  continue

       do 610 j=1,nn
          ccov(j,j)=0.
  610  continue

       return
       end

c----------------------------------------------------------------------
       subroutine cv(der,dclr,rmax,vmax,ccov,nitem)

c      calculate DETECT, idn index, and a ratio r by
c      utilizing item memberships stored in vmax via
c      function det
c
c      functions called: sgn, det
c
c      inputs: vmax,ccov,nitem
c      outputs: der,dclr,rmax

       parameter(imx=150)
       integer vmax(imx)
       real ccov(imx,imx)
       real tmp(imx,imx)

       upc=0.

       do 10 i=1,nitem
          do 20 j=1,nitem
	     tmp(i,j)=sgn(ccov(i,j)) 
	     upc=upc+abs(ccov(i,j))
   20     continue
   10  continue

       upc=upc/(nitem*(nitem-1))

       der=det(vmax,ccov,nitem)
       dclr=det(vmax,tmp,nitem)

       rmax=der/upc

       return
       end

c----------------------------------------------------------------------
       subroutine uni(vmax,kmax,rmax,dem,dcl,duni,ccov,
     1            	                  mut,nitem,iseed,mxdu)

c      calculate DETECT using cluster formation
c      found by G.A.
c
c      subroutines called: hac, gen, moment, cv
c
c      inputs: ccov,mut,nitem,iseed,mxdu
c      outputs: vmax,kmax,rmax,dem,dcl,duni

       parameter(mxd=12,imx=150,nsc=3)
       parameter(nstop=100,tol=1.e-4)
       integer vmax(imx),kmax
       real ccov(imx,imx)
       integer part(imx,mxd),vint(imx,nsc),vfin(imx,nsc)
       real de(0:mxd),dval(nstop),djk(10)
       integer temp1(imx),temp2(imx)
       integer clm(mxd,imx),nic(mxd)
       
       upc=0.
       duni=0.

       do 10 it=1,nitem
          do 20 jt=1,nitem
	     upc=upc+abs(ccov(it,jt))
	     duni=duni+ccov(it,jt)
   20     continue
   10  continue

       upc=upc/(nitem*(nitem-1))
       duni=duni/(nitem*(nitem-1))
      
       de(0)=0.
       de(1)=duni

       do 30 i=1,nitem
          part(i,1)=1
   30  continue

       ijunk=mxdu
       if(mxdu.gt.nitem) ijunk=nitem

       do 100 k=2,ijunk
          call hac(vint,ccov,k,nitem)
          do 110 i=1,10
	     call gen(vfin,dmax,vint,ccov,mut,k,nitem,iseed)

	     do 120 j=1,nsc
                do 130 it=1,nitem
                  vint(it,j)=vfin(it,j)
  130           continue
  120        continue
           
	     dval(i)=dmax
  110     continue

          do 150 i=11,nstop
	     call gen(vfin,dmax,vint,ccov,mut,k,nitem,iseed)
	     do 160 j=1,nsc
                do 170 it=1,nitem
                   vint(it,j)=vfin(it,j)
  170           continue
  160        continue

	     do 180 it=1,nitem
                vmax(it)=vfin(it,nsc)
  180        continue
	     dval(i)=dmax

	     do 190 l=1,10
                djk(l)=dval(i-10+l)
  190        continue
	     call moment(djk,10,av,sd)
	     ttol=sd/abs(av) 
	     if(ttol.le.tol)then
	       ngk=i
	       goto 200
	     endif
  150     continue

  200     if(ngk.eq.0)ngk=nstop
          de(k)=dval(ngk)

          do 210 it=1,nitem
             part(it,k)=vmax(it)
  210     continue

	  if(max(de(k-1),de(k)).le.de(k-2))then
             kmax=k-2
             goto 300
	  endif

  100  continue

       if(de(mxdu-1).ge.de(mxdu))then
          kmax=mxdu-1
       else
          kmax=mxdu
       endif

  300  do 310 i=1,nitem
          vmax(i)=part(i,kmax)
          temp1(i)=part(i,kmax)
  310  continue


       call relabel(temp1,temp2,nitem,k,clm,nic,kt)

       do it=1,nitem
          vmax(it)=temp2(it)
       end do

       kmax=kt

       call cv(dem,dcl,rmax,vmax,ccov,nitem)

       return
       end

c-----------------------------------------------------------------------
       subroutine hac(b,pin,k,nj)

c      This version of hac is a subroutine that was written
c      for Jinming, using genetic algorithm.
c
c      subroutines called: parm, relabel
c      
c      It takes as input:
c      nj:  the number of objects to be clustered
c      pin: the input (nj)x(nj) proximity matrix
c      k:   the number of clusters
c
c      It spits out as output:
c      b:   a (nj)x(3) matrix containing the cluster membership 
c           of the nj objects for the k-cluster solutions 
c           corresponding to the complete link, UPGMA, and
c           WPGMA cluster analysis methods.

       real pin(150,150)
       integer b(150,3)
       parameter(mxd=12)

c      working variables

       real p(150,150)
       integer c(150,150),bvec(150),cvec(150)
       integer ic(150),ict(150),iden(150),n(150)
       integer clm(mxd,150),nic(mxd)

       p(nj,nj)=0.

       do 1000 ifunc=2,4

         pmax=0.

         do 100 i=1,nj-1
	     p(i,i)=0.
	     do 110 j=i+1,nj
                p(i,j)=-pin(i,j)
                p(j,i)=p(i,j)
                if(p(i,j).ge.pmax)pmax=p(i,j)
  110        continue
  100    continue

c      initial clusters

         do 20 i=1,nj
            c(i,1)=i
            n(i)=1
            ic(i)=0
            iden(i)=1
   20    continue

         do 30 lp1=2,nj-k+1 
            ll=lp1-1
 
c     at each level, search the proximity matrix for its min. value
 
	    pmin=pmax
 
	    do 40 i=1,nj-1
               if(iden(i).eq.0)goto 40
               do 50 j=i+1,nj
                  if(iden(j).eq.0)goto 50
                  if(p(i,j).lt.pmin)then
                    pmin=p(i,j)
                    istar=i
                    jstar=j
                  endif
   50          continue
   40       continue

c     join the clusters that correspond to min. p(i,j)

            iden(jstar)=0

            do 60 i=1,nj
               c(i,lp1)=c(i,ll)
               if(c(i,ll).eq.jstar)c(i,lp1)=istar
   60       continue

            if(lp1.eq.2)then
              ic(1)=istar
              ic(2)=jstar
              icmax=2
            else
              if(n(istar).eq.1.and.n(jstar).eq.1)then
                ic(icmax+1)=istar
                ic(icmax+2)=jstar
                icmax=icmax+2
              elseif(n(istar).eq.1.or.n(jstar).eq.1)then
                if(n(istar).gt.1)then
                  itstar=istar
                  nitstar=jstar
                else
                  itstar=jstar
                  nitstar=istar
                endif

                do 80 i=1,icmax
                   if(c(ic(i),ll).eq.itstar)icstar=i
   80           continue

                if(icstar.lt.icmax)then
                  do 90 i=icmax,icstar+1,-1
                     ic(i+1)=ic(i)
   90             continue
                  ic(icstar+1)=nitstar
                else
                  ic(icstar+1)=nitstar
                endif

                icmax=icmax+1

              else

               do 120 ici=1,icmax
                  if(c(ic(ici),ll).eq.istar.or.c(ic(ici),ll).eq.jstar)
     1                         goto 25
  120          continue
   25          icount=1
               do 130 icl=ici+1,ici+n(istar)+n(jstar)-1
                if(c(ic(icl),ll).eq.istar.or.c(ic(icl),ll)
     1                         .eq.jstar)then
                    icount=icount+1
                else
                    goto 45
                endif
  130          continue
   45          if(icount.lt.(n(istar)+n(jstar)))then
                igcount=1
                do 140 ice=icl+1,icmax
                 if(c(ic(ice),ll).ne.istar.and.c(ic(ice),ll).ne.jstar)
     1                          then
                   igcount=igcount+1
                 else
                   goto 35
                 endif
  140           continue
   35            if(c(ic(ice),ll).eq.istar)icstar=istar
                 if(c(ic(ice),ll).eq.jstar)icstar=jstar

                 do 150 icp=icl,icl+igcount-1
                     ict(icp+n(icstar))=ic(icp)
  150            continue

                 do 160 icr=ice,ice+n(icstar)-1
                     ic(icr-igcount)=ic(icr)
  160            continue

                 do 170 icp=icl,icl+igcount-1
                     ic(icp+n(icstar))=ict(icp+n(icstar))
  170            continue
                endif
              endif
            endif

c     recalculate the proximities for cluster istar

           do 70 i=1,nj
             if(iden(i).eq.0)goto 70
             if(i.eq.istar.or.i.eq.jstar)goto 70
             call parm(alfa1,alfa2,gamma,n,istar,jstar,ifunc)
             p(istar,i)=alfa1*p(istar,i)+alfa2*p(jstar,i)
     @               +gamma*abs(p(istar,i)-p(jstar,i))
             p(i,istar)=p(istar,i)
   70      continue
           n(istar)=n(istar)+n(jstar)

   30    continue

c     store the result for this value of ifunc in matrix b

	 do 1010 i=1,nj       
	    cvec(i)=c(i,nj-k+1)
 1010    continue

	 call relabel(cvec,bvec,nj,k,clm,nic,kt)

	 do 1020 i=1,nj
            b(i,ifunc-1)=bvec(i)
 1020    continue

 1000  continue

       return
       end

c----------------------------------------------------------------------
       subroutine parm(alfai,alfaj,gamma,n,istar,jstar,ifunc)

c      calculate parameters alfai and alfaj
c
c      inputs: n,istar,jstar,ifunc
c      outputs: a;fai,alfaj,gamma

       integer n(150)

       alfai=.5
       alfaj=.5
       gamma=0.

       if(ifunc.eq.2)then
	   gamma=.5
       elseif(ifunc.eq.3)then
	   alfai=real(n(istar))/(n(istar)+n(jstar))
	   alfaj=real(n(jstar))/(n(istar)+n(jstar))
       endif
 
       return
       end

c----------------------------------------------------------------------
       subroutine relabel(x,y,nitem,ndim,clm,nic,ndimt)

c      relabel vector x, making new y
c
c      inputs: x,nitem,ndim
c      outputs: y,clm,nic,ndimt

       parameter(mxd=12,imx=150)
       integer x(nitem),y(nitem),ndimt,ndim
       integer luni(mxd),clm(mxd,imx),nic(mxd)

       luni(1)=x(1)
       if(ndim.eq.1)goto 100

       do 10 k=2,ndim
	  luni(k)=0
   10  continue

       do 50 k=2,ndim
	  if(luni(k).ne.0)goto 100
	  do 60 i=2,nitem
	     do 70 kp=1,k-1
	       if(x(i).eq.luni(kp))goto 60
   70        continue
	     luni(k)=x(i)
	     goto 50
   60     continue
   50  continue

       do 80 k=1,ndim
	  nic(k)=0
   80  continue

  100  do 110 i=1,nitem
	  do 120 k=1,ndim
	     if(x(i).eq.luni(k))then
	       y(i)=k
	       nic(k)=nic(k)+1
	       clm(k,nic(k))=i
	       goto 110
	     endif
  120     continue
  110  continue

       ndimt=ndim

       do 200 k=2,ndim
	  if(luni(k).eq.0)then
	    ndimt=k-1
	    goto 300
	  endif
  200  continue

  300  return
       end

c----------------------------------------------------------------------- 
       subroutine gen(vfin,dmax,vint,ccov,mut,ndim,nitem,iseed)

c      find maximum detect and the accompanying cluster formation
c
c      subroutines called: sample, indexx
c      function called: det
c
c      inputs: vint,ccov,mut,ndim,nitem,iseed
c      outputs: vfin,dmax
 
       parameter(imx=150,mxp=500,nsc=3)
       integer vfin(imx,nsc),vint(imx,nsc)
       real ccov(imx,imx)
       integer genv(imx,mxp+nsc)
       integer id(imx),vvec(imx),idp(mxp+nsc)
       real dvec(mxp+nsc)

       np=0

       do 10 i=1,nsc
         call sample(id,nitem,mut,iseed)
         do 20 j=1,mut
           do 30 k=1,ndim
             if(vint(id(j),i).eq.k)goto 30
             np=np+1
             do 40 it=1,nitem
               genv(it,np)=vint(it,i)
   40        continue
             genv(id(j),np)=k
   30      continue
   20    continue
   10  continue

       do 100 j=1,np
         do 110 it=1,nitem
           vvec(it)=genv(it,j)
  110    continue
         dvec(j)=det(vvec,ccov,nitem)
  100  continue

       do 150 j=1,nsc
         do 160 it=1,nitem
           genv(it,np+j)=vint(it,j)
           vvec(it)=genv(it,np+j)
  160    continue
         dvec(np+j)=det(vvec,ccov,nitem)
  150  continue

       call indexx(np+nsc,dvec,idp)

       dmax=dvec(idp(np+nsc))

       do 200 j=1,nsc
         do 210 it=1,nitem
           vfin(it,j)=genv(it,idp(np+j))
  210    continue
  200  continue

       return
       end

c-----------------------------------------------------------------------
       subroutine sample(id,n,mut,iseed)

c      generate mut integers within (0,n+1) 
c
c      function called: ran1
c
c      inputs: n,mut,iseed
c      output: id

       integer id(mut),itmp(150)

       do 10 j=1,n
	   itmp(j)=1
   10  continue

       k=1
       do 20 while(k.le.mut)
	   iran=ran1(iseed)*n+1
	   if(itmp(iran).ne.0)then
	     id(k)=iran
	     itmp(iran)=0
	     k=k+1
	   endif
   20  continue

       return
       end

c-----------------------------------------------------------------------
       subroutine indexx(n,arr,indx)

c      index an array arr(1:n), i.e., outputs 
c      the array indx(1:n) such that arr(indx(j))
c      is in ascending order for j=1,2,...,n.
c      the input quantities n and arr are not changed.
c
c      inputs: n,arr
c      outputs: indx

       integer indx(n)
       real arr(n)
       parameter(m=7,nstack=50)
       integer istack(nstack)

       do 10 j=1,n
	   indx(j)=j
   10  continue

       jstack=0
       l=1
       ir=n

   70  if(ir-l.lt.m)then
	   do 20 j=l+1,ir
	     indxt=indx(j)
	     a=arr(indxt)
	     do 25 i=j-1,1,-1
	       if(arr(indx(i)).le.a)goto 30
	       indx(i+1)=indx(i)
   25      continue
	     i=0
   30      indx(i+1)=indxt
   20    continue
	   if(jstack.eq.0)return
	   ir=istack(jstack)
	   l=istack(jstack-1)
	   jstack=jstack-2
         else
	   k=(l+ir)/2
	   itemp=indx(k)
	   indx(k)=indx(l+1)
	   indx(l+1)=itemp
	   if(arr(indx(l+1)).gt.arr(indx(ir)))then
	     itemp=indx(l+1)
	     indx(l+1)=indx(ir)                 
	     indx(ir)=itemp
	   endif
	   if(arr(indx(l)).gt.arr(indx(ir)))then
	     itemp=indx(l)
	     indx(l)=indx(ir)
	     indx(ir)=itemp
	   endif
	   if(arr(indx(l+1)).gt.arr(indx(l)))then
	     itemp=indx(l+1)
	     indx(l+1)=indx(l)
	     indx(l)=itemp
	   endif
	   i=l+1
	   j=ir
	   indxt=indx(l)
	   a=arr(indxt)
   40   continue
	  i=i+1
	  if(arr(indx(i)).lt.a)goto 40
   50  continue
	 j=j-1
	 if(arr(indx(j)).gt.a)goto 50
	 if(j.lt.i)goto 60
	 itemp=indx(i)
	 indx(i)=indx(j)
	 indx(j)=itemp
	 goto 40
   60  indx(l)=indx(j)
	 indx(j)=indxt
	 jstack=jstack+2
	 if(jstack.gt.nstack)pause 'nstack too small in indexx'
	 if(ir-i+1.ge.j-1)then
	   istack(jstack)=ir
	   istack(jstack-1)=i
	   ir=j-1
	 else
	   istack(jstack)=j-1
	   istack(jstack-1)=l
	   l=i
	 endif
       endif
       goto 70  

       end               

c----------------------------------------------------------------------
       subroutine moment(z,n,av,sd)

c      calculate the 1st and 2nd moments of an array z
c
c      inputs: z,n
c      outputs: av,sd

       real z(n)

       av=0.
       do 10 i=1,n
	   av=av+z(i)
   10  continue

       av=av/n

       var=0.
       do 20 i=1,n
	   var=var+(z(i)-av)*(z(i)-av)
   20  continue

       sd=sqrt(var/(n-1))

       return
       end

c-----------------------------------------------------------------------
        subroutine NRABS(R,A,B,N,P)

c       generate N random integers U(A,B)
c
c       inputs: R,A,B,N
c       outputs: P

        parameter(mxe=999999)
        real R
        integer P(mxe)
        integer A,B,N

        S=B-A+1.0
        K=log(S-0.5)/log(2.0)+1
        L=1

        Do 1000 I=1,K
           L=2*L
 1000   continue

        K=1
        S=4.0*L
        I=1

 1020   IF ((I.LE.L).AND.(K.LE.N)) THEN
           R=R+R+R+R+R
           M=R/S
           R=R-M*S
           J=A+R/4.0
           IF (J.LE.B) THEN
              P(K)=J
              K=K+1
           END IF
              I=I+1
              GOTO 1020
        END IF

        return
        end

c-----------------------------------------------------------------------
       subroutine newrelab(x,nitem,ndim,clm,nic,ndimt)

c      provide items with new orders so that items 
c      gather according to clusters       
c
c      inputs: x, nitem, ndim, 
c      outputs: clm, nic, ndimt
       
       parameter(mxd=12,imx=150)
       integer x(imx),nitem,ndim,clm(mxd,imx),nic(mxd)
       integer ndimt
       
       ndimt=ndim
       
       if (ndim.eq.1) then
	   nic(1)=nitem
	   do 23 i=1,nitem
	     clm(1,i)=i
   23    continue
       endif
       
       if (ndim.ne.1) then
	   do 24 j=1,ndim
	     nic(j)=0
	   do 25 i=1,nitem
	     if (x(i).eq.j) then
	       nic(j)=nic(j)+1
	       clm(j,nic(j))=i
	     endif
   25      continue
   24    continue
       endif

c       if (nic(ndim).eq.0) ndimt=ndimt-1
c
c       k=1
c
c
c   30  do i=ndim-1,1,-1
c          if (nic(i).eq.0) then
c             ndimt=ndimt-1
c             if (nic(i+1).eq.0) go to 30
c             k=k+1
c             nic(k)=nic(k+1)
c             do j=1,nic(k)
c                clm(k,j)=clm(k+1,j)
c             end do
c
c          endif
c       end do
c
c       if (nic(ndim).eq.0) ndimt=ndimt-1

       return
       end           


c----------------------------------------------------------------------
c----------------------------------------------------------------------
       function sgn(x)

c      assign 1,0,-1 if the argument is positive, zero, or
c      negative, respectively

       real x,sgn

       if(x.gt.0.)then
	   sgn=1.
       elseif(x.lt.0.)then
	   sgn=-1.
       else
	   sgn=0.
       endif

       return
       end

c----------------------------------------------------------------------
       real function det(vvec,amat,len)

c      return a real value after adding or
c      subtracting covariances in amat 

       parameter(lmax=150)
       integer vvec(lmax)
       real amat(lmax,lmax)

       det=0.
       do 10 i=1,len
	   do 20 j=1,len
	    if(vvec(i).eq.vvec(j))then
	      det=det+amat(i,j)
	    else
	      det=det-amat(i,j)
	    endif
   20    continue
   10  continue

       det=det/(len*(len-1))

       return
       end

c----------------------------------------------------------------------
       real function ran1(idum)

c      genarate and return a random number from U[0,1]

c      input value idum used as a seed

       parameter(im1=2147483563,im2=2147483399,am=1./im1,imm1=im1-1,
     +    ia1=40014,ia2=40692,iq1=53668,iq2=52774,ir1=12211,ir2=3791,
     +    ntab=32,ndiv=1+imm1/ntab,eps=1.2e-7,rnmx=1.-eps)
       integer iv(ntab)
       save iv,iy,idum2
       data idum2/123456789/,iv/ntab*0/,iy/0/

       if(idum.le.0)then 
	  idum=max(-idum,1)
	  idum2=idum
	  do 10 j=ntab+8,1,-1
	    k=idum/iq1
	    idum=ia1*(idum-k*iq1)-k*ir1
	    if(idum.lt.0)idum=idum+im1
	    if(j.le.ntab)iv(j)=idum
   10     continue
	  iy=iv(1)
       endif

       k=idum/iq1
       idum=ia1*(idum-k*iq1)-k*ir1
       if(idum.lt.0)idum=idum+im1
       k=idum2/iq2
       idum2=ia2*(idum2-k*iq2)-k*ir2
       if(idum2.lt.0)idum2=idum2+im2
       j=1+iy/ndiv
       iy=iv(j)-idum2
       iv(j)=idum
       if(iy.lt.1)iy=iy+imm1
       ran1=min(am*iy,rnmx)

       return
       end
       
c----------------------------------------------------------------------
c     subroutines sort and dropitms are not called 
c----------------------------------------------------------------------

       subroutine sort(hmatr, hord, nn)
       
       parameter(imx=150)
       real hmatr(imx,imx),temp(imx,imx)
       integer hord(imx),hord1(imx),nn,xmin,index

       do 100 i=1,nn
          hord1(i) = hord(i)
  100  continue

       do 10030 i=1,nn-1
          xmin=hord(i)
          index=i
          do 10050 j=i+1,nn
             if (hord(j)<xmin) then
	        xmin=hord(j)
	        index=j
             endif
10050     continue

          swap=hord(i)
          hord(i)=hord(index)
          hord(index)=swap

          do 10060 k=1,nn
             temp(i,k)=hmatr(i,k)
             hmatr(i,k)=hmatr(index,k)
             hmatr(index,k)=temp(i,k)
10060     continue
10030  continue
	 
       do 10080 i=1,nn-1
          xmin=hord1(i)
          index=i
          do 20000 j=i+1,nn
	     if (hord1(j)<xmin) then
                xmin=hord1(j)
                index=j
	     endif
20000     continue
          swap=hord1(i)
          hord1(i)=hord1(index)
          hord1(index)=swap
          do 20010 k=1,nn
	     temp(k,i)=hmatr(k,i)
	     hmatr(k,i)=hmatr(k,index)
	     hmatr(k,index)=temp(k,i)
20010     continue
10080  continue
       
       return 
       end
       
c----------------------------------------------------------------------
        subroutine dropitms (matr,nn,drop,ndrop,itemlabl)
	
        parameter(imx=150)
        real matr(imx,imx)
        integer drop(imx), ndrop, nn,itemlabl(imx)
        integer newnn
	
        newnn=nn
	
        do 5555 i=1,ndrop
	   do 5544 j=1,nn
              if (drop(i).eq.itemlabl(j)) then
		 do 5543 jj=j+1,newnn
		    itemlabl(jj-1)=itemlabl(jj)
		    do 5542 ii=1,newnn
                       matr(jj-1,ii)=matr(jj,ii)
 5542               continue                  
		    do 5547 ii=1,newnn
                       matr(ii,jj-1)=matr(ii,jj)
 5547               continue  
 5543            continue
		 newnn=newnn-1
              endif
 5544      continue
 5555   continue

        nn=newnn
	
        return
        end
