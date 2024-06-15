module algos


contains



    function rtsafe(funcd,x1,x2,acc,info) 
        !
 
           implicit none
        !
           double precision :: rtsafe
           double precision, intent(in) :: x1
           double precision, intent(in) :: x2
           double precision, intent(in) :: acc
        !
           double precision :: df, dx, dxold, f, fh, fl, temp, xh, xl
        !
           integer, intent(out) :: info
           integer, parameter :: maxit = 300
           integer  :: j
        !
           interface
              subroutine funcd(x,f,df)

                double precision, intent(in) :: x
                double precision, intent(out) :: f
                double precision, intent(out) :: df
              end subroutine funcd
           end interface
        !
           info=0
        !
        !  -------------------------------------------------------------------
           call funcd(x1,fl,df)
           call funcd(x2,fh,df)
        !  -------------------------------------------------------------------
           print *, "fl is ", fl
           print *, "fh is ", fh

        !
        !    if((fl.gt.0d0 .and. fh.gt.0d0) .or. (fl.lt.0d0 .and. fh.lt.0d0)) then
        !       info=1
        !       return       ! 'root is not bracketed in rtsafe'
        !    else if(fl.eq.0d0)then
        !       rtsafe=x1
        !       return
        !    else if(fh.eq.0d0)then
        !       rtsafe=x2
        !       return
        !    else if(fl.lt.0d0)then
        !       xl=x1
        !       xh=x2
        !    else
        !       xh=x1
        !       xl=x2
        !    endif
           rtsafe=0.5*(x1+x2)
           dxold=abs(x2-x1)
           dx=dxold
        !  -------------------------------------------------------------------
           call funcd(rtsafe,f,df)
        !  -------------------------------------------------------------------
           do j=1,maxit
              if(((rtsafe-xh)*df-f)*((rtsafe-xl)*df-f).ge.0d0 .or.           &
                 abs(2*f).gt.abs(dxold*df) ) then
                 dxold=dx
                 dx=0.5*(xh-xl)
                 rtsafe=xl+dx
              else
                 dxold=dx
                 dx=f/df
                 temp=rtsafe
                 rtsafe=rtsafe-dx
              endif
              if(abs(dx).lt.acc) then
                 info=2  ! 'rtsafe is not able to find a root in the range'
                 return
              endif
        !     ----------------------------------------------------------------
              call funcd(rtsafe,f,df)
        !     ----------------------------------------------------------------
              if(abs(f).lt.acc) then
                 return
              else if(f.lt.0d0) then
                 xl=rtsafe
              else
                 xh=rtsafe
              endif
           enddo
           info=3 
           return  ! 'rtsafe exceeding maximum iterations'
        end function rtsafe

    subroutine rosenBrock(x, f, df)
            implicit none
            double precision, intent(in) :: x
            double precision, intent(out) :: f
            double precision, intent(out) :: df

            double precision :: alpha = 100
            double precision :: x2 = 10

        f = (1-x)**2 + alpha*(x2-x**2)**2    
        df = 2*(1-x)-4*alpha*x*(x2-x**2)

    end subroutine 

    subroutine tstNewtonRaphson()

        double precision :: result
        double precision :: f, df
        integer :: info

        result = rtsafe(rosenBrock, -100.0*1d0, 100.0*1d0, .001*1d0, info)

        print *, "Reslt is ", result
        call rosenBrock(result, f, df)
        print *, "Min Val is ", f
        print *, "Deriv is ", df
        call rosenBrock(1.0d0, f, df)
        print *, "Min Val is ", f
        print *, "Deriv is ", df    



        

    end subroutine

    function func_rosenBrock(x) result(f)
        implicit none
        double precision, intent(in) :: x
        double precision :: f
        !double precision, intent(out) :: df

        double precision :: alpha = 10
        double precision :: x2 = 1

    f = (1-x)**2 + alpha*(x2-x**2)**2    
    !df = 2*(1-x)-4*alpha*x*(x2-x**2)

    end function 

    subroutine tstBrent()

        double precision :: result
        double precision :: f, xmin

        result = brent(-100*1d0,100*1d0,0*1d0,func_rosenBrock, .000001*1d0, xmin)
  
        print *, "Reslt is ", result
        !call rosenBrock(result, f, df)
        print *, "Min Val is ", xmin
        print *, "Double Check Result is ", func_rosenBrock(xmin)
        !print *, "Deriv is ", df
        !call rosenBrock(1.0d0, f, df)
        !print *, "Min Val is ", f
        !print *, "Deriv is ", df    



        

    end subroutine


    FUNCTION brent(ax,bx,cx,func,tol,xmin) 
        double precision, INTENT(IN) :: ax,bx,cx,tol 
        double precision, INTENT(OUT) :: xmin
        double precision:: brent 
        INTERFACE
        FUNCTION func(x)
        IMPLICIT NONE
        double precision, INTENT(IN) :: x 
        double precision :: func
        END FUNCTION func
        END INTERFACE
        INTEGER, PARAMETER :: ITMAX=100
        double precision, PARAMETER :: CGOLD=0.3819660,ZEPS=1.0e-3*epsilon(ax)
        INTEGER:: iter
        double precision :: a,b,d,e,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm


        a=min(ax,cx) 
        b=max(ax,cx) 
        v=bx
        w=v
        x=v
        e=0.0 
        fx=func(x) 
        fv=fx
        fw=fx

        do iter=1,ITMAX
            xm=0.5*1d0*(a+b)
            tol1=tol*abs(x)+ZEPS
            tol2=2.0*1d0*tol1
            if (abs(x-xm) <= (tol2-0.5*1d0*(b-a))) then 
            xmin=x 
            brent=fx 
            RETURN
            end if
            if (abs(e) > tol1) then
                r=(x-w)*(fx-fv) 
                q=(x-v)*(fx-fw) 
                p=(x-v)*q-(x-w)*r 
                q=2.0*1d0*(q-r)
                if (q > 0.0) p=-p 
                q=abs(q)
                etemp=e
                e=d
                if (abs(p) >= abs(0.5*1d0*q*etemp) .or. & 
                & p <= q*(a-x) .or. p >= q*(b-x)) then    
                    e=merge(a-x,b-x, x >= xm )
                    d=CGOLD*e
                    else 
                    d=p/q
                    u=x+d
                    if (u-a < tol2 .or. b-u < tol2) d=sign(tol1,xm-x)
                    end if           
                Else
                    e=merge(a-x,b-x, x >= xm ) 
                    d=CGOLD*e
                end if 
                u=merge(x+d,x+sign(tol1,d), abs(d) >= tol1 ) 
                fu=func(u)       
                if (fu <= fx) then
                    if (u >= x) then
                        a=x 
                    else
                        b=x 
                    end if
                        call shft(v,w,x,u)
                        call shft(fv,fw,fx,fu)
                    else
                        if (u < x) then 
                            a=u
                        else 
                            b=u
                        end if              
                        if (fu <= fw .or. w == x) then
                            v=w
                            fv=fw
                            w=u
                            fw=fu    
                        else if (fu <= fv .or. v == x .or. v == w) then 
                            v=u
                            fv=fu 
                        end if
                    end if      
                end do
                CONTAINS
                SUBROUTINE shft(a,b,c,d) 
                    double precision, INTENT(OUT) :: a 
                    double precision, INTENT(INOUT) :: b,c 
                    double precision, INTENT(IN) :: d
                    a=b
                    b=c
                    c=d
                    END SUBROUTINE shft 
                END FUNCTION brent


end module