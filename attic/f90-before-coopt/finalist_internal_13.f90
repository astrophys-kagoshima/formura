module finalist_internal_13
  use finalist_header
contains
  subroutine Formura_internal_57 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  250
             Rsc0(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_79_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  26
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_79_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(28), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  250
             Rsc0(ix+(2), iy+(28), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_79_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(252), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  26
          do ix = 1 ,  2
             Rsc0(ix+(252), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_79_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(252), iy+(28), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_80_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  250
             Rsc1(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_80_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  26
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_80_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(28), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_80_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  250
             Rsc1(ix+(2), iy+(28), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_80_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(252), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_80_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  26
          do ix = 1 ,  2
             Rsc1(ix+(252), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_80_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(252), iy+(28), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_80_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 1 ,  36 ,  1 

       do  iy = 3 ,  30 ,  1 

          do  ix = 3 ,  254 ,  1 

             a=Rsc0(ix+(-1), iy+(-1), iz+(1))
             a_0=Rsc1(ix+(-1), iy+(-1), iz+(1))

 Rsc3(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc0(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc0(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc0(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc0(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc0(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc0(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  28
          do ix = 1 ,  252
             Ridge_0_0_0_Om_81_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc3(ix+(2), iy+(2), iz+(34))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  28
          do ix = 1 ,  252
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_81_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc3(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_57

  subroutine Formura_internal_58 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 1 ,  36 ,  1 

       do  iy = 3 ,  30 ,  1 

          do  ix = 3 ,  254 ,  1 

             a=Rsc1(ix+(-1), iy+(-1), iz+(1))

 Rsc2(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc0(ix+(-1), iy+(-1), iz+(1)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc1(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc1(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc1(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc1(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc1(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc1(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  28
          do ix = 1 ,  252
             Ridge_0_0_0_Om_82_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc2(ix+(2), iy+(2), iz+(34))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  28
          do ix = 1 ,  252
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_82_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc2(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_58

  subroutine Formura_internal_59 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_81_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  252
             Rsc3(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_81_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  28
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_81_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(30), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_81_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  252
             Rsc3(ix+(2), iy+(30), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_81_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(254), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_81_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  28
          do ix = 1 ,  2
             Rsc3(ix+(254), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_81_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(254), iy+(30), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_81_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_82_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  252
             Rsc2(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_82_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  28
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_82_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(30), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_82_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  252
             Rsc2(ix+(2), iy+(30), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_82_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(254), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_82_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  28
          do ix = 1 ,  2
             Rsc2(ix+(254), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_82_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  36
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(254), iy+(30), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_82_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 1 ,  34 ,  1 

       do  iy = 3 ,  32 ,  1 

          do  ix = 3 ,  256 ,  1 

             a=Rsc3(ix+(-1), iy+(-1), iz+(1))
             a_0=Rsc2(ix+(-1), iy+(-1), iz+(1))

 Rsc1(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc3(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc3(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc3(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc3(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc3(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc3(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  30
          do ix = 1 ,  254
             Ridge_0_0_0_Om_83_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc1(ix+(2), iy+(2), iz+(32))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  30
          do ix = 1 ,  254
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_83_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc1(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_59

  subroutine Formura_internal_60 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 1 ,  34 ,  1 

       do  iy = 3 ,  32 ,  1 

          do  ix = 3 ,  256 ,  1 

             a=Rsc2(ix+(-1), iy+(-1), iz+(1))

 Rsc0(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc3(ix+(-1), iy+(-1), iz+(1)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc2(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc2(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc2(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc2(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc2(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc2(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  30
          do ix = 1 ,  254
             Ridge_0_0_0_Om_84_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(2), iy+(2), iz+(32))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  30
          do ix = 1 ,  254
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_84_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_60

  subroutine Formura_internal_61 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_83_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  254
             Rsc1(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_83_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  30
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_83_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(32), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_83_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  254
             Rsc1(ix+(2), iy+(32), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_83_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(256), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_83_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  30
          do ix = 1 ,  2
             Rsc1(ix+(256), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_83_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(256), iy+(32), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_83_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_84_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  254
             Rsc0(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_84_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  30
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_84_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(32), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_84_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  254
             Rsc0(ix+(2), iy+(32), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_84_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(256), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_84_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  30
          do ix = 1 ,  2
             Rsc0(ix+(256), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_84_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  34
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(256), iy+(32), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_84_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 1 ,  32 ,  1 

       do  iy = 3 ,  34 ,  1 

          do  ix = 3 ,  258 ,  1 

             a=Rsc1(ix+(-1), iy+(-1), iz+(1))
             a_0=Rsc0(ix+(-1), iy+(-1), iz+(1))

 Rsc2(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc1(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc1(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc1(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc1(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc1(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc1(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  32
          do ix = 1 ,  256
             Ridge_0_0_0_Om_85_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc2(ix+(2), iy+(2), iz+(30))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  32
          do ix = 1 ,  256
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_85_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc2(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_61

  subroutine Formura_internal_62 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 1 ,  32 ,  1 

       do  iy = 3 ,  34 ,  1 

          do  ix = 3 ,  258 ,  1 

             a=Rsc0(ix+(-1), iy+(-1), iz+(1))

 Rsc3(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc1(ix+(-1), iy+(-1), iz+(1)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc0(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc0(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc0(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc0(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc0(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc0(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  32
          do ix = 1 ,  256
             Ridge_0_0_0_Om_86_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc3(ix+(2), iy+(2), iz+(30))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  32
          do ix = 1 ,  256
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_86_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc3(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_62

  subroutine Formura_internal_63 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_85_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  256
             Rsc2(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_85_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  32
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_85_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(34), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_85_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  256
             Rsc2(ix+(2), iy+(34), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_85_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(258), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_85_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  32
          do ix = 1 ,  2
             Rsc2(ix+(258), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_85_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(258), iy+(34), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_85_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_86_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  256
             Rsc3(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_86_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  32
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_86_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(34), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_86_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  256
             Rsc3(ix+(2), iy+(34), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_86_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(258), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_86_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  32
          do ix = 1 ,  2
             Rsc3(ix+(258), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_86_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  32
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(258), iy+(34), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_86_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 1 ,  30 ,  1 

       do  iy = 3 ,  36 ,  1 

          do  ix = 3 ,  260 ,  1 

             a=Rsc2(ix+(-1), iy+(-1), iz+(1))
             a_0=Rsc3(ix+(-1), iy+(-1), iz+(1))

 Rsc0(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc2(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc2(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc2(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc2(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc2(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc2(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  34
          do ix = 1 ,  258
             Ridge_0_0_0_Om_87_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(2), iy+(2), iz+(28))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  34
          do ix = 1 ,  258
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_87_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_63

  subroutine Formura_internal_64 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 1 ,  30 ,  1 

       do  iy = 3 ,  36 ,  1 

          do  ix = 3 ,  260 ,  1 

             a=Rsc3(ix+(-1), iy+(-1), iz+(1))

 Rsc1(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc2(ix+(-1), iy+(-1), iz+(1)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc3(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc3(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc3(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc3(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc3(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc3(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  34
          do ix = 1 ,  258
             Ridge_0_0_0_Om_88_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc1(ix+(2), iy+(2), iz+(28))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  34
          do ix = 1 ,  258
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_88_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc1(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_64

  subroutine Formura_internal_65 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_87_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  258
             Rsc0(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_87_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  34
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_87_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(36), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_87_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  258
             Rsc0(ix+(2), iy+(36), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_87_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(260), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_87_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  34
          do ix = 1 ,  2
             Rsc0(ix+(260), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_87_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(260), iy+(36), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_87_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_88_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  258
             Rsc1(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_88_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  34
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_88_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(36), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_88_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  258
             Rsc1(ix+(2), iy+(36), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_88_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(260), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_88_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  34
          do ix = 1 ,  2
             Rsc1(ix+(260), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_88_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(260), iy+(36), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_88_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 1 ,  28 ,  1 

       do  iy = 3 ,  38 ,  1 

          do  ix = 3 ,  262 ,  1 

             a=Rsc0(ix+(-1), iy+(-1), iz+(1))
             a_0=Rsc1(ix+(-1), iy+(-1), iz+(1))

 Rsc3(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc0(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc0(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc0(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc0(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc0(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc0(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Ridge_0_0_0_Om_89_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc3(ix+(2), iy+(2), iz+(26))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_89_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc3(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_65

  subroutine Formura_internal_66 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 1 ,  28 ,  1 

       do  iy = 3 ,  38 ,  1 

          do  ix = 3 ,  262 ,  1 

             a=Rsc1(ix+(-1), iy+(-1), iz+(1))

 Rsc2(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc0(ix+(-1), iy+(-1), iz+(1)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc1(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc1(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc1(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc1(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc1(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc1(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Ridge_0_0_0_Om_90_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc2(ix+(2), iy+(2), iz+(26))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_90_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc2(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_66

  subroutine Formura_internal_67 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_89_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Rsc3(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_89_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_89_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(0), iy+(38), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_89_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Rsc3(ix+(2), iy+(38), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_89_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(262), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_89_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Rsc3(ix+(262), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_89_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc3(ix+(262), iy+(38), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_89_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_90_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Rsc2(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_90_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_90_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(0), iy+(38), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_90_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Rsc2(ix+(2), iy+(38), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_90_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(262), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_90_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Rsc2(ix+(262), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_90_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc2(ix+(262), iy+(38), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_90_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 1 ,  26 ,  1 

       do  iy = 3 ,  40 ,  1 

          do  ix = 3 ,  264 ,  1 

             a=Rsc3(ix+(-1), iy+(-1), iz+(1))
             a_0=Rsc2(ix+(-1), iy+(-1), iz+(1))

 Rsc1(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc3(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc3(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc3(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc3(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc3(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc3(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Ridge_0_0_0_Om_91_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc1(ix+(2), iy+(2), iz+(24))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_91_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc1(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_67

  subroutine Formura_internal_68 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 1 ,  26 ,  1 

       do  iy = 3 ,  40 ,  1 

          do  ix = 3 ,  264 ,  1 

             a=Rsc2(ix+(-1), iy+(-1), iz+(1))

 Rsc0(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc3(ix+(-1), iy+(-1), iz+(1)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc2(ix+(0), iy+(-1), iz+(1)) - a ) - ( a - &
Rsc2(ix+(-2), iy+(-1), iz+(1)) ) ) + ( ( Rsc2(ix+(-1), iy+(0), iz+(1)) - a ) - ( a - Rsc2(ix+(-1), iy+(-2), iz+(1)) ) ) ) + ( ( Rsc2(ix+(-1), iy+(-1), iz+(2)) - a ) - ( a - Rsc2(ix+(-1), iy+(-1), iz+(0)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Ridge_0_0_0_Om_92_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(2), iy+(2), iz+(24))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Facet_0_0_1_src__1_1_0_dest__1_1_1_Send%Ridge_0_0_1_Om_92_r__1_1_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(2), iy+(2), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_68

  subroutine Formura_internal_69 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_91_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Rsc1(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_91_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_91_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(0), iy+(40), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_91_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Rsc1(ix+(2), iy+(40), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_91_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(264), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_91_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Rsc1(ix+(264), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_91_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc1(ix+(264), iy+(40), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_91_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_92_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Rsc0(ix+(2), iy+(0), iz+(0))=Ridge_0_0_0_Om_92_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(2), iz+(0))=Ridge_0_0_0_Om_92_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(0), iy+(40), iz+(0))=Facet_0_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_92_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Rsc0(ix+(2), iy+(40), iz+(0))=Facet_0_1_0_src__1_0_0_dest__1_1_0_Recv%Ridge_0_1_0_Om_92_r__1_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(264), iy+(0), iz+(0))=Facet_1_0_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_92_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Rsc0(ix+(264), iy+(2), iz+(0))=Facet_1_0_0_src__0_1_0_dest__1_1_0_Recv%Ridge_1_0_0_Om_92_r__0_1_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Rsc0(ix+(264), iy+(40), iz+(0))=Facet_1_1_0_src__0_0_0_dest__1_1_0_Recv%Ridge_1_1_0_Om_92_r__0_0_0_r__1_1_0(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 11 ,  34 ,  1 

       do  iy = 35 ,  74 ,  1 

          do  ix = 259 ,  522 ,  1 

             a=Rsc1(ix+(-257), iy+(-33), iz+(-9))
             a_0=Rsc0(ix+(-257), iy+(-33), iz+(-9))

 U(ix+(-8)+(0), iy+(-8)+(0), iz+(-8)+(0))=( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc1(ix+(-256), iy+(-33), iz+(-9)) - a ) &
- ( a - Rsc1(ix+(-258), iy+(-33), iz+(-9)) ) ) + ( ( Rsc1(ix+(-257), iy+(-32), iz+(-9)) - a ) - ( a - Rsc1(ix+(-257), iy+(-34), iz+(-9)) ) ) ) + ( ( Rsc1(ix+(-257), iy+(-33), iz+(-8)) - a ) - ( a - Rsc1(ix+(-257), iy+(-33), iz+(-10)) ) ) ) ) ) ) )

          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_69

  subroutine Formura_internal_7 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 1 ,  38 ,  1 

       do  iy = 1 ,  38 ,  1 

          do  ix = 1 ,  262 ,  1 

             a=U(ix+(3), iy+(3), iz+(3))
             a_0=V(ix+(3), iy+(3), iz+(3))

 Rsc0(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( U(ix+(4), iy+(3), iz+(3)) - a ) - ( a - U(ix+(2), &
iy+(3), iz+(3)) ) ) + ( ( U(ix+(3), iy+(4), iz+(3)) - a ) - ( a - U(ix+(3), iy+(2), iz+(3)) ) ) ) + ( ( U(ix+(3), iy+(3), iz+(4)) - a ) - ( a - U(ix+(3), iy+(3), iz+(2)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  38
          do ix = 1 ,  2
             Ridge_0_0_0_Om_79_r__0_0_0_r__1_0_0(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  262
             Ridge_0_0_0_Om_79_r__0_0_0_r__0_1_0(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Ridge_0_0_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)=Rsc0(ix+(260), iy+(36), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Ridge_0_0_0_Om_79_r__0_0_0_r__0_0_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  2
             Ridge_0_0_0_Om_79_r__0_0_0_r__1_0_1(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  262
             Ridge_0_0_0_Om_79_r__0_0_0_r__0_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Ridge_0_0_0_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(260), iy+(36), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Facet_0_0_1_src__0_0_0_dest__0_0_1_Send%Ridge_0_0_1_Om_79_r__0_0_0_r__0_0_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  2
             Facet_0_0_1_src__0_0_0_dest__1_0_1_Send%Ridge_0_0_1_Om_79_r__0_0_0_r__1_0_1(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  262
             Facet_0_0_1_src__0_0_0_dest__0_1_1_Send%Ridge_0_0_1_Om_79_r__0_0_0_r__0_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_0_1_src__0_0_0_dest__1_1_1_Send%Ridge_0_0_1_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(260), iy+(36), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  262
             Facet_0_1_0_src__0_0_0_dest__0_1_0_Send%Ridge_0_1_0_Om_79_r__0_0_0_r__0_1_0(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_0_src__0_0_0_dest__1_1_0_Send%Ridge_0_1_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  262
             Facet_0_1_0_src__0_0_0_dest__0_1_1_Send%Ridge_0_1_0_Om_79_r__0_0_0_r__0_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_0_src__0_0_0_dest__1_1_1_Send%Ridge_0_1_0_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  262
             Facet_0_1_1_src__0_0_0_dest__0_1_1_Send%Ridge_0_1_1_Om_79_r__0_0_0_r__0_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_1_src__0_0_0_dest__1_1_1_Send%Ridge_0_1_1_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  38
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_0_dest__1_0_0_Send%Ridge_1_0_0_Om_79_r__0_0_0_r__1_0_0(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_0_dest__1_1_0_Send%Ridge_1_0_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_0_dest__1_0_1_Send%Ridge_1_0_0_Om_79_r__0_0_0_r__1_0_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_0_dest__1_1_1_Send%Ridge_1_0_0_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  2
             Facet_1_0_1_src__0_0_0_dest__1_0_1_Send%Ridge_1_0_1_Om_79_r__0_0_0_r__1_0_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_1_src__0_0_0_dest__1_1_1_Send%Ridge_1_0_1_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  38
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_0_src__0_0_0_dest__1_1_0_Send%Ridge_1_1_0_Om_79_r__0_0_0_r__1_1_0(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_0_src__0_0_0_dest__1_1_1_Send%Ridge_1_1_0_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(36))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_1_src__0_0_0_dest__1_1_1_Send%Ridge_1_1_1_Om_79_r__0_0_0_r__1_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(0))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_7

  subroutine Formura_internal_70 ()
    double precision  :: a
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 11 ,  34 ,  1 

       do  iy = 35 ,  74 ,  1 

          do  ix = 259 ,  522 ,  1 

             a=Rsc0(ix+(-257), iy+(-33), iz+(-9))

 V(ix+(-8)+(0), iy+(-8)+(0), iz+(-8)+(0))=( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc1(ix+(-257), iy+(-33), iz+(-9)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc0(ix+(-256), iy+(-33), iz+(-9)) &
- a ) - ( a - Rsc0(ix+(-258), iy+(-33), iz+(-9)) ) ) + ( ( Rsc0(ix+(-257), iy+(-32), iz+(-9)) - a ) - ( a - Rsc0(ix+(-257), iy+(-34), iz+(-9)) ) ) ) + ( ( Rsc0(ix+(-257), iy+(-33), iz+(-8)) - a ) - ( a - Rsc0(ix+(-257), iy+(-33), iz+(-10)) ) ) ) ) &
) ) )

          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_70

  subroutine Formura_internal_71 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  64
          do ix = 1 ,  512
             U(ix+(2), iy+(2), iz+(66))=Facet_0_0_1_src__0_0_0_dest__0_0_1_Recv%Ridge_0_0_1_St_U(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  64
          do ix = 1 ,  512
             V(ix+(2), iy+(2), iz+(66))=Facet_0_0_1_src__0_0_0_dest__0_0_1_Recv%Ridge_0_0_1_St_V(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 3 ,  28 ,  1 

       do  iy = 1 ,  38 ,  1 

          do  ix = 1 ,  262 ,  1 

             a=U(ix+(3), iy+(3), iz+(39))
             a_0=V(ix+(3), iy+(3), iz+(39))

 Rsc0(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( U(ix+(4), iy+(3), iz+(39)) - a ) - ( a - U(ix+(2), &
iy+(3), iz+(39)) ) ) + ( ( U(ix+(3), iy+(4), iz+(39)) - a ) - ( a - U(ix+(3), iy+(2), iz+(39)) ) ) ) + ( ( U(ix+(3), iy+(3), iz+(40)) - a ) - ( a - U(ix+(3), iy+(3), iz+(38)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Ridge_0_0_0_Om_79_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Ridge_0_0_0_Om_79_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Ridge_0_0_0_Om_79_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc0(ix+(260), iy+(36), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Facet_0_1_0_src__0_0_1_dest__0_1_1_Send%Ridge_0_1_0_Om_79_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_0_1_0_Om_79_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc0(ix+(260), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_0_1_Send%Ridge_1_0_0_Om_79_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_0_0_Om_79_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(36), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_1_0_Om_79_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc0(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_71

  subroutine Formura_internal_72 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 3 ,  28 ,  1 

       do  iy = 1 ,  38 ,  1 

          do  ix = 1 ,  262 ,  1 

             a=V(ix+(3), iy+(3), iz+(39))

 Rsc1(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * U(ix+(3), iy+(3), iz+(39)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( V(ix+(4), iy+(3), iz+(39)) - a ) - ( a - V(ix+(2), &
iy+(3), iz+(39)) ) ) + ( ( V(ix+(3), iy+(4), iz+(39)) - a ) - ( a - V(ix+(3), iy+(2), iz+(39)) ) ) ) + ( ( V(ix+(3), iy+(3), iz+(40)) - a ) - ( a - V(ix+(3), iy+(3), iz+(38)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Ridge_0_0_0_Om_80_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc1(ix+(260), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Ridge_0_0_0_Om_80_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(36), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Ridge_0_0_0_Om_80_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(260), iy+(36), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  262
             Facet_0_1_0_src__0_0_1_dest__0_1_1_Send%Ridge_0_1_0_Om_80_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_0_1_0_Om_80_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(260), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  38
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_0_1_Send%Ridge_1_0_0_Om_80_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc1(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_0_0_Om_80_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(36), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  26
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_1_0_Om_80_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_72

  subroutine Formura_internal_73 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Rsc0(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_79_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Rsc0(ix+(0), iy+(0), iz+(28))=Facet_0_0_1_src__0_0_0_dest__0_0_1_Recv%Ridge_0_0_1_Om_79_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Rsc1(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_80_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  38
          do ix = 1 ,  262
             Rsc1(ix+(0), iy+(0), iz+(28))=Facet_0_0_1_src__0_0_0_dest__0_0_1_Recv%Ridge_0_0_1_Om_80_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 3 ,  30 ,  1 

       do  iy = 1 ,  36 ,  1 

          do  ix = 1 ,  260 ,  1 

             a=Rsc0(ix+(1), iy+(1), iz+(-1))
             a_0=Rsc1(ix+(1), iy+(1), iz+(-1))

 Rsc2(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc0(ix+(2), iy+(1), iz+(-1)) - a ) - ( a - &
Rsc0(ix+(0), iy+(1), iz+(-1)) ) ) + ( ( Rsc0(ix+(1), iy+(2), iz+(-1)) - a ) - ( a - Rsc0(ix+(1), iy+(0), iz+(-1)) ) ) ) + ( ( Rsc0(ix+(1), iy+(1), iz+(0)) - a ) - ( a - Rsc0(ix+(1), iy+(1), iz+(-2)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Ridge_0_0_0_Om_81_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc2(ix+(258), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Ridge_0_0_0_Om_81_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc2(ix+(0), iy+(34), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Ridge_0_0_0_Om_81_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc2(ix+(258), iy+(34), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Facet_0_1_0_src__0_0_1_dest__0_1_1_Send%Ridge_0_1_0_Om_81_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc2(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_0_1_0_Om_81_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc2(ix+(258), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_0_1_Send%Ridge_1_0_0_Om_81_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc2(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_0_0_Om_81_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc2(ix+(0), iy+(34), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_1_0_Om_81_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc2(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_73

  subroutine Formura_internal_74 ()
    double precision  :: a
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz, a)
    do  iz = 3 ,  30 ,  1 

       do  iy = 1 ,  36 ,  1 

          do  ix = 1 ,  260 ,  1 

             a=Rsc1(ix+(1), iy+(1), iz+(-1))

 Rsc3(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( ( ( ( ( 1.0 / 900.0 ) * Rsc0(ix+(1), iy+(1), iz+(-1)) ) * a ) * a ) - ( ( 6.0 / 86400.0 ) * a ) ) + ( ( 6.1e-11 * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc1(ix+(2), iy+(1), iz+(-1)) - a ) - ( a - &
Rsc1(ix+(0), iy+(1), iz+(-1)) ) ) + ( ( Rsc1(ix+(1), iy+(2), iz+(-1)) - a ) - ( a - Rsc1(ix+(1), iy+(0), iz+(-1)) ) ) ) + ( ( Rsc1(ix+(1), iy+(1), iz+(0)) - a ) - ( a - Rsc1(ix+(1), iy+(1), iz+(-2)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Ridge_0_0_0_Om_82_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc3(ix+(258), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Ridge_0_0_0_Om_82_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc3(ix+(0), iy+(34), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Ridge_0_0_0_Om_82_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc3(ix+(258), iy+(34), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  260
             Facet_0_1_0_src__0_0_1_dest__0_1_1_Send%Ridge_0_1_0_Om_82_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc3(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_0_1_0_Om_82_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc3(ix+(258), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  36
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_0_1_Send%Ridge_1_0_0_Om_82_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc3(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_0_0_Om_82_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc3(ix+(0), iy+(34), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  28
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_1_0_Om_82_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc3(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_74

  subroutine Formura_internal_75 ()
    double precision  :: a
    double precision  :: a_0
    integer :: ix
    integer :: iy
    integer :: iz
    !$omp parallel

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Rsc2(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_81_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Rsc2(ix+(0), iy+(0), iz+(30))=Facet_0_0_1_src__0_0_0_dest__0_0_1_Recv%Ridge_0_0_1_Om_81_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Rsc3(ix+(0), iy+(0), iz+(0))=Ridge_0_0_0_Om_82_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  2
       do iy = 1 ,  36
          do ix = 1 ,  260
             Rsc3(ix+(0), iy+(0), iz+(30))=Facet_0_0_1_src__0_0_0_dest__0_0_1_Recv%Ridge_0_0_1_Om_82_r__0_0_0_r__0_0_1(ix, iy, iz)
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz, a, a_0)
    do  iz = 3 ,  32 ,  1 

       do  iy = 1 ,  34 ,  1 

          do  ix = 1 ,  258 ,  1 

             a=Rsc2(ix+(1), iy+(1), iz+(-1))
             a_0=Rsc3(ix+(1), iy+(1), iz+(-1))

 Rsc1(ix+(0), iy+(0), iz+(0))=(+( a + ( 200.0 * ( ( (-( ( ( ( 1.0 / 900.0 ) * a ) * a_0 ) * a_0 )) + ( ( 1.0 / 86400.0 ) * ( 1.0 - a ) ) ) + ( ( ( 0.1 * 2.3e-9 ) * ( 1000.0 * 1000.0 ) ) * ( ( ( ( Rsc2(ix+(2), iy+(1), iz+(-1)) - a ) - ( a - &
Rsc2(ix+(0), iy+(1), iz+(-1)) ) ) + ( ( Rsc2(ix+(1), iy+(2), iz+(-1)) - a ) - ( a - Rsc2(ix+(1), iy+(0), iz+(-1)) ) ) ) + ( ( Rsc2(ix+(1), iy+(1), iz+(0)) - a ) - ( a - Rsc2(ix+(1), iy+(1), iz+(-2)) ) ) ) ) ) ) ))

          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  34
          do ix = 1 ,  2
             Ridge_0_0_0_Om_83_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc1(ix+(256), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  258
             Ridge_0_0_0_Om_83_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(32), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Ridge_0_0_0_Om_83_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(256), iy+(32), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  258
             Facet_0_1_0_src__0_0_1_dest__0_1_1_Send%Ridge_0_1_0_Om_83_r__0_0_1_r__0_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_0_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_0_1_0_Om_83_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(256), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  34
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_0_1_Send%Ridge_1_0_0_Om_83_r__0_0_1_r__1_0_1(ix, iy, iz)=Rsc1(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_0_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_0_0_Om_83_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(32), iz+(2))
          end do
       end do
    end do

    !$omp do collapse(2) private(ix, iy, iz)
    do iz = 1 ,  30
       do iy = 1 ,  2
          do ix = 1 ,  2
             Facet_1_1_0_src__0_0_1_dest__1_1_1_Send%Ridge_1_1_0_Om_83_r__0_0_1_r__1_1_1(ix, iy, iz)=Rsc1(ix+(0), iy+(0), iz+(2))
          end do
       end do
    end do

    !$omp end parallel


  end subroutine Formura_internal_75


end module finalist_internal_13

