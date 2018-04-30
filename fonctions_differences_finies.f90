! Module de dérivées par différences finies 
!==============================================================================
! Le schéma est celui de l'article de C. Bogey et C. Bailly:
!   "A family of low dispersive and low dissipative explicit schemes for flow
!    and noise computations"

! Ainsi que l'article de J. Berland, C. Bogey, O. Marsden et C. Ballay
!   " High-order, low dispersive and low dissipative explicit schemes for
!     multiple-scale and boundary problems "

! Les fonctions implémentées couvrent les cas périodiques, centrés, décentrées à
! gauche, à droite, ou les deux à la fois, le tout pour un cas 1D ou 2D. Les
! prototypes des fonctions sont disponibles ci dessous.


! SOMMAIRE DES FONCTIONS ET SUBROUTINES DU MODULE
!=============================================================================
! Nbx correspond au nombre de points en X du domaine
! Nby correspond au nombre de points en Y du domaine
! dx et dy sont les pas spatiaux en X et Y

! Nb et da sont le nombre de point du vecteur et le pas spatial correspondant
! pour le cas 1D.

MODULE fonctions_differences_finies
        use iso_fortran_env
     !$ use OMP_lib
        implicit none
        private

! Fonctions de différences finies décentrées
public dfonc2dxd


contains

function dfonc2dxd(foncad,DimX, DimY,da) result(fd)
        implicit none
        integer, intent(in) :: DimX, DimY
        real(real64), dimension(DimX,DimY) :: fd
        real(real64), dimension(DimX,DimY), intent(in) :: foncad
        real(real64),intent(in) :: da
        integer :: qq , ii , xx
real(real64), dimension(11,5), parameter :: coef_diff5dc = reshape((/&
-2.391602219538d0,  5.832490322294d0, -7.650218001181d0, 7.907810563576d0, -5.922599052629d0,  3.071037015445d0,& 
-1.014956769726d0,  0.170022256519d0,  0.002819958377d0,-0.004791009708d0, -0.000013063429d0,&

-0.180022054228d0, -1.237550583044d0,  2.484731692990d0,-1.810320814061d0,  1.112990048440d0, -0.481086916514d0,& 
 0.126598690230d0, -0.015510730165d0,  0.000021609059d0, 0.000156447570d0, -0.000007390277d0,&

 0.057982271137d0, -0.536135360383d0, -0.264089548965d0, 0.917445877604d0, -0.169688364841d0, -0.029716326170d0,&
 0.029681617641d0, -0.005222483773d0, -0.000118806260d0,-0.000118806260d0, -0.000020069730d0,&

-0.013277273810d0,  0.115976072920d0, -0.617479187931d0,-0.274113948204d0,  1.086208764653d0, -0.402951626982d0,&
 0.131066986242d0, -0.028154858354d0,  0.002596328316d0, 0.000128743150d0,  0.000000000000d0,&

 0.016756572303d0, -0.117478455239d0,  0.411034935097d0,-1.130286765151d0,  0.341435872099d0,  0.556396830543d0,&
-0.082525734207d0,  0.003565834658d0,  0.001173034777d0,-0.000071772607d0, -0.000000352273d0 /)               &
 ,shape(coef_diff5dc),order=(/1,2/))
real(real64),dimension(5),parameter::coef_diff=(/0.872756993962667d0,-0.286511173973333d0,0.09032000128000002d0,&
                                                 -0.020779405824000d0,0.0024845946880000d0/)
        real(real64) :: unsda
        unsda = 1.0d0/da

write(*,*) omp_get_thread_num(), loc(fd)
!$OMP BARRIER

!$OMP DO SCHEDULE(STATIC)
do qq = 1, DimY

!calcul des points centrés
        do ii=6, DimX-5
                fd(ii,qq) = 0
                do xx=1, 5
                      fd(ii,qq) = fd(ii,qq) + coef_diff(xx)*(foncad(ii+xx,qq) - foncad(ii-xx,qq))
                enddo
                fd(ii,qq) = unsda * fd(ii,qq) 
        enddo

! calcul des points décentrés
       do ii=1,5  !points avant
               fd(ii,qq) = 0 
               do xx=1, 11
                       fd(ii,qq) = fd(ii,qq) + coef_diff5dc(xx,ii)*foncad(xx,qq)        
               enddo
               fd(ii,qq) = unsda*fd(ii,qq) 
       enddo
       do ii = DimX, DimX-4,-1   !points arriere
               fd(ii,qq) = 0
               do xx=1, 11
                        fd(ii,qq) = fd(ii,qq) - coef_diff5dc(xx,DimX-ii+1)*foncad(DimX-xx+1,qq)
                enddo
                fd(ii,qq) = unsda * fd(ii,qq) 
        enddo
enddo
!$OMP END DO
end function dfonc2dxd



end MODULE fonctions_differences_finies
