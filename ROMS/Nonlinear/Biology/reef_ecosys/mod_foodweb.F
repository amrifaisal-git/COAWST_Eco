
!!!=== Copyright (c) 2012-2020 Takashi NAKAMURA  =====
!!!      modified by Shinya AMANO, Faisal AMRI

#include "cppdefs.h"


!!!**** MODULE OF FOOD WEB *******************************************

MODULE mod_foodweb
  implicit none
CONTAINS

!!! **********************************************************************
!!!  Main program of foodweb model (Modified from Yamamoto et al. under review)
!!! **********************************************************************

  SUBROUTINE foodweb &
!   input parameters
    ( ng, n, i, j    &   ! ng: nested grid number; n: coral compartment; i,j: position
    , dt             &   ! Time step (sec)
    , PFD            &   ! Photon flux density (umol m-2 s-1)
    , rho_sw         &   ! Density of seawater (g cm-3)
    , Tmp            &   ! Temperature (oC)
    , Sal            &   ! Salinity (PSU)
    , DIC            &   ! Total dissolved inorganic carbon (DIC: umol kg-1)
    , TA             &   ! Total alkalinity (TA: umol kg-1)
    , DOx            &   ! Dissolved oxygen (umol L-1)
    , CaCO3          &   ! Calcium carbonate (umol L-1)    
    , DOC            &   ! Dissolved organic carbon (DOC: umol L-1)
    , POC            &   ! Particulate organic carbon (POC: umol L-1)
    , PHY1           &   ! phytoplankton (umol C L-1) small phytoplankton
    , PHY2           &   ! phytoplankton (umol C L-1) diatom
    , PHY3           &   ! phytoplankton (umol C L-1) coccolithoporid --ADDED BY AMRI, JAN 2020--
    , PHY4           &   ! phytoplankton (umol C L-1) diazotroph      --ADDED BY AMRI, FEB 2020--
    , ZOO            &   ! zooplankton (umol C L-1)
#if defined CARBON_ISOTOPE
    , DI13C          &   !13C of DIC (umol kg-1)
#endif
#if defined NUTRIENTS         
    , NO3            &   ! NO3 (umol L-1)
    , NO2            &   ! NO2 (umol L-1)
    , NH4            &   ! NH4 (umol L-1)
    , PO4            &   ! PO4 (umol L-1)
    , DON            &   ! Dissolved organic nitrogen     (DON: umol L-1)
    , PON            &   ! Particulate organic nitrogen   (PON: umol L-1)
    , DOP            &   ! Dissolved organic phosporius   (DOP: umol L-1)
    , POP            &   ! Particulate organic phosporius (POP: umol L-1)
#endif
#if defined COT_STARFISH         
    , COTe           &   ! COT starfish egg (umol L-1)
    , COTl           &   ! COT starfish larvae (umol L-1)
#endif
!   output parameters
    , dDIC_dt        &   ! dDIC/dt  (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    , dTA_dt         &   ! dTA/dt   (umol kg-1 s-1) 
    , dDOx_dt        &   ! dDOx/dt  (umol L-1 s-1)
    , dCaCO3_dt      &   ! dCaCO3/dt  (umol L-1 s-1)     
    , dDOC_dt        &   ! dDOC/dt  (umol L-1 s-1) 
    , dPOC_dt        &   ! dPOC/dt  (umol L-1 s-1) 
    , dPHY1_dt       &   ! dPHY1/dt (umol L-1 s-1)  
    , dPHY2_dt       &   ! dPHY2/dt (umol L-1 s-1) 
    , dPHY3_dt       &   ! dPHY3/dt (umol L-1 s-1) 
    , dPHY4_dt       &   ! dPHY4/dt (umol L-1 s-1)
    , dZOO_dt        &   ! dZOO/dt  (umol L-1 s-1)  
#if defined CARBON_ISOTOPE
    , dDI13C_dt      &   ! dDI13C/dt (umol kg-1 s-1)
#endif
#if defined NUTRIENTS
    , dNO3_dt        &   ! dNO3/dt (umol L-1 s-1)
    , dNO2_dt        &   ! dNO2/dt (umol L-1 s-1)
    , dNH4_dt        &   ! dNH4/dt (umol L-1 s-1)
    , dPO4_dt        &   ! dPO4/dt (umol L-1 s-1)
    , dDON_dt        &   ! dDON/dt (umol L-1 s-1)
    , dPON_dt        &   ! dPON/dt (umol L-1 s-1)
    , dDOP_dt        &   ! dDOP/dt (umol L-1 s-1)
    , dPOP_dt        &   ! dPOP/dt (umol L-1 s-1)
#endif                                
#if defined COT_STARFISH         
    , dCOTe_dt       &   ! dCOTe/dt (umol L-1 s-1)
    , dCOTl_dt       &   ! dCOTl/dt (umol L-1 s-1)
#endif
    )

    USE mod_geochem
!-----------------------------------------------------------------------
!
    implicit none

!        input parameters
    integer, intent(in) :: ng, n, i, j    ! ng: nested grid number; n: coral compartment; i,j: position
    real(8), intent(in) :: dt             ! Time step (sec)
    real(8), intent(in) :: PFD            ! Photon flux density (umol m-2 s-1)
    real(8), intent(in) :: rho_sw         ! Density of seawater (g cm-3)
    real(8), intent(in) :: Tmp            ! Temperature (oC)
    real(8), intent(in) :: Sal            ! Salinity (PSU)
    real(8), intent(in) :: DIC            ! Total dissolved inorganic carbon (DIC: umol kg-1)
    real(8), intent(in) :: TA             ! Total alkalinity (TA: umol kg-1)
    real(8), intent(in) :: DOx            ! Dissolved oxygen (umol L-1)
    real(8), intent(in) :: CaCO3          ! Calcium carbonate (umol L-1)
    real(8), intent(in) :: DOC            ! Dissolved organic carbon (DOC: umol L-1)
    real(8), intent(in) :: POC            ! Particulate organic carbon (DOC: umol L-1)
    real(8), intent(in) :: PHY1           ! phytoplankton (umol C L-1) small phytoplankton
    real(8), intent(in) :: PHY2           ! phytoplankton (umol C L-1) diatom
    real(8), intent(in) :: PHY3           ! phytoplankton (umol C L-1) coccolithopores --ADDED BY AMRI, JAN 2020--
    real(8), intent(in) :: PHY4           ! phytoplankton (umol C L-1) diazotroph      --ADDED BY AMRI, FEB 2020--
    real(8), intent(in) :: ZOO            ! zooplankton (umol C L-1)
#if defined CARBON_ISOTOPE
    real(8), intent(in) :: DI13C          !13C of DIC (umol kg-1)
#endif
#if defined NUTRIENTS         
    real(8), intent(in) :: NO3            ! NO3 (umol L-1)
    real(8), intent(in) :: NO2            ! NO2 (umol L-1)
    real(8), intent(in) :: NH4            ! NH4 (umol L-1)
    real(8), intent(in) :: PO4            ! PO4 (umol L-1)
    real(8), intent(in) :: DON            ! Dissolved organic nitrogen (DON: umol L-1)
    real(8), intent(in) :: PON            ! Particulate organic nitrogen (PON: umol L-1)
    real(8), intent(in) :: DOP            ! Dissolved organic phosporius (DOP: umol L-1)
    real(8), intent(in) :: POP            ! Particulate organic phosporius (POP: umol L-1)
#endif
#if defined COT_STARFISH         
    real(8), intent(in) :: COTe           ! COT starfish egg (umol L-1)
    real(8), intent(in) :: COTl           ! COT starfish larvae (umol L-1)
#endif
!          output parameters
    real(8), intent(out) :: dDIC_dt       ! dDIC/dt   (umol kg-1 s-1)  1 mmol m-3 = 1 umol L-1 = 1/1.024 umol kg-1
    real(8), intent(out) :: dTA_dt        ! dTA/dt    (umol kg-1 s-1) 
    real(8), intent(out) :: dDOx_dt       ! dDO/dt    (umol L-1 s-1) 
    real(8), intent(out) :: dCaCO3_dt     ! dCaCO3/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOC_dt       ! dDOC/dt   (umol L-1 s-1) 
    real(8), intent(out) :: dPOC_dt       ! dPOC/dt   (umol L-1 s-1) 
    real(8), intent(out) :: dPHY1_dt      ! dPHY1/dt  (umolC L-1 s-1)  
    real(8), intent(out) :: dPHY2_dt      ! dPHY2/dt  (umolC L-1 s-1)  
    real(8), intent(out) :: dPHY3_dt      ! dPHY3/dt  (umolC L-1 s-1) --ADDED BY AMRI, JAN 2020--
    real(8), intent(out) :: dPHY4_dt      ! dPHY4/dt  (umolC L-1 s-1) --ADDED BY AMRI, FEB 2020--
    real(8), intent(out) :: dZOO_dt       ! dZOO/dt   (umolC L-1 s-1)  
#if defined CARBON_ISOTOPE
    real(8), intent(out) :: dDI13C_dt     ! dDI13C/dt (umol kg-1 s-1)
#endif
#if defined NUTRIENTS
    real(8), intent(out) :: dNO3_dt       ! dNO3/dt (umol L-1 s-1)
    real(8), intent(out) :: dNO2_dt       ! dNO2/dt (umol L-1 s-1)
    real(8), intent(out) :: dNH4_dt       ! dNH4/dt (umol L-1 s-1)
    real(8), intent(out) :: dPO4_dt       ! dPO4/dt (umol L-1 s-1)
    real(8), intent(out) :: dDON_dt       ! dDON/dt (umol L-1 s-1)
    real(8), intent(out) :: dPON_dt       ! dPON/dt (umol L-1 s-1)
    real(8), intent(out) :: dDOP_dt       ! dDOP/dt (umol L-1 s-1)
    real(8), intent(out) :: dPOP_dt       ! dPOP/dt (umol L-1 s-1)
#endif
#if defined COT_STARFISH
    real(8), intent(out) :: dCOTe_dt      ! dCOTe/dt (umol L-1 s-1)
    real(8), intent(out) :: dCOTl_dt      ! dCOTl/dt (umol L-1 s-1)
#endif

!!!------------Set parameters  ----------------------------------

!------- Phytoplankton parameters ------------------------
! PHY1 Parameter (Small Phytoplankton)
!    real(8), parameter :: k_Pphy1 =  4.4d0/86400.0d0     ! (s-1)          PHY1 maximum growth rate (Krumhardt et al., 2019)
!    real(8), parameter :: b_Pphy1 =  0.0693d0            ! (degC-1)       Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)
!    real(8), parameter :: Iphy1   = 48.83d0*1.82d0       ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
!    real(8), parameter :: k_Rphy1 =  0.03d0/86400.0d0    ! (s-1)          PHY respiration rate at 0 oC (Kawamiya et al., 1995)
!    real(8), parameter :: b_Rphy1 =  0.0519d0            ! (degC-1)       Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
!    real(8), parameter :: k_Mphy1 =  0.00562d0/86400.0d0 ! (L umol-1 s-1) PHY mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day?0.0088; Kishi et al., 2001)0.0066
!    real(8), parameter :: b_Mphy1 =  0.069d0             ! (degC-1)       Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
!    real(8), parameter :: k_Ephy1 =  0.135d0             ! (no dim.)      PHY ratio of extracellular excretion to production 0.135(Kawamiya et al., 1995)
!
! PHY2 Parameter (Diatom)
!    real(8), parameter :: k_Pphy2 =  5.0d0/86400.0d0     ! (s-1)          PHY2 maximum growth rate (Krumhardt et al., 2019)
!    real(8), parameter :: b_Pphy2 =  0.0693d0            ! (degC-1)       Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)
!    real(8), parameter :: Iphy2   = 48.83d0*1.82d0       ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
!    real(8), parameter :: k_Rphy2 =  0.03d0/86400.0d0    ! (s-1)          PHY respiration rate at 0 oC (Kawamiya et al., 1995)
!    real(8), parameter :: b_Rphy2 =  0.0519d0            ! (degC-1)       Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
!    real(8), parameter :: k_Mphy2 =  0.00562d0/86400.0d0 ! (L umol-1 s-1) PHY mortality rate at 0 oC (0.00281d0 umol-1 d-1; 0.029L/umolN/day?0.0063; Kishi et al., 2001)0.003
!    real(8), parameter :: b_Mphy2 =  0.069d0             ! (degC-1)       Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
!    real(8), parameter :: k_Ephy2 =  0.135d0             ! (no dim.)      PHY ratio of extracellular excretion to production (Kawamiya et al., 1995)
!
! PHY3 Parameter (Coccolithoporid) --ADDED BY AMRI, JAN 2020--
!    real(8), parameter :: k_Pphy3 =  4.7d0/86400.0d0     ! (s-1)          PHY3 maximum growth rate (Krumhardt et al., 2019)
!    real(8), parameter :: b_Pphy3 =  0.0693d0            ! (degC-1)       Temperature coefficient for PHY photosynthesis (0.063; Kawamiya et al., 1995)
!    real(8), parameter :: Iphy3   = 48.83d0*1.82d0       ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
!    real(8), parameter :: k_Rphy3 =  0.03d0/86400.0d0    ! (s-1)          PHY respiration rate at 0 oC (Kawamiya et al., 1995)
!    real(8), parameter :: b_Rphy3 =  0.0519d0            ! (degC-1)       Temperature coefficient for PHY respiration rate (0.03d0 d-1; Kawamiya et al., 1995)
!    real(8), parameter :: k_Mphy3 =  0.00562d0/86400.0d0 ! (L umol-1 s-1) PHY mortality rate at 0 oC (0.00281d0 umol-1 d-1; 0.029L/umolN/day?0.0063; Kishi et al., 2001)0.003
!    real(8), parameter :: b_Mphy3 =  0.069d0             ! (degC-1)       Temperature coefficient for PHY mortality (Kawamiya et al., 1995)
!    real(8), parameter :: k_Ephy3 =  0.135d0             ! (no dim.)      PHY ratio of extracellular excretion to production (Kawamiya et al., 1995)

! PHY1 Parameter (Small Phytoplankton) --CHANGED BY AMRI--
    real(8), parameter :: k_Pphy1 = 4.4d0/86400.0d0       ! (s-1)          PHY1 maximum growth rate (Krumhardt et al., 2019)
    real(8), parameter :: Iphy1   = 48.83d0*1.82d0        ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
    real(8), parameter :: k_Rphy1 = 0.03d0/86400.0d0      ! (s-1)          PHY1 respiration rate at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Rphy1 = 0.0519d0              ! (degC-1)       Temperature coefficient for PHY1 respiration rate (Kishi et al., 2001) 
    real(8), parameter :: k_Mphy1 = 0.00562d0/86400.0d0   ! (L umol-1 s-1) PHY1 mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day?0.0088; Kishi et al., 2001)  
    real(8), parameter :: b_Mphy1 = 0.0693d0              ! (degC-1)       Temperature coefficient for PHY1 mortality (Kishi et al., 2001)        
    real(8), parameter :: k_Ephy1 = 0.135d0               ! (no dim.)      PHY1 ratio of extracellular excretion to production (Kishi et al., 2001)

! PHY2 Parameter (Diatom) --CHANGED BY AMRI--
    real(8), parameter :: k_Pphy2 = 5.0d0/86400.0d0       ! (s-1)          PHY2 maximum growth rate (Krumhardt et al., 2019)
    real(8), parameter :: Iphy2   = 48.83d0*1.82d0        ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)  
    real(8), parameter :: k_Rphy2 = 0.03d0/86400.0d0      ! (s-1)          PHY2 respiration rate at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Rphy2 = 0.0519d0              ! (degC-1)       Temperature coefficient for PHY2 respiration rate (Kishi et al., 2001)
    real(8), parameter :: k_Mphy2 = 0.00562d0/86400.0d0   ! (L umol-1 s-1) PHY2 mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day?0.0088; Kishi et al., 2001)      
    real(8), parameter :: b_Mphy2 = 0.0693d0              ! (degC-1)       Temperature coefficient for PHY2 mortality (Kishi et al., 2001) 
    real(8), parameter :: k_Ephy2 = 0.135d0               ! (no dim.)      PHY2 ratio of extracellular excretion to production (Kishi et al., 2001)

! PHY3 Parameter (Coccolithopores) --CHANGED BY AMRI--
    real(8), parameter :: k_Pphy3 = 4.7d0/86400.0d0       ! (s-1)          PHY3 maximum growth rate (Krumhardt et al., 2019)
    real(8), parameter :: Iphy3   = 48.83d0*1.82d0        ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
    real(8), parameter :: k_Rphy3 = 0.03d0/86400.0d0      ! (s-1)          PHY3 respiration rate at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Rphy3 = 0.0519d0              ! (degC-1)       Temperature coefficient for PHY3 respiration rate (Kishi et al., 2001)
    real(8), parameter :: k_Mphy3 = 0.00562d0/86400.0d0   ! (L umol-1 s-1) PHY3 mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day?0.0088; Kishi et al., 2001)                
    real(8), parameter :: b_Mphy3 = 0.0693d0              ! (degC-1)       Temperature coefficient for PHY3 mortality (Kishi et al., 2001) 
    real(8), parameter :: k_Ephy3 = 0.135d0               ! (no dim.)      PHY3 ratio of extracellular excretion to production (Kishi et al., 2001)

! PHY4 Parameter (Diazotroph) --CHANGED BY AMRI--
    real(8), parameter :: k_Pphy4 = 2.2d0/86400.0d0       ! (s-1)          PHY4 maximum growth rate (Krumhardt et al., 2019)
    real(8), parameter :: Iphy4   = 48.83d0*1.82d0        ! (umol m-2 s-1) PHY optimum light intensity (48.83d0 J m2 s-1; Kawamiya et al., 1995)
    real(8), parameter :: k_Rphy4 = 0.03d0/86400.0d0      ! (s-1)          PHY4 respiration rate at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Rphy4 = 0.0519d0              ! (degC-1)       Temperature coefficient for PHY4 respiration rate (Kishi et al., 2001)
    real(8), parameter :: k_Mphy4 = 0.00562d0/86400.0d0   ! (L umol-1 s-1) PHY4 mortality rate at 0 oC (0.00562d0 umol-1 d-1; 0.0585L/umolN/day?0.0088; Kishi et al., 2001)                
    real(8), parameter :: b_Mphy4 = 0.0693d0              ! (degC-1)       Temperature coefficient for PHY4 mortality (Kishi et al., 2001) 
    real(8), parameter :: k_Ephy4 = 0.135d0               ! (no dim.)      PHY4 ratio of extracellular excretion to production (Kishi et al., 2001)    
#if defined NUTRIENTS
! PHY1 Nutrient Assimilation Parameter         
    real(8), parameter :: Kphy1_NH4 = 0.01d0  ! (umol L-1)  Krumhardt et al., 2019       
    real(8), parameter :: Kphy1_NO3 = 0.2d0   ! (umol L-1)  Krumhardt et al., 2019     
    real(8), parameter :: Kphy1_PO4 = 0.005d0 ! (umol L-1)  Krumhardt et al., 2019

! PHY2 Nutrient Assimilation Parameter
    real(8), parameter :: Kphy2_NH4 = 0.05d0  ! (umol L-1)  Krumhardt et al., 2019
    real(8), parameter :: Kphy2_NO3 = 0.5d0   ! (umol L-1)  Krumhardt et al., 2019
    real(8), parameter :: Kphy2_PO4 = 0.05d0  ! (umol L-1)  Krumhardt et al., 2019

! PHY3 Nutrient Assimilation Parameter --ADDED BY AMRI, JAN 2020--
    real(8), parameter :: Kphy3_CO2 = 1.0d0    ! (umol L-1) Krumhardt et al., 2019
    real(8), parameter :: Kphy3_NH4 = 0.01d0   ! (umol L-1) Krumhardt et al., 2019   
    real(8), parameter :: Kphy3_NO3 = 0.2d0    ! (umol L-1) Krumhardt et al., 2019   
    real(8), parameter :: Kphy3_PO4 = 0.006d0  ! (umol L-1) Krumhardt et al., 2019

! PHY4 Nutrient Assimilation Parameter --ADDED BY AMRI, FEB 2020--
    real(8), parameter :: Kphy4_NH4 = 0.2d0    ! (umol L-1) Krumhardt et al., 2019   
    real(8), parameter :: Kphy4_NO3 = 2.0d0    ! (umol L-1) Krumhardt et al., 2019   
    real(8), parameter :: Kphy4_PO4 = 0.015d0  ! (umol L-1) Krumhardt et al., 2019    

    real(8), parameter :: psi1 =  1.5d0    !(umol L-1)-1  Fasham (1992)
    real(8), parameter :: psi2 =  1.5d0    !(umol L-1)-1  Fasham (1992) 
    real(8), parameter :: psi3 =  1.5d0    !(umol L-1)-1  Fasham (1992)
    real(8), parameter :: psi4 =  1.5d0    !(umol L-1)-1  Fasham (1992)    

#endif
!--------- Zooplankton parameters (MODIFIED BY AMRI, JAN 2020)--------
    real(8), parameter :: k_Gphy12zoo = 3.60d0/86400.0d0 ! (s-1)          Maximum grazing rate of PHY1 by ZOO (Krumhardt et al., 2019)
    real(8), parameter :: p_Gphy12zoo = 0.54d0           ! (umol L-1)     ZOO-PHY1 grazing half saturation state (Krumhardt et al., 2019)    
    real(8), parameter :: e_Gphy12zoo = 0.70d0           ! (no dim.)      Assimilation efficiency of ZOO (Kawamiya et al., 1995; Kishi et al., 2001)
    real(8), parameter :: g_Gphy12zoo = 0.30d0           ! (no dim.)      Growth efficiencty of ZOO (Kawamiya et al., 1995; Kishi et al., 2001)
    real(8), parameter :: lam1 = 0.211d0                 ! ((umol C L-1)-1)       zooplankton Ivlev constant for PHY1 (1.4L/umolN.0.211; Kishi et al., 2007)

    real(8), parameter :: k_Gphy22zoo = 3.41d0/86400.0d0 ! (s-1)          Maximum grazing rate of PHY2 by ZOO (Krumhardt et al., 2019)
    real(8), parameter :: p_Gphy22zoo = 0.72d0           ! (umol L-1)     ZOO-PHY2 grazing half saturation state (Krumhardt et al., 2019)     
    real(8), parameter :: e_Gphy22zoo = 0.70d0           ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
    real(8), parameter :: g_Gphy22zoo = 0.30d0           ! (no dim.)      Growth efficiencty of ZOO (Kawamiya et al., 1995; Kishi et al., 2001)
    real(8), parameter :: lam2 = 0.211d0                 ! ((umol C L-1)-1)       zooplankton Ivlev constant for PHY2 (1.4L/umolN.0.211; Kishi et al., 2007)    

    real(8), parameter :: k_Gphy32zoo = 2.95d0/86400.0d0 ! (s-1)          Maximum grazing rate of PHY3 by ZOO (Krumhardt et al., 2019)
    real(8), parameter :: p_Gphy32zoo = 0.854d0          ! (umol L-1)     ZOO-PHY3 grazing half saturation state (Krumhardt et al., 2019)     
    real(8), parameter :: e_Gphy32zoo = 0.70d0           ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
    real(8), parameter :: g_Gphy32zoo = 0.30d0           ! (no dim.)      Growth efficiencty of ZOO (Kawamiya et al., 1995; Kishi et al., 2001) 
    real(8), parameter :: lam3 = 0.211d0                 ! ((umol C L-1)-1)       zooplankton Ivlev constant for PHY3 (1.4L/umolN.0.211; Kishi et al., 2007)      

    real(8), parameter :: k_Gphy42zoo = 3.35d0/86400.0d0 ! (s-1)          Maximum grazing rate of PHY4 by ZOO (Krumhardt et al., 2019)
    real(8), parameter :: p_Gphy42zoo = 0.600d0          ! (umol L-1)     ZOO-PHY4 grazing half saturation state (Krumhardt et al., 2019)     
    real(8), parameter :: e_Gphy42zoo = 0.70d0           ! (no dim.)      Assimilation efficiency of ZOO (0.7:Kawamiya et al., 1995)
    real(8), parameter :: g_Gphy42zoo = 0.30d0           ! (no dim.)      Growth efficiencty of ZOO (Kawamiya et al., 1995; Kishi et al., 2001)   
    real(8), parameter :: lam4 = 0.211d0                 ! ((umol C L-1)-1)       zooplankton Ivlev constant for PHY4 (1.4L/umolN.0.211; Kishi et al., 2007)    

    real(8), parameter :: k_Rzoo = 0.005d0/86400.0d0     ! (s-1)          ZOO respiration rate at 0  oC 
    real(8), parameter :: b_Rzoo = 0.0693d0              ! (degC-1)       Temperature coefficient for ZOO respiration rate (Kawamiya et al., 1995)
    real(8), parameter :: k_Mzoo = 0.0176d0/86400.0d0    ! (L umol-1 s-1) ZOO mortality rate at 0 oC 3.0d0/86400.0d0(0.0088d0 umol-1 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Mzoo = 0.0693d0              ! (degC-1)       Temperature coefficient for ZOO mortality (Kawamiya et al., 1995)

!    real(8), parameter :: b_Gphy12zoo = 0.0693d0         ! (degC-1)       Temperature coefficient for ZOO grazing (Kawamiya et al., 1995; Kishi et al., 2001)
!    real(8), parameter :: b_Gphy22zoo = 0.0693d0         ! (degC-1)       Temperature coefficient for ZOO grazing (Kawamiya et al., 1995)
!    real(8), parameter :: b_Gphy32zoo = 0.0693d0         ! (degC-1)       Temperature coefficient for ZOO grazing (Kawamiya et al., 1995)
    
!    real(8), parameter :: t_Gphy12zoo = 0.265d0          ! (umol C L-1)       PHY1 threshold value for grazing by ZOO (0.04umolN/L0.265; Kishi et al., 2007)
!    real(8), parameter :: t_Gphy22zoo = 0.265d0          ! (umol C L-1)       PHY2 threshold value for grazing by ZOO (0.04umolN/L; Kishi et al., 2007)
!    real(8), parameter :: t_Gphy32zoo = 0.265d0          ! (umol C L-1)       PHY3 threshold value for grazing by ZOO (0.04umolN/L; Kishi et al., 2007)    
!------- Microbial loop parameters  -------
    real(8), parameter :: k_Gdoc2zoo = 0.0d0          ! (s-1)          Maximum grazing rate of DOC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gdoc2zoo = 0.0d0          ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)
    real(8), parameter :: k_Gpoc2zoo = 0.0d0          ! (s-1)          Maximum grazing rate of POC by ZOO at 0 oC (0.3d0 d-1; Kawamiya et al., 1995)
    real(8), parameter :: b_Gpoc2zoo = 0.0d0          ! (degC-1)       Temperature coefficient of DOC grazing by ZOO (Kawamiya et al., 1995)
!------- Decomposition organic matter parameters --------------------
    real(8), parameter :: k_Ddoc = 0.05d0/86400.0d0   ! (s-1)          Decomposition rate of DOC at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Ddoc = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DOC (Kishi et al., 2001)
    real(8), parameter :: k_Dpoc = 0.05d0/86400.0d0   ! (s-1)          Decomposition rate of POC at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Dpoc = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: k_Ddon = 0.05d0/86400.0d0   ! (s-1)          Decomposition rate of DON at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Ddon = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DON (Kishi et al., 2001)
    real(8), parameter :: k_Dpon = 0.05d0/86400.0d0   ! (s-1)          Decomposition rate of PON at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Dpon = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POC (Kishi et al., 2001)
    real(8), parameter :: k_Ddop = 0.05d0/86400.0d0   ! (s-1)          Decomposition rate of DOP at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Ddop = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of DOP (Kishi et al., 2001)
    real(8), parameter :: k_Dpop = 0.05d0/86400.0d0   ! (s-1)          Decomposition rate of POP at 0 oC (Kishi et al., 2001)
    real(8), parameter :: b_Dpop = 0.0693d0           ! (degC-1)       Temperature coefficient for decomposition of POP (Kishi et al., 2001)
    real(8), parameter :: k_Dcal = 0.1d0/86400.0d0    !                Fraction of remineralized CaCO3 (% d-1)
!------- Physical parameters --------------------
#if defined NUTRIENTS
    real(8), parameter :: k_Nit = 0.03/86400.0d0   	 ! (s-1)          Nitrification rate at 0 oC (0.03d0 d-1; Kishi et al., 2007)
    real(8), parameter :: b_Nit = 0.0693d0           ! (degC-1)       Temperature coefficient for nitrification (Kishi et al., 2007)
#endif
!------- Physical parameters --------------------
#if defined NUTRIENTS   

! Redfield Ratio (1934) C:N:P -> C:N=6.6  C:P=106    N:P=16
! Martiny et al  (2014) C:N:P -> C:N=7.4  C:P=163    N:P=22
! GLODAP Data           C:N:P -> C:N=9.2  C:P=131.9  N:P=14.4

    real(8), parameter :: rCNphy1 = 9.76d0      ! (no dim.) PHY1 C:N ratio
    real(8), parameter :: rCPphy1 = 141.23d0    ! (no dim.) PHY1 C:P ratio
    real(8), parameter :: rCNphy2 = 9.76d0      ! (no dim.) PHY2 C:N ratio
    real(8), parameter :: rCPphy2 = 142.23d0    ! (no dim.) PHY2 C:P ratio
    real(8), parameter :: rCNphy3 = 9.76d0      ! (no dim.) PHY3 C:N ratio
    real(8), parameter :: rCPphy3 = 142.23d0    ! (no dim.) PHY3 C:P ratio
    real(8), parameter :: rCNphy4 = 9.76d0      ! (no dim.) PHY4 C:N ratio
    real(8), parameter :: rCPphy4 = 142.23d0    ! (no dim.) PHY4 C:P ratio          
    real(8), parameter :: rCNzoo = 9.76d0       ! (no dim.) ZOO  C:N ratio
    real(8), parameter :: rCPzoo = 141.23d0     ! (no dim.) ZOO  C:P ratio
    real(8), parameter :: rNH4 = 0.3d0          ! (no dim.) PHY NH4:Nitrogen ratio 
    real(8), parameter :: rNP = 14.4d0          ! (no dim.) N:P ratio       (ADDED BY AMRI, JAN 2020)
    real(8), parameter :: rCP = 141.23d0        ! (no dim.) C:P ratio       (ADDED BY AMRI, JAN 2020)
    real(8), parameter :: rO2C = -1.453d0       ! (no dim.) O2:C ratio      (ADDED BY AMRI, FEB 2020)
    real(8), parameter :: rO2N = -10.625d0      ! (no dim.) O2:N ratio      (ADDED BY AMRI, FEB 2020)
    real(8), parameter :: rO2P = -170.0d0       ! (no dim.) O2:P ratio      (ADDED BY AMRI, FEB 2020)
#endif                                

!------- Local variables --------------------
    real(8) :: Pphy1, Rphy1, Mphy1, Ephy1, Aphy1
    real(8) :: Pphy2, Rphy2, Mphy2, Ephy2, Aphy2
    real(8) :: Pphy3, Rphy3, Mphy3, Ephy3, Aphy3        !ADDED BY AMRI, JAN 2020
    real(8) :: Pphy4, Rphy4, Mphy4, Ephy4, Aphy4        !ADDED BY AMRI, FEB 2020                
    real(8) :: pH, CO2aq, TmpK                          !ADDED BY AMRI, JAN 2020
    real(8) :: Gphy12zoo, Gphy22zoo, Rzoo, Mzoo
    real(8) :: Gphy32zoo, Gphy42zoo
    real(8) :: Gdoc2zoo, Gpoc2zoo
    real(8) :: Ddoc, Dpoc, Dpoc2doc
#if defined NUTRIENTS
    real(8) :: Gdon2zoo, Gpon2zoo
    real(8) :: Gdop2zoo, Gpop2zoo
    real(8) :: Ddon, Dpon, Dpon2don
    real(8) :: Ddop, Dpop, Dpop2dop
    real(8) :: Vphy1_NH4, Vphy1_NO3, Vphy1_PO4
    real(8) :: Vphy2_NH4, Vphy2_NO3, Vphy2_PO4
    real(8) :: Vphy3_NH4, Vphy3_NO3, Vphy3_PO4, Vphy3_CO2   !ADDED BY AMRI, JAN 2020
    real(8) :: Vphy4_NH4, Vphy4_NO3, Vphy4_PO4              !ADDED BY AMRI, FEB 2020
    real(8) :: Vphy1_N, Vphy2_N                             !ADDED BY AMRI, FEB 2020
    real(8) :: Vphy3_N, Vphy4_N                             !ADDED BY AMRI, FEB 2020
    real(8) :: CaCO3_dis, CaCO3_for, rCaCO3_C               !ADDED BY AMRI, JAN 2020
    real(8) :: rphy1_NO3, rphy2_NO3                         !ADDED BY AMRI, FEB 2020
    real(8) :: rphy3_NO3, rphy4_NO3                         !ADDED BY AMRI, FEB 2020    
    real(8) :: rphy1_NO3_NH4, rphy2_NO3_NH4                 !ADDED BY AMRI, FEB 2020
    real(8) :: rphy3_NO3_NH4, rphy4_NO3_NH4                 !ADDED BY AMRI, FEB 2020
    real(8) :: Nit, N_fix, N_ex                             !ADDED BY AMRI, FEB 2020
    real(8) :: k_Nitphy4, Nit_phy4                          !ADDED BY AMRI, FEB 2020
#endif

!!!------- Phytoplankton reaction (MODIFIED BY AMRI, JAN 2020)------------------------

!----- Gross photosynthetic rate (umolC L-1 s-1) -----------------
!    Pphy = k_Pphy * exp(b_Pphy*Tmp) * PFD/Iphy*exp(1.0d0-PFD/Iphy) * PHY
!    Pphy = k_Pphy * exp(b_Pphy*Tmp) * tanh(PFD/Iphy) * PHY
!    Pphy2 = k_Pphy2 * exp(b_Pphy2*Tmp) * tanh(PFD/Iphy2) * PHY2
!    Pphy3 = k_Pphy3 * exp(b_Pphy3*Tmp) * tanh(PFD/Iphy3) * PHY3 
                                                        
     Pphy1 = k_Pphy1 * tanh(PFD/Iphy1) * PHY1                ! --Equation from Sarmiento and Gruber (2006)--               
     Pphy2 = k_Pphy2 * tanh(PFD/Iphy2) * PHY2                
     Pphy3 = k_Pphy3 * tanh(PFD/Iphy3) * PHY3 
     Pphy4 = k_Pphy4 * tanh(PFD/Iphy4) * PHY4

    IF(DIC <= 0.d0) THEN !-----For Error handling
      Pphy1 = 0.d0
      Pphy2 = 0.d0
      Pphy3 = 0.d0
      Pphy4 = 0.d0
    ENDIF

    IF(Tmp <= 16.0d0) THEN  !----Diazotroph photosynthesis ceased at 16deg C-------
      Pphy4 = 0.d0          ! Moore et al., 2002
    ENDIF
!----- Assimilation rate (umolC L-1 s-1) -----------------

#if defined NUTRIENTS         
!    Vphy1_NH4 = NH4/(NH4+Kphy1_NH4)
!    Vphy1_NO3 = NO3/(NO3+Kphy1_NO3) * exp(-psi1 * NH4)
!    Vphy1_NH4 = NH4/(NH4+Kphy1_NH4) * rNH4
!    Vphy1_NO3 = NO3/(NO3+Kphy1_NO3) * exp(-psi1 * NH4) *(1.0d0 - rNH4)

!    Vphy2_NH4 = NH4 * NH4/(NH4 * NH4+Kphy2_NH4 * Kphy2_NH4) * rNH4										
!    Vphy2_NO3 = NO3 * NO3/(NO3 * NO3+Kphy2_NO3 * Kphy2_NO3) * exp(-psi2 * NH4)	*(1.0d0 - rNH4)			
!    Vphy2_NH4 = NH4 * NH4/(NH4 * NH4+Kphy2_NH4 * Kphy2_NH4)											
!    Vphy2_NO3 = NO3 * NO3/(NO3 * NO3+Kphy2_NO3 * Kphy2_NO3) * exp(-psi2 * NH4)				               
!    Vphy2_PO4 = PO4 * PO4/(PO4 * PO4+Kphy2_PO4 * Kphy2_PO4)											
    
!----------------CHANGED BY AMRI (JAN 2020)--------------------
    Vphy1_PO4 = PO4/(PO4+Kphy1_PO4)
    Vphy1_NO3 = NO3/(NO3+Kphy1_NO3)
    Vphy1_NH4 = NH4/(NH4+Kphy1_NH4)

    Vphy2_PO4 = PO4/(PO4+Kphy2_PO4)
    Vphy2_NO3 = NO3/(NO3+Kphy2_NO3)
    Vphy2_NH4 = NH4/(NH4+Kphy2_NH4)

    TmpK = Tmp + 273.15
    pH = pH_fromATCT( TA, DIC, TmpK, Sal )
    CO2aq = cCO2aq_fromCTpH( DIC, pH, TmpK, Sal )       ! Calculate CO2(aq) from DIC, pH, T, S (umol kg-1)
    
    Vphy3_CO2 = CO2aq/(CO2aq+(Kphy3_CO2/rho_sw))
    Vphy3_PO4 = PO4/(PO4+Kphy3_PO4)
    Vphy3_NO3 = NO3/(NO3+Kphy3_NO3)
    Vphy3_NH4 = NH4/(NH4+Kphy3_NH4)    

    Vphy4_PO4 = PO4/(PO4+Kphy4_PO4)
    Vphy4_NO3 = NO3/(NO3+Kphy4_NO3)
    Vphy4_NH4 = NH4/(NH4+Kphy4_NH4) 

    rphy1_NO3 = (Vphy1_NO3 * exp(-psi1 * NH4))          ! Modified nitrogen limiting factor (Sarmiento and Gruber., 2006)
    rphy2_NO3 = (Vphy2_NO3 * exp(-psi2 * NH4))          ! Modified nitrogen limiting factor (Sarmiento and Gruber., 2006)
    rphy3_NO3 = (Vphy3_NO3 * exp(-psi3 * NH4))          ! Modified nitrogen limiting factor (Sarmiento and Gruber., 2006)
    rphy4_NO3 = (Vphy4_NO3 * exp(-psi4 * NH4))          ! Modified nitrogen limiting factor (Sarmiento and Gruber., 2006)

    Vphy1_N = rphy1_NO3 + Vphy1_NH4 
    Vphy2_N = rphy2_NO3 + Vphy2_NH4
    Vphy3_N = rphy3_NO3 + Vphy3_NH4
    Vphy4_N = rphy4_NO3 + Vphy4_NH4

    rphy1_NO3_NH4 = rphy1_NO3/(Vphy1_N)       ! Nitrate to DIN uptake ratio of PHY1 (Kawamiya et al., 1995)
    rphy2_NO3_NH4 = rphy2_NO3/(Vphy2_N)       ! Nitrate to DIN uptake ratio of PHY2 (Kawamiya et al., 1995)
    rphy3_NO3_NH4 = rphy3_NO3/(Vphy3_N)       ! Nitrate to DIN uptake ratio of PHY3 (Kawamiya et al., 1995)
    rphy4_NO3_NH4 = rphy4_NO3/(Vphy4_N)       ! Nitrate to DIN uptake ratio of PHY4 (Kawamiya et al., 1995)
!---------------------------------------------------------------

    Aphy1 = MIN( Vphy1_N, Vphy1_PO4 )             ! Growth limiting factor of PHY1 --ADDED BY AMRI, JAN 2020--
    Aphy2 = MIN( Vphy2_N, Vphy2_PO4 )             ! Growth limiting factor of PHY2 --ADDED BY AMRI, JAN 2020--
    Aphy3 = MIN( Vphy3_N, Vphy3_PO4, Vphy3_CO2 )  ! Growth limiting factor of PHY3 --ADDED BY AMRI, JAN 2020--
    Aphy4 = MIN( Vphy4_N, Vphy4_PO4 )             ! Growth limiting factor of PHY4 --ADDED BY AMRI, JAN 2020--    

    Aphy1 = Aphy1 * Pphy1       ! Modified Gross Photosynthetic Production (Assimilation)
    Aphy2 = Aphy2 * Pphy2       ! Modified Gross Photosynthetic Production (Assimilation)
    Aphy3 = Aphy3 * Pphy3       ! Modified Gross Photosynthetic Production (Assimilation)
    Aphy4 = Aphy4 * Pphy4       ! Modified Gross Photosynthetic Production (Assimilation)    
#else
    Aphy1 = (1.0d0-k_Ephy1) * Pphy1
    Aphy2 = (1.0d0-k_Ephy2) * Pphy2
    Aphy3 = (1.0d0-k_Ephy3) * Pphy3  
    Aphy4 = (1.0d0-k_Ephy4) * Pphy4       
#endif

!----- Excretion rate (umolC L-1 s-1) -----------------
#if defined NUTRIENTS
    Ephy1 = Pphy1 - Aphy1      ! Residual photosynthesis result excreted
    Ephy2 = Pphy2 - Aphy2      ! Residual photosynthesis result excreted
    Ephy3 = Pphy3 - Aphy3      ! Residual photosynthesis result excreted
    Ephy4 = Pphy4 - Aphy4      ! Residual photosynthesis result excreted    
#else
    Ephy1 = k_Ephy1 * Pphy1
    Ephy2 = k_Ephy2 * Pphy2
    Ephy3 = k_Ephy3 * Pphy3
    Ephy4 = k_Ephy4 * Pphy4        
#endif

!----- Respiration rate (umolC L-1 s-1) -----------------
    Rphy1 = k_Rphy1 * exp(b_Rphy1 * Tmp) * PHY1
    Rphy2 = k_Rphy2 * exp(b_Rphy2 * Tmp) * PHY2
    Rphy3 = k_Rphy3 * exp(b_Rphy3 * Tmp) * PHY3 ! --ADDED BY AMRI, JAN 2020--  
    Rphy4 = k_Rphy4 * exp(b_Rphy4 * Tmp) * PHY4 ! --ADDED BY AMRI, FEB 2020--       
    IF(DOx <= 0.d0) THEN !-----For Error handling
      Rphy1 = 0.d0
      Rphy2 = 0.d0
      Rphy3 = 0.d0
      Rphy4 = 0.d0
    ENDIF

!----- Mortality (umolC L-1 s-1) -----------------
!   Mphy  = k_Mphy * exp(b_Mphy*Tmp) * PHY    
    Mphy1 = k_Mphy1 * exp(b_Mphy1 * Tmp) * PHY1 * PHY1
    Mphy2 = k_Mphy2 * exp(b_Mphy2 * Tmp) * PHY2 * PHY2
    Mphy3 = k_Mphy3 * exp(b_Mphy3 * Tmp) * PHY3 * PHY3
    Mphy4 = k_Mphy4 * exp(b_Mphy4 * Tmp) * PHY4 * PHY4   

!!!------- Zooplankton reaction ------------------------

!----- Grazing rate of PHY by ZOO (umolC L-1 s-1) -----------------

!    Gphy12zoo = k_Gphy12zoo * exp(b_Gphy12zoo*Tmp) * (1-exp(lam1*(t_Gphy12zoo - PHY1))) * ZOO
!    Gphy22zoo = k_Gphy22zoo * exp(b_Gphy22zoo*Tmp) * (1-exp(lam2*(t_Gphy22zoo - PHY2))) * ZOO
!    Gphy32zoo = k_Gphy32zoo * exp(b_Gphy32zoo*Tmp) * (1-exp(lam3*(t_Gphy32zoo - PHY3))) * ZOO              

    Gphy12zoo = k_Gphy12zoo * (1-exp(lam1*(p_Gphy12zoo - PHY1))) * ZOO  
    IF(Gphy12zoo <= 0.d0) THEN !-----For Error handling
    	Gphy12zoo = 0
    ENDIF

    Gphy22zoo = k_Gphy22zoo * (1-exp(lam2*(p_Gphy22zoo - PHY2))) * ZOO   
    IF(Gphy22zoo <= 0.d0) THEN !-----For Error handling
    	Gphy22zoo = 0
    ENDIF

    Gphy32zoo = k_Gphy32zoo * (1-exp(lam3*(p_Gphy32zoo - PHY3))) * ZOO    
    IF(Gphy32zoo <= 0.d0) THEN !-----For Error handling
    	Gphy32zoo = 0
    ENDIF

    Gphy42zoo = k_Gphy42zoo * (1-exp(lam4*(p_Gphy42zoo - PHY4))) * ZOO    
    IF(Gphy42zoo <= 0.d0) THEN !-----For Error handling
    	Gphy42zoo = 0
    ENDIF   
!----- Respiration rate (umolC L-1 s-1) -----------------
    Rzoo = k_Rzoo * exp(b_Rzoo * Tmp) * ZOO
    IF(DOx <= 0.d0) THEN !-----For Error handling
      Rzoo = 0.d0
    ENDIF

!----- Mortality (umolC L-1 s-1) -----------------
    Mzoo = k_Mzoo * exp(b_Mzoo * Tmp) * ZOO * ZOO
!    Mzoo = k_Mzoo * exp(b_Mzoo*Tmp) * ZOO
!    Mzoo = k_Mzoo * exp(b_Mzoo*Tmp) * (Gphy12zoo + Gphy22zoo)

!!!------- Microbial loop (implicitly assumed) ---------------

!----- Grazing rate of DOC by ZOO (umolC L-1 s-1) -----------------
    Gdoc2zoo = k_Gpoc2zoo * exp(b_Gdoc2zoo * Tmp) * DOC * ZOO

!----- Grazing rate of POC by ZOO (umolC L-1 s-1) -----------------
    Gpoc2zoo = k_Gdoc2zoo * exp(b_Gpoc2zoo * Tmp) * POC * ZOO

!!!------- Decomposition ------------------------

!----- Remineralization of organic matter (umol L-1 s-1) -----------------
!    Ddoc = k_Ddoc * exp(b_Ddoc*Tmp) * DOC
!    Ddon = k_Ddon * exp(b_Ddon*Tmp) * DON
!    Ddop = k_Ddop * exp(b_Ddop*Tmp) * DOP    
!    Ddoc = k_Ddoc * exp(b_Ddoc*Tmp) * DOC**2.0d0/(60.0d0**2.0d0+DOC**2.0d0)
!    Ddon = k_Ddon * exp(b_Ddon*Tmp) * DON**2.0d0/(60.0d0**2.0d0+DON**2.0d0)
!    Ddop = k_Ddop * exp(b_Ddop*Tmp) * DOP**2.0d0/(60.0d0**2.0d0+DOP**2.0d0)        

    Ddoc = k_Ddoc * exp(b_Ddoc * Tmp) * DOC   !DOC -> DIC
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Ddoc = 0.0d0
    ENDIF
#if defined NUTRIENTS
    Ddon = k_Ddon * exp(b_Ddon * Tmp) * DON   !DON -> NH4
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Ddon = 0.0d0
    ENDIF

    Ddop = k_Ddop * exp(b_Ddop * Tmp) * DOP   !DOP -> PO4
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Ddop = 0.0d0
    ENDIF
#endif

!----- Decomposition rate of POM (umol L-1 s-1) -----------------
!   Dpoc = k_Dpoc * exp(b_Dpoc*Tmp) * POC**2.0d0/(60.0d0**2.0d0+POC**2.0d0) 
!   Dpon = k_Dpon * exp(b_Dpon*Tmp) * PON**2.0d0/(60.0d0**2.0d0+PON**2.0d0) 
!   Dpop = k_Dpop * exp(b_Dpop*Tmp) * POP**2.0d0/(60.0d0**2.0d0+POP**2.0d0)

    Dpoc = k_Dpoc * exp(b_Dpoc * Tmp) * POC   !POC -> DIC
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Dpoc = 0.0d0
    ENDIF
#if defined NUTRIENTS
    Dpon = k_Dpon * exp(b_Dpon * Tmp) * PON   !PON -> NH4
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Dpon = 0.0d0
    ENDIF
    Dpop = k_Dpop * exp(b_Dpop * Tmp) * POP   !POP -> PO4
    IF(DOx <= 0.0d0) THEN !-----For Error handling
      Dpop = 0.0d0
    ENDIF
#endif
!----- Decomposition rate from POM to DOM (umol L-1 s-1) -----------------
    Dpoc2doc = k_Dpoc * exp(b_Dpoc * Tmp) * POC !POC -> DOC
    IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpoc2doc = 0.0d0
    ENDIF    
#if defined NUTRIENTS
    Dpon2don = k_Dpon * exp(b_Dpon * Tmp) * PON !PON -> DON
    IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpon2don = 0.0d0
    ENDIF    
    Dpop2dop = k_Dpop * exp(b_Dpop * Tmp) * POP !POP -> DOP
    IF(DOx <= 0.0d0) THEN !-----For Error handling
        Dpop2dop = 0.0d0
    ENDIF    
#endif

!----- Nitrogen fixation by diazotroph (umol L-1 s-1) ---------
    k_Nitphy4 = k_Pphy4 * 0.17d0 * 1.43d0 * 0.97d0                  !Reference maximum N2 uptake for diazotroph
    Nit_phy4 = k_Nitphy4 * (PHY4 / rCNphy4)                           !Nitrogen fixation rate by PHY4

    N_ex  = 0.3d0 * Nit_phy4                                        !Excreted DON
    N_fix = 0.7d0 * Nit_phy4                                        !Assimilated N2

!!!------- Nitrification ------------------------
#if defined NUTRIENTS
    Nit = k_Nit * exp(b_Nit * Tmp) * NH4                 ! (Kishi et al. 2001)
#endif

#if defined ORGANIC_MATTER
!!!------- Mass balance equations ------------------------
!dZOO_dt = (Gphy12zoo*e_Gphy12zoo) + (Gphy22zoo*e_Gphy22zoo) + (Gphy32zoo*e_Gphy32zoo) + Gdoc2zoo + Gpoc2zoo - Rzoo - Mzoo

    dPHY1_dt = Pphy1 - Rphy1 - Mphy1 - Ephy1 - Gphy12zoo
    
    dPHY2_dt = Pphy2 - Rphy2 - Mphy2 - Ephy2 - Gphy22zoo
    
    dPHY3_dt = Pphy3 - Rphy3 - Mphy3 - Ephy3 - Gphy32zoo

    dPHY4_dt = Pphy4 - Rphy4 - Mphy4 - Ephy4 - Gphy42zoo    
    
    dZOO_dt = Gphy12zoo + Gphy22zoo + Gphy32zoo + Gphy42zoo        &   !ZOO Grazing 
            - (Gphy12zoo * (1.0d0 - e_Gphy12zoo))                  &   !ZOO Egestion
            - (Gphy22zoo * (1.0d0 - e_Gphy22zoo))                  &   !ZOO Egestion 
            - (Gphy32zoo * (1.0d0 - e_Gphy32zoo))                  &   !ZOO Egestion
            - (Gphy42zoo * (1.0d0 - e_Gphy42zoo))                  &   !ZOO Egestion
            - (Gphy12zoo * (e_Gphy12zoo - g_Gphy12zoo))            &   !ZOO Excretion
            - (Gphy22zoo * (e_Gphy22zoo - g_Gphy22zoo))            &   !ZOO Excretion 
            - (Gphy32zoo * (e_Gphy32zoo - g_Gphy32zoo))            &   !ZOO Excretion
            - (Gphy42zoo * (e_Gphy42zoo - g_Gphy42zoo))            &   !ZOO Excretion
            - Rzoo - Mzoo + Gdoc2zoo + Gpoc2zoo                        !Mortality , DOC/POC grazing rate
    
#if defined COT_STARFISH
    dCOTe_dt = 0.0d0
    dCOTl_dt = 0.0d0
#endif

#if defined NUTRIENTS         
    dNO3_dt = - ((Aphy1 - Rphy1) * (rphy1_NO3_NH4/rCNphy1))         &   ! NO3 uptake by PHY1
              - ((Aphy2 - Rphy2) * (rphy2_NO3_NH4/rCNphy2))         &   ! NO3 uptake by PHY2
              - ((Aphy3 - Rphy3) * (rphy3_NO3_NH4/rCNphy3))         &   ! NO3 uptake by PHY3
              + Nit                                                     ! Nitrification

    dNH4_dt = - ((Aphy1 - Rphy1) * ((1 - rphy1_NO3_NH4)/rCNphy1))   &   ! NH4 uptake by PHY1
              - ((Aphy2 - Rphy2) * ((1 - rphy2_NO3_NH4)/rCNphy2))   &   ! NH4 uptake by PHY2
              - ((Aphy3 - Rphy3) * ((1 - rphy3_NO3_NH4)/rCNphy3))   &   ! NH4 uptake by PHY3
              + (Gphy12zoo * (e_Gphy12zoo - g_Gphy12zoo)/rCNzoo)    &   ! ZOO excretion
              + (Gphy22zoo * (e_Gphy22zoo - g_Gphy22zoo)/rCNzoo)    &   ! ZOO excretion
              + (Gphy32zoo * (e_Gphy32zoo - g_Gphy32zoo)/rCNzoo)    &   ! ZOO excretion
              + (Gphy42zoo * (e_Gphy42zoo - g_Gphy42zoo)/rCNzoo)    &   ! ZOO excretion
              + Ddon + Dpon - Nit                                       ! Decomposition - Nitrification

    dDON_dt = (Ephy1/rCNphy1) + (Ephy2/rCNphy2)                     &   ! PHY1 and PHY2 Excretion
            + (Ephy3/rCNphy3) + (Ephy4/rCNphy4)                     &   ! PHY3 and PHY4 Excretion
            - (Gdoc2zoo/rCNzoo) - Ddon + Dpon2don + N_ex                ! DOC grazing and decomposition

    dPON_dt = (Mphy1/rCNphy1) + (Mphy2/rCNphy2)                     &   ! PHY Mortality
            + (Mphy3/rCNphy3) + (Mphy4/rCNphy4)                     &   ! PHY Mortality
            + (Mzoo/rCNzoo)                                         &   ! ZOO Mortality
            + ((Gphy12zoo * (1.0d0 - e_Gphy12zoo))/rCNzoo)          &   ! ZOO Egestion
            + ((Gphy22zoo * (1.0d0 - e_Gphy22zoo))/rCNzoo)          &   ! ZOO Egestion
            + ((Gphy32zoo * (1.0d0 - e_Gphy32zoo))/rCNzoo)          &   ! ZOO Egestion
            + ((Gphy42zoo * (1.0d0 - e_Gphy42zoo))/rCNzoo)          &   ! ZOO Egestion
            - Dpon2don - Dpon - (Gpoc2zoo/rCNzoo)                       ! Decomposition   

    dNO2_dt = 0.0d0
    
    dPO4_dt = ((Gphy12zoo * (e_Gphy12zoo - g_Gphy12zoo))/rCPzoo)    &   ! ZOO excretion
            + ((Gphy22zoo * (e_Gphy22zoo - g_Gphy22zoo))/rCPzoo)    &   ! ZOO excretion
            + ((Gphy32zoo * (e_Gphy32zoo - g_Gphy32zoo))/rCPzoo)    &   ! ZOO excretion
            + ((Gphy42zoo * (e_Gphy42zoo - g_Gphy42zoo))/rCPzoo)    &   ! ZOO excretion
            - ((Aphy1 - Rphy1) * Vphy1_PO4 / rCPphy1)               &   ! PO4 uptake by PHY1
            - ((Aphy2 - Rphy2) * Vphy2_PO4 / rCPphy2)               &   ! PO4 uptake by PHY2
            - ((Aphy3 - Rphy3) * Vphy3_PO4 / rCPphy3)               &   ! PO4 uptake by PHY3
            - ((Aphy4 - Rphy4) * Vphy4_PO4 / rCPphy4)               &   ! PO4 uptake by PHY4
            + Ddop + Dpop                                               ! Decomposition

    dDOP_dt = (Ephy1/rCPphy1) + (Ephy2/rCPphy2)                     &
            + (Ephy3/rCPphy3) + (Ephy4/rCPphy4)                     &
            - (Gdoc2zoo/rCPzoo) - Ddop + Dpop2dop

    dPOP_dt = (Mphy1/rCPphy1) + (Mphy2/rCPphy2)                     &   ! PHY Mortality
            + (Mphy3/rCPphy3) + (Mphy4/rCPphy4)                     &   ! PHY Mortality
            + (Mzoo/rCPzoo)                                         &   ! ZOO Mortality
            + ((Gphy12zoo * (1.0d0 - e_Gphy12zoo))/rCPzoo)          &   ! ZOO Egestion
            + ((Gphy22zoo * (1.0d0 - e_Gphy22zoo))/rCPzoo)          &   ! ZOO Egestion
            + ((Gphy32zoo * (1.0d0 - e_Gphy32zoo))/rCPzoo)          &   ! ZOO Egestion
            + ((Gphy42zoo * (1.0d0 - e_Gphy42zoo))/rCPzoo)          &   ! ZOO Egestion
            - Dpop2dop - Dpop - (Gpoc2zoo/rCPzoo)                       ! Decomposition               

    dDOx_dt  = ((Pphy1 + Pphy2 + Pphy3 + Pphy4)*(-rO2C))            &     ! PHY Photosynthesis
             - ((Rphy1 + Rphy2 + Rphy3 + Rphy4)*(-rO2C))            &     ! PHY Respiration
             - ((Ddoc + Dpoc + Dpoc2doc)*(-rO2C))                   &     ! C org decomposition             
             - ((Ddon + Dpon + Dpon2don)*(-rO2N))                   &     ! N org decomposition
             - ((Ddop + Dpop + Dpop2dop)*(-rO2P))                   &     ! P org decomposition
             - ((Rzoo)*(-rO2C))                                     &     ! ZOO Respiration
             - ((Gphy12zoo*e_Gphy12zoo)*(-rO2C))                    &     ! ZOO - PHY1 Digestion
             - ((Gphy22zoo*e_Gphy22zoo)*(-rO2C))                    &     ! ZOO - PHY2 Digestion
             - ((Gphy32zoo*e_Gphy32zoo)*(-rO2C))                    &     ! ZOO - PHY3 Digestion
             - ((Gphy42zoo*e_Gphy42zoo)*(-rO2C))                    &     ! ZOO - PHY4 Digestion
             - (Nit*(-rO2N))                                              ! Nitrification
#else
    dDOx_dt  = ((Pphy1 + Pphy2 + Pphy3 + Pphy4)*(-rO2C))            &     ! PHY Photosynthesis
             - ((Rphy1 + Rphy2 + Rphy3 + Rphy4)*(-rO2C))            &     ! PHY Respiration
             - ((Ddoc + Dpoc + Dpoc2doc)*(-rO2C))                   &     ! C org decomposition             
             - ((Rzoo)*(-rO2C))                                     &     ! ZOO Respiration
             - ((Gphy12zoo*e_Gphy12zoo)*(-rO2C))                    &     ! ZOO - PHY1 Digestion
             - ((Gphy22zoo*e_Gphy22zoo)*(-rO2C))                    &     ! ZOO - PHY2 Digestion
             - ((Gphy32zoo*e_Gphy32zoo)*(-rO2C))                    &     ! ZOO - PHY3 Digestion
             - ((Gphy42zoo*e_Gphy42zoo)*(-rO2C))                          ! ZOO - PHY4 Digestion
#endif

    dDOC_dt = Ephy1 + Ephy2 + Ephy3 + Ephy4 - Gdoc2zoo - Ddoc + Dpoc2doc

    dPOC_dt = Mphy1 + Gphy12zoo*(1.0d0-e_Gphy12zoo) &
            + Mphy2 + Gphy22zoo*(1.0d0-e_Gphy22zoo) &
            + Mphy3 + Gphy32zoo*(1.0d0-e_Gphy32zoo) &
            + Mphy4 + Gphy42zoo*(1.0d0-e_Gphy42zoo) &
            + Mzoo - Gpoc2zoo - Dpoc - Dpoc2doc

!-------------------Coccolithopores Calcification---------------------------------     
    rCaCO3_C = (-0.0136d0 * CO2aq) + 1.21d0     ! CaCO3 : Corg ratio
    IF (rCaCO3_C <= 0) THEN
        rCaCO3_C = 0.0d0
    END IF
    
    CaCO3_for = rCaCO3_C * Pphy3
    CaCO3_for = CaCO3_for * (MIN( Vphy3_N, Vphy3_PO4, Vphy3_CO2 ))**2.0d0 
    
    IF (Tmp < 5.0d0) THEN
        CaCO3_for = CaCO3_for * ((Tmp + 2.0d0)/28.0d0)
    END IF

    IF (Tmp < 0.0d0) THEN
        CaCO3_for = CaCO3_for * 0.0001d0
    END IF    

    IF(PHY3 > 2.0d0) THEN
        CaCO3_for = CaCO3_for * PHY3 * 0.5d0
    END IF
!---------------------------------------------------------------------------------

!------------------------CaCO3 remineralization-----------------------------------    
    CaCO3_dis = CaCO3 * k_Dcal * 0.01d0     ! CaCO3 remineralization
!---------------------------------------------------------------------------------

    dCaCO3_dt = - CaCO3_dis &
                + ((Mphy3 + (Gphy32zoo * (1 - e_Gphy42zoo))) * rCaCO3_C) 

    dDIC_dt = Rzoo + Rphy1 + Rphy2 + Rphy3 + Rphy4  &                     ! DIC change due to organic matter
            + Ddoc + Dpoc                           &
            - Pphy1 - Pphy2 - Pphy3 - Pphy4
    dTA_dt  = dDIC_dt * (-rNP/rCP)                                        ! TA change due to organic matter

    dDIC_dt = dDIC_dt - (1.0d0 * (CaCO3_for - CaCO3_dis))                 ! DIC change due to CaCO3
    dTA_dt  = dTA_dt - (2.0d0 * (CaCO3_for - CaCO3_dis))                  ! TA change due to CaCO3
    dDIC_dt = dDIC_dt/rho_sw                                              ! (umol L-1 s-1) -> (umol kg-1 s-1)

    dTA_dt  = dTA_dt/rho_sw                                               ! (umol L-1 s-1) -> (umol kg-1 s-1)
#endif      
    RETURN

  END SUBROUTINE foodweb

END MODULE mod_foodweb