!----------------------------------------------------------------------------------------------------
MODULE SharedInflowDefns
! This module is used to define shared types and parameters that are used in the module InflowWind.
! 7 Oct 2009    B. Jonkman, NREL/NWTC
!----------------------------------------------------------------------------------------------------

   USE NWTC_Library                                               ! Precision module 

   !-------------------------------------------------------------------------------------------------
   ! Shared types
   !-------------------------------------------------------------------------------------------------

!   TYPE, PUBLIC :: InflLoc   
!      REAL(ReKi)                    :: Position(3)                ! X, Y, Z
!   END TYPE InflLoc
   
   
   TYPE, PUBLIC :: InflIntrpOut
      REAL(ReKi)                    :: Velocity(3)                ! U, V, W
   END TYPE InflIntrpOut

   !-------------------------------------------------------------------------------------------------
   ! Shared parameters, defining the wind types
   ! THEY MUST BE UNIQUE!
   !-------------------------------------------------------------------------------------------------

   INTEGER, PARAMETER, PUBLIC  :: DEFAULT_Wind = -1        ! Undetermined wind type; calls internal routine to guess what type of file it is.
   INTEGER, PARAMETER, PUBLIC  :: Undef_Wind   =  0        ! This is the code for an undefined WindType
   INTEGER, PARAMETER, PUBLIC  :: HH_Wind      =  1        ! Hub-Height wind file
   INTEGER, PARAMETER, PUBLIC  :: FF_Wind      =  2        ! Binary full-field wind file
   INTEGER, PARAMETER, PUBLIC  :: UD_Wind      =  3        ! User-defined wind
   INTEGER, PARAMETER, PUBLIC  :: FD_Wind      =  4        ! 4-dimensional wind (LES)
   INTEGER, PARAMETER, PUBLIC  :: CTP_Wind     =  5        ! Coherent turbulence wind field (superimpose KH billow on background wind)
   INTEGER, PARAMETER, PUBLIC  :: HAWC_Wind    =  6        ! Binary full-field wind file in HAWC format

END MODULE SharedInflowDefns
