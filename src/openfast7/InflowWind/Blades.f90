MODULE Blades


   ! This MODULE stores input variables for the blades.


USE                             Precision


REAL(ReKi)                   :: AdjBlMs                                         ! Factor to adjust blade mass density.
REAL(ReKi)                   :: AdjEdSt                                         ! Factor to adjust edge stiffness.
REAL(ReKi)                   :: AdjFlSt                                         ! Factor to adjust flap stiffness.
REAL(ReKi), ALLOCATABLE      :: AerCen    (:)                                   ! Aerodynamic center for distributed input data.
REAL(ReKi), ALLOCATABLE      :: AeroCent  (:,:)                                 ! Aerodynamic center for analysis nodes.
REAL(ReKi), ALLOCATABLE      :: AeroTwst  (:)                                   ! Aerodynamic twist of the blade at the analysis nodes.
REAL(ReKi), ALLOCATABLE      :: Alpha     (:)                                   ! Blade coupling coefficient between flap and twist for a given input station.
REAL(ReKi), ALLOCATABLE      :: AxRedBld  (:,:,:,:)                             ! The axial-reduction terms of the blade shape function.
REAL(ReKi), ALLOCATABLE      :: BAlpha    (:,:)                                 ! Interpolated blade coupling coefficient between flap and twist.
REAL(ReKi), ALLOCATABLE      :: BldEDamp  (:,:)                                 ! Blade edgewise damping coefficients.
REAL(ReKi)                   :: BldEdDmp  (1)                                   ! Blade structural damping ratios in edgewise direction.
REAL(ReKi), ALLOCATABLE      :: BldFDamp  (:,:)                                 ! Blade flapwise damping coefficients.
REAL(ReKi)                   :: BldFlDmp  (2)                                   ! Blade structural damping ratios in flapwise direction.
REAL(ReKi)                   :: BldFlexL                                        ! Flexible blade length.
REAL(ReKi), ALLOCATABLE      :: BlFract   (:)                                   ! Blade fractional radius for distributed input data.
REAL(ReKi), ALLOCATABLE      :: BMassDen  (:)                                   ! Blade mass density for distributed input data.
REAL(ReKi), ALLOCATABLE      :: CAeroTwst (:)                                   ! Cosine of the aerodynamic twist of the blade at the analysis nodes.
REAL(ReKi), ALLOCATABLE      :: CBE       (:,:,:)                               ! Generalized edgewise damping of the blades.
REAL(ReKi), ALLOCATABLE      :: CBF       (:,:,:)                               ! Generalized flapwise damping of the blades.
REAL(ReKi), ALLOCATABLE      :: cgOffBEdg (:,:)                                 ! Interpolated blade edge (along local aerodynamic yb-axis) mass cg offset.
REAL(ReKi), ALLOCATABLE      :: cgOffBFlp (:,:)                                 ! Interpolated blade flap (along local aerodynamic xb-axis) mass cg offset.
REAL(ReKi), ALLOCATABLE      :: Chord     (:)                                   ! Chord of the blade at the analysis nodes.
REAL(ReKi), ALLOCATABLE      :: CThetaS   (:,:)                                 ! COS( ThetaS )
REAL(ReKi), ALLOCATABLE      :: DRNodes   (:)                                   ! Length of variable-spaced blade elements.
REAL(ReKi), ALLOCATABLE      :: EAOffBEdg (:,:)                                 ! Interpolated blade edge (along local aerodynamic yb-axis) elastic axis offset.
REAL(ReKi), ALLOCATABLE      :: EAOffBFlp (:,:)                                 ! Interpolated blade flap (along local aerodynamic xb-axis) elastic axis offset.
REAL(ReKi), ALLOCATABLE      :: EAStff    (:)                                   ! Blade extensional stiffness for a given input station.
REAL(ReKi), ALLOCATABLE      :: EdgcgOf   (:)                                   ! Blade edge (along local aerodynamic yb-axis) mass cg offset for a given input station.
REAL(ReKi), ALLOCATABLE      :: EdgEAOf   (:)                                   ! Blade edge (along local aerodynamic yb-axis) elastic axis offset for a given input station.
REAL(ReKi), ALLOCATABLE      :: EdgIner   (:)                                   ! Blade edge (about local structural xb-axis) mass inertia per unit length for a given input station.
REAL(ReKi), ALLOCATABLE      :: EdgStff   (:)                                   ! Blade edge stiffness for distributed input data.
REAL(ReKi), ALLOCATABLE      :: FlpcgOf   (:)                                   ! Blade flap (along local aerodynamic xb-axis) mass cg offset for a given input station.
REAL(ReKi), ALLOCATABLE      :: FlpEAOf   (:)                                   ! Blade flap (along local aerodynamic xb-axis) elastic axis offset for a given input station.
REAL(ReKi), ALLOCATABLE      :: FlpIner   (:)                                   ! Blade flap (about local structural yb-axis) mass inertia per unit length for a given input station.
REAL(ReKi), ALLOCATABLE      :: FlpStff   (:)                                   ! Blade flap stiffness for distributed input data.
REAL(ReKi), ALLOCATABLE      :: FStTunr   (:,:)                                 ! Blade flapwise modal stiffness tuners (stored for all blades).
REAL(ReKi)                   :: FlStTunr  (2)                                   ! Blade flapwise modal stiffness tuners (input).
REAL(ReKi), ALLOCATABLE      :: GJStff    (:)                                   ! Blade torsional stiffness for a given input station.
REAL(ReKi), ALLOCATABLE      :: InerBEdg  (:,:)                                 ! Interpolated blade edge (about local structural xb-axis) mass inertia per unit length.
REAL(ReKi), ALLOCATABLE      :: InerBFlp  (:,:)                                 ! Interpolated blade flap (about local structural yb-axis) mass inertia per unit length.
REAL(ReKi), ALLOCATABLE      :: KBE       (:,:,:)                               ! Generalized edgewise stiffness of the blades.
REAL(ReKi), ALLOCATABLE      :: KBF       (:,:,:)                               ! Generalized flapwise stiffness of the blades.
REAL(ReKi), ALLOCATABLE      :: MassB     (:,:)                                 ! Interpolated lineal blade mass density.
REAL(ReKi), ALLOCATABLE      :: PrecrvRef (:)                                   ! Offset for defining the reference axis from the pitch axis for precurved blades at a given input station.
REAL(ReKi), ALLOCATABLE      :: PreswpRef (:)                                   ! Offset for defining the reference axis from the pitch axis for preswept  blades at a given input station.
REAL(ReKi), ALLOCATABLE      :: RefAxisxb (:,:)                                 ! Interpolated Offset for defining the reference axis from the pitch axis for precurved blades at a given input station (along xb-axis).
REAL(ReKi), ALLOCATABLE      :: RefAxisyb (:,:)                                 ! Interpolated Offset for defining the reference axis from the pitch axis for preswept  blades at a given input station (along yb-axis).
REAL(ReKi), ALLOCATABLE      :: RNodes    (:)                                   ! Radius to analysis nodes relative to hub ( 0 < RNodes(:) < BldFlexL )
REAL(ReKi), ALLOCATABLE      :: RNodesNorm(:)                                   ! Normalized radius to analysis nodes relative to hub ( 0 < RNodesNorm(:) < 1 )
REAL(ReKi), ALLOCATABLE      :: rSAerCenn1(:,:)                                 ! Distance from point S on a blade to the aerodynamic center in the n1 direction (m).
REAL(ReKi), ALLOCATABLE      :: rSAerCenn2(:,:)                                 ! Distance from point S on a blade to the aerodynamic center in the n2 direction (m).
REAL(ReKi), ALLOCATABLE      :: SAeroTwst (:)                                   ! Sine of the aerodynamic twist of the blade at the analysis nodes.
REAL(ReKi), ALLOCATABLE      :: StiffBE   (:,:)                                 ! Interpolated edgewise blade stiffness.
REAL(ReKi), ALLOCATABLE      :: StiffBEA  (:,:)                                 ! Interpolated blade extensional stiffness.
REAL(ReKi), ALLOCATABLE      :: StiffBF   (:,:)                                 ! Interpolated flapwise blade stiffness.
REAL(ReKi), ALLOCATABLE      :: StiffBGJ  (:,:)                                 ! Interpolated blade torsional stiffness.
REAL(ReKi), ALLOCATABLE      :: SThetaS   (:,:)                                 ! SIN( ThetaS )
REAL(ReKi), ALLOCATABLE      :: StrcTwst  (:)                                   ! Structural twist for distributed input data.
REAL(ReKi), ALLOCATABLE      :: ThetaS    (:,:)                                 ! Structural twist for analysis nodes.
REAL(ReKi), ALLOCATABLE      :: TwistedSF (:,:,:,:,:)                           ! Interpolated lineal blade mass density.

INTEGER(4)                   :: BldNodes                                        ! Number of blade nodes used in the analysis.
INTEGER(4)                   :: NBlInpSt                                        ! Number of blade input stations.
INTEGER(4)                   :: TipNode                                         ! Index of the additional node located at the blade tip = BldNodes + 1


END MODULE Blades
