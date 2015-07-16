MODULE Blade


   ! Contains blade information.


USE                             Precision


REAL(ReKi), ALLOCATABLE      :: C       (:)     ! Chord of each blade element (FROM INPUT FILE)
REAL(ReKi), ALLOCATABLE      :: DR      (:)     ! Span-wise width of the element (length of the element, centered at RELM(i)) (FROM INPUT FILE)
REAL(ReKi)                   :: R               ! rotor radius

INTEGER                      :: NB              ! number of blades


END MODULE Blade
