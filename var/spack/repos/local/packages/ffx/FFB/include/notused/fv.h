C Numeric tags (codes) for FIELDVIEW binary file format. 

C Bit pattern used to detect byte ordering.
      integer FV_MAGIC
      parameter(FV_MAGIC = 66051)

C Data section id codes
      integer FV_NODES
      parameter(FV_NODES = 1001)

      integer FV_FACES
      parameter(FV_FACES = 1002)

      integer FV_ELEMENTS
      parameter(FV_ELEMENTS = 1003)

      integer FV_VARIABLES
      parameter(FV_VARIABLES = 1004)

C Element (cell) type id codes
      integer FV_TET_ELEM_ID
      parameter(FV_TET_ELEM_ID = 1)

      integer FV_HEX_ELEM_ID
      parameter(FV_HEX_ELEM_ID = 2)

      integer FV_PRISM_ELEM_ID
      parameter(FV_PRISM_ELEM_ID = 3)

      integer FV_PYRA_ELEM_ID
      parameter(FV_PYRA_ELEM_ID = 4)

C Values for "wall_info" array (see comments in ftn_encode_header)
      integer A_WALL
      parameter (A_WALL = 7)

      integer NOT_A_WALL
      parameter (NOT_A_WALL = 0)

