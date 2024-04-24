!/***** global *****/
      integer*4 dim !3 /* the dimension */
      integer*4 max_enon  !8 /* maximum size of elemental matrices */
      integer*4 max_enon1 !9 /* maximum size of elemental matrices plus 1 for effective vectorization */
      integer*4 max_snop !4 /* maximum size of integration points on a surface */
      parameter(dim=3, max_enon=8, max_enon1=9, max_snop=4 )

!/***** capacity *****/
      integer*4 node_cap !100 /* capacity of nodal array */
      integer*4 element_cap !100 /* capacity of elemental array */
      integer*4 surface_cap !100 /* capacity of surfical array */
      parameter( node_cap=100, element_cap=100, surface_cap=100 )

!/***** hex *****/
      integer*4 hex_enon !8 /* number of consisting nodes of a hex */
      integer*4 hex_enos !6 /* number of surfaces of a hex */
      parameter( hex_enon=8, hex_enos=6 )

!/***** wed *****/
      integer*4 wed_enon !6 /* number of consisting nodes of a wed */
      integer*4 wed_enos !5 /* number of surfaces of a wed */
      parameter( wed_enon=6, wed_enos=5 )

!/***** pyr *****/
      integer*4 pyr_enon !5 /* number of consisting nodes of a pyr */
      integer*4 pyr_enos !5 /* number of surfaces of a pyr */
      parameter( pyr_enon=5, pyr_enos=5 )

!/***** tet *****/
      integer*4 tet_enon !4 /* number of consisting nodes of a tet */
      integer*4 tet_enos !4 /* number of surfaces of a tet */
      parameter( tet_enon=4, tet_enos=4 )

!/***** others *****/
      integer*4 stdout !6
      integer*4 stderr !0
      integer*4 stdin  !5
      parameter( stdout=6, stderr=0, stdin=5 )

