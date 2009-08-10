draw.arc               package:plotrix               R Documentation

_D_r_a_w _a_r_c

_D_e_s_c_r_i_p_t_i_o_n:

     Draw one or more arcs using classic graphics.

_U_s_a_g_e:

      draw.arc(x=1,y=NULL,radius=1,angle1=deg1*pi/180,angle2=deg2*pi/180, 
       deg1=0,deg2=45,n=35,col=1,...)

_A_r_g_u_m_e_n_t_s:

       x: x coordinate of center.  Scalar or vector. 

       y: y coordinate of center.  Scalar or vector. 

  radius: radius.  Scalar or vector.  

  angle1: Starting angle in radians. Scalar or vector. 

  angle2: Ending angle in radians. Scalar or vector. 

    deg1: Starting angle in degrees. Scalar or vector. 

    deg2: Ending angle in degrees. Scalar or vector. 

       n: Number of polygons to use to approximate the arc. 

     col: Arc colors. 

     ...: Other arguments passed to segments.  Vectorization  is not
          supported for these. 

_D_e_t_a_i_l_s:

     Draws one or more arcs from 'angle1' to 'angle2'. If 'angle1' is
     numerically greater than 'angle2', then the angles are swapped.

_V_a_l_u_e:

     Returns a matrix of expanded arguments invisibly.

_A_u_t_h_o_r(_s):

     Gabor Grothendieck

_E_x_a_m_p_l_e_s:

     plot(1:10, asp = 1,main="Test draw.arc")
     draw.arc(5, 5, 1:10/10, deg2 = 1:10*10, col = "blue")
     draw.arc(8, 8, 1:10/10, deg2 = 1:10*10, col = 1:10)

