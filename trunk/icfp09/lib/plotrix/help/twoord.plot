twoord.plot             package:plotrix             R Documentation

_P_l_o_t _w_i_t_h _t_w_o _o_r_d_i_n_a_t_e_s

_D_e_s_c_r_i_p_t_i_o_n:

     Two sets of values are displayed on the same plot with different
     ordinate scales on the left and right.

_U_s_a_g_e:

      twoord.plot(lx,ly,rx,ry,data=NULL,xlim=NULL,lylim=NULL,rylim=NULL,
      mar=c(5,4,4,4),lcol=1,rcol=2,xlab="",ylab="",rylab="",lpch=1,rpch=2,
      type="b",halfwidth=0.4,...)

_A_r_g_u_m_e_n_t_s:

lx,ly,rx,ry: y and optional x values for the plot

    data: an optional data frame from which to obtain the above values

    xlim: optional x limits as in 'plot'

lylim,rylim: optional y limits for the left and right axes respectively

     mar: optional margin adjustment, defaults to 'c(5,4,4,4)'

lcol,rcol: colors to distinguish the two sets of values

xlab,ylab: axis labels as in 'plot'

   rylab: label for the right axis

lpch,rpch: plot symbols to distinguish the two sets of values

    type: as in 'plot'

halfwidth: Half the width of the bars in user units. The bars are
          centered on successive integers if no 'x' values are
          supplied.

     ...: additional arguments passed to 'axis'.

_D_e_t_a_i_l_s:

     'twoord.plot' automates the process of displaying two sets of
     values that have different ranges on the same plot. It is
     principally useful in illustrating some relationship between the
     values across the observations. It is assumed that the 'lx' and
     'rx' values are at least adjacent, and probably overlapping.

     It is best to pass all the arguments 'lx, ly, rx, ry', but the
     function will attempt to substitute sensible x values if one or
     two are missing.

     If at least one of the 'type' arguments is "bar", bars will be
     plotted instead of points or lines. It is best to plot the bars
     first (i.e. relative to the left axis) if the other type is points
     or lines, as the bars will usually obscure at least some of the
     points or lines. Using NA for the color of the bars will partially
     correct this. If both types are to be bars, remember to pass
     somewhat different x values or the bars will be overlaid.

_V_a_l_u_e:

     nil

_N_o_t_e:

     There are many objections to the use of plots with two different 
     ordinate scales, and some of them are even sensible and supported
     by  controlled observation. Many of the objections rest on
     assertions that the  spatial arrangement of the values plotted
     will override all other  evidence. Here are two:

     The viewer will assume that the vertical position of the data
     points  indicates a quantitative relationship.

     To some extent. It is probably not a good idea to have the spatial
      relationship of the points opposed to their numerical
     relationship. That  is to say, if one set of values is in the
     range of 0-10 and the other  20-100, it is best to arrange the
     plot so that the latter values are  not plotted below the former.

     The viewer will assume that an intersection of lines indicates an 
     intersection of values.

     If the visual elements representing values can be arranged to
     avoid  intersections, so much the better. Many people have no
     trouble  distinguishing which visual elements are linked to which
     axis as long as  they are both coded similarly, usually with
     colors and/or symbols. In the  special case where there is an
     underlying relationship between the two  such as the probability
     of that value occurring under some conditions, it  may help to
     mark the point(s) where this occurs.

     It may be useful to consider 'gap.plot' as an alternative.

_A_u_t_h_o_r(_s):

     Jim Lemon (thanks to Christophe Dutang for the idea of using bars
     and lines in the same plot)

_S_e_e _A_l_s_o:

     'plot'

_E_x_a_m_p_l_e_s:

      twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
       1:15,rev(60:74)+rnorm(15),xlab="Sequence",
       ylab="Ascending values",rylab="Descending values",
       main="Plot with two ordinates - points and lines")
      twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
       1:15,rev(60:74)+rnorm(15),xlab="Sequence",
       ylab="Ascending values",rylab="Descending values",
       main="Plot with two ordinates - bars on the left",
       type=c("bar","l"),lcol=3,rcol=4)
      twoord.plot(2:10,seq(3,7,by=0.5)+rnorm(9),
       1:15,rev(60:74)+rnorm(15),xlab="Sequence",
       ylab="Ascending values",rylab="Descending values",
       main="Plot with two ordinates - bars on the right",
       type=c("b","bar"),lcol=2,rcol=NA,halfwidth=0.2)

