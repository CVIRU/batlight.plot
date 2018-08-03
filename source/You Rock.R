library(ggforce)
geomp <- ggplot()
geom_magnify <- function(obj,x,y,x0,y0,r,X,ymin,ymax,h) {
  
  ## Transformation Variables (from the origin) ##
  x1 = x - x0
  y1 = y - y0
  
  ## Tangent Points 1 & 2 (At the origin) ##
  a1 = (2*x1*r^2 + sqrt(4*x1^2*r^4-4*(x1^2+y1^2)
                        *(r^4-r^2*y1^2)))/(2*(x1^2+y1^2))
  b1 = (r^2-a1*x1)/y1
  
  a2 = (2*x1*r^2 - sqrt(4*x1^2*r^4-4*(x1^2+y1^2)
                        *(r^4-r^2*y1^2)))/(2*(x1^2+y1^2))
  b2 = (r^2-a2*x1)/y1
  
  c1 = x0 - sqrt(r^2 - h^2)
  c2 = x0 + sqrt(r^2 - h^2)
  
  dt1<-data.frame( x = x, y = y, x1 = x1, x0 = x0, y1 = y1, y0 = y0,
                   r = r, a1 = a1, a2 = a2, b1 = b1, b2 = b2, X=X, 
                   ymin=ymin, ymax=ymax, h=h, c1=c1, c2=c2)
  
  ### Starting Point ###
  obj + layer(geom="point",
              data =dt1, 
              mapping = aes(x = x, y = y), 
              stat = "identity", 
              position = "identity") +
    
    ### Circle ###
    geom_circle(data=dt1,
                mapping = aes(x0=x0, y0=y0, r=r),
                linetype=1)  + 
    
    ### Left Segment ###
    geom_segment(data = dt1, 
          mapping = aes(x=x,y=y,xend=a1+x0,yend=b1+y0),
          linetype=4) +
    
    ### Right Segment ###
    geom_segment(data = dt1, 
          mapping = aes(x=x,y=y,xend=a2+x0,yend=b2+y0), 
          linetype=4) +
    
    ### Horizontal Segment ###
    geom_segment(data = dt1, 
          mapping = aes(x=x0-r,y=y0,xend=x0+r,yend=y0), 
          linetype = 3,
          color="red") +
    
    ### Vertical Segment ###
    geom_segment(data = dt1, 
          mapping = aes(x=x0,y=y0-r,xend=x0,yend=y0+r), 
          linetype = 3,
          color="red")  +   
    
  geom_errorbar(data=dt1,
                mapping = aes(x=X+x0,ymin=ymin+y0,ymax=ymax+y0),
                width=0.2,
                color="blue") + 
    geom_segment(data=dt1,
                 mapping = aes(x=c1,y=y0+h,xend=c2,yend=y0+h),
                 linetype=5)
  


}

geom_magnify(geomp,x = 3, y = 1, x0 = 10, y0 = -6, r=1, X=.5, ymin=0.5,ymax=0.6,h=0.2)





