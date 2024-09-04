        integer function indx(m, ndia)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c                          I N D X
c
c  indx is the number of matrix elements prior to column
c  'm', i.e. c(indx(m)+1) is first element of column 'm'
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        if (m.le.ndia) then 
          indx = m-1 
        else
          indx = (m*(m-1) + ndia*(1-ndia))/2
        endif
        return
        end
