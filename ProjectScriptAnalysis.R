



# import csv file
shape.dims = read.csv("shapeData.csv")

#colnames(shape.dims) = c("X","sample.name","p.area", "p.perimeter", "ellipse.major", "ellipse.minor","chull.area", "chull.perimeter", "bc.area", "br.width", "br.height" )

shape.dims$sample.name = as.character(shape.dims$sample.name)
shape.dims$p.area = as.numeric(shape.dims$p.area)
shape.dims$p.perimeter = as.numeric(shape.dims$p.perimeter)
shape.dims$ellipse.major = as.numeric(shape.dims$ellipse.major)
shape.dims$ellipse.minor = as.numeric(shape.dims$ellipse.minor)
shape.dims$chull.area = as.numeric(shape.dims$chull.area)
shape.dims$chull.perimeter = as.numeric(shape.dims$chull.perimeter)
shape.dims$bc.perimeter = as.numeric(shape.dims$bc.perimeter)
shape.dims$br.width = as.numeric(shape.dims$br.width)
shape.dims$br.height = as.numeric(shape.dims$br.height)


# for loop to calcualte all the shape parameters

shape.params = matrix(NA, nrow = 98, ncol = 7)
colnames(shape.params) = c( "form.factor", "circularity", "rectangularity", "solidity", "convexity", "axial.ratio", "regularity")

for (i in 1:98){
  # put in sample name
  #shape.params[i,1] = as.character(shape.dims$sample.name[i])
  # calculate form factor
  shape.params[i,1] = as.numeric(form.factor.fun(shape.dims$p.area[i], shape.dims$p.perimeter[i]))
  # calculate circularity
  shape.params[i,2] = as.numeric(circularity.fun(shape.dims$p.area[i], shape.dims$bc.perimeter[i]))
  # calculate rectangularity
  shape.params[i,3] = as.numeric(rectangularity.fun(shape.dims$p.area[i], shape.dims$br.height[i], shape.dims$br.width[i]))
  # calcualte solidity
  shape.params[i,4] = as.numeric(solidity.fun(shape.dims$p.area[i], shape.dims$chull.area[i]))
  # calculate convexity
  shape.params[i,5] = convexity.fun(shape.dims$chull.perimeter[i], shape.dims$p.perimeter[i])
  # calculate axial ratio
  shape.params[i,6] = as.numeric(axial.ratio.fun(shape.dims$ellipse.major[i], shape.dims$ellipse.minor[i]))
  # calculate regularity 
  shape.params[i,7] = as.numeric(shape.params[i,3])*as.numeric(shape.params[i,4])*as.numeric(shape.params[i,2])
  
}


# export shape parameters
#write.csv(shape.params, file = "shapeParmameters.csv")


# plot 
# regularity vs. aspect ratio, similar to schmith et al.
ggplot()+
  geom_point(aes(x = as.numeric(shape.params[,6]), y = as.numeric(shape.params[,7])))+
  scale_x_continuous(limits = c(.9,8))+
  labs(x = "Aspect Ratio", y = "Regularity Index", title = "Regularity Index vs Axial Ratio")

# regularity
ggplot()+
  geom_point(aes(x = c(1:98), y = as.numeric(shape.params[,7])))+
  labs(x = "Grain", y = "Regularity Index", title = "Regularity Index for all grains")


# convexity vs. solidity, similar to Liu et al
ggplot()+
  geom_point(aes(x = as.numeric(shape.params[,4]), y = as.numeric(shape.params[,5])))+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(.4,1.1))+
  labs(x = "Solidity", y = "Convexity", title = "Convexity vs Solidity")



# take the mean of all shape parameters:
# make new matrix of just numeric values
shape.params1 = apply(shape.params[,-1], 2, as.numeric)

mean.shape.params = matrix(NA, nrow = 2, ncol = 7)
colnames(mean.shape.params) = c("m.form.factor", "m.circularity", "m.rectangularity", "m.solidity", "m.convexity", "m.axial.ratio", "m.regularity")
rownames(mean.shape.params) = c("mean", "stan.dev")

for (i in 1:7){
  mean.shape.params[1,i] = mean(shape.params1[,i])
  mean.shape.params[2,i] = sd(shape.params1[,i])
}






# plot 
# regularity vs. aspect ratio, similar to schmith et al.
ggplot()+
  geom_point(aes(x = as.numeric(shape.params[,6]), y = as.numeric(shape.params[,1])))+
  #scale_x_continuous(limits = c(.9,8))+
  labs(x = "Axial Ratio", y = "Form Factor", title = "Form Factor vs Axial Ratio")





