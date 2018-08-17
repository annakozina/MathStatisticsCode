
x <- c(559, 600, 650, 600, 500, 650, 450, 500)
mean_y <- mean(y)

y <- c(420, 380, 350, 400, 440, 380, 450, 420)
mean_x <- mean(x)



sd_x <- sd(x)
#0.7440238

sd_y <- sd(y)
#33.80617


frame <- data.frame(x, y)
frame
func1 <- function(vector){
  vector_minus_mean <- vector - mean_x
  return(vector_minus_mean)
}

func2 <- function(vector){
  vector_minus_mean <- vector - mean_y
  return(vector_minus_mean)
}

x_min_mean <- func1(x) 
y_min_mean <- func2(y)

frame$x_min_mean <- data.frame(x_min_mean)
frame$y_min_mean <- data.frame(y_min_mean)

multiplied <- x_min_mean*y_min_mean
frame$multiplied <- multiplied 
frame

sum1 <- sum(multiplied)  

colnames(frame)

x_min_mean_квадрат <- x_min_mean*x_min_mean
x_min_mean_квадрат_s <-sum(x_min_mean_квадрат)

y_min_mean_квадрат <- y_min_mean*y_min_mean
y_min_mean_квадрат_s <-sum(y_min_mean_квадрат)

frame$x_min_mean_квадрат <- x_min_mean_квадрат
frame$y_min_mean_квадрат <- y_min_mean_квадрат
frame

xxx <- sqrt(sum(x_min_mean_квадрат))
yyy <- sqrt(sum(y_min_mean_квадрат))

# оценка стандартного оклонения является смещенной и для этого мы используем поправочный коэффициент  корень(n/n-1)
проводим смещение так как выборка н слишком мала и 

colnames(frame)

# нормирование 
 
sm <- x_min_mean/(sd_x*1.069)
yy <- y_min_mean/(sd_y*1.069)

sum(sm)
sum(yy)

sd_x*1.069
sd_y*1.069

frame

func3 <- function(vector){
  vector_minus_mean <- vector - mean_x
  return(vector_minus_mean)
}

func4 <- function(vector){
  vector_minus_mean <- vector - mean_y
  return(vector_minus_mean)
}

correlation <- sum1/(xxx*yyy)

# построим график корреляции

library(ggplot2)
ggplot(frame, aes(x = frame$x, y = frame$y))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y="ПРОДАЖИ", x="цена")+
  ggtitle("Продажа стереосистем в долларах США")+
theme(plot.title = element_text(hjust = 0.5))  

##############################################
##############################################
# при простой линейной регресси существует всегда 2 коэффициента
# 1 коэффициент B1
# B1 показывает на сколько уменьшиться y при увеличении х

b1 <- correlation*(sd_y/sd_x)

mean(y)
mean(x)

bo <- mean(y)-b1
#  уровнение для нахождения количества продаж (увеличение на 100 долларов)
# y = - 0,42x + 405,42

##############################################
##############################################
