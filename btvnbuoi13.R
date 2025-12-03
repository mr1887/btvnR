# Câu 1 
install.packages("Stat2Data")

library(Stat2Data)

#1
data("BirdNest")
nest = BirdNest
nest = na.omit(nest)
nest
colnames(nest)
# 2
unique(nest$Nesttype)
nest_types_table = table(nest$Nesttype)
print(nest_type_table)
#3
pie(nest_types_table,
    col = rainbow(length(nest_types_table)),
    )
#4
print(table(nest$Location))
barplot(table(nest$Location),
        col = c("red","blue","green","brown","purple","yellow","pink","grey","gold"),
        xlab = "vi trí tổ",
        ylab = "Số lượng chim",
        main = "số lượng chim theo vị trí tổ"
        
        )
# chú thích
legend("topleft",legend = unique(nest$Location),
       fill = c("red","blue","green","brown","purple","yellow","pink","grey","gold"))
#5
boxplot(nest$No.eggs~nest$Nesttype,
        col = c("red","blue","green","brown","purple","yellow","pink"),
        xlab = "Loại tổ",
        ylab = " số lượng trứng",
        main = " số lượng trứng trong tổ chim giữa các loại tổ ",
        )
#6
hist(nest$No.eggs,
     col = "grey",
     freq = FALSE)
lines(density(nest$No.eggs),col = "red")
# bài 2 
library(Stat2Data)
data("BlueJays")
bird = BlueJays
bird = na.omit(bird)
bird
#2
print(table(bird$KnownSex))
pie(table(bird$KnownSex),
    col = c("blue","pink"))
legend("topleft",
       legend= c("Đực","Cái"),
       fill = c("blue","pink"))
#3
bird$phan_loai = c()
bird$phan_loai[bird$Mass < 60]="Nhỏ"
bird$phan_loai[bird$Mass >= 60 & bird$Mass < 70]="Bình thường"
bird$phan_loai[bird$Mass >= 70 & bird$Mass < 80]="Lớn"
bird$phan_loai[bird$Mass >= 80]="Rất lớn"
bird$phan_loai
barplot(table(bird$phan_loai),
        col = c("red","green","yellow","blue"),
        xlab= "loại kích cỡ",
        ylab = " số lượng",
        main ="Số lượng theo từng loại kích cỡ"
        )
legend("topright",
       legend= c("Bình thường:[60,70)","Lớn:[70,80)","Nhỏ: <60","Rất lớn :>80"),
       fill = c("red","green","yellow","blue"))
#4
hist(bird$BillLength,
     col = "grey",
     freq= FALSE)
lines(density(bird$BillLength),col="red")

# Bài 3
#1
data("ChickWeight")
chicken = ChickWeight
chicken
is.na(chicken)
#2
#tính trọng lượng trung bình của cac con gà có chế độ ăn 2 theo tùng thời điểm
ga2 = subset(chicken, chicken$Diet == 2)
ga2
trong_luong = c()
for (i in unique(ga2$Time)){
  mean = mean(ga2$weight[ga2$Time == i])
  trong_luong = c(trong_luong,mean)
}
Thoi_diem = unique(ga2$Time)

trong_luong
barplot(trong_luong,Thoi_diem,
        col = rainbow(length(table)),
        xlab= "Thời điểm",
        ylab = "trọng lượng trung bình",
        main = "biểu đồ cột thể hiện trọng lượng trung bình của các
con gà này theo từng thời điểm.")
#3
ga20 = subset(chicken, chicken$Chick == 20)
ga22 = subset(chicken, chicken$Chick == 22)
ga23 = subset(chicken, chicken$Chick == 23)
y_range = range(c(ga20$weight,ga22$weight,ga23$weight))
x_range = range(ga20$Time)

plot(ga20$Time,
     ga20$weight,
     type = "b",
     col = "red",
     xlim = x_range,
     ylim = y_range,
     main = "sự thay đổi trọng lượng của các loại gà 20,22,23",
     xlab = "Thời điểm",
     ylab = "Trọng lượng")
lines(ga22$Time,
      ga22$weight,
      type="b",
      col="blue")
lines(ga23$Time,
      ga23$weight,
      type="b",
      col="green")
legend(
  "topleft", 
  legend = c("ID 20", "ID 22", "ID 23"),
  fill = c("red", "blue", "green"))
#4
ga3 = subset(chicken,chicken$weight == 3)
ga3
plot(ga3$Time,ga3$weight)
abline(lm(ga3$weight~ga3$Time),col = "red")
#5
ga0 = subset(chicken,chicken$Time==0)
ga0
hist(ga0$weight,
     col ="grey",
     freq = FALSE)
lines(density(ga0$weight),col = "red")
# Bài 4
#1
data("iris")
flower = iris
flower
if(sum(is.na(flower)) >0){
  na.omit(flower)
}
#2
Q1 = quantile(flower$Petal.Width,0.25)
Q1
Q3 = quantile(flower$Petal.Width,0.75)
Q3 
IQR = Q3 - Q1 
# Bài 7 
#1 
data("mtcars")
mtcars = mtcars
barplot(table(mtcars$cyl),
        col = c("red","yellow","green"),
        xlab = "số xi lanh",
        ylab = " số lượng xe",
        main=" số lượng xe theo xi lanh")
#2
gttb = c()
for (i in unique(mtcars$gear)){
  mean = mean(mtcars$mpg[mtcars$gear == i])
  gttb = c(gttb,mean)
}
gttb
So_banh = unique(mtcars$gear)
barplot(
  height = gttb, 
  names.arg = So_banh, 
  col = c("red", "yellow", "green"),
  main = "Giá trị trung bình của mức tiêu thụ nhiên liệu theo số bánh răng", # Sửa nhãn trục X
  xlab = "Số bánh răng (Gear)", 
  ylab = "Trung bình mức tiêu thụ nhiên liệu (MPG)"
)
#3
hist(mtcars$hp,col = "grey",freq = FALSE)
lines(density(mtcars$hp),col = "red")
#5
boxplot(mtcars$mpg~mtcars$cyl,
        col = c("red", "yellow", "green"))

