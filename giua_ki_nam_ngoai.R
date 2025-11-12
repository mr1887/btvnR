# de giua ki nam ngoai 
#Cau 1 
# a
thoi_gian = c(40,42,44,46,48,50)
so_nhan_vien = c(25,50,60,110,90,15)
du_lieu1 = rep(thoi_gian,so_nhan_vien)
du_lieu1
#b
kiem_tra =function(du_lieu, pO){
  count = 0
  for( i in 1:length(du_lieu)){
    if(du_lieu[i] > 46){
      count= count + 1
    }
  }
  n = length(du_lieu)
  m = count
  f = m/n
  f1 = f - 1.96*sqrt(f*(1-f))/ sqrt(n)
  f2 = f + 1.96*sqrt(f*(1-f))/ sqrt(n)
  if (f > f1 & f < f2){
    cat("p_0=",pO,"vua nhap dai dien cho ti le nhan vien di lam nhieu hon 46 phut")
  }else{
    cat("p_0=",pO,"vua nhap khong dai dien cho ti le nhan vien di lam nhieu hon 46 phut")  }
}
kiem_tra(du_lieu1,0.28)
kiem_tra(du_lieu1,0.33)
# cau 2
#a
thoi_gian_lam = c(17,16,20,24,22,15,21,15,17,22)
min(thoi_gian_lam) # timf gia tri nho nhat
max(thoi_gian_lam) # tim gia tri lon nhat
mean(thoi_gian_lam) # tim gttb
#b
thoi_gian_lam[thoi_gian_lam == 24] = 18
#c
so_ngay_di_hon_17 = sum(thoi_gian_lam > 17)
so_ngay = sum(thoi_gian_lam > 17 & thoi_gian_lam<= 20)
ti_le = so_ngay/ so_ngay_di_hon_17
ti_le