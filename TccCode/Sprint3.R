devtools::install_github("ropenscilabs/opencv")

install.packages("imager")

library(imager)
library(opencv)

rosto1<-ocv_read("C:/Users/arnal/OneDrive/Imagens/frente.png")

contraste1<- ocv_stylize(rosto1)
ocv_write(contraste1,"C:/Users/arnal/OneDrive/Imagens/seila.jpg")
dark1<-ocv_edges(contraste1)
ocv_write(dark1,"C:/Users/arnal/OneDrive/Imagens/Face.jpg")

circulo1<-ocv_face(contraste1)

dimen1<-attr(ocv_facemask(contraste1), 'faces')

ocv_write(circulo1,"C:/Users/arnal/OneDrive/Imagens/Silueta.jpg")
ocv_write(ocv_facemask(circulo1),"C:/Users/arnal/OneDrive/Imagens/circulo.jpg")

centrox <- as.integer(dimen1[2])
centroy <- as.integer(dimen1[3])
raio <- as.integer(dimen1[1])

baixoy1<-centroy + as.integer(dimen1[1])
topoy1<-centroy - as.integer(dimen1[1])

esqLong1<-centrox - as.integer(dimen1[1])
dirLong1<-centrox + as.integer(dimen1[1])

img <- load.image("C:/Users/arnal/OneDrive/Imagens/Face.jpg")

cinza<-grayscale(img)
cor <- cinza[centroy, esqLong1]

#encontra o primeiro valor que não é preto, nesse caso é uma orelha
for (x in esqLong1:centrox) {
  cor<-color.at(img, x, centroy,z=1)
  if(cor!=0){
    esqLong1<-x
    break
  }
}
#encontra o primeiro valor que não é preto, nesse caso é uma orelha
for (x in dirLong1:centrox) {
  cor<-color.at(img, x, centroy,z=1)
  if(cor!=0){
    dirLong1<-x
    break
  }
}
#encontrar o circulo do olho
olho <- load.image("C:/Users/arnal/OneDrive/Imagens/Silueta.jpg")

azul<-color.at(olho,299, 481, z=1)

olho_vety<-c()
olho_vetx<-c()

raiox<-0
raioy<-0
for (x in esqLong1:dirLong1) {
  for(y in topoy1:baixoy1){  
    cor<-color.at(olho, x, y,z=1)
    raioy<- x-centrox
    raiox<- y-centroy
      if((cor[1]=0.1)&&(cor[2]<=azul[2])&&(cor[2]<=0.3)&&((0.65<=cor[3])&&(cor[3]<=azul[3]))){
        if(sqrt((raiox*raiox)+(raioy*raioy))<(raio-50)){
          olho_vetx<-append(olho_vetx,x)
          olho_vety<-append(olho_vety,y)
          print(cat(x,y))
        }
      }
  }
}

#encontrar o centro do olho
olho_centroy<- (min(olho_vety)+max(olho_vety))/2
olho_centrox<- (min(olho_vetx)+max(olho_vetx))/2
#encontra o topo do ciclo do olho
topo_olho_dir_y<-min(olho_vety)
topo_olho_dir_x<-olho_vetx[which.min(olho_vety)]
#encontra o fundo do ciclo do olho
baixo_olho_dir_y<-max(olho_vety)
baixo_olho_dir_x<-olho_vetx[which.max(olho_vety)]
#encontra o esquerda do ciclo do olho
esq_olho_dir_x<-min(olho_vetx)
esq_olho_dir_y<-olho_vety[which.min(olho_vetx)]
#encontra o direita do ciclo do olho
dir_olho_dir_x<-max(olho_vetx)
dir_olho_dir_y<-olho_vety[which.max(olho_vetx)]

#encontrar o x do outro olho
#o y vai ser o mesmo do primeiro
#encontra o topo do ciclo do olho
dist_topo<-topo_olho_dir_x - centrox
topo_olho_esq_x <- centrox - dist_topo
#encontra o fundo do ciclo do olho
dist_baixo<-baixo_olho_dir_x - centrox
baixo_olho_esq_x <- centrox - dist_baixo
#encontra o esquerda do ciclo do olho
dist_esq<-dir_olho_dir_x - centrox
esq_olho_esq_x <- centrox - dist_esq
#encontra o direita do ciclo do olho
dist_dir<- esq_olho_dir_x - centrox
dir_olho_esq_x<- centrox - dist_dir

#Foto de lado
rosto2<-ocv_read("C:/Users/arnal/OneDrive/Imagens/Lado.png")

contraste2<-ocv_stylize(rosto2)

dark2<-ocv_edges(contraste2)
ocv_write(dark2,"C:/Users/arnal/OneDrive/Imagens/Darkside.jpg")

ref_lado <- load.image("C:/Users/arnal/OneDrive/Imagens/Darkside.jpg")

#encontra o z da testa e do fundo da cabeça
topoz<-c()
for (x in 1:1080) {
  cor<-color.at(ref_lado,x,topoy1,z=1)
  if(cor==1){
    topoz<-append(topoz,x)
  }
}
#encontra o z do queixo e do fundo da cabeça
baixoz <-c()
for (x in 1:1080) {
  cor<-color.at(ref_lado,x,baixoy1,z=1)
  if(cor==1){
    baixoz<-append(baixoz,x)
  }
}

#encontra o z do nariz e fundo. não retorna a ponta ainda
centroz <-c()
for (x in 1:1080) {
  cor<-color.at(ref_lado,x,centroy,z=1)
  if(cor==1){
    centroz<-append(centroz,x)
  }
}

#encontra a ponta do nariz
vec_narizy<-c()
vec_narizx<-c()
for(y in 1:1080){
  for (x in 1:1080) {
    cor<-color.at(ref_lado, x, y,z=1)
    print(y)
    if(cor==1){
      vec_narizy<-append(vec_narizy,y)
      vec_narizx<-append(vec_narizx,x)
    }
  }
}

local_nariz <- which.max(vec_narizx)
nariz_z <- vec_narizx[local_nariz]
nariz_y <- vec_narizy[local_nariz]

#z do topo do olho
olho_z1 <- c()
for (x in 1:1080) {
  cor<-color.at(ref_lado,x,topo_olho_dir_y,z=1)
  if(cor==1){
    olho_z1<-append(olho_z1,x)
  }
}

#z do baixo do olho
olho_z2 <- c()
for (x in 1:1080) {
  cor<-color.at(ref_lado,x,baixo_olho_dir_y,z=1)
  if(cor==1){
    olho_z2<-append(olho_z2,x)
  }
}

#z do lado do olho
olho_z3 <- c()
for (x in 1:1080) {
  cor<-color.at(ref_lado,x,esq_olho_dir_y,z=1)
  if(cor==1){
    olho_z3<-append(olho_z3,x)
  }
}

#os 3 pontos da bochecha
boche_y<- centroy+(raio/2)
boche_x<-c()
for (x in 1:1080) {
  cor<-color.at(img,x,boche_y,z=1)
  if(cor==1){
    boche_x<-append(boche_x,x)
  }
}
boche_z<-c()
for (x in 1:1080) {
  cor<-color.at(ref_lado,x,boche_y,z=1)
  if(cor==1){
    boche_z<-append(boche_z,x)
  }
}

#encontrar a orelha

ocv_write(ocv_stylize(ocv_face(rosto2)),"C:/Users/arnal/OneDrive/Imagens/orelha.png")
orelha_img<-load.image("C:/Users/arnal/OneDrive/Imagens/orelha.png")
ore_y<-c()
ore_x<-c()
for(x in 1:1080){
  for(y in 1:1080){
    cor<-color.at(orelha_img, x, y,z=1)
    print(cor[3])
    if((cor[1]==0)&&(cor[2]==0)&&(cor[3]==1)){
      ore_y<-append(ore_y,y)
      ore_x<-append(ore_x,x)
    }
  }
}
orelha_topo_y<-min(ore_y)
orelha_topo_z<-ore_x[which.min(ore_y)]
orelha_baixo_y<-max(ore_y)
orelha_baixo_z<-ore_x[which.max(ore_y)]
orelha_esq_y<-min(ore_x)
orelha_esq_z<-ore_y[which.min(ore_x)]
orelha_dir_y<-max(ore_x)
orelha_dir_z<-ore_y[which.max(ore_x)]

#encontrar o topo da cabeça
pico_z<-0
for(y in 1080:1){
  for(z in 1080:1){
    cor<-color.at(ref_lado, z, y,z=1)
    print(y)
    if(cor == 1){
        pico_z<-z
    }
  }
}
pico_y<-0
for(y in 1080:1){
  cor<-color.at(img, centrox, y,z=1)
  if(cor != 0){
    print(y)
    pico_y<-y
  }
}

#escrever o modelo
sink("C:/Users/arnal/OneDrive/Imagens/Rosto.obj")
#queixo
cat(c('v', (centrox/100), (orelha_topo_y/-100),(min(topoz)/100),'\n'))#1
cat(c('v', (centrox/100),(baixoy1/-100),(max(baixoz)/100),'\n'))#2
#testa centro(vou ter que fazer testa lado depois)
cat(c('v', (centrox/100),(topoy1/-100),(min(topoz)/100),'\n'))#3
cat(c('v', (centrox/100),(topoy1/-100),(max(topoz)/100),'\n'))#4
#estremidades laterais
cat(c('v', (esqLong1+15/100), (centroy/-100),(median(centroz)/100),'\n'))#5
cat(c('v', (dirLong1-15/100),(centroy/-100),(median(centroz)/100),'\n'))#6
#nariz
cat(c('v', (centrox/100),(nariz_y/-100),(nariz_z/100),'\n'))#7
#centro da face de frente
cat(c('v', ((esqLong1+15)/100), (centroy/-100),(min(centroz)/100),'\n'))#8
cat(c('v', ((esqLong1+15)/100), (centroy/-100),(max(baixoz)/100),'\n'))#9
cat(c('v', ((dirLong1-15)/100), (centroy/-100),(min(centroz)/100),'\n'))#10
cat(c('v', ((dirLong1-15)/100), (centroy/-100),(max(baixoz)/100),'\n'))#11
#olho direito
cat(c('v', (topo_olho_dir_x/100),(topo_olho_dir_y/-100),(max(olho_z1))/100,'\n'))#12
cat(c('v', (baixo_olho_dir_x/100),(baixo_olho_dir_y/-100),(max(olho_z2))/100,'\n'))#13
cat(c('v', (esq_olho_dir_x/100),(esq_olho_dir_y/-100),(max(olho_z3))/100,'\n'))#14
cat(c('v', (dir_olho_dir_x/100),(dir_olho_dir_y/-100),(max(olho_z3))/100,'\n'))#15
#olho esquerdo
cat(c('v', (topo_olho_esq_x/100),(topo_olho_dir_y/-100),(max(olho_z1))/100,'\n'))#16
cat(c('v', (baixo_olho_esq_x/100),(baixo_olho_dir_y/-100),(max(olho_z2))/100,'\n'))#17
cat(c('v', (esq_olho_esq_x/100),(esq_olho_dir_y/-100),(max(olho_z3))/100,'\n'))#18
cat(c('v', (dir_olho_esq_x/100),(dir_olho_dir_y/-100),(max(olho_z3))/100,'\n'))#19
#meio dos olhos, ponte entre os dois
cat(c('v', (centrox/100),(topo_olho_dir_y/-100),(max(olho_z1))/100,'\n'))#20
cat(c('v', (centrox/100),(baixo_olho_dir_y/-100),(max(olho_z2))/100,'\n'))#21
cat(c('v', (centrox/100),(esq_olho_dir_y/-100),(max(olho_z3))/100,'\n'))#22
#bochecha
cat(c('v', min(boche_x)/100,boche_y/-100,(max(baixoz)/100),'\n'))#23
cat(c('v', baixo_olho_esq_x/100,boche_y/-100,max(boche_z)/100,'\n'))#24
cat(c('v', centrox/100,boche_y/-100,max(boche_z)/100,'\n'))#25
cat(c('v', baixo_olho_dir_x/100,boche_y/-100,max(boche_z)/100,'\n'))#26
cat(c('v', max(boche_x)/100,boche_y/-100,(max(baixoz)/100),'\n'))#27
#vertice adijacente ao olho
cat(c('v', ((dirLong1-15)/100), (esq_olho_dir_y/-100),(max(baixoz)/100),'\n'))#28
cat(c('v', ((esqLong1+15)/100), (esq_olho_dir_y/-100),(max(baixoz)/100),'\n'))#29

cat(c('v', (baixo_olho_dir_x/100),(nariz_y/-100),((nariz_z - 10)/100),'\n'))#30
cat(c('v', ((dirLong1-15)/100), (nariz_y/-100),(max(baixoz)/100),'\n'))#31

cat(c('v', (baixo_olho_esq_x/100),(nariz_y/-100),((nariz_z - 10)/100),'\n'))#32
cat(c('v', ((esqLong1+15)/100), (nariz_y/-100),(max(baixoz)/100),'\n'))#33
#vertice orelha esquerda
cat(c('v', esqLong1/100,orelha_topo_y/-100, orelha_topo_z/100,'\n'))#34
cat(c('v', esqLong1/100,orelha_baixo_y/-100, orelha_baixo_z/100,'\n'))#35
cat(c('v', esqLong1/100,orelha_esq_y/-100, orelha_esq_z/100,'\n'))#36
cat(c('v', esqLong1/100,orelha_dir_y/-100, orelha_dir_z/100,'\n'))#37
cat(c('v', (esqLong1+10)/100,orelha_topo_y/-100, orelha_topo_z/100,'\n'))#38
cat(c('v', (esqLong1+10)/100,orelha_baixo_y/-100, orelha_baixo_z/100,'\n'))#39
cat(c('v', (esqLong1+10)/100,orelha_esq_y/-100, orelha_esq_z/100,'\n'))#40
cat(c('v', (esqLong1+10)/100,orelha_dir_y/-100, orelha_dir_z/100,'\n'))#41
#vertice orelha direita
#orelha_direita = orelha direita
#orelha_topo = orelha esquerda
#orelha_baixo = orelha baixo
cat(c('v', dirLong1/100,orelha_topo_y/-100, orelha_topo_z/100,'\n'))#42
cat(c('v', dirLong1/100,orelha_baixo_y/-100, orelha_baixo_z/100,'\n'))#43
cat(c('v', dirLong1/100,orelha_esq_y/-100, orelha_esq_z/100,'\n'))#44
cat(c('v', dirLong1/100,orelha_dir_y/-100, orelha_dir_z/100,'\n'))#45
cat(c('v', (dirLong1-10)/100,orelha_topo_y/-100, orelha_topo_z/100,'\n'))#46
cat(c('v', (dirLong1-10)/100,orelha_baixo_y/-100, orelha_baixo_z/100,'\n'))#47
cat(c('v', (dirLong1-10)/100,orelha_esq_y/-100, orelha_esq_z/100,'\n'))#48
cat(c('v', (dirLong1-10)/100,orelha_dir_y/-100, orelha_dir_z/100,'\n'))#49
#em cima do olho
cat(c('v', (esqLong1-15)/100,topo_olho_dir_y/-100, (max(baixoz)/100),'\n'))#50
cat(c('v', (dirLong1+15)/100,topo_olho_dir_y/-100, (max(baixoz)/100),'\n'))#51
#testa
cat(c('v',(esqLong1-15)/100,topoy1/-100, max(baixoz)/100),'\n')#52
cat(c('v',topo_olho_esq_x/100,topoy1/-100, max(topoz)/100),'\n')#53
cat(c('v',(dirLong1-15)/100,topoy1/-100, max(baixoz)/100),'\n')#54
cat(c('v',topo_olho_dir_x/100,topoy1/-100, max(topoz)/100),'\n')#55
#ponte testa ao meio
cat(c('v',(esqLong1-15)/100,(topoy1-80)/-100,orelha_esq_z/100,'\n'))#56
cat(c('v',topo_olho_esq_x/100,(topoy1-80)/-100,orelha_esq_z/100,'\n'))#57
cat(c('v',centrox/100,(topoy1-80)/-100,orelha_esq_z/100,'\n'))#58
cat(c('v',topo_olho_dir_x/100,(topoy1-80)/-100,orelha_esq_z/100,'\n'))#59
cat(c('v',(dirLong1+15)/100,(topoy1-80)/-100,orelha_esq_z/100,'\n'))#60
#meio da cabeça
cat(c('v',(esqLong1-15)/100,pico_y/-100,pico_z/100,'\n'))#61
cat(c('v',topo_olho_esq_x/100,pico_y/-100,pico_z/100,'\n'))#62
cat(c('v',centrox/100,pico_y/-100,pico_z/100,'\n'))#63
cat(c('v',topo_olho_dir_x/100,pico_y/-100,pico_z/100,'\n'))#64
cat(c('v',(dirLong1+15)/100,pico_y/-100,pico_z/100,'\n'))#65
#em cima orelha
cat(c('v',(esqLong1-15)/100,topoy1/-100,orelha_esq_z/100,'\n'))#66
cat(c('v',(dirLong1+15)/100,topoy1/-100,orelha_esq_z/100,'\n'))#67
cat(c('v',(esqLong1-15)/100,topoy1/-100,orelha_topo_z/100,'\n'))#68
cat(c('v',(dirLong1+15)/100,topoy1/-100,orelha_topo_z/100,'\n'))#69
#Pescoço
cat(c('v', (centrox/100),(baixoy1/-100),(max(baixoz)-100)/100,'\n'))#70
cat(c('v',(esqLong1+45)/100,boche_y/-100,orelha_topo_z/100,'\n'))#71
cat(c('v',(dirLong1-45)/100,boche_y/-100,orelha_topo_z/100,'\n'))#72
#ponte meio fundo
cat(c('v',(esqLong1-15)/100,(topoy1-80)/-100,(orelha_topo_z-100)/100,'\n'))#73
cat(c('v',topo_olho_esq_x/100,(topoy1-80)/-100,(orelha_topo_z-100)/100,'\n'))#74
cat(c('v',centrox/100,(topoy1-80)/-100,(orelha_topo_z-100)/100,'\n'))#75
cat(c('v',topo_olho_dir_x/100,(topoy1-80)/-100,(orelha_topo_z-100)/100,'\n'))#76
cat(c('v',(dirLong1+15)/100,(topoy1-80)/-100,(orelha_topo_z-100)/100,'\n'))#77
#Fundo topo
cat(c('v',(esqLong1-15)/100,topoy1/-100, min(baixoz)/100),'\n')#78
cat(c('v',topo_olho_esq_x/100,topoy1/-100, min(topoz)/100),'\n')#79
cat(c('v',(dirLong1-15)/100,topoy1/-100, min(baixoz)/100),'\n')#80
cat(c('v',topo_olho_dir_x/100,topoy1/-100, min(topoz)/100),'\n')#81
#fundo baixo
cat(c('v', min(boche_x)/100,boche_y/-100,(min(baixoz)/100),'\n'))#82
cat(c('v', baixo_olho_esq_x/100,boche_y/-100,min(topoz)/100,'\n'))#83
cat(c('v', centrox/100,boche_y/-100,min(topoz)/100,'\n'))#84
cat(c('v', baixo_olho_dir_x/100,boche_y/-100,min(topoz)/100,'\n'))#85
cat(c('v', max(boche_x)/100,boche_y/-100,(min(baixoz)/100),'\n'))#86
#fundo meio
cat(c('v', min(boche_x)/100,orelha_topo_y/-100,(min(baixoz)/100),'\n'))#87
cat(c('v', baixo_olho_esq_x/100,orelha_topo_y/-100,min(topoz)/100,'\n'))#88
cat(c('v', baixo_olho_dir_x/100,orelha_topo_y/-100,min(topoz)/100,'\n'))#89
cat(c('v', max(boche_x)/100,orelha_topo_y/-100,(min(baixoz)/100),'\n'))#90
#face olho
cat('f 12 20 22 14 \n')
cat('f 13 21 22 14 \n')
cat('f 17 19 22 21 \n')
cat('f 19 22 20 16 \n')
cat('f 50 16 18 29\n')
cat('f 51 12 15 28\n')
#face nariz direito
cat('f 15 28 11 13 \n')
cat('f 30 7 21 13 \n')
cat('f 30 31 11 13 \n')
#face nariz esquerdo
cat('f 18 29 9 17 \n')
cat('f 32 7 21 17 \n')
cat('f 32 33 9 17 \n')
#face Bochechas
cat('f 7 25 24 32 \n')
cat('f 24 32 33 23 \n')
cat('f 7 25 26 30 \n')
cat('f 26 30 31 27 \n')
#face Queixo
cat('f 2 23 24 \n')
cat('f 2 25 24 \n')
cat('f 2 25 26 \n')
cat('f 2 27 26 \n')
#face orelha esquerda
cat('f 35 34 36 37\n')
cat('f 34 36 40 38\n')
cat('f 34 35 39 38\n')
cat('f 40 41 37 36\n')
cat('f 39 35 37 41\n')
#face orelha direita
cat('f 43 42 44 45\n')
cat('f 42 46 48 44\n')
cat('f 42 43 47 46\n')
cat('f 44 45 49 48\n')
cat('f 43 47 49 45\n')
#conectar a orelha esquerda ao resto
cat('f 9 41 39 33\n')
cat('f 40 41 33 29\n')
#conectar a orelha direita ao resto
cat('f 11 49 47 31\n')
cat('f 31 28 48 49\n')
#testa
cat('f 53 52 50 16\n')
cat('f 16 53 4 20\n')
cat('f 55 54 51 12\n')
cat('f 12 55 4 20\n')
#ponte topo
cat('f 52 56 57 53\n')
cat('f 57 58 4 53\n')
cat('f 4 58 59 55\n')
cat('f 59 60 54 55\n')
#topo
cat('f 56 61 62 57\n')
cat('f 57 62 63 58\n')
cat('f 58 59 64 63\n')
cat('f 65 64 59 60\n')
#orelha
cat('f 66 68 38 40\n')
cat('f 66 68 61 56\n')
cat('f 67 69 65 60\n')
cat('f 67 69 46 48\n')
cat('f 52 66 40 50\n')
cat('f 54 51 48 67\n')
cat('f 66 61 56 52\n')
cat('f 54 60 67 65\n')
cat('f 51 28 48\n')
cat('f 50 29 40\n')
#Pescoço
cat('f 70 71 23 2\n')
cat('f 70 72 27 2\n')
cat('f 33 23 71 39\n')
cat('f 31 27 72 47\n')
#fundo ponte
cat('f 61 73 74 62\n')
cat('f 62 74 75 63\n')
cat('f 63 75 76 64\n')
cat('f 64 76 77 65\n')
#fundo topo
cat('f 73 78 79 74\n')
cat('f 79 74 75 3\n')
cat('f 3 75 76 81\n')
cat('f 81 76 77 80\n')
cat('f 65 77 80 69\n')
cat('f 61 73 78 68\n')
#fundo boche
cat('f 87 78 79 88\n')
cat('f 88 79 3 1\n')
cat('f 81 3 1 89\n')
cat('f 80 90 89 81\n')
cat('f 68 38 87 78\n')
cat('f 69 46 90 80\n')
#fundo baixo
cat('f 82 87 88 83\n')
cat('f 83 88 1 84\n')
cat('f 84 1 89 85\n')
cat('f 85 89 90 86\n')
cat('f 87 38 39 82\n')
cat('f 90 46 47 86')
sink()

#parece que ocorreu um erro ao detectra os pontos da orelha
# conectar baixo e topo, esquerda e direita

#limpar o ambiente(vai fazer startar mais facil)
rm(list=ls())

