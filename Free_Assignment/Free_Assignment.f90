
! 名前： Ratdotl

! タイトル：
! Inspirintegral traning
 

! プログラムの説明:
! 定積分をするにおいて、勘で大体このくらいと予測する力を養うトレーニング。計算力のトレーニングにもなる。
! ある関数に関して、精度の低い近似積分と解析解の差をとって、その差を解析解の±範囲にして、
! その範囲でランダムに4つ値を取り出してそれを選択肢とし、解析解に1番近いものを正解とするゲーム。
! 計算、予測の難易度に応じて得点が変わり、Lv1~3で1~3点とし、10点を超えると勝ち、-10点より小さく
! なると負けとなる。
! 間違えると-1点、1番解析解から離れた選択肢を選ぶとさらに-1点で計-２点となる。
! Lv1は50%、Lv2は30%、Lv3は20%の確率で出現する。
 

! （ゲームの流れの例）
! 1(プログラム実行後):ターミナルに以下の文章を表示。
! ////////////////////////
! Inspirintegral traningを開始します。よければs入力してください。ゲームの途中でやり直したい場合
! はrを入力してください。
! ////////////////////////
 

! 2(sを入力後):ターミナルに以下の文章を表示。（関数=x、積分範囲=[0,1]、精度の低い近似積分（台形
! 公式）=0.7、解析解=0.5、±範囲=0.2）
! ////////////////////////
! （Lv1問題）関数=x、積分範囲=[0,1]です。以下４つの値のうち、解析解に一番近いものの番号を入力して
! ください。
! ①0.7 ②0.5 ③0.4 ④0.6
! ////////////////////////
 

! 3(1を入力後):ターミナルに以下の文章を表示。
! ////////////////////////
! 残念。正解は②0.5でした。-2点です。
! 現在の得点は-2点です。
! 次の問題に行く場合はsを入力してください。
! ////////////////////////
 

! 4(2,3の操作を繰り返して10または-10超えた後):ターミナルに以下の文章を表示。
! ////////////////////////
! 得点が10点を超えました。勝ちです。おめでとうございます!。
 

! 得点が-10点になりました。負けです。また挑戦してください。
! ////////////////////////
 

! 工夫した点:
! ・難易度を分けるのに様々な要素を考えて工夫した。近似積分の刻み値と種類、範囲である。
! 　近似積分の種類に関しては、長方形公式と台形公式を用いた。
! ・コードが重複して無駄に要領が重くならないように工夫した。
! ・±範囲でランダムに値を取り出すことで、より小さい値を選ぶ方がよいなどという偏りを無くした。
! ・±範囲が小さくなりすぎる場合があるので、最低の±範囲を設定した。



! 苦労した点:
! ・character型なのか数字型の変数なのかを意識せず代入してしまうことがあり、忘れず型を覚えておく
! 　のに苦労した。
! ・if文を大量に用いる方法しか思いつかなかったため、状況整理に苦労した。
! ・解析解の積分を２０個程度用意するのに苦労した。

program Free_Assignment
implicit none
character(len=10) :: g 
real(8) :: sumrec, sumtra, sumsol, f, sol, random_lv, range, lower, correct, fun_num, bad=0,t
real(8), allocatable ::  correct_can(:),  bad_can(:), randomSol(:)
integer(8) :: m,i, sel, point, allpoint, random_fun, xmin, xmax, correct_sel, bad_sel, nmax
allocate(correct_can(5),  bad_can(5), randomSol(5))

do 
m=0 !mの初期化
write(*,*) "Start Inspirintegral traning. Enter 1 if you like. Enter 2 to exit the program."
read(5,*) m
point=0 !pointの初期化
allpoint=0 !allpointの初期化

if(m == 2) exit !プログラムの終了

if (m == 1) then

! トレーニングスタート
do 
if(m == 2) exit

if (m == 1) then
call random_number(random_lv) !levelの決定のための乱数

!lv1
if (0 <= random_lv .and. random_lv < 0.5) then
point = 1
nmax = 8
call random_number(t)
random_fun = Int(t*10) !関数の決定のための乱数
if (random_fun == 0) g = 'x'
if (random_fun == 1) g = 'x^2'
if (random_fun == 2) g = 'x^3'
if (random_fun == 3) g = 'x/6'
if (random_fun == 4) g = 'x/8'
if (random_fun == 5) g = '2x^2'
if (random_fun == 6) g = 'x/2'
if (random_fun == 7) g = 'x/4'
if (random_fun == 8) g = '8x^2'
if (random_fun == 9) g = 'x^2/2'

xmin = 0
call random_number(t) !xmaxの決定のための乱数
xmax = Int(1+t*4)
fun_num = 1.D0 + dble(random_fun)/10.D0 !関数の番号
call rec(xmin,xmax,nmax,fun_num,sumrec) !長方形公式の計算

end if

!lv2
if (0.5 <= random_lv .and. random_lv < 0.8) then
point = 2
nmax = 16
call random_number(t)
random_fun = Int(t*10) !関数の決定のための乱数
if (random_fun == 0) g = 'x'
if (random_fun == 1) g = 'x^2'
if (random_fun == 2) g = 'x^3'
if (random_fun == 3) g = 'x/6'
if (random_fun == 4) g = 'x/8'
if (random_fun == 5) g = '2x^2'
if (random_fun == 6) g = 'x/2'
if (random_fun == 7) g = 'x/4'
if (random_fun == 8) g = '8x^2'
if (random_fun == 9) g = 'x^2/2'
call random_number(t) !xminの決定のための乱数
xmin = Int(3 + t*2)
call random_number(t)
xmax = Int(6 + t*4)
fun_num = 1.D0 + dble(random_fun)/10.D0
call rec(xmin,xmax,nmax,fun_num,sumrec)

end if

!lv3
if (0.8 <= random_lv .and. random_lv <= 1.0) then
point = 3
nmax = 16
call random_number(t)
random_fun = Int(t*10) !関数の決定のための乱数
if (random_fun == 0) g = 'x^5'
if (random_fun == 1) g = 'x^6'
if (random_fun == 2) g = 'x^7'
if (random_fun == 3) g = 'x^4+2x^3'
if (random_fun == 4) g = '4x^8'
if (random_fun == 5) g = '5x^5'
if (random_fun == 6) g = 'x^5-2x^3'
if (random_fun == 7) g = 'x^4-x^5'
if (random_fun == 8) g = '8x^5'
if (random_fun == 9) g = 'x^5/2'
call random_number(t)
xmin = Int(3 + t*2)
call random_number(t)
xmax = Int(6 + t*4)
fun_num = 3.D0 + dble(random_fun)/10.D0
call tra(xmin,xmax,nmax,fun_num,sumtra)

end if

sumsol = sol(xmin,xmax,fun_num)

range = 2 + abs(sumrec - sumsol) !±範囲
lower = sumsol - range !選択肢の値の最小値
correct = 100000 !correctの初期化。correctはより小さいものが代入されていくため、大きな値を初期値としている。
bad = 0 !badの初期化。badはより大きいものが代入されていくため、0を初期値としている。

! 選択肢の作成と正解、ひどい不正解の決定
do i = 1, 4
call random_number(t) !選択肢作成のための乱数
randomSol(i) = lower + 2.D0 * range * t !選択肢の作成
correct_can(i) = abs(randomSol(i) - sumsol) !正解との差の絶対値
bad_can(i) = abs(randomSol(i) - sumsol) !ひどい不正解の決定のために便宜上作成

! 正解の決定
if (correct_can(i)<correct) then
correct = correct_can(i)
correct_sel = i
end if
! ひどい不正解の決定
if (bad_can(i)>bad) then
bad = bad_can(i)
bad_sel = i
end if

end do

! 問題の出力
write(*,'("(Lv", I0, "problem) function =", (a))')point,g
write(*,'("integral range = [", I0, ",", I0, "]. ")')xmin, xmax
write(*,'("Enter the number of the closest of the following four values to the analytical solution.")')
write(*,'("(1)",F16.5, " (2)",F16.5, " (3)",F16.5, " (4)",F16.5)')randomSol(1), randomSol(2), randomSol(3), randomSol(4)
! 確認用
! write(*,'("(1)",F16.5, " (2)",F16.5, " (3)",F16.5, " (4)",F16.5)')correct_can(1), correct_can(2), correct_can(3), correct_can(4)
! write(*,'("sumsol",F16.5,"  sumrec",F16.5,"  sumtra",F16.5)')sumsol,sumrec,sumtra
! sumsol=0.D0
! sumrec=0.D0
! sumtra=0.D0

! 選択結果の入力
read(5,*) sel

! 点数の計算（正誤判定）
if (sel == correct_sel) then !正解の場合
allpoint = allpoint + point
write(*,*)"Great!"
write(*,'("The correct answer was (", I0," )", F16.5,".   You get ", I0," points.")')correct_sel,randomSol(correct_sel),point
else if (sel == bad_sel) then !ひどい不正解の場合
point = -2
allpoint = allpoint - 2
write(*,*)"Bad!"
write(*,'("The correct answer was (", I0," )", F16.5,".   You get ", I0," points.")')correct_sel,randomSol(correct_sel),point
else !ただの不正解の場合
point = -1
allpoint = allpoint - 1
write(*,*)"Oh my god!"
write(*,'("The correct answer was (", I0," )", F16.5,".   You get ", I0," points.")')correct_sel,randomSol(correct_sel),point
end if

! 現在の点数、継続かスタートに戻るかの判定と結果の出力
write(*,'("The current score is ", I3," points.")')allpoint

if (allpoint > 10) then
write(*,'("The score exceeded 10 points. You have won. Congratulation!")')
exit !スタートに戻る

else if (allpoint < -10) then
write(*,'("The score is smaller than -10 points. You lost. Please try again.")')
exit !スタートに戻る

else 
write(*,'("Enter 1 if you want to go to the next problem. Enter 2 if you want to start over in the middle of the game.")')
read(5,*) m
end if

end if
end do
end if
end do



end program Free_Assignment

! Integral function
function f(x,fun_num) result(y)
implicit none
real(8), intent(IN) :: x,fun_num
real(8):: y
if (fun_num == 1.D0) y = x
if (fun_num == 1.1D0) y = x**2.D0
if (fun_num == 1.2D0) y = x**3.D0
if (fun_num == 1.3D0) y = x/6.D0
if (fun_num == 1.4D0) y = x/8.D0
if (fun_num == 1.5D0) y = 2.D0*x**2.D0
if (fun_num == 1.6D0) y = x/2.D0
if (fun_num == 1.7D0) y = x/4.D0
if (fun_num == 1.8D0) y = 8.D0*x**2.D0
if (fun_num == 1.9D0) y = x**2.D0/2.D0
if (fun_num == 3.D00) y = x**5.D0
if (fun_num == 3.1D0) y = x**6.D0
if (fun_num == 3.2D0) y = x**7.D0
if (fun_num == 3.3D0) y = x**4.D0+2.D0*x**3.D0
if (fun_num == 3.4D0) y = 4.D0*x**8.D0
if (fun_num == 3.5D0) y = 5.D0*x**5.D0
if (fun_num == 3.6D0) y = x**5.D0-2.D0*x**3.D0
if (fun_num == 3.7D0) y = x**4.D0-x**5.D0
if (fun_num == 3.8D0) y = 8.D0*x**5.D0
if (fun_num == 3.9D0) y = x**5.D0/2.D0

return
end function f

! analytical solution
function sol(xmin,xmax,fun_num) result(y)
implicit none
integer(8), intent(IN) :: xmin,xmax
real(8), intent(IN) :: fun_num
real(8):: y
if (fun_num == 1.D0) y = 1.D0/2.D0*(dble(xmax)**2.D0 - dble(xmin)**2.D0)
if (fun_num == 1.1D0) y = 1.D0/3.D0*(dble(xmax)**3.D0 - dble(xmin)**3.D0)
if (fun_num == 1.2D0) y = 1.D0/4.D0*(dble(xmax)**4.D0 - dble(xmin)**4.D0)
if (fun_num == 1.3D0) y = 1.D0/12.D0*(dble(xmax)**2.D0 - dble(xmin)**2.D0)
if (fun_num == 1.4D0) y = 1.D0/18.D0*(dble(xmax)**2.D0 - dble(xmin)**2.D0)
if (fun_num == 1.5D0) y = 2.D0/3.D0*(dble(xmax)**3.D0 - dble(xmin)**3.D0)
if (fun_num == 1.6D0) y = 1.D0/4.D0*(dble(xmax)**2.D0 - dble(xmin)**2.D0)
if (fun_num == 1.7D0) y = 1.D0/8.D0*(dble(xmax)**2.D0 - dble(xmin)**2.D0)
if (fun_num == 1.8D0) y = 8.D0/3.D0*(dble(xmax)**3.D0 - dble(xmin)**3.D0)
if (fun_num == 1.9D0) y = 1.D0/6.D0*(dble(xmax)**3.D0 - dble(xmin)**3.D0)
if (fun_num == 3.D0) y = 1.D0/6.D0*(dble(xmax)**6.D0 - dble(xmin)**6.D0)!
if (fun_num == 3.1D0) y = 1.D0/7.D0*(dble(xmax)**7.D0 - dble(xmin)**7.D0)
if (fun_num == 3.2D0) y = 1.D0/8.D0*(dble(xmax)**8.D0 - dble(xmin)**8.D0)
if (fun_num == 3.3D0) y = 1.D0/5.D0*(dble(xmax)**5.D0 - dble(xmin)**5.D0)+2.D0/4.D0*(dble(xmax)**4.D0 - dble(xmin)**4.D0)
if (fun_num == 3.4D0) y = 4.D0/9.D0*(dble(xmax)**9.D0 - dble(xmin)**9.D0)
if (fun_num == 3.5D0) y = 5.D0/6.D0*(dble(xmax)**6.D0 - dble(xmin)**6.D0)
if (fun_num == 3.6D0) y = 1.D0/6.D0*(dble(xmax)**6.D0 - dble(xmin)**6.D0)-2.D0/4.D0*(dble(xmax)**4.D0 - dble(xmin)**4.D0)
if (fun_num == 3.7D0) y = 1.D0/5.D0*(dble(xmax)**5.D0 - dble(xmin)**5.D0)-1.D0/6.D0*(dble(xmax)**6.D0 - dble(xmin)**6.D0)
if (fun_num == 3.8D0) y = 8.D0/9.D0*(dble(xmax)**9.D0 - dble(xmin)**9.D0)
if (fun_num == 3.9D0) y = 1.D0/12.D0*(dble(xmax)**6.D0 - dble(xmin)**6.D0)
return
end function sol

! rectangular formula
subroutine rec(xmin,xmax,nmax,fun_num,sumrec)
implicit none
real(8), intent(IN) :: fun_num
real(8), intent(OUT) :: sumrec 
integer(8), intent(IN) :: nmax,xmin,xmax
real(8) :: dx, x, f
integer(8) :: ix
dx = (dble(xmax)-dble(xmin))/dble(nmax)
sumrec = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+dble(xmin)
sumrec = sumrec +f(x,fun_num)*dx
end do
return
end subroutine rec

! trapezoidal formula
subroutine tra(xmin,xmax,nmax,fun_num,sumtra)
implicit none
real(8), intent(IN) :: fun_num
real(8), intent(OUT) :: sumtra
integer(8), intent(IN) :: xmin,xmax,nmax 
real(8) :: dx, x, facx, f
integer(8) :: ix
dx = (dble(xmax)-dble(xmin))/dble(nmax)
sumtra = 0.D0
do ix = 0, nmax
x = dble(ix)*dx+dble(xmin)
facx = 1.D0
if(ix==0.or.ix==nmax)facx=0.5D0
sumtra = sumtra + f(x,fun_num)*dx*facx
end do
return
end subroutine tra