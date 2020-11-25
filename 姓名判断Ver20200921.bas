'Version221:Date2020.06.22:Ver20191227より改変
'Version223:Date2020.06.23:Ver20191227より改変
'Version224:Date2020.06.24:Ver20191227より改変
'Version225:Date2020.06.25:Ver20191227より改変
'Version226:Date2020.06.26:Ver20191227より改変
'Version227:Date2020.06.27:Ver20191227より改変
'Version228:Date2020.06.29:Ver20191227より改変
'Version229:Date2020.06.30:ver20191227より改変
'Version230:Date2020.07.02:Ver20191227より改変
'Version231:Date2020.07.03:Ver20191227より改変
'Version232:Date2020.07.04:Ver20191227より改変
'Version233:Date2020.07.12:Ver20191227より改変
'Version234:Date2020.07.13:Ver20191227より改変
'Version235:Date2020.07.15:Ver20191227より改変
'Version235_02:Date:2020.11.02:Version235表示不具合修正
'Version23601:Date:2020.11.04:Ver202009より改変
'Version23602:Date:2020.11.06:Ver202009より改変
'Version23604:Date:2020.11.14
'初期設定項目
'メモリー定義 文字データー3倍 ,数値データー2倍
clear 3.0*4000,2.0*100000
counts=0
dim  buf_namearray$(10),name_array$(10),buf_namearray2$(10),name_array2$(10),buf_lines$(3*10),buffLines$(20),count$(10),count1$(10),count2$(10)
'性別の選択用変数
dim sex_type$(2)
'相性占い　設定項目 ここから
count=0:N=0:n=0
dim buffer_Aisyou_type$(10,10)
dim buffer_Kaimei_data_name$(10)
'総数を出す変数 改名チェック変数
'dim buffer_total$
'1.理解し合える最良のカップル
dim buf_good_couple1(20)
dim buf_good_couple2(20)
'2.互いに自然体でつきあえるカップル
dim buf_natural_couple1(20)
dim buf_natural_couple2(20)
'3.男性にとって居心地の良い相性
dim buf_good_for_man1(20)
dim buf_good_for_man2(20)
'4.女性にとって居心地の良い相性
dim buf_good_for_woman1(20)
dim buf_good_for_woman2(20)
'5.恋愛経験を重ねた後なら愛を育める
dim short_of_experience1(20)
dim short_of_experience2(20)
'6.結婚への発展が困難なカップル
dim buf_difficult_for_couple1(20)
dim buf_difficult_for_couple2(20)
'7.愛し方にズレが出てくる二人
dim buf_difference_of_love1(20)
dim buf_difference_of_love2(20)
'相性占い　設定項目　ここまで
'8.互いの価値観が噛み合わない相性 ここから
dim buf_difference_of_KachiKan1(20)
dim buf_difference_of_KachiKan2(20)
'8.互いに価値観が噛み合わない相性 ここまで
'相性診断　相性パターン結果　ここから
dim Result_Aisyou_type$(8)
'相性診断 相性パターン結果 ここまで
'2019/04/07 姓名判断アプリ 作成開始
'姓名判断　 名前の総数での吉凶を調べる
buf_count=0:buffer_count=0:count=0:buffer=0
'合計文字数
totalmoji=0
dim buf_Input_data$(10),buf_Input_data2$(10)
dim bufer_name$(10),bufer_name2$(10)
dim buf_Input_name$(10),buf_Input_name2$(10)
'定数文字1画〜24画
Moji_1=29:Moji_2=79:Moji_3=100:Moji_4=113:Moji_5=135:Moji_6=177:Moji_7=191:Moji_8=287:Moji_9=275:Moji_10=291:Moji_11=297:Moji_12=196:Moji_13=230:Moji_14=156:Moji_15=151:Moji_16=107:Moji_17=63:Moji_18=25:Moji_19=17:Moji_20=13:Moji_21=6:Moji_22=4:Moji_23=3:Moji_24=3
Moji_total=Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21+Moji_22+Moji_23+Moji_24
'1画
Moji_Min_1=1:Moji_Max_1=Moji_1+1:
'2画
Moji_Min_2=(Moji_1)+2:Moji_Max_2=Moji_1+Moji_2+2
'3画
Moji_Min_3 =(Moji_1) + (Moji_2) + 3:Moji_Max_3 =Moji_1 + Moji_2 + Moji_3+3
'4画
Moji_Min_4 = (Moji_3) + Moji_2 + Moji_1 + 4:Moji_Max_4=Moji_1+Moji_2 + Moji_3 + Moji_4 + 4
'5画
Moji_Min_5 = (Moji_1 + Moji_2 + Moji_3 + Moji_4) + 5:Moji_Max_5 = (Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5)+5
'6画
Moji_Min_6 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5)+6:Moji_Max_6=(Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6)+6
'7画
Moji_Min_7 = (Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6) + 7:Moji_Max_7 = (Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6 + Moji_7) + 7
'8画
Moji_Min_8 = Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+8:Moji_Max_8=(Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6 + Moji_7 + Moji_8)+8
'9画
Moji_Min_9 = Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6 + Moji_7 + Moji_8 + 9:Moji_Max_9 = Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6 + Moji_7 + Moji_8 + Moji_9 + 9
'10画
Moji_Min_10 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9)+10:Moji_Max_10=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10)+10
'11画
Moji_Min_11 = (Moji_1+Moji_2+Moji_3+Moji_4 + Moji_5 + Moji_6 + Moji_7 + Moji_8 + Moji_9 + Moji_10)+11:Moji_Max_11=(Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6 + Moji_7 + Moji_8 + Moji_9 + Moji_10 + Moji_11)+11
'12画
Moji_Min_12 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11)+12:Moji_Max_12=Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+12
'13画
Moji_Min_13=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12)+13:Moji_Max_13=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13)+13
'14画
Moji_Min_14 =(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13)+14:Moji_Max_14=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14)+14
'15画
Moji_Min_15 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14)+15:Moji_Max_15=(Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6+ Moji_7 + Moji_8 + Moji_9+ Moji_10 + Moji_11 + Moji_12+Moji_13+Moji_14+Moji_15)+15
'16画
Moji_Min_16 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15)+16:Moji_Max_16=Moji_1 + Moji_2 + Moji_3 + Moji_4 + Moji_5 + Moji_6 + Moji_7 + Moji_8 + Moji_9 + Moji_10 + Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+16
'17画
Moji_Min_17 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16) + 17:Moji_Max_17 = Moji_1 + Moji_2 + Moji_3 + Moji_4+Moji_5 + Moji_6 + Moji_7+Moji_8 + Moji_9 + Moji_10 + Moji_11 + Moji_12 + Moji_13 + Moji_14 + Moji_15+Moji_16+Moji_17+17
'18画
Moji_Min_18 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17)+18:Moji_Max_18 = (Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18)+18
'19画
Moji_Min_19=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18)+19:Moji_Max_19=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19)+19
'20画
Moji_Min_20=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19)+20:Moji_Max_20=Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+20
'21画
Moji_Min_21=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20)+21:Moji_Max_21=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21)+21
'22画
Moji_Min_22=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21)+22:Moji_Max_22=Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21+Moji_22+22
'23画
Moji_Min_23=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21+Moji_22):Moji_Max_23=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21+Moji_22+Moji_23)
'24画
Moji_Min_24=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21+Moji_22+Moji_23)+24:Moji_Max_24=(Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+Moji_18+Moji_19+Moji_20+Moji_21+Moji_22+Moji_23+Moji_24)+24
dim Moji_data$(Moji_total+24)
'dim bufmoji$(10),Input_data$(10)
'１画の文字   サイズ:21+2(漢字)
dim buf_char_hiragana1$(Moji_1)
'2画の文字  サイズ 58+14=72(漢字)
dim buf_char_hiragana2$(Moji_2)
'3画の文字 サイズ:48+29=77(漢字)
dim buf_char_hiragana3$(Moji_3)
'4画の文字 サイズ:29+51=80(漢字)
dim buf_char_hiragana4$(Moji_4)
'５画の文字  サイズ:18+59=77(漢字)
dim buf_char_hiragana5$(Moji_5)
'6画の文字  サイズ:79(漢字)
dim buf_char_hiragana6$(Moji_6)
'7画の文字  サイズ:88(漢字)
dim buf_char_hiragana7$(Moji_7)
'8画の文字
dim buf_char_hiragana8$(Moji_8)
'9画の文字
dim buf_char_hiragana9$(Moji_9)
'10画の文字 98文字
dim buf_char_hiragana10$(Moji_10)
'11画の文字
dim buf_char_hiragana11$(Moji_11)
'12画の文字
dim buf_char_hiragana12$(Moji_12)
'13画の文字 81文字
dim buf_char_hiragana13$(Moji_13)
'14画の文字
dim buf_char_hiragana14$(Moji_14)
'15画の文字
dim buf_char_hiragana15$(Moji_15)
'16画の文字
dim buf_char_hiragana16$(Moji_16)
'17画の文字
dim buf_char_hiragana17$(Moji_17)
'18画の文字
dim buf_char_hiragana18$(Moji_18)
'19画の文字
dim buf_char_hiragana19$(Moji_19)
'20画の文字
dim buf_char_hiragana20$(Moji_20)
'21画の文字
dim buf_char_hiragana21$(Moji_21)
'22画の文字
dim buf_char_hiragana22$(Moji_22)
'23画の文字
dim buf_char_hiragana23$(Moji_23)
'24画の文字
dim buf_char_hiragana24$(Moji_24)
'結果表示1　吉凶データー 81パターン
dim buf_Kikkyo$(81)
'結果表示２
'安斎流姓名判断 吉凶  1.地運
dim buf_Kikkyo_Anzai_chiunn$(70)
'安斎流姓名判断　　吉凶  2.人運
dim buf_Kikkyo_Anzai_jinunn$(69)
'安斎流姓名判断 吉凶 3.外運
'dim buf_Kikkyo_Anzai_gaiunn$(70)
'安斎流姓名判断 吉凶　　4.総運
dim buf_Kikkyo_Anzai_total$(80)
'データー読み込み　ここから
'データー読み込み　1.ひらがな
'1画の文字   6文字 23文字
open "config/Kanji_data/Mojidata.dat" for input as #1
'全ファイルを読み込む
j=0:
for i = 1 to ((Moji_total + 24)-1)
input #1,Moji_data$(i-1)
next i
close #1
'全ファイル読み込み　ここまで
for n=1 to (Moji_1)-1
'1画の文字にデーターをコピーする
buf_char_hiragana1$(n-1) = Moji_data$(n+1)
next n
'2画の文字    73文字
for i = Moji_Min_2  to (Moji_Max_2)-1
buf_char_hiragana2$(i-Moji_Min_2)=Moji_data$(i+1)
next i
'3文字の文字    77文字
for i = Moji_Min_3  to (Moji_Max_3)-1
buf_char_hiragana3$(i-(Moji_Min_3)) = Moji_data$(i+1)
next i
'4文字の文字   80文字
for i = (Moji_Min_4)  to (Moji_Max_4)-1
buf_char_hiragana4$(i-Moji_Min_4)=Moji_data$(i+1)
next i
'5文字の文字 77文字
for i = Moji_Min_5 to (Moji_Max_5) - 1
buf_char_hiragana5$(i - Moji_Min_5)=Moji_data$(i+1)
next i
'6文字の文字 79文字
for i = Moji_Min_6  to Moji_Max_6
buf_char_hiragana6$(i-Moji_Min_6) = Moji_data$(i+1)
next i
'7文字の文字 170文字
for i = Moji_Min_7 to Moji_Max_7
buf_char_hiragana7$(i-Moji_Min_7)=  Moji_data$(i+1)
next i
'8画の文字 120文字
for i = Moji_Min_8 to Moji_Max_8
buf_char_hiragana8$(i - Moji_Min_8)=Moji_data$(i+1)
next i
'9画の文字  103文字
for i = Moji_Min_9 to Moji_Max_9
buf_char_hiragana9$(i - Moji_Min_9)=Moji_data$(i+1)
next i
'10画の文字 285文字
for i = Moji_Min_10 to (Moji_Max_10)
buf_char_hiragana10$(i-Moji_Min_10)=Moji_data$(i+1)
next i
'11画の文字
for i = Moji_Min_11 to Moji_Max_11
buf_char_hiragana11$(i-Moji_Min_11)=Moji_data$(i+1)
next i
'12画の文字
for i = Moji_Min_12 to (Moji_Max_12)
buf_char_hiragana12$(i-Moji_Min_12)=Moji_data$(i+1)
next i
'13画の文字 81
for i = Moji_Min_13 to (Moji_Max_13)
buf_char_hiragana13$(i-Moji_Min_13)=Moji_data$(i+1)
next i
'14画の文字 66
for i = Moji_Min_14 to Moji_Max_14
buf_char_hiragana14$(i-Moji_Min_14)=Moji_data$(i+1)
next i
'15画の文字 59
'for i = Moji_Min_15 to (Moji_Max_15) - 1
for i=Moji_Min_15 to Moji_Max_15
buf_char_hiragana15$(i-Moji_Min_15)=Moji_data$(i+1)
next i
'16画の文字 44
for i=Moji_Min_16 to Moji_Max_16
buf_char_hiragana16$(i-Moji_Min_16)=Moji_data$(i+1)
next i
'17画の文字
for i = Moji_Min_17 to Moji_Max_17
buf_char_hiragana17$(i-Moji_Min_17)=Moji_data$(i+1)
next i
'18画の文字
for i = Moji_Min_18 to Moji_Max_18
buf_char_hiragana18$(i-Moji_Min_18)=Moji_data$(i+1)
next i
'19画の文字 17文字
for i = Moji_Min_19 to Moji_Max_19
buf_char_hiragana19$(i-Moji_Min_19)=Moji_data$(i+1)
next i
'20 画の文字 13文字
for  i = Moji_Min_20 to Moji_Max_20
buf_char_hiragana20$(i-Moji_Min_20)=Moji_data$(i+1)
NEXT i
'21画の文字 6
for i = Moji_Min_21 to Moji_Max_21
buf_char_hiragana21$(i-Moji_Min_21)=Moji_data$(i+1)
next i
'22 画の文字 4
for i = Moji_Min_22 to Moji_Max_22
buf_char_hiragana22$(i-Moji_Min_22)=Moji_data$(i+1)
next i
'23画の文字  3文字
for i = Moji_Min_23 to Moji_Max_23
buf_char_hiragana23$(i-Moji_Min_23)=Moji_data$(i+1)
next i
'24画の文字  3文字
for i = Moji_Min_24 to Moji_Max_24-1
buf_char_hiragana24$(i-Moji_Min_24)=Moji_data$(i+1)
next i
'吉凶データー読み込み
open "config/Kikkyo_data/Kikkyo_data.dat" for input as #1
for i=0 to 80
input #1,buf_Kikkyo$(i)
next i
close #1
open "config/Anzai_Kikkyo/Anzai_Kikkyo_chiunn.dat" for input as #1
for m=0 to 69
input #1,buf_Kikkyo_Anzai_chiunn$(m)
next m
close #1
open "config/Anzai_Kikkyo/Anzai_Kikkyo_jinunn.dat" for input as #1
for i=1 to 70
input #1,buf_Kikkyo_Anzai_jinunn$(i-1)
next i
close #1
open "config/Anzai_Kikkyo/Anzai_Kikkyo_Total.dat" for input as #1
for i=0 to 79
input #1,buf_Kikkyo_Anzai_total$(i)
next i
close #1
restore 4050
for i=0 to 19
'  for j=0 to 11
read buf_good_couple1(i)
'  next j
next i
restore 4060
for j=0 to 19
read buf_good_couple2(j)
next j
'0.理解し合えるカップル ここまで
'1.互いに自然体でつきあえる二人　ここから
restore 4090
for i=0 to 19
read buf_natural_couple1(i)
next i
restore 4100
for j=0 to 19
read buf_natural_couple2(j)
next j
'1.互いに自然体でつきあえる二人　ここまで
'2        ここから
restore 4130
for i=0 to 19
read buf_good_for_man1(i)
next i
restore 4140
for j=0 to 19
read buf_good_for_man2(j)
next j
'2        ここまで
'3        ここから
restore 4170
for i=0 to 19
read buf_good_for_woman1(i)
next i
restore 4180
for j=0 to 19
read buf_good_for_woman2(j)
next j
'3        ここまで
'4        ここから
restore 4210
for i=0 to 19
read short_of_experience1(i)
next i
restore 4220
for j=0 to 19
read short_of_experience2(j)
next j
'4        ここまで
'5.結婚への発展が困難なカップル  ここから
restore 4250
for i=0 to 19
read buf_difficult_for_couple1(i)
next i
restore 4260
for j=0 to 19
read buf_difficult_for_couple2(j)
next j
'5.結婚への発展が困難なカップル  ここまで
'6.愛し方にズレが出る二人 ここから
restore 4290
for i=0 to 19
read buf_difference_of_love1(i)
next i
restore 4300
for j=0 to 19
read buf_difference_of_love2(j)
next j
'6.愛し方にズレが出る二人 ここまで
'7.互いの価値観が噛み合わない相性 ここから
restore 4330
for i=0 to 19
read buf_difference_of_KachiKan1(i)
next i
restore 4340
for j=0 to 19
read buf_difference_of_KachiKan2(j)
next j
'7.互いの価値観が噛み合わない相性 ここまで
'相性占いタイプ ここから
restore 4380
for i=0 to 7
read Result_Aisyou_type$(i)
next i
data 1,5,6,2,5,7,3,5,8,4,6,0,-1,-1,-1,-1,-1,-1,-1,-1
data 8,2,2,6,8,4,9,0,1,7,1,5,-1,-1,-1,-1,-1,-1,-1,-1
'0.理解し合えるカップル　ここまで
'1.互いに自然体でつきあえるカップル ここから
data 1,4,6,7,9,2,4,6,8,9,3,4,6,8,0,3,4,7,8,0
data 6,1,6,2,6,5,4,7,7,0,2,9,8,8,3,3,0,1,0,7
'1.互いに自然体でつきあえるカップル　ここまで
'2.男性にとって居心地の良いカップル  ここから
data 1,6,9,2,8,9,3,8,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
data 1,5,4,0,5,8,4,9,9,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
'2男性にとって居心地の良いカップル ここまで
'3女性にとって居心地の良いカップル  ここから
data 1,2,6,7,1,5,6,0,2,5,7,0,-1,-1,-1,-1,-1,-1,-1,-1
data 3,2,4,0,9,3,9,2,1,6,8,8,-1,-1,-1,-1,-1,-1,-1,-1
'3女性にとって居心地の良いカップル  ここまで
'4恋愛経験を重ねた後なら愛を育める ここから
data 1,3,6,8,1,3,7,2,4,8,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
data 7,6,3,3,0,7,5,4,5,2,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
'4恋愛経験を重ねた後なら愛を育める ここまで
'5.結婚への発展が困難なカップル ここから
data 1,4,7,2,4,9,3,7,0,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
data 2,3,7,3,6,3,5,6,1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
'5.結婚への発展が困難なカップル ここまで
'6.愛し方にズレが生じる二人  ここから
data 1,5,9,2,5,9,3,7,0,3,8,-1,-1,-1,-1,-1,-1,-1,-1,-1
data 5,4,5,7,7,7,8,9,6,0,6,-1,-1,-1,-1,-1,-1,-1,-1,-1
'6.愛し方にずれが生じる二人  ここまで
'7.互いに価値観が噛み合わない相性　ここから
data 1,4,5,9,0,2,4,6,9,2,5,7,9,3,5,8,0,-1,-1,-1
data 4,2,9,1,0,8,8,0,2,9,1,3,9,1,5,4,4,-1,-1,-1
'7.互いに価値観が噛み合わない相性 ここまで
'相性占い結果パターン ここから
'0
data "理解し合える最良のカップル"
'1
data "互いに自然体でつきあえるカップル"
'2
data "男性にとって居心地の良い相性"
'3
data "女性にとって居心地の良い相性"
'4
data "恋愛経験を重ねた後なら愛を育める"
'5
data "結婚への発展が困難なカップル"
'6
data "愛し方にずれが生じる二人"
'7
data "互いの価値観が噛み合わない相性"
'相性占い結果パターン ここまで
'メイン画面 Top画面1
Main_Screen:
cls 3:font 48:color rgb(255,255,255),,rgb(176,196,222)
talk "番号を選んでエンターキーを押してください"
'Text ,Grapgic clear:cls 3
'グラフィック領域　ここから
line (0,0)-(870,60),rgb(0,0,255),bf
pen 5:line(0,0)-(867,57),rgb(255,255,255),b
line (0,60)-(870,460),rgb(127,255,212),bf
pen 5:line(0,63)-(873,462),rgb(0,0,0),b
line (0,457)-(873,637),rgb(0,255,0),bf
pen 5:line(0,463)-(874,640),rgb(255,0,255),b
'グラフィック領域 ここまで
'touch(0):x=touch(4)
print"◎姓名判断　メイン画面"+chr$(13)
'文字色:黒
color rgb(255,0,255)
print"1.姓名判断"+chr$(13)
print"2.姓名判断の設定"+chr$(13)
print"3.ヘルプ"+chr$(13)
print"4.プログラムの終了"+chr$(13)
'x=touch(4)
'print"7.プログラム終了"+chr$(13)
'文字:黒
color rgb(0,0,0)
print"番号を選んでください"+chr$(13)
Input"番号:",selectNo
if selectNo = 1 then goto SeimeiHandan_Top:
if selectNo = 2 then goto Menu2_Setting:
if selectNo = 3  then goto Menu2_Help:
if selectNo = 4 then talk"終了します":cls 3:end
if  selectNo > 4 or selectNo=0  then goto Main_Screen:
if str$(selectNo)="" then goto Main_Screen:
'1.姓名判断トップ画面
screen 1,1
'タイトル文字:白
font 48:color rgb(255,255,255),,rgb(176,196,222)
talk "姓名判断トップメニューです。姓名判断の種類の番号を選んでエンターキーを押してください"
'グラフィック 描画領域　ここから
SeimeiHandan_Top:
cls 3
'1.Title:青
line (0,0)-(870,60),rgb(0,0,255),bf
pen 5:line (0,3)-(873,63),rgb(255,255,255),b
line (0,60)-(870,360),rgb(127,255,212),bf
pen 5:LINE (0,63)-(873,363),rgb(255,0,255),b
line (0,360)-(870,520),rgb(0,255,0),bf
pen 5:line (0,363)-(873,523),rgb(0,0,0),b
'グラフィック 描画領域 ここまで
talk"姓名判断トップ画面です。番号を選んでエンターキーを押してください"
color rgb(255,255,255)
print"◎姓名判断の種類トップメニュー"+chr$(13)
color rgb(255,0,255):print"1.九星姓名判断"+chr$(13)
color rgb(255,0,255):print"2.安斎流姓名判断"+chr$(13)
COLOR rgb(255,0,255):print"3.前の画面に戻る"+chr$(13)
color rgb(0,0,0):Print"番号を選んでエンターを押してください"
color rgb(0,0,0):Input"番号:",selectNo
IF selectNo=1 then goto Menu1_kyusei:
if selectNo=2 then goto Menu1_2_Anzai_Top:
if selectNo=3 then goto Main_Screen:
'１．姓名判断(九星姓名判断トップ)
'グラフィック領域　ここから
Menu1_kyusei:
cls 3
line (0,0)-(720,60),rgb(0,0,250),bf
pen 5:line (0,0)-(723,63),rgb(255,255,255),b
line (0,60)-(720,450),rgb(127,255,212),bf
pen 5:line(0,63)-(723,453),rgb(0,0,0),b
line (0,450)-(720,650),rgb(0,255,0),bf
pen 5:line(0,453)-(723,653),rgb(255,0,255),b
'グラフィック領域　ここまで
font 48:color rgb(255,255,255),,rgb(176,196,222)
'talk "姓名判断トップ画面です。番号を選んでエンターキーを押してください。"
cls:color rgb(255,255,255):print"◎1.姓名判断(九星姓名判断)"+chr$(13)
color rgb(255,0,255):print"1.人名の吉凶を見る"+chr$(13)
color rgb(255,0,255):print"2.総数で名前の吉凶を見る"+chr$(13)
color rgb(255,0,255):print"3.名前の陰陽を見る"+chr$(13)
color rgb(255,0,255):print"4.前の画面に戻る"+chr$(13)
color rgb(0,0,0):print"番号を選んでください"+chr$(13)
color rgb(0,0,0):Input"番号:",selectNo
if selectNo=1 then goto Menu1_name_Kikkyo:
if selectNo=2 then goto Menu1_2_Total_name:
if selectNo=3 then goto Input_name_InYo:
if selectNo=4 then goto Main_Screen:
if selectNo > 4 or selectNo=0 then goto Menu1_kyusei:
'2.姓名判断 安斎流姓名判断　トップ画面
'2-1名前の姓の部分を入力
'グラフィック領域　ここから
Menu2_Anzai_Kikkyo:
cls 3:LINE (0,0)-(950,60),rgb(0,0,255),bf
pen 5:line(0,0)-(953,63),rgb(255,255,255),b
line (0,63)-(947,275),rgb(127,255,212),bf
pen 5:line(0,63)-(950,278),rgb(0,255,0),b
line (0,275)-(950,375),rgb(0,255,0),bf
pen 5:line(0,272)-(947,372),rgb(0,0,0),b
'グラフィック領域 ここまで
color rgb(255,255,255):print"安斎流　姓名判断　トップメニュー"+chr$(13)
talk"安斎流姓名判断トップメニューです,名前を2回に分けて入力してください。まず最初に名前の姓の部分を入れてください"
COLOR rgb(255,0,255)
print"名前を2回に分けて入力してください"+chr$(13)
print"名前の姓の部分を入れてください"+chr$(13)
color rgb(0,0,0)
input"名前の姓:",bufname$
buff1=len(bufname$)
'2-2名前の名の部分を入力
'グラフィック描画領域　ここから
cls 3
line (0,0)-(950,60),rgb(0,0,255),bf
pen 5:line(0,0)-(950,60),rgb(255,255,255),b
line (0,60)-(950,160),rgb(127,255,212),bf
pen 5:line(0,63)-(953,163),rgb(255,0,255),b
line (0,160)-(950,260),rgb(0,255,0),bf
pen 5:line(0,163)-(953,263),rgb(0,0,0),b
'グラフィック描画領域 ここまで
COLOR rgb(255,255,255)
cls:print"安斎流姓名判断トップメニュー"+chr$(13)
talk"つぎに、名前のめいの部分を入れてください"
color rgb(255,0,255)
print"名前の名の部分を入れてください"+chr$(13)
color rgb(0,0,0)
input"名前の名:",bufname2$
buff2=len(bufname2$)
bufff=buff1+buff2
'goto 15900
select case bufff
'姓1文字,名1文字
case 2:
cls
'天運:buf_tenunn
buf_tenunn=char_count(bufname$)
'地運:buf_chiunn
buf_chiunn=char_count(bufname2$)
'人運 = 天運 + 地運
buf_jinunn=buf_tenunn + buf_chiunn
'外運 = 天運 + 人運
buf_gaiunn = buf_tenunn + buf_chiunn
'総数=buf_gaiunn
buf_total=buf_gaiunn
goto Menu2_Result_Anzai_Kikkyo:
'姓１，名:2
case 3:
cls:
if buff1=1 and buff2=2 then
'1.天運:buf_tenunn
buf_tenunn=char_count(bufname$)
'2文字目の文字
bufer_name2$(0)=Mid$(bufname2$,1,1)
'3文字目の文字
bufer_name2$(1)=Mid$(bufname2$,2,1)
'2.人運
buf_jinunn=char_count(bufer_name$(0))+char_count(bufer_name2$(1))
'3.地運:buf_chiunn
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
'4外運:buf_gaiunn
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name2$(1))
'5.総数:buf_total
buf_total=char_count(bufer_name$(0))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
goto Menu2_Result_Anzai_Kikkyo:
endif
if buff1=2 and buff2=1 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name2$(0)=Mid$(bufname$,1,1)
'1.天運
buf_tenunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))
'2.人運
buf_jinunn=char_count(bufer_name$(1))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name2$(0))
'5.総数
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(0))
goto Menu2_Result_Anzai_Kikkyo:
endif
case 4:
if buff1=2 and buff2=2 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
bufer_name2$(1)=Mid$(bufname2$,2,1)
'1.天運
buf_tenunn = char_count(bufer_name$(0)) + char_count(bufer_name$(1))
'2.人運
buf_jinunn=char_count(bufer_name$(1))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name2$(1))
'5.総数
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
goto Menu2_Result_Anzai_Kikkyo:
endif
'パターン2 姓3文字 名1文字 total4文字
if buff1=3 and buff2=1 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name$(2)=Mid$(bufname$,3,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
'1.天運
buf_tenunn = char_count(bufer_name$(0)) + char_count(bufer_name$(1)) + char_count(bufer_name$(2))
'2.人運
buf_jinunn = char_count(bufer_name$(2))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))
'4.外運
buf_gaiunn = char_count(bufer_name$(0)) + char_count(bufer_name$(1)) + char_count(bufname2$)
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name2$(0))
goto Menu2_Result_Anzai_Kikkyo:
endif
'パターン３ 姓1,名３  合計４文字
if buff1=1 and buff2=3 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
bufer_name2$(1)=Mid$(bufname2$,2,1)
bufer_name2$(2)=Mid$(bufname2$,3,1)
'1.天運
buf_tenunn = char_count(bufer_name$(0))
'2.人運
buf_jinunn=char_count(bufer_name$(0))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
goto Menu2_Result_Anzai_Kikkyo:
endif
case 5:
'５文字の名前
'1.  3文字姓 2字名
if buff1=3 and buff2=2 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name$(2)=Mid$(bufname$,3,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
bufer_name2$(1)=Mid$(bufname2$,2,1)
'1.天運
buf_tenunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))
'2.人運
buf_jinunn=char_count(bufer_name$(2))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(1))
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
goto Menu2_Result_Anzai_Kikkyo:
endif
if buff1=2 and buff2=3 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
bufer_name2$(1)=Mid$(bufname2$,2,1)
bufer_name2$(2)=Mid$(bufname2$,3,1)
'1.天運
buf_tenunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))
'2.人運
buf_jinunn=char_count(bufer_name$(1))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
goto Menu2_Result_Anzai_Kikkyo:
endif
if buff1=4 and buff2=1 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=mid$(bufname$,2,1)
bufer_name$(2)=mid$(bufname$,3,1)
bufer_name$(3)=mid$(bufname$,4,1)
bufer_name2$(0)=mid$(bufname2$,1,1)
'1.天運
buf_tenunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name$(3))
'2.人運
buf_jinunn=char_count(bufer_name$(3))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(0))
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name$(3))+char_count(bufer_name2$(0))
goto Menu2_Result_Anzai_Kikkyo:
endif
case 6:
'3字姓 3字名
if buff1=3 and buff2=3 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name$(2)=Mid$(bufname$,3,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
bufer_name2$(1)=Mid$(bufname2$,2,1)
bufer_name2$(2)=Mid$(bufname2$,3,1)
'1.天運
buf_tenunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))
'2.人運
buf_jinunn=char_count(bufer_name$(2))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
goto Menu2_Result_Anzai_Kikkyo:
endif
'4字姓 2字名
if buff1=4 and buff2=2 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name$(2)=Mid$(bufname$,3,1)
bufer_name$(3)=Mid$(bufname$,4,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
bufer_name2$(1)=Mid$(bufname2$,2,1)
'1.天運
buf_tenunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name$(3))
'2.人運
buf_jinunn=char_count(bufer_name$(3))+char_count(bufer_name2$(0))
'3.地運
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(1))
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name$(3))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))
goto Menu2_Result_Anzai_Kikkyo:
endif
'4字姓 3字名
case 7:
if buff1=4 and buff2=3 then
bufer_name$(0)=Mid$(bufname$,1,1)
bufer_name$(1)=Mid$(bufname$,2,1)
bufer_name$(2)=Mid$(bufname$,3,1)
bufer_name$(3)=Mid$(bufname$,4,1)
bufer_name2$(0)=Mid$(bufname2$,1,1)
bufer_name2$(1)=Mid$(bufname2$,2,1)
bufer_name2$(2)=Mid$(bufname2$,3,1)
'1.天運
buf_tenunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name$(3))
'2.人運
buf_jinunn=char_count(bufer_name$(3))+char_count(bufer_name2$(0))
' 3.地運
buf_chiunn=char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))+char_count(bufer_name2$(3))
'4.外運
buf_gaiunn=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
'5.総運
buf_total=char_count(bufer_name$(0))+char_count(bufer_name$(1))+char_count(bufer_name$(2))+char_count(bufer_name$(3))+char_count(bufer_name2$(0))+char_count(bufer_name2$(1))+char_count(bufer_name2$(2))
goto Menu2_Result_Anzai_Kikkyo:
endif
case else:
end select
'2.設定
Menu2_Setting:
font 48:color rgb(0,0,0),,rgb(176,196,222)
talk"設定画面です。番号を選んでエンターキーを押してください"
'グラフィック領域　ここから
cls 3:
line (0,0)-(700,60),rgb(0,0,255),bf
pen 5:line(0,0)-(703,63),rgb(255,255,255),b
line (0,60)-(700,570),rgb(127,255,212),bf
pen 5:line(0,66)-(703,576),rgb(255,0,255),b
line (0,570)-(700,740),rgb(0,255,0),bf
pen 5:line(0,576)-(703,746),rgb(0,0,0),b
'グラフィック領域 ここまで
'1行目　文字色　 白
color rgb(255,255,255):print"姓名判断　設定画面トップ画面"+chr$(13)
color rgb(255,0,255):print"1.登録文字の確認"+chr$(13)
color rgb(255,0,255):print"2.登録文字数の表示"+chr$(13)
color rgb(255,0,255):print"3.メンバーリストの表示"+chr$(13)
color rgb(255,0,255):print"4.前の画面に戻る"+chr$(13)
color rgb(255,0,255):PRINT"5.プログラムの終了"+chr$(13)
color rgb(0,0,0)
print"番号を選んでください"+chr$(13)
Input"番号:",selectNo
if selectNo=1 then font 48:goto Menu2_moji_check:
if selectNo=2 then font 48:goto Menu2_Entry_moji:
if selectNo=3 then font 48:goto Member_List_Top:
if selectNo=4 then font 48:goto Main_Screen:
if selectNo=5 then talk"終了します":cls 3:font 48:end
if selectNo > 5 or selectNo = 0 then goto Menu2_Setting:
'3 番号で吉凶を見る 入力
Menu1_2_Total_name:
font 48:color rgb(0,0,0),,rgb(176,196,222)
'グラフィック描画領域　ここから
cls 3
line (0,0)-(700,60),rgb(0,0,255),bf
pen 5:line (0,0)-(703,63),rgb(255,255,255),b
line (0,60)-(700,250),rgb(127,255,212),bf
pen 5:line(0,63)-(703,253),rgb(255,0,255),b
line (0,247)-(697,357),rgb(0,255,0),bf
pen 5:line(0,263)-(703,363),rgb(0,0,0),b
'グラフィック描画領域 ここまで
color rgb(255,255,255)
print"画数での吉凶判定"+chr$(13)
color rgb(255,0,255)
print"画数を入れてください"+chr$(13)
print"(Max:81文字)"+chr$(13)
color rgb(0,0,0)
Input"文字の画数:",Number
if Number > 81 then goto Menu1_2_Total_name:
if Number <=81 then goto Result_Total_name:
'3.番号で吉凶を見る 結果表示
'グラフィック描画領域　ここから
Result_Total_name:
cls 3
line (0,0)-(700,60),rgb(0,0,255),bf
pen 5:line(0,0)-(703,63),rgb(255,255,255),b
line (0,60)-(700,253),rgb(127,255,212),bf
pen 5:line(0,63)-(703,256),rgb(255,0,255),b
line (0,253)-(700,357),rgb(0,255,0),bf
pen 5:line(0,250)-(703,360),rgb(0,0,0),b
'グラフィック描画領域　ここまで
color rgb(255,255,255)
print "画数で吉凶を求める"+chr$(13)
color rgb(255,0,255)
talk str$(Number)+"画のきっきょうは、"+buf_Kikkyo$(Number)+"です"
print"画数:";Number;chr$(13):print"吉凶:";buf_Kikkyo$(Number);chr$(13)
'endif
color rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
key$=input$(1)
If key$=chr$(13) then goto Menu1_kyusei:
'3.ヘルプ
Menu2_Help:
cls 3:font 48:color rgb(0,0,0),,rgb(176,196,222)
'描画領域 ここから
line (0,0)-(650,60),rgb(0,0,255),bf
pen 5:line(0,0)-(653,63),rgb(255,255,255),b
line (0,60)-(650,560),rgb(127,255,212),bf
pen 5:line(0,63)-(653,563),rgb(255,0,255),b
line (0,560)-(650,660),rgb(0,255,0),bf
pen 5:line(0,563)-(653,663),rgb(0,0,0),b
'描画領域　ここまで
talk"ヘルプ画面です番号を選んでエンターキーを押してください"
color rgb(255,255,255):print "姓名判断　ヘルプ"+chr$(13)
color rgb(255,0,255):print"1.バージョン情報"+chr$(13)
color rgb(255,0,255):print"2.参考文献"+chr$(13)
color rgb(255,0,255):print"3.プログラムの終了"+chr$(13)
color rgb(255,0,255):print"4.メンバーリストの削除"+chr$(13)
color rgb(255,0,255):PRINT"5.前の画面に戻る"+chr$(13)
color rgb(0,0,0):Input"番号:",selectNo
if selectNo=1 then goto Version_Info:
if selectNo=2 then goto Reference_book:
if selectNo=3 then talk"終了いたします":cls 3:end
if selectNo=4 then goto Remove_MemberList:
if selectNo=5 then goto Main_Screen:
if selectNo > 5 or selectNo = 0 then goto 8750
Menu1_name_Kikkyo:
'Menu1　画面
talk"調べたい名前のみよじをいれてください"
'グラフィック描画領域　ここから
cls 3
line (0,0)-(1000,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1003,63),rgb(255,255,255),b
line (0,60)-(1000,170),rgb(127,255,212),bf
pen 5:line(0,63)-(1003,173),rgb(255,0,255),b
line (0,170)-(1000,300),rgb(0,255,0),bf
pen 5:line(0,173)-(1003,303),rgb(0,0,0),b
'グラフィック描画領域　ここまで
color rgb(255,255,255):print"調べたい名前の苗字を入れてください"+chr$(13)
color rgb(255,0,255):print"例:山田太郎の山田の部分"+chr$(13)
color rgb(0,0,0):input"調べたい名前の苗字(上の文字):",name$
cls
talk "調べたい名前の下の名を入れてください":color rgb(255,255,255):print"調べたい名前の名の部分を入れてください"+chr$(13)
color rgb(255,0,255):print"例:山田太郎の太郎の部分を入れてください"+chr$(13)
color rgb(0,0,0):input"調べたい名前の名の部分(下の部分):",name2$
goto Menu1_Result_Kyusei:
'メニュー6　バージョン表示
'グラフィック　描画　領域　 ここから
Version_Info:
cls 3
line (0,0)-(1050,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1053,63),rgb(255,255,255),b
line (0,60)-(1050,630),rgb(127,255,212),bf
pen 5:line(0,0)-(1053,633),rgb(0,255,0),b
line (0,630)-(1050,750),rgb(0,255,0),bf
pen 5:line(0,633)-(1053,753),rgb(0,0,0),b
'グラフィック　描画 領域　 ここまで
color rgb(255,255,255):print"バージョン情報"+chr$(13)
color rgb(255,0,255):print"姓名判断"+chr$(13)
color rgb(255,0,255):PRINT"Ver:235_04.20200915"+chr$(13)
color rgb(255,0,255):print"対応文字:漢字、ひらがな、カタカナ、数字"+chr$(13)
color rgb(255,0,255):print"アルファベット（大文字、小文字)"+chr$(13)
color rgb(255,0,255):print"対応漢字画数：1画から24画まで"+chr$(13)
color rgb(255,0,2550):print"制作開始:since 2019.04.07"+chr$(13)
'ここを書き換える
talk "このプログラムは、姓名判断バージョン2.35です"
color rgb(0,0,0):print"エンターキーを押してください"+chr$(13)
key$=INPUT$(1)
if key$=chr$(13) then goto Main_Screen:
'文字数を求める変数
buf_char_size=len(name$)
buf_char_size2=len(name2$)
'入力した文字を代入する変数
'入力した文字を配列に代入する処理
'姓名判断データー文字比較
'画数を求める関数
func char_count(buf_count$)
count=0:buffer=0
'1画の文字 23文字
for j=0 to ((Moji_1)-1)
if buf_count$=buf_char_hiragana1$(j) then
count =1:
endif
next j
'2画の文字
for j=0 to ((Moji_2)-1)
if buf_count$=buf_char_hiragana2$(j)  then
count = 2:
endif
next j
for j=0 to ((Moji_3)-1)
if buf_count$=buf_char_hiragana3$(j) then
count =3:
endif
next j
for j=0 to ((Moji_4)-1)
if buf_count$=buf_char_hiragana4$(j) then
count = 4:
endif
next j
for j=0 to ((Moji_5)-1)
if buf_count$=buf_char_hiragana5$(j) then
count = 5:
endif
next j
for j=0 to ((Moji_6)-1)
if buf_count$=buf_char_hiragana6$(j) then
count= 6
endif
next j
for  j=0 to ((Moji_7)-1)
if buf_count$=buf_char_hiragana7$(j) then
count=  7
endif
next j
for j=0 to ((Moji_8)-1)
if buf_count$=buf_char_hiragana8$(j) then
count= 8
endif
next j
for j=0 to ((Moji_9)-1)
if buf_count$=buf_char_hiragana9$(j) then
count=9
endif
next j
for j=0 to ((Moji_10)-1)
if buf_count$=buf_char_hiragana10$(j) then
count=10
endif
next j
for j=0 to ((Moji_11)-1)
if buf_count$=buf_char_hiragana11$(j) then
count=11
endif
next j
for j=0 to  ((Moji_12)-1)
if buf_count$=buf_char_hiragana12$(j) then
count=12
endif
next j
for j=0 to ((Moji_13)-1)
if buf_count$=buf_char_hiragana13$(j) then
count=13
endif
next j
for j=0 to ((Moji_14)-1)
if buf_count$=buf_char_hiragana14$(j) then
count=14
endif
next j
for j=0 to ((Moji_15)-1)
if buf_count$=buf_char_hiragana15$(j) then
count=15
endif
next j
for j=0 to ((Moji_16)-1)
if buf_count$=buf_char_hiragana16$(j) then
count=16
endif
next j
for j=0 to ((Moji_17)-1)
if buf_count$=buf_char_hiragana17$(j) then
count=17
endif
next j
for j=0 to ((Moji_18)-1)
if buf_count$=buf_char_hiragana18$(j) then
count=18
endif
next j
for j=0 to ((Moji_19)-1)
if buf_count$=buf_char_hiragana19$(j) then
count=19
endif
next j
for j=0 to ((Moji_20)-1)
if buf_count$=buf_char_hiragana20$(j) then
count=20
endif
next i
for j=0 to ((Moji_21)-1)
if buf_count$=buf_char_hiragana21$(j) then
count=21
endif
next j
for j=0 to ((Moji_22)-1)
if buf_count$=buf_char_hiragana22$(j) then
count=22
endif
next j
for j=0 to ((Moji_23)-1)
if buf_count$=buf_char_hiragana23$(j) then
count=23
endif
next j
for j=0 to ((Moji_24)-1)
if buf_count$=buf_char_hiragana24$(j) then
count=24
endif
buffer = count
next j
endfunc buffer
'合計を求める関数
func totalcounts(buffers$)
buffers=0:
for i=0 to len(buffers$)-1
buffers=buffers+char_count(buffers$)
next i
endfunc buffers
buf_count2=0:buf_count3=0
'dim buf_Input_data$(len(name$))
'1.苗字の文字数
for n=0 to (buf_char_size-1)
buf_Input_data$(n)=mid$(name$,n+1,1)
next n
for i=0 to (buf_char_size-1)
buf_count2 = buf_count2 + char_count(buf_Input_data$(i))
next i
'2.名の文字数
for n2=0 to (buf_char_size2-1)
buf_Input_data2$(n2)=mid$(name2$,n2+1,1)
next n2
for i2=0 to (buf_char_size2-1)
buf_count3 = buf_coun3 + char_count(buf_Input_data2$(i2))
next i2
'総数を出す
select case (buf_char_size+buf_char_size2)
case 5:
if ((buf_char_size=2) and (buf_char_size2=3)) then
buf_total=char_count(buf_Input_data$(0))+char_count(buf_Input_data$(1))+char_count(buf_Input_data2$(0))+char_count(buf_Input_data2$(1))+char_count(buf_Input_data2$(2))
endif
if ((buf_char_size=3) and (buf_char_size2=2)) then
buf_total=char_count(buf_Input_data$(0))+char_count(buf_Input_data$(1))+char_count(buf_Input_data$(2))+char_count(buf_Input_data2$(0))+char_count(buf_Input_data2$(1))
endif
case 4:
if ((char_count(buf_Input_data$(0))=0) or (char_count(buf_Input_data$(1))=0) or ((char_count(buf_Input_data2$(0))=0) or char_count(buf_Input_data2$(1))=0)) then
color rgb(255,0,255)
cls:print name$+name2$;"は、登録されていない文字があります"+chr$(13)
color rgb(0,0,0)
Input"エンターキーを押してください",key$
if (key$="") then goto Main_Screen:
else
buf_total = char_count(buf_Input_data$(0))+char_count(buf_Input_data$(1))+char_count(buf_Input_data2$(0))+char_count(buf_Input_data2$(1))
endif
case 3:
if ((buf_char_size = 1) and (buf_char_size2 = 2)) then
buf_total = char_count(buf_Input_data$(0))+char_count(buf_Input_data2$(0))+char_count(buf_Input_data2$(1))
endif
if ((buf_char_size=2) and (buf_char_size2=1)) then
buf_total=char_count(buf_Input_data$(0))+char_count(buf_Input_data$(1))+char_count(buf_Input_data2$(0))
endif
case 2:
buf_total = char_count(buf_Input_data$(0)) + char_count(buf_Input_data2$(0))
case else:
end select
'文字の総数をだす
total_name$=name$+name2$
'1.姓星を求める
select case buf_char_size
'苗字1文字のとき
case 1:
buf_seisei=char_count(buf_Input_data$(0)) + 1
'苗字2文字の時
case 2:
for i = 0 to 1
buf_seisei = buf_seisei + char_count(buf_Input_data$(i))
next i
'苗字3文字のとき
case 3:
buf_seisei = char_count(buf_Input_data$(0)) + char_count(buf_Input_data$(1) + buf_Input_data$(2))
case else:
end select
'2.主星をだす
'2.主星
select case (buf_char_size + buf_char_size2)
'姓名4文字のとき
case 4:
if ((buf_char_size = 2) and (buf_char_size2 = 2)) then
buf_syusei = char_count(buf_Input_data$(1)) + char_count(buf_Input_data2$(0))
endif
'姓名3文字のとき
case 3:
if ((buf_char_size = 1) and (buf_charsize2 = 2)) then
buf_syusei = char_count(buf_Input_data$(1)) + char_count(buf_Input_data2$(0))
else
buf_syusei = char_count(buf_Input_data$(1)) + char_count(buf_Input_data2$(0))
endif
case 2:
buf_syusei = char_count(buf_Input_data$(1)) + char_count(buf_Input_data2$(0))
case else:
end select
'3.名星を求める
select case (buf_char_size + buf_char_size2)
case 4:
buf_meisei = char_count(buf_Input_data2$(0)) + char_count(buf_Input_data2$(1))
case 3:
if ((buf_char_size = 1) and (buf_char_size2 = 2)) then
buf_meisei=char_count(buf_Input_data2$(0)+buf_Input_data2$(1))
endif
if ((buf_char_size=2) and (buf_char_size2=1)) then
buf_meisei=char_count(buf_Input_data2$(0))+1
endif
case 2:
buf_meisei=char_count(buf_Input_data2$(0))+1
case else:
end select
'4.外星を求める
select case (buf_char_size+buf_char_size2)
case 4:
buf_gaisei=char_count(buf_Input_data$(0))+char_count(buf_Input_data2$(1))
case 3:
if ((buf_char_size=1) and (buf_char_size2=2)) then
buf_gaisei=1+char_count(buf_Input_data2$(1))
endif
if ((buf_char_size=2) and (buf_char_size2=1)) then
buf_gaisei=char_count(buf_Input_data$(0))+1
endif
case 2:
buf_gaisei=2
case else:
end select
'診断結果表示
'1.姓星を出す
'グラフィック描画領域　ここから
Menu1_Result_Kyusei:
cls 3:
'縦の画面
line (0,0)-(950,60),rgb(0,0,255),bf
pen 5:line (0,0)-(953,63),rgb(255,255,255),b
line (0,60)-(950,650),rgb(127,255,212),bf
pen 5: line (0,63)-(953,653),rgb(0,255,0),b
line (0,650)-(950,770),rgb(0,255,0),bf
pen 5: line (0,653)-(953,773),rgb(0,0,0),b
'横の画面　吉凶のランク表
'グラフィック描画領域　ここまで
talk"診断結果です"
color rgb(255,255,255):print "九星姓名判断　吉凶の結果表示"+chr$(13)
color Rgb(255,0,255)
print total_name$;"の姓星:";buf_seisei;"吉凶:";buf_Kikkyo$(buf_seisei-1)+chr$(13)
'2.主星を出す
print total_name$;"の主星:";buf_syusei;"吉凶:";buf_Kikkyo$(buf_syusei-1)+chr$(13)
'3.名星を求める
print total_name$;"の名星:";buf_meisei;"吉凶:";buf_Kikkyo$(buf_meisei-1)+chr$(13)
'4.外星を求める
print total_name$;"の外星:";buf_gaisei;"吉凶:";buf_Kikkyo$(buf_gaisei-1)+chr$(13)
'5.総数を出す
print total_name$;"の総数:";buf_total;"です"+chr$(13)
print total_name$;"の文字の総合的な吉凶:";buf_Kikkyo$(buf_total-1)+chr$(13)
color rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
key$=input$(1)
if key$=chr$(13) then bufferCount=0:buf_count2=0:count=0:buffer=0:buf_seisei=0:buf_syusei=0:buf_meisei=0:buf_gaisei=0:goto Main_Screen:
'2.主星を出す
'メニュー3 登録文字の確認 ここから
'グラフィック領域　ここから
'タイトル青 文字:白
Menu2_moji_check:
cls 3:line (0,0)-(1100,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1097,57),rgb(255,255,255),b
line (0,57)-(1100,180),rgb(0,255,255),bf
pen 5:line(0,63)-(1103,183),rgb(0,0,255),b
line (0,180)-(1100,300),rgb(0,255,0),bf
pen 5:line(0,183)-(1103,303),rgb(0,0,0),b
'グラフィック領域　ここまで
'文字:白
color rgb(255,255,255):print"登録文字の確認"+chr$(13)
'文字:アクア
color rgb(255,0,0):print"文字を入力してエンターキーを押してください"+chr$(13)
color rgb(0,0,0):Input"登録文字を入れてください(1文字):",name$
if (len(name$)>1) then ui_msg("１文字で入れてください"):goto Menu2_moji_check:
'1画の文字
for i=0 to ((Moji_1)-1)
if (name$=buf_char_hiragana1$(i)) then
buffer_count=1:goto Result_moji_check:
endif
next i
'2画の文字
for i=0 to ((Moji_2)-1)
if (name$=buf_char_hiragana2$(i)) then
buffer_count=2:goto Result_moji_check:
endif
next i
'3画の文字
for i=0 to ((Moji_3)-1)
if (name$=buf_char_hiragana3$(i)) then
buffer_count=3:goto Result_moji_check:
endif
next i
'4画の文字
for i=0 to ((Moji_4)-1)
if (name$=buf_char_hiragana4$(i)) then
buffer_count=4:goto Result_moji_check:
endif
next i
'5画の文字
for i=0 to ((Moji_5)-1)
if (name$=buf_char_hiragana5$(i)) then
buffer_count=5:goto Result_moji_check:
endif
next i
'6画の文字
for i=0 to ((Moji_6)-1)
if (name$=buf_char_hiragana6$(i)) then
buffer_count=6:goto Result_moji_check:
endif
next i
'7画の文字
for i=0 to ((Moji_7)-1)
if (name$=buf_char_hiragana7$(i)) then
buffer_count=7:goto Result_moji_check:
endif
next i
'8画の文字 120 文字
for i=0 to ((Moji_8)-1)
if (name$=buf_char_hiragana8$(i)) then
buffer_count=8:goto Result_moji_check:
endif
next i
'9画の文字  103文字
for i=0 to ((Moji_9)-1)
if (name$=buf_char_hiragana9$(i)) then
buffer_count=9:goto Result_moji_check:
endif
next i
'10画の文字 98文字
for i=0 to ((Moji_10)-1)
if (name$=buf_char_hiragana10$(i)) then
buffer_count=10:goto Result_moji_check:
endif
next i
'11画の文字 98文字
for i=0 to ((Moji_11)-1)
if (name$=buf_char_hiragana11$(i)) then
buffer_count=11:goto Result_moji_check:
endif
next i
'12画の文字
for i=0 to ((Moji_12)-1)
if (name$=buf_char_hiragana12$(i)) then
buffer_count=12:goto Result_moji_check:
endif
next i
'13画の文字
for i=0 to ((Moji_13)-1)
if (name$=buf_char_hiragana13$(i)) then
buffer_count=13:goto Result_moji_check:
endif
next i
'14画の文字
for i=0 to ((Moji_14)-1)
if (name$=buf_char_hiragana14$(i)) then
buffer_count=14:goto Result_moji_check:
endif
next i
'15画の文字
for i=0 to ((Moji_15)-1)
if (name$=buf_char_hiragana15$(i)) then
buffer_count=15:goto Result_moji_check:
endif
next i
'16画の文字
for i=0 to ((Moji_16)-1)
if (name$=buf_char_hiragana16$(i)) then
buffer_count=16:goto Result_moji_check:
endif
next i
'17画の文字
for i=0 to ((Moji_17)-1)
if (name$=buf_char_hiragana17$(i)) then
buffer_count=17:goto Result_moji_check:
endif
next i
'18画の文字 25
for i=0 to ((Moji_18)-1)
if (name$=buf_char_hiragana18$(i)) then
buffer_count=18:goto Result_moji_check:
endif
next i
'19画の文字 17
for i=0 to ((Moji_19)-1)
if (name$=buf_char_hiragana19$(i)) then
buffer_count=19:goto Result_moji_check:
endif
next i
'20画の文字 13
for i=0 to ((Moji_20)-1)
if (name$=buf_char_hiragana20$(i)) then
buffer_count=20:goto Result_moji_check:
endif
next i
'21画の文字 6
for i=0 to ((Moji_21)-1)
if (name$=buf_char_hiragana21$(i)) then
buffer_count=21:goto Result_moji_check:
endif
next i
'22画の文字 4
for i=0 to ((Moji_22)-1)
if (name$=buf_char_hiragana22$(i)) then
buffer_count=22:goto Result_moji_check:
endif
next i
'23画の文字 3
for i=0 to ((Moji_23)-1)
if (name$=buf_char_hiragana23$(i)) then
buffer_count=23:goto Result_moji_check:
endif
next i
'24画の文字
for i=0 to ((Moji_24)-1)
if (name$=buf_char_hiragana24$(i)) then
buffer_count=24:goto Result_moji_check:
endif
next i
'Menu3 結果表示 画数の登録確認
Result_moji_check:
if (buffer_count=0) then
'登録文字がない場合の処理
'グラフィック領域　ここから
cls 3
line (0,0)-(850,40),rgb(0,0,255),bf
line (0,40)-(850,200),rgb(127,255,212),bf
line (0,203)-(853,323),rgb(0,0,255),bf
line (0,320)-(850,520),rgb(0,255,0),bf
'グラフィック領域　ここまで
color rgb(255,255,255):print"登録文字数結果確認"+chr$(13)
color rgb(255,0,0):print name$;"は、登録されていません"+chr$(13)
talk "この文字は、登録されていません"
color rgb(255,255,255):print"q+エンターキー:トップ画面"+chr$(13)
COLOR rgb(255,255,255):print"エンターキー:もう一度調べる"+chr$(13)
color rgb(0,0,0):print"コマンド:"+chr$(13)
key$=input$(1)
if key$="q" then goto Main_Screen:
if key$=chr$(13) then goto Menu2_moji_check:
else
'グラフィック描画領域　ここから
cls 3
line (0,0)-(850,52),rgb(0,0,255),bf
pen 5:line (0,0)-(853,55),rgb(255,255,255),b
line (0,52)-(850,102),rgb(127,255,212),bf
pen 5:line (0,55)-(847,99),rgb(0,0,0),b
line (0,102)-(850,260),rgb(0,0,255),bf
pen 5:line(0,105)-(847,257),rgb(255,0,255),b
line (0,260)-(850,360),rgb(0,255,0),bf
pen 5:line (0,257)-(853,363),rgb(0,0,0),b
'グラフィック描画領域　ここまで
color rgb(255,255,255):print "登録文字画数結果表示"
talk "この文字は"+ str$(buffer_count) +"かくでとうろくされています":color rgb(255,0,255):print name$;"は、";buffer_count;"画で登録されています":
endif
color rgb(255,255,255)
print"q+エンターキー:トップ画面"+chr$(13)
print"エンターキー:もう一度やる"+chr$(13)
color rgb(0,0,0)
print"コマンド:" + chr$(13)
key$=input$(1)
if key$=chr$(13)  then buffer_count=0:goto Menu2_moji_check:
if key$="q" then buffer_count=0:goto Main_Screen:
if not(key$="q") then goto Result_moji_check:
'Menu2 名前の陰陽を見る
'グラフィック描画領域 ここから
Input_name_InYo:
cls 3:line (0,0)-(820,60),rgb(0,0,255),bf
pen 5:line(0,0)-(823,63),rgb(255,255,255),b
LINE (0,60)-(820,250),rgb(127,255,212),bf
pen 5:line(0,57)-(817,247),rgb(255,0,255),b
line (0,250)-(820,350),rgb(0,255,0),bf
pen 5:line(0,253)-(823,353),rgb(0,0,0),b
'グラフィック描画領域　ここまで
color rgb(255,255,255)
print"Menu2 名前の陰陽を見る"+chr$(13)
COLOR rgb(255,0,255)
PRINT"(●:陽,○:陰)"+chr$(13)
print"調べたい名前苗字を入れてください"+chr$(13)
color rgb(0,0,0)
Input"調べたい名前の苗字:",name$
'Menu2 陰陽の吉凶を見る 名入力
cls:color rgb(255,255,255)
print"Menu2 名前の陰陽を見る"+chr$(13)
color rgb(255,0,255)
print"(●:陽,○:陰)"+chr$(13)
print"調べたい名前の名を入れてください"+chr$(13)
color rgb(0,0,0)
Input"調べたい名前の名:",name2$
for i=0 to len(name$)-1
name_array$(i)=Mid$(name$,i+1,1)
if char_count(name_array$(i)) mod 2 = 1  then
name_array$(i)="○":buffer$=buffer$+name_array$(i)
else
name_array$(i)="●":buffer$=buffer$+name_array$(i)
endif
next i
for i=0 to len(name2$)-1
name_array2$(i)=Mid$(name2$,i+1,1)
if char_count(name_array2$(i)) mod 2 = 1 then
name_array2$(i)="○":buffer2$=buffer2$+name_array2$(i)
else
name_array2$(i)="●":buffer2$=buffer2$+name_array2$(i)
endif
next i
bufname$=buffer$+buffer2$
'陰陽のタイプ ここから
select case (len(bufname$))
'2文字
case 2:
if ((bufname$="○●") or (bufname$="●○")) then buffer_name$="吉相:姓名2字の陰陽吉相"
'
'  endif
if ((bufname$="○○") or (bufname$="●●")) then buffer_name$ = "一律陰陽:大凶相"
'  endif
'3文字
case 3:
if (((bufname$)="●○○") or ((bufname$)="○○●") or ((bufname$)="●●○") or ((bufname$)="○●●")) then buffer_name$="吉相:姓名3字陰陽吉相"
if (((bufname$)="●○●") or ((bufname$)="○●○")) then buffer_name$="凶相:ハサミ陰陽"
if (((bufname$)="●●●") or ((bufname$)="○○○")) then buffer_name$="大凶相:一律陰陽"
'4文字
case 4:
if bufname$="●○●○" or bufname$="○●○●" or bufname$ ="●●○●" or bufname$ ="○○●○" or bufname$="●○●●" or bufname$="○●○○" then buffer_name$="吉相:陰陽吉相"
if bufname$="●○○●" or bufname$="○●●○" then buffer_name$="凶相:ハサミ陰陽"
if bufname$="○○○●" or bufname$="●●●○"  then buffer_name$="凶相:片寄り陰陽"
if bufname$="●●○○" or bufname$="○○●●"  then buffer_name$="大凶相:分離陰陽"
if bufname$ ="●●●●" or bufname$ = "○○○○"  then buffer_name$="大凶相:一律陰陽"
'5文字
case 5:
if bufname$="○●○○●" or bufname$="●○●●○" or bufname$="○●○●●" or bufname$="●○●○○" or bufname$="●○○●○" or bufname$="●○●●●" or bufname$="○●○○○" or bufname$="○○●○●" or  bufname$="●●○○●" or bufname$="●○○●●" or bufname$="○○●●○" or bufname$="○●●○○"  or bufname$="○●○○○" or bufname$="●●●○●" or bufname$="○●○●●" Then buffer_name$="吉相:陰陽吉相"
if bufname$="○○●○○" or bufname$="●●○●●" then buffer_name$="大凶相:中縛り陰陽"
if bufname$="●●●○○" or bufname$="○○○●●" then buffer_name$="大凶相:分離陰陽"
if bufname$="○●●●○" or bufname$="●○○○●" or bufname$="●○●○●" or bufname$="○●○●○" then buffr_name$="凶相:ハサミ陰陽"
case 6:
if bufname$="○○○○○○" or bufname$="●●●●●●" then buffer_name$="大凶相:一律陰陽"
if bufname$="●●●○○○" or bufname$="○○○●●●" then buffer_name$="大凶相:分離陰陽"
if bufname$="○○●●○○" or bufname$="●●○○●●" then buffer_name$="大凶相:中縛り陰陽"
if bufname$="○●○○○●" or bufname$="○●○●○○" or bufname$="●○●●●○" or bufname$="●○●○●●" or bufname$="●○○●○○" or bufname$="○●●○●●" then buffer_name$="吉相:陰陽吉相"
case else:
buffer_name$="例外に入りました"
end select
'陰陽のタイプ　ここまで
'グラフィック描画領域　ここから
Result_Inyo:
cls 3
line (0,0)-(900,60),rgb(0,0,250),bf
pen 5:line(0,0)-(903,63),rgb(255,255,255),b
line (0,60)-(900,250),rgb(127,255,212),bf
pen 5:line(0,57)-(897,297),rgb(255,0,255),b
line (0,250)-(900,460),rgb(0,255,0),bf
pen 5:line(0,247)-(897,457),rgb(0,0,0),b
'グラフィック描画領域 ここまで
color rgb(255,255,255)
cls:print"名前:";name$ + name2$ + chr$(13)
color rgb(255,0,255)
print"陰陽配列(陽:●,陰:○)";buffer$ + buffer2$ + chr$(13)
print buffer_name$;chr$(13)
color rgb(0,0,0)
print"エンターキー:トップ画面へ行く"+chr$(13)
print"S or s:保存する"+chr$(13)
key$=input$(1)
'1.メイン画面に行く
if key$=chr$(13) then buffer$="":buffer2$="": goto Main_Screen:
'2.データーの保存
if key$="S" or key$="s" then goto Save_Inyo_array:
'Menu3
Menu2_Entry_moji:
'登録文字数の確認
totalmoji=Moji_1+Moji_2+Moji_3+Moji_4+Moji_5+Moji_6+Moji_7+Moji_8+Moji_9+Moji_10+Moji_11+Moji_12+Moji_13+Moji_14+Moji_15+Moji_16+Moji_17+moji_18+Moji_19+Moji_20+Moji_21+Moji_21+moji_22+Moji_23+Moji_24:
'グラフィック描画領域　ここから
cls 3
line (0,0)-(780,60),rgb(0,0,255),bf
pen 5:line(0,0)-(777,57),rgb(255,255,255),b
line (0,60)-(780,170),rgb(127,255,212),bf
pen 5:line(0,64)-(783,173),rgb(0,0,255),b
line (0,167)-(777,267),rgb(0,255,0),bf
pen 5:line(0,173)-(783,273),rgb(0,0,0),b
'グラフィック描画領域 ここまで
color rgb(255,255,255)
PRINT"登録文字数の確認"+chr$(13)
color rgb(255,0,255)
PRINT"登録文字数は";totalmoji;"文字です"+chr$(13)
TALK"登録文字数は"+str$(totalmoji)+"もじです"
color rgb(0,0,0)
print"エンターキーを押してください"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Main_Screen:
'Menu2 文字の総数で名前の吉凶を見る（総数で判定)
Menu1_2_Total_name:
talk"名前の総数で吉凶を判定します。名前を入れてください"
'グラフィック描画領域　ここから
cls 3
line (0,0)-(650,60),rgb(0,0,250),bf
line (0,60)-(650,140),rgb(127,255,212),bf
line (0,140)-(650,220),rgb(0,255,0),bf
'グラフィック描画領域　ここまで
buf=0
color rgb(255,255,255)
Print"名前の総数で吉凶を判断します"+chr$(13)
color rgb(255,0,255)
PRINT"名前を入れてください"+chr$(13)
color rgb(0,0,0)
Input"名前:",name$
for n=0 to (len(name$)-1)
buf_Input_data$(n)=mid$(name$,n+1,1)
buf=buf+char_count(buf_Input_data$(n))
next n
cls:color rgb(255,255,255):print name$;"の総数:";buf;chr$(13)
color rgb(255,0,255):print name$;"の吉凶:";buf_Kikkyo$(buf-1);chr$(13)
color rgb(0,0,0):Input"エンターキーを押してください",key$
if key$="" then goto Main_Screen:
'名前2文字
'グラフィック描画領域　ここから
Menu2_Result_Anzai_Kikkyo:
cls 3
line (0,0)-(1000,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1000,57),rgb(255,255,255),b
line (0,60)-(1000,360),rgb(127,255,212),bf
pen 5:line(0,63)-(1000,363),rgb(255,0,255),b
line (0,360)-(1000,500),rgb(0,255,0),bf
pen 5:line(0,363)-(1000,503),rgb(0,0,0),b
'グラフィック描画領域 ここまで
font 32:cls:color rgb(255,255,255)
print "名前:";bufname$+bufname2$+chr$(13)
color rgb(255,0,255)
print"天運";buf_tenunn;chr$(13)
print"地運";buf_chiunn;",地運の吉凶(初年運:0〜20歳):";buf_Kikkyo_Anzai_chiunn$(buf_chiunn-1);chr$(13)
print"人運";buf_jinunn;",人運の吉凶(中年運:20〜50歳):";buf_Kikkyo_Anzai_jinunn$(buf_jinunn-2);chr$(13)
print"外運";buf_gaiunn;chr$(13)
print"総数";buf_total;",総運の吉凶(晩年運:50歳以上):";buf_Kikkyo_Anzai_total$(buf_total-2);chr$(13)
COLOR rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Main_Screen:
'参考文献 表示 ここから
'グラフィック描画領域　 ここから
Reference_book:
cls 3
line (0,0)-(720,60),rgb(0,0,255),bf
pen 5:line(0,0)-(723,63),rgb(255,255,255),b
line (0,60)-(720,420),rgb(127,255,212),bf
pen 5:line(0,63)-(723,423),rgb(255,255,255),b
line (0,420)-(720,500),rgb(0,255,0),bf
pen 5:line(0,423)-(723,503),rgb(0,0,0),b
'グラフィック描画領域  ここまで
'参考文献１
font 32
color rgb(255,255,255)
print"◎参考文献"+chr$(13)
color rgb(255,0,255)
print "参考文献１"+chr$(13)
print "Title:九星姓名判断"+chr$(13)
print "Author:高嶋　 美伶"+chr$(13)
print "出版社:日本文芸者"+chr$(13)
'font 32
print "ISBN:4-537-20073-1"+chr$(13)
'font 32
print "定価:1,200+税"+chr$(13)
color rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Reference_book2:
'参考文献２
Reference_book2:
cls:font 32
color rgb(255,255,255)
print "◎参考文献"+chr$(13)
color rgb(255,0,255)
print "参考文献２"+chr$(13)
print "Title:究極の姓名判断"+chr$(13)
print "Author:安斎　勝洋"+chr$(13)
print "出版社:説話社"+chr$(13)
print "ISBN:978-4-916217-61-5"+chr$(13)
print "定価:1,800円+税"+chr$(13)
color rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Reference_book3:
'参考文献３
Reference_book3:
cls:font 32
color rgb(255,255,255)
print"◎参考文献"+chr$(13)
color rgb(255,0,255)
print "参考文献3"+chr$(13)
print "Title:新明解現在漢和辞典"+chr$(13)
print "Author:影山輝國(編集主幹)他"+chr$(13)
print "出版社:三省堂"+chr$(13)
print "ISBN:978-4-385-13755-1"+chr$(13)
print "定価:2,800円 + 税"+chr$(13)
print "エンターキーを押してください"+chr$(13)
'トップ画面に戻る
key$=input$(1)
if key$=chr$(13) then goto Main_Screen:
'安斎流姓名判断　メニュー
'グラフィック領域　ここから
Menu1_2_Anzai_Top:
cls 3
line (0,0)-(1000,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1003,63),rgb(255,255,255),b
line (0,60)-(1000,450),rgb(127,255,212),bf
pen 5:line(0,63)-(1003,453),rgb(255,0,255),b
line (0,450)-(1000,630),rgb(0,255,0),bf
pen 5:line(0,453)-(1003,633),rgb(0,0,0),b
'グラフィック描画領域　ここまで
color rgb(255,255,255)
print "安斎流姓名判断　トップメニュー" + chr$(13)
color rgb(255,0,255)
print "1.安斎流姓名判断 名前の吉凶判定" + chr$(13)
print "2.安斎流姓名判断 改名チェック" + chr$(13)
print "3.安斎流姓名判断 相性占い"+chr$(13)
print "4.前の画面に戻る"+chr$(13)
color rgb(0,0,0)
print"番号を選んでください"+chr$(13)
Input "番号:",key
if key=1 then goto Menu2_Anzai_Kikkyo:
if key=2 then goto Menu2_Anzai_name_check:
if key=3 then goto Anzai_Aisyou_Top:
if key=4 then goto SeimeiHandan_Top:
'2.安斎流姓名判断　男女の相性占い　ここから
'グラフィック描画領域 ここから
Anzai_Aisyou2:
cls 3
line (0,0)-(850,60),rgb(0,0,255),bf
pen 5:line(0,0)-(853,63),rgb(255,255,255),b
line (0,60)-(850,180),rgb(127,255,212),bf
line (0,63)-(853,183),rgb(255,0,255),b
line (0,180)-(850,270),rgb(0,255,0),bf
pen 5:line(0,183)-(853,273),rgb(0,0,0),b
'グラフィック描画領域　男女の相性占い ここまで
's2=1:男
if s2 = 1 then goto Input_female:
's2=2:女
if s2 = 2 or s2=0  then goto Input_male:
'1.名前の姓を入力  男性
's2=2:女
's2=1:男
Input_male:
color rgb(255,255,255)
print "安斎流姓名判断　相性占い(男性)"+chr$(13)
color rgb(255,0,255)
print "男性の名前(姓)を入れてください"+chr$(13)
color rgb(0,0,0)
Input "男性の名前(姓):",name$
'2.名前の名を入力  男性
cls
color rgb(255,255,255)
print "安斎流姓名判断 相性占い(男性)"+chr$(13)
color rgb(255,0,255)
print "男性の名前（名）をいれてください"+chr$(13)
color rgb(0,0,0)
Input "男性の名前(名):",name2$
if s2=0 then goto Input_female:
's2=2:女
's2=1:男
if s2=2 then goto male_complate:
'3.名前(姓)入力 女性
Input_female:
cls
color rgb(255,255,255)
print "安斎流姓名判断 相性占い(女性)"+chr$(13)
color rgb(255,0,255)
print "女性の名前(姓)を入れてください"+chr$(13)
color rgb(0,0,0)
Input"女性の名前:",name3$
'4.名前（名)入力 女性
cls
color rgb(255,255,255)
print "安斎流姓名判断 相性占い(女性)"+chr$(13)
color rgb(255,0,255)
print "女性の名前（名）を入れてください"+chr$(13)
color rgb(0,0,0)
Input "女性の名前(名):",name4$
goto male_complate:
'男性　の地運 を求める
'姓名の合計数
'1.男性
male_complate:
buff_name1 = len(name$)
buff_name2 = len(name2$)
buff_name = buff_name1 + buff_name2
'診断結果に飛ぶ
'goto Anzai_Result_Aisyou_male_female:
'2.女性
'female_complate:
buff_name3 = len(name3$)
buff_name4 = len(name4$)
buff_wname = buff_name3 + buff_name4
'診断結果に飛ぶ
goto Anzai_Result_Aisyou:
'1.男性の地運を求める
select case buff_name
case 2:
'男性の地運を求める
buf_chiunn=char_count(name2$)
case 3:
'姓が一文字 名が2文字
if buff_name1=1 and buff_name2=2 then
buf_Input_name$(0)=mid$(name2$,1,1)
buf_Input_name$(1)=mid$(name2$,2,1)
'地運を計算
buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))
endif
if buff_name1=2 and buff_name2=1 then
buf_Input_name$(0)=Mid$(name2$,1,1)
buf_chiunn=char_count(buf_Input_name$(0))
endif
case 4:
'1.姓１，名３
if buff_name1=1 and buff_name2=3 then
buf_Input_name$(0)=Mid$(name2$,1,1)
buf_Input_name$(1)=Mid$(name2$,2,1)
buf_Input_name$(2)=Mid$(name2$,3,1)
buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))+char_count(buf_Input_name$(2))
endif
'2.姓２,名２
if buff_name1=2 and buff_name2=2 then
buf_Input_name$(0)=Mid$(name2$,1,1)
buf_Input_name$(1)=Mid$(name2$,2,1)
buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))
endif
'3.姓３、名１
if buff_name1=3 and buff_name2=1 then
buf_Input_name$(0)=Mid$(name2$,1,1)
'地運を求める
buf_chiunn=char_count(buf_Input_name$(0))
endif
case 5:
'1.姓２，名３
if buff_name1=2 and buff_name2=3 then
buf_Input_name$(0)=Mid$(name2$,1,1)
buf_Input_name$(1)=Mid$(name2$,2,1)
buf_Input_name$(2)=Mid$(name2$,3,1)
'地運を求める
buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))+char_count(buf_Input_name$(2))
endif
'2.姓３，名２
if buff_name1=3 and buff_name2=2 then
buf_Input_name$(0)=Mid$(name2$,1,1)
buf_Input_name$(1)=Mid$(name2$,2,1)
'地運を求める
buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))
endif
'3.姓４、名１
buf_Input_name$(0)=Mid$(name2$,1,1)
'地運を求める
buf_chiunn = char_count(buf_Input_name$(0))
case 6:
'1.姓３，名３
if buff_name1=3 and buff_name2=3 then
buf_Input_name$(0)=Mid$(name2$,1,1)
buf_Input_name$(1)=Mid$(name2$,2,1)
buf_Input_name$(2)=Mid$(name2$,3,1)
'地運を求める
buf_chiunn = char_count(buf_Input_name$(0)) + char_count(buf_Input_name$(1))+char_count(buf_Input_name$(2))
endif
'２.姓４，名２
if buff_name1=4 and buff_name2=2 then
buf_Input_name$(0)=Mid$(name2$,1,1)
buf_Input_name$(1)=Mid$(name2$,2,1)
'地運を求める
buf_chiunn = char_count(buf_Input_name$(0)) + char_count(buf_Input_name$(1))
endif
case else:
end select
'if s2=1 then goto Input_female:
'if s2=2 then goto Main_Screen:
'2.女性　外運を求める
select case buff_wname
'姓と名の合計数
case 2:
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name4$,1,1)
buf_gaiunn=char_count(buf_Input_name2$(0)) + char_count(buf_Input_name2$(1))
case 3:
if buff_name3=2 and buff_name4=1 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name4$,1,1)
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(2))
endif
if buff_name3=1 and buff_name4=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name4$,1,1)
buf_Input_name2$(2)=Mid$(name4$,2,1)
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(2))
endif
case 4:
'姓１、名３
if buff_name3=1 and buff_name4=3 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name4$,1,1)
buf_Input_name2$(2)=Mid$(name4$,2,1)
buf_Input_name2$(3)=Mid$(name4$,3,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(2))+char_count(buf_Input_name2$(3))
endif
'姓２,名２
if buff_name3=2 and buff_name4=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name4$,1,1)
buf_Input_name2$(3)=Mid$(name4$,2,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(3))
endif
'姓３，名１
if buff_name3=3 and buff_name4=1 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name4$,1,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name2$(3))
endif
case 5:
'姓 3,名2
if buff_name3=3 and buff_name=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name4$,1,1)
buf_Input_name2$(4)=Mid$(name4$,2,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name2$(4))
endif
'姓４、名１
if buff_name3=4 and buff_name4=1 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name3$,4,1)
buf_Input_name2$(4)=Mid$(name4$,1,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name$(4))
endif
'姓２、名３
if buff_name3=2 and buff_name4=3 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name4$,1,1)
buf_Input_name2$(3)=Mid$(name4$,2,1)
buf_Input_name2$(4)=Mid$(name4$,3,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(3))+char_count(buf_Input_name$(4))
endif
case 6:
'1.姓３，名３
if buff_name3=3 and buff_name4=3 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name4$,1,1)
buf_Input_name2$(4)=Mid$(name4$,2,1)
buf_Input_name2$(5)=Mid$(name4$,3,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name2$(4))+char_count(buf_Input_name$(5))
endif
'2.姓４，名２
if buff_name3=4 and buff_name4=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name3$,4,1)
buf_Input_name2$(4)=Mid$(name4$,1,1)
buf_Input_name2$(5)=Mid$(name4$,2,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name$(5))
endif
case 7:
'1姓４，名３
'外運を求める
end select
if s2=1 then goto Input_female:
'if s2=2 then goto Main_Screen:
if s2=0 or s2=2 then goto Anzai_Result_Aisyou:
func Aisyou_type$(man,woman)
Result$="No data"
'1.理解し合える最良のカップル
'1のFor文
for i=0 to 19
if buf_good_couple1(i) = man and buf_good_couple2(i) = woman then
Result$ = "1.理解し合える最良のカップル"
ResultNo = 0
endif
if buf_natural_couple1(i) = man and  buf_natural_couple2(i) = woman then
Result$="2.互いに自然体でつきあえるカップル"
ResultNo=1
endif
'3.男性にとって居心地の良いカップル
if buf_good_for_man1(i)= man AND buf_good_for_man2(i)= woman then
Result$="3.男性にとって居心地の良いカップル"
ResultNo=2
endif
'4.女性にとって居心地の良いカップル
if buf_good_for_woman1(i) = man AND buf_good_for_woman2(i) = woman then
Result$ = "4.女性にとって居心地の良いカップル"
ResultNo=3
endif
'5.恋愛経験を重ねた後なら愛を育める
'for i=0 to 9
'for j=0 to 9
if short_of_experience1(i) = man AND short_of_experience2(i) = woman then
Result$="5.恋愛経験を重ねた後なら愛を育める"
ResultNo=4
endif
'next j
'next i
'6
if buf_difficult_for_couple1(i)=man AND  buf_difficult_for_couple2(i)=woman then
Result$="6.結婚への発展が困難なカップル"
endif
'7
if buf_difference_of_love1(i)=man AND buf_difference_of_love2(i)=woman then
Result$="7.愛し方にずれが出てくる二人"
endif
'8
if buf_difference_of_KachiKan1(i)=man AND buf_difference_of_KachiKan2(i)=woman  then
Result$="8.互いに価値観が噛み合わない相性"
endif
next i
'
endfunc  Result$
'Anzai_Result_Aisyou_male_female:
'cls 3:
'buf_t_chiunn = buf_number(buf_chiunn)
'buf_t_gaiunn=buf_number(buf_gaiunn)
'bufferAisyou$ = Aisyou_type$(buf_t_chiunn,buf_t_gaiunn)
'グラフィック描画領域　ここから
Anzai_Result_Aisyou:
cls 3
line(0,0)-(1200,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1203,63),rgb(255,255,255),b
line(0,60)-(1200,560),rgb(127,255,212),bf
pen 5:line(0,63)-(1203,563),rgb(255,0,255),b
LINE(0,560)-(1200,770),rgb(0,255,0),bf
pen 5:line(0,563)-(1203,773),rgb(0,0,0),b
if s2=1 then goto　read_mydata:
if s2=0 then
bufname$ = name$ + name2$
bufname2$=name3$ + name4$
buf_chiunn=buf_number(get_chiunn(name2$))
buf_gaiunn=buf_number(get_gaiunn(name3$,name4$))
Aisyou$=Aisyou_type$(buf_chiunn,buf_gaiunn)
else
if s2=1 then
bufname$=Mid$(buffername$,3,len(buffername$))
bufname2$=name3$+name4$
buf_chiunn=val(Mid$(buffername3$,4,1))
buf_gaiunn=buf_number(get_gaiunn(name3$,name4$))
Aisyou$=Aisyou_type$(buf_chiunn,buf_gaiunn)
else
if s2=2 then
bufname$=name$+name2$
bufname2$=Mid$(buffername$,3,len(buffername$))
buf_gaiunn=val(Mid$(buffername3$,4,1))
buf_chiunn=buf_number(get_chiunn(name2$))
Aisyou$=Aisyou_type$(buf_chiunn,buf_gaiunn)
endif
endif
endif
'グラフィック描画領域　ここまで
color rgb(255,255,255)
print "安斎流姓名判断　相性占い　結果表示"+chr$(13)
color rgb(255,0,255)
print "男性の名前:";bufname$;chr$(13)
print "地運の単数";buf_chiunn;chr$(13)
print "女性の名前:";bufname2$;chr$(13)
print "外運の単数";buf_gaiunn;chr$(13)
print"二人の相性:";Aisyou$;chr$(13)
color rgb(0,0,0)
print "エンターキー:トップ画面へ行く"+chr$(13)
print "S or s:保存する"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Main_Screen:
if key$="s" or key$="S" then goto Save_Good_fortune:
'相性占い　データー保存
'Save_Good_fortune:
'1.ファイル：fortune_list.datが無い時
'1-0.保存フォルダ:config/Kikkyo_data/
'1-1.ファイル:fortune_list.datを作る
'1-2.異性の名前と自分との相性を保存
'1-3.データーを保存したと表示する
Save_Good_fortune:
open "config/Kikkyo_data/fortune_list.dat" for append as #1
'
close #1
'改名チェック
'グラフィック描画領域 ここから
Menu2_Anzai_name_check:
cls 3
'1行目
line (0,0)-(1300,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1297,57),rgb(255,255,255),b
'2行目
line (0,60)-(1300,450),rgb(127,255,212),bf
pen 5:line(0,63)-(1303,453),rgb(255,0,255),b
'3行目
line (0,450)-(1300,550),rgb(0,255,0),bf
pen 5:line(0,453)-(1303,553),rgb(0,0,0),b
'グラフィック描画領域 ここまで
talk"ニックネームまたは会社名から改名が必要かチェックします、10文字以内で名前を入れてください"
color rgb(255,255,255)
print "安斎流姓名判断 改名チェッカー"+chr$(13)
color rgb(255,0,255)
print "総数で、改名が、必要か否かを調べます"+chr$(13)
print "名前(ニックネーム or 会社名等)を" + chr$(13)
print "入れてください" + chr$(13)
print "10文字までで入れてください" + chr$(13)
color rgb(0,0,0)
Input "名前:",name$
'計算領域　ここから
'名前から総運を計算
'1.名前の文字数を出す
n=len(name$)
if n > 10 then
'文字数が10個までという表示
'トーストで表示
ui_msg "文字は10個までです"
goto Menu2_Anzai_name_check:
endif
'それ以外
'総数を出す処理を出す
'画面消去　データー初期化
Result_Anzai_Kaimei:
cls 3:bufer_total=0
'グラフィック描画領域 ここから
line(0,0)-(960,60),rgb(0,0,255),bf
pen 5:line(0,0)-(963,63),rgb(255,255,255),b
line(0,60)-(960,320),rgb(127,255,212),bf
pen 5:line(0,63)-(963,323),rgb(255,0,255),b
line(0,320)-(960,600),rgb(0,255,0),bf
pen 5:line(0,323)-(963,603),rgb(0,0,0),b
'グラフィック描画領域 ここまで
'cls 3
color rgb(255,255,255)
print"安斎流姓名判断　改名チェック　診断結果"+chr$(13)
for i=0 to len(name$)-1
buf_Input_name$(i)=Mid$(name$,i+1,1)
bufer_total = char_count(buf_Input_name$(i)) + bufer_total
next i
color rgb(255,0,255)
print "名前:";name$
print "この名前の総数:";bufer_total
print "この名前の吉凶:";buf_Kikkyo_Anzai_total$(bufer_total - 2);"です"
color rgb(255,0,255)
buffer_total$=Kaimei_check$(buf_Kikkyo_Anzai_total$(bufer_total - 2 ))
print bufer_total$;chr$(13)
color rgb(0,0,0)
locate 0,7
print"エンターキー:メイン画面"+chr$(13)
print"スペース:もう一度やる"+chr$(13)
print"s or S:保存する"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Main_Screen:
if key$=" "  then goto Menu2_Anzai_name_check:
if key$="S" or key$="s" then
count=0
if dir$("config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat")="" then
count=0
else
open "config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat" for input as #1
while eof(1)=0
line input #1,lines$:count=count+1
wend
close #1
endif
if count => 5 then
ui_msg"登録できるのは5件までです。"
goto Result_Anzai_Kaimei:
else
if count<6 then
open "config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat" for append as #1
print #1,"名前:";name$;",総数:";str$(bufer_total);",吉凶:";buf_Kikkyo_Anzai_total$(bufer_total-2)
close #1
ui_msg"保存しました"
endif
endif
endif
goto Result_Anzai_Kaimei:
func Kaimei_check$(r$)
if r$="大吉数" or r$="中吉数" or r$="小吉数" or r$="半吉数" or r$="六大吉数" or r$="吉数" then
'改名の必要なし
talk"この名前は改名の必要がございません エンターキーを押してください"
print"この名前は、改名の必要はありません"+chr$(13)
'改名の必要ナシの場合
K=0
else
'改名の必要あり
talk"この名前は、改名の必要があります"
print"この名前は、改名の必要があります"+chr$(13)
K=1
'◎表示パターン
'パターン１
'color rgb(0,0,0)
'Input"エンターキーを押してください",key$
endif
endfunc result$
'ui_msg="1件追加しました,残り9件追加できます"
'ファイルの存在確認 設定ファイル:mydata.dat
Anzai_Result_Aisyou_male_female:
cls 3
'設定ファイルの確認
if dir$("config/Mydata/mydata.dat")="" then
'print "File not found"+chr$(13)
'1.ファイルがない時
goto Input_profile:
else
'ファイルがある時
goto read_mydata:
endif
'1-1.ファイルがない時の処理
'1-1.自分の姓名判断を入力
'グラフィック描画領域 ここから
Input_profile:
cls 3
line(0,0)-(1000,60),rgb(0,0,250),bf
line(0,60)-(1000,150),rgb(127,255,212),bf
line(0,150)-(1000,240),rgb(0,255,0),bf
'グラフィック描画領域 ここまで
'1.名前の姓の入力
'1行目 Title
color rgb(255,255,255)
print"安斎流姓名判断　相性占い　自分のプロフィール入力"+chr$(13)
'2行目 名前の姓を入力
color rgb(255,0,255)
print "自分の名前の姓を入れてください"+chr$(13)
color rgb(0,0,0)
'buf_name1$:自分の名前の姓
Input"名前の姓:",buf_name1$
'2.名前の名の入力
'画面消去
cls
'1行目 Title
color rgb(255,255,255)
print "安斎流姓名判断 相性占い 自分のプロフィール入力"+chr$(13)
'2行目 名前の名の入力
color rgb(255,0,255)
print "自分の名前の名を入れてください"+chr$(13)
color rgb(0,0,0)
'buf_name2$:自分の名前の名
input "名前の名:",buf_name2$
'3.性別入力
cls
'1行目 Title
color rgb(255,255,255)
print "安斎流姓名判断 相性占い 自分のプロフィール入力"+chr$(13)
'2行目 性別入力
color rgb(255,0,255)
print"自分の性別を入れてください(女性 or 男性)"+chr$(13)
sex_type$(0)="女性"
sex_type$(1)="男性"
type=ui_select("sex_type$","性別を選んでください")
'3行目
'性別変数 sex_type$
if type=1 then
print"自分の性別(女性 or 男性):";sex_type$(1)+chr$(13):sex_type$=sex_type$(1):goto Input_male_:
else
print"自分の性別(女性 or 男性):";sex_type$(0)+chr$(13):sex_type$=sex_type$(0):goto Input_female_:
endif
Input_male_:
if sex_type$ = "男性" then
name$=buf_name1$
name2$=buf_name2$
's2=1:男性
s2=1
goto Entry_Profile:
endif
Input_female_:
If sex_type$="女性" then
name3$=buf_name1$
name4$=buf_name2$
's2=2:女性
s2=2
goto Entry_Profile:
endif
'登録プロフィール確認画面
Entry_Profile:
cls 3
'グラフィック描画領域　ここから
line(0,0)-(1000,60),rgb(0,0,255),bf
line(0,60)-(1000,60*5),rgb(127,255,212),bf
line(0,300)-(1000,460),rgb(0,255,0),bf
'グラフィック描画領域　ここまで
'1行目 Title
color rgb(255,255,255)
print"安斎流姓名判断 自分のプロフィール　登録確認"+chr$(13)
color rgb(255,0,255)
print"名前:";buf_name1$+buf_name2$;chr$(13)
PRINT"性別:";sex_type$;chr$(13)
if sex_type$="女性" then
buf_gaiunn = buf_number(get_gaiunn(buf_name1$,buf_name2$))
print"外運:";buf_gaiunn;chr$(13)
buffer$="外運:"+str$(buf_gaiunn)
endif
if sex_type$="男性" then
buf_chiunn = buf_number(get_chiunn(buf_name2$))
print"地運:";buf_chiunn;chr$(13)
buffer$="地運:" + str$(buf_chiunn)
endif
color rgb(0,0,0)
input"(登録する:Yes/登録しない:No):",key$
if key$="Yes" or key$="yes" then
open "config/Mydata/mydata.dat" for output as #1
print #1,"名前:";buf_name1$+buf_name2$
print #1,"性別:";sex_type$
print #1,buffer$
close #1
ui_msg"データーを保存しました。"
goto Main_Screen:
Else
goto Main_Screen:
ui_msg"データーを保存していません。"
endif
'２．設定ファイルが存在する場合
'ファイル読み込み 自分のデーターを表示
read_mydata:
cls 3:line=0
open "config/Mydata/mydata.dat" for input as #2
while eof(2) = 0
Select case line
case 0:
line input #2,a$
line = line + 1
case 1:
line input #2,b$
line = line + 1
case 2:
line input #2,c$
line = line + 1
end Select
wend
close #2
line=0
buffername$ = a$
buffername2$ = b$
buffername3$ = c$
bufff$ = Mid$(buffername$,1,3)
buff2$ = Mid$(buffername2$,1,3)
buff3$ = Mid$(buffername3$,1,3)
n=len(a$)
sextype$=Mid$(buffername2$,4,2)
if sextype$ = "男性" then
'男性:s2=1
s2=1
'女性のデーター入力
goto Anzai_Aisyou2:
else
if sextype$="女性" then
s2=2
'男性のデーター入力
goto Anzai_Aisyou2:
endif
endif
'女性のデーター結果表示
'1.プロフィール性別男性の場合 ここから
cls
color rgb(255,255,255)
print "安斎流姓名判断　 相性占い　結果表示"+chr$(13)
name_length = len(bufername$)
myname$ = Mid$(buffername$,4,4)
chiunn = val(Mid$(buffername3$,4,2))
tansuu1 = buf_number(chiunn)
bufname$ = name3$ + name4$
bufferAisyou$ = Aisyou_type$(tansuu1,buf_t_gaiunn)
color rgb(255,0,255)
print "自分の名前(男性):";myname$;chr$(13)
print "地運の端数:";tansuu1;chr$(13)
print "相手の名前(女性):";bufname$;chr$(13)
print "外運の端数:";buf_t_gaiunn;chr$(13)
print "二人の相性:";bufferAisyou$;chr$(13)
talk "二人の相性は";bufferAisyou$
color rgb(0,0,0)
print"エンターキー:トップ画面へ行く"+chr$(13)
print"S or s:データーを保存"+chr$(13)
key$=input$(1)
'トップ画面に行く
if key$ = chr$(13) then goto Main_Screen:
if key$="s" or key$="S" then goto Save_my_fortune_list:
'データー保存
'ファイル名:my_fortune_list_male.dat
'フォルダ:config/
'Save_my_fortune_list:相性リストに保存
Save_my_fortune_list:
open "config/Mydata/my_fortune_list_male.dat" for append as #1
print #1,"相手の名前:";bufname$;",あなたとの相性:";bufferAisyou$
close #1
ui_msg "保存しました"
goto Main_Screen:
'1-1.データーを保存する
'1-2.appendモードで保存
'1-3.ui_msgで保存したと表示する
'1-4.終了する
'1.プロフィール性別男性の場合　ここまで
'2.プロフィール性別女性の場合　ここから
Anzai_Aisyou_female:
cls 3
'描画領域　ここから
line (0,0)-(1200,60),rgb(0,0,255),bf
line (0,60)-(1200,560),rgb(127,255,212),bf
LINE (0,560)-(1200,780),rgb(0,255,0),bf
'描画領域 ここまで
color rgb(255,255,255)
print"安斎流姓名判断　相性占い　結果表示"+chr$(13)
'計算領域　ここから
myname$=Mid$(buffername$,4,4)
buf_gaiunn=val(Mid$(buffername3$,4,1))
buf_t_gaiunn=buf_number(buf_gaiunn)
buf_t_chiunn=buf_number(buf_chiunn)
bufferAisyou$ = Aisyou_type$(buf_t_chiunn,buf_t_gaiunn)
bufname$ = name$ + name2$
'計算領域 ここまで
color rgb(255,0,255)
print "自分の名前(女性):";myname$;chr$(13)
print "外運の端数";buffer_t_gaiunn;chr$(13)
print "相手の名前(男性):";bufname$;chr$(13)
print "地運の端数:";buf_t_chiunn;chr$(13)
print "二人の相性:";bufferAisyou$;chr$(13)
color rgb(0,0,0)
print "エンターキー:トップ画面へ行く"+chr$(13)
print "S or s:データー保存"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Main_Screen:
'2.プロフィール性別女性の場合　ここまで
'endfunc result$
'取扱説明書
'相性占い　トップメニュー ここから
'グラフィック ここから
Anzai_Aisyou_Top:
cls 3:
line(0,0)-(1100,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1103,63),rgb(255,255,255),b
line(0,60)-(1100,450),rgb(127,255,212),bf
pen 5:line(0,63)-(1103,453),rgb(255,0,255),b
line(0,450)-(1100,650),rgb(0,255,0),bf
pen 5:line(0,453)-(1103,653),rgb(0,0,0),b
'グラフィック　ここまで
color rgb(255,255,255)
print"安斎流姓名判断 　相性占い トップメニュー"+chr$(13)
color rgb(255,0,255)
print"1.自分と異性の相性"+chr$(13)
print"2.男女の相性"+chr$(13)
print"3.前の画面に戻る"+chr$(13)
print"4.トップ画面に戻る"+chr$(13)
color rgb(0,0,0)
print"番号を選んでください:"+chr$(13)
Input"番号:",key
if key=1 then goto Anzai_Result_Aisyou_male_female:
if key=2 then s2=0:goto Anzai_Aisyou2:
if key=3 then goto Menu1_2_Anzai_Top:
if key=4 then goto Main_Screen:
if key>4 or key=0  then goto Anzai_Aisyou_Top:
'グラフィック領域　ここから
cls 3:line(0,0)-(1150,60),rgb(0,0,255),bf
line(0,60)-(1150,180),rgb(127,255,212),bf
line(0,120)-(1150,200),rgb(0,255,0),bf
'グラフィック領域　ここまで
'誕生日入力　生まれた年
color rgb(255,255,255)
print"誕生日入力　生まれた年"+chr$(13)
COLOR rgb(255,0,255)
print"生まれた年を入れてください"+chr$(13)
color rgb(0,0,0)
Input"生まれた年:",year
'誕生日入力 生まれた月
cls
color rgb(255,255,255)
print"誕生日入力 生まれた月"+chr$(13)
color rgb(255,0,255)
print"生まれた月を入力してください"+chr$(13)
color rgb(0,0,0)
Input"生まれた月:",month
'誕生日入力 生まれた日を入力
cls
color rgb(255,255,255)
print"誕生日入力 生まれた日入力"+chr$(13)
color rgb(255,0,255)
print"生まれた日を入力してください"+chr$(13)
color rgb(0,0,0)
Input"生まれた日:",day
'人生の転機を見る
'グラフィック領域　ここから
cls 3
line(0,0)-(1150,60),rgb(0,0,255),bf
line(0,60)-(1150,470),rgb(127,255,212),bf
line(0,470)-(1150,520),rgb(0,255,0),bf
'グラフィック描画領域  ここまで
cls
color rgb(255,255,255)
print"人生の転機を見る"+chr$(13)
color rgb(255,0,255)
buf_total2=buf_total-(fix(buf_total/10)*10)
print"誕生日";year;"年";month;"月";day;"日"+chr$(13)
print year+buf_total2;"年";buf_total2;"歳"+chr$(13)
print year+buf_total2+5;"年";buf_total2+5;"歳"+chr$(13)
Member_List_Top:
cls 3
line (0,0)-(950,60),rgb(0,0,255),bf
pen 5:line(0,0)-(947,57),rgb(255,255,255),b
line (0,60)-(950,460),rgb(127,255,212),bf
pen 5:LINE(0,63)-(953,463),rgb(255,0,255),b
line (0,463)-(950,640),rgb(0,255,0),bf
pen 5:line(0,457)-(953,643),rgb(0,0,0),b
color rgb(255,255,255)
print "●メンバーリスト トップ"+chr$(13)
color rgb(255,0,255)
print "1.安斎流姓名判断　改名チェックリスト"+chr$(13)
print "2.安斎流姓名判断 相性メンバーリスト"+chr$(13)
print "3.九星姓名判断　陰陽配列"+chr$(13)
print "4.トップ画面に行く"+chr$(13)
color rgb(0,0,0)
print "番号を選んでください"+chr$(13)
Input "番号:",key
if key=1 then goto Anzai_Rename_Entry_List:
if key=2 then goto Anzai_Aisyou_Member_list_Top:
if key=3 then goto Kyusei_Inyou_Array_List:
if key=4 then goto Main_Screen:
if key>4 or key=0 then goto Member_List_Top:
'1.安斎流姓名判断改名リスト
Anzai_Rename_Entry_List:
if dir$("config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat")="" then
cls 3
line (0,0)-(850,60),rgb(0,0,255),bf
pen 5:line(0,0)-(853,63),rgb(255,255,255),b
line (0,60)-(850,180),rgb(127,255,212),bf
pen 5:line(0,63)-(853,183),rgb(255,0,255),b
line (0,180)-(850,250),rgb(0,255,0),bf
pen 5:line(0,183)-(853,253),rgb(0,0,0),b
color rgb(255,255,255)
cls :print"1.安斎流姓名判断改名リスト"+chr$(13)
color rgb(255,0,255)
print "登録データーは、ございません"+chr$(13)
color rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
key$=Input$(1)
if key$=chr$(13) then goto Main_Screen:
else
counts=0
open "config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat" for input as #2
while eof(2) = 0
line input #2,line$:counts = counts + 1
wend
close #2
cls 3:
line (0,0)-(900,60),rgb(0,0,255),bf
pen 5:line(0,0)-(903,63),rgb(255,255,255),b
line (0,60)-(900,180),rgb(127,255,212),bf
pen 5:line(0,63)-(903,183),rgb(255,0,255),b
line (0,180)-(900,250),rgb(0,255,0),bf
pen 5:line(0,183)-(903,253),rgb(0,0,0),b
color rgb(255,255,255)
print "1.安斎流姓名判断改名リスト登録件数"+chr$(13)
color rgb(255,0,255)
print "登録件数:";counts;"件"+chr$(13)
talk "登録件数は"+str$(counts)+"件です。"
color rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
endif
key$ = input$(1)
if key$ = chr$(13) then goto Show_Entry_list:
'
'
Show_Entry_list:
open "config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat" for input as #1
for i=0 to 3 * counts - 1
input #1,buf_lines$(i)
next i
close #1
n=0:cls 3
'グラフィック領域　ここから
line (0,0)-(1300,60),rgb(0,0,255),bf
pen 5:line(0,0)-(1303,63),rgb(255,255,255),b
line (0,60)-(1300,100*counts+60),rgb(124,255,212),bf
pen 5:line(0,63)-(1303,100*counts+60+3),rgb(255,0,255),b
line (0,100*counts+60)-(1300,60+100*counts+120),rgb(0,255,0),bf
pen 5:line(0,100*counts+60+3)-(1300,60+100*counts+120+3),rgb(0,0,0),b
'グラフィック領域　ここまで
'count:登録件数
while n < counts
font 48:color rgb(255,255,255):PRINT"安斎流姓名判断　ネームリスト"+chr$(13):color rgb(255,0,255):print n+1 ; ".";buf_lines$(3*n+0) ; ",";buf_lines$(3*n+1);",";buf_lines$(3*n+2);chr$(13)
n=n+1
wend
color rgb(0,0,0)
print"エンターキーを押してください"+chr$(13)
key$=INPUT$(1)
if key$=chr$(13) then goto Main_Screen:
Anzai_Aisyou_Member_list_Top:
n=0
if dir$("config/Mydata/my_fortune_list_male.dat")="" then
goto Empty_member_list:
else
goto show_Aisyou_member_List:
endif
'
show_Aisyou_member_List:
'ファイルを読み込む 1.登録件数、登録データーを読み込む
'1.ファイルがない時　ファイルがありませんと表示する
if dir$("config/Mydata/my_fortune_list_male.dat") = " " then
'1.ファイルがない時、ファイルがありませんと表示させる
'グラフィック描画領域 ここから
line (0,0)-(1200,60),rgb(0,0,255),bf
line (0,60)-(1200,273),rgb(127,255,212),bf
line (0,270)-(1200,500),rgb(0,255,0),bf
'グラフィック描画領域　ここまで
print "ファイルがありません" + chr$(13)
else
'n:ファイルを開いて、登録件数の確認
open "config/Mydata/my_fortune_list_male.dat" for input as #1
while eof(1) = 0
line input #1,lines$:n=n+1
'input #1,buffLines$(n)
'n = n + 1
wend
close #1
'ファイルを開いて、配列にデーターを入力する
open "config/Mydata/my_fortune_list_male.dat" for input as #2
for i=0 to n*2-2
input #2,buffLines$(i)
next i
close #2
'ファイル読み込み　ここまで
N=0
'グラフィック領域　ここから
cls 3
line (0,0)-(1200,60),rgb(0,0,255),bf
pen 5:line (0,0)-(1203,63),rgb(255,255,255),b
line (0,60)-(1200,160),rgb(127,255,212),bf
pen 5:line (0,63)-(1203,163),rgb(255,0,255),b
line (0,160)-(1200,250),rgb(0,255,0),bf
pen 5:line(0,163)-(1203,253),rgb(0,0,0),b
'グラフィック領域　ここまで
color rgb(255,255,255)
print "安斎流姓名判断　相性メンバーリスト 登録件数"+chr$(13)
color rgb(255,0,255)
print "登録件数:";n;"件"+chr$(13)
color rgb(0,0,0)
print "エンターキーを押してください"+chr$(13)
key$=input$(1)
if key$ = chr$(13) then goto Aisyou_List:
endif
'安斎流姓名判断　相性リスト　メンバー表示
Aisyou_List:
cls 3:
line (0,0)-(1300,60),rgb(0,0,255),bf
pen 5:line (0,0)-(1303,63),rgb(255,255,255),b
line (0,60)-(1300,260),rgb(127,255,212),bf
pen 5:line(0,63)-(1303,263),rgb(255,0,255),b
line (0,260)-(1300,470),rgb(0,255,0),bf
line (0,263)-(1303,473),rgb(0,0,0),b
color rgb(255,255,255)
print "安斎流姓名判断 相性メンバーリスト 登録件数";count+1;"件目"+chr$(13)
while n > count - 1
color rgb(255,0,255)
print buffLines$(2*count+0)+chr$(13)
print buffLines$(2*count+1)+chr$(13)
color rgb(0,0,0)
print "エンターキー:前の画面"+chr$(13)
print "スペースキー:次のリストへ行く"+chr$(13)
key$=input$(1)
if key$=chr$(13) then goto Member_List_Top:
if key$=" " then
count= ((count + 1) Mod n)
if n = count then ui_msg"データーが一杯です.":count = ((count + 1) Mod n)
goto Aisyou_List:
endif
wend
'空っぽのときの表示
Empty_member_list:
cls 3
line(0,0)-(850,60),rgb(0,0,255),bf
pen 5:line(0,0)-(853,183),rgb(255,255,255),b
line(0,60)-(850,180),rgb(127,255,212),bf
pen 5:line(0,63)-(853,183),rgb(255,0,255),b
line (0,180)-(850,250),rgb(0,255,0),bf
pen 5:line(0,183)-(853,253),rgb(0,0,0),b
color rgb(255,255,255)
print "安斎流姓名判断　相性メンバーリスト" + chr$(13)
color rgb(255,0,255)
print "登録メンバーはいません" + chr$(13)
color rgb(0,0,0)
print"エンターキーを押してください" + chr$(13)
key$ = input$(1)
if key$=chr$(13) then goto Main_Screen:
Remove_MemberList:
cls 3:
line (0,0)-(1200,60),rgb(0,0,255),bf
line (0,3)-(1197,57),rgb(255,255,255),b
line (0,60)-(1202,362),rgb(127,255,212),bf
line (0,57)-(1197,357),rgb(255,0,255),b
line (0,357)-(1197,557),rgb(0,255,0),bf
line(0,360)-(1200,560),rgb(0,0,0),b
color rgb(255,255,255)
print"メンバーリストの編集(安斎流姓名判断)" + chr$(13)
color rgb(255,0,255)
print "1.改名チェックリストの編集" + chr$(13)
print "2.相性メンバーリストの編集" + chr$(13)
print "3.トップ画面へ行く" + chr$(13)
color rgb(0,0,0)
print"番号を選んでください" + chr$(13)
Input"番号:",No
if No = 0 or No > 3 then goto Remove_MemberList:ui_msg "番号をもう一度、入れ直してください "
if No=1 then
If dir$("config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat") = "" then
ui_msg"そのファイルはありません":goto Remove_MemberList:
else
kill "config/Anzai_Kikkyo/Anzai_Kikkyo_name_list.dat":ui_msg"改名チェックリストを削除しました":goto Remove_MemberList:
endif
endif
if No=2 then
if dir$("config/Mydata/my_fortune_list_male.dat")="" then
ui_msg"そのファイルはありません":goto Remove_MemberList:
else
kill "config/Mydata/my_fortune_list_male.dat":ui_msg"相性メンバーリストを削除しました":goto Remove_MemberList:
endif
endif
if No=3 then goto Main_Screen:
'九星姓名判断 1.陰陽配列の保存
Save_Inyo_array:
open "config/Inyo_list/Inyo_array.dat" for append as #1
print #1,"名前:";name$ + name2$;",陰陽配列:";buffer$ + buffer2$
close #1
ui_msg "保存しました"
'九星姓名判断 陰陽配列 List
Kyusei_Inyou_Array_List:
cls 3
'グラフィック領域　ここから
Line (0,0)-(1200,60),rgb(0,0,255),bf
line (0,0)-(1197,57),rgb(255,255,255),b
line (0,60)-(1203,273),rgb(127,255,212),bf
line (0,63)-(1200,270),rgb(255,0,255),b
line (0,270)-(1200,500),rgb(0,255,0),bf
line (0,273)-(1203,503),rgb(0,0,0),b
'グラフィック描画領域　ここまで
'ファイル読み込み  ここから
if dir$("config/Inyo_list/Inyo_array.dat")="" then
mode=0
else
mode=1
'登録件数を求める
open "config/Inyo_list/Inyo_array.dat" for input as #1
bcount=0
while eof(1)=0
line input #1,bufLine$:bcount = bcount + 1
wend
close #1
open "config/Inyo_list/Inyo_array.dat" for input as #2
for i=0 to 2 * bcount - 1
input #2,count1$(i)
next i
close #2
endif
'ファイル読み込み  ここまで
'1行目　文字色　白
'1.ファイルがない時
color rgb(255,255,255)
print "九星姓名判断 陰陽配列　リスト" + chr$(13)
if mode=0 then
color rgb(255,0,255)
print "ファイルがありません"+chr$(13)
else
'2.ファイルが有る時
color rgb(255,0,255)
'bcount:登録件数
select case bcount
'登録件数が一件のとき
case 1:
print count1$(0) + chr$(13)
print count1$(1) + chr$(13)
color rgb(0,0,0)
print "エンターキー:トップへ行く" + chr$(13)
key$ = input$(1)
if key$ = chr$(13) then goto Main_Screen:
'登録件数が2件以上の時
case else:
n = 0
while (1)
l=n mod bcount
if l => 0 then
'登録件数がn件のときの
cls
color rgb(255,255,255)
print"九星姓名判断 陰陽配列 リスト"+chr$(13)
color rgb(255,0,255)
print count1$(2*l + 0) + chr$(13)
'l = l + 1
print count1$(2*l + 1) + chr$(13)
print"スペースキー:次へ行く" + chr$(13)
print"エンターキー:トップへ行く" + chr$(13)
key$ = input$(1)
if key$ = " " then n = (n + 1) Mod bcount
if key$ = Chr$(13) then goto Main_Screen:
endif
wend
end Select
endif
func get_chiunn(bufername2$)
buff_name=len(bufername2$)
'1.男性の地運を求める
select case buff_name
case 1:
'男性の地運を求める
buf_chiunn=char_count(buffername2$)
case 2:
'姓が一文字 名が2文字
if buff_name=2 then
buf_Input_name$(0)=mid$(bufername2$,1,1)
buf_Input_name$(1)=mid$(bufername2$,2,1)
'地運を計算
buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))
endif
'if buff_name1=2 and buff_name2=1 then
'buf_Input_name$(0)=Mid$(bufname2$,1,1)
'buf_chiunn=char_count(buf_Input_name$(0))
'endif
case 3:
'1.姓１，名３
if buff_name=3 then
buf_Input_name$(0)=Mid$(bufername2$,1,1)
buf_Input_name$(1)=Mid$(bufername2$,2,1)
buf_Input_name$(2)=Mid$(bufername2$,3,1)
buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))+char_count(buf_Input_name$(2))
endif
'2.姓２,名２
'if buff_name1=2 and buff_name2=2 then
'buf_Input_name$(0)=Mid$(bufername2$,1,1)
'buf_Input_name$(1)=Mid$(bufername2$,2,1)
'buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))
'endif
'3.姓３、名１
'if buff_name1=3 and buff_name2=1 then
' buf_Input_name$(0)=Mid$(bufname2$,1,1)
'地運を求める
'buf_chiunn=char_count(buf_Input_name$(0))
'endif
'case 5:
'1.姓２，名３
'if buff_name1=2 and buff_name2=3 then
'buf_Input_name$(0)=Mid$(bufname2$,1,1)
'buf_Input_name$(1)=Mid$(bufname2$,2,1)
'buf_Input_name$(2)=Mid$(bufname2$,3,1)
'地運を求める
'buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))+char_count(buf_Input_name$(2))
' endif
'2.姓３，名２
'if buff_name1=3 and buff_name2=2 then
'buf_Input_name$(0)=Mid$(bufname2$,1,1)
'buf_Input_name$(1)=Mid$(bufname2$,2,1)
'地運を求める
'buf_chiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(1))
'endif
'3.姓４、名１
'buf_Input_name$(0)=Mid$(bufname2$,1,1)
'地運を求める
'buf_chiunn = char_count(buf_Input_name$(0))
'case 6:
'1.姓３，名３
'if buff_name1=3 and buff_name2=3 then
'buf_Input_name$(0)=Mid$(bufname2$,1,1)
''buf_Input_name$(1)=Mid$(bufname2$,2,1)
'buf_Input_name$(2)=Mid$(bufname2$,3,1)
'地運を求める
'buf_chiunn = char_count(buf_Input_name$(0)) + char_count(buf_Input_name$(1))+char_count(buf_Input_name$(2))
'endif
'２.姓４，名２
'if buff_name1=4 and buff_name2=2 then
'buf_Input_name$(0)=Mid$(bufname2$,1,1)
'buf_Input_name$(1)=Mid$(bufname2$,2,1)
'地運を求める
'buf_chiunn = char_count(buf_Input_name$(0)) + char_count(buf_Input_name$(1))
'endif
'case else:
'chiunn=buf_chiunn
end select
endfunc buf_chiunn
func get_gaiunn(name3$,name4$)
buff_name3 = len(name3$)
buff_name4 = len(name4$)
buff_wname = buff_name3 + buff_name4
select case buff_wname
'姓と名の合計数
case 2:
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name4$,1,1)
buf_gaiunn=char_count(buf_Input_name2$(0)) + char_count(buf_Input_name2$(1))
case 3:
if buff_name3=2 and buff_name4=1 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name4$,1,1)
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(2))
endif
if buff_name3=1 and buff_name4=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name4$,1,1)
buf_Input_name2$(2)=Mid$(name4$,2,1)
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(2))
endif
case 4:
'姓１、名３
if buff_name3=1 and buff_name4=3 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name4$,1,1)
buf_Input_name2$(2)=Mid$(name4$,2,1)
buf_Input_name2$(3)=Mid$(name4$,3,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(2))+char_count(buf_Input_name2$(3))
endif
'姓２,名２
if buff_name3=2 and buff_name4=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name4$,1,1)
buf_Input_name2$(3)=Mid$(name4$,2,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(3))
endif
'姓３，名１
if buff_name3=3 and buff_name4=1 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name4$,1,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name2$(3))
endif
case 5:
'姓 3,名2
if buff_name3=3 and buff_name=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name4$,1,1)
buf_Input_name2$(4)=Mid$(name4$,2,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name2$(4))
endif
'姓４、名１
if buff_name3=4 and buff_name4=1 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name3$,4,1)
buf_Input_name2$(4)=Mid$(name4$,1,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name$(4))
endif
'姓２、名３
if buff_name3=2 and buff_name4=3 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name4$,1,1)
buf_Input_name2$(3)=Mid$(name4$,2,1)
buf_Input_name2$(4)=Mid$(name4$,3,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name$(0))+char_count(buf_Input_name$(3))+char_count(buf_Input_name$(4))
endif
case 6:
'1.姓３，名３
if buff_name3=3 and buff_name4=3 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name4$,1,1)
buf_Input_name2$(4)=Mid$(name4$,2,1)
buf_Input_name2$(5)=Mid$(name4$,3,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name2$(4))+char_count(buf_Input_name$(5))
endif
'2.姓４，名２
if buff_name3=4 and buff_name4=2 then
buf_Input_name2$(0)=Mid$(name3$,1,1)
buf_Input_name2$(1)=Mid$(name3$,2,1)
buf_Input_name2$(2)=Mid$(name3$,3,1)
buf_Input_name2$(3)=Mid$(name3$,4,1)
buf_Input_name2$(4)=Mid$(name4$,1,1)
buf_Input_name2$(5)=Mid$(name4$,2,1)
'外運を求める
buf_gaiunn=char_count(buf_Input_name2$(0))+char_count(buf_Input_name2$(1))+char_count(buf_Input_name$(5))
endif
case 7:
'1姓４，名３
'外運を求める
end select
endfunc buf_gaiunn
func buf_number(a)
if a < 10 then
buf_tansu=a
endif
if a>9 and a < 20 then
buf_tansu = a - 10
endif
if a>19 and a < 30 then
buf_tansu = a - 20
endif
endfunc buf_tansu
