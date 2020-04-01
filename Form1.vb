Public Class Form1 '人物基礎傷害傷害10,30
    Dim bgi As Bitmap, action(80) As Bitmap, skill(40) As Bitmap, fire(12) As Bitmap, action2(40) As Bitmap, mbgi As Bitmap
    Dim dragon(10) As Bitmap, iaction(80) As Bitmap, iaction2(40) As Bitmap
    Dim deep(80) As Bitmap, deepaction(40) As Bitmap, iskill(30) As Bitmap, thunder(12) As Bitmap
    Dim pc(80) As Bitmap, pc1(14) As Bitmap, wind(10) As Bitmap, magic(50) As Bitmap, pc2(80) As Bitmap
    Dim ice(6) As Bitmap, done As Boolean = True, pcball(12) As Bitmap, pcha(8) As Bitmap, boom(15) As Bitmap
    Dim extra(4) As Bitmap, maxextra(4) As Bitmap, dice(6) As Bitmap
    Dim characters(2) As Bitmap, cnum As Integer = 0 'characters num
    Dim firenblood As Single = 100, bossblood As Single = 200, freezeblood As Single = 100, deepblood As Single = 100
    Dim damagetimes As Integer = 1, choose As Boolean = False, playerturn As Boolean = True '玩家的回合
    Dim ignite As Boolean = False, frozen As Boolean = False, week As Boolean = False '附加效果(點燃 冰凍(ok) 虛弱)
    Dim firedamage As Integer = 0 '灼燒傷害
    Dim manx As Integer = 40, many As Integer = 210, pcx As Integer = 400, pcy As Integer = 190
    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Randomize() : Button1.Hide() : Button2.Hide() : Label4.Hide()
        characters(0) = My.Resources.firen_f : characters(1) = My.Resources.freeze_f : characters(2) = My.Resources.deep_f
        bgi = My.Resources.未命名 'create(陣列,圖片,張數,每幾張換行,X,Y)
        create(action, My.Resources.firen_0, 80, 10, 80, 80) : create(skill, My.Resources.firen_2, 40, 10, 80, 80)
        create(fire, My.Resources.firen_ball, 12, 4, 85, 80) : create(action2, My.Resources.firen_1, 40, 10, 80, 80)
        create(dragon, My.Resources.firen_chase, 10, 2, 320, 150) : create(magic, My.Resources.john_ball, 50, 10, 80, 80)
        create(iaction, My.Resources.freeze_0, 80, 10, 80, 80) : create(iaction2, My.Resources.freeze_1, 40, 10, 80, 80)
        create(ice, My.Resources.freeze_col2, 6, 6, 140, 140) : create(deepaction, My.Resources.deep_2, 40, 10, 80, 80)
        create(deep, My.Resources.deep_0, 80, 10, 80, 80) : create(iskill, My.Resources.freeze_2, 30, 10, 80, 80)
        create(pc, My.Resources.julian_0, 80, 10, 80, 100) : create(pc1, My.Resources.julian_1, 14, 7, 110, 120)
        create(pc2, My.Resources.julian_2, 80, 10, 80, 100) : create(pcball, My.Resources.julian_ball, 12, 4, 80, 80)
        create(thunder, My.Resources.thunder, 12, 12, 100, 510) : create(wind, My.Resources.freeze_ww, 10, 5, 160, 160)
        create(pcha, My.Resources.julian_ball2, 8, 4, 110, 100) : create(boom, My.Resources.julian_exp, 15, 5, 160, 160)
        create(extra, My.Resources.kamahameha, 4, 0, 405, 200) : create(maxextra, My.Resources.julian_col, 4, 4, 200, 405)
        dice(1) = My.Resources.point1 : dice(2) = My.Resources.point2 : dice(3) = My.Resources.point3 : dice(4) = My.Resources.point4 : dice(5) = My.Resources.point5 : dice(6) = My.Resources.point6
        Graphics.FromImage(bgi).DrawImage(action(1), manx, many) : Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy)
        PictureBox1.Image = bgi : PictureBox2.Image = characters(cnum) : PictureBox3.Image = My.Resources.julian_f : PictureBox4.Image = My.Resources.bloodline : PictureBox5.Image = My.Resources.bloodline2
    End Sub
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Button1.Hide() : Button2.Hide() : playerturn = False : choose = False
        If cnum = 0 Then fireball()
        If cnum = 1 Then ices()
        If cnum = 2 Then sword()
    End Sub
    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Button1.Hide() : Button2.Hide() : playerturn = False : choose = False
        If cnum = 0 Then firedragon()
        If cnum = 1 Then hasaki()
        If cnum = 2 Then hit69()
    End Sub
    Sub delay(ByVal time As Single)
        Dim tt As Date = Now.AddSeconds(time)
        Do Until Now > tt
            Application.DoEvents()
        Loop
    End Sub
    Sub create(ByRef pic() As Bitmap, ByRef orginal As Bitmap, ByVal count As Integer, ByVal n1 As Integer, ByVal cut As Integer, ByVal cuty As Integer)
        Dim x As Integer = 0, y As Integer = -cuty
        For i = 1 To count
            pic(i) = New Bitmap(cut, cuty)
            If n1 = 0 Then x = 0 : y = y + cuty Else If i Mod n1 = 1 Then x = 0 : y = y + cuty
            Graphics.FromImage(pic(i)).DrawImage(orginal, New Rectangle(0, 0, cut, cuty), New Rectangle(x, y, cut, cuty), GraphicsUnit.Pixel)
            x = x + cut
        Next
    End Sub
    Sub icicle(ByVal i As Integer, ByVal x As Integer, ByVal y As Integer) '冰柱
        Graphics.FromImage(bgi).DrawImage(ice(i), x, y)
        If i < 2 Then extramode()
        If i = 3 Then Graphics.FromImage(bgi).DrawImage(pc(1), pcx + 10, pcy - 10) : less(False, 2.5)
        If i = 4 Then Graphics.FromImage(bgi).DrawImage(pc(1), pcx + 20, pcy)
        If i > 4 Then Graphics.FromImage(bgi).DrawImage(pc(1), pcx + 20, pcy)
        PictureBox1.Image = bgi
    End Sub
    Sub fireball() '10
        If done = False Then Exit Sub Else done = False
        Label3.Text = "接招吧! 地獄火焰"
        Dim fi As Integer = 10, firex As Integer = manx + 80, firen As Integer = 1, bleed As Boolean = False
        Dim hited As Boolean = False, pchit As New List(Of Bitmap), pcn As Integer = 0, back As Integer = 10
        pchit.Add(pc1(10)) : pchit.Add(pc1(11)) : ignite = True : firedamage = firedamage + 10
        For i = 1 To 19
            bgi = My.Resources.未命名 : extramode()
            If i = 11 Then i = 13
            If i Mod 2 = 1 Then If fi = 10 Then fi = 9 Else fi = 10
            If i < 14 Then
                Graphics.FromImage(bgi).DrawImage(fire(fi), firex, many - 80)
                Graphics.FromImage(bgi).DrawImage(fire(fi), firex, many)
                Graphics.FromImage(bgi).DrawImage(fire(fi), firex, many + 80)
            Else
                Graphics.FromImage(bgi).DrawImage(fire(firen), firex, many - 80)
                Graphics.FromImage(bgi).DrawImage(fire(firen), firex, many)
                Graphics.FromImage(bgi).DrawImage(fire(firen), firex, many + 80) : firex = firex + 50 : If firen = 6 Then firen = 1 Else firen = firen + 1
            End If
            Graphics.FromImage(bgi).DrawImage(skill(i), manx, many)
            PictureBox1.Image = bgi
            delay(0.09)
        Next
        Do Until firex > 655
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(skill(19), manx, many)
            Graphics.FromImage(bgi).DrawImage(fire(firen), firex, many - 80)
            If hited = False Then Graphics.FromImage(bgi).DrawImage(fire(firen), firex, many)
            Graphics.FromImage(bgi).DrawImage(fire(firen), firex, many + 80)
            If firex >= pcx Then
                If bleed = False Then less(False, 10) : bleed = True
                Graphics.FromImage(bgi).DrawImage(pchit(pcn), pcx + back, pcy + 20) : hited = True
                If pcn = 1 Then pcn = 0 Else pcn = 1 : back = back + 10
            Else
                Graphics.FromImage(bgi).DrawImage(fire(firen), firex, many)
            End If
            firex = firex + 50 : If firen = 6 Then firen = 1 Else firen = firen + 1
            PictureBox1.Image = bgi
            delay(0.09)
        Loop
        For i = 1 To 10
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(skill(19), manx, many)
            Graphics.FromImage(bgi).DrawImage(pchit(pcn), pcx + back, pcy + 20)
            If pcn = 1 Then pcn = 0 Else pcn = 1
            PictureBox1.Image = bgi
            delay(0.09)
        Next
        bgi = My.Resources.未命名
        Graphics.FromImage(bgi).DrawImage(action(1), manx, many)
        Graphics.FromImage(bgi).DrawImage(pc(10), pcx + back, pcy)
        PictureBox1.Image = bgi
        done = True : Timer1.Enabled = True : frozen = False
    End Sub
    Sub firedragon() '30
        If done = False Then Exit Sub Else done = False
        Label3.Text = "翱翔吧! 龍之吐息"
        Dim hited As Boolean = False, pchit As New List(Of Bitmap), pcn As Integer = 0, back As Integer = 10
        pchit.Add(pc1(10)) : pchit.Add(pc1(11)) : extramode()
        Dim move As New List(Of Bitmap), dragonx As Integer = 80, dc As Integer = 1
        For i = 1 To 5
            move.Add(action2(5))
        Next
        For i = 6 To 8
            move.Add(action2(i))
        Next
        For i = 0 To move.Count - 1
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(move(i), manx, many) : extramode()
            If i >= 6 Then
                Graphics.FromImage(bgi).DrawImage(dragon(dc), dragonx, 150)
                Graphics.FromImage(bgi).DrawImage(dragon(dc), dragonx, 100) : dc = dc + 1 : dragonx = dragonx + 40
            End If
            PictureBox1.Image = bgi
            delay(0.09)
        Next
        Do Until dc = 10
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(move(move.Count - 1), manx, many)
            Graphics.FromImage(bgi).DrawImage(dragon(dc), dragonx, 150)
            Graphics.FromImage(bgi).DrawImage(dragon(dc), dragonx, 100) : dc = dc + 1 : dragonx = dragonx + 40
            If dragonx >= 160 Then
                Graphics.FromImage(bgi).DrawImage(pchit(pcn), pcx + back, pcy + 20) : hited = True
                If dc <> 9 Then less(False, 5)
                If pcn = 1 Then pcn = 0 Else pcn = 1 : back = back + 10
            End If
            PictureBox1.Image = bgi
            delay(0.09)
        Loop
        For i = 1 To 10
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(action(1), manx, many)
            Graphics.FromImage(bgi).DrawImage(pchit(pcn), pcx + back, pcy + 20)
            If pcn = 1 Then pcn = 0 Else pcn = 1
            PictureBox1.Image = bgi
            delay(0.09)
        Next
        bgi = My.Resources.未命名
        Graphics.FromImage(bgi).DrawImage(action(1), manx, many)
        Graphics.FromImage(bgi).DrawImage(pc(10), pcx + back, pcy)
        PictureBox1.Image = bgi
        done = True : Timer1.Enabled = True : frozen = False
    End Sub
    Sub ices() '10
        If done = False Then Exit Sub Else done = False
        Label3.Text = "ice make!! 冰霜之柱"
        Dim move As New List(Of Bitmap)
        move.Add(iaction2(8)) : move.Add(iaction2(9)) : move.Add(iaction2(10)) : move.Add(iaction2(18)) : move.Add(iaction2(19)) : move.Add(iaction2(20))
        For i = 0 To move.Count - 1
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(move(i), manx, many) : extramode()
            PictureBox1.Image = bgi
            delay(0.09)
        Next
        For i = 1 To 6
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(move(move.Count - 1), manx, many)
            icicle(i, 95, many - 60)
            If i = 2 Then icicle(1, 215, many - 60) : icicle(2, 215, many - 60) : icicle(1, 335, many - 60) : icicle(2, 335, many - 60) : icicle(1, 455, many - 60) : icicle(2, 455, many - 60)
            If i = 3 Then icicle(3, 215, many - 60) : icicle(3, 335, many - 60) : icicle(3, 455, many - 60)
            If i = 4 Then icicle(4, 215, many - 60) : icicle(4, 335, many - 60) : icicle(4, 455, many - 60)
            If i = 5 Then icicle(5, 215, many - 60) : icicle(5, 335, many - 60) : icicle(5, 455, many - 60)
            If i = 6 Then icicle(6, 215, many - 60) : icicle(6, 335, many - 60) : icicle(6, 455, many - 60)
            delay(0.09)
        Next
        background(My.Resources.未命名, manx, many, 80, 80) '把背景定義於mbgi
        Graphics.FromImage(bgi).DrawImage(ice(6), 95, many - 60)
        Graphics.FromImage(bgi).DrawImage(iaction(1), manx, many)
        PictureBox1.Image = bgi
        done = True : Timer1.Enabled = True : frozen = True
    End Sub
    Sub hasaki() '30
        If done = False Then Exit Sub Else done = False
        Label3.Text = "結凍吧! 冰之龍捲"
        Dim windc As Integer = 1, x As Integer = 0
        For i = 11 To 19
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(iskill(i), manx, many)
            If i > 13 Then
                Graphics.FromImage(bgi).DrawImage(wind(windc), x, 100)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x, 200)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 320, 100)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 320, 200)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 160, 200)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 160, 100) : x = x + 40 : windc = windc + 1
                If i Mod 2 = 0 Then pcx = pcx + 10 Else pcx = pcx - 10
                pcy = pcy - 10
                Graphics.FromImage(bgi).DrawImage(pc(1), pcx, pcy) : less(False, 1) '6
            Else
                extramode()
            End If
            PictureBox1.Image = bgi
            delay(0.09)
        Next
        For i = 1 To 2
            Do Until windc = 10
                bgi = My.Resources.未命名
                Graphics.FromImage(bgi).DrawImage(iaction(1), manx, many)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x, 100)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x, 200)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 320, 200)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 320, 200)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 160, 200)
                Graphics.FromImage(bgi).DrawImage(wind(windc), x + 160, 100) : x = x + 40 : windc = windc + 1
                If windc Mod 2 = 0 Then pcx = pcx + 10 Else pcx = pcx - 10
                pcy = pcy - 10
                Graphics.FromImage(bgi).DrawImage(pc(1), pcx, pcy) : less(False, 1) '6
                PictureBox1.Image = bgi
                delay(0.09)
            Loop
            windc = 1
        Next
        Do Until pcy = 190
            background(My.Resources.未命名, pcx, pcy, 114, 120) '把背景定義於mbgi
            If pcy + 50 > 190 Then pcy = 190 Else pcy = pcy + 50
            Graphics.FromImage(bgi).DrawImage(pc(1), pcx, pcy)
            PictureBox1.Image = bgi
            delay(0.09)
        Loop : less(False, 12)
        Dim wave As New List(Of Bitmap)
        wave.Add(pc(2)) : wave.Add(pc1(2)) : wave.Add(pc1(1))
        For i = 0 To wave.Count - 1
            If i = 0 Then background(My.Resources.未命名, pcx, pcy, 80, 100) Else background(My.Resources.未命名, pcx, pcy, 114, 120) '把背景定義於mbgi
            Graphics.FromImage(bgi).DrawImage(wave(i), pcx, pcy)
            PictureBox1.Image = bgi
            delay(0.09)
        Next
        delay(0.5) : background(My.Resources.未命名, pcx, pcy, 114, 120) '把背景定義於mbgi
        pcx = 400 : pcy = 190 : Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy) : PictureBox1.Image = bgi
        done = True : Timer1.Enabled = True : frozen = False
    End Sub
    Sub sword()
        If done = False Then Exit Sub Else done = False
        Label3.Text = "你躲不掉的! 萬象天引!!"
        Dim move As New List(Of Bitmap), hited As Integer = 0, pchit As New List(Of Bitmap) : bgi = My.Resources.未命名
        pchit.Add(pc2(9)) : pchit.Add(pc2(10))
        For i = 31 To 37
            move.Add(deepaction(i))
        Next
        For i = 37 To 31 Step -1
            move.Add(deepaction(i))
        Next
        For i = 0 To move.Count - 1
            background(My.Resources.未命名, pcx, pcy, 80, 100) : background(My.Resources.未命名, manx, many, 80, 80)
            If i < 4 Then
                If i < 3 Then extramode() Else Graphics.FromImage(bgi).DrawImage(pc2(5), pcx, pcy)
            Else
                If i = 5 Or i = 10 Then less(False, 5)
                Graphics.FromImage(bgi).DrawImage(pchit(hited), pcx, pcy) : If hited = 1 Then hited = 0 Else hited = 1
            End If
            Graphics.FromImage(bgi).DrawImage(move(i), manx, many)
            If i = 2 Then                     'attract pc
                Dim mc As Integer = 42, check As Boolean = False
                Do Until pcx = manx + 40
                    background(My.Resources.未命名, pcx, pcy, 80, 100) : background(My.Resources.未命名, manx + 50, many, 80, 80)
                    Graphics.FromImage(bgi).DrawImage(move(i), manx, many)
                    If pcx - 80 < manx + 40 Then pcx = manx + 40 : check = True Else pcx = pcx - 50
                    If mc <= 50 And check = False Then Graphics.FromImage(bgi).DrawImage(magic(mc), manx + 50, many) : mc = mc + 1
                    Graphics.FromImage(bgi).DrawImage(pc2(5), pcx, pcy)
                    PictureBox1.Image = bgi : If check = False Then delay(0.09)
                Loop
            End If
            PictureBox1.Image = bgi
            If i <> 6 And i <> move.Count - 1 Then delay(0.09)
        Next
        background(My.Resources.未命名, manx, many, 80, 80) : background(My.Resources.未命名, pcx, pcy, 80, 100) : manx = 40 : many = 210 : pcx = 400 : pcy = 190
        Graphics.FromImage(bgi).DrawImage(deep(1), manx, many) : Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy)
        PictureBox1.Image = bgi
        done = True : Timer1.Enabled = True : frozen = False : week = True
    End Sub
    Sub hit69()
        If done = False Then Exit Sub Else done = False
        Label3.Text = "解放吧! 閃電劍!"
        bgi = My.Resources.未命名
        Dim move As New List(Of Bitmap), pchit As New List(Of Bitmap), hitn As Integer = 0, lasthit As Integer = 0
        pchit.Add(pc2(10)) : pchit.Add(pc2(9)) : pchit.Add(pc1(3)) : pchit.Add(pc1(10)) : pchit.Add(pc1(11))
        For i = 21 To 30
            move.Add(deepaction(i))
        Next
        For i = 40 To 37 Step -1
            move.Add(deepaction(i))
        Next
        For i = 0 To move.Count - 1
            background(My.Resources.未命名, manx - 20, many, 80, 80) : If lasthit >= 2 Then background(My.Resources.未命名, manx + 50, pcy, 110, 120) Else background(My.Resources.未命名, manx + 50, pcy, 80, 100)
            Graphics.FromImage(bgi).DrawImage(pchit(hitn), manx + 50, pcy) : lasthit = hitn : If hitn = 2 Then hitn = 0 Else hitn = hitn + 1
            If i = 1 Or i = 2 Or i = 6 Or i = 7 Or i = 10 Or i = 11 Then less(False, 1)
            Graphics.FromImage(bgi).DrawImage(move(i), manx, many)
            PictureBox1.Image = bgi : If i < move.Count - 1 Then delay(0.09) : manx = manx + 10
        Next
        For i = move.Count - 1 To 0 Step -1
            background(My.Resources.未命名, manx - 20, many, 80, 80) : If lasthit >= 2 Then background(My.Resources.未命名, manx + 50, pcy, 110, 120) Else background(My.Resources.未命名, manx + 50, pcy, 80, 100)
            Graphics.FromImage(bgi).DrawImage(pchit(hitn), manx + 50, pcy) : lasthit = hitn : If hitn = 2 Then hitn = 0 Else hitn = hitn + 1
            If i = 1 Or i = 2 Or i = 6 Or i = 7 Or i = 10 Or i = 11 Then less(False, 1)
            Graphics.FromImage(bgi).DrawImage(move(i), manx, many)
            PictureBox1.Image = bgi : If i < move.Count - 1 Then delay(0.09) : manx = manx + 10
        Next
        Dim lastx As Integer = 0, lasty As Integer = 0
        For i = 1 To 10
            background(My.Resources.未命名, manx - 20, many, 80, 80) : background(My.Resources.未命名, lastx, lasty, 110, 120)
            If i > 5 And i <= 8 Then many = many - 20
            If i > 8 Then many = many + 20
            lastx = manx + 20 : lasty = pcy '上一步的雙軸
            Graphics.FromImage(bgi).DrawImage(pchit(2), manx + 20, pcy) : If i <= 3 Then pcy = pcy - 10 * i * i + 1 Else pcy = pcy + 20
            If i = 2 Or i = 3 Or i = 4 Or i = 8 Or i = 9 Then less(False, 1)
            If hitn = 2 Then hitn = 1 Else hitn = hitn + 1
            Graphics.FromImage(bgi).DrawImage(deepaction(i), manx, many)
            PictureBox1.Image = bgi : delay(0.09) : manx = manx + 10
        Next : hitn = 3
        For i = 1 To 12
            For j = 1 To 6
                background(My.Resources.未命名, 100 * j - 100, 0, 100, 510)
                Graphics.FromImage(bgi).DrawImage(thunder(i), 100 * j - 100, 0)
            Next
            Graphics.FromImage(bgi).DrawImage(deepaction(20), manx, many)
            If hitn = 3 Then hitn = 4 Else hitn = 3
            Graphics.FromImage(bgi).DrawImage(pchit(hitn), manx + 30, pcy) : less(False, 1)
            PictureBox1.Image = bgi : delay(0.01)
        Next
        bgi = My.Resources.未命名
        Graphics.FromImage(bgi).DrawImage(deep(1), manx, many)
        Graphics.FromImage(bgi).DrawImage(pc1(1), manx + 30, pcy - 10) : less(False, 1)
        PictureBox1.Image = bgi : delay(0.5)
        manx = 40 : many = 210 : pcx = 400 : pcy = 190
        bgi = My.Resources.未命名
        Graphics.FromImage(bgi).DrawImage(deep(1), manx, many)
        Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy) : PictureBox1.Image = bgi
        done = True : Timer1.Enabled = True : frozen = False
    End Sub
    Private Sub PictureBox2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox2.Click
        changecharacters()
    End Sub
    Sub background(ByVal bg As Bitmap, ByVal x As Integer, ByVal y As Integer, ByVal width As Integer, ByVal high As Integer) '玩家身後的背景
        mbgi = New Bitmap(width, high)
        Graphics.FromImage(mbgi).DrawImage(bg, New Rectangle(0, 0, width, high), New Rectangle(x, y, width, high), GraphicsUnit.Pixel)
        Graphics.FromImage(bgi).DrawImage(mbgi, x, y)
    End Sub
    Sub kamahameha() '50
        If done = False Then Exit Sub Else done = False
        Label3.Text = "           被黑暗吞噬吧 暗影之球"
        Dim bossact As New List(Of Bitmap), hax As Integer = pcx - 110, hay As Integer = pcy, hac As Integer = 4, mc As Integer = 0
        bossact.Add(pc2(40)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(50)) : bossact.Add(pc2(49)) : bossact.Add(pc2(48))
        Dim manhit As New List(Of Bitmap), last As Bitmap = New Bitmap(80, 80)
        character(manhit, last)
        bgi = My.Resources.未命名 : Graphics.FromImage(bgi).DrawImage(last, manx, many)
        For i = 0 To bossact.Count - 1
            background(bgi, pcx, pcy, 80, 100)
            If i > 8 Then
                background(My.Resources.未命名, hax + 80, hay, 110, 100) : Graphics.FromImage(bgi).DrawImage(pcha(hac), hax, hay) : If hac = 4 Then hac = 3 Else hac = 4
                hax = hax - 80
            End If
            Graphics.FromImage(bgi).DrawImage(bossact(i), pcx, pcy)
            PictureBox1.Image = bgi : delay(0.09)
        Next
        Do Until hax = manx
            background(My.Resources.未命名, hax + 80, hay, 110, 100) : Graphics.FromImage(bgi).DrawImage(pcha(hac), hax, hay) : If hac = 4 Then hac = 3 Else hac = 4
            If hax - 80 < manx Then hax = manx Else hax = hax - 80
            PictureBox1.Image = bgi : delay(0.09)
        Loop
        less(True, 50)
        For i = 1 To 12
            background(My.Resources.未命名, hax - 30, hay - 50, 160, 160) : Graphics.FromImage(bgi).DrawImage(boom(i), hax - 30, hay - 50)
            If i > 6 Then Graphics.FromImage(bgi).DrawImage(manhit(mc), manx, many) : If mc = 0 Then mc = 1 Else mc = 0
            PictureBox1.Image = bgi : delay(0.09)
        Next
        For i = 1 To 3
            For j = 0 To 1
                background(My.Resources.未命名, manx, many, 80, 80) : Graphics.FromImage(bgi).DrawImage(manhit(j), manx, many)
                PictureBox1.Image = bgi : delay(0.09)
            Next
        Next
        bgi = My.Resources.未命名 : Graphics.FromImage(bgi).DrawImage(last, manx, many)
        Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy) : PictureBox1.Image = bgi : done = True
    End Sub
    Sub extrakamahameha() '75
        If done = False Then Exit Sub Else done = False
        Label3.Text = "                    防看看吧! 地獄轟炸"
        Dim bossact As New List(Of Bitmap), hax As Integer = pcx - 405, hay As Integer = pcy, hac As Integer = 4, mc As Integer = 0
        bossact.Add(pc2(40)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(39)) : bossact.Add(pc2(38)) : bossact.Add(pc2(50)) : bossact.Add(pc2(49)) : bossact.Add(pc2(48))
        Dim manhit As New List(Of Bitmap), last As Bitmap = New Bitmap(80, 80)
        character(manhit, last)
        bgi = My.Resources.未命名 : Graphics.FromImage(bgi).DrawImage(last, manx, many)
        For i = 0 To bossact.Count - 1
            background(My.Resources.未命名, pcx, pcy, 80, 100)
            If i > 8 Then
                background(My.Resources.未命名, hax, hay - 50, 405, 200) : Graphics.FromImage(bgi).DrawImage(extra(hac), hax, hay - 50) : hac = hac - 1
            End If
            Graphics.FromImage(bgi).DrawImage(bossact(i), pcx, pcy)
            PictureBox1.Image = bgi : delay(0.09)
        Next
        For i = 1 To 6
            If i = 6 Then
                Do Until hax = -405
                    hax = hax - 100 : background(My.Resources.未命名, hax + 100, hay - 50, 405, 200)
                    If hax <= -305 Then Graphics.FromImage(bgi).DrawImage(manhit(mc), manx, many) : If mc = 0 Then mc = 1 Else mc = 0
                    Graphics.FromImage(bgi).DrawImage(extra(hac), hax, hay - 50) : If hac = 1 Then hac = 3 Else hac = hac - 1
                    PictureBox1.Image = bgi : delay(0.09)
                Loop
            Else
                For j = 3 To 1 Step -1
                    less(True, 5)
                    background(My.Resources.未命名, hax, hay - 50, 405, 200) : Graphics.FromImage(bgi).DrawImage(extra(j), hax, hay - 50) : PictureBox1.Image = bgi : delay(0.09)
                Next
            End If
        Next
        For i = 1 To 5
            For j = 0 To 1
                background(My.Resources.未命名, manx, many, 80, 80) : Graphics.FromImage(bgi).DrawImage(manhit(j), manx, many)
                PictureBox1.Image = bgi : delay(0.09)
            Next
        Next
        bgi = My.Resources.未命名 : Graphics.FromImage(bgi).DrawImage(last, manx, many)
        Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy) : PictureBox1.Image = bgi : done = True
    End Sub
    Sub bossextra()
        If done = False Then Exit Sub Else done = False
        Label3.Text = "       就這樣下地獄吧! 死亡之刑"
        Dim flash As New List(Of Bitmap), bossact As New List(Of Bitmap), bc As Integer = 0, attack As New List(Of Bitmap)
        Dim manhit As New List(Of Bitmap), last As Bitmap = New Bitmap(80, 80) : character(manhit, last)
        bgi = My.Resources.未命名 : Graphics.FromImage(bgi).DrawImage(last, manx, many)
        For i = 17 To 12 Step -1
            flash.Add(pc2(i))
        Next
        For i = 47 To 43 Step -1
            bossact.Add(pc2(i))
            If i = 45 Then bossact.Add(pc2(46)) : bossact.Add(pc2(45))
        Next
        For i = 1 To 4
            attack.Add(maxextra(i))
        Next
        For i = 1 To 10
            attack.Add(maxextra(3)) : attack.Add(maxextra(4))
        Next
        For i = 0 To flash.Count - 1 '殘相拳
            For j = 0 To i
                background(My.Resources.未命名, pcx - 80 * j, pcy, 80, 100)
                Graphics.FromImage(bgi).DrawImage(flash(i), pcx - 80 * j, pcy)
            Next
            resetman()
            PictureBox1.Image = bgi : delay(0.1)
        Next
        Do Until bc = bossact.Count - 1 '集氣
            For i = 0 To flash.Count - 1
                background(My.Resources.未命名, pcx - 80 * i, pcy, 80, 100)
                Graphics.FromImage(bgi).DrawImage(bossact(bc), pcx - 80 * i, pcy)
                resetman()
            Next
            PictureBox1.Image = bgi : delay(0.09)
            bc = bc + 1
        Loop : bc = 0
        Do Until bc = attack.Count - 1
            bgi = My.Resources.未命名
            For i = 0 To flash.Count - 1
                If i = 0 Then Graphics.FromImage(bgi).DrawImage(attack(bc), pcx - 60, pcy - 295) Else Graphics.FromImage(bgi).DrawImage(attack(bc), pcx - 60 - 80 * i, pcy - 295)
            Next
            PictureBox1.Image = bgi : delay(0.09)
            bc = bc + 1 : less(True, 5)
        Loop
        bgi = My.Resources.未命名
        For j = 0 To flash.Count - 1
            If j = 0 Then Graphics.FromImage(bgi).DrawImage(attack(0), pcx - 60, pcy - 295) Else Graphics.FromImage(bgi).DrawImage(attack(0), pcx - 60 - 80 * j, pcy - 295)
        Next
        PictureBox1.Image = bgi
        bgi = My.Resources.未命名
        Graphics.FromImage(bgi).DrawImage(pc2(43), pcx, pcy) : delay(0.09) : PictureBox1.Image = bgi
        background(My.Resources.未命名, pcx, pcy, 80, 100) : Graphics.FromImage(bgi).DrawImage(pc2(42), pcx, pcy) : delay(0.09) : PictureBox1.Image = bgi
        background(My.Resources.未命名, pcx, pcy, 80, 100) : Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy) : PictureBox1.Image = bgi
        many = 10
        Do Until many = 210
            For j = 0 To 1
                background(My.Resources.未命名, manx, many - 50, 80, 80) : Graphics.FromImage(bgi).DrawImage(manhit(j), manx, many)
                PictureBox1.Image = bgi : delay(0.09) : many = many + 50
            Next
        Loop
        background(My.Resources.未命名, manx, many - 50, 80, 80) : Graphics.FromImage(bgi).DrawImage(manhit(0), manx, many)
        PictureBox1.Image = bgi : delay(0.09)
        For i = 1 To 2
            For j = 1 To 0 Step -1
                background(My.Resources.未命名, manx, many, 80, 80) : Graphics.FromImage(bgi).DrawImage(manhit(j), manx, many)
                PictureBox1.Image = bgi : delay(0.09)
            Next
        Next
        background(My.Resources.未命名, manx, many, 80, 80) : Graphics.FromImage(bgi).DrawImage(last, manx, many) : PictureBox1.Image = bgi
        done = True
    End Sub
    Sub resetman()
        If cnum = 0 Then Graphics.FromImage(bgi).DrawImage(action(1), manx, many)
        If cnum = 1 Then Graphics.FromImage(bgi).DrawImage(iaction(1), manx, many)
        If cnum = 2 Then Graphics.FromImage(bgi).DrawImage(deep(1), manx, many)
    End Sub
    Sub character(ByRef manhit As List(Of Bitmap), ByRef last As Bitmap)
        If cnum = 0 Then
            manhit.Add(action(68)) : manhit.Add(action(69)) : last = action(1)
        ElseIf cnum = 1 Then
            manhit.Add(iaction(68)) : manhit.Add(iaction(69)) : last = iaction(1)
        ElseIf cnum = 2 Then
            manhit.Add(deep(69)) : manhit.Add(deep(70)) : last = deep(1)
        End If
    End Sub
    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        If done = True Then
            If firenblood <= 0 And freezeblood <= 0 And deepblood <= 0 Then Timer1.Enabled = False : MsgBox("YOU LOSE") : End
            If bossblood <= 0 Then Timer1.Enabled = False : MsgBox("YOU WIN") : End
            Dim n As Integer = Int(Rnd() * 6) + 1, n1 As Integer = Int(Rnd() * 6) + 1
            'Dim n As Integer = 1, n1 As Integer = 5
            If playerturn = True Then
                Label3.Text = "決定我們的命運吧!" : Button4.Show() : week = False
            ElseIf frozen = True Then
                Label3.Text = "                            可惡 無法動彈" : Timer1.Enabled = False : delay(1) : playerturn = True : Timer1.Enabled = True
            Else
                Label3.Text = "                    為黑暗奉獻生命吧" : Button4.Show()
                If week = True Then
                    background(My.Resources.未命名, pcx, pcy, 80, 100)
                    Graphics.FromImage(bgi).DrawImage(pc(16), pcx, pcy) : PictureBox1.Image = bgi
                End If
            End If
            If choose = True Then
                Button4.Hide() : Timer1.Enabled = False : Label4.Show()
                Graphics.FromImage(bgi).DrawImage(dice(n), 190, 200) : Graphics.FromImage(bgi).DrawImage(dice(n1), 290, 200) : PictureBox1.Image = bgi
                If playerturn = True Then
                    If n - n1 < 0 Then
                        Label3.Text = "可惡! 喪失攻擊機會" : playerturn = False : choose = False
                        If frozen = True Then frozen = False : breakfreeze() : delay(0.46) Else delay(1)
                        Timer1.Enabled = True
                    Else
                        Button1.Show() : Button2.Show()
                        If n - n1 > 1 Then damagetimes = n - n1 : Label3.Text = "準備接下我" & damagetimes & "倍的攻擊吧" Else damagetimes = 1 : Label3.Text = "準備接下我的攻擊吧"
                    End If
                Else
                    Label3.Text = "                      準備好你的遺言" : delay(1) : playerturn = True : choose = False
                    If n1 - n < 3 Then
                        kamahameha()
                    ElseIf n1 - n < 5 Then
                        extrakamahameha()
                    Else
                        bossextra()
                    End If
                    If PictureBox6.Width <= 0 Then choose = True : changecharacters() : choose = False
                    If ignite = True Then less(False, firedamage) : fired() : ignite = False
                    Timer1.Enabled = True
                End If
            Else
                Graphics.FromImage(bgi).DrawImage(dice(n), 190, 200) : Graphics.FromImage(bgi).DrawImage(dice(n1), 290, 200) : PictureBox1.Image = bgi : Label4.Hide()
            End If
        End If
    End Sub
    Sub less(ByVal x As Boolean, ByVal n As Single)
        If x = True Then
            If week = True Then n = n / 2
            PictureBox6.Width = PictureBox6.Width - 2 * n
            If cnum = 0 Then firenblood = firenblood - n
            If cnum = 1 Then freezeblood = freezeblood - n
            If cnum = 2 Then deepblood = deepblood - n
        End If
        If x = False Then n = n * damagetimes : PictureBox7.Left = PictureBox7.Left + n : bossblood = bossblood - n
    End Sub
    Sub manblood()
        If cnum = 0 Then PictureBox6.Width = firenblood * 2
        If cnum = 1 Then PictureBox6.Width = freezeblood * 2
        If cnum = 2 Then PictureBox6.Width = deepblood * 2
    End Sub
    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        choose = True : Button4.Hide()
    End Sub
    Sub changecharacters()
        If done = True And choose = True Then
            If cnum = 2 Then cnum = 0 Else cnum = cnum + 1
            If cnum = 0 And firenblood <= 0 Then cnum = 1
            If cnum = 1 And freezeblood <= 0 Then cnum = 2
            If cnum = 2 And deepblood <= 0 Then cnum = 0
            If cnum = 0 And firenblood <= 0 Then cnum = 1
            If cnum = 0 Then Label1.Text = "Firen" Else If cnum = 1 Then Label1.Text = "Freeze" Else Label1.Text = "Deep"
            PictureBox2.Image = characters(cnum) : manblood()
            Dim man As New List(Of Bitmap), prepare As New List(Of Bitmap)
            man.Add(action(1)) : man.Add(iaction(1)) : man.Add(deep(1))
            prepare.Add(action(61)) : prepare.Add(iaction(61)) : prepare.Add(deep(61))
            Graphics.FromImage(bgi).DrawImage(My.Resources.未命名, 0, 0) : extramode()
            Graphics.FromImage(bgi).DrawImage(prepare(cnum), manx, many)
            PictureBox1.Image = bgi : delay(0.1)
            bgi = My.Resources.未命名
            Graphics.FromImage(bgi).DrawImage(man(cnum), manx, many) : extramode()
            PictureBox1.Image = bgi
        End If
    End Sub
    Sub extramode()
        If frozen = True Then Graphics.FromImage(bgi).DrawImage(pc(1), pcx, pcy) Else Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy)
        ' If ignite = True Then
    End Sub
    Sub breakfreeze()
        Dim move As New List(Of Bitmap)
        move.Add(pc(2)) : move.Add(pc(2)) : move.Add(pc2(43)) : move.Add(pc2(42)) : move.Add(pc(10))
        bgi = My.Resources.未命名 : resetman()
        For i = 0 To move.Count - 1
            background(My.Resources.未命名, pcx, pcy, 80, 100)
            Graphics.FromImage(bgi).DrawImage(move(i), pcx, pcy) : PictureBox1.Image = bgi : delay(0.09)
        Next
    End Sub
    Sub fired()
        Label3.Text = "                      阿阿阿阿阿阿!!"
        Dim move As New List(Of Bitmap)
        move.Add(pc1(10)) : move.Add(pc1(11))
        background(My.Resources.未命名, pcx, pcy, 80, 100)
        For i = 1 To 5
            For j = 0 To 1
                background(My.Resources.未命名, pcx, pcy, 110, 120)
                Graphics.FromImage(bgi).DrawImage(move(j), pcx, pcy) : PictureBox1.Image = bgi : delay(0.09)
            Next
        Next
        background(My.Resources.未命名, pcx, pcy, 110, 120) : Graphics.FromImage(bgi).DrawImage(pc(10), pcx, pcy) : PictureBox1.Image = bgi
    End Sub

    Private Sub PictureBox1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PictureBox1.Click

    End Sub
End Class
