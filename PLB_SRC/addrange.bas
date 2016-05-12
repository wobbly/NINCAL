Public Function AddRange1(range1 As Range) As Long

  For Each c In range1
    If c.Font.Strikethrough = False Then
        AddRange1 = AddRange1 + c.Value
    End If
  Next c
  
End Function
