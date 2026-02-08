declare void @use1(i32)
declare void @use2(i32)
declare void @use3(i32)

define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
entry:
  switch i32 %cond1, label %case_x_1
    [i32 1, label %case_x_2
     i32 2, label %case_x_3]
case_x_1:
  %x1 = add i32 %a, %b
  call void @use1(i32 %x1)
  br label %merge
case_x_2:
  %x2 = add i32 %b, %a
  call void @use2(i32 %x2)
  br label %merge
case_x_3:
  %x3 = add i32 %a, %b
  call void @use3(i32 %x3)
  br label %merge
merge:
  br i1 %cond2, label %case_y_1, label %case_y_2
case_y_1:
  %y1 = add i32 %a, %b
  ret i32 %y1
case_y_2:
  %y2 = add i32 %b, %a
  ret i32 %y2
}
