  $ ls
  NewGVNtricky.ll
$ cat 1.ll
Here second simplifycfg doesn't do what expected
$ opt-18 --passes='simplifycfg,early-cse,tailcallelim,reassociate,gvn,simplifycfg' -print-after-all NewGVNtricky.ll -o /dev/null
  $ opt-18  -O2 -print-after-all NewGVNtricky.ll -o /dev/null
  ; *** IR Dump After Annotation2MetadataPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    br i1 %cond2, label %case_y_1, label %case_y_2
  
  case_y_1:                                         ; preds = %merge
    %y1 = add i32 %a, %b
    ret i32 %y1
  
  case_y_2:                                         ; preds = %merge
    %y2 = add i32 %b, %a
    ret i32 %y2
  }
  ; *** IR Dump After ForceFunctionAttrsPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    br i1 %cond2, label %case_y_1, label %case_y_2
  
  case_y_1:                                         ; preds = %merge
    %y1 = add i32 %a, %b
    ret i32 %y1
  
  case_y_2:                                         ; preds = %merge
    %y2 = add i32 %b, %a
    ret i32 %y2
  }
  ; *** IR Dump After InferFunctionAttrsPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    br i1 %cond2, label %case_y_1, label %case_y_2
  
  case_y_1:                                         ; preds = %merge
    %y1 = add i32 %a, %b
    ret i32 %y1
  
  case_y_2:                                         ; preds = %merge
    %y2 = add i32 %b, %a
    ret i32 %y2
  }
  ; *** IR Dump After CoroEarlyPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    br i1 %cond2, label %case_y_1, label %case_y_2
  
  case_y_1:                                         ; preds = %merge
    %y1 = add i32 %a, %b
    ret i32 %y1
  
  case_y_2:                                         ; preds = %merge
    %y2 = add i32 %b, %a
    ret i32 %y2
  }
  ; *** IR Dump After LowerExpectIntrinsicPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    br i1 %cond2, label %case_y_1, label %case_y_2
  
  case_y_1:                                         ; preds = %merge
    %y1 = add i32 %a, %b
    ret i32 %y1
  
  case_y_2:                                         ; preds = %merge
    %y2 = add i32 %b, %a
    ret i32 %y2
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    %y2 = add i32 %b, %a
    %common.ret.op = select i1 %cond2, i32 %y1, i32 %y2
    ret i32 %common.ret.op
  }
  ; *** IR Dump After SROAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    %y2 = add i32 %b, %a
    %common.ret.op = select i1 %cond2, i32 %y1, i32 %y2
    ret i32 %common.ret.op
  }
  ; *** IR Dump After EarlyCSEPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After OpenMPOptPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After IPSCCPPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After CalledValuePropagationPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32)
  
  declare void @use2(i32)
  
  declare void @use3(i32)
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After GlobalOptPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After PromotePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After AlwaysInlinerPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After RequireAnalysisPass<llvm::GlobalsAA, llvm::Module, llvm::AnalysisManager<Module>> on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After InvalidateAnalysisPass<llvm::AAManager> on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After RequireAnalysisPass<llvm::ProfileSummaryAnalysis, llvm::Module, llvm::AnalysisManager<Module>> on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After InlinerPass on (example) ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After PostOrderFunctionAttrsPass on (example) ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After OpenMPOptCGSCCPass on (example) ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After SROAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After EarlyCSEPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After SpeculativeExecutionPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After JumpThreadingPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After CorrelatedValuePropagationPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After AggressiveInstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After LibCallsShrinkWrapPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After TailCallElimPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %a, %b
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %a, %b
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %a, %b
    ret i32 %y1
  }
  ; *** IR Dump After ReassociatePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After ConstraintEliminationPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After LoopSimplifyPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After LCSSAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After LoopSimplifyPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After LCSSAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After SROAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After VectorCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After MergedLoadStoreMotionPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1 = add i32 %b, %a
    ret i32 %y1
  }
  ; *** IR Dump After GVNPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After SCCPPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After BDCEPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After JumpThreadingPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After CorrelatedValuePropagationPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After ADCEPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After MemCpyOptPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After DSEPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After MoveAutoInitPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After LoopSimplifyPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After LCSSAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After CoroElidePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    %x1 = add i32 %b, %a
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    %x2 = add i32 %b, %a
    tail call void @use2(i32 %x2)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    %x3 = add i32 %b, %a
    tail call void @use3(i32 %x3)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x3, %case_x_3 ], [ %x2, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    %y1.pre-phi = phi i32 [ %x1, %case_x_3 ], [ %x1, %case_x_2 ], [ %x1, %case_x_1 ]
    ret i32 %y1.pre-phi
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After PostOrderFunctionAttrsPass on (example) ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After RequireAnalysisPass<llvm::ShouldNotRunFunctionPassesAnalysis, llvm::Function, llvm::AnalysisManager<Function>> on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After CoroSplitPass on (example) ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InvalidateAnalysisPass<llvm::ShouldNotRunFunctionPassesAnalysis> on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After DeadArgumentEliminationPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After CoroCleanupPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After GlobalOptPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After GlobalDCEPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After EliminateAvailableExternallyPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After ReversePostOrderFunctionAttrsPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After RecomputeGlobalsAAPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After Float2IntPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LowerConstantIntrinsicsPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LoopSimplifyPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LCSSAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LoopDistributePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InjectTLIMappings on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LoopVectorizePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InferAlignmentPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LoopLoadEliminationPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After SLPVectorizerPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After VectorCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LoopUnrollPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After WarnMissedTransformationsPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After SROAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InferAlignmentPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InstCombinePass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LoopSimplifyPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LCSSAPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After AlignmentFromAssumptionsPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After LoopSinkPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After InstSimplifyPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After DivRemPairsPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After TailCallElimPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After SimplifyCFGPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After GlobalDCEPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After ConstantMergePass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After CGProfilePass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After RelLookupTableConverterPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After AnnotationRemarksPass on example ***
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
  ; *** IR Dump After BitcodeWriterPass on [module] ***
  ; ModuleID = 'NewGVNtricky.ll'
  source_filename = "NewGVNtricky.ll"
  
  declare void @use1(i32) local_unnamed_addr
  
  declare void @use2(i32) local_unnamed_addr
  
  declare void @use3(i32) local_unnamed_addr
  
  define i32 @example(i32 %cond1, i1 %cond2, i32 %a, i32 %b) local_unnamed_addr {
  entry:
    %x1 = add i32 %b, %a
    switch i32 %cond1, label %case_x_1 [
      i32 1, label %case_x_2
      i32 2, label %case_x_3
    ]
  
  case_x_1:                                         ; preds = %entry
    tail call void @use1(i32 %x1)
    br label %merge
  
  case_x_2:                                         ; preds = %entry
    tail call void @use2(i32 %x1)
    br label %merge
  
  case_x_3:                                         ; preds = %entry
    tail call void @use3(i32 %x1)
    br label %merge
  
  merge:                                            ; preds = %case_x_3, %case_x_2, %case_x_1
    ret i32 %x1
  }
