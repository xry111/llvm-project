; Check whether LoongArchSERegisterInfo::eliminateFI works well
; RUN: llc -march=loongarch64 -o - %s | FileCheck %s

define signext i32 @ldptr_w_unaligned() {
; CHECK-LABEL: ldptr_w_unaligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5001
  %0 = bitcast i8* %arrayidx to i32*
; the offset MUST be 0
; CHECK: ldptr.w $r{{[0-9]+}}, $r{{[0-9]+}}, 0
  %1 = load i32, i32* %0, align 1
  ret i32 %1
}

define signext i32 @ldptr_w_aligned() {
; CHECK-LABEL: ldptr_w_aligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5000
  %0 = bitcast i8* %arrayidx to i32*
; the offset may not be 0, but MUST be 4-bytes aligned
; CHECK: ldptr.w $r{{[0-9]+}}, $r{{[0-9]+}}, {{[0-9]+}}
  %1 = load i32, i32* %0, align 1
  ret i32 %1
}

define signext i64 @ldptr_d_unaligned() {
; CHECK-LABEL: ldptr_d_unaligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5001
  %0 = bitcast i8* %arrayidx to i64*
; the offset MUST be 0
; CHECK: ldptr.d $r{{[0-9]+}}, $r{{[0-9]+}}, 0
  %1 = load i64, i64* %0, align 1
  ret i64 %1
}

define signext i64 @ldptr_d_aligned() {
; CHECK-LABEL: ldptr_d_aligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5000
  %0 = bitcast i8* %arrayidx to i64*
; the offset may not be 0, but MUST be 4-bytes aligned
; CHECK: ldptr.d $r{{[0-9]+}}, $r{{[0-9]+}}, {{[0-9]+}}
  %1 = load i64, i64* %0, align 1
  ret i64 %1
}

define void @stptr_w_unaligned(i32 signext %val) {
; CHECK-LABEL: stptr_w_unaligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5001
  %0 = bitcast i8* %arrayidx to i32*
; the offset MUST be 0
; CHECK: stptr.w $r{{[0-9]+}}, $r{{[0-9]+}}, 0
  store i32 %val, i32* %0, align 1
  ret void
}

define void @stptr_w_aligned(i32 signext %val) {
; CHECK-LABEL: stptr_w_aligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5000
  %0 = bitcast i8* %arrayidx to i32*
; the offset may not be 0, but MUST be 4-bytes aligned
; CHECK: stptr.w $r{{[0-9]+}}, $r{{[0-9]+}}, {{[0-9]+}}
  store i32 %val, i32* %0, align 1
  ret void
}

define void @stptr_d_unaligned(i64 %val) {
; CHECK-LABEL: stptr_d_unaligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5001
  %0 = bitcast i8* %arrayidx to i64*
; the offset MUST be 0
; CHECK: stptr.d $r{{[0-9]+}}, $r{{[0-9]+}}, 0
  store i64 %val, i64* %0, align 1
  ret void
}

define void @stptr_d_aligned(i64 %val) {
; CHECK-LABEL: stptr_d_aligned:
; CHECK: # %bb.0: # %entry
entry:
  %array = alloca [6000 x i8], align 1
  %arrayidx = getelementptr inbounds [6000 x i8], [6000 x i8]* %array, i64 0, i64 5000
  %0 = bitcast i8* %arrayidx to i64*
; the offset may not be 0, but MUST be 4-bytes aligned
; CHECK: stptr.d $r{{[0-9]+}}, $r{{[0-9]+}}, {{[0-9]+}}
  store i64 %val, i64* %0, align 1
  ret void
}
