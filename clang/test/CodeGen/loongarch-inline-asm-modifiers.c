// RUN: %clang -target loongarch64-unknown-linux-gnu -S -o - -emit-llvm %s \
// RUN: | FileCheck %s

// This checks that the frontend will accept inline asm operand modifiers

int printf(const char*, ...);

typedef long long v2i64 __attribute__ ((vector_size(16), aligned(16)));
typedef long long v4i64 __attribute__ ((vector_size(32), aligned(32)));

  // CHECK: %{{[0-9]+}} = call i32 asm ".set noreorder;\0Ald.w    $0,$1;\0A.set reorder;\0A", "=r,*m"(i32* getelementptr inbounds ([8 x i32], [8 x i32]* @b, i32 {{[0-9]+}}, i64 {{[0-9]+}})) #2,
  // CHECK: %{{[0-9]+}} = call i32 asm "ld.w    $0,${1:D};\0A", "=r,*m"(i32* getelementptr inbounds ([8 x i32], [8 x i32]* @b, i32 {{[0-9]+}}, i64 {{[0-9]+}})) #2,
int b[8] = {0,1,2,3,4,5,6,7};
int  main()
{
  int i;
  v2i64 v2i64_r;
  v4i64 v4i64_r;

  // The first word. Notice, no 'D'
  {asm (
  ".set noreorder;\n"
  "ld.w    %0,%1;\n"
  ".set reorder;\n"
  : "=r" (i)
  : "m" (*(b+4)));}

  printf("%d\n",i);

  // The second word
  {asm (
  "ld.w    %0,%D1;\n"
  : "=r" (i)
  : "m" (*(b+4))
  );}

  printf("%d\n",i);

  return 1;
}
