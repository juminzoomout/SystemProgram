#include "hpfp.h"
#include <limits.h>
#include <stdlib.h>

hpfp int_converter(int input){
  int s, exp, frac = 0;

  if(input > 32736) return 0x7C00;
  else if(input < -32736) return 0xFC00;
  else if(input == 0) return 0x0000; // 조건
  else if(input > 0) s = 0;
  else s = 1 << 15; // s 결정

  int mask = 1;
  for(int i = 0; i < 16; i++) {
    if(input & mask) exp = i;
    mask = mask << 1;
  }

  char *reversed = (char*)malloc(sizeof(char) * exp);
  int remainder;
  for(int j = 0; j <= exp; j++) {
    remainder = input % 2;
    input /= 2;
    reversed[j] = remainder;
  }

  int bi = 1;
  for(int k = 0; k < exp; k++) { // < exp로 해서 맨 마지막 인덱스 즉 맨 앞의 1을 없앰
    frac += reversed[k] * bi;
    bi *= 2;
  }

  frac = frac << 10 - exp; //frac 결정

  exp += 15;
  exp = exp << 10; // exp 결정

  free(reversed);

  return s + exp + frac;
}

int hpfp_to_int_converter(hpfp input){
  hpfp s = input, exp = input, frac = input;

  if(input == 0x7C00) return INT_MAX;
  else if(input == 0xFC00) return INT_MIN;
  
  exp = exp << 1;
  exp = exp >> 11;
  if(exp == 0x1F) return INT_MIN;
  exp -= 15; // E 결정

  frac = frac << 6;
  frac = frac >> 16 - exp; // round to zero
  frac = frac + (1 << exp); // 원래 representation으로 복귀

  if(s >> 15) return -frac;
  else return frac;
}

hpfp float_converter(float input){
  if(input > 32736) return 0x7C00;
  else if(input < -32736) return 0xFC00;
  else if(input == 0) return 0x0000; // 조건

  union FloatInt {
    float f;
    unsigned int i;
  };

  union FloatInt inPut;
  inPut.f = input;

  unsigned int s, exp, frac; // for logical right shift

  // float랑 double은 비트연산자를 못쓴단다. 하. 그럼 같은 바이트인 int에다가 비트배열을 옮겨야하는데 union을 썼다.
  if(inPut.i >> 31 == 1) s = 1 << 15;
  else s = 0;

  exp = inPut.i;
  exp = exp << 1;
  exp = exp >> 24;

  int e = exp;
  e -= 127;

  frac = inPut.i;

  if(e > -14) {
    frac = frac << 9;
    frac = frac >> 22;
    e += 15;
  }
  else if(e == -14) {
    frac = frac << 9;
    frac = frac >> 22;
    if(inPut.f > 0.00005) e += 15;
    else e = 0;
  }
  else return 0; // hpfp로 표현하기엔 0에 너무 가까운 수

  e = e << 10;

  return s + e + frac;
}

float hpfp_to_float_converter(hpfp input){
  union IntFloat {
    float f;
    unsigned int i;
  };

  union IntFloat fl;

  unsigned int s = input, exp = input, frac = input;

  s = s >> 15;
  s = s << 31;

  exp = exp & 0x7c00;
  exp = exp >> 10;
  exp -= 15;
  exp += 127;
  exp = exp << 23;

  frac = frac << 22;
  frac = frac >> 9;

  fl.i = s + exp + frac;
  return fl.f;
}

hpfp makeMantissa(hpfp x) {
  if(x & 0x7C00 == 0) return x & 0x03FF;
  else {
    x = x & 0x03FF;
    return x | 0x0400;
  }
}

hpfp addition_function(hpfp a, hpfp b){
  if(a >> 10 == 0x001F && makeMantissa(a) || a >> 10 == 0x003F && makeMantissa(a) || b >> 10 == 0x001F && makeMantissa(b) || b >> 10 == 0x003F && makeMantissa(b)) return 0x7C01; // NaN
  if(a == 0x7C00) {
    if(b == 0xFC00) return 0x7C01;
    else return 0x7C00;
  }
  else if(a == 0xFC00) {
    if(b == 0x7C00) return 0x7C01;
    else return 0xFC00;
  } // a에 대한 예외

  if(b == 0x7C00) {
    if(a == 0xFC00) return 0x7C01;
    else return 0x7C00;
  }
  else if(b == 0xFC00) {
    if(a == 0x7C00) return 0x7C01;
    else return 0xFC00;
  } // b에 대한 예외

  hpfp result;
  unsigned short big, small, flag;
  if((a & 0x7FFF) > (b & 0x7FFF)){
    big = a;
    small = b;
    flag = 1;
  }
  else {
    big = b;
    small = a;
    flag = 2;
  }
  big = big << 1;
  big = big >> 11;
  small = small << 1;
  small = small >> 11;
  short diff = big - small;

  hpfp aFrac, bFrac;
  aFrac = makeMantissa(a);
  bFrac = makeMantissa(b);
  if(flag == 1) bFrac = bFrac >> diff;
  else aFrac = aFrac >> diff;

  if(a >> 15 == b >> 15) result = aFrac + bFrac;
  else {
    if(flag == 1) result = aFrac - bFrac;
    else result = bFrac - aFrac;
  }

  hpfp exp = big;
  if(result & 0x0800) {
    if(result & 0x0001 && result & 0x0002) result = result + 1; // round to even
    result = result >> 1;
    result = result & 0x03FF;
    exp += 1;
  }
  else if(result & 0x0400) {
    result = result & 0x03FF;
  }
  else {
    int d = 1;
    while(d<11) {
      if(result & 0x0400 >> d) break;
      d++;
    }
    result = result << d;
    result = result & 0x03FF;
    exp -= d;
  }

  hpfp s;
  if(flag == 1) s = a & 0x8000;
  else s = b & 0x8000;

  if(exp > 0x001F) {
    if(s == 0) return 0x7C00;
    else return 0xFC00;
  } // 더했을 떄 overflow

  exp = exp << 10;
  result += exp + s;

  return result;
}

hpfp multiply_function(hpfp a, hpfp b){
  if(a >> 10 == 0x001F && makeMantissa(a) || a >> 10 == 0x003F && makeMantissa(a) || b >> 10 == 0x001F && makeMantissa(b) || b >> 10 == 0x003F && makeMantissa(b)) return 0x7C01; // NaN
  if(a == 0x7C00) {
    if(b == 0xFC00) return 0xFC00;
    else if(b == 0 || b == 0x8000) return 0x7C01;
    else {
      if(b >> 15 == 1) return 0xFC00;
      else return 0x7C00;
    }
  }
  else if(a == 0xFC00) {
    if(b == 0x7C00) return 0xFC00;
    else if(b == 0 || b == 0x8000) return 0x7C01;
    else {
      if(b >> 15 == 1) return 0x7C00;
      else return 0xFC00;
    }
  } // a에 대한 예외
  if(b == 0x7C00) {
    if(a == 0xFC00) return 0xFC00;
    else if(a == 0 || a == 0x8000) return 0x7C01;
    else {
      if(a >> 15 == 1) return 0xFC00;
      else return 0x7C00;
    }
  }
  else if(b == 0xFC00) {
    if(a == 0x7C00) return 0xFC00;
    else if(a == 0 || a == 0x8000) return 0x7C01;
    else {
      if(a >> 15 == 1) return 0x7C00;
      else return 0xFC00;
    }
  } // b에 대한 예외

  hpfp s, aExp, bExp;
  int aFrac, bFrac;

  aExp = a & 0x7C00;
  aExp = aExp >> 10;
  bExp = b & 0x7C00;
  bExp = bExp >> 10;
  hpfp rExp = aExp + bExp - 15;

  aFrac = makeMantissa(a);
  bFrac = makeMantissa(b);

  int rFrac = aFrac * bFrac;
  if(rFrac > 0x0800) {
    if(rFrac & 0x200000){
      if(rFrac & 0x07FF > 0x0400) rFrac += 0x0800;
      else if(rFrac & 0x07FF == 0x0400){
        if(rFrac & 0x0800 == 0x0800) rFrac += 0x0800;
      }
      rExp += 1;
      rFrac = rFrac >> 11;
    } // 곱했더니 1x.xxx일 때
    else {
      if(rFrac & 0x03FF > 0x0200) rFrac += 0x0400;
      else if(rFrac & 0x03FF == 0x0200){
        if(rFrac & 0x0400 == 0x0400) rFrac += 0x0400;
      }
      rFrac = rFrac >> 10;
    } // 곱했더니 1.xxx일 때
    rFrac = rFrac & 0x03FF;
  }
  else rFrac = rFrac & 0x03FF;

  if(a >> 15 == 0) {
    if(b >> 15 == 0) s = 0;
    else s = 0x8000;
  }
  else {
    if(b >> 15 == 0) s = 0x8000;
    else s = 0;
  }

  if(rExp > 0x001F) {
    if(s == 0) return 0x7C00;
    else return 0xFC00;
  } // 곱했을 떄 overflow

  rExp = rExp << 10;

  return s + rExp + rFrac;

}

char* comparison_function(hpfp a, hpfp b){
  if((a & 0x7C00) >> 10 == 31 && a & 0x03FF != 0) return "=";
  else if((b & 0x7C00) >> 10 == 31 && b & 0x03FF != 0) return "=";

  hpfp aS, bS, aExp, bExp, aFrac, bFrac;
  aS = a & 8000;
  aS = a >> 15;
  bS = b & 8000;
  bS = b >> 15;
  if(aS > bS) return "<";
  else if(aS < bS) return ">";

  aExp = a & 0x7C00;
  aExp = aExp >> 10;
  bExp = b & 0x7C00;
  bExp = bExp >> 10;
  if(aS == 0) {
    if(aExp > bExp) return ">";
    else if(aExp < bExp) return "<";
  }
  else {
    if(aExp > bExp) return "<";
    else if(aExp < bExp) return ">";
  }

  aFrac = makeMantissa(a);
  bFrac = makeMantissa(b);
  if(aS == 0) {
    if(aFrac > bFrac) return ">";
    else if(aFrac < bFrac) return "<";
  }
  else {
    if(aFrac > bFrac) return "<";
    else if(aFrac < bFrac) return ">";
  }
  return "=";
}

char* hpfp_to_bits_converter(hpfp result){
  char *arr = (char*)malloc(sizeof(char)*17);
  short mask = 1;
  for(int i = 15; i >= 0; i--) {
    if(result & mask << i) arr[15-i] = '1';
    else arr[15-i] = '0';
  }
  arr[16] = '\0';
  return arr;
}

char* hpfp_flipper(char* input){
  char *arr = (char*)malloc(sizeof(char)*17);
  for(int i = 0; i < 17; i++) arr[i] = '0';
  return arr;
}
