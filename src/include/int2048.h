#pragma once
#ifndef SJTU_BIGINTEGER
#define SJTU_BIGINTEGER

// Integer 1:
// 实现一个有符号的大整数类，只需支持简单的加减

// Integer 2:
// 实现一个有符号的大整数类，支持加减乘除，并重载相关运算符

// 请不要使用除了以下头文件之外的其它头文件
#include <complex>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <vector>

// 请不要使用 using namespace std;

namespace reverSilly 
{
  using u8=unsigned char;
  using u16=unsigned short;
  using u32=unsigned int;
  using u64=unsigned long long;

  // https://chat.deepseek.com/share/tv9ai8xucir93d2qw6
  template<typename T>
  u32 bitlen(T x)//not suitable for u64
  {
    if(x==0)
      return 0;
    double d=x;
    u64 bits;
    std::memcpy(&bits,&d,sizeof(bits));
    return ((bits>>52)&0b111'111'11111)-1022;
  }
  template<typename T>
  u32 __manual_clz(T x)
  {
    return sizeof(T)*8-bitlen(x);
  }
  template<typename T>
  T lowbit(T x)
  {
    return x&-x;
  }
  inline u32 ceil2(u32 x)
  {
    if(x==lowbit(x))
      return x;
    return 1u<<(32-__manual_clz(x));
  }
  namespace int2048
  {
    struct Integer;
    std::vector<Integer> const&pow10();
    Integer operator+(Integer a,Integer const &b);
    Integer operator*(Integer const&a,Integer const &b);
    std::strong_ordering operator<=>(Integer const &a,Integer const &b);
    inline bool operator==(Integer const &a,Integer const &b)
    {
      return (a<=>b)==0;
    }

    struct Integer//unsigned
    {
      std::vector<u8>data;
      explicit Integer(u32 n):data(n){}
      void shrink()
      {
        while(!data.empty()&&data.back()==0)
          data.pop_back();
      }
      u32 len()const
      {
        int i=data.size()-1;
        for(;i>=0&&data[i]==0;--i);
        if(i<0)
          return 0;
        return i*8+8-__manual_clz(data[i]);
      }
      template<typename T>
      static Integer from(T i)
      {
        Integer res(sizeof(T));
        *reinterpret_cast<T*>(res.data.data())=i;
        return res;
      }
      static Integer from(std::string_view sv);
      template<typename T>
      T to()const
      {
        return get<T>(0);
      }
      std::string str()const;
      u8 get(u32 i)const
      {
        if(i<data.size())
          return data[i];
        return 0;
      }
      template<typename T>
      T &get(u32 x) //no out_of_bound check
      {
        return *reinterpret_cast<T*>(data.data()+x*sizeof(T));
      }
      template<typename T>
      T get(u32 x)const //with out_of_bound check
      {
        auto base=x*sizeof(T);
        T val=0;
        if(base>=data.size())
          return val;
        std::memcpy(&val,data.data()+base,
            std::min(sizeof(val),data.size()-base));
        return val;
      }
      bool get_bit(u32 i)const
      {
        return (get(i>>3)>>(i&0b111))&1;
      }
      void set_bit(u32 i,bool v)
      {
        if(v)
          data[i>>3]|=1<<(i&0b111);
        else
          data[i>>3]&=~(1<<(i&0b111));
      }
    };
  }
}

namespace sjtu {
class int2048 {
  // todo
  int sign;
  reverSilly::int2048::Integer value;
public:
  // 构造函数
  int2048();
  int2048(long long);
  int2048(const std::string &);
  int2048(const int2048 &);
  int2048(int2048&&)=default;

  // 以下给定函数的形式参数类型仅供参考，可自行选择使用常量引用或者不使用引用
  // 如果需要，可以自行增加其他所需的函数
  // ===================================
  // Integer1
  // ===================================

  // 读入一个大整数
  void read(const std::string &);
  // 输出储存的大整数，无需换行
  void print()const;
  std::string str()const;

  // 加上一个大整数
  int2048 &add(const int2048 &);
  // 返回两个大整数之和
  friend int2048 add(int2048, const int2048 &);

  // 减去一个大整数
  int2048 &minus(const int2048 &);
  // 返回两个大整数之差
  friend int2048 minus(int2048, const int2048 &);

  // ===================================
  // Integer2
  // ===================================

  int2048 operator+() const;
  int2048 operator-() const;

  int2048 &operator=(const int2048 &);
  int2048 &operator=(int2048 &&)=default;

  int2048 &operator+=(const int2048 &);
  friend int2048 operator+(int2048 const&, const int2048 &);

  int2048 &operator-=(const int2048 &);
  friend int2048 operator-(int2048 const&, const int2048 &);

  int2048 &operator*=(const int2048 &);
  friend int2048 operator*(int2048, const int2048 &);

  int2048 &operator/=(const int2048 &);
  friend int2048 operator/(const int2048&, const int2048 &);

  int2048 &operator%=(const int2048 &);
  friend int2048 operator%(const int2048&, const int2048 &);

  friend std::istream &operator>>(std::istream &, int2048 &);
  friend std::ostream &operator<<(std::ostream &, const int2048 &);

  friend bool operator==(const int2048 &a, const int2048 &b)
  {
    return (a<=>b)==std::strong_ordering::equal;
  }
  friend bool operator!=(const int2048 &, const int2048 &)=default;
  friend bool operator<(const int2048 &, const int2048 &)=default;
  friend bool operator>(const int2048 &, const int2048 &)=default;
  friend bool operator<=(const int2048 &, const int2048 &)=default;
  friend bool operator>=(const int2048 &, const int2048 &)=default;

  friend std::strong_ordering operator<=>(int2048 const &a,int2048 const &b)
  {
    if(a.value.len()==0&&b.value.len()==0)
      return std::strong_ordering::equal;
    if(a.sign*b.sign<0)
      return a.sign<=>b.sign;
    if(a.sign>0)
      return a.value<=>b.value;
    else
      return b.value<=>a.value;
  }

  friend std::pair<int2048,int2048>div(int2048 const &a,int2048 const &b);
};
} // namespace sjtu

#endif
