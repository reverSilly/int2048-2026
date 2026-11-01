#include "int2048.h"

namespace reverSilly
{
	using u8=unsigned char;
	using u16=unsigned short;
	using u32=unsigned int;
	using u64=unsigned long long;
	u32 ceildiv(u32 a,u32 b)
	{
		if(a%b==0)
			return a/b;
		return a/b+1;
	}
	u32 __manual_clz(u8 x)
	{
		for(u32 i=0;i<8;++i)
			if(x&(1<<(7-i)))
				return i;
		return 8;
	}
	namespace int2048
	{
		struct Integer//unsigned
		{
			std::vector<u8>data;
			Integer(u32 n):data(n){}
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
			static Integer from(u32 i)
			{
				Integer res(sizeof(u32));
				*reinterpret_cast<u32*>(res.data.data())=i;
				return res;
			}
			template<typename T>
			T to()const
			{
				T res{};
				for(u32 i=0;i<sizeof(T)&&i<data.size();++i)
					reinterpret_cast<u8*>(&res)[i]=data[i];
				return res;
			}
			u8 get(u32 i)const
			{
				if(i<data.size())
					return data[i];
				return 0;
			}
		};
		std::strong_ordering operator<=>(Integer const &a,Integer const &b)
		{
			int cur=std::max(a.data.size(),b.data.size())-1;
			for(;cur>=0;--cur)
				if(a.get(cur)!=b.get(cur))
					return a.get(cur)<=>b.get(cur);
			return std::strong_ordering::equal;
		}
		//test result:
		//u64 a[a_0..a_7]
		//     *2^0 *2^56
		//data[0] lowest bit
		Integer &operator+=(Integer &a,Integer const &b)
		{
			a.data.resize(std::max(a.data.size(),b.data.size())+1,0);
			bool carry=false;
			for(u32 i=0;i<a.data.size();++i)
			{
				bool nxcarry=(a.data[i]+carry+b.get(i))>(1<<8);
				a.data[i]+=b.get(i)+carry;
				carry=nxcarry;
			}
			return a;
		}
		Integer &operator-=(Integer &a,Integer const &b)//asserts a>=b
		{
			bool carry=false;
			for(u32 i=0;i<a.data.size();++i)
			{
				bool nxcarry=(a.data[i]-carry-b.get(i))<0;
				a.data[i]-=b.get(i)+carry;
				carry=nxcarry;
			}
			return a;
		}
		Integer &operator<<=(Integer &a,u32 b)
		{
			int B=b>>3;
			a.shrink();
			a.data.resize(ceildiv(a.len()+b,8),0);
			for(int i=a.data.size()-1;i>=B;--i)
			{
				a.data[i]=a.data[i-B]<<(b&0b111);
				if(i-B>0)
					a.data[i]|=a.data[i-B-1]>>(8-(b&0b111));
			}
			std::fill_n(a.data.begin(),B,0);
			return a;
		}
		Integer &operator>>=(Integer &a,u32 b)
		{
			int B=b>>3;
			for(u32 i=0;i<a.data.size();++i)
			{
				a.data[i]=a.get(i+B)>>(b&0b111);
				a.data[i]|=a.get(i+B+1)<<(8-(b&0b111));
			}
			a.shrink();
			return a;
		}
		Integer operator+(Integer a,Integer const &b)
		{
			return a+=b;
		}
		Integer operator-(Integer a,Integer const &b)
		{
			return a-=b;
		}
		Integer operator<<(Integer a,u32 b)
		{
			return a<<=b;
		}
		Integer operator>>(Integer a,u32 b)
		{
			return a>>=b;
		}
	}
}

namespace sjtu {

int2048::int2048() {}

} // namespace sjtu
