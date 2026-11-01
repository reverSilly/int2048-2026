#include "int2048.h"

namespace reverSilly
{
	using u8=unsigned char;
	using u16=unsigned short;
	using u32=unsigned int;
	using u64=unsigned long long;
	namespace int2048
	{
		struct Integer//unsigned
		{
			std::vector<u8>data;
			Integer(int n):data(n){}
			static Integer from(int i)
			{
				Integer res(sizeof int);
				*reinterpret_cast<int*>(res.data.data())=i;
				return res;
			}
			u8 get(int i)const
			{
				if(i<data.size())
					return data[i];
				return 0;
			}
		};
		//test result:
		//u64 a[a_0..a_7]
		//     *2^0 *2^56
		//data[0] lowest bit
		Integer &operator+=(Integer &a,Integer const &b)
		{
			a.data.resize(std::max(a.data.size(),b.data.size())+1,0);
			bool carry=false;
			for(int i=0;i<a.data.size();++i)
			{
				bool nxcarry=(a.data[i]+carry+b.get(i))>(1<<8);
				a.data[i]+=b.get(i)+carry;
				carry=nxcarry;
			}
			return a;
		}
		Integer operator+(Integer a,Integer const &b)
		{
			return a+=b;
		}
		Integer &operator-=(Integer &a,Integer const &b)//asserts a>=b
		{
			bool carry=false;
			for(int i=0;i<a.data.size();++i)
			{
				bool nxcarry=(a.data[i]-carry-b.get(i))<0;
				a.data[i]-=b.get(i)+carry;
				carry=nxcarry;
			}
			return a;
		}
		Integer operator-(Integer a,Integer const &b)
		{
			return a-=b;
		}
	}
}

namespace sjtu {

int2048::int2048() {}

} // namespace sjtu
