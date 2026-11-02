#include "int2048.h"
#include <stdexcept>
#include <utility>

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
	template<typename T>
	u32 __manual_clz(T x)
	{
		for(u32 i=0;i<8*sizeof(T);++i)
			if(x&(1ull<<(sizeof(T)*8-1-i)))
				return i;
		return 8*sizeof(T);
	}
	template<typename T>
	T lowbit(T x)
	{
		return x&-x;
	}
	u32 ceil2(u32 x)
	{
		if(x==lowbit(x))
			return x;
		return 1u<<(32-__manual_clz(x));
	}
	template<u32 p,u32 g>
	struct Mod
	{
		static constexpr u32 qpow(u64 a,u64 b)
		{
			u64 res=1;
			for(;b;a=a*a%p,b>>=1)
				if(b&1)
					res=res*a%p;
			return res;
		}
		static constexpr u32 inv(u32 x)
		{
			return qpow(x,p-2);
		}
		static constexpr u32 root(u32 n)
		{
			return qpow(g,(p-1)/n);
		}
		template<u32 p2>
		static constexpr u64 merge(u64 x,u64 x2)
		{
			constexpr u32 ip2=inv(p2);
			u64 k=u64(x%p+p-x2%p)%p*ip2%p;
			return k*p2+x2;
		}
		template<bool INV>
		static std::vector<u32> construct()
		{
			std::vector<u32>a(32);
			for(u32 i=0;i<32;++i)
				a[__manual_clz(1<<i)]=root(1<<i);
			if constexpr(INV)
				for(u32 i=0;i<32;++i)
					a[i]=inv(a[i]);
			return a;
		}
		static std::vector<u32>w,iw;
		template<bool iNTT>
		static std::vector<u32> NTT(std::vector<u32>const &a)//assert a.size() is 2pow
		{
			u32 const n=a.size();
			if(n==0)
				return {};
			if(n==1)
				return a;
			std::vector<u32>a0(n/2),a1(n/2),res(n);
			for(u32 i=0;i<n;i+=2)
			{
				a0[i/2]=a[i];
				a1[i/2]=a[i+1];
			}
			u64 w=Mod::w[__manual_clz(n)],ww=1;
			if constexpr(iNTT)
				w=inv(w);
			a0=NTT<iNTT>(a0);
			a1=NTT<iNTT>(a1);
			for(u32 i=0;i<n/2;++i,ww=ww*w%p)
			{
				res[i]=(a0[i]+ww*a1[i])%p;
				res[i+n/2]=((a0[i]+u64(p)-ww*a1[i]%p)%p+p)%p;
			}
			return res;
		}
		static std::vector<u32>mul(std::vector<u32> a,std::vector<u32> b)
		{
			u32 const n=ceil2(a.size()+b.size());
			if(n>=1<<25)
				throw std::out_of_range("integer too big");
			a.resize(n,0);
			b.resize(n,0);
			a=NTT<false>(a);
			b=NTT<false>(b);
			for(u32 i=0;i<n;++i)
				a[i]=u64(a[i])*b[i]%p;
			a=NTT<true>(a);
			for(u32 i=0,in=inv(n);i<n;++i)
				a[i]=u64(a[i])*in%p;
			return a;
		}
	};
	template<u32 p,u32 g>
	std::vector<u32>Mod<p,g>::w=Mod<p,g>::construct<false>();
	template<u32 p,u32 g>
	std::vector<u32>Mod<p,g>::iw=Mod<p,g>::construct<true>();

	std::vector<u64>mul(std::vector<u32>const &a,std::vector<u32>const &b)
	{
		constexpr u32 p1{167772161},p2{469762049};
		auto r1=Mod<p1,3>::mul(a,b);
		auto r2=Mod<p2,3>::mul(a,b);
		std::vector<u64>res(r1.size());
		for(u32 i=0;i<r1.size();++i)
			res[i]=Mod<p1,3>::merge<p2>(r1[i],r2[i]);
		return res;
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
			template<typename T>
			T get(u32 x)const
			{
				T res;
				for(u32 i=0;i<sizeof(T);++i)
					reinterpret_cast<u8*>(&res)[i]=get(x*sizeof(T)+i);
				return res;
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
				bool nxcarry=(a.data[i]+carry+b.get(i))>=(1<<8);
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
			a.shrink();
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
		Integer operator*(Integer const&a,Integer const &b)
		{
			std::vector<u32>aa(ceildiv(a.len(),16)),bb(ceildiv(b.len(),16));
			for(u32 i=0;i<aa.size();++i)
				aa[i]=a.get<u16>(i);
			for(u32 i=0;i<bb.size();++i)
				bb[i]=b.get<u16>(i);
			auto res=mul(aa,bb);
			u64 carry=0;
			for(u32 i=0;i<res.size();++i)
			{
				res[i]+=carry;
				carry=res[i]>>16;
				res[i]%=(1<<16);
			}
			if(carry)
				res.push_back(carry);
			Integer res1(res.size()*2);
			for(u32 i=0;i<res.size();++i)
			{
				res1.data[i*2]=res[i]%(1<<8);
				res1.data[i*2+1]=res[i]>>8;
			}
			res1.shrink();
			return res1;
		}
		Integer operator<<(Integer a,u32 b);
		std::pair<Integer,Integer> div(Integer a,Integer const &b)
		{
			if(b.len()==0)
				throw std::invalid_argument("divided by zero.");
			Integer res(a.data.size());
			int bit=a.len()-b.len();
			if(bit<0)
				return {std::move(res),std::move(a)};
			Integer Bsh=b<<bit;
			for(;bit>=0;)
			{
				if(a>=Bsh)
				{
					a-=Bsh;
					res.set_bit(bit,true);
				}
				Bsh>>=1;
				--bit;
			}
			return {std::move(res),std::move(a)};
		}
		Integer operator/(Integer const &a,Integer const &b)
		{
			return div(a,b).first;
		}
		Integer operator%(Integer const &a,Integer const &b)
		{
			return div(a,b).second;
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
