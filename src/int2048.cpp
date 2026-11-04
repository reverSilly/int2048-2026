#include "int2048.h"

namespace reverSilly
{
	u32 ceildiv(u32 a,u32 b)
	{
		if(a%b==0)
			return a/b;
		return a/b+1;
	}
	void reverse(std::vector<u32>&v)
	// https://oiwiki.org/math/poly/fft/
	{
		//(v.size()==lowbit(v.size()));
		for(u32 i=0,r=0;i<v.size();++i)
		{
			if(i<r)
				std::swap(v[i],v[r]);
			for(u32 j=v.size()/2;j;j>>=1)
			{
				if(r&j)
					r&=~j;
				else
				{
					r|=j;
					break;
				}
			}
		}
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
		template<bool iNTT>
		static void NTT(u32 *a,u32 n)//assert a.size() is 2pow
		{
			static std::vector<u32>w=Mod<p,g>::construct<false>(),iw=Mod<p,g>::construct<true>();
			if(n==0||n==1)
				return;
			NTT<iNTT>(a,n/2);
			NTT<iNTT>(a+n/2,n/2);
			u64 W=w[__manual_clz(n)],ww=1;
			if constexpr(iNTT)
				W=iw[__manual_clz(n)];
			for(u32 i=0;i<n/2;++i,ww=ww*W%p)
			{
				u64 const G=a[i],H=a[i+n/2];
				a[i]=(G+ww*H)%p;
				a[i+n/2]=(G+p-ww*H%p)%p;
			}
		}
		static std::vector<u32>mul(std::vector<u32> a,std::vector<u32> b)
		{
			u32 const n=ceil2(a.size()+b.size());
			if(n>=1<<25)
				throw std::out_of_range("integer too big");
			a.resize(n,0);
			b.resize(n,0);
			reverse(a);
			reverse(b);
			NTT<false>(a.data(),n);
			NTT<false>(b.data(),n);
			for(u32 i=0;i<n;++i)
				a[i]=u64(a[i])*b[i]%p;
			reverse(a);
			NTT<true>(a.data(),n);
			for(u32 i=0,in=inv(n);i<n;++i)
				a[i]=u64(a[i])*in%p;
			return a;
		}
	};

	std::vector<u64>mul(std::vector<u32>const &a,std::vector<u32>const &b)
	{
		if(a.size()*b.size()<(1<<8))
		{
			std::vector<u64>res(a.size()+b.size(),0);
			for(u32 i=0;i<a.size();++i)
			{
				for(u32 j=0;j<b.size();++j)
					res[i+j]+=a[i]*b[j];
			}
			return res;
		}
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
		std::pair<Integer,Integer> div(Integer const &a,Integer const &b);
		template<typename T>
		std::pair<Integer,T>div(Integer const &a,T b);
		Integer operator-(Integer a,Integer const &b);
		Integer operator>>(Integer a,u32 b);

		std::vector<Integer> const &pow10()
		{
			constexpr u32 prebuilt=19;
			static std::vector<Integer> cache=[]()
			{
			  	std::vector<Integer>res(prebuilt,Integer::from(10));
			  	for(u32 i=1;i<prebuilt;++i)
					res[i]=res[i-1]*res[i-1];
				return res;
			}();
			return cache;
		}//sv[0] is low
		Integer Integer::from(std::string_view sv)
		{
			u32 const n=sv.size();
			if(n<=18)
			{
				u64 res=0;
				for(u32 i=sv.size()-1;i<sv.size();--i)
				{
					res*=10;
					res+=sv[i]-'0';
				}
				return Integer::from(res);
			}
			if(31-__manual_clz(n)>=pow10().size())
				throw std::out_of_range("from string too long");
			u32 cur=ceil2(n)/2;
			Integer left=from(sv.substr(0,cur)),base=from(sv.substr(cur));
			return left+base*pow10()[31-__manual_clz(cur)];
		}
		std::string Integer::str()const //reversed
		{
			std::string str;
			str.reserve(len()*0.3);
			auto app=[&str](u64 x)
			{
				for(int i=0;i<16;++i)
				{
					str.push_back('0'+x%10);
					x/=10;
				}
				return false;
			};
			for(auto res=div(*this,(u64)1e16);res.first.len()||app(res.second);res=div(res.first,(u64)1e16))
				app(res.second);
			while(!str.empty()&&str.back()=='0')
				str.pop_back();
			return str;
		}

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
			a.data.resize(ceildiv(std::max(a.len(),b.len())+1,64)*8,0);
			bool carry=false;
			for(u32 i=0;i*8<a.data.size();++i)
			{
				u64 &c=a.get<u64>(i);
				u64 const bef=c,bi=b.get<u64>(i);
				c+=bi+carry;
				carry=(bef>c)|(bi+carry<bi);
			}
			return a;
		}
		Integer &operator-=(Integer &a,Integer const &b)//asserts a>=b
		{
			u32 const al=a.len();
			a.data.resize(ceildiv(al,64)*8,0);
			bool carry=false;
			for(u32 i=0;i*8<a.data.size();++i)
			{
				u64 &c=a.get<u64>(i);
				u64 const bef=c,bi=b.get<u64>(i);
				c-=carry+bi;
				carry=(bef<c)|(bi+carry<bi);
			}
			a.shrink();
			return a;
		}
		Integer &operator<<=(Integer &a,u32 const b)
		{
			u32 const B=b>>6;
			u32 const rem=b%(1<<6);
			a.shrink();
			a.data.resize(ceildiv(a.len()+b,64)*8,0);
			if(rem)
			{
				for(u32 i=a.data.size()/8-1;i>B;--i)
				{
					a.get<u64>(i)=a.get<u64>(i-B)<<rem;
					a.get<u64>(i)|=a.get<u64>(i-B-1)>>(64-rem);
				}
			}
			else for(u32 i=a.data.size()/8-1;i>B;--i)
				a.get<u64>(i)=a.get<u64>(i-B);
			if(a.data.size()>=B*8+8)
				a.get<u64>(B)=a.get<u64>(0)<<rem;
			std::fill_n(a.data.begin(),B*8,0);
			return a;
		}
		Integer &operator>>=(Integer &a,u32 b)
		{
			u32 const B=b>>6;
			u32 const rem=b%(1<<6);
			a.shrink();
			a.data.resize(ceildiv(a.len()+b,64)*8,0);
			if(rem)
			{
				for(u32 i=0;(i+B+1)*8<a.data.size();++i)
				{
					a.get<u64>(i)=a.get<u64>(i+B)>>rem;
					a.get<u64>(i)|=a.get<u64>(i+B+1)<<(64-rem);
				}
			}
			else for(u32 i=0;i*8<a.data.size();++i)
				a.get<u64>(i)=a.get<u64>(i+B);
			if(a.data.size()>=B*8+8)
			{
				u64 const iback=a.data.size()/8-1;
				a.get<u64>(iback-B)=a.get<u64>(iback)>>rem;
			}
			a.shrink();
			return a;
		}
		Integer operator*(Integer const&a,Integer const &b)
		{
			u32 const la=a.len(),Ba=ceildiv(la,8);
			u32 const lb=b.len(),Bb=ceildiv(lb,8);
			if(la+lb<=64)
				return Integer::from(a.to<u64>()*b.to<u64>());
			std::vector<u32>aa(ceildiv(la,16)),bb(ceildiv(lb,16));
			for(u32 i=0;i<Ba/2;++i)
				aa[i]=a.get<u16>(i);
			if(Ba%2)
				aa.back()=a.get(Ba-1);
			for(u32 i=0;i<Bb/2;++i)
				bb[i]=b.get<u16>(i);
			if(Bb%2)
				bb.back()=b.get(Bb-1);
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
		std::pair<Integer,Integer> div_plain(Integer const &a,Integer const &b)
		{
			if(b.len()==0)
				throw std::invalid_argument("divided by zero.");
			Integer res(a.data.size()),rem(0);
			for(int bit=a.len()-1;bit>=0;--bit)
			{
				rem<<=1;
				rem.set_bit(0,a.get_bit(bit));
				if(rem>=b)
				{
					rem-=b;
					res.set_bit(bit,true);
				}
			}
			res.shrink();
			rem.shrink();
			return {std::move(res),std::move(rem)};
		}
		// https://chat.deepseek.com/share/zl3qgw9x663nryo7am
		std::pair<Integer,Integer> div_newton(Integer const &a,Integer const &D)
		{
			static Integer const _1=Integer::from(1);
			u32 const la=a.len(),lD=D.len();
			if(la==0)
				return {Integer(0),Integer(0)};
			Integer x=_1;
			u32 len=lD,precision=1;//means X(\in R) eq x/2^len
			for(;precision<la;)
			{
				x=x*((_1<<(len+1))-x*D);
				len<<=1;
				precision<<=1;
				if(u32 lx=x.len();lx>precision)
				{
					x>>=lx-precision;
					len-=lx-precision;
				}
			}
			Integer d=(a*x)>>len,r=a-D*d;
			while(r>=D)
			{
				d+=_1;
				r-=D;
			}
			return {std::move(d),std::move(r)};
		}
		std::pair<Integer,Integer> div(Integer const &a,Integer const &b)
		{
			constexpr u64 lim=1ull<<10;
			if(a.data.size()*b.data.size()<lim)
				return div_plain(a,b);
			else
			 	return div_newton(a,b);
		}
		template<typename T>
		std::pair<Integer,T>div(Integer const &a,T b)
		{
			if(b==0)
				throw std::invalid_argument("divided by zero.");
			Integer res(a.data.size());
			T rem=0;
			for(int i=a.data.size()-1;i>=0;--i)
			{
				rem<<=8;
				rem|=a.data[i];
				res.data[i]=rem/b;
				rem%=b;
			}
			res.shrink();
			return {std::move(res),rem};
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

using reverSilly::int2048::Integer;
int2048::int2048():sign{1},value(0){}
int2048::int2048(long long x):value(Integer::from(std::abs(x)))
{
	if(x<0)
		sign=-1;
	if(x>=0)
		sign=1;
}
int2048::int2048(const std::string &s):value(0)
{
	std::string ss=s;
	for(int i=0;i*2u<ss.size();++i)
		std::swap(ss[i],ss[ss.size()-1-i]);
	if(!ss.empty()&&ss.back()=='-')
	{
		sign=-1;
		ss.pop_back();
	}
	else
	 	sign=1;
	value=Integer::from(std::string_view(ss));
}
int2048::int2048(const int2048&)=default;

void int2048::read(std::string const &s)
{
	operator=(s);
}
void int2048::print()const
{
	std::cout<<*this;
}
std::string int2048::str()const
{
	auto s=value.str();
	if(s.empty())
		s="0";
	if(value.len()&&sign<0)
		s.push_back('-');
	for(reverSilly::u32 i=0;i*2<s.size();++i)
		std::swap(s[i],s[s.size()-1-i]);
	return s;
}

int2048 minus(int2048 a,int2048 const&b)
{
	return a.minus(b);
}
int2048 &int2048::minus(int2048 const &b)
{
	if(b.sign<0)
		return add(-b);
	else if(sign<0)
	{
		sign=-sign;
		add(b);
		sign=-sign;
		return *this;
	}
	else
	{
		if(value>=b.value)
			value-=b.value;
		else
		{
			value=b.value-value;
			sign=-1;
		}
		return *this;
	}
}
int2048 &int2048::add(int2048 const &b)
{
	if(b.sign<0)
		return minus(-b);
	else if(sign<0)
		return operator=(b-(this->operator-()));
	else
	{
		value+=b.value;
		return *this;
	}
}
int2048 add(int2048 a,int2048 const &b)
{
	a.add(b);
	return a;
}
int2048 int2048::operator+()const
{
	return *this;
}
int2048	int2048::operator-()const
{
	int2048 res(*this);
	res.sign=-sign;
	return res;
}
int2048 &int2048::operator=(int2048 const&)=default;

int2048 &int2048::operator+=(const int2048 &b)
{
	return add(b);
}
int2048 operator+(int2048 const &a,int2048 const &b)
{
	return add(a,b);
}
int2048 &int2048::operator-=(const int2048 &b)
{
	return minus(b);
}
int2048 operator-(int2048 const &a, const int2048 &b)
{
	return minus(a,b);
}

int2048 &int2048::operator*=(const int2048 &a)
{
	value=value*a.value;
	sign=sign*a.sign;
	return *this;
}
int2048 operator*(int2048 a,const int2048 &b)
{
	return a*=b;
}

std::pair<int2048,int2048>div(int2048 const &a,int2048 const &b)
{
	auto[d,r]=div(a.value,b.value);
	int2048 dd,rr;
	dd.value=std::move(d);
	rr.value=std::move(r);
	dd.sign=a.sign*b.sign;
	rr.sign=a.sign;
	if(rr.value.len()!=0&&dd.sign<0)
	{
		dd.add(-1);
		rr.add(b);
	}
	return {std::move(dd),std::move(rr)};
}

int2048 &int2048::operator/=(const int2048 &b)
{
	return operator=(*this/b);
}
int2048 operator/(int2048 const &a,int2048 const&b)
{
	return div(a,b).first;
}

int2048 &int2048::operator%=(const int2048 &b)
{
	return operator=(*this%b);
}
int2048 operator%(int2048 const &a,int2048 const &b)
{
	return div(a,b).second;
}

std::istream &operator>>(std::istream &is,int2048 &a)
{
	std::string s;
	is>>s;
	a=s;
	return is;
}
std::ostream &operator<<(std::ostream &os,const int2048 &a)
{
	return os<<a.str();
}
} // namespace sjtu
