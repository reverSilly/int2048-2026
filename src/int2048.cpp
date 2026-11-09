#include "int2048.h"
#include <stdexcept>

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
			std::vector<u32>W(n);
			for(u32 len=2;len<=n;len<<=1)
			{
				W.resize(len/2);
				W[0]=1;
				if(len>2)
				{
					if constexpr(iNTT)
						W[1]=iw[__manual_clz(len)];
					else
						W[1]=w[__manual_clz(len)];
				}
				for(u32 i=2;i<len/2;++i)
					W[i]=W[i-1]*u64(W[1])%p;
				for(u32 i=0;i<n;i+=len)
				{
					for(u32 j=0;j<len/2;++j)
					{
						u64 const G=a[i+j],H=a[i+j+len/2];
						u64 ww=W[j];
						a[i+j]=(G+ww*H)%p;
						a[i+j+len/2]=(G+p-ww*H%p)%p;
					}
				}
			}
		}
		static std::vector<u32>mul(std::vector<u32> a,std::vector<u32> b)
		{
			u32 const n=ceil2(a.size()+b.size());
			if(n>=1<<25)
				throw std::out_of_range("integer too big");
			a.resize(n,0);
			b.resize(n,0);
			if(a!=b)
			{
				reverse(a);
				reverse(b);
				NTT<false>(a.data(),n);
				NTT<false>(b.data(),n);
				for(u32 i=0;i<n;++i)
					a[i]=u64(a[i])*b[i]%p;
			}
			else
			{
				reverse(a);
				NTT<false>(a.data(),n);
				for(u32 i=0;i<n;++i)
					a[i]=u64(a[i])*a[i]%p;
			}
			reverse(a);
			NTT<true>(a.data(),n);
			for(u32 i=0,in=inv(n);i<n;++i)
				a[i]=u64(a[i])*in%p;
			return a;
		}
	};

	std::vector<u64>mul(std::vector<u32>const &a,std::vector<u32>const &b)
	{
		if(a.size()*b.size()<230*(a.size()+b.size()))
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
		Integer operator-(Integer a,Integer const &b);
		Integer operator>>(Integer a,u32 b);
		Integer &operator+=(Integer &a,Integer const &b);
		std::pair<Integer,Integer>div_inv(Integer const &a,Integer const &D,Integer const &x,u32 len);

		Integer const &_1()
		{
			static Integer const _1=Integer::from(1);
			return _1;
		}
		Integer const &_2()
		{
			static Integer const _2=Integer::from(2);
			return _2;
		}
		u16 stoi(std::string_view sv)
		{
			u16 res=0;
			for(u32 i=sv.size()-1;i<sv.size();--i)
			{
				res*=10;
				res+=sv[i]-'0';
			}
			return res;
		}
		Integer Integer::from(std::string_view sv)
		{
			u32 const n=sv.size();
			Integer res(ceildiv(n,4));
			for(int i=0;i<res.data.size()/2;++i)
				res.get<u16>(i)=stoi(sv.substr(i*4,4));
			return res;
		}
		std::string Integer::str()const //reversed
		{
			u32 const l=len();
			std::string res(len()*4,'0');
			for(int i=0;i<l;++i)
			{
				u16 cur=get<u16>(i);
				for(int j=0;j<4;++j)
				{
					res[i*4+j]='0'+cur%10;
					cur/=10;
				}
			}
			while(!res.empty()&&res.back()=='0')
				res.pop_back();
			return res;
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
			int const n=std::max(a.len(),b.len())+1;
			a.data.resize(n*2,0);
			bool carry=false;
			for(u32 i=0;i<n;++i)
			{
				u16 &c=a.get<u16>(i);
				u16 const bi=b.get<u16>(i);
				c+=bi+carry;
				if(c>=10000)
				{
					carry=true;
					c-=10000;
				}
				else carry=false;
				if(i*2>b.data.size()&&!carry)
					break;
			}
			a.shrink();
			return a;
		}
		Integer &operator-=(Integer &a,Integer const &b)//asserts a>=b
		{
			u32 const al=a.len();
			a.data.resize(al*2,0);
			bool carry=false;
			for(u32 i=0;i<al;++i)
			{
				u16 &c=a.get<u16>(i);
				u16 const bef=c,bi=b.get<u16>(i);
				c-=carry+bi;
				if(bef<c)
				{
					carry=true;
					c+=10000;
				}
				else carry=false;
				if(i*2>b.data.size()&&!carry)
					break;
			}
			a.shrink();
			return a;
		}
		Integer &operator<<=(Integer &a,u32 const b)
		{
			a.shrink();
			a.data.insert(a.data.begin(),b*2,0);
			return a;
		}
		Integer &operator>>=(Integer &a,u32 b)
		{
			if(b>a.len())
			{
				a.data.clear();
				return a;
			}
			a.data.erase(a.data.begin(),a.data.begin()+b*2);
			return a;
		}
		Integer operator*(Integer const&a,Integer const &b)
		{
			u32 const la=a.len();
			u32 const lb=b.len();
			if(la+lb<=4)
				return Integer::from(a.to<u64>()*b.to<u64>());
			std::vector<u32>aa(la),bb(lb);
			for(u32 i=0;i<la;++i)
				aa[i]=a.get<u16>(i);
			for(u32 i=0;i<lb;++i)
				bb[i]=b.get<u16>(i);
			auto res=mul(aa,bb);
			u64 carry=0;
			for(u32 i=0;i<res.size();++i)
			{
				res[i]+=carry;
				carry=res[i]/10000;
				res[i]%=10000;
			}
			while(carry)
			{
				res.push_back(carry%10000);
				carry/=10000;
			}
			Integer res1(res.size());
			for(u32 i=0;i<res.size();++i)
				res1.get<u16>(i)=res[i];
			res1.shrink();
			return res1;
		}
		Integer operator<<(Integer a,u32 b);
		u64 inv_1e20(u64 x)
		{
			constexpr u64 _1e15=1e15,_1e5=1e5;
			u64 d=_1e15/x,r=_1e15%x;
			d=d*_1e5+r*_1e5/x;
			return d;
		}
		std::pair<Integer,u32> inv(Integer const &D,u32 la)
		{
			u32 const lD=D.len(),front=std::min(lD,3u);
			u64 Dapprox=(D>>(lD-front)).to<u64>();
			if(front<lD)
				++Dapprox;
			for(u32 i=front;i<3;++i)
    			Dapprox*=10000;
			Integer x=Integer::from(inv_1e20(Dapprox));
			u32 len=2+lD,precision=2;
			// means X(\in R) eq x/2^len
			// |X-1/D|<=2^{-precision-lD}
			// DX'-1<(DX-1)^2
			for(;precision+lD<=la;)
			{
				precision<<=1;//new precision
				bool approx=false;
				Integer curD=D;
				u32 excessive=lD-std::min(lD,precision+1);
				if(excessive)
				{
					curD>>=excessive;
					curD+=_1();
					approx=true;
				}
				x=x*((_2()<<(len-excessive))-x*curD);
				len=len+len-excessive;
				if(u32 lx=x.len();lx>precision+1)
				{
					x>>=lx-precision-1;
					len-=lx-precision-1;
					approx=true;
				}
				if(approx)
					--precision; //new precision decrease because of approx
			}
			return {std::move(x),len};
		}
		std::pair<Integer,Integer>div_inv(Integer const &a,Integer const &D,Integer const &x,u32 len)
		{
			if(D.len()==0)
				throw std::invalid_argument{"divided by zero."};
			Integer d=(a*x)>>len,r=a-D*d;
			//precision
			while(r>=D)
			{
				d+=_1();
				r-=D;
			}
			return {std::move(d),std::move(r)};
		}
		// https://chat.deepseek.com/share/zl3qgw9x663nryo7am
		std::pair<Integer,Integer> div_newton(Integer const &a,Integer const &D)
		{
			auto[x,len]=inv(D,a.len());
			return div_inv(a,D,x,len);
		}
		std::pair<Integer,Integer> div(Integer const &a,Integer const &b)
		{
			return div_newton(a,b);
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
