#include<bits/stdc++.h>
#include"int2048.cpp"
using namespace std;
using reverSilly::int2048::Integer;
int main()
{
	auto a=Integer::from((1<<20)-1);
	auto b=Integer::from(5);
	cout<<(a-b).to<int>()<<'\n'
		<<a.len()<<'\n'
		<<(b<<1).to<int>()<<'\n'
		<<(b<<10).to<int>()<<'\n'
		<<(a>>10).to<int>();
	assert(a>b);
	// unsigned long long aaa=100;
	// for(int i=0;i<8;++i)
	// {
	// 	unsigned char *p=(unsigned char*)&aaa;
	// 	cout<<(int)p[i]<<' ';
	// }
}
