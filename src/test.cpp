#include<bits/stdc++.h>
using namespace std;
int main()
{
	unsigned long long aaa=100;
	for(int i=0;i<8;++i)
	{
		unsigned char *p=(unsigned char*)&aaa;
		cout<<(int)p[i]<<' ';
	}
}
