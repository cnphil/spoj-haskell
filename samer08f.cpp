#include <stdio.h>
using namespace std;

int main()
{
    int n;
    int f[102];
    f[0] = 0; f[1] = 1;
    for(int i = 2; i <= 100; i++) f[i] = i * i + f[i - 1];
    while (scanf("%d", &n), n != 0) printf("%d\n", f[n]);
}
