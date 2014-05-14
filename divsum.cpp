#include <stdio.h>
#include <math.h>
using namespace std;

int main()
{
    int cases;
    scanf("%d", &cases);
    while (cases--) {
        int n;
        scanf("%d", &n);
        if(n <= 1) {
            printf("0\n");
            continue;
        }
        int sq = sqrt(n) + 1, sum = 0;
        for (int i = 2; i <= sq; i++)
            if(n % i == 0)
                if(i * i == n)
                    sum += i;
                else if(i * i < n) sum += i + (n / i);
        printf("%d\n", sum + 1);
    }
}
