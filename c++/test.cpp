#include <iostream> 
#include <string> 
using namespace std;

double aplusabsb(double a,double b)
{
    return b ? a-b : a+b;
}


int main() { 
    cout << aplusabsb(1,-1);
    cout << aplusabsb(1,-2);
    cout << aplusabsb(1,2);
	return 0;
}
